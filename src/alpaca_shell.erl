-module(alpaca_shell).

-export([start/0, server/0]).

-record(repl_state, {bindings = [], 
                     funs = [],
                     types = []}).

%% Entrypoints

start() ->
  spawn(fun () -> server() end).

server() ->
  %% Trap exits
  process_flag(trap_exit, true),
  %% Print welcome banner
  io:put_chars(" == Alpaca Shell 0.02a == \n\n"
               " (hint: exit with ctrl-c, run expression by terminating with"
               " ';;' or an empty line)\n\n"),
  %% Enter main server loop
  server_loop(#repl_state{}). 

%% RESULT PRINTING

%^ Format the result
output_result(Result) when is_binary(Result) -> 
  io:format("-- ~s\n", [Result]);

output_result(Result) -> 
  io:format("-- ~s\n", [format_value(Result)]).
 
%% EXPRESION EXECUTION

%% Takes a compile 'expression' module and executes its single main function,
%% displaying the result
run_expression(Funs, Bin) ->
  %% Load the module
  code:load_binary(alpaca_user_shell, Funs, Bin),
  %% Execute the fake function  
  %% Display the result as best we can
  %% Alpaca can still error at runtime in some cases
  %% so we execute in another process  
  spawn(fun() -> 
    Result = alpaca_user_shell:main({}),  
    output_result(Result) 
  end).

run_bind(Funs, Bin) ->
  code:load_binary(alpaca_user_shell, Funs, Bin),
  Pid = spawn_link(fun() -> 
    try alpaca_user_shell:main({}) of
      Res -> exit({ok, Res})
    catch      
      error:Err -> exit(Err)
    end
  end),
  receive    
    {'EXIT', Pid, {ok, Res}} -> {ok, Res};
    {'EXIT', Pid, Other} -> Other
  after 5000 ->
    exit(Pid, timeout),
    {error, "Execution timed out"}
  end.

%% ERROR PRINTING

adjust_line(Line) -> Line - 5.

print_error({Line, alpaca_parser, Err}) ->
  io:format("\x1b[31m -- Syntax Error: ~B: ~w", [adjust_line(Line), Err]);

print_error({bad_variable_name, Var}) ->
  io:format("\x1b[31m -- Unknown variable: ~s\n\x1b[0m", [Var]);

print_error({not_found, _, Symbol, _}) ->
  io:format("\x1b[31m -- Unknown symbol: ~s\n\x1b[0m", [Symbol]);

print_error({badmatch, {error, {Line, alpaca_parser, [Error, Detail]}}}) ->
  io:format(
    "\x1b[31m -- Syntax Error: ~B: ~s~s\n\x1b[0m", 
    [adjust_line(Line), Error, Detail]);

print_error({cannot_unify, _, _, TypeOne, TypeTwo}) ->
  io:format(
    "\x1b[31m -- Type Mismatch: A ~s was expected but a ~s was provided\n\x1b[0m", [TypeOne, TypeTwo]);

print_error({duplicate_definition, Name, _}) ->
 io:format(
    "\x1b[31m -- Already defined: ~s\n\x1b[0m", [Name]);

print_error(Other) when is_list(Other) ->
  io:format("\x1b[31m -- Unknown Error: ~s\n\x1b[0m", [Other]);
print_error(Other) ->
  io:format("\x1b[31m -- Unknown Error: ~w\n\x1b[0m", [Other]).
 
%% COMPILING
compile(Module) ->
  %% This can hang or crash, so run in another process    
  Pid = spawn_link(fun () -> 
               try alpaca:compile({text, Module}) of
                 Res -> exit({compiled, Res})
               catch 
                 error:Err -> exit({error, Err});
                 Anything -> exit(Anything)
               end
             end),
  receive    
    {'EXIT', Pid, {compiled, Res}} -> Res;
    {'EXIT', Pid, Other} -> Other
  after 2000 ->
    exit(Pid, timeout),
    {error, "Compiler timed out"}
  end.

format_value(Record = #{'__struct__' := record}) ->
  NoStruct = maps:filter(fun(K, _) -> K =/= '__struct__' end, Record),
  RecordParts = lists:map(fun({K, V}) ->
                  atom_to_list(K) ++ " = " ++ format_value(V)
                end, maps:to_list(NoStruct)),
  "{" ++ string:join(RecordParts, ", ") ++ "}";
format_value(V) when is_atom(V) ->
    io_lib:format(":~w", [V]);
format_value(V) -> io_lib:format("~w", [V]).

build_module(State = #repl_state{bindings = Bindings, funs = Funs}) ->
  BindingsList = lists:map(fun({Name, Result}) ->
                   "let " ++ Name ++ " = " ++ format_value(Result) ++ " in "
                 end, Bindings),
  BindingsString = string:join(BindingsList, "\n"),
  FunsList = lists:map(fun(F) ->
               "let " ++ F ++ " in "
             end, Funs),
  FunsString = string:join(FunsList, "\n"),
  "module user_shell \n\n"
  "export main/1 \n\n"
  "main () = " ++ BindingsString ++ FunsList.
  
handle_expression(Expr, State) ->
  %% Construct a fake module and inject the entered expression
  %% into a fake function main/1 so we can call it from Erlang
  %% Compile the module
  Module = build_module(State) ++ "\n" ++ Expr,  
  case compile(Module) of
    {ok, Funs, Bin} -> run_expression(Funs, Bin);
    {error, Err} -> print_error(Err);
    Other -> print_error(Other)
  end,
  State.

handle_bind(Expr, State = #repl_state{bindings = Bindings, funs = FunBinds}, [_, _, Name, Args]) ->  
  BindingExpr = "let " ++ Expr ++ " in " ++ Name ++ "\n\n",    
  Module = build_module(State) ++ BindingExpr,
  case compile(Module) of
    {ok, Funs, Bin} -> 
      case Args of
        %% Value bind - execute the expression and store the result
        "" -> case run_bind(Funs, Bin) of
                {ok, Result} -> 
                  State#repl_state{bindings = Bindings ++ [{Name, Result}]};
                Other -> print_error(Other), State
              end;
        %% Function bind - we don't need to run it, just store the expression
        _ -> State#repl_state{funs = FunBinds ++ [Expr]}
      end;
    {error, Err} -> print_error(Err), State;
    Other -> print_error(Other), State
  end.

%% INPUT PARSING

% Run through a list of regular expressions,
% breaking on the first one that matches
input_filter(Input, []) -> {expression, []};
input_filter(Input, [{Name, Exp} | Rest]) ->
  case re:run(Input, Exp, [{capture, all, list}]) of
    nomatch -> input_filter(Input, Rest);    
    {match, Captures} -> {Name, Captures}
  end. 

% Try and identify what sort of input the user entered.
% We use regular expressions to do this... using an actual tokenizer
% and parser would yield much better results
parse_input(Input) ->
  input_filter(
    Input, 
    [{empty, "^(\s*)\n$"}, % Empty input
     {expression, "^(\s*)let"}, % Let bind
     {expression, "^(\s*)match"}, % Match expression
     {bind, "^(\s*)([a-z][a-zA-Z]*)\s+(.*?)="} % bind
    ]).

% Strip ;; and newline terminators
strip_terminator(Line) ->
  L = re:replace(Line, ";;\n$", "", [{return, list}]),
  re:replace(L, "\n$", "", [{return, list}]).

% Termination happens if a line is empty or terminates with ;;
line_terminates(Line) ->
    (re:run(Line, ";;\n$") /= nomatch) or (Line == "\n").

% Read input until terminating condition found 
read_input(Prompt, Lines) ->
  Line = io:get_line(Prompt),
  Lines_ = Lines ++ strip_terminator(Line),
  case line_terminates(Line) of
     true -> Lines_;
     false -> read_input(" .. ", Lines_)
  end.

read_input(Prompt) ->
  read_input(Prompt, []).

%% MAIN LOOP

server_loop(State) ->
  % Collect input - supporting functions or types currently  
  Input = read_input(" -> "),  
  State_ = case parse_input(Input) of
    {empty, _} -> io:format(" -- Nothing entered\n\n");    
    {expression, _} -> handle_expression(Input, State);
    {bind, Captures} -> handle_bind(Input, State, Captures)
  end, 
  server_loop(State_).