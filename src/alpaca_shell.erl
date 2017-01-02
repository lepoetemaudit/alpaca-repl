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
format_result(Result) when is_binary(Result) -> 
  io_lib:format("\"~s\"", [Result]);

format_result(Result) -> 
  io_lib:format("~s", [format_value(Result)]).

output_result(Result, {t_arrow, Args, Return}) ->  
  ListifiedArgs = lists:map(fun atom_to_list/1, Args),
  ArgList = string:join(ListifiedArgs, " -> "),
  io:format("<fun> :: ~s -> ~s~n", [ArgList, Return]);

output_result(Result, Type) ->
  io:format("~s :: ~s~n", [format_result(Result), Type]).
 
%% EXPRESION EXECUTION

%% Takes a compile 'expression' module and executes its single main function,
%% displaying the result
run_expression(Funs, Bin, Type) ->
  %% Load the module
  code:load_binary(alpaca_user_shell, Funs, Bin),
  %% Execute the fake function  
  %% Display the result as best we can
  %% Alpaca can still error at runtime in some cases
  %% so we execute in another process  
  spawn(fun() -> 
    Result = alpaca_user_shell:main({}),  
    output_result(Result, Type) 
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

compile_typed(Module) ->
 %% This can hang or crash, so run in another process    
  Pid = spawn_link(fun () -> 
    {ok, NV, Map, Mod} = alpaca_ast_gen:parse_module(0, Module),                                     
    case alpaca_typer:type_modules([Mod]) of
        {error, _}=Err -> Err;
        {ok, [TypedMod]} ->
          {ok, Forms} = alpaca_codegen:gen(TypedMod, []),
          exit({compiled, 
                compile:forms(Forms, [report, verbose, from_core]), 
                TypedMod})
    end
  end),
  receive    
    {'EXIT', Pid, {compiled, Res, Types}} -> {Res, Types};
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
  FunsString = string:join(Funs, "\n\n"),
  "module user_shell\n"
  "export main/1\n\n" ++ FunsString ++ "\n\n" ++
  "let main () = \n    " ++ BindingsString.
  
find_main_type([]) ->
  {error, main_not_found};
find_main_type([Type | Rest] = Types) when is_list(Types) ->
  case Type of
    {alpaca_fun_def, 
      {t_arrow, [t_unit], ReturnType}, {symbol, _, "main"},
      _, _} -> ReturnType;
    _ -> find_main_type(Rest)
  end;
  
find_main_type({alpaca_module, user_shell, Funs, _, _, _, FunDefs, _}) ->
  find_main_type(FunDefs).

handle_expression(Expr, State) ->
  %% Construct a fake module and inject the entered expression
  %% into a fake function main/1 so we can call it from Erlang
  %% Compile the module
  Module = build_module(State) ++ "\n" ++ Expr,  
  case compile_typed(Module) of
    {{ok, Funs, Bin}, Types} -> 
      MainType = find_main_type(Types),
      run_expression(Funs, Bin, MainType);
    {error, Err} -> print_error(Err);
    Other -> print_error(Other)
  end,
  State.

handle_bind(Expr, 
            BindType, 
            State = #repl_state{bindings = Bindings, funs = FunBinds}, 
            {symbol, _, Name}) ->  
  BindingExpr = Expr ++ " in " ++ Name ++ "\n\n",    
  Module = build_module(State) ++ BindingExpr,
  case compile(Module) of
    {ok, Funs, Bin} -> 
      case BindType of
        %% Value bind - execute the expression and store the result
        value -> case run_bind(Funs, Bin) of
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

% Try and identify what sort of input the user entered.
parse_input(Input) ->
  {ok, Toks, NumLines} = alpaca_scanner:scan(Input),
  case alpaca_ast_gen:parse(Toks) of
    {ok, {alpaca_fun_def, _, Name, Arity, Versions}} ->
      case Arity of
        0 -> {bind_value, Name};
        _ -> {bind_fun, Name}
      end;
    {ok, Other} -> {expression, Other}
  end.

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
    {bind_value, Name} -> handle_bind(Input, value, State, Name);
    {bind_fun, Name} -> handle_bind(Input, function, State, Name)
  end, 
  server_loop(State_).