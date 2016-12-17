-module(alpaca_shell).

-export([start/0]).

start() ->
  spawn(fun () -> server() end).

server() ->
  process_flag(trap_exit, true),
  % Print welcome banner
  io:put_chars(" == Alpaca Shell 0.01a == \n\n"
               " (hint: exit with ctrl-c, run expression by terminating with"
               " ';;' or an empty line)\n\n"),
  % Enter main server loop
  server_loop(). 

% Strip ;; if line terminates with it
strip_terminator(Line) ->
  re:replace(Line, ";;\n$", "\n\n", [{return, list}]).

% Termination happens if a line is empty or terminates with ;;
line_terminates(Line) ->
    (re:run(Line, ";;\n$") /= nomatch) or (Line == "\n").

% Read input until terminating condition found 
read_expression(Prompt, Lines) ->
  Line = io:get_line(Prompt),
  Lines_ = Lines ++ strip_terminator(Line),
  case line_terminates(Line) of
     true -> Lines_;
     false -> read_expression(" ..) ", Lines_)
  end.

read_expression(Prompt) ->
  read_expression(Prompt, []).

% Format the result
output_result(Result) when is_binary(Result) -> 
  io:format("~s\n", [Result]);

output_result(Result) -> 
  io:format("~w\n", [Result]).
 
% Takes a compile 'expression' module and executes its single main function,
% displaying the result
run_expression(Funs, Bin) ->
  % Load the module
  code:load_binary(dummy, Funs, Bin),
  % Execute the fake function  
  % Display the result as best we can
  % Alpaca can still error at runtime in some cases
  % so we execute in another process  
  spawn(fun() -> 
    Result = dummy:main({}),  
    output_result(Result) 
  end).

adjust_line(Line) -> Line - 5.

print_error({Line, alpaca_parser, Err}) ->
  io:format("\x1b[31m -- Syntax Error: ~B: ~w ~w", [adjust_line(Line), Err]);

print_error({bad_variable_name, Var}) ->
  io:format("\x1b[31m -- Unknown variable: ~s\n\x1b[0m", [Var]);

print_error({not_found, _, Symbol, _}) ->
  io:format("\x1b[31m -- Unknown symbol: ~s\n\x1b[0m", [Symbol]);

print_error({badmatch, {error, {Line, alpaca_parser, [Error, Detail]}}}) ->
  io:format(
    "\x1b[31m -- Syntax Error: ~B: ~s~s\n\x1b[0m", 
    [adjust_line(Line), Error, Detail]);

print_error(Other) when is_list(Other) ->
  io:format("\x1b[31m -- Unknown Error: ~s\n\x1b[0m", [Other]);
print_error(Other) ->
  io:format("\x1b[31m -- Unknown Error: ~w\n\x1b[0m", [Other]).
 
compile(Module) ->
  % This can hang or crash, so run in another process    
  Pid = spawn_link(fun () -> 
               try alpaca:compile({text, Module}) of
                 Res -> exit({compiled, Res})
               catch 
                 error:Err -> exit({error, Err})
               end
             end),
  receive    
    {'EXIT', Pid, {compiled, Res}} -> Res;
    {'EXIT', Pid, Other} -> Other
  after 2000 ->
    exit(Pid, timeout),
    {error, "Compiler timed out"}
  end.

server_loop() ->
  % Collect input, assuming expressions only
  case read_expression(" ->) ") of
    "\n" -> io:format(" -- Nothing entered\n\n");
    Expr -> 
            % Construct a fake module and inject the entered expression
            % into a fake function main/1 so we can call it from Erlang
            Module = "module dummy \n\n"
                     "export main/1 \n\n"
                     "main () = \n" ++ Expr,
            % Compile the module
            case compile(Module) of
              {ok, Funs, Bin} -> run_expression(Funs, Bin);
              {error, Err} -> print_error(Err);
              Other -> print_error(Other)
            end
  end, 
  server_loop().
            