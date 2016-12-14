-module(alpaca_shell).

-export([start/0]).

start() ->
  spawn(fun () -> server() end).

server() ->
  process_flag(trap_exit, true),
  io:put_chars(" == Alpaca Shell Version 0.01a == \n\n"),
  server_loop(). 

read_expression(Prompt, Lines) ->
  Line = io:get_line(Prompt),
  Lines_ = Lines ++ Line,
  ContainsTerminator = string:str(Line, ";;") > 0,

  if Line == "\n" -> Lines_;
     ContainsTerminator == true -> Lines_;
     true  -> read_expression("... ", Lines_)
  end.

read_expression(Prompt) ->
  read_expression(Prompt, []).

server_loop() ->
  Expr = read_expression(" -> "),
  Module = "module dummy \n\n"
           "export main/1 \n\n"
           "main () = " ++ Expr,
  {ok, Funs, Bin} = alpaca:compile({text, Module}),
  code:load_binary(dummy, Funs, Bin),
  io:format("   ~w\n", [dummy:main({})]),
  server_loop().

  