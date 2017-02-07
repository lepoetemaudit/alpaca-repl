-module(alpaca_shell).

-export([start/0, server/0]).

-include_lib("alpaca/src/alpaca_ast.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(repl_state, {bindings = [], 
                     functions = [],
                     types = []}).

%% Entrypoints

start() ->
    spawn(fun () -> server() end).

server() ->
    %% Trap exits
    process_flag(trap_exit, true),
    %% Print welcome banner
    io:put_chars(" == \x1b[34m Alpaca Shell 0.0.3 \x1b[0m== \n\n"
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

format_type({unbound, T, _}) ->
    case T of
        t0 -> "'a";
        t1 -> "'b";
        t2 -> "'c";
        t3 -> "'d";
        t4 -> "'e";
        t5 -> "'f";
        t6 -> "'g";
        t7 -> "'h";
        t8 -> "'i";
        _  -> "'?"
    end;
format_type(T) when is_atom(T) ->
    atom_to_list(T);
format_type({t_list, T}) ->
    io_lib:format("list ~s", [format_type(T)]);
format_type({t_tuple, Types}) ->
    TypeNames = lists:map(fun format_type/1, Types),
    TypeString = string:join(TypeNames, ", "),
    Output = io_lib:format("(~s)", [TypeString]),
    lists:flatten(Output);
format_type({t_record, Members, _}) ->
    MemberList = lists:map(fun({t_record_member, Name, T}) ->
                               atom_to_list(Name) ++ " : " ++ format_type(T)
                           end, Members),
    MemberString = string:join(MemberList, ", "),
    "{" ++ MemberString ++ "}";
format_type(Other) ->
    io_lib:format("~p", [Other]).

output_result(Result, {t_arrow, Args, Return}) ->  
    ListifiedArgs = lists:map(fun format_type/1, Args),
    ArgList = string:join(ListifiedArgs, " -> "),

    print_result(io_lib:format("<fun> :: ~s -> ~s", [ArgList, format_type(Return)]));

output_result(Result, Type) ->
    print_result(io_lib:format("~s :: ~s", [format_result(Result), format_type(Type)])).
 
 print_result(Text) ->
    io:format("\x1b[32m -- ~s\x1b[0m\n\n", [Text]).

%% EXPRESION EXECUTION

run_bind(Funs, Bin) ->
    code:load_binary(alpaca_user_shell, Funs, Bin),
    try alpaca_user_shell:main({}) of
        Res -> {ok, Res}
    catch      
        error:Err -> {error, Err}
    end.

%% ERROR PRINTING

format_error({Line, alpaca_parser, [Error, Detail]}) ->
    {HumanDetail, Help} = case Detail of
        "assign" -> {"=", " (maybe you're missing a let?)"};
        Other -> {Other, ""}
    end,
    io_lib:format("Syntax Error: ~s'~s'~s",  [Error, HumanDetail, Help]);
format_error({bad_variable_name, Var}) ->
    io_lib:format("Unknown variable: ~s", [Var]);
format_error({not_found, _, Symbol, _}) ->
    io_lib:format("Unknown symbol: ~s", [Symbol]);  
format_error({cannot_unify, _, _, TypeOne, TypeTwo}) ->
    io_lib:format("Type Mismatch: ~s was expected but ~s provided", [TypeOne, TypeTwo]);
format_error({duplicate_definition, Name, _}) ->
    io_lib:format("Already defined: ~s", [Name]);
format_error({_, alpaca_scan, {_, Msg}}) ->
    io_lib:format("Scan error: ~s", [Msg]);    
format_error(Other) when is_list(Other) ->
    io_lib:format("Unknown Error: ~s", [Other]);
format_error(Other) ->
    io_lib:format("Unknown Error: ~p", [Other]).

output_error(Text) ->
    io:format("\x1b[31m -- ~s\x1b[0m\n\n", [Text]).

%% COMPILING
compile_typed(Module) ->
    {ok, Mods} = alpaca_ast_gen:make_modules([{alpaca_usershell, Module}]), 
    case alpaca_typer:type_modules(Mods) of
        {ok, [TypedMod]} ->
            {ok, Forms} = alpaca_codegen:gen(TypedMod, []),
            {compile:forms(Forms, [report, verbose, from_core]), TypedMod};
        Err -> Err
  end.
   
%% VALUE FORMATTING (injects Erlang values into Alpaca source)
%% TODO - it might be wiser to generate tokens rather than raw strings

format_value(Record = #{'__struct__' := record}) ->
    NoStruct = maps:filter(fun(K, _) -> K =/= '__struct__' end, Record),
    RecordParts = lists:map(fun({K, V}) ->
                      atom_to_list(K) ++ " = " ++ format_value(V)
                  end, maps:to_list(NoStruct)),
  "{" ++ string:join(RecordParts, ", ") ++ "}";
format_value(V) when is_atom(V) ->
    io_lib:format(":~w", [V]);
format_value(V) -> io_lib:format("~w", [V]).

render_bind({Name, Type, Result}) ->
    lists:flatten(io_lib:format("let ~s = ~s in \n", [Name, format_value(Result)])).

render_fun(Body) ->
    Body ++ "\n".

build_module(State = #repl_state{bindings = Bindings, functions = Funs}) ->
    FunsList = lists:map(fun render_fun/1, Funs),
    FunsString = string:join(FunsList, "\n"),
    BindingsList = lists:map(fun render_bind/1, Bindings),    
    BindingsString = string:join(BindingsList, "\n"),
    "module user_shell\n"
    "export main/1\n\n" ++
    FunsString ++
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
  
find_main_type(#alpaca_module{functions=FunDefs}) ->
    find_main_type(FunDefs).

run_expression(Expr, State) ->
    %% Construct a fake module and inject the entered expression
    %% into a fake function main/1 so we can call it from Erlang
    %% Compile the module
    Module = build_module(State) ++ "\n    " ++ Expr ++ "\n\n",  
    case compile_typed(Module) of
        {{ok, Funs, Bin}, Types} -> 
            MainType = find_main_type(Types),
            %% Load the created module
            code:load_binary(alpaca_user_shell, Funs, Bin),
            %% Execute the main function and return both the value and the
            %% inferred type.
            try alpaca_user_shell:main({}) of
                Val -> {ok, {Val, MainType}}
            catch
                Other -> Other
        end;
        {error, _} = Err -> Err;
        Other -> Other
  end.

run_expression(Expr) ->
    run_expression(Expr, #repl_state{}).

handle_expression(Expr, State) ->
    case run_expression(Expr, State) of
        {ok, {Val, MainType}} -> output_result(Val, MainType);
        {error, Err} -> output_error(format_error(Err))
    end,
    State.

handle_fundef(Expr, State = #repl_state{functions = Functions}, {symbol, _, Name}) ->
    NewFuns = [Expr | Functions],
    StateWithFun = State#repl_state{functions=NewFuns},
    Module = build_module(StateWithFun) ++ "    :ok",
    case compile_typed(Module) of
        {{ok, Funs, Bin}, Types} ->
            State#repl_state{functions = NewFuns};
        {error, Err} -> {error, Err, State};
        Other -> {error, Other, State}
    end.

handle_bind(Expr, 
            State = #repl_state{bindings = Bindings}, 
            {symbol, _, Name}) ->  
    
    BindingExpr = Expr ++ " in " ++ Name ++ "\n\n",
    Module = build_module(State) ++ BindingExpr,
    case compile_typed(Module) of
        {{ok, Funs, Bin}, Types} -> 
            MainType = find_main_type(Types),
            %% Value bind - execute the expression and store the result
            {ok, Result} = run_bind(Funs, Bin),                
            Bindings_ = Bindings ++ [{Name, MainType, Result}],
            State#repl_state{bindings = Bindings_};
    
        {error, Err} -> {error, Err, State};
        Other -> {error, Other, State}
    end.

%% INPUT PARSING

% Try and identify what sort of input the user entered.
parse_input("") -> {empty, ""};
parse_input(Input) ->
    case alpaca_scanner:scan(Input) of
        {ok, Toks, NumLines} ->
            case alpaca_ast_gen:parse(Toks) of
                {ok, {alpaca_fun_def, _, Name, Arity, Versions}} ->
                    case Arity of
                        0 -> {bind_value, Name};
                        _ -> {bind_fun, Name}
                    end;
                    %% TODO - this is nasty
                    {ok, {error, non_literal_value, Name, _}} -> 
                        {bind_value, Name};

                    {ok, Other} -> {expression, Other};
                    {error, _} = Err -> Err
            end;
        {error, Err, _} -> {error, Err}
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
        false -> read_input(" \x1b[33m... \x1b[0m", Lines_)
    end.

read_input(Prompt) ->
     read_input(Prompt, []).

%% MAIN LOOP

server_loop(State) ->
    % Collect input - supporting functions or types currently  
    Input = read_input(" \x1b[33m " ++ [955] ++ "\x1b[0m  "),  
    State_ = case parse_input(Input) of
        {empty, _} -> io:format(" -- Nothing entered\n\n"), State;
        {expression, _} -> handle_expression(Input, State);
        {bind_value, Name} -> handle_bind(Input, State, Name);
        {bind_fun, Name} -> handle_fundef(Input, State, Name);
        {error, Err} -> output_error(format_error(Err)), State
    end, 
    server_loop(State_).

-ifdef(TEST).

input_type_test_() -> 
    [?_assertMatch({bind_fun, {symbol, _, "myfun"}}, parse_input("let myfun f = 10")),
     ?_assertMatch({bind_value, {symbol, _, "myval"}}, parse_input("let myval = 42")),
     ?_assertMatch({expression, _}, parse_input("100")),
     ?_assertMatch({expression, _}, parse_input("let f = 10 in f")),
     ?_assertMatch({expression, _}, parse_input("let f x = x in f"))].

expression_type_test_() ->
    [?_assertMatch({ok, {42, t_int}}, run_expression("42")),
     ?_assertMatch({ok, {<<"hello">>, t_string}}, run_expression("\"hello\"")),
     ?_assertMatch({ok, {_, {t_arrow, [t_int], t_int}}}, 
                   run_expression("let f x = x + 1 in f"))].
     
error_test_() ->
    [?_assertMatch({error, {cannot_unify, _, _, t_int, t_string}}, 
                 run_expression("\"hello\" + 42")),
     ?_assertMatch({error, {1, alpaca_parser, ["syntax error before: ", "break"]}},
                 parse_input("let a b c;;"))].

value_bind_test() ->
    State = handle_bind("let num = 42", #repl_state{}, {symbol, 1, "num"}),
    ?assertMatch(#repl_state{bindings = [{"num", t_int, 42}]}, State),
    ?assertMatch({ok, {42, t_int}}, run_expression("num", State)).

value_expression_bind_test() ->
    State = handle_bind("let num = 24 + 24", #repl_state{}, {symbol, 1, "num"}),
    ?assertMatch(#repl_state{bindings = [{"num", t_int, 48}]}, State),
    ?assertMatch({ok, {48, t_int}}, run_expression("num", State)).

fun_bind_test() ->
    State = handle_fundef("let sqr x = x * x", #repl_state{}, {symbol, 1, "sqr"}),
    ?assertMatch(#repl_state{functions = ["let sqr x = x * x"]}, State).

-endif.
