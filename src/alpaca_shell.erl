-module(alpaca_shell).

-export([start/0, server/0]).

-include_lib("src/alpaca_ast.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(repl_state, {bindings = [],
                     types = [],
                     infixes = [],
                     shell_id = "__shell__"}).

%% Entrypoints

start() ->
    spawn(fun () -> server() end).

server() ->
    %% Trap exits
    process_flag(trap_exit, true),
    %% Print welcome banner
    io:put_chars("\x1b[32m==\x1b[0m\x1b[34m Alpaca Shell 0.0.4 \x1b[32m==\x1b[0m\n\n"
                 "\x1b[32m(Exit with ctrl-c, run expressions by terminating with "
                 "';;' or an empty line)\x1b[0m\n\n"),
    %% Generate a unique identifier for this shell
    ShellId = make_shell_id(),
    %% Enter main server loop
    server_loop(#repl_state{shell_id=ShellId}).

make_shell_id() ->
    Ident = base64:encode(crypto:strong_rand_bytes(12)),
    re:replace(Ident, "[=+/]", "", [{return, list}]).

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

    print_result(io_lib:format("<fun> \x1b[32m::\x1b[0m ~s -> ~s", [ArgList, format_type(Return)]));

output_result(Result, Type) ->
    print_result(io_lib:format("~s \x1b[32m::\x1b[0m ~s", [format_result(Result), format_type(Type)])).

 print_result(Text) ->
    io:format("~s\x1b[0m\n\n", [Text]).

%% EXPRESION EXECUTION

%%run_bind(Funs, Bin) ->
%%    code:load_binary(alpaca_user_shell, Funs, Bin),
%% ERROR PRINTING

format_error(Err) ->
    alpaca_error_format:fmt({error, Err}, "en_US").

output_error(Text) ->
    io:format("\x1b[31m -- ~s\x1b[0m\n\n", [Text]).

%% COMPILING
compile_typed(Module, Beams, State = #repl_state{shell_id=ShellId}) ->
    %% Write module code to temporary file
    TempFile = ".alpaca-repl" ++ ShellId ++ ".alp",
    file:write_file(TempFile, Module, [write, sync]),
    %% Wait until it has definitely written (sync)
    (fun WaitSync() ->
         case file:read_file_info(TempFile) of
             {ok, _} -> ok;
             {error, enoent} -> timer:sleep(5), WaitSync()
         end
     end
    )(),

    case alpaca:compile({files, [TempFile | Beams]}) of
        {ok, Mods} ->
            [{compiled_module, Name, FN, B}] = Mods,
            {module, Mod} = code:load_binary(Name, FN, B),
            ModTypes = proplists:get_value(
                         alpaca_typeinfo, Mod:module_info(attributes)),
            {ok, {Mod, ModTypes}};
        {error, _} = Err -> Err
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

render_bind({_, Expr}) ->
    Expr ++ " in \n".

render_fun(Body) ->
    Body ++ "\n".

build_module(State = #repl_state{bindings = Bindings}) ->
    BindingsList = lists:map(fun render_bind/1, Bindings),
    BindingsString = string:join(BindingsList, "\n"),
    "module user_shell\n"
    "export main/1\n\n" ++
    "let main () = \n    " ++ BindingsString.

find_main_type([]) ->
  {error, main_not_found};
find_main_type([Type | Rest] = Types) when is_list(Types) ->
    case Type of
        #alpaca_binding{
            type={t_arrow, [t_unit], ReturnType},
            name={symbol, _, "main"}} ->
            ReturnType;

        #alpaca_binding{
            type={t_arrow, [t_unit], ReturnType},
            name={'Symbol', #{name := <<"main">>}}} ->
            ReturnType;

    _ -> find_main_type(Rest)
  end;

find_main_type(#alpaca_module{functions=FunDefs}) ->
    find_main_type(FunDefs).

collect_beams(Module) ->
    %% Collect .beam files for any referenced dependencies
    {user_shell, DepModules} = alpaca:list_dependencies(Module),
    ModRefs = lists:map(
                fun(M) -> "alpaca_" ++ atom_to_list(M) ++ ".beam" end,
                DepModules),

    lists:filtermap(fun(M) -> case code:where_is_file(M) of
                                  non_existing -> false;
                                  Path -> {true, Path}
                              end
                    end,
                    ModRefs).

run_expression(Expr, State) ->
    %% Construct a fake module and inject the entered expression
    %% into a fake function main/1 so we can call it from Erlang
    %% Compile the module
    Module = build_module(State) ++ "\n    " ++ Expr ++ "\n\n",
    Beams = collect_beams(Module),

    case compile_typed(Module, Beams, State) of
        {ok, {Mod, Types}} ->
            MainType = find_main_type(Types),
            %% Execute the main function and return both the value and the
            %% inferred type.
            try Mod:main({}) of
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

handle_fundef(Expr, State, {'Symbol', #{name := <<40, _/binary >>}}) ->
    ok = infixes;

handle_fundef(Expr, State, Name) ->
    io:format("Name is ~p~n", [Name]),
    handle_bind(Expr, State, Name).

handle_bind(Expr,
            State = #repl_state{bindings = Bindings},
            {'Symbol', #{name := Name}}) ->

    NewBinding = [{Name, Expr}],

    FilteredBindings = lists:filter(
        fun({N, _}) -> N /= Name end, Bindings) ++ NewBinding,
    %%BindingExpr = Expr ++ " in " ++ binary_to_list(Name) ++ "\n\n",

    FilteredState = State#repl_state{bindings = FilteredBindings},
    Module = build_module(FilteredState) ++ " " ++ binary_to_list(Name),
    Beams = collect_beams(Module),
    case compile_typed(Module, Beams, FilteredState) of
        {ok, {Mod, Types}} ->
            %%MainType = find_main_type(Types),
            %% Value bind - execute the expression and store it
            try Mod:main({}) of
                _ -> FilteredState
            catch
                error:Err -> {error, Err}
            end
    end.

%% INPUT PARSING

% Try and identify what sort of input the user entered.
parse_input("") -> {empty, ""};
parse_input(Input) ->
    case alpaca_scanner:scan(Input) of
        {ok, Toks, NumLines} ->
            case alpaca_ast_gen:parse(Toks) of

                {ok, #alpaca_binding{
                        name=Name,
                        bound_expr=#alpaca_fun{arity=Arity},
                        body=undefined}} ->
                    case Arity of
                        0 -> {bind_value, Name};
                        _ -> {bind_fun, Name}
                    end;

                {ok, #alpaca_binding{name=Name, body=undefined}} -> {bind_value, Name};

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
  re:replace(L, "\n$", " ", [{return, list}]).

% Termination happens if a line is empty or terminates with ;;
line_terminates(Line) ->
    (re:run(Line, ";;\n$") /= nomatch) or (Line == "\n").

% Read input until terminating condition found
read_input(Prompt, Lines) ->
    Line = io:get_line(Prompt),
    Lines_ = Lines ++ strip_terminator(Line),
    case line_terminates(Line) of
        true -> Lines_;
        false -> read_input("\x1b[33m\xb7 \x1b[0m", Lines_)
    end.

read_input(Prompt) ->
     read_input(Prompt, []).

%% MAIN LOOP

server_loop(State) ->
    % Collect input - supporting functions or types currently
    Input = read_input("\x1b[33m" ++ [955] ++ "\x1b[0m "),
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
    [?_assertMatch(
        {bind_fun, {'Symbol', #{name := <<"myfun">>}}},
        parse_input("let myfun f = 10")),
     ?_assertMatch({bind_value, {'Symbol', #{name := <<"myval">>}}},
        parse_input("let myval = 42")),
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
    AlpacaModules =
        [alpaca, alpaca_ast, alpaca_ast_gen, alpaca_codegen,
         alpaca_compiled_po, alpaca_error_format, alpaca_exhaustiveness,
         alpaca_parser, alpaca_scan, alpaca_scanner, alpaca_typer],
    ok = code:ensure_modules_loaded(AlpacaModules),
    State = handle_bind(
        "let num = 42",
         #repl_state{},
         {'Symbol', #{name => <<"num">>}}),
    ?assertMatch(#repl_state{bindings = [{<<"num">>, "let num = 42"}]}, State),
    ?assertMatch({ok, {42, t_int}}, run_expression("num", State)).

value_expression_bind_test() ->
    State = handle_bind(
        "let num = 24 + 24",
        #repl_state{},
        {'Symbol', #{name => <<"num">>}}),
    ?assertMatch(#repl_state{bindings = [{<<"num">>, "let num = 24 + 24"}]}, State),
    ?assertMatch({ok, {48, t_int}}, run_expression("num", State)).

fun_bind_test() ->
    State = handle_fundef(
        "let sqr x = x * x",
        #repl_state{},
        {'Symbol', #{name => <<"sqr">>}}),
    ?assertMatch(#repl_state{bindings = [{<<"sqr">>, "let sqr x = x * x"}]}, State).

expression_rebind_test() ->
    State1 = handle_bind(
        "let num = 4",
        #repl_state{},
        {'Symbol', #{name => <<"num">>}}),
    State2 = handle_bind(
        "let num = 8",
        State1,
        {'Symbol', #{name => <<"num">>}}),

    ?assertMatch(
        {ok, {8, t_int}},
        run_expression("num", State2)).

bind_infix_test() ->
    State = handle_fundef(
        "let (<|) f x = f x",
        #repl_state{},
        {'Symbol', #{name => <<"(<|)">>}}),

    ?assertMatch(
        {ok, {20, t_int}},
        run_expression("let add10 x = x + 10 in add10 <| 10")).

-endif.
