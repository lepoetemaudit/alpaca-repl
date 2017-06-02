-module(alpaca_boot).

-export([start/0]).



start() ->
    %% Locate Alpaca compiler. Default to ALPACA_ROOT if set.
    AlpacaPaths = [
        os:getenv("ALPACA_ROOT"),
        "/usr/lib/alpaca",
        "/usr/local/lib/alpaca"],
    AlpacaHome = get_best_path(AlpacaPaths),
    code:add_path(AlpacaHome),
    AlpacaModules =
        [alpaca, alpaca_ast, alpaca_ast_gen, alpaca_codegen,
         alpaca_compiled_po, alpaca_error_format, alpaca_exhaustiveness,
         alpaca_parser, alpaca_scan, alpaca_scanner, alpaca_typer],
    case code:ensure_modules_loaded(AlpacaModules) of
        ok ->
            user_drv:start(['tty_sl -c -e',{alpaca_shell,start,[]}]);
        _ ->
            print_no_alpaca_error()
    end.

get_best_path([]) ->
    print_no_alpaca_error();

get_best_path([Path | Rest]) ->
    case filelib:is_dir(Path) of
        true -> {ok, Path};
        false -> get_best_path(Rest)
    end.

print_no_alpaca_error() ->
    user:start(),
    io:put_chars(
        standard_error, "Error: Cannot find Alpaca. Please follow"
                        " instructions at http://alpaca-lang.org\n"),
    halt(1, []).
