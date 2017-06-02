-module(alpaca_boot).

-export([start/0]).

start() ->
    %% Locate Alpaca compiler
    AlpacaHome = os:getenv("ALPACA_ROOT", "/usr/local/opt/alpaca/ebin"),
    code:add_path(AlpacaHome),
    AlpacaModules =
        [alpaca, alpaca_ast, alpaca_ast_gen, alpaca_codegen,
         alpaca_compiled_po, alpaca_error_format, alpaca_exhaustiveness,
         alpaca_parser, alpaca_scan, alpaca_scanner, alpaca_typer],
    case code:ensure_modules_loaded(AlpacaModules) of
        ok ->
            user_drv:start(['tty_sl -c -e',{alpaca_shell,start,[]}]);
        _ ->
            user:start(),
            io:put_chars(
                standard_error, "Error: Cannot find Alpaca. Please follow"
                                " instructions at http://alpaca-lang.org\n"),
            halt(1, [])
    end.
