-module(alpaca_boot).

-export([start/0]).

start() -> user_drv:start(['tty_sl -c -e',{alpaca_shell,start,[]}]).