%% @doc Signal handler for the nREPL command line on the BEAM.
%%
%% The BEAM cannot trap SIGINT, so the command-line client uses SIGQUIT
%% (Control+\) to interrupt an evaluation. This handler takes the place of
%% the default `erl_signal_handler', which halts the VM on SIGQUIT. It keeps
%% the default behavior for SIGTERM and SIGUSR1.
-module(nrepl_signal_handler).

-behaviour(gen_event).

-export([install/1]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2]).

%% @doc Install the handler. `Fun' is called with the signal atom: in a new
%% process for `sigquit', and before the VM stops for `sigterm'.
install(Fun) ->
  _ = gen_event:delete_handler(erl_signal_server, erl_signal_handler, []),
  ok = gen_event:add_handler(erl_signal_server, ?MODULE, Fun),
  ok = os:set_signal(sigquit, handle),
  ok.

init(Fun) ->
  {ok, Fun}.

handle_event(sigquit, Fun) ->
  _ = spawn(fun() -> Fun(sigquit) end),
  {ok, Fun};
handle_event(sigterm, Fun) ->
  try Fun(sigterm) catch _:_ -> ok end,
  ok = init:stop(),
  {ok, Fun};
handle_event(sigusr1, Fun) ->
  erlang:halt("Received SIGUSR1"),
  {ok, Fun};
handle_event(_Signal, Fun) ->
  {ok, Fun}.

handle_call(_Request, Fun) ->
  {ok, ok, Fun}.

handle_info(_Info, Fun) ->
  {ok, Fun}.

terminate(_Args, _Fun) ->
  ok.
