%% @doc Entry point of the `nrepl' escript. It runs `nrepl.cmdline' with the
%% command line arguments, like `clojure -M -m nrepl.cmdline' on the JVM.
-module(nrepl_escript).

-export([main/1]).

main(Args) ->
  ok = clojerl:start(),
  BinArgs = [unicode:characters_to_binary(Arg) || Arg <- Args],
  'clojure.main':main([<<"-m">>, <<"nrepl.cmdline">> | BinArgs]).
