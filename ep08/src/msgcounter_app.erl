%%% msgcounter_supモジュールをスーパバイザとする
%%% アプリケーションのためのモジュールです
%%% これをapplicationモジュールからロードすることで
%%% Erlangのアプリケーションとして認識されます
-module(msgcounter_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    msgcounter_sup:start_link().

stop(_State) ->
    ok.
