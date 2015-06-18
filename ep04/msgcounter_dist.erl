%%% 分散Erlangノードでカウンタを実行するモジュールです
%%% 詳細は第3回のmsgcounter.erlを参照してください

-module(msgcounter_dist).

-export([start/0, stop/1, inc/1, dec/1, zero/1, val/1]).

%%% カウンタの別名をマクロとして定義しておきます
-define(COUNTER_NAME, message_counter).

%%% 自ノードでカウンタのプロセスをスタートします
%%% 上記の別名で登録します

-spec start() -> true.
start() ->
    register(?COUNTER_NAME, spawn(fun counter/0)).

%%% ノード名を指定してカウンタへ指示を送るための関数群です

-spec stop(node()) -> ok.
stop(Node) -> sendmsg(Node, stop).

-spec inc(node()) -> integer().
inc(Node) -> sendmsg(Node, inc).

-spec dec(node()) -> integer().
dec(Node) -> sendmsg(Node, dec).

-spec zero(node()) -> 0.
zero(Node) -> sendmsg(Node, zero).

-spec val(node()) -> integer().
val(Node) -> sendmsg(Node, val).

%%% 各カウンタのプロセスへメッセージを送る関数です

sendmsg(Node, Req) ->
    % リファレンスを取得します
    Ref = make_ref(),
    % 登録済みプロセスとノード名を指定して
    % 別のノードにメッセージを送ることができます
    {?COUNTER_NAME, Node} ! {self(), Ref, Req},
    % 受信したリファレンスの値で自分宛かどうかを判定します
    receive
        {Ref, Resp} -> Resp
    end.

counter() ->
    counter(0).

%%% 返すメッセージにリファレンスを含むことで
%%% pidに頼らずメッセージを識別できます

counter(Count) ->
    receive
        {From, Ref, zero} ->
            From ! {Ref, 0},
            counter(0);
        {From, Ref, inc} ->
            From ! {Ref, Count + 1},
            counter(Count + 1);
        {From, Ref, dec} ->
            From ! {Ref, Count - 1},
            counter(Count - 1);
        {From, Ref, val} ->
            From ! {Ref, Count},
            counter(Count);
        {From, Ref, stop} ->
            From ! {Ref, ok},
            exit(normal);
        {From, Ref, Else} ->
            From ! {Ref, {unknown_message, Else}},
            counter(Count)
    end.
