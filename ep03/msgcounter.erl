-module(msgcounter).

-export([start/0, stop/1, inc/1, dec/1, zero/1, val/1]).

%%% カウンタのプロセスをスタートします
%%% プロセスのPidを返します
%%% 各カウンタのプロセスは中にカウンタの値を状態として保持します

-spec start() -> pid().

start() -> spawn(fun counter/0).

%%% Pidで指定したカウンタのプロセスを終了します

-spec stop(pid()) -> ok.

stop(Pid) -> sendmsg(Pid, stop).

%%% Pidで指定したカウンタの値を1つ増やします

-spec inc(pid()) -> integer().

inc(Pid) -> sendmsg(Pid, inc).

%%% Pidで指定したカウンタの値を1つ減らします

-spec dec(pid()) -> integer().

dec(Pid) -> sendmsg(Pid, dec).

%%% Pidで指定したカウンタの値をゼロ(0)にします

-spec zero(pid()) -> 0.

zero(Pid) -> sendmsg(Pid, zero).

%%% Pidで指定したカウンタの値を返します

-spec val(pid()) -> integer().

val(Pid) -> sendmsg(Pid, val).

%%% 各カウンタのプロセスへメッセージを送り
%%% 返答を受けとるための関数です
%%% （該当するプロセスがなかった場合はどうなるか試してみてください）

sendmsg(Pid, Req) ->
    % 「Pid ! メッセージの値」という式を評価すると，
    % Pidに対応するプロセスにメッセージが送られます
    % self()は自分自身が動いているプロセスのPidです
    % self()を相手に送ることで相手が誰にメッセージを
    % 返せばいいかがわかります
    Pid ! {self(), Req},
    % receive式でメッセージを受信します
    % receiveの後に列挙されたパターンにマッチしていれば
    % -> の右側の値を返します
    % case式と似た構文で条件判定が可能です
    receive
        {Pid, Resp} -> Resp
    end.

counter() ->
    counter(0).

%%% 各カウンタのプロセスは起動すると
%%% ループを作ってメッセージを待ちます
%%% 意味のあるメッセージを受信すると
%%% それぞれに応じた値を返して状態を更新します

counter(Count) ->
    receive
        {From, zero} ->
            From ! {self(), 0},
            counter(0);
        {From, inc} ->
            From ! {self(), Count + 1},
            counter(Count + 1);
        {From, dec} ->
            From ! {self(), Count - 1},
            counter(Count - 1);
        {From, val} ->
            From ! {self(), Count},
            counter(Count);
        {From, stop} ->
            From ! {self(), ok},
            % プロセスを終了します
            exit(normal);
        {From, Else} ->
            From ! {self(), {unknown_message, Else}},
            counter(Count)
    end.
