%%% リンクに関する各種関数を紹介します
-module(link_example).
-export([wait_spawn_link/2,
         wait_spawn_link/1,
         trap_link_msg/0,
         gen_prob_true/1,
         spawn_prob_link/1]).
%%% この関数ではNミリ秒待った後に
%%% 理由を変数Statusとして終了する
%%% プロセスを起動してリンクを張ります
-spec wait_spawn_link(pos_integer(), term()) -> pid().
wait_spawn_link(N, Status) when N >= 1 ->
    spawn_link(fun() ->
                       timer:sleep(N),
                       exit(Status)
               end).
%%% この関数では上記関数の正常終了版です
-spec wait_spawn_link(pos_integer()) -> pid().
wait_spawn_link(N) ->
    wait_spawn_link(N, normal).
%%% この関数ではtrap_exitをtrueとした
%%% プロセスを起動してリンクを張ります
%%% この関数がメッセージを1つ受信したら
%%% その旨標準出力に出します
%%% こうすることでリンクの先のプロセスに
%%% どのような終了シグナルが伝わっているかを確認できます
-spec trap_link_msg() -> pid().
trap_link_msg() ->
    spawn_link(
      fun() ->
              process_flag(trap_exit, true),
              receive X ->
                      io:format("Process ~p received: ~p~n",
                                [self(), X])
              end
      end).
%%% この関数はtrueとfalseをランダムに選んで
%%% N個の要素を持つリストにします
%%% （出現確率は1/2に近いですがrand:uniform/1で
%%% 決めているだけなのでN個の中で厳密には決めていません）
-spec gen_prob_true(pos_integer()) -> [true | false].
gen_prob_true(N) when N >= 1 ->
    [rand:uniform(2) =:= 1 || _ <- lists:seq(1, N)].
%%% この関数では前述のランダムにtrue/falseを出す
%%% リストを使って、N個のプロセスを起動します
%%% これらのプロセスは無期限に待つだけで何もしません
%%% そしてtrueになっているものにはリンクを張り、
%%% falseであるものにはリンクを張らないという動作をします
%%% 関数の返り値{P, L}のうち
%%% Pはpidのリスト、Lは対応するtrue/falseの別のリストです
-spec spawn_prob_link(pos_integer()) -> {[pid()], [true | false]}.
spawn_prob_link(N) ->
    L = gen_prob_true(N),
    P = [case C of
             true ->
                 spawn_link(fun() -> timer:sleep(infinity) end);
             false ->
                 spawn(fun() -> timer:sleep(infinity) end)
         end || C <- L],
    {P, L}.
