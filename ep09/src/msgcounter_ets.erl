%%% ETSを使ったカウンタの実装です
%%% 関数名は連載第3回のmsgcounterモジュールに準じます
-module(msgcounter_ets).
-export([init/0, cleanup/0,
         start/1, stop/1, inc/1, dec/1, zero/1, val/1]).
%%% ?MODULEはモジュール名(msgcounter_ets)を示すマクロです
%%% ETSのテーブル名は?MODULEとします
%%% カウンタを列挙するETSテーブルを初期化します
%%% アクセス権はpublicなのでどのプロセスからも読み書きできます
%%% ETSテーブルを検索するキーは、keyposオプションで
%%% 位置を指定できます（既定値は1でタプルの最初の要素）
-spec init() -> atom().
init() ->
    ets:new(?MODULE, [named_table, public]).
%%% カウンタを列挙するETSテーブルを消去します
-spec cleanup() -> true.
cleanup() ->
    ets:delete(?MODULE).
%%% 名前を引数に取るカウンタを使えるようにします
-spec start(term()) -> term().
start(Name) ->
    ets:insert(?MODULE, {Name, 0}).
%%% 名前を引数に取るカウンタを消します
-spec stop(term()) -> ok.
stop(Name) -> 
    ets:delete(?MODULE, Name).
%%% 名前で指定したカウンタの値を1つ増やします
%%% ここで登場するets:update_counter/3は
%%% テーブル内のエントリに存在する
%%% タプルの指定した位置の要素をカウンタとして操作します
%%% 操作中は他のプロセスはテーブルにアクセスできないため
%%% 値の更新に関する一貫性が保たれます
-spec inc(term()) -> integer().
inc(Name) ->
    ets:update_counter(?MODULE, Name, {2, 1}).
%%% 名前で指定したカウンタの値を1つ減らします
-spec dec(term()) -> integer().
dec(Name) ->
    ets:update_counter(?MODULE, Name, {2, -1}).
%%% 名前で指定したカウンタの値をゼロ(0)にします
-spec zero(term()) -> 0.
zero(Name) ->
    ets:insert(?MODULE, {Name, 0}),
    0.
%%% 名前で指定したカウンタの値を返します
-spec val(pid()) -> integer().
val(Name) ->
    ets:lookup_element(?MODULE, Name, 2).
