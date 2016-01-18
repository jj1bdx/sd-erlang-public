%%% TinyMT（32ビット版）の疑似乱数生成パラメータを
%%% Mnesiaで入出力するためのモジュールです
-module(tinymt_params).
%%% パラメータのレコード定義をインクルードします
-include("tinymt_params.hrl").
%%% 外部に見せる関数定義です
-export([first_time/0,
         init/0,
         add_record/1,
         select_record_tmat_delete/1,
         pickup_random_param/0]).
%%% 最初にMnesiaのスキーマを作る関数です
%%% この関数を動かす時は分散ノードの接続が確立している必要があります
-spec first_time() -> ok.
first_time() ->
    ok = mnesia:create_schema([node() | nodes()]).
%%% Mnesiaのテーブルを作る関数です
%%% 分散ノードそれぞれのメモリとディスクにコピーを作ります
-spec init() -> {atomic, ok}.
init() ->
    {atomic, ok} = mnesia:create_table(
      tinymt32param,
      [{disc_copies, [node() | nodes()]},
       {attributes,
           record_info(fields, tinymt32param)}]).
%%% Mnesiaのテーブルにレコードを加えます
%%% テーブル書き込みはトランザクションとして扱います    
-spec add_record(#tinymt32param{}) -> {atomic, ok}.
add_record(R) ->
    mnesia:transaction(
      fun() -> mnesia:write(R) end).
%%% Mnesiaのテーブルにあるレコードのうち
%%% tmatが引数と同じか大きいものを1つ選び
%%% その選んだレコードを返すと同時に消去します
%%% 選択、読み出し、消去はまとめてトランザクションとして扱います
-spec select_record_tmat_delete(uint32()) -> #tinymt32param{}.
select_record_tmat_delete(Tmat) ->
%%% マッチスペックを定義します
%%% tmatフィールドが与えられた値以上の場合
%%% characteristicを返すという動作を定義します
    Matchhead = #tinymt32param{
        characteristic = '$1', tmat = '$2', _ = '_'},
    Guard = [{'>=', '$2', Tmat}],
    Result = '$1',
%%% トランザクションのための関数Fを定義します
    F = fun() ->
%%% マッチした最初のレコードのキーを取り出します
            {[V], _} = mnesia:select(tinymt32param,
                          [{Matchhead, Guard, [Result]}], 1, write),
%%% キーを使ってレコード全体を読み出します
            [Rec] = mnesia:read(tinymt32param, V),
%%% キーを使って読み出したレコードを消去します
            ok = mnesia:delete(tinymt32param, V, write),
            Rec
        end,
%%% 関数Fを1つのトランザクションとしてまとめて実行します
    {atomic, R} = mnesia:transaction(F),
%%% 得たレコードを返します
    R.
%%% この関数は上記の関数を使い、ランダムに1つパラメータを選んで返します
%%% 選ばれたパラメータはMnesiaのテーブルから消えるので重複はありません
-spec pickup_random_param() -> #tinymt32param{}.
pickup_random_param() ->
    select_record_tmat_delete(rand:uniform(16#100000000) - 1).
