-module(loopmap).

-export([loopx2/1, mapx2/1, loopsum/1, foldsum/1,
         loopmod3/1, filtermod3/1]).

%%% [1..N]のリストの各要素を2倍して
%%% [2, 4, 6, .. N * 2]とするコードです

%%% 再帰によるループの例
%%% このモジュールの中の再帰によるコードは
%%% すべて末尾再帰として書いています

-spec loopx2(non_neg_integer()) -> list(non_neg_integer()).

%%% when はガードといい，定義 when 条件 の形で
%%% 条件が成立した場合のみ
%%% パターンマッチングを試行します

%%% loopx2/1では内部状態を取る変数が2つ必要なため
%%% loopx2/3へ制御を渡します

loopx2(N) when N >= 0 ->
    loopx2(N, 1, []).

loopx2(0, _, L) ->
    % 結果をリストの左側から積んでいくため
    % 終了時には左右をひっくり返す処理が必要になります
    lists:reverse(L);
loopx2(N, M, L) ->
    % 以下のio:format/2のコメントを外して有効にすると
    % ループの中間結果がわかります
    % io:format("N = ~p, M = ~p, L = ~p~n", [N, M, L]),
    loopx2(N - 1, M + 1, [M * 2 | L]).

-spec mapx2(non_neg_integer()) -> list(non_neg_integer()).

%%% mapによるリスト処理の例
%%% lists:seq/2は[1..N]のリストを生成します
%%% このリスト内包表記では
%%% "||"の右側のリストの各要素に対し
%%% 左側の関数を適用した結果を返します

mapx2(N) ->
    [X * 2 || X <- lists:seq(1, N)].

%%% 1からNまでの整数の和を返すコードです

-spec loopsum(non_neg_integer()) -> non_neg_integer().

%%% 再帰によるループの例

loopsum(N) when N >= 0 ->
    loopsum(N, 0).

loopsum(0, Sum) -> Sum;
loopsum(N, Sum) ->
    % io:format("N = ~p, Sum = ~p~n", [N, Sum]),
    loopsum(N - 1, N + Sum).

%%% fold演算の1つfoldl（左側へのたたみ込み）により
%%% [1..N]のリストに対し（0は初期値）
%%% (((((0 + 1) + 2) + 3) + ... ) + N)を計算します

-spec foldsum(non_neg_integer()) -> non_neg_integer().

foldsum(N) ->
    % 最初の関数がリストの要素とそれまでの計算結果に適用されます
    % 2番目の引数が初期値となります
    % 3番目の引数が計算対象となるリストです
    lists:foldl(fun(X, Sum) -> X + Sum end, 0, lists:seq(1, N)).

%%% 1からNまでの整数の中で3で割り切れるものを返すコードです

-spec loopmod3(non_neg_integer()) -> list(non_neg_integer()).

%%% 再帰によるループの例

loopmod3(N) when N >= 0 ->
    loopmod3(N, 1, []).

loopmod3(0, _, L) ->
    lists:reverse(L);
loopmod3(N, M, L) ->
    % io:format("N = ~p, M = ~p, L = ~p~n", [N, M, L]),
    case (M rem 3) =:= 0 of
        true -> loopmod3(N - 1, M + 1, [M | L]);
        false -> loopmod3(N - 1, M + 1, L)
    end.

%%% filter関数によって条件に合致するものだけを
%%% リストとして出力します

-spec filtermod3(non_neg_integer()) -> list(non_neg_integer()).

filtermod3(N) ->
    % =:= は両辺の型も値も完全に一致していることを示します
    % （浮動小数点数と整数の比較には =:= ではなく == を使います）
    lists:filter(fun(X) -> (X rem 3) =:= 0 end, lists:seq(1, N)).

