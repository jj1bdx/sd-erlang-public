%%% -*- coding: utf-8 -*-
%%% RGB値をHUV値に変換する関数を定義するモジュールです
%%% 参考URL: http://en.wikipedia.org/wiki/HSL_and_HSV

%%% ←行中のパーセントで始まる部分から行の終わりまではコメントです

%%% モジュール宣言（ファイル名はモジュール名と同じ(rgb2hiv.beam)にします）
-module(rgb2huv). 

%%% モジュール内のどの関数を外部に見せるかを選択します
%%% exportの引数は関数のリストを取ります
%%% リストは大カッコでくくります
-export([tohuv/1,
         tohuv/3]).

%%% マクロ定義（モジュール内で関数の外でのみ有効）
-define(MAXVAL, 255.0).

%%% 型定義 huv()を3つの浮動小数点数を要素とするタプルとします
%%% タプルは中カッコでくくります
-type huv() :: {float(), float(), float()}.

%%% 関数 zerodiv/2 の定義です
%%% この関数はexportのリストに入っていないため外部から見えません
%%% 関数の型定義 zerodiv/2は整数の引数を2つ持ち浮動小数点数を返します
-spec zerodiv(integer(), integer()) -> float().
%%% 関数定義はここから始まります
% パターンマッチングのパターンが変わるごとにセミコロンを使います
% "_"はどんな引数にもマッチするという意味です
zerodiv(_, 0) -> 0.0; % ←ここには2番目の引数がゼロであればマッチします
% すべての変数は「大文字」で始まります
% 演算子「/」は浮動小数点数を扱います（整数同士の割り算は「div」剰余は「rem」）
zerodiv(X, Y) -> float(X) / float(Y). % ←マッチしない場合この式に来ます
%%% 関数定義は式の最後のピリオドで終わります

%%% 関数 diffdiv/3 の定義です
-spec diffdiv(integer(), integer(), integer()) -> float().
diffdiv(X1, X2, Y) -> zerodiv((X1 - X2), Y).
            
%%% 関数 tohuv/3 の定義です
%%% 0~255の間のRGBの整数値をそれぞれ引数に取り
%%% 返り値は{Hue, Saturation, Value}のタプルで返します
%%% （0.0 =< Hue < 360.0, 0.0 =< Saturation =< 1.0, 0.0 =< Value =< 1.0）
-spec tohuv(integer(), integer(), integer()) -> huv().
tohuv(R, G, B) ->
    % bandはビットAND演算子， borはビットOR演算子です
    % bslはビット左シフトを右側に指定したビット数分行います
    tohuv(((R band 16#ff) bsl 16) bor
          ((G band 16#ff) bsl 8) bor
           (B band 16#ff)).

%%% 関数 tohuv/1 の定義です
%%% tohuv/3と同様ですが引数はRGB値を8ビットごとにまとめたものを
%%% 24ビットの整数として与えます
%%% 例: HTMLのRGBコード"#88AA55"は"16#88AA55"と与えます
-spec tohuv(integer()) -> huv().
%%% 以下どの変数も一度しか代入されていません
tohuv(C) ->
    % 関数の中での逐次実行は式の最後にカンマを付けて続けます
    % 次の行はビットストリングのパターンマッチです
    <<R:8, G:8, B:8>> = <<C:24>>, % 24bitの整数を8bitごとに分割します
    Max = max(max(R, G), B), % erlang:max/2は大きな値を取るBIF
    Min = min(min(R, G), B), % erlang:min/2は小さな値を取るBIF
    D = Max - Min,
    H1 = 60.0 * (
        % 「case 条件式 of 値1 -> 式1; ... 値n -> 式n end」は
        % 条件式と各値の比較を行って一致すれば対応する式の値を返します
        % 最後の式にはセミコロンをつけず「end」で終わります
        case Max of
            R -> diffdiv(G, B, D) + 6.0;
            G -> diffdiv(B, R, D) + 2.0;
            B -> diffdiv(R, G, D) + 4.0
        end),
    Hue = case H1 >= 360.0 of
              true -> H1 - 360.0;
              false -> H1
          end,
    % 各関数は最後に評価した式の値を返します
    % マクロの参照は「?」を最初につけて行います
    {Hue, zerodiv(D, Max), Max / ?MAXVAL}.

%%% 以上でソースコード終わり
                  

            
    
                                     
    



