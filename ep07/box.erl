%%% 箱を扱うモジュールです
-module(box).
-export([coord/2, initbox/0,
         setcolor/2, setsize/2, setcoord/2,
         getcolor/1, getcoord/1, area/1]).
%%% 箱のレコード定義をインクルードします
-include("box.hrl").
%%% 浮動小数点で与えたXY座標をレコードxyにします
-spec coord(float(), float()) -> #xy{}.
coord(X, Y) -> #xy{x = X, y = Y}.
%%% 大きさゼロの箱を返します
-spec initbox() -> #box{}.
initbox() ->
    #box{size = #xy{x = 0.0, y = 0.0},         
         color = undefined,
         coord = #xy{x = 0.0, y = 0.0}}.
%%% すでに作った箱の色を再設定した箱を返します
-spec setcolor(#box{}, atom()) -> #box{}.
setcolor(Box, Color) -> Box#box{color = Color}.
%%% すでに作った箱の大きさを変えた箱を返します
-spec setsize(#box{}, #xy{}) -> #box{}.
setsize(Box, Size) when
    Size#xy.x >= 0.0, Size#xy.y >= 0.0 ->
    Box#box{size = Size}.
%%% すでに作った箱の位置を変えた箱を返します
-spec setcoord(#box{}, #xy{}) -> #box{}.
setcoord(Box, Coord) -> Box#box{coord = Coord}.
%%% 箱の色を返します
-spec getcolor(#box{}) -> atom().
getcolor(Box) -> Box#box.color.
%%% 箱の左下の座標を返します
-spec getcoord(#box{}) -> #xy{}.
getcoord(Box) -> Box#box.coord.
%%% 箱の面積を求めます
-spec area(#box{}) -> float().
area(Box) ->
    % ネストしたレコードの中から値を取り出します
    Box#box.size#xy.x * Box#box.size#xy.y.
