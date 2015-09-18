%%% 箱同士の距離を計算します
-module(boxdist).
-export([center/2, distance/2,
         boxdistance/2]).
%%% 箱のレコード定義をインクルードします
-include("box.hrl").
%%% 箱の中央の座標を求めます
-spec center(#xy{}, #xy{}) -> #xy{}.
center(Coord, Size) ->
    #xy{x = Coord#xy.x + (Size#xy.x / 2.0),
        y = Coord#xy.y + (Size#xy.y / 2.0)}.
%%% 2点間の距離を求めます
-spec distance(#xy{}, #xy{}) -> float().
distance(C1, C2) ->
    DX = C2#xy.x - C1#xy.x,
    DY = C2#xy.y - C1#xy.y,
    math:sqrt((DX*DX) + (DY*DY)).
%%% 箱の中心座標間の距離を求めます
-spec boxdistance(#box{}, #box{}) -> #xy{}.
boxdistance(Box1, Box2) ->
    distance(center(Box1#box.coord, Box1#box.size),
             center(Box2#box.coord, Box2#box.size)).
