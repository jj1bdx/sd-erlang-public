%%% 箱同士の距離を計算します（マップ版）
-module(boxmapdist).
-export([center/2, distance/2,
         boxdistance/2]).
%%% 箱の中央の座標を求めます
-spec center(#{}, #{}) -> #{}.
center(Coord, Size) ->
    #{x => maps:get(x, Coord) + (maps:get(x, Size) / 2.0),
      y => maps:get(y, Coord) + (maps:get(y, Size) / 2.0)}.
%%% 2点間の距離を求めます
-spec distance(#{}, #{}) -> float().
distance(C1, C2) ->
    DX = maps:get(x, C2) - maps:get(x, C1),
    DY = maps:get(y, C2) - maps:get(y, C1),
    math:sqrt((DX*DX) + (DY*DY)).
%%% 箱の中心座標間の距離を求めます
-spec boxdistance(#{}, #{}) -> #{}.
boxdistance(Box1, Box2) ->
    distance(center(maps:get(coord, Box1),
                    maps:get(size, Box1)),
             center(maps:get(coord, Box2),
                    maps:get(size, Box2))).
