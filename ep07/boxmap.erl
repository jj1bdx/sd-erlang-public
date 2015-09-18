%%% 箱を扱うモジュールです（マップ版）
-module(boxmap).
-export([coord/2, initbox/0,
         setcolor/2, setsize/2, setcoord/2,
         getcolor/1, getcoord/1, area/1]).
%%% マップでは個別の名前は付きません
%%% 浮動小数点で与えたXY座標をレコードxyにします
-spec coord(float(), float()) -> #{}.
coord(X, Y) -> #{x => X, y => Y}.
%%% 大きさゼロの箱を返します
-spec initbox() -> #{}.
initbox() ->
    #{size => #{x => 0.0, y => 0.0},         
      color => undefined,
      coord => #{x => 0.0, y => 0.0}}.
%%% すでに作った箱の色を再設定した箱を返します
-spec setcolor(#{}, atom()) -> #{}.
%%% マップの要素更新のときはmaps:update/3を使います
setcolor(Box, Color) -> maps:update(color, Color, Box).
%%% すでに作った箱の大きさを変えた箱を返します
%%% （大きさのチェックをしていないことに注意）
-spec setsize(#{}, #{}) -> #{}.
setsize(Box, Size) -> Box#{size := Size}.
%%% すでに作った箱の位置を変えた箱を返します
-spec setcoord(#{}, #{}) -> #{}.
setcoord(Box, Coord) -> Box#{coord := Coord}.
%%% 箱の色を返します
-spec getcolor(#{}) -> atom().
%%% マップの要素取得のときはmaps:get/2を使います
getcolor(Box) -> maps:get(color, Box).
%%% 箱の左下の座標を返します
-spec getcoord(#{}) -> #{}.
getcoord(Box) -> maps:get(coord, Box).
%%% 箱の面積を求めます
-spec area(#{}) -> float().
area(Box) ->
    % ネストしたレコードの中から値を取り出します
    Size = maps:get(size, Box),
    maps:get(x, Size) * maps:get(y, Size).
