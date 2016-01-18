%%% TinyMT（32ビット版）の疑似乱数生成パラメータを定義するレコードです
%%% 32bit符号なし整数の型です
-type uint32() :: 0..16#ffffffff.
%%% レコードを定義します
-record(tinymt32param, {
    %%% 特性多項式を示す128ビットの一意な数です
    characteristic :: 0..16#ffffffffffffffffffffffffffffffff,
    %%% mat1, mat2, tmat の組で疑似乱数生成が一意にできます
    mat1 :: uint32(),
    mat2 :: uint32(),
    tmat :: uint32(),
    %%% これらは特性多項式の性質を示す数です
    weight :: 0..127,
    delta :: 0..31
    }).
