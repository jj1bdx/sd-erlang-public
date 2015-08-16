%%% EUnitによるテストケースのモジュールです
-module(sd_ep06_eunit).
%%% -ifdef(マクロ) ... -endif. の間は
%%% マクロが定義されている場合のみ
%%% コンパイルされます
%%% EUnitではTESTというマクロを定義しています
-ifdef(TEST).
%%% EUnitに必要な定義ファイルをインクルードします
-include_lib("eunit/include/eunit.hrl").
-endif.
%%% テストコードです
-ifdef(TEST).
%%% 最後が"_test_"で終わっている関数は
%%% テストを生成する役割をします
counter_test_() ->
%%% このsetupで始まるタプルで次の定義をします
%%% * テスト全体はcounter_check/1で返されるテストセットで実行
%%% テスト実行前にcounter_start_link/0を実行
%%% テスト終了後はcounter_stop/1を実行
    {setup,
     fun counter_start_link/0,
     fun counter_stop/1,
     fun counter_check/1
    }.
%%% カウンタのgen_serverを起動してPidを返します
counter_start_link() ->
    {ok, Pid} = msgcounter_gen_server:start_link(),
    Pid.
%%% Pidで示されたカウンタを停止します
counter_stop(Pid) ->
    msgcounter_gen_server:stop(Pid).
%%% Pidで示したカウンタに対するテストセットを返します
counter_check(Pid) ->
    [
    %%% 初期値が0であるかどうかのテストを返します
    ?_assertEqual(0, msgcounter_gen_server:val(Pid)),
    %%% ひとつ増やしたら1になるかどうかのテストを返します
    ?_assertEqual(1, msgcounter_gen_server:inc(Pid)),
    ?_assertEqual(2, msgcounter_gen_server:inc(Pid)),
    %%% ゼロに戻す関数の戻り値がokであるかどうかのテストを返します
    ?_assertEqual(ok, msgcounter_gen_server:zero(Pid)),
    ?_assertEqual(0, msgcounter_gen_server:val(Pid)),
    ?_assertEqual(1, msgcounter_gen_server:inc(Pid)),
    ?_assertEqual(0, msgcounter_gen_server:dec(Pid)),
    ?_assertEqual(-1, msgcounter_gen_server:dec(Pid)),
    ?_assertEqual(0, msgcounter_gen_server:inc(Pid)),
    ?_assertEqual(0, msgcounter_gen_server:val(Pid))
    %% ここまでで10個のテストを定義したことになります
    ].
%%% ここまででテストケースのモジュールは終了です
-endif.
