%%% プロセス辞書を使ったカウンタです
%%% 関数名は連載第3回のmsgcounterモジュールに準じます
-module(msgcounter_procdict).
-export([start/1, stop/1, inc/1, dec/1, zero/1, val/1]).
%%% 辞書内のカウンタのキーは{?MODULE, カウンタの名前}とします
%%% ?MODULEはモジュール名(msgcounter_procdict)を示すマクロです
%%% 名前を引数に取るカウンタを使えるようにします
-spec start(term()) -> term().
start(Name) ->
    put({?MODULE, Name}, 0),
    Name.
%%% 名前を引数に取るカウンタを消します
-spec stop(term()) -> ok.
stop(Name) -> 
    erase({?MODULE, Name}),
    ok.
%%% 名前で指定したカウンタの値を1つ増やします
-spec inc(term()) -> integer().
inc(Name) ->
    New = get({?MODULE, Name}) + 1,
    put({?MODULE, Name}, New),
    % put/2の返り値は変更「前」の値ですので
    % 明示的に変更後の値を返すようにします
    New.
%%% 名前で指定したカウンタの値を1つ減らします
-spec dec(term()) -> integer().
dec(Name) ->
    New = get({?MODULE, Name}) - 1,
    put({?MODULE, Name}, New),
    New.
%%% 名前で指定したカウンタの値をゼロ(0)にします
-spec zero(term()) -> 0.
zero(Name) ->
    put({?MODULE, Name}, 0),
    0.
%%% 名前で指定したカウンタの値を返します
-spec val(pid()) -> integer().
val(Name) -> get({?MODULE, Name}).
