%%% カウンタを状態に持つサーバをgen_serverで書いています
%%% 詳細は第3回のmsgcounter.erlを参照してください
-module(msgcounter_gen_server).
%%% gen_serverビヘイビアを使う旨宣言します
-behaviour(gen_server).
%%% gen_serverのためのコールバック関数もexportします
-export([start_link/0, init/1,
         inc/1, dec/1, zero/1, val/1, stop/1,
         handle_call/3, terminate/2,
         handle_cast/2, handle_info/2, code_change/3]).
%%% レコードの中にカウンタの内部状態を入れます
-record(state, {counter = 0}).
%%% リンク付きでサーバを起動します
%%% 成功すると {ok, Pid} で，pidがタプルの中に返ります
-spec start_link() -> {ok, pid()}.
start_link() ->
    % ?MODULE はこのモジュール自身の名前のアトムです
    gen_server:start_link(?MODULE, [], []).
%%% ここからはgen_server:call/2を通じて
%%% サーバにどんなメッセージを送るかを書くための関数です
%%% 内部状態のカウンタの値を1つ増やしてその後の値を返します
-spec inc(pid()) -> integer().
inc(Pid) ->
    gen_server:call(Pid, inc).
%%% 内部状態のカウンタの値を1つ減らしてその後の値を返します
-spec dec(pid()) -> integer().
dec(Pid) ->
    gen_server:call(Pid, dec).
%%% 内部状態のカウンタの値を1つゼロにして成功したらokを返します
-spec zero(pid()) -> ok.
zero(Pid) ->
    gen_server:call(Pid, zero).
%%% 内部状態のカウンタの値を返します
-spec val(pid()) -> integer().
val(Pid) ->
    gen_server:call(Pid, val).
%%% サーバを止めます
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:call(Pid, terminate).
%%% ここから先はビヘイビアからのコールバック関数の定義です
%%% gen_serverからのサーバ初期化作業のコールバック関数です
-spec init([]) -> {ok, #state{}}.
init([]) ->
    % カウンタの値をゼロに初期化しています
    {ok, #state{counter = 0}}.
%%% gen_server:call/2で同期メッセージを送った際に
%%% 返事を返すためのコールバック関数です
%%% この関数単独ですべてのメッセージをさばくため
%%% 各種要求はセミコロンで区切られた
%%% パターンマッチングの節として実装しています
-spec handle_call(term(), pid(), #state{}) -> term().
%%% 第1引数にはgen_server:call/2からのメッセージの内容
%%% 第2引数には呼出元のPidと識別用のタグの組み合わせ
%%% 第3引数には呼ばれる前の内部状態が与えられます
handle_call(inc, _From, #state{counter = Count}) ->
    % 返り値としてタプルを設定し次の動作を決めます
    % 第1要素は動作の指定(replyなら返答を返す)
    % 第2要素は返答の内容，
    % 第3要素は返答を返した後の内部状態を示します
    {reply, Count + 1, #state{counter = Count + 1}};
%%% 前の節ではinc/1の処理，ここからはdec/1の処理です
handle_call(dec, _From, #state{counter = Count}) ->
    {reply, Count - 1, #state{counter = Count - 1}};
%%% zero/1の処理です
handle_call(zero, _From, _S) ->
    {reply, ok, #state{counter = 0}};
%%% var/1の処理です
handle_call(val, _From, S = #state{counter = Count}) ->
    {reply, Count, S};
%%% stop/1の処理です
handle_call(terminate, _From, S) ->
    % タプルの第1要素がstopだとサーバのプロセスは停止して消滅します
    % 第2要素は停止理由（terminate/2で判断します）
    % 第3要素は返答内容，第4要素は内部状態を示します
    {stop, normal, ok, S}.
%%% これでhandle_call/3の処理は終了なので最後はピリオドで終わっています
%%% terminate/2はgen_serverから
%%% プロセス終了時に呼ばれるコールバック関数です
-spec terminate(normal, #state{}) -> ok.
%%% 第1引数は停止理由です（問題がなければokを返しておきます）
terminate(normal, _S) -> ok.
%%% ここから先の関数は本サンプルコードでは明示的には使いません
%%% handle_cast/2はcast/2で送られた非同期メッセージを処理します
-spec handle_cast(term(), #state{}) -> term().
handle_cast(_Msg, S) -> {noreply, S}.
%%% handle_info/2はcallやcast以外のメッセージを受けとった時の処理をします
-spec handle_info(term(), #state{}) -> term().
handle_info(_Info, S) -> {noreply, S}.
%%% code_change/3ではモジュールの動的なアップデートの際に
%%% サーバの内部状態を更新するかどうかを指定します
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, S, _Extra) -> {ok, S}.
