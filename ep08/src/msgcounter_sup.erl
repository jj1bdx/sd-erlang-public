%%% スーパバイザの起動用モジュールです
-module(msgcounter_sup).
-behaviour(supervisor).
-export([start/0,
         start_from_shell/0,
         start_link/0,
         start_link/1,
         init/1]).
%%% ?MODULE は自分のモジュール名です
%%% （ここではmsgcounter_sup）
%%% ローカルノードでスーパバイザを
%%% 別プロセスとしてspawnして起動します
-spec start() -> pid().
start() ->
    spawn(fun() ->
              supervisor:start_link(
              {local, ?MODULE}, ?MODULE, [])
          end).
%%% シェルから起動するときのためにリンクを切っておきます
-spec start_from_shell() -> true.
start_from_shell() ->
    {ok, Pid} = supervisor:start_link(
                  {local, ?MODULE}, ?MODULE, []),
    unlink(Pid).
%%% 関数supervisor:start_link/3への橋渡しをします
-spec start_link(list()) -> {ok, pid()}.
start_link(Args) ->
     supervisor:start_link(
         {local, ?MODULE}, ?MODULE, Args).
%%% 上記関数で引数がない場合の定義をします    
-spec start_link() -> {ok, pid()}.
start_link() ->
     start_link([]).
%%% スーパバイザの初期化をします
-spec init([]) -> {ok, {term(), term()}}.
init([]) ->
    % ここではスーパバイザ配下のプロセスが異常終了したときに
    % どのような動作をするかを指定します
    SupFlags =
        #{strategy => one_for_one,
          intensity => 3,
          period => 10},    
    % ここでは配下のプロセスをどのように起動するかを指定します
    % gen_serverなどであればそのまま起動できます
    % このスーパバイザでは
    % msgcounter_gen_serverのプロセスを4つ
    % 起動するように設定します
    ChildSpecs = [
        #{id => counter1,
          start => {msgcounter_gen_server, start_link, []},
          shutdown => brutal_kill},
        #{id => counter2,
          start => {msgcounter_gen_server, start_link, []},
          shutdown => brutal_kill},
        #{id => counter3,
          start => {msgcounter_gen_server, start_link, []},
          shutdown => brutal_kill},
        #{id => counter4,
          start => {msgcounter_gen_server, start_link, []},
          shutdown => brutal_kill}
        ],
    % 返り値として初期設定を返します
    {ok, {SupFlags, ChildSpecs}}.
