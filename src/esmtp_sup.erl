%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Mail client supervisor
%% @end
%%%-------------------------------------------------------------------
-module(esmtp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc: Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init
%% @spec (Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                 ignore                          |
%%                 {error, Reason}
%% @doc Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Clients = {"ESMTP Client",
               {esmtp_client,start_link,[]},
               temporary,2000,worker,
               [esmtp_client, esmtp_code, esmtp_sock]},
    {ok,{{simple_one_for_one,0,1}, [Clients]}}.

%%====================================================================
%% Internal functions
%%====================================================================
