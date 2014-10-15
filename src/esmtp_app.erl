%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc ESMTP application module
%% @end
%%%-------------------------------------------------------------------
-module(esmtp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([config/1, start_ssl/0, need_ssl/1]).

-define(SMTP_PORT_TLS, 587).
-define(SMTP_PORT_SSL, 465).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, _StartArgs) ->
    case need_ssl() of
        true -> start_ssl();
        false -> ok
    end,
    esmtp_sup:start_link().

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored.
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @spec config(Item::atom()) -> term()
%% @doc Retrieve the configuration value for key Item from the tbld
%% OTP application environment.
config(login) ->
    case application:get_env(esmtp, login) of
        {ok, Term} -> Term;
        undefined -> no_login
    end;
config(Item) ->
    case application:get_env(esmtp, Item) of
        {ok, Term} -> Term;
        undefined ->
            error_logger:error_msg("esmtp not correctly configured: missing ~p",
                                   [Item]),
            exit(esmtp_misconfigured)
    end.

need_ssl() ->
    {_Host, Port} =  config(smarthost),
    need_ssl(Port).

need_ssl(?SMTP_PORT_TLS) -> true;
need_ssl(?SMTP_PORT_SSL) -> true;
need_ssl(_) -> false.

ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok;
        Err -> Err
    end.

start_ssl() ->
    ok = ensure_started(crypto),
    ok = ensure_started(asn1),
    ok = ensure_started(public_key),
    ok = ensure_started(ssl).
