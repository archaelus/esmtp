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

-export([config/1]).

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
start(_Type, StartArgs) ->
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
config(Item) ->
    case application:get_env(esmtp, Item) of
        {ok, Term} -> Term;
        undefined ->
            error_logger:error_msg("esmtp not correctly configured: missing ~p",
                                   [Item]),
            exit(esmtp_misconfigured)
    end.
