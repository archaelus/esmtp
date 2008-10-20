%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
%%%-------------------------------------------------------------------
-module(esmtp_client).

%% API
-export([send/4
         ,start_link/4
         ,sendemail/4]).

%%====================================================================
%% API
%%====================================================================

send(MX, From, To, Msg) ->
    supervisor:start_child(esmtp_sup, [MX, From, To, Msg]).

start_link(MX = {_Host, _Port},
           From, To, Msg) when is_list(From), is_list(To) ->
    proc_lib:start_link(?MODULE, sendemail, [MX, From, To, Msg]).

sendemail({Host, Port}, From, To, Msg) ->
    proc_lib:init_ack({ok, self()}),
    {ok, Fsm} = esmtp_fsm:start_link(Host, Port),
    {ok, _} = esmtp_fsm:ehlo(Fsm),
    {ok, _} = esmtp_fsm:mail_from(Fsm, From),
    {ok, _} = esmtp_fsm:rcpt_to(Fsm,To),
    {ok, _} = esmtp_fsm:message(Fsm,Msg),
    {ok, _} = esmtp_fsm:close(Fsm).
        

%%====================================================================
%% Internal functions
%%====================================================================
