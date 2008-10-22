%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Simple one-shot client using esmtp_fsm.
%% @end
%%%-------------------------------------------------------------------
-module(esmtp_client).

%% API
-export([send/5
         ,start_link/5
         ,init/5
         ,sendemail/5]).

%%====================================================================
%% API
%%====================================================================

send(MX={_Host,Port}, Ehlo, From, To, Msg) when is_list(From), is_list(To),
                                               is_integer(Port) ->
    supervisor:start_child(esmtp_sup, [MX, Ehlo, From, To, Msg]).

start_link({Host,Port}, Ehlo, From, To, Msg) ->
    proc_lib:start_link(?MODULE, init, [{Host,Port}, Ehlo, From, To, Msg]).

%%====================================================================
%% Internal functions
%%====================================================================

init({Host,Port},Ehlo,From,To,Msg) ->
    proc_lib:init_ack({ok, self()}),
    sendemail({Host,Port},Ehlo,From,To,Msg).

sendemail({Host,Port},Ehlo,From,To,Msg) ->
    {ok, Fsm} = esmtp_fsm:start_link(Host, Port),
    {ok, _} = esmtp_fsm:ehlo(Fsm, Ehlo),
    {ok, _} = esmtp_fsm:mail_from(Fsm, From),
    {ok, _} = esmtp_fsm:rcpt_to(Fsm,To),
    {ok, _} = esmtp_fsm:message(Fsm,Msg),
    ok = esmtp_fsm:close(Fsm).
