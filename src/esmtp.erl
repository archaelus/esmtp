%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc ESMTP api module
%% @end
%%%-------------------------------------------------------------------
-module(esmtp).

-include("../include/esmtp_mime.hrl").

%% API
-export([send/1
         ,send/2
         ,send/3
         ,send/5
         ,mailq/0]).

%%====================================================================
%% API
%%====================================================================

send(Msg= #mime_msg{}) ->
    send(esmtp_mime:from(Msg),
         esmtp_mime:to(Msg),
         esmtp_mime:encode(Msg)).

send(To, Msg) ->
    send(undefined, To, Msg).

send(undefined, To, Msg) ->
    From = esmtp_app:config(default_from),
    send(From, To, Msg);
send(From, To, Message) ->
    MX = esmtp_app:config(smarthost),
    Ehlo = esmtp_app:config(default_ehlo),
    send(MX, Ehlo, From, To, Message).

send(MX, Ehlo, From, To, Msg) ->
    esmtp_client:send(MX, Ehlo, From, To, Msg).

mailq() ->
    supervisor:which_children(esmtp_sup).

%%====================================================================
%% Internal functions
%%====================================================================

