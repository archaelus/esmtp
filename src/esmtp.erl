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
-export([send/2, send/3]).

%%====================================================================
%% API
%%====================================================================

send(To, Msg) ->
    send(esmtp_app:config(default_from), To, Msg).

send(From, To, Msg = #mime_msg{}) ->
    Data = mail_mime:encode(Msg),
    send(From, To, Msg);
send(From, To, Message) ->
    MX = esmtp_app:config(smarthost),
    supervisor:start_child(esmtp_sup, [MX, From, To, Message]).

%%====================================================================
%% Internal functions
%%====================================================================

