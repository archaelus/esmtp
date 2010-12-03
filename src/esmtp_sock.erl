%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc ESMTP SMTP Socket
%% @end
%%%-------------------------------------------------------------------
-module(esmtp_sock).

%% API
-export([connect/3
         ,read_response/1
         ,read_response_all/1
         ,command/2
         ,send/2
         ,send_data/2
         ,close/1
        ]).

-record(esmtp_sock, {sock, type=gen_tcp}).
-define(TCP_OPTS, [binary,
                   {packet, line},
                   {active, false}]).


%%====================================================================
%% API
%%====================================================================

connect(Host, Port, Type) ->
    case Type:connect(Host, Port,
                      ?TCP_OPTS) of
        {ok, Sock} ->
            {ok, #esmtp_sock{sock=Sock,
                             type=Type}};
        Err -> Err
    end.

read_response(S = #esmtp_sock{sock=Sock,
                              type=Type}) ->
    case Type:recv(Sock, 0) of
        {ok, Line} ->
            {ok, S, esmtp_codec:decode(Line)};
        {error, Reason} ->
            {error, S, Reason}
    end.

read_response_all(S) ->
    case read_response(S) of
        {ok, S1, {_, more, _}} ->
            read_response_all(S1);
        {ok, _, {_, last, _}} = FinalResponse ->
            FinalResponse;
        {error, _} = E -> E
    end.


command(S = #esmtp_sock{},
        Command) when is_tuple(Command) ->
    {ok, S1} = send(S, [esmtp_codec:encode(Command), $\r, $\n]),
    read_response_all(S1);
command(S = #esmtp_sock{},
        Command) when is_atom(Command) ->
    {ok, S1} = send(S, [esmtp_codec:encode(Command), $\r, $\n]),
    read_response_all(S1);
command(S = #esmtp_sock{},
        Command) ->
    {ok, S1} = send(S, Command),
    read_response_all(S1).

send(S = #esmtp_sock{sock=Sock,
                     type=Type},
    Data) ->
    case Type:send(Sock, Data) of
        ok ->
            {ok, S};
        {error, Reason} ->
            {error, S, Reason}
    end.

send_data(S = #esmtp_sock{}, Data) ->
    {ok, S1, {354, last, _}} = command(S, data),
    {ok, S2} = send(S1, [Data, "\r\n"]),
    command(S2, data_end).

close(#esmtp_sock{sock=Sock,
                  type=Type}) ->
    Type:close(Sock).
