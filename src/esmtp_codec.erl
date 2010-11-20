%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc SMTP encoding/decoding.
%% @end
%%%-------------------------------------------------------------------
-module(esmtp_codec).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([decode/1
         ,encode/1
         ,encode_auth/2]).

%%====================================================================
%% API
%%====================================================================

decode(<<"HELO ", Host/binary>>) ->
    {helo, Host};
decode(<<"EHLO ", Host/binary>>) ->
    {ehlo, Host};
decode(<<"MAIL FROM: ", Address/binary>>) ->
    {mail_from, Address};
decode(<<"MAIL FROM:", Address/binary>>) ->
    {mail_from, Address};
decode(<<"RCPT TO: ", Address/binary>>) -> 
    {rcpt_to, Address};
decode(<<"RCPT TO:", Address/binary>>) -> 
    {rcpt_to, Address};
decode(<<"DATA">>) -> 
    data;
decode(<<"STARTTLS">>) -> 
    starttls;
decode(<<".">>) -> 
    data_end;
decode(<<"QUIT">>) -> 
    quit;
decode(<<C1, C2, C3, Sep, Message/binary>>)
  when $0 =< C1, C1 =< $9,
       $0 =< C2, C2 =< $9,
       $0 =< C3, C3 =< $9,
       (Sep =:= $- orelse Sep =:= $\s) ->
    {list_to_integer([C1, C2, C3]),
     case Sep of
         $- -> more;
         $\s -> last
     end,
     Message};
decode(<<C1, C2, C3>>)
  when $0 =< C1, C1 =< $9,
       $0 =< C2, C2 =< $9,
       $0 =< C3, C3 =< $9 ->
    {list_to_integer([C1, C2, C3]), last, <<>>};
decode(<<$., Line/binary>>) ->
    {raw, Line};
decode(Line) ->
    {raw, Line}.

encode({helo, Host}) ->
    [<<"HELO ">>, Host];
encode({ehlo, Host}) ->
    [<<"EHLO ">>, Host];
encode({mail_from, Host}) ->
    [<<"MAIL FROM: ">>, Host];
encode({rcpt_to, Address}) ->
    [<<"RCPT TO: ">>, Address];
encode({auth, Msg}) ->
    [<<"AUTH ">>, Msg];
encode(starttls) ->
    [<<"STARTTLS">>];
encode(data) ->
    [<<"DATA">>];
encode(data_end) ->
    [<<".">>];
encode(quit) ->
    [<<"QUIT">>];
encode({Code, more, Message}) when is_integer(Code) ->
    [integer_to_list(Code), $-, Message];
encode({Code, last, <<>>}) when is_integer(Code) ->
    integer_to_list(Code);
encode({Code, last, Message}) when is_integer(Code) ->
    [integer_to_list(Code), $\s, Message];
encode({auth_plain, Username, Password}) ->
    [encode_auth(Username, Password)];
encode({raw, <<$., Line/binary>>}) ->
    ["..", Line];
encode({raw, Line}) ->
    Line.

encode_auth(Username, Password) ->
    AuthString = iolist_to_binary([0, Username, 0, Password]),
    base64:encode(AuthString).

%%====================================================================
%% Testing
%%====================================================================

roundtrip_test_() ->
    SMTP_Strings =[<<"HELO foo.com">>
                   ,<<"EHLO foo.com">>
                   ,<<"MAIL FROM: <foo@foo.com>">>
                   ,<<"RCPT TO: <bob@localhost.com>">>
                   ,<<"DATA">>
                   ,<<"SOME Message text">>
                   ,<<".">>
                   ,<<"QUIT">>
                   ,<<"220 smtp.example.com ESMTP Postfix">>
                   ,<<"250 Hello relay.example.org, I am glad to meet you">>
                   ,<<"250 Ok">>
                   ,<<"354 End data with <CR><LF>.<CR><LF>">>
                   ,<<"250 Ok: queued as 12345">>
                   ,<<"221 Bye">>
                   ,<<"334">>
                   ,<<"334 Go ahead">>
                   ,<<"STARTTLS">>
                   ],
    [ ?_assertMatch(S when S =:= String,
                           iolist_to_binary(encode(decode(String))))
      || String <- SMTP_Strings].
