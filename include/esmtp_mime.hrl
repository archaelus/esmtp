%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Mail Mime library headers and record definitions.
%% @end

-ifndef(esmtp_mime).
-define(esmtp_mime, true).

-record(mime_msg, {headers = [], boundary, parts = []}).
-record(mime_part, {type,
                    encoding = {"7bit", "text/plain","iso-8859-1"},
                    name,
                    data,
                    content_id = undefined}).

-endif.
