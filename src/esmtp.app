{application, esmtp,
 [{description, "Erlang SMTP client"}
  ,{vsn, "0.1"}
  ,{applications, [kernel, stdlib]}
  ,{modules, [esmtp
              ,esmtp_app
              ,esmtp_client
              ,esmtp_mime
              ,esmtp_sup]}
  ,{mod, {esmtp_app, []}}
  ,{env, [{smarthost, {"localhost", 25}}
          ,{default_ehlo, "localhost"}
          ,{default_from, "Erlang/OTP <erlang@localhost>"}]}
  ,{registered, [esmtp_sup]}
 ]}.
