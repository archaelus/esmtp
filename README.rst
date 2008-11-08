=======================================
esmtp - A simple SMTP client for Erlang
=======================================

esmtp is a simple OTP application providing a way to send emails (and
attachments) from erlang systems.

Configuration
=============

The esmtp application is configured with OTP application configuration
env variables.

smarthost
  This is a tuple giving the hostname and port of the smtp server to
  send mail via. This will usually be a local smtp server on port 25.
default_from
  This is the default From address to use on outgoing mail if a From
  address is not supplied.

Example
-------

system.config::

  [{esmtp, [{smarthost, {"localhost", 25}}
           ,{default_from, "Erlang/OTP <erlang@localhost>"}]}].


