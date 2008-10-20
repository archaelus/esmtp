VSN          := 0.1
ERL          ?= erl
EBIN_DIRS    := $(wildcard lib/*/ebin)
APP          := esmtp

all: erl ebin/$(APP).app

erl: ebin lib
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B \
	  -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

clean: 
	@echo "removing:"
	@rm -fv ebin/*.beam ebin/*.app

ebin/$(APP).app: src/$(APP).app
	@cp -v src/$(APP).app $@

ebin:
	@mkdir ebin

lib:
	@mkdir lib

dialyzer: erl
	@dialyzer -c ebin
