ERL			?= erl
ERL			= erlc
EBIN_DIRS		:= $(wildcard deps/*/ebin)

PRIV                    = priv

.PHONY: rel deps

all: deps compile

compile: deps
	@./rebar compile

deps:
	@./rebar get-deps
	@./rebar check-deps

clean:
	@./rebar clean

realclean: clean
	@./rebar delete-deps

test:
	@./rebar skip_deps=true ct

eunit:
	@./rebar skip_deps=true eunit

rel: deps
	@./rebar compile generate

doc:
	@./rebar skip_deps=true doc

console:
	@erl -pa deps/*/ebin deps/*/include ebin include -boot start_sasl  -sname esmtp

analyze: checkplt
	@./rebar skip_deps=true dialyze

buildplt:
	@./rebar skip_deps=true build-plt

checkplt: buildplt
	@./rebar skip_deps=true check-plt

xref:
	@./rebar skip_deps=true xref
                
                
$(PRIV)/docroot/index.html:
	cd java_src && 	mvn install
	cp -r java_src/target/umts/css $(PRIV)/docroot
	cp -r java_src/target/umts/img $(PRIV)/docroot
	cp -r java_src/target/umts/net.caixagaliciamoviles.gumts.GUMTS $(PRIV)/docroot
	cp -r java_src/target/umts/index.html $(PRIV)/docroot
	cp -r java_src/target/umts/res $(PRIV)/docroot
	cp -r  java_src/target/umts/favicon.ico  $(PRIV)/docroot	
java_clean:
	cd java_src &&  mvn clean
	rm -rf $(PRIV)/docroot/css
	rm -rf $(PRIV)/docroot/img
	rm -rf $(PRIV)/docroot/net.caixagaliciamoviles.gumts.GUMTS
	rm -rf $(PRIV)/docroot/res
	rm -rf $(PRIV)/docroot/index.html
	rm -rf $(PRIV)/docroot/favicon.ico

