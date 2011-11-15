PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar

all: log
	@$(REBAR) get-deps compile

edoc:
	@$(REBAR) doc

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean
	@rm -rf log/sasl/*

build_plt:
	@$(REBAR) build-plt

dialyzer:
	@$(REBAR) dialyze

app: log
	@$(REBAR) create template=mochiwebapp dest=$(DEST) appid=$(PROJECT)

log:
	@mkdir -p log/sasl
