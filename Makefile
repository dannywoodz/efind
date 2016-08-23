PROJECT=efind
PLTNAME=$(PROJECT).plt
REBAR=`which rebar || ./rebar`
DEPS=$(shell ./epaths deps | sed -e 's/ / -pa /g' -e 's/^/-pa /') -pa ../$(PROJECT)/ebin
REPOSITORY=https://github.com/dannywoodz/efind

all: deps compile test check 
deps:
	@$(REBAR) get-deps
compile:
	@$(REBAR) -C rebar.config compile
test:
ifeq ($(tests),)
	@$(REBAR) -C test.config skip_deps=true eunit
else
	@$(REBAR) -C test.config skip_deps=true eunit tests=$(tests)
endif

clean:
	@$(REBAR) clean
	@-rm -rf docs

$(PLTNAME):
	dialyzer --build_plt $(shell ./epaths) --output_plt $(PLTNAME)

update_plt: $(PLTNAME)
	dialyzer --add_to_plt $(shell ./epaths) --plt $(PLTNAME)

check: $(PLTNAME) update_plt
	dialyzer --plt $(PLTNAME) $(DEPS) ebin/*.beam

docs:
	erl $(DEPS) -noshell -run edoc_run application $(PROJECT) '[{doclet, edown_doclet},{top_level_readme, {"./README.md","$(REPOSITORY)"}},{edown_target, github}]'

.PHONY: clean check test compile deps all update_plt docs
