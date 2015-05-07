PROJECT=fast_disk_log
REBAR=./rebar

all: compile

build-plt: all
	@dialyzer --build_plt --output_plt ~/.$(PROJECT).plt \
		--apps erts kernel stdlib crypto public_key ssl

check-plt:
	@dialyzer --check_plt --plt ~/.$(PROJECT).plt

clean:
	@echo "Running rebar clean..."
	@$(REBAR) clean

compile:
	@echo "Running rebar compile..."
	@$(REBAR) compile

dialyze:
	@dialyzer ebin/*.beam --plt ~/.$(PROJECT).plt -I include

xref:
	@$(REBAR) skip_deps=true xref

.PHONY: dialyze eunit xref
