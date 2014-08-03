SERVER := erl -pa apps/inspector/ebin -pa deps/*/ebin -smp enable -s lager -s inspector -setcookie QWJCDLYIBTECKABSLLNC -config rel/files/app.config ${ERL_ARGS}

all:
	rebar get-deps && rebar compile

erl:
	rebar compile

clean:
	rebar clean

clean_logs:
	rm -rf log*

DEPSOLVER_PLT=$(CURDIR)/.depsolver_plt

.PHONY: dialyzer typer clean distclean

$(DEPSOLVER_PLT):
	dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt \
		--apps erts kernel stdlib erts compiler stntax_tools crypto public_key -r deps

# -Wunderspecs \

dialyzer: $(DEPSOLVER_PLT)
	dialyzer --verbose --plt $(DEPSOLVER_PLT) -I apps/inspector/include \
		-Werror_handling -Wrace_conditions -Wno_undefined_callbacks \
		apps/inspector/ebin | \
		fgrep -v -f ./dialyzer.ignore-warnings

typer: $(DEPSOLVER_PLT)
	typer --plt $(DEPSOLVER_PLT) -I apps/inspector/include -r apps/inspector/src -pa deps/lager/ebin


xref: all
	rebar skip_deps=true xref

shell:
	if [ -n "${NODE}" ]; then ${SERVER} -name ${NODE}@`hostname` -boot start_sasl; \
	else ${SERVER} -name inspector@`hostname` -boot start_sasl; \
	fi

run: erl
	if [ -n "${NODE}" ]; then ${SERVER} -name ${NODE}@`hostname` -boot start_sasl -s inspector; \
	else ${SERVER} -name inspector@`hostname` -boot start_sasl -s inspector; \
	fi

test: erl
	mkdir -p log/ct
	rebar skip_deps=true ct
	open log/ct/index.html

doc: erl
	rebar skip_deps=true doc
