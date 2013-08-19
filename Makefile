SERVER := erl -pa apps/packet_inspector/ebin -pa deps/*/ebin -smp enable -s lager -s packet_inspector -setcookie QWJCDLYIBTECKABSLLNC -config rel/files/app.config ${ERL_ARGS}

all:
	rebar get-deps && rebar compile

erl:
	rebar compile

clean:
	rebar clean

clean_logs:
	rm -rf log*

build_plt: erl
	dialyzer --verbose --build_plt --apps kernel stdlib erts compiler hipe crypto \
		edoc gs syntax_tools --output_plt ~/.packet_inspector.plt -pa deps/*/ebin ebin

analyze: erl
	dialyzer --verbose -pa deps/*/ebin --plt ~/.packet_inspector.plt -Werror_handling ebin

xref: all
	rebar skip_deps=true xref

shell:
	if [ -n "${NODE}" ]; then ${SERVER} -name ${NODE}@`hostname` -boot start_sasl; \
	else ${SERVER} -name packet_inspector@`hostname` -boot start_sasl; \
	fi

run: erl
	if [ -n "${NODE}" ]; then ${SERVER} -name ${NODE}@`hostname` -boot start_sasl -s packet_inspector; \
	else ${SERVER} -name packet_inspector@`hostname` -boot start_sasl -s packet_inspector; \
	fi

test: erl
	mkdir -p log/ct
	rebar skip_deps=true ct
	open log/ct/index.html

doc: erl
	rebar skip_deps=true doc
