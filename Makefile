EBINS=-pa $(realpath .)/ebin


compile:
	@rebar compile

clean:
	@rebar clean

run: compile
	erl ${EBINS} -noinput -s erlang_mpm_app

debug: compile
	erl ${EBINS}  -config config -s erlang_mpm_app
