all:
	rebar -C rebar.conf compile
shell:
	rebar -C rebar.conf shell
ct:
	rebar -C rebar.conf ct
clean:
	rebar -C rebar.conf clean
	rm -f db/nodes
	rm -f log/log
	rm -rf logs
	rm -f test/*.beam
