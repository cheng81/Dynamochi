all:
	(cd src;$(MAKE))

clean:
	(cd src;$(MAKE) clean)

runshell: all
	erl -pa $(PWD)/ebin $(PWD)/deps/*/ebin -boot start_sasl