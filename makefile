all:
	erlc *erl
	erl -eval "lexgrm:start(), halt()" -noshell -detached

run: all
	erl

clean:
	rm -f *beam grm.erl lex.erl

debug:
	erlc +debug_info *erl 
	erl -eval "lexgrm:start(), halt()" -noshell -detached
