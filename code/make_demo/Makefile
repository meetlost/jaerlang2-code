.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean

MODS = a

all: compile

compile: ${MODS:%=%.beam}

test: compile
	erl -noshell -s a test -s init stop

clean:	
	rm -rf *.beam erl_crash.dump
