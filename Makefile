TARGET=src/exceml.bc.js

.PHONY: all clean

all:
	@dune build $(TARGET) --profile release

test:
	@dune exec src/test_tableur.exe -f

clean:
	@dune clean
