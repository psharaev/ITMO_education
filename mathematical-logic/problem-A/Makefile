
.PHONY: all happyAlex compile run clean

all: happyAlex compile

happyAlex:
	alex Parser/Lexer.x
	happy Parser/Syntax.y

compile: happyAlex
	ghc Main.hs

clean:
	find . -name '*.o' -delete
	find . -name '*.hi' -delete
	rm -f Parser/Lexer.hs
	rm -f Parser/Syntax.hs
	rm -f inp.txt
	rm -f out.txt

test: compile
	python3 /Users/pechhenka/Desktop/mathlog-lab/test/testA.py

run:
	./Main
