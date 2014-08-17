SOURCES=main.hs Types.hs Tokens.hs Lexer.hs ParseMonad.lhs

parser: Parser.y
	happy Parser.y

f: parser $(SOURCES)
	ghc --make $(SOURCES)

test: all
	./main test.f

clean:
	rm *.hi *.o
	rm Parser.hs
	rm main

all: parser f
