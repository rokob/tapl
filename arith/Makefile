parser: Parser.y
	happy Parser.y

f: parser main.hs Types.hs Tokens.hs Lexer.hs ParseMonad.hs
	ghc --make main.hs Parser.hs Types.hs Lexer.hs Tokens.hs ParseMonad.hs

test: all
	./main test.f

clean:
	rm *.hi *.o
	rm Parser.hs
	rm main

all: parser f
