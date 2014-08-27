SOURCES=Lexer.hs Parser.hs main.hs

lexer: Lexer.x
	alex Lexer.x

parser: Parser.y
	happy Parser.y

f: lexer parser $(SOURCES)
	ghc --make $(SOURCES)

test: all
	./main test.f

clean:
	rm *.hi *.o
	rm Lexer.hs
	rm Parser.hs
	rm main

all: lexer parser f
