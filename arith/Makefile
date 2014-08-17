parser: parser.y
	happy parser.y

f: parser main.hs types.hs
	ghc --make main.hs parser.hs types.hs

test: all
	./main test.f

clean:
	rm *.hi *.o
	rm parser.hs
	rm main

all: parser f
