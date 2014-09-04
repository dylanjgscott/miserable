LEXER=alex
HC=ghc

all: lexer.hs

lexer.hs: miserable.x
	$(LEXER) -o $@ $<

clean:
	rm lexer.hs
