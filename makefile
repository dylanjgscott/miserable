LEXER=alex

lexer.hs: miserable.x
	$(LEXER) -o $@ $<
