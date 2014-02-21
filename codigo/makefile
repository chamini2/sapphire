main: clean lexer

lexer:
	ghc Lexer.hs

run: main
	./Lexer $(file)

ghci: main
	ghci Lexer

file: main
	./Lexer ../ejemplos/sapphire/ejemplo.sp

clean:
	@rm Lexer.hs || true
	@rm *.hi *.o || true
	@rm Lexer || true
	alex -g Lexer.x
