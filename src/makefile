main: clean alex happy
	ghc -O2 Sapphire.hs

alex:
	alex -g Lexer.x

happy:
	happy -g -s Parser.y

run:
	./Sapphire $(file)

ghci:
	ghci Sapphire

file:
	./Sapphire ../ejemplos/sapphire/ejemplo.sp

clean:
	@rm *.hi *.o || true
	@rm Lexer.hs || true
	@rm Parser.hs || true
	@rm Sapphire || true
