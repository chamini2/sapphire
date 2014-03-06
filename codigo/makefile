main: clean alex happy
	ghc -O2 -W Main

alex:
	alex -g Lexer.x

happy:
	happy -g -s Parser.y

run:
	./Main $(file)

ghci:
	ghci Main

file:
	./Main ../ejemplos/sapphire/ejemplo.sp

clean:
	@rm *.hi *.o || true
	@rm Lexer.hs || true
	@rm Parser.hs || true
	@rm Main || true
