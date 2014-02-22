main: clean alex happy
	ghc Main

alex:
	alex -g Lexer.x

happy:
	happy Parser.y

run: main
	./Main $(file)

ghci: main
	ghci Main

file: main
	./Main ../ejemplos/sapphire/ejemplo.sp

clean:
	@rm *.hi *.o || true
	@rm Lexer.hs || true
	@rm Parser.hs || true
	@rm Main || true
