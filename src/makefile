# If the first argument is "run"...
ifeq (run,$(firstword $(MAKECMDGOALS)))
  # use the rest as arguments for "run"
  RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  # ...and turn them into do-nothing targets
  $(eval $(RUN_ARGS):;@:)
endif

main: clean happy
	ghc -O2 Sapphire.hs

alex:
	alex -g Lexer.x

happy: alex
	happy -g Parser.y

# Can be used as 'make run ../examples/sapphire/ejemplo.sp'
run:
	./Sapphire $(RUN_ARGS)

ghci:
	@ghci Sapphire

# To set a specific file to run always
file:
	./Sapphire ../examples/sapphire/ejemplo.sp

clean:
	@rm *.hi *.o 2> /dev/null || true
	@rm Lexer.hs 2> /dev/null || true
	@rm Parser.hs 2> /dev/null || true
	@rm Sapphire 2> /dev/null || true
