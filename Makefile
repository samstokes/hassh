.PHONY: touchUp

testConfigParser: touchUp
	ghc -Wall -fno-warn-orphans -Werror --make $@

touchUp:
	find . -name '*.hs' -exec touch \{\} \;
