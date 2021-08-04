index.html: src/Main.elm
	elm-format src/Main.elm --yes
	elm make src/Main.elm
