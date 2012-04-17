BIN=tictactoe

# Make using installed package
build::
	ghc --make Main.hs -o $(BIN)

# execute
run::
	./$(BIN)

# Make using source
build-inplace::
	ghc --make Main.hs -i.:../..:../../dist/build/autogen/ -o $(BIN)-inplace

# execute inplace
run-inplace::
	export blank_canvas_datadir=../.. ; ./$(BIN)-inplace

