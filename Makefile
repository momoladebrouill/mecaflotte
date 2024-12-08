
all:
	@dune build --profile release 
	@mv ./_build/default/src/main.exe ./bin/main.exe -f
	@rm -rf _build
	@./bin/main.exe

