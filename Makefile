.PHONY: run build ghcid

run:
	@stack install

build:
	@stack build

clean:
	@stack clean
	@rm -r .stack-work

ghci:
	@stack ghci --ghci-options=-fobject-code

ghcid:
	@ghcid -c "stack ghci --ghci-options=-fobject-code"
