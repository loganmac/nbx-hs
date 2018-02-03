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

autocomplete:
	nbx --zsh-completion-script /Users/logan/.local/bin/nbx > _nbx
	mv _nbx ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions

