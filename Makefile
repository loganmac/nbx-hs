run:
	@stack install

autocomplete:
	nbx --zsh-completion-script /Users/logan/.local/bin/nbx > _nbx
	mv _nbx ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions