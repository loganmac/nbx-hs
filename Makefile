.PHONY: run ghcid

run:
	@stack install

ghcid:
	@ghcid -c "stack ghci --ghci-options=-fno-code"
