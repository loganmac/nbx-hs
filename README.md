# nbx

requirements:
logins - nanobox
docker building, tagging, pushing
odin - generate & send schema and post deploy
console, tunnel, logs

nbx push (docker)
for each service:

1. commit (creating a new image)
2. tag the image
3. push to the remote registry

nbx push (nbx)

1. docker push
2. post schema
3. POST /deploys

## Commands

*MVP

- nbx init
- nbx push    [remote]
- nbx logs    [remote] [service]
- nbx console [remote] [service]
- nbx tunnel  [remote] [service]
- nbx login   [remote]

## Bash, Zsh, and Fish Completions (so I don't have to find this later)

This tool has built-in support for the completion of
command line options and arguments in bash, zsh, and fish shells.
It is automatically extended with a few (hidden) options for the
completion system:

- `--bash-completion-script`: this takes the full path of the program as
   argument, and prints a bash script, which, when sourced into a bash session,
   will install the necessary machinery to make bash completion work. For a
   quick test, you can run something like (for a program called `foo` on the
   `PATH`):

   ```console
   source <(nbx --bash-completion-script `which nbx`)
   ```

   Normally, the output of `--bash-completion-script` should be shipped with
   the program and copied to the appropriate directory (usually
   `/etc/bash_completion.d/`) during installation;

- `--zsh-completion-script`: which is analogous for zsh;

- `--fish-completion-script`: which is analogous for fish shell;

- `--bash-completion-index`, `--bash-completion-word`: internal options used
   by the completion script to obtain a list of possible completions for a
   given command line;

- `--bash-completion-enriched`: a flag to tell the completion system to emit
   descriptions along with possible completions. This is used to provide help
   along with the completion for `zsh` and `fish`.

## Example nbx.yml file

[Example file here](nbx.yml)
