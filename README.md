# nbx

requirements:
docker running, `which docker && docker ps` with a 0 exit code

nbx push (docker)
for each service:
  1 - if there is a build section
    a - pulling or building image from image attribute in build
    b - create a container from this new image (mount the source into the container at /app)
    c - run all steps
    d - delete container
  2 - look in the live section, pull image
    a - if image is a Dockerfile, then first, we need to do a docker build
    b - if image is remote registry, then we need to pull the image
  3 - create final image
    a. create container
    b. copy dirs specific in build
    c. commit (creating a new image)
  4 - tag the image
  5 - push to the remote registry

nbx push (nbx)

1 - docker push
2 - if commit
  a - fetch the nbx checksum from odin
  b - generate a checksum of nbx file
  c - If checksum is different
    1 - POST /commits to odin
    2 - provide nbx file
    3 - provide checksum
3 - if deploy
  a. POST /deploys

## Workflow

*MVP

- nbx init *
- nbx run     [-l] *
- nbx push    [remote]

## Troubleshooting

- nbx logs    [remote] [service]
- nbx console [remote] [service]
- nbx tunnel  [remote] [service]
- nbx status  [remote] [service]

## Helpers

- nbx login [remote] *
- nbx setup
- nbx implode

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