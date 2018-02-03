# nbx

## Hurdles
- Boxfile parsing

## Examples

### Workflow
- nbx init
- nbx run
- nbx push [remote]

### Troubleshooting
- nbx logs [remote]
- nbx console [remote]
- nbx tunnel [remote]
- nbx status [remote]

### Helpers
- nbx setup
- nbx implode

### Bash, Zsh, and Fish Completions (so I don't have to find this later)

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
   $ source <(nbx --bash-completion-script `which nbx`)
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
```yaml
# CONFIGURE NBX
config:
  remotes:
    - prod: nbx:team/group/app
    - staging: nbx:team/group/app
    - storage: docker:dockerhub/user

# A SERVICE ("web")
web:
  dev:
    image: nanobox/python-3-dev
    run:
      nginx:  nginx -c /etc/nginx.conf
      python: python app.py
    aliases:
      - nanoapp.local
    dependencies:
      - service1
      - service2
      - s3

  live:
    image: nanobox/python-3
    build:
      image: nanobox/python-3-dev
      steps:
        - python compile.py
      dirs:
        - src: app
          target: /app
        - src: bin
          target: /usr/local/sbin
    run:
      nginx: nginx -c /etc/nginx.conf
      python: nanoinit python app.py
    http:
      expose: 8080
      force_ssl: true
      health_route: /health
      routes:
        - 'admin:'
        - '^/admin/'
    tcp:
      - 5555:5555
    udp:
      - 9654:10000

  env.local:
    FOO: bar

  env.staging:
    BAZ: boa

  env.production:
    BLA: ber

  env.whatever:
    BOO: yah

# ANOTHER SERVICE ("worker")
worker:
  dev:
    image: nanobox/python-3-dev
    run:
      python: python app.py
    aliases:
      - nanoapp.local
    dependencies:
      - service1

# A DATA SERVICE ("db")
data.db:
  image: nanobox/mysql

  config:
    foo: bar
    ram: 512

  data_dir: /var/db/mysql

# A DATA SERVICE ("queue")
data.queue:
  image: nanobox/redis

  config:
    foo: bar
    ram: 512

  data_dir: /var/db/redis
```