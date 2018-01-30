# nbx

## Hurdles
  Log
  DB (key/val)
  Global Config
  Boxfile parsing

## Examples

### Workflow
nbx init
nbx run
nbx push [remote]

### Troubleshooting
nbx logs [remote]
nbx console [remote]
nbx tunnel [remote]
nbx status [remote]

### Helpers
nbx setup
nbx implode


## Example nbx.yml file
```
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
      to: 8080
      force_ssl: true
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