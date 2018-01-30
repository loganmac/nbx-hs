#!/bin/bash

# print with formatting - to stderr
p () {
  printf '\x1b[31;1m %s \x1b[0m\n' "$@" >&2
  sleep 0.3
}

p "there is no stage"
p "who was supposed to bring drums"
p "these amps have EU plugs"
p "why are the mic stands so short"
p "these are acoustic guitars"
p "the band is an acapella group"
p "they are all crying"
p "lead singer is too afraid to go out"
p "accidentally turned on sprinklers"
p "i smell smoke"
p "fire department just showed up"

printf "\n"

exit 113
