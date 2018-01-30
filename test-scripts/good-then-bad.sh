#!/bin/bash

# sleep for some amount of time
sleepy () {
  sleep 0.3
}

# print with formatting
p () {
  printf '\x1b[35;1m %s \x1b[0m\n' "$@"
  sleepy
}

# print with formatting to stderr
pe () {
  printf '\x1b[33;1m %s \x1b[0m\n' "$@" >&2
  sleepy
}

p "setting up stage"
p "constructing drums"
p "plugging in amplifiers"
p "putting microphones in stands"
p "connecting guitars to amps"
pe "the band is an acapella group"
pe "they are all crying"
pe "lead singer is too afraid to go out"
pe "accidentally turned on sprinklers"
pe "i smell smoke"
pe "fire department just showed up"

printf "\n"

exit 113