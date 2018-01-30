#!/bin/bash


# print with formatting
p () {
  printf '\x1b[35;1m %s \x1b[0m\n' "$@"
  sleep 0.3
}

sleep 0.5
p "setting up stage"
p "constructing drums"
p "plugging in amplifiers"
p "putting microphones in stands"
p "connecting guitars to amps"
p "giving instruments to musicians"
p "pep talking musicians"
p "sending musicians onstage"
p "fading in lights"
p "flipping switch"
p "turning up to 11"

printf "\n"