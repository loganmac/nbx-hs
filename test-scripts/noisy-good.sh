#!/bin/bash


for i in {1..600}
do
  printf "installing package #%s...\n" $i
  sleep 0.01
done
printf "\n"