#!/usr/bin/env bash

cd "$(dirname "$(readlink -f "$0")")"
exec swipl -g "['prolog/$1.pl']" -g 'solution' -g 'halt'
