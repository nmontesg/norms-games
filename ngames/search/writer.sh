#!/bin/bash

swipl -s "ngames/search/writer.pl" -g "build_ASL_description(\"$1\",\"$2\",$3,$4)" -t "halt"
