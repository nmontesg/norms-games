#!/bin/bash

swipl -s "ngames/search/writer.pl" -g "build_ASL_description(\"$DEFAULTS\",\"$NORMS\",ns{firstInTime:1,firstToAnnounce:2},fishers)" -t "halt"
