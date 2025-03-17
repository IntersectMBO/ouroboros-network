#!/bin/bash

systemStartISO=$(jq -r '.systemStart' <"$1")
slotsPerKESPeriod=$(jq -r '.slotsPerKESPeriod' <"$1")
slotLength=$(jq -r '.slotLength' <"$1")
systemStart=$(date +%s -d "$systemStartISO")
now=$(date +%s)
timeDiff=$(($now - $systemStart))
echo $(($timeDiff / $slotsPerKESPeriod))
