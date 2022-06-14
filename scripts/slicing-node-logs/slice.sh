# As of 2022 June 13, these four events have risingEdge variants.
#  ChainSyncServerEvent.TraceChainSyncServerUpdate
#  TraceAddBlockEvent.AddedBlockToQueue
#  TraceAddBlockEvent.PipeliningEvent.SetTentativeHeader
#  TraceAddBlockEvent.PoppedBlockFromQueue

for i in $(seq 0 51); do
  echo $i "$(date)"

  mkdir -p wholeselected
  echo 'PoppedBlockFromQueue to AddedToCurrentChain' >wholeselected/README
  cat $(ls -1tr ~/$run/node-$i/node*json) \
    | awk '/PoppedBlockFromQueue/ {x=$0}; /AddedToCurrentChain/ {print "[" x "," $0 "]";}' \
    | jq -c '(last | {at}) + (first | {herald: .at}) + (last | .data.newtip | ppoint)' \
    >wholeselected/node-$i.json

  mkdir -p forged
  echo 'TraceNodeIsLeader to TraceForgedBlock' >forged/README
  cat $(ls -1tr ~/$run/node-$i/node*json) \
    | awk '/TraceNodeIsLeader/ {x=$0}; /TraceForgedBlock/ {print "[" x "," $0 "]";}' \
    | jq -c '(last | {at}) + (first | {herald: .at}) + (last | .data.val | {hash: .block, slot})' \
    >forged/node-$i.json

  mkdir -p check
  echo 'TraceStartLeadershipCheck to TraceForgedBlock' >check/README
  cat $(ls -1tr ~/$run/node-$i/node*json) \
    | awk '/TraceStartLeadershipCheck/ {x=$0}; /TraceForgedBlock/ {print "[" x "," $0 "]";}' \
    | jq -c '(.[1] | {at}) + (first | {herald: .at}) + (last | .data.val | {hash: .block, slot})' \
    >check/node-$i.json

  mkdir -p announce
  echo 'TraceChainSyncServerUpdate rising edge' >announce/README
  cat $(ls -1tr ~/$run/node-$i/node*json) \
    | grep -e TraceChainSyncServerUpdate \
    | jq -c 'select(.data.risingEdge) | select(.data.addBlock) | {at} + (.data.addBlock | ppoint)' \
    >announce/node-$i.json
done

# just as a visibile sanity check -- otherwise unused
cat $(ls -1tr ~/$run/node-1/node*json | head -n1) \
  | grep -e 'basicInfo.epochLength' -e 'basicInfo.slotLength' \
  | jq -c '{length: (.data.message | split("s") | first | tonumber), ns}'

start=$(cat $(ls -1tr ~/$run/node-1/node*json | head -n1) \
  | grep -e 'basicInfo.systemStartTime' \
  | jq -c '.data.message' \
  | tr -d '"' \
  | awk '{ print $1 "T" $2 ".00Z" }')

# NB We group by slot as well only so that the results are chronologically sorted.
cat forged/node-*.json \
  | jq -cs 'sort_by([.slot,.hash]) | map(.slot)' \
  >slots.json
cat forged/node-*.json \
  | jq -cs "p(\"$start\") as \$start | sort_by([.slot,.hash]) | map(\$start + 1000 * .slot - p(.at))" \
  >onset.json
cat check/node-*.json \
  | jq -cs 'sort_by([.slot,.hash]) | map(p(.herald) - p(.at))' \
  >check.json
cat forged/node-*.json \
  | jq -cs 'sort_by([.slot,.hash]) | map(p(.herald) - p(.at))' \
  >isleader.json

# duration from forged to wholeselected on the nth node to do so
#
# NB We group by slot as well only so that the results are chronologically sorted.
#cat forged/node-*.json wholeselected/node-*.json \
#  | jq -cs 'group_by([.slot,.hash]) | .[] | map(p(.at)) | first as $base | .[1:] | map(. - $base) | sort' \
#  >nadds.json

# duration from forged to first announce on the block that forged it
# for i in $(seq 0 51); do
#  { cat forged/node-$i.json | jq -c '. + {forged: true}'; cat announce/node-$i.json; } \
#     | jq -cs 'group_by(.hash) | .[] | select(5 < length) | select(first | .forged) | (p(.[1] | .at) - p(first | .at))'
# done \
#   | sort -n | awk -f binning.awk -v sz=10 | uniq -c | awk -f cumsum1.awk

echo unsetting run=$run
unset run

