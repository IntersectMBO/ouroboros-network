set -x
topology="$(dirname $0)/simple-topology.json"
slot_duration=2
now="`date '+%Y-%m-%d 00:00:00'`"
set +x

start-node() {
  local node_id="$1"; shift
  cabal new-run demo-playground -- \
        --system-start "$now" \
        --slot-duration ${slot_duration} \
        node \
        --topology "${topology}" \
        --node-id $node_id \
        --host localhost \
        --port 300$node_id \
        "$@" &
}
