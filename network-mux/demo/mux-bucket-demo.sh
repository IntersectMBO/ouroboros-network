#!/usr/bin/env bash
# Runs mux-bucket-demo series on loopback: one server, X peers, optional tc
# emulation, both outputs kept per run, results as two tables.  From the
# repository root:
#
#   network-mux/demo/mux-bucket-demo.sh [OUTDIR]                       # the standard matrix
#   network-mux/demo/mux-bucket-demo.sh OUTDIR run NAME "SERVER ARGS" "CLIENT ARGS" ["TC QDISC"]
#   network-mux/demo/mux-bucket-demo.sh OUTDIR report NAME...          # tables from saved outputs
#
# Environment: PEERS (200) EB_MB (12) BUDGET (950, Mb/s) TC (1; set 0 to skip
# the runs that need `sudo tc` on lo).  tc runs change ALL loopback traffic
# while they last; the qdisc is removed after each run and on exit.
set -euo pipefail

OUT=${1:-/tmp/mux-bucket-demo}; mkdir -p "$OUT"
TC=${TC:-1}
PORT=6200
declare -a CLIENT_ROWS SERVER_ROWS

tc_clear() { if [ "$TC" = 1 ]; then sudo tc qdisc del dev lo root 2>/dev/null || true; fi; }
tc_set() {
  local -a spec
  if [ -n "$1" ]; then read -r -a spec <<< "$1"; sudo tc qdisc add dev lo root "${spec[@]}"; fi
}
trap tc_clear EXIT

# "25% of peers complete at 5.13 s" -> 5.13; "never complete" -> -
quantile() { awk -v q="$1%" '$1 == q { print ($0 ~ /complete at/) ? $(NF-1) : "-"; exit }' "$2"; }

collect() {  # name -> appends one row to each table, from $OUT/name.{client,server}
  local name=$1 c="$OUT/$1.client" s="$OUT/$1.server"
  local inside agg grants mb wmean wmax tmean tmax notdone stalled
  inside=$(sed -n 's/.*inside 7 s: *\([0-9]*\) *\/ *\([0-9]*\).*/\1\/\2/p' "$c" | head -1)
  agg=$(sed -n 's/.*= *\([0-9.]*\) Mb\/s.*/\1/p' "$c" | head -1)
  CLIENT_ROWS+=("$(printf '%-26s %9s %8s %8s %8s %8s %10s' "$name" "${inside:--}" \
      "$(quantile 25 "$c")" "$(quantile 50 "$c")" "$(quantile 75 "$c")" "$(quantile 95 "$c")" "${agg:--}")")
  # grants: 17800 (2401.6 MB); waited writable mean 0.10 ms max 8.81 ms; waited tokens mean 114.54 ms max 20109.52 ms
  read -r grants mb wmean wmax tmean tmax < <(awk '/^grants:/ { gsub(/[();]/, " "); print $2, $3, $8, $11, $16, $19; found = 1; exit }
                                                  END { if (!found) print "- - - - - -" }' "$s") || true
  notdone=$(awk '/^served:/ { print $4; exit }' "$s")
  # connections that did not complete: count, mean grants and MB absorbed before the gate closed
  stalled=$(awk '/^  conn +[0-9]+ \(/ { n++; g += $(NF-11); m += $(NF-9) }
                 END { if (n) printf "%d × %.0f grants, %.2f MB", n, g/n, m/n; else print "-" }' "$s")
  SERVER_ROWS+=("$(printf '%-26s %7s %8s %9s/%-9s %9s/%-9s %5s  %s' "$name" "$grants" "$mb" \
      "$wmean" "$wmax" "$tmean" "$tmax" "${notdone:-0}" "$stalled")")
}

tables() {
  echo
  echo "peers (client side): completion quantiles in seconds"
  printf '%-26s %9s %8s %8s %8s %8s %10s\n' run "inside 7s" "25%" "50%" "75%" "95%" "Mb/s"
  printf '%s\n' "${CLIENT_ROWS[@]}"
  echo
  echo "server (egress bucket): waits in ms; n/c = connections not completed"
  printf '%-26s %7s %8s %19s %19s %5s  %s\n' run grants MB "writable mean/max" "tokens mean/max" "n/c" "n/c conns (each)"
  printf '%s\n' "${SERVER_ROWS[@]}"
}

run() {  # name  "server args"  "client args"  ["tc qdisc spec"]  — the strings are split on spaces
  local name=$1 tc=${4:-}
  local -a sargs cargs
  read -r -a sargs <<< "$2"; read -r -a cargs <<< "$3"
  if [ -n "$tc" ] && [ "$TC" != 1 ]; then echo "skipping $name (TC=0)"; return; fi
  tc_clear; tc_set "$tc"
  PORT=$((PORT + 1))
  "$BIN" server --port "$PORT" "${sargs[@]}" > "$OUT/$name.server" 2>&1 &
  local spid=$!
  sleep 1.5
  timeout 600 "$BIN" client --host 127.0.0.1 --port "$PORT" "${cargs[@]}" > "$OUT/$name.client" 2>&1 || true
  wait "$spid" || true
  tc_clear
  echo "done $name ($(date +%T))"
  collect "$name"
}

case "${2:-}" in
  report) shift 2; for n in "$@"; do collect "$n"; done; tables; exit 0 ;;
  run)    BIN=$(cabal list-bin network-mux:exe:mux-bucket-demo 2>/dev/null)
          run "$3" "$4" "$5" "${6:-}"; tables; exit 0 ;;
esac
BIN=$(cabal list-bin network-mux:exe:mux-bucket-demo 2>/dev/null)
if [ "$TC" = 1 ] && ! sudo -n true 2>/dev/null; then
  echo "tc runs need passwordless sudo; set TC=0 to skip them" >&2; exit 1
fi

X=${PEERS:-200}; EB=${EB_MB:-12}; B=${BUDGET:-950}
S="--peers $X --eb-mb $EB"; C="--peers $X"
TBF="tbf rate ${B}mbit burst 2mb latency 100ms"; RTT="netem delay 33ms limit 100000"

run baseline_tbf        "$S --budget-mbps 0 --horizon 150"                                 "$C --horizon 150" "$TBF"
run equal_bucket        "$S --budget-mbps $B --lowat 131072 --horizon 150"                 "$C --horizon 150"
run strict_bucket       "$S --budget-mbps $B --lowat 131072 --order arrival --horizon 150" "$C --horizon 150"
run bucket_below_link   "$S --budget-mbps 300 --lowat 131072 --horizon 200"                "$C --horizon 200" "$TBF"
run stall_nolowat       "$S --budget-mbps $B --lowat 0 --sdu-timeout 120 --horizon 60"      "$C --stall 4 --horizon 60"
run stall_lowat         "$S --budget-mbps $B --lowat 131072 --sdu-timeout 120 --horizon 60" "$C --stall 4 --horizon 60"
run rtt66_equal         "$S --budget-mbps $B --lowat 131072 --horizon 150"                 "$C --horizon 150" "$RTT"
run rtt66_strict        "$S --budget-mbps $B --lowat 131072 --order arrival --horizon 150" "$C --horizon 150" "$RTT"
run rtt66_single_lowat   "--peers 1 --eb-mb $EB --budget-mbps $B --lowat 131072 --horizon 60" "--peers 1 --horizon 60" "$RTT"
run rtt66_single_nolowat "--peers 1 --eb-mb $EB --budget-mbps $B --lowat 0 --horizon 60"      "--peers 1 --horizon 60" "$RTT"
tables
echo "outputs in $OUT"
