set -exuo pipefail

server_pid=0
client_pid=0
dump_pid=0

cleanup() {
    sudo ip netns exec ns1 kill ${server_pid} || true
    sudo ip netns exec ns2 kill ${client_pid} || true
    sudo ip netns exec ns2 kill ${dump_pid} || true
    sudo ip netns del ns1 || true
    sudo ip netns del ns2 || true
}
trap cleanup EXIT

# scrub
cleanup

# create namespaces
sudo ip netns add ns1
sudo ip netns add ns2

# create veth pairs
sudo ip link add veth-server netns ns1 type veth peer name veth-client netns ns2

# Bring the devices up.
sudo ip -n ns1 link set veth-server up
sudo ip -n ns2 link set veth-client up

# Assign IP address to the veth interface at the host side.
sudo ip -n ns1 addr add 10.0.0.1/24 dev veth-server
sudo ip -n ns2 addr add 10.0.0.2/24 dev veth-client

# shape egress
sudo tc -n ns1 qdisc add dev veth-server root handle 1: htb default 1
sudo tc -n ns1 class add dev veth-server parent 1: classid 1:1 htb rate 20mbit burst 20kb
# pace transmission
sudo tc -n ns1 qdisc add dev veth-server parent 1:1 fq_codel \
     quantum 1514 target 5ms interval 10ms

sudo ip netns exec ns1 sysctl -w net.ipv4.tcp_slow_start_after_idle=0

sudo ip netns exec ns2 \
     tcpdump -w capture.pcap -s 0 &
dump_pid=$!

cmd="$(cabal list-bin incremental-benchmark)"

sudo ip netns exec ns1 bash -c "$cmd server +RTS -N2 -RTS" &
server_pid=$!
sleep 1

sudo ip netns exec ns2 bash -c "$cmd client +RTS -A64m -N2 -l-au -RTS"
client_pid=$!

ghc-events-util --match '(DURATION) small-nonincremental' show-delta incremental-benchmark.eventlog \
                > small_nonincremental
ghc-events-util --match '(DURATION|STEP) small-nonincremental' show-delta incremental-benchmark.eventlog \
                >> small_nonincremental
ghc-events-util --match '(DURATION) small-incremental' show-delta incremental-benchmark.eventlog \
                > small_incremental
ghc-events-util --match '(DURATION|STEP) small-incremental' show-delta incremental-benchmark.eventlog \
                >> small_incremental

ghc-events-util --match '(DURATION) large-nonincremental' show-delta incremental-benchmark.eventlog \
                > large_nonincremental
ghc-events-util --match '(DURATION|STEP) large-nonincremental' show-delta incremental-benchmark.eventlog \
                >> large_nonincremental
ghc-events-util --match '(DURATION) large-incremental' show-delta incremental-benchmark.eventlog \
                > large_incremental
ghc-events-util --match '(DURATION|STEP) large-incremental' show-delta incremental-benchmark.eventlog \
                >> large_incremental
