#!/bin/env bash

addr() {
  echo "10.1.0.$1"
}

# server's configuration
SERVER_ADDR=$(addr 2)
SERVER_PORT=9001
REQUEST_SIZE=10 # 10B
MUX_SDU=12288
LEIOS_BLOCK_SIZE=$((($MUX_SDU - 5) * 1000))
PRAOS_BLOCK_SIZE=$((($MUX_SDU - 5) * 10))

# client's configuration
NUM_LEIOS_REQUESTS=20
NUM_PRAOS_REQUESTS=1000 # $(($NUM_LEIOS_REQUESTS * $LEIOS_BLOCK_SIZE / $PRAOS_BLOCK_SIZE))

EGRESS_THROUPUT="100mbit"
INGRESS_DELAY="20ms"

RTS_OPTIONS="-N2"

TMP_DIR=$(mktemp -d ${TMPDIR:-/tmp}/mux-leios-demo.XXXXXX)
rm -f ./mux-leios-tmp-dir
ln -s "$TMP_DIR" ./mux-leios-tmp-dir
echo "Using temporary directory: $TMP_DIR"

echo "REQUEST_SIZE: ${REQUEST_SIZE}B" >> $TMP_DIR/config
echo "PRAOS_BLOCK_SIZE: $(bc <<< "scale=2; $PRAOS_BLOCK_SIZE / 1000")KB" >> $TMP_DIR/config
echo "LEIOS_BLOCK_SIZE: $(bc <<< "scale=2; $LEIOS_BLOCK_SIZE / 1000000")MB" >> $TMP_DIR/config

cleanup_netns() {
  for i in 1 2; do
    for pid in $(sudo ip netns pids ns$i); do
      # kill server and tcpdump
      sudo kill -9 $pid
    done
  done
  for i in 1 2; do sudo ip netns del ns$i; done

  # no need to cleanup links, since deleting the namespace deletes those
  # links

  # the script itself invokes this handler directly to stop the processes, so
  # reset the SIGNAL handlers
  trap - EXIT INT TERM
}
trap cleanup_netns EXIT INT TERM

for i in 1 2; do
  set -x;
  sudo ip netns add ns$i;
  { set +x; } 2>/dev/null
done

# adapted from https://unix.stackexchange.com/a/558427
add_edge() {
  i=$1
  j=$2
  set -x
  sudo ip link add veth$i$j netns ns$i type veth peer name veth$j$i netns ns$j

  # Assign IP address to the veth interface.
  sudo ip -n ns$i addr add $(addr $i)/32 dev veth$i$j

  # Setup routing rules.
  sudo ip -n ns$i addr add local $(addr $i) peer $(addr $j) dev veth$i$j

  # Allocate IFBs for applying qdiscs ingress policing.
  sudo ip -n ns$i link add ifb$i$j type ifb

  # Bring the devices up.
  sudo ip -n ns$i link set veth$i$j up
  sudo ip -n ns$i link set ifb$i$j up

  { set +x; } 2>/dev/null
}

add_edge 1 2
add_edge 2 1

add_qdiscs() {
  i=$1
  j=$2
  set -x

  # Limit bandwidth of and pace outgoing traffic.
  sudo tc -n ns$i qdisc add dev veth$i$j root handle 1: htb default 1
  sudo tc -n ns$i class add dev veth$i$j parent 1: classid 1:1 htb rate $EGRESS_THROUPUT
  sudo tc -n ns$i qdisc add dev veth$i$j parent 1:1 handle 10: fq_codel

  # Delay incoming traffic.
  sudo tc -n ns$j qdisc add dev veth$j$i handle ffff: ingress
  sudo tc -n ns$j filter add dev veth$j$i parent ffff: protocol ip u32 match u32 0 0 action mirred egress redirect dev ifb$j$i
  sudo tc -n ns$j qdisc add dev ifb$j$i root netem delay $INGRESS_DELAY

  { set +x; } 2>/dev/null
}

add_qdiscs 1 2
add_qdiscs 2 1

cabal build mux-leios-demo
CMD=$(cabal list-bin exe:mux-leios-demo)

# client is executed in `ns1`
# CLIENT_CMD="$CMD client $SERVER_ADDR $SERVER_PORT $REQUEST_SIZE $NUM_PRAOS_REQUESTS $NUM_LEIOS_REQUESTS +RTS ${RTS_OPTIONS} -RTS 2>$TMP_DIR/client.stderr | tee $TMP_DIR/client.stdout"
CLIENT_CMD="$CMD client-burst $SERVER_ADDR $SERVER_PORT +RTS ${RTS_OPTIONS} -RTS 2>$TMP_DIR/client.stderr | tee $TMP_DIR/client.stdout"
# server is executed in `ns2`
# SERVER_CMD="$CMD server $SERVER_ADDR $SERVER_PORT $PRAOS_BLOCK_SIZE $LEIOS_BLOCK_SIZE +RTS ${RTS_OPTIONS} -RTS 2>$TMP_DIR/server.stderr 1>$TMP_DIR/server.stdout"
SERVER_CMD="$CMD server-burst $SERVER_ADDR $SERVER_PORT $NUM_PRAOS_REQUESTS $PRAOS_BLOCK_SIZE $NUM_LEIOS_REQUESTS $LEIOS_BLOCK_SIZE +RTS $RTS_OPTIONS -RTS 2>$TMP_DIR/server.stderr 1>$TMP_DIR/server.stdout"

echo "Server command: $SERVER_CMD" | tee -a $TMP_DIR/config
echo "Client command: $CLIENT_CMD" | tee -a $TMP_DIR/config

sudo ip netns exec ns1 bash -c "{ echo "ns1"; ip addr show veth12; }"
sudo ip netns exec ns2 bash -c "{ echo "ns2"; ip addr show veth21; }"
# run tcpdump in `ns1` to capture packats from the client side
sudo ip netns exec ns1 tcpdump -i veth12 -w $TMP_DIR/ns1-veth12.pcap &
sudo ip netns exec ns2 tcpdump -i veth21 -w $TMP_DIR/ns2-veth21.pcap &

# run the server asynchronously
sudo ip netns exec ns2 bash -c "$SERVER_CMD" &
# wait for the server to warm up
sleep 0.05

# run the client synchronously
sudo ip netns exec ns1 bash -c "$CLIENT_CMD"

# wait for the server to terminate
sleep 0.1

sudo chown $USER:$USER $TMP_DIR/*
