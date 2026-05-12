#!/bin/env bash

# network shaping parameters
THROUPUT=$1 # e.g. 100mbit
DELAY=$2 # e.g. 10ms
# temporary directory
TMP_DIR=$3
# client command to run in ns1
CLIENT_CMD="${4} 2>${TMP_DIR}/client.stderr | tee ${TMP_DIR}/client.stdout"
# server command to run in ns2
SERVER_CMD="${5} 2>${TMP_DIR}/server.stderr | tee ${TMP_DIR}/server.stdout"

echo "THROUPUT: ${THROUPUT}" | tee -a $TMP_DIR/config
echo "DELAY: ${DELAY}" | tee -a $TMP_DIR/config
echo "CLIENT_CMD: ${CLIENT_CMD}" | tee -a $TMP_DIR/config
echo "SERVER_CMD: ${SERVER_CMD}" | tee -a $TMP_DIR/config

cleanup_netns() {
  for i in 1 2 3; do
    for pid in $(sudo ip netns pids ns$i); do
      # kill server and tcpdump
      sudo kill -9 $pid
    done
  done
  for i in 1 2 3; do sudo ip netns del ns$i; done

  # no need to cleanup links, since deleting the namespace deletes those
  # links

  # the script itself invokes this handler directly to stop the processes, so
  # reset the SIGNAL handlers
  trap - EXIT INT TERM
}
trap cleanup_netns EXIT INT TERM

# Network topology:
#
# ┌──────────────────────┐      ┌────────────────────────────┐      ┌──────────────────────┐
# │    Namespace ns1     │      │      Namespace ns3         │      │    Namespace ns2     │
# │  (client namespace)  │      │    (bridge namespace)      │      │  (server namespace)  │
# │                      │      │                            │      │                      │
# │  veth1               │◄––––►│  veth1-br        veth2-br  │◄––––►│  veth2               │
# │  10.0.0.1 / 24       │      │  [netem]         [netem]   │      │  10.0.0.2 / 24       │
# │  [HTB + fq_codel]    │      │      │               │     │      │  [HTB + fq_codel]    │
# │                      │      │      └──────┬────────┘     │      │                      │
# └──────────────────────┘      │             │              │      └──────────────────────┘ 
#                               │           [ br ]           │
#                               │        Linux Bridge        │
#                               │                            │
#                               └────────────────────────────┘
# 
# Legend:
#   veth1 ◄––► veth1-br  : veth pair #1
#   veth2 ◄––► veth2-br  : veth pair #2
#   br                   : Layer-2 bridge connecting veth1-br and veth2-br
#
#
# veth{1,2} egress shapring:
# veth{1,2}
#  └─ htb 1:
#      └─ class 1:1 $THROUPUT
#          └─ class 1:10
#              └─ fq_codel
#
# veth{1,2}-br egress shaping:
# veth{1,2}-br
#  └─ netem delay $DELAY


# create nemaspaces
for i in 1 2 3; do
  set -x;
  sudo ip netns add ns$i;
  { set +x; } 2>/dev/null
done

# adapted from https://unix.stackexchange.com/a/558427
add_edge() {
  i=$1
  set -x
  # create veth pair to connect ns${i} to ns3
  sudo ip link add veth${i} netns ns$i type veth peer name veth${i}-br netns ns3

  # Bring the devices up.
  sudo ip -n ns$i link set lo up
  sudo ip -n ns$i link set veth${i} up
  sudo ip -n ns3  link set veth${i}-br up

  # Assign IP address to the veth interface.
  sudo ip -n ns$i addr add 10.0.0.$i/24 dev veth${i}

  { set +x; } 2>/dev/null
}

add_edge 1
add_edge 2

add_qdiscs() {
  i=$1
  set -x

  # Limit bandwidth and pace outgoing traffic.
  sudo tc -n ns$i qdisc del dev veth${i} root
  sudo tc -n ns$i qdisc add dev veth${i} root handle 1: htb default 10
  sudo tc -n ns$i class add dev veth${i} parent 1: classid 1:1 htb rate $THROUPUT ceil $THROUPUT
  sudo tc -n ns$i class add dev veth${i} parent 1:1 classid 1:10 htb rate $THROUPUT ceil $THROUPUT
  sudo tc -n ns$i qdisc add dev veth${i} parent 1:10 handle 10: fq_codel
  sudo tc -n ns$i class show dev veth${i}

  { set +x; } 2>/dev/null
}

add_qdiscs 1
add_qdiscs 2

# Create a bridge in ns3 and connect veth1-ns2 and veth2-br through it.
setup_bridge() {
  set -x
  sudo ip -n ns3 link add name br type bridge
  sudo ip -n ns3 link set lo up
  sudo ip -n ns3 link set dev br up
  # disable STP on the bridge, since we don't have loops in our setup
  sudo ip -n ns3 link set br type bridge stp_state 0

  for i in 1 2; do
    sudo ip -n ns3 link set veth${i}-br master br;
  done

  # Create ifb device for traffic shaping on the bridge
  # sudo ip -n ns3 link add ifb-br type ifb
  # sudo ip -n ns3 link set ifb-br up

  for i in 1 2; do
    # delay egress traffic on veth${i}-br interface
    sudo tc -n ns3 qdisc add dev veth${i}-br root netem delay $DELAY
  done;

  sudo ip -n ns1 route get 10.0.0.2
  sudo ip -n ns2 route get 10.0.0.1

  { set +x; } 2>/dev/null
}

setup_bridge

# For debuging throuput shaping
# sudo ip netns exec ns2 iperf3 -s &
# sudo ip netns exec ns1 iperf3 -c 10.0.0.2
# exit 0

sudo ip netns exec ns1 bash -c "{ echo "ns1"; ip addr show; ip route show; tc qdisc show dev veth1; }"
sudo ip netns exec ns2 bash -c "{ echo "ns2"; ip addr show; ip route show; tc qdisc show dev veth2; }"
sudo ip netns exec ns3 bash -c "{ echo "ns3"; ip addr show; ip route show; tc qdisc show dev br; }"

# run tcpdump in `ns1` to capture packets from the client side
sudo ip netns exec ns1 tcpdump -i veth1 -w $TMP_DIR/ns1-veth1.pcap &
sudo ip netns exec ns2 tcpdump -i veth2 -w $TMP_DIR/ns2-veth2.pcap &

# run the server asynchronously
sudo ip netns exec ns2 bash -c "$SERVER_CMD" &
# wait for the server to warm up
sleep 0.05

# run the client synchronously
sudo ip netns exec ns1 bash -c "$CLIENT_CMD"

# wait for the server to terminate
sleep 0.1

sudo chown $USER:$USER $TMP_DIR/*
