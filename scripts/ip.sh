#!/bin/env bash
set -euo pipefail

# This might require loading veth kernel module with
# modprove veth

# create two network namespaces
ip netns add ouroboros-network-ns-0
ip netns add ouroboros-network-ns-1

# create a pair of veth network interfaces
ip link add veth0 type veth peer name veth1

# add each one to its own namespace
ip link set veth0 netns ouroboros-network-ns-0
ip link set veth1 netns ouroboros-network-ns-1
# ip netns exec ouroboros-network-ns-0 ip link list 
# ip netns exec ouroboros-network-ns-1 ip link list 

# configure addresses
ip netns exec ouroboros-network-ns-0 ifconfig veth0 10.10.10.0/31
ip netns exec ouroboros-network-ns-1 ifconfig veth1 10.10.10.1/31
