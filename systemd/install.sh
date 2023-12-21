#!/bin/sh
PREFIX=/usr/local
SERVICEDIR=/etc/systemd/system
CONFIGDIR=/etc/kes-agent
COLDKEY=

while test -n "$1"
do
    case "$1" in
        --cold-key)
            shift
            COLDKEY="$1"
            ;;
        --prefix)
            shift
            PREFIX="$1"
            ;;
        --service-dir)
            shift
            SERVICEDIR="$1"
            ;;
        --config-dir)
            shift
            CONFIGDIR="$1"
            ;;
        -h|--help)
            cat <<EOT
install.sh - install kes-agent as a systemd service

Options:

--help,-h             Display this help
--cold-key FILE       Copy cold key file from FILE (default: prompt)
--prefix PATH         Set the install prefix to PATH (default: /usr/local)
--service-dir PATH    Install systemd service file into directory PATH
                      (default: /etc/systemd/system)
--config-dir PATH     Install kes-agent configuration files into directory PATH
                      (default: /etc/kes-agent)
EOT
            exit 0
            ;;
        *)
            echo "Invalid option: $1"
            exit 2
            ;;
    esac
done

read -p "Enter path to cold key file (default: ./cold.vkey): " COLDKEY
if test -z "$COLDKEY"; then
    COLDKEY=./cold.vkey
fi
COLDKEY=`realpath $COLDKEY`
cd $(dirname "$0")
if ! test -f $COLDKEY; then
    echo "File does not exist: $COLDKEY"
    exit 1
fi
cabal install exe:kes-agent --installdir=/tmp --install-method=copy
sudo groupadd kes-agent -f
sudo useradd kes-agent -g kes-agent -r
sudo install /tmp/kes-agent "$PREFIX/bin/kes-agent"
sudo install "$COLDKEY" "$CONFIGDIR/cold.vkey"
sudo install etc/kes-agent/kes-agent.env "$CONFIGDIR/kes-agent.env"
sudo install etc/kes-agent/mainnet-shelley-genesis.json "$CONFIGDIR/mainnet-shelley-genesis.json.vkey"
sudo install etc/systemd/system/kes-agent.service "$SERVICEDIR"/kes-agent.service 
sudo systemctl daemon-reload
sudo systemctl start kes-agent
sudo systemctl status kes-agent
