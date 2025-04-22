#!/bin/sh
PREFIX=/usr/local
SERVICEDIR=/etc/systemd/system
CONFIGDIR=/etc/kes-agent
COLDKEY=
DO_BUILD=
BINDIR="$(realpath ./bin)"

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
        --build)
            DO_BUILD="1"
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

if ! test -f "$CONFIGDIR/cold.vkey"; then
    read -p "Enter path to cold key file (default: ./cold.vkey): " COLDKEY
fi
if test -z "$COLDKEY"; then
    COLDKEY=./cold.vkey
fi
COLDKEY=`realpath $COLDKEY`
cd $(dirname "$0")
if ! test -f "$CONFIGDIR/cold.vkey"; then
    if ! test -f "$COLDKEY"; then
        echo "File does not exist: $COLDKEY"
    fi
fi
if test "$DO_BUILD" = "1"; then
	mkdir -p "$BINDIR"
    cabal install exe:kes-agent --installdir="$BINDIR" --install-method=copy
fi
if ! test -f "$BINDIR/kes-agent"; then
	echo "File does not exist: $BINDIR/kes-agent"
	exit 2
fi
mkdir -p /etc/kes-agent
groupadd kes-agent -f
useradd kes-agent -g kes-agent -r
install "$BINDIR/kes-agent" "$PREFIX/bin/kes-agent"
if ! test -f "$CONFIGDIR/cold.vkey"; then
    install "$COLDKEY" "$CONFIGDIR/cold.vkey"
fi
if ! test -f "$CONFIGDIR/kes-agent.env"; then
    install etc/kes-agent/kes-agent.env "$CONFIGDIR/kes-agent.env"
fi
if ! test -f "$CONFIGDIR/kes-agent.toml"; then
    install etc/kes-agent/kes-agent.toml "$CONFIGDIR/kes-agent.toml"
fi
install etc/kes-agent/mainnet-shelley-genesis.json "$CONFIGDIR/mainnet-shelley-genesis.json"
install etc/systemd/system/kes-agent.service "$SERVICEDIR"/kes-agent.service
install etc/systemd/system/kes-agent-hibernate.service "$SERVICEDIR"/kes-agent-hibernate.service
systemctl daemon-reload
systemctl start kes-agent
systemctl start kes-agent-hibernate
systemctl enable kes-agent
systemctl enable kes-agent-hibernate
systemctl status kes-agent
