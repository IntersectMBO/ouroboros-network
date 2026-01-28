# Wireshark Dissector for Ouroboros-Network.

To use it copy `ouroboros_network.lua` to `$HOME/.config/wireshark/plugins`, or
create a symbolic link:

```bash
ls -s $(pwd)/ouroboros_network/wireshark-plugin/ouroboros_network.lua $HOME/.config/wireshark/plugins/ouroboros_network.lua
```

## Mux disector

There is also another dissector for the mux protocol in
`./network-mux/demo/mux-leios-demo.lua`.


## Understanding the code

To read more about Wireshark dissectors in Lua, see the [WireShark
Wiki](https://wiki.wireshark.org/Lua/Dissectors).

## Development

To debug it one can use `tshark` in the terminal.  All `print` statements
are printed to the terminal, e.g.

```bash
tshark -X lua_script:ouroboros_network.lua -r ouroboros-network.pcap
```
