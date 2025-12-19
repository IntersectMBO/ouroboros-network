### Non-Breaking

- Added `network-mux:mux-leios-demo`
- Added binary encoding to `Test.Mux.ReqResp`:
  * `runClient` renamed as `runClientCBOR`
  * `runServer` renamed as `runServerCBOR`
  added API:
  * `runClientBin`
  * `runServerBin`
  * `runClientBurstBin`
  * `runServerBurstBin`
  * `runClientBurstCBOR`
  * `runServerBurstCBOR`

- Added `network-mux/wireshark-plugin/` - a WireShark dissector plugin for mux SDUs.
