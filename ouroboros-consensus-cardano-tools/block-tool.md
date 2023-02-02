# Block Inspector

`block-tool` is a command-line utility that can dump the content of a full block to `stdout` in JSON. The block can either:

* Be given as standalone hex-CBOR encoded data, either to its standard input or from a file,
* Be read from an existing _immutable_ DB.

## Rationale

There already exists tools providing human-readable representation of the content of a block:

* Publicly available explorers like [Cardano Explorer](https://explorer.cardano.org/), [Cardanoscan](https://cardanoscan.io/), ...
* Services like [Blockfrost](https://blockfrost.io/), [Ogmios](https://ogmios.dev/), [scrolls](https://github.com/txpipe/scrolls), ...

This tool fits some more specialised use cases:

* Inspecting data directly from an existing node's DB which might live on a different network,
* Dumping content of some random block that might  not even exist on the chain anymore,
* Debugging output of consensus code.

## Usage

```
Usage: block-tool [--file-in FILE | --db-directory DIR --config FILE
                    --point SLOT.HEADER_HASH]

  Dump JSON representation of a CBOR-encoded block.

Available options:
  --file-in FILE           Path to file containing hex-encoded CBOR-encoded
                           block
  --db-directory DIR       Path to the directory where cardano DB lives
  --config FILE            Path to cardano node configuration file
  --point SLOT.HEADER_HASH The id of the block we want to start observing the
                           chain from. If not given, uses the chain tip at
                           startup. Composed by the slot number, a separator
                           ('.') and the hash of the block header. For example:
                           52970883.d36a9936ae7a07f5f4bdc9ad0b23761cb7b14f35007e54947e27a1510f897f04.
  -h,--help                Show this help text
```

### Extracting Block from file

```
$ block-tool -- --file-in block.cbor  | jq .
{
  "blockHash": {
    "blockHash": "77e81e9305859032fea6a8e79f5936e96723a1620608d2bd0100c2421f5ef667",
    "blockNo": 8300569,
    "slotNo": 82779850
  },
  "era": 5,
  "transactions": [
    {
    ....
    }
  ]
}
```

### Extracting Block from DB

```
$ cabal exec block-tool -- --db-directory ../cardano-node/preview/mainnet/db/ --config ../cardano-node/preview/cardano-node/config.json --point 8074331.753a31bff579c1c6fdce2e13a23bb20c9728612e449fafcf952ba7d1b3aa9931
{
  "blockHash": {
    "blockHash": "753a31bff579c1c6fdce2e13a23bb20c9728612e449fafcf952ba7d1b3aa9931",
    "blockNo": 380064,
    "slotNo": 8074331
  },
  "era": 5,
  "transactions": [
    {
      "auxiliaryData": "d90103a100a11902a2a1636d73678664f09f918b7825576879206469642074686520636f77626f7920686176652061207765696e657220646f673f64f09fa49460782d536f6d65626f647920746f6c642068696d20746f206765742061206c6f6e67206c6974746c6520646f6767792e64f09f989c",
      "body": {
        "auxiliaryDataHash": "853c4e9228f8972e7cc82cf78485e0974096f2cbc98d92ffe6fb226d7d8d609e",
        "fees": 196389,
        "inputs": [
          "285ed441c6d8f3b4260c06c13c7ded063366c4280291b1b0469d12ffe40f2c8e#TxIx 1",
          "a7cb1e998463b73727d83f4c825c87795d17832227c79b0985bf8f835a9c39d3#TxIx 2"
        ],
        "outputs": [
          {
            "address": "003347ec9ba22fe1531ddfe2031be059ad5d7c7e8165197524d408aabecba422308ce25990a7e1423e98a7ff9bc3cff17921f6f9092ce4a0d3",
            "value": {
              "lovelace": 1874850,
              "tokens": {
                "7c5562b0845a9097cb8620e982f50d33c97dd454bc4fd7947fc17265": {
                  "5748414c45": 440
                },
                "957b8ebe31bf1d426dce030165668407259bd5692a92a89012d3f80e": {
                  "4455434b53": 343
                },
                "c462512684cf5a5ee0b176326c724d5879a37a4977d3bf1e4edc39f6": {
                  "424c5545": 377,
                  "475245454e": 575,
                  "505552504c45": 485,
                  "5241494e424f57": 307,
                  "524544": 327,
                  "59454c4c4f57": 360
...
```
