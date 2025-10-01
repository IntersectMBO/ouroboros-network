### Breaking

- `VersionDataCodec` and `cborTermVersionDataCodec` moved to
  `ouroboros-network:api` package (`Ouroboros.Network.CodecCBORTerm` module).
- `VersionDataCodec`: removed the `bytes` polymorphic variable, since it was
  always instantiated to `CBOR.Term`.
- `CodecCBORTerm` module provides now `VersionedCodecCBORTerm` a versioned
  version of `CodecCBORTerm`, and a pattern synonym `VersionDataCodec` as used
  in the rest of the codebase.  The `cborTermVersionDataCodec` was renamed as
  `mkVersionedCodecCBORTerm`.  Also added its inverse `unVersionCodecCBORTerm`.
