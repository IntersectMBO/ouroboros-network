### Non-Breaking

- Fixed accepting connections on 32bit platforms.  `AcceptedConnectionsLimit`
  fields are `Word32` and they are converted to `Int` with `fromIntegral`, thus
  we impose `maxBound :: Int32` limit on them by a smart constructor
  `mkAcceptedConnectionsLimit`.
