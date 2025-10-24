<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- Using `KESPeriod` from `Cardano.Crypto.KES` instead of `SigKESPeriod`
  newtype.  `KESPeriod` is used by `SigRaw` data type.
- `SigKESSignature` holds `SigKES (KES crypto)` instead of a `ByteString`.
- `SigColdKey` holds `VerKeyDSIGN` instead of a `ByteString`.
- `ntnApps` constraints changed in order to use `sigValidate` function.

### Non-Breaking

- `Sig` codec decodes KES signatures, and the cold key.
- Added `DMQ.SigSubmission.Type.validateSig` and `SigValidationError`.

