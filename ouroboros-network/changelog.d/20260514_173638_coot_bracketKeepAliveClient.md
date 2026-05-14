<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- Split `FetchClientRegistry` into two parts
  - `FetchClientRegistry` - block-fetch related
  - `KeepAliveRegistry` - keep-alive related
  Added `newKeepAliveRegistry` to create `KeepAliveRegistry`, it should be
  called along side `newFetchClientRegistry` whenever `block-fetch` is used.
- `FetchClientRegistry` record fields where renamed, the `fcr` prefix was
  dropped,  `KeepAliveRegistry` field names were kept without the prefix too.

<!--
### Non-Breaking

- A bullet item for the Non-Breaking category.

-->
<!--
### Patch

- A bullet item for the Patch category.

-->
