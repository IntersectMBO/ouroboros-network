<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
-->

<!--
### Patch

- A bullet item for the Patch category.

-->
<!--
### Non-Breaking

- A bullet item for the Non-Breaking category.

-->
### Breaking

- `Ouroboros.Consensus.Storage.LedgerDB.*` and `Ouroboros.Consensus.Mempool.*`
  modules now have deprecation warnings for the previously exposed API to ease
  updates downstream. Old modules have deprecation headers and also every
  function and type exposed is now an alias to the right entity coupled together
  with a deprecation warning.


