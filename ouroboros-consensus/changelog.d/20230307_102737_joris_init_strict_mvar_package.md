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

- The `Ouroboros.Consensus.Util.MonadSTM.NormalForm` and
  `Ouroboros.Consensus.Util.MonadSTM.StrictMVar` modules have been moved to a
  new package called `strict-mvar`, where they are now called
  `Control.Concurrent.Class.MonadMVar.NormalForm` and
  `Control.Concurrent.Class.MonadMVar.StrictMVar` respectively. Deprecation
  warnings have been left behind in the old module locations.
