## Documents that could be useful

- [How to add an era?](AddingAnEra.md): the Cardano blockchain spans multiple eras,
  each of which introduce a hard-fork. The process for adding an era to
  `ouroboros-consensus-cardano` is documented in this file. However, this is
  Cardano specific, and maybe it should not live inside consensus.
- [Chain sync client specification](ChainSync.md) describes the chain-sync client specification.
- [Hard won wisdom](HardWonWisdom.md) contains several realizations we had
  while working on the code base. This could be used both as a place to look
  random information, and source of inspiration for improving our documentation
  further.
- [Our style guide](StyleGuide.md).
- [Interface changelog](interface-CHANGELOG.md) I do not know if this file
  belongs to the documentation section, but it is definitely useful.
- [The consensus report](report/): this is a great source of information.

## Documents that we have to remove

Part of the content of these documents can be moved to some other documents,
but the documents themselves should be removed.

- [Genesis decomposition](GenesisDecomposition.md): this should probably part of
  the genesis design document, or its documentation. Furthermore, the
  information here could be out of date.
- [Git process](GitProcess.md): this seems to belong to a `CONTRIBUTING`
  document.
- [IOHKOnboarding.md](IOHKOnboarding.md): this is IOG specific, and should be
  placed in a IOG specific repository or page.
- [Description of the consensus tests](Testing.md): this should be moved into the code (See [this issue](https://github.com/input-output-hk/ouroboros-network/issues/4145)).
- [How to configure CODEOWNERS](config-CODEOWNERS.md): this is out of date and
  probably belongs to a `CONTRIBUTING` document.
- [Node initialization steps and
  events](initialization-steps-and-the-related-tracer.org): this contains way
  too many code details and it is likely to bitrot. Maybe we can move some of
  the information in this file to source code comments.
- [UTxO HD early plans](utxo-hd-MVP-home-stretch-roadmap.md): this is outdated
  and should be removed.

## Documents that we have to rewrite

- [Onboarding.md](Onboarding.md)


