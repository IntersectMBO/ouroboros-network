<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- `cardano-diffusion:cardano-diffusion-tests-lib`:
  - `Test.Cardano.Network.Diffusion.Testnet.Simulation`: `SimArgs` has a new
    `saTxOutboundVersion` field, `NodeArgs` has a new `naTxSubmissionConfig`
    field (nodes can use different configurations with `TxOutboundV_2`), and
    `mainnetSimArgs` takes a `TxOutboundVersion`.
  - `Test.Cardano.Network.Diffusion.Testnet.MiniProtocols`: `AppArgs` has new
    `aaTxSubmissionConfig` and `aaTxOutboundVersion` fields.

<!--
### Patch

- A bullet item for the Patch category.

-->
