### Patch

- Avoid a peer selection governor assertion when a peer sharing request fails
  after its peer has disconnected. Apply the peer sharing retry delay only to
  peers which are still established when the request completes.
