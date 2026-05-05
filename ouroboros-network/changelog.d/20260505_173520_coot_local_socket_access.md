<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- Added `Interfaces.diNtcConfigureSocketFile`.  It recieves the file path of
  the local socket.
- Added `DiffusionTracer` constructors: `ConfiguredLocalSocket` (fired
  after the local socket's permissions are tightened),
  `InsecureLocalSocketDirectory` (warning when the parent directory of the
  local socket has group or other write permission) and
  `InsecureLocalSocketPermissions` (warning when the socket is readable or
  writable by `other`).
- `mkInterfaces`'s tracer type is now specialised to `LocalAddress`
  (`Tracer IO (DiffusionTracer ntnAddr LocalAddress)`).

### Non-Breaking

- Trace a warning at start-up if the local-socket permissions are too broad
  (e.g. `other` has read or write access to it).  We leave it to the user to
  control file permissions created by the running process, e.g. by `umask`.
- Trace a warning at start-up if the parent directory of the local-socket
  path has `group` or `other` write permission, since a `0600` socket
  inside such a directory remains vulnerable to manipulation by another
  local user with write access to that directory.

<!--
### Patch

- A bullet item for the Patch category.

-->
