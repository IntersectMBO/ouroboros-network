<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

<!--
### Breaking

- A bullet item for the Breaking category.

-->
### Non-Breaking

- Improved `cardano-diffusion:ping` output:
  * don't report results, if there were none
  * changed `DNSError` show instance:  it's easier to read if the error is
    followed by dns name, especially if there are multiple errors of the
    same type, like `NameError`.
  * when resolving an SRV record fails show the effective domain name, e.g.
    `_cardano._tcp.domain.com` rather than `domain.com`.

<!--
### Patch

- A bullet item for the Patch category.

-->
