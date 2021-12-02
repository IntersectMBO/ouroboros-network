# How to configure `CODEOWNERS` with primary and secondary owners 

This document explains how to configure a repository so that there are primary
code owners automatically requested for review and also separate secondary code
owners who are neither automatically requested nor notified but whose Approval
still unblocks merging. This setup naturally accommodates scenarios such as an
urgent PR when the primary code owners are unavailable.

## Executive summary

  * Make a Team in IOHK that is configured as "secret" and contains the desired
    secondary code owners.

  * Add the Team to the repository. (I used Write access; something weaker
    might suffice.)

  * Add a `CODEOWNERS` or `.github/CODEOWNERS` file. Every relevant line should
    include `@input-output-hk/TEAMNAME`.

  * Tick the box in the repository's Settings -> Branches -> rule(s) that
    requires a code owner Approval before a PR can be merged.

  * This Team of secondary owners will never receive any notifications, but
    their Approval will suffice for merge.

  * If a file is owned by one primary owner and more than zero secondary
    owners, a PR opened against that file by the primary owner will require
    approval from a secondary owner.

## Detailed discussion

The `CODEOWNERS` and/or Team documentation available from GitHub did not make me
confident about how to achieve such secondary owners. So I performed experiments
and finally found success with the following.

I created a new GitHub Team in the IOHK organization,
`@input-output-hk/consensus-superowners`. I marked it as "secret", so it can't
be `@`-ed and never generates notifications for its members. This is what we
want, since we're just using this Team to grant permissions. I added Duncan
Coutts (Maintainer), Edsko de Vries (Member), and Philipp Kant (Maintainer).

Because it's an IOHK Team, I can only add it to an IOHK repository. So I also
created the temporary `input-output-hk/test-CODEOWNERS` to avoid noise in a
genuine repository. I configured that repo as follows.

  * I added a Branches rule for the `main` branch and ticked the two boxes so
    that PRs require Approvals before merge and that in particular at least one
    Approval must come from a code owner.

  * I added Philipp, Pawel Szulc, and Jared Corduan to the repository as
    individuals with Write access.

  * On the `@input-output-hk/consensus-superowners` page, I added the
    `input-output-hk/test-CODEOWNERS` repository to the team, giving it Write
    access. I was able to do that, because I was the repository admin. (I think
    I also could have done this from the repo's Settings page, but maybe not
    since it's "secret".)

In the repository, I pushed up the following `.github/CODEOWNERS` file.

```
*                  @input-output-hk/consensus-superowners   @nfrisby
special_case.txt   @input-output-hk/consensus-superowners   @kantp
```

We then had a third member (thank you Pawel and Jared) open two PRs. One PR for
`dummy.txt` (ie first line) and one for `special_case.txt` (ie second line).

The `dummy.txt` PR automatically added only me as a reviewer, and I was
notified. Then we had Philipp Approve it. His Approval unblocked the PR, making
it mergable. Moreover, Philipp never received a notification. And lastly, the
fine print on the PR itself explained that the Team's Approval would also
unblock it. This is all exactly what we wanted.

The `special_case.txt` PR was testing for corner-case bugs in GH's
implementation. For this one, Philipp was notified, but we had Duncan Approve.
That also worked as expected. Note that Duncan as an individual is not even a
member of this test repository.

Finally, I created a PR editing `dummy.txt`. I'm the primary owner of that
file. No reviewers were automatically added, but the PR fine print explained
`Waiting on code owner review from input-output-hk/consensus-superowners`.

*Note about cleanup*. To delete the temporary repository, I needed assistance
from Charles Morgan, Ger Maroney (in a pinch), or Jonathan Wessels.
