# Description

<!-- CI flakiness -- delete this before opening a PR

Sadly, some CI checks are currently flaky. Right now, this includes:

 - GH Actions Windows job sometimes run out of memory

 - The "Subscription.Resolve Subscribe (IO)" test sometimes fails

If you encounter one of these, try restarting the job to see if the failure vanishes. If it does not or when in doubt, consider posting in the #network or #consensus channels on Slack.
-->

_description of the pull request, if it fixes a particular issue it should link the PR to a particular issue, see [ref](https://docs.github.com/en/issues/tracking-your-work-with-issues/linking-a-pull-request-to-an-issue#linking-a-pull-request-to-an-issue-using-a-keyword=)_

# Checklist

- Branch
    - [ ] Commit sequence broadly makes sense
    - [ ] Commits have useful messages
    - [ ] The documentation has been properly updated
    - [ ] New tests are added if needed and existing tests are updated
    - [ ] If this branch changes Network and has any consequences for downstream repositories or end users, said changes must be documented in [`interface-CHANGELOG.md`](../docs/interface-CHANGELOG.md)
    - [ ] If serialization changes, user-facing consequences (e.g. replay from genesis) are confirmed to be intentional.
- Pull Request
    - [ ] Self-reviewed the diff
    - [ ] Useful pull request description at least containing the following information:
      - What does this PR change?
      - Why these changes were needed?
      - How does this affect downstream repositories and/or end-users?
      - Which ticket does this PR close (if any)? If it does, is it [linked](https://docs.github.com/en/issues/tracking-your-work-with-issues/linking-a-pull-request-to-an-issue)?
    - [ ] Reviewer requested
