# Consensus git process

This document describes the git history we strive for in the Consensus Team. The
git history is a key asset for understanding and for debugging, so we take care
to refine it before merging each PR.

As part of each PR review, we also check for consistency with the content of
this document. We find that the rules herein become familiar and intuitive after
some use: eventually it'll just be a document you refer to only occasionally.
But we don't expect the first several PRs to perfectly adhere to these rules. So
please make an effort, but don't worry too much: our highest priority is to see
your PR's content! We'll help tidy up any deviations.

## Guiding principles

_Messages matter_: Commit messages summarize an atomic coherent change and its
motivation. They help reviewers do their job, future developers understand the
past design process, troubleshooters debug changes in behavior, downstream
developers understand recent changes and their context, and so on. In these kind
of scenarios, commit messages are often the easiest documentation to read first.

_Small and organized commits_: The commits themselves, beyond just their
messages, also contribute to the story in their own way. Prefer small commits
that include only necessary changes, and choose a commit order that seems most
natural to you. Doing so often also makes it easier to write useful commit
messages. Rebase before merging, so the repository's full story is as linear as
it can be. At the very least, this effort will make it easier for your teammates
to review the PR.

_Never break the build_: The code should build and pass all test suites after
each commit, not just after each PR. There is no good reason for a commit to
break the build. It is only slightly less important that the tests pass after
each commit. The Continuous Integration (CI) machinery ensures that the tip of a
PR builds before it's merged, so every commit on the `master` branch will build
and pass the tests that are enabled in CI. In particular, this process ensures
`git bisect` can be as useful as possible -- when you reach for that kind of
sledgehammer, you want it to work well.

Note that CI enables `-Werror`, but we don't require you to enable it locally.
If you'd like to opt-in to that or similar, add a `cabal.project.local` file.

## Guidelines

  * Write useful commit messages. There are lots of guides on the Internet about
    how to do that. We don't have any strict guidelines beyond appreciating the
    value of a good commit message and appreciating their permanence.

  * We only merge via GitHub PR. Moreover, we favor the command `bors r+` or the
    equivalent `bors merge` as the final step on a PR, when the `bors` bot is
    behaving. Usually it's the PR author that issue that command.

  * The tip of the `master` branch is always the latest and greatest we have to
    offer, closer to our current primary goals. As mentioned above, it should
    build clean and pass our test suites. We do also rely on some additional
    and/or external testing that is only done near release points.

  * GitHub copies the PR title and description into the merge commit, so the PR
    title and description should be useful and meaningful. Don't use the default
    title.

    If you want to initiate discussion on the PR (eg "This is the direction I
    come up with, what do you think?"), do so in the first comment, immediately
    after opening the PR -- we don't want that recorded in the merge commit.

  * If the PR resolves an Issue, the PR description should start with `Fixes
    #NNNN`. See the [GitHub documentation][gh-auto-link-issue] for similar
    keywords and syntax that will trigger useful automatic behaviors.

    [gh-auto-link-issue]: https://docs.github.com/en/github/managing-your-work-on-github/linking-a-pull-request-to-an-issue#linking-a-pull-request-to-an-issue-using-a-keyword

  * Your PR should be a [Draft PR][github-draft-pr] until you think it is ready
    for final review and merging. I often open my PR as Draft PR in order to get
    early feedback and then later promote it to a proper PR. Or if I realize
    there is big hole in my PR, I will convert it back into a Draft PR.

    This is far superior to including "WIP" or "DO NOT MERGE" in the PR title,
    or a WIP label, etc.

    [github-draft-pr]: https://github.blog/2019-02-14-introducing-draft-pull-requests

  * Your branch name is impermanent, so it's less important than the above.
    However, we prefer the format `username/issue-NNNN-short-description` or
    `username/JJJ-MMMM-short-description` to indicate where the work is being
    tracked by one of our lower level GitHub Issues (`NNNN`) and/or IOHK's JIRA
    issues (`JJJ-MMMM` where the `JJJ` is the acronym of the JIRA project, eg
    `CAD`).
