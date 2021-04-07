# IOHK-level access to the Consensus Layer

The HR will run separate "on-boarding" that will help you to get to know
the company better. The purpose of this document is to make you feel comfortable
and productive in the Consensus team.

Welcome, we are thrilled to have you here :)!

# What to do on my first week?

## Meet the team

Currently the team consists of:

* [Nicolas Frisby](https://github.com/nfrisby)
* [Pawel Szulc](https://github.com/EncodePanda)
* [Edsko de Vries](https://github.com/edsko) (8 hours a week)

### Join our slack channels

Please join our internal slack channel `#ouroboros-prototyping` and say "hi"
to the rest of the team.

It is worth also joining:

* `#haskell` - collective mind of all IOHK's Haskellers
* `#consensus` - public channel where users of our layer will ask questions

## Clone the repository

If you have not done this already please clone
[this repo](https://github.com/input-output-hk/ouroboros-network/). You don't have
to fork it.

Work with your IOHK manager and IOHK DevOps to acquire permissions to push
branches to and open PRs in this repository. Also acquire permissions to
Approve PRs (in particular, our `bors` bot must accept your commands).
You'll eventually also need to be in the `CODEOWNERS` file.

## Create your first PR!

* Create new branch - convention is `/github_nickname/meaningful_name`
* Modify `ouroboros-consensus/docs/IOHKOnboarding.md` by adding yourself to the `## Meet the team` section
* Commit, push and make a PR
* Find some one to review it by the end of the day
* Merge it! - note that we use `bors` to merge our PRs
    * Issuing `bors` commands requires certains permissions.
    * First, someone with permissions to do so must give you write access in
      [the repository
      settings](https://github.com/input-output-hk/ouroboros-network/settings/access).
    * Then, someone with permissions to do so must click the "Update" in [the
      `bors` settings](https://bors-ng.aws.iohkdev.io/repositories/8/settings).

The general IOHK onboarding should have discussed establishing hardware-based
Two Factor Authentication on your GitHub account; that's another prerequisite.

We're currently bridging the gap with both GitHub Issues and tickets in IOHK's
JIRA. For now, we are therefore also mentioning JIRA Issues such as `CAD-NNNN`
in issues' and PRs' titles and/or descriptions.

## Build the project!

Please follow instructions of the "Quickstart" section of the [Onboarding.md](Onboarding.md)

## Familiarize yourself with the Consensus project

* Read the "Very High-Level Motivation" of the [Onboarding.md](Onboarding.md)
* Read the Technical Report

# When in doubt

1. Don't hesitate to ask questions
2. Pair with other team members as much as you can
