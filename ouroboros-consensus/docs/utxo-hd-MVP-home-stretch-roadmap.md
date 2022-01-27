[The Steps below roughly suggest blocking dependencies, but there can be more dovetailing than shown here.]

Step 1a: finish remaining code tasks

  * mempool

  * ChainDB initialization (Javier and Damian reviewing this on 27 Jan, optimistic that it'll be done)

  * ChainDB snapshotting (stop-gap: write a snapshot whenever we flush | longer term option: flush freely and snapshot at the usual pace)

Step 1b: finish the in-memory backing store

Step 1c: implement the Ledger bridge code (block -> txin, tx -> txin, passing partial UTxO to and from ledger rules)

Step 1d: update a node branch to use this new code

[As of Step 1, we can actually run the node, albeit with in-memory backing store only.]

Step 2a: some kind of local integration tests (some involving the dual ledger)

Step 2b: give concrete guidance/hot-spot warning to QA Team

  * We already suggested some emphasis regarding ChainDB initialization & snapshotting etc, on 26 Jan to Alan and Dorin

Step 2c: warm up the Benchmarking Team by having them benchmark the in-memory one

Step 2d: integrate the LMDB backing store

Step 2e: stop-gap node integration (or a proper one, if it's simple enough to add the memory-vs-disk feature flag)

[As of Step 2, we can actually run the node with an on-disk backing store.]

Step 3a: run tests with on-disk store

Step 3b: benchmarking on-disk backing store

Step 3c: give concrete guidance/hot-spot warning to Benchmarking Team

Step 3d: specific benchmark-driven question: do we need to refine the flushing and snapshotting policy?

  * We anticipate that benchmarking will demonstrate a need for this, but it's possible it will show that we could ship a stop-gap without it

  * flush freely/promptly, occasionally make pairs of ledger snapshot file and db restore point

  * this will require revisiting ChainDB initialization logic

[As of Step 3, Benchmarking and QA could start assessing our integration branch, but its alpha-quality, not a release candidate.]

Step 4a: create mergeable PRs (our code has been prototype-y so far)

  * How will this step relate to removing the dual ledger?

Step 4b: cardano-node integration (merely a couple new config options)

[As of Step 4, Consensus is "done" with the first increment, until Benchmarking and QA find problems.]

Step 5a: QA Team process

  * the UTxO HD foci should be about bulk fetch times, and successfully stopping and starting the node without having to replay from genesis

Step 5b: Benchmarking Team process

  * the UTxO HD foci should be about bulk fetch times, and successfully stopping and starting the node without having to replay from genesis

Step 6: ship the first UTxO HD release



RISK! Our PRs will ultimately involve quite a bit of code, so there's a lot of variance in how long debugging might take. Especially since we don't yet have a strong unit testing plan.

RISK! Similar concern regarding performance. As soon as we get something running and measurable, we'll have a better idea of how much optimization work remains here.

RISK! How to navigate the CAD-3672 risk?

   OPTION A: three modes: OnlyNew, OnlyOld, Both      (the CAD-3672 query doesn't work in OnlyNew)

   OPTION B: two modes: OnlyNew, Both
             and UTxO HD backing store interface offers a linear traversal

   OPTION C: two modes: OnlyNew, Both
             and we introduce a second table (the CAD-3672 index) and update ledger rules to maintain it

   OPTION D: two modes: OnlyNew, Both
             and we maintain the CAD-3672 index in-memory (either they do it in
             NewEpochState, or we do it in `LedgerState (ShelleyBlock era)`)

             for OnlyNew, we maintain the index incrementally
	     for Both, we could maintain the index incrementally or use a fast-enough linear scan (eg Alexey)

RISK! Jared confirmed during the 26 Jan UTxO HD Synch meeting that passing partial UTxO maps to the ledger should work

  * but the CAD-3672 came up as an example where it requires the entire UTxO map to answer the query. Is there anywhere else that requires the whole UTxO all at once?

  * plan to somehow codify/document which parts already support UTxO-partiality in the ledger code and which parts must continue to do so

