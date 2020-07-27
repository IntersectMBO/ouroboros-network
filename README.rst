.. raw:: html

   <p align="center">
      <a href='https://cardano-foundation-ouroboros-network.readthedocs-hosted.com/en/latest/?badge=latest'><img src='https://readthedocs.com/projects/cardano-foundation-ouroboros-network/badge/?version=latest&token=303e7bcb5816d902dcbc690e400427d18217bfa8912d1b567348c4d95bd2d2dc' alt='Documentation Status' /></a>
      <a href='https://buildkite.com/input-output-hk/ouroboros-network'><img src='https://badge.buildkite.com/3c5e581fd69202ceddd64e91351846c41baa285aaca835cdd9.svg?style=flat-square&branch=master' alt='Build Status' /></a>
   </p>

Ouroboros-Network
=================

-  io-sim - ``IOSim`` simulator monad which supports asynchronous
   exceptions, ``STM`` transactions and ``async`` interface, timers.
-  io-sim-classes - type classes, all of them have instance for both
   ``IOSim`` and ``IO``.
-  ``typed-protocols`` - session type framework with support of
   `protocol
   pipelining <https://en.wikipedia.org/wiki/Protocol_pipelining>`__

   -  See 45min Haskell eXchange 2019
      `talk <https://skillsmatter.com/skillscasts/14633-45-minute-talk-by-duncan-coutts>`__
      by @dcoutts.
   -  See three 50min Monadic Party 2019 workshop talks by @coot: `Part
      1 <https://www.youtube.com/watch?v=j8gza2L61nM>`__, `Part
      2 <https://www.youtube.com/watch?v=oV6KSl1srL8>`__, `Part
      3 <https://www.youtube.com/watch?v=nOIQCRPwmPA>`__.

-  ``ouroboros-network``- ouroboros network package which implements
   protocols which to run ouroboros family of protocols, multiplexing
   layer.
-  The
   ```byron-proxy`` <https://github.com/input-output-hk/cardano-byron-proxy>`__
   is a network protocol proxy between Byron and Shelley. It now lives
   in a seaprate repository.

Ouroboros-Network API
---------------------

The API consisists of three layers:

• mini-protocol api's, which are GADTs for each mini-protocol under
``Ouroboros.Network.Protocol``; this hides heavy type machinery of
session types. One only needs the typed ``Peer`` type when one is using
``runPeer`` or ``runPeerPipelined`` function and each protocol exposes a
function to create it (e.g.
``Ouroboros.Network.Protocol.ChainSync.Client.chainSyncClientPeer``)

• callback ``ptcl -> channel -> m ()`` where ``ptcl`` is enumeration for
each mini-protoicol, this is either ``NodeToNodeProtocols`` or
``NodeToClientProtocols``. The callback is wrapped in
``OuroborosApplication`` GADT which allows to differentiate the
initiator / responder (or client / server) callbacks.

• versioning which is a map from version numbers to the above callbacks
and version data (the tricky part here is that version data type can be
different between different versions; there is a simple way of building
this map using a semigroup). You can use ``simpleSingletonVersion`` if
your application does not depend on negotated version data. However,
``Ouroboros.Network.NodeToNode`` and ``Ouroboros.Network.NodeToClient``
expose ``V1`` api which hides versioning from the caller.

Demo application
----------------

You can run a demo application, check
`chain-sync-demo <https://github.com/input-output-hk/ouroboros-network/wiki/Ouroboros-Network-Demo>`__
wiki page.

Tests
-----

Typed Protocols test suite
~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   cabal new-run pkg:typed-protocols:tests

or with ``nix``

::

   nix-build -A haskellPackages.typed-protocols.checks

IOSim test suite
~~~~~~~~~~~~~~~~

::

   cabal new-run pkg:io-sim:tests

or with ``nix``

::

   nix-build -A haskellPackages.io-sim.checks

Ouroboros-Network test suite
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   cabal new-run pkg:ouroboros-network:tests

or with ``nix``

::

   nix-build -A haskellPackages.ouroboros-network.checks

Ouroboros-Consensus test suite
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   cabal new-run pkg:ouroboros-consensus:tests

or with ``nix``

::

   nix-build -A haskellPackages.ouroboros-consensus.checks

Ouroboros-Consensus
===================

Consensus layer of the family Ouroboros blockchain protocols.

.. _tests-1:

Tests
-----

Consensus test suite
~~~~~~~~~~~~~~~~~~~~

::

   cabal new-run ouroboros-consensus:test-consensus

or with ``nix``

::

   nix-build -A haskellPackages.ouroboros-consensus.checks.test-consensus

Storage test suite
~~~~~~~~~~~~~~~~~~

::

   cabal new-run ouroboros-consensus:test-storage

or with ``nix``

::

   nix-build -A haskellPackages.ouroboros-consensus.checks.test-storage

Mock test suite
~~~~~~~~~~~~~~~

::

   cabal new-run ouroboros-consensus-mock:test

or with ``nix``

::

   nix-build -A haskellPackages.ouroboros-consensus-mock.checks.test

Byron test suite
~~~~~~~~~~~~~~~~

::

   cabal new-run ouroboros-consensus-byron:test

or with ``nix``

::

   nix-build -A haskellPackages.ouroboros-consensus-byron.checks.test

Shelley test suite
~~~~~~~~~~~~~~~~~~

::

   cabal new-run ouroboros-consensus-shelley:test

or with ``nix``

::

   nix-build -A haskellPackages.ouroboros-consensus-shelley.checks.test

Test infrastructure test suite
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   cabal new-run ouroboros-consensus-test-infra:test

or with ``nix``

::

   nix-build -A haskellPackages.ouroboros-consensus-test-infra.checks.test

Formatting
----------

The consensus team uses ``stylish-haskell`` >= 0.11.0.0 to format its
code. This is enforced by CI.

Either enable editor integration or run the following command to
manually format all of the consensus code (but not the network code):

.. code:: bash

   stylish-haskell -i `git ls-files -- 'ouroboros-consensus*/*.hs' | grep -v Setup.hs`

Alternatively, call the script used by CI itself:
`https://github.com/input-output-hk/ouroboros-network/blob/master/scripts/buildkite/check-stylish.sh <https://github.com/input-output-hk/ouroboros-network/blob/master/scripts/buildkite/check-stylish.sh>`__

.. code:: bash

   ./scripts/buildkite/check-stylish.sh

When using Nix, you can use the following command, which will build and
use the right version of ``stylish-haskell``.

.. code:: bash

   nix-shell --run ./scripts/buildkite/check-stylish.sh
