# Network Style Guide

This style guide is based on [consensus][consensus-style-guide].  Our guiding
principles and enforcement through PR reviews are the same, so please read the
relevant sections of that document, we just repeat here the most important
topics.  However our formatting rules are slightly different.

## Guiding principles

We value the following principles in the networking team:

* __Optimise for clarity__
* __Consistency__: inconsistent style, especially within a single module,
  looks sloppy, inspires little confidence in the quality of the code,
  and distracts. Consistency is also a helpful guiding factor when deciding
  on style guidelines in the first place; sometimes a choice between formatting
  something this way or that way seems arbitrary in isolation, but becomes
  clearer when seen as part of a coherent whole.

## Formatting

We now list the formatting rules we have converged on. As these have grown
organically, not all code follows these rules. When touching some existing code,
we in general recommend sticking to the existing style, but when it differs from
the rules below, it is good practice to update the code's style to match them.

2. __Indentation__: we indent by 2 spaces.

   *Why:* to avoid wasting horizontal screen space.

   Some consequences of this rule:

   a. The `where` clause of a function body is indented 2 spaces from the left
      margin, and the function body is indented 2 spaces from the `where`:

      ```haskell
      foo x y z =
          ..
        where
          a = ..
      ```

      The `where` keyword acts as a separator between the body and the bindings.
      Keeping them at the same indentation level would make it hard to see where
      the body ends.

      We stick with this indentation even if the `where` clause is not present,
      just to avoid unnecessary changes when a `where` clause is added.

   b. We indent record `data` and `newtype` definitions as follows:

      ```haskell
      data Foo = Foo {
            fooBar      :: Int,
            fooArgument :: Bool
          }
        deriving (Show, Eq)

      newtype Foo = Foo { unFoo :: Int }
        deriving (Show, Eq)
      ```

      The `deriving` is indented from the left margin, and the constructors
      are indented from the `deriving` clause. This provides a consistent
      style for data types with multiple constructors (see below).

      Multiple deriving clauses using `DerivingStrategies` are aligned:

      ```haskell
      data Foo = Foo {
            fooBar      :: Int,
            fooArgument :: Bool
          }
        deriving stock    (Show, Eq, Generic)
        deriving anyclass (NoThunks, NFData)

      newtype Foo = Foo {
            unFoo :: Int
          }
        deriving stock   (Show)
        deriving newtype (Eq)
        deriving NoThunks via InspectHeapNamed "Foo" Foo
      ```

      Parentheses around a singleton list of classes are optional.

      Records that fit onto a single line can be formatted like this:

      ```haskell
      data Foo = X {foo :: A, bar :: B} | Y
      ```

      We aren't very consistent with our data declarations, following the local
      style is always a good choice.

      We prefer this style also when creating terms:

      ```haskell
      let foo = Foo {
              fooBar      = ...,
              fooArgument = ...
            }
      ```
      but if terms are large (span multiple lines or use `do` notation) it might be better to use hanging `,` style:

      ```haskell
      let foo = Foo {
              fooBar      = foo bar
                                baz
                                xi
            , fooArgument = ...
            }
      ```

   c. We indent `data` definitions with multiple constructors as follows:

      ```haskell
      data Foo
        = Bar Int Int
        | Baz
            Int
            Int
            (Maybe Bool)
            [Foo]
      ```

      or

      ```haskell
      data Foo
        = -- | Bar
          --
          Bar Int Int

          -- | Baz
          --
        | Baz
            Int
            Int
            (Maybe Bool)
            [Foo]

      ```

      Note the argument of `Baz` being indented by two spaces.
  
   d. Both of the following are fine

      ```haskell
      let fooBarBaz = fooBar
                        baz

      let fooBarBaz =
            fooBar baz
      ```

      whichever is more natural.

      In the rare case that you want a `where` clause on a `let` binding, indent
      by 4 spaces, like described in (a):

      ```haskell
      let fooBarBaz =
              ..
            where
              aux = ..
      ```

   e. `do` is placed after the `=`:

      ```haskell
      foo .. = do
        bar
        baz
      ```

      Function calls can be placed on the same line as the `do`, unless this
      would make the line too long:

      ```haskell
      foo .. = atomically $ do
        bar
        baz

      -- If the first line too long:
      foo .. =
        atomically $ do
          bar
          baz
      ```

      The `where` block can be indented by 2 spaces, as described in (a).

      In a `case`, use hanging `do`:

      ```haskell
      case foo of
        X -> do
          ..
        Y -> do
          ..
      ```

   f. While it is technically possible to add a `where` clause to a pattern
      match case, use a `let` instead, to emphasise that the binding is local:

      ```haskell
      case x of y
        A x -> A_body
        B y ->
          let bl = bl_body y
          in B_body
      ```

      Note that we align `B_body` with `bl` in the `let` block. At the moment we
      are not being very consistent with this.

      Using a `where` clause for a `case` can be okay, but tends to make the
      scope a bit confusing, so we try to avoid it.

2. __Line length__: we limit the number of characters per line to 80.

   *Why:* long lines are less readable (there is a reason why books and
   newspapers limit their line length). It's also practical: even with
   (ultra-)wide monitors, most people tend to have many windows side by side.

   If you are going beyond 80 characters, wrap the line, introduce local
   bindings, etc.

   Comments and docstrings should also be wrapped at 80 characters.

   There are a few exceptions:

   * Sometimes alignment trumps line length. When many lines are aligned and a
     few of them are too long because of that, the clarity that comes from
     alignment (emphasising differences and similarities) can outweigh the line
     length limit.

   * Diagrams, examples, or long URLs in the comments can be wider than 80
     characters.

   For certain constructs we have concrete recommendations on how to wrap them
   in case their length exceeds 80 characters:

3. __Type signatures__

   a. if a type signature doesn't fit on one line, wrap it like
      this:

      ```haskell
      fooBar
        :: a
        -> ..
        -> ..
      ```

      When there are constraints:

      ```haskell
      fooBar
        :: ( Eq a
           , ..
           )
        => a
        -> ..
        -> ..
      ```

      Using multiple `=>` is also ok, e.g.

      ```haskell
      fooBar
        :: forall a.
           Eq a
        => Ord a
        => a
        -> ...
      ```

      When there is an explicit `forall`:

      ```haskell
      fooBar
        :: forall a .. z.
           ( Eq a
           , ..
           )
        => a
        -> ..
        -> ..
      ```

      Note that the `.` after the `forall` stays on the same line and that there
      is no space before it.


      If there is a large function argument in the type signature:

      ```haskell
      fooBar
        :: a
        -> (   forall c.
               Eq c
            => c
            -> ..
            -> ..
           )
        -> ..
      ```

      Note that the first arrow in the function argument is indented one space
      relative to the opening parenthesis. The above line wrapping rules apply
      to the nested function type as well.

   b. Function calls: when not all arguments to a function call fit on a single
      line, either introduce clear local bindings for the arguments or put each
      argument on a separate line, indented 2 spaces from the function call:

      ```haskell
      fooBar
        x
        (baz + 1)
        bar
        (foo (bar x))
      ```

      *Why*: multiple lines of multiple arguments are hard to read; for example,
      in

      ```haskell
      fooBar
        x (baz + 1)
        bar (foo (bar x))
      ```

      it might look like `bar` is applied to `(foo (bar x))`, whereas in fact
      of course they are both just two arguments to `fooBar`. So, either
      everything on the same line as the function call, or else a line
      per argument.

      When writing a function call in the applicative style that does not fit on
      a single line, indent it as follows:

      ```haskell
      fooBar
        <$> x
        <*> baz + 1
        <*> bar
        <*> foo (bar x)
      ```

   c. Argument lists: put the formal arguments of a function on a single line
      when possible:

      ```haskell
      foo a b (SomeRecord {field = x}) =
          ..
      ```

      Bracketing a pattern match on a record is optional, but we feel it aids
      clarity.

      When that does not fit on a single line, move any pattern matches to a
      `where` block:

      ```haskell
      foo a b c =
          ..
        where
          SomeRecord {field = x} = c
      ```

      When that is still not enough, then the function has so many arguments
      that *naming* them is not only useful for alignment, it also helps to
      clarify call sites: introduce a record.

      ```haskell
      foo args =
          ..
        where
          Args {
              argA = a,
              argB = b,
              argC = SomeRecord {field = x}
            } = args
      ```

   d. Using `RecordWildCards` for unpacking large records is discuraged.

   e. Class or instance contexts: when a class or instance declaration doesn't
      fit onto a single line because of the super-class context, wrap the line
      before the `=>` and align the class name with the first character in the
      context:

      ```haskell
      class ( Eq a
            , ..
            )
         => C a where

      instance ( Eq a
               , ..
               )
            => C a where
      ```

   e. Tuples in type signatures:

      ```haskell
      foo ::
           a
        -> ( ..
           , ..
           )
        -> ( ..
           , ..
           )
      ```

   f. Datatypes:

      ```haskell
      data Foo =
          Bar
           Arg1
           Arg2
           ..
           ArgN
        | Baz

      data Foo = Foo
          { longFieldName
              :: HasCallStack
              => Int
              -> ..
          }
      ```

   g. Type synonyms:

      ```haskell
      type Foo a b =
        AVeryLongTypeHereAndItKeepsGoing
          Arg1
          (Maybe b)
          Arg3

      type Cts a =
        ( Eq a
        , ..
        , ..
        )
      ```

   h. Function composition:

      ```haskell
      foo = h
          . g
          . f
      ```

      *Why*: The alignment of the `.`s and the function names makes the
      structure easy to see at a glance.

      This generalises to other binary operators, e.g., `+`, `*`, etc.

4. __Parentheses__: avoid redundant parentheses, except when they help with the
   order of operations. Use your judgement, and aim for clarity. Redundant
   parentheses sometimes help the reader, but sometimes confuse as they
   might suggest that they are there to disambiguate something whereas in fact
   there is nothing to disambiguate.

   ```haskell
   -- NO
   foo (Bar x) = (Bar (succ x))
   -- YES
   foo (Bar x) = Bar (succ x)

   -- NO
   ((x + y), z)
   -- YES
   (x + y, z)

   -- OKAY
   (fromIntegral x) * y
   ```

5. __Spaces__: surround binary operators with a space on each side. A comma is
   *always* followed by a space.

   *Why:* this is a general convention that is also used in text and math books.
   Not doing so makes it harder to read and is sloppy.

   ```haskell
   avg x y = (x + y) / 2

   let ((x, y), z) = foo
   in (y, z)
   ```

   The only exception is in tuple sections:

   ```haskell
   (,) <$> foo <*> bar
   (True,) <$> foo
   ```

6. __Function composition and the dollar operator__:

   Choose between using parenthesis, `$` and `.` in whichever way you think
   results in the most readable code.

7. __Blank lines__
   Keep two empty lines between top level definitions.

   This is natural and useful especially when one is using a single empty
   lines to delimit code sections inside a longer code block.

   ```haskell
   data Foo = Foo


   foo :: Foo -> ()
   foo Foo = ()
   ```

   Use this wisely, sometimes it makes sense to use a single empty line, for
   example to delimit short instances which all refer to the same data type.

   Sometimes it's more logical too keep `Foo` and `foo` close together but
   `Bar` more distinct, e.g.

   ```haskell
   data Foo = Foo

   foo :: Foo -> ()
   foo Foo = ()

  
   data Bar = Bar

   bar :: Bar -> ()
   bar Bar = ()
   ```

   Always end a file with a *newline*, which is not the same as a blank line.
   ```
   -- NO
   ..

   <EOF>

   -- NO
   ..<EOF>

   -- YES
   ..
   <EOF>
   ```
   *Why:* see [this StackOverflow answer][posix-line], moreover, GitHub will
   highlight a missing newline at the end of the file.

   [posix-line]: https://stackoverflow.com/questions/729692/why-should-text-files-end-with-a-newline#answer-729795

8. __Sections__: we group related definitions in sections that start with a
   section title. The same grouping can be replicated in the export list.

    ```haskell
    module AmazingModule
      ( -- Foo
        Foo (..)
      , mkFoo
        -- Bar
      , ..
      ) where

    --
    --  Foo
    --

    data Foo = ..

    mkFoo :: ..

    ..

    --
    -- Bar
    --
    -- Bar is bla bla
    --

    type Bar = ..
    ..
    ```

    The two lines of the section header are each 80 characters in total. The
    title is indented by two spaces. The section header can contain more text,
    which is separated from the first line by one empty comment line. The
    section header has a single blank line above and below it.

9. __Comment style__: in general we tend to use `--` instead of `{- .. -}`. We
   sometimes make exceptions for big non-Haddock comments.

10. __Haddock formatting__: we use [Haddock formatting][haddock-formatting] in
    docstrings. We also do this in comments for consistency.

    ```haskell
    -- | Short title
    --
    -- Longer description .. 'Foo' .. "Data.String" .. @a@ .. /not/ ..
    -- __never__ .. called \"foo bars\" .. alternative style " foo bars "
    -- .. @'Foo' a@
    --
    -- > foo bar baz
    --
    -- more documentation, and an empty comment line before the declaration.
    --
    foo :: ..
    ```

    Note the space before and after the `|`. We do not align the following lines
    with the first character of the `|`.

    Haddock treats something between double quotes as a link to a module. So
    when you try to quote something, either use backslashes or extra spaces as
    in the example above.

    We prefer `-- |` over `-- ^`. We only use the latter when documenting the
    arguments to a constructor or a function:

    ```haskell
    foo
      :: Word -- ^ Max size
      -> ..

    data Foo
      = -- | Foo
        --
        -- ..
        Foo
          Int          -- ^ @x@
          (Maybe Bool) -- ^ .. long line ..

        -- | Baar
      | Baar
    ```

    Note the indentation of `-- |`, the two spaces before the `-- ^`, and the
    blank line between the constructors.

    Note that we leave an empty comment line as the last haddock or comment
    line.

    We often document preconditions, invariants, and postcondition using the
    following style:

    ```haskell
    -- | Foo
    --
    -- PRECONDITION: x must be greater than y
    -- > x > y
    --
    -- POSTCONDITION: the result will be positive
    foo :: ..

    data Foo = Foo {
          -- | The bar ..
          --
          fooBar :: Int,

          -- | The baz ..
          --
          -- INVARIANT: 'fooBaz' is always greater than 7
          --
          fooBaz :: Int
        }
    ```

    [haddock-formatting]: https://www.haskell.org/haddock/doc/html/ch03s08.html

11. __Alignment__: we align things when it helps with readability.

    Alignment makes it clear which things are the *same* and which things are
    *different*, compare the following code block

    ```haskell
    foo (Quux a b c) = bar a b c
    foo (Bar b c) = bar [] b c
    foo (FooBar a c) = bar a [] c
    ```

    with the aligned version:

    ```haskell
    foo (Quux   a b c) = bar a  b  c
    foo (Bar      b c) = bar [] b  c
    foo (FooBar a   c) = bar a  [] c
    ```

    Alignment makes it easier to spot errors. For example, compare the two code
    blocks, where the `c` argument is forgotten on the second line:

    ```haskell
    foo (Quux a b c) = bar a b c
    foo (Bar b c) = bar [] b c
    foo (FooBar a c) = bar a []
    ```

    ```haskell
    foo (Quux   a b c) = bar a  b  c
    foo (Bar      b c) = bar [] b  c
    foo (FooBar a   c) = bar a  []
    ```

    It is immediately obvious in the aligned code, but not in the unaligned
    code.

12. __Pattern guard alignment__:

    This is one area in which we have not yet converged on a single style,
    and there are two styles in use:

    ```haskell
    foo x y z
        | x == y
        = ..
        | Just z' <- z
        , z' == x
        , let x' = ..
        = .. x'
        | otherwise
        = ..
      where
        ..
    ```

    versus

    ```haskell
    foo x y z
      | x == y =
          ..
      | otherwise =
          ..
      where
        ..
    ```

    Similarly for `case`:

    ```haskell
    case mX of
      Just x
        | x > 100
        -> ..
        | x > 0
        -> ..
      _otherwise
        -> ..
    ```

    versus

    ```haskell
    case mX of
      Just x
        | x > 100 ->
            ..
        | x > 0 ->
            ..
      _otherwise ->
        ..
    ```

    Choose whichever style you prefer. The latter style is more suitable for
    hanging `do`.

    In either style, use of `_otherwise` instead of `_`, as the latter is
    easy to miss.

13. __case vs function with multiple clauses__:

    The choice between using a `case` and having multiple clauses of the
    function can help emphasise the structure of the code, and the differences
    and commonalities between the cases.

    ```haskell
    foo acc visited = \case
        []   -> ..
        x:xs -> ..
    ```

14. __if-then-else__:

    When using `if-then-else` in combination with `do`, follow the following
    style:

    ```haskell
    if foo then do
      bar
      baz
    else do
      quux
      bar
    ```

    *Why:* to avoid wasting horizontal screen space.

15. __Import lists__: we use `stylish-haskell` to automatically format import
    lists. See the `.stylish-haskell.yaml` file in this repo.

    We prefer the following order of import groups that has to be maintained
    manually:

    ```haskell
    -- Prelude
    import Prelude hiding (..)

    -- base + third-party non-Cardano packages
    import Control.Monad (mplus)
    import Data.Text (Text)
    import Data.Text qualified as Text
    import NoThunks.Class (NoThunks)

    -- cardano-prelude
    import Cardano.Prelude (forceElemsToWHNF)

    -- ouroboros-network and other network packages,
    -- each namespace in a separate group
    import Ouroboros.Network.Block (Serialised)
    ```

    Each group is of course optional and must *not* be preceded by the comment
    like in the example above.

    The idea behind the ordering is to start with the most general
    packages/modules and then go more and more specific, ending with the local
    package. In general, an import group will only depend on packages in import
    groups *above* it, *not below* it. For example, the network layer import
    group comes *before* the consensus import group, as the latter depends on
    the former. The Shelley ledger import group comes before the Shelley ledger
    consensus integration import group. In case of ties, i.e., when multiple
    import groups don't depend on each other, we have no real preference. We do
    put Byron before Shelley.

    When importing modules from consensus and in particular modules from the
    same package, an import list and a qualifier can be omitted. For example,
    importing `Ouroboros.Consensus.Block` is often done without an import list
    as it brings many basic definitions that are relied upon in scope.

    When importing from other packages, we prefer to use either an import list
    or a qualifier.

16. __Export lists__: we format export lists in the following way:

    ```haskell
    module X
      ( ..
      , ..
      ) where
    ```

    We sometimes use Haddock headings:

    ```haskell
    module X
      ( -- * Foo
        ..
        -- ** Foo Bar
      , ..
        -- * Bar
      , ..
      ) where
    ```

    When exporting something with members, e.g., a datatype with
    constructors or a class with methods, we format them in the following way
    (note the space):

    ```haskell
    module X
      ( Foo (..)
      , Bar (MkBar)
      ) where
    ```

    *Why:* this is consistent with how `stylish-haskell` formats it when
    importing it. (We are not particularly consistent with this at present,
    however.)

    When intentionally hiding the constructor of a datatype or newtype, we add
    a `-- opaque` comment after it in the export list to be explicit about this:

    ```haskell
    module X
      ( Foo -- opaque
      ) where
    ```

    *Why:* otherwise, people unfamiliar with this type might be tempted to
    export its constructor without realising they're hidden for a reason. This
    comment should make them (and the reviewer) think twice.

    When re-exporting several modules from one module, use the following pattern:

    ```haskell
    module Foo (module X) where

    import Foo.A as X
    import Foo.B as X
    import Foo.C as X

    ```
    *Why:* one can add extra imports without having to modify the export list.

17. __Syntactic extensions__: we like to use some syntactic language extensions.
    Some argue against having to learn additional syntax, but we believe the
    learning curve is minimal and using them can help improve the clarity of the
    code.

    We like to use `LambdaCase` to avoid giving intermediate results a redundant
    name:

    ```haskell
    -- OKAY
    mFoo <- getFoo
    case mFoo of
      Nothing  -> ..
      Just foo -> ..

    -- OKAY
    getFoo >>= \case
      Nothing  -> ..
      Just foo -> ..
    ```

    In the second snippet, there was no need to name the intermediary `mFoo`
    result. Especially when its name is long or coming up with a reasonable name
    for it is tricky, we recommend using `LambdaCase`.

    The use of `MultiWayIf` is also recommended when it improves the
    readability:

    ```haskell
    if | Set.member pt prevApplied -> Just True
       | Map.member hash invalid   -> Just False
       | otherwise                 -> Nothing
    ```

    In our opinion, this is more readable than alternatives like:

    ```haskell
    if Set.member pt prevApplied then Just True else
    if Map.member hash invalid   then Just False else
                                      Nothing
    ```

18. __Records__:

    For records we often use `NamedFieldPuns` to make it convenient to
    extract fields from the record. We also discourage the use
    `RecordWildCards` when the fields of the record are all of the form

    ```haskell
    data SomeRecord = SomeRecord {
          someRecordA :: ...,
          someRecordB :: ..
        }
    ```

    which is a naming convention we use a lot to avoid duplicate record fields
    (we do not use `DuplicateRecordFields`).

    To avoid long lines, it is sometimes useful to use record deconstruction in
    local bindings:

    ```haskell
    foo someRecord =
        ..
      where
        SomeRecord {someRecordA, someRecordB} = someRecord
    ```

    We try to avoid partial fields, but replacing partial fields such as

    ```haskell
    data Foo = FooX {foo :: A, bar :: B} | FooY
    ```

    with

    ```haskell
    data Foo = FooX A B | FooY
    ```

    is _not_ an improvement: replacing record field names with positional
    arguments is a big loss in clarity. Instead, introduce a record to be used
    as an argument to the `FooX` constructor.

    ```haskell
    data X = X {foo :: A, bar :: B}
    data Foo = FooX X | FooY
    ```

19. __Pointfree__: Use your judgement when to use pointfree style and when not
    to use it; aim for clarity.

20. __Warnings__: we use the following warnings for each Cabal component:

    ```haskell
    -Wall
    -Wcompat
    -Wincomplete-uni-patterns
    -Wincomplete-record-updates
    -Wpartial-fields
    -Widentities
    -Wredundant-constraints
    -Wmissing-export-lists
    -Wunused-packages
    -Wno-unticked-promoted-constructors
    ```

    *Why:* the warnings produced by the above list of flags signal code smells
    or enforce good practices. There is seldom a reason to disable one of them.
    At the time of speaking, we haven't needed any CPP yet to accomplish this.

    We also keep the code entirely warning free; doing this consistently and
    without exception means that important warnings don't get lost. We enforce
    this by using `-Werror` in CI.

    We sometimes make exceptions for test code, e.g.,
    `-Wno-incomplete-uni-patterns`.

    For consistency, always use `-Wx` and `-Wno-x` instead of `-fwarn-x` and
    `-fno-warn-x`.

    Most often (whenever we can), we use `-Wno-unticked-promoted-constructors`.
    This allows to use unticked promoted data contructors as types.

21. __HasCallStack__: when using `error` in code paths should be impossible and
    are indicative of bugs, make sure enough `HasCallStack` constraints are in
    scope so that the error message will result in a useful callstack.

    Note that `HasCallStack` constraints on record fields will need manual
    wrappers to work properly:

    ```haskell
    data API m = API {
          foo_ :: HasCallStack => Maybe a -> m a
        }
    foo :: HasCallStack => API m -> Maybe a -> m a
    foo = foo_
    ```

    Without the extra wrapper `foo`, the call stack would only start at `foo_`,
    which is rather useless.

22. __Ambiguous types__: we avoid `AllowAmbiguousTypes`. Instead, we add a
    `Proxy` argument for the ambiguous type variable.

    *Why:* this makes it explicit which type variable is ambiguous.

    When passing the `Proxy`, use `Proxy @X` where `X` is the concrete type.

    *Why:* this is less verbose than `Proxy :: Proxy X`.

    Generally try to avoid type applications, as they are rather brittle: if the
    type arguments to the function change order, suddenly a function call might
    no longer work, often with a hard to understand error message. This gets
    even worse when a function doesn't have an explicit `forall`, and so the
    order is not even specified. Prefer to use `Proxy`, possibly by introducing
    some auxiliary functions.

    When the same `Proxy` can be used multiple times, one can define it locally
    like so:

    ```haskell
    pb :: Proxy blk
    pb = Proxy
    ```

23. __Redundant pragmas__: remove unused language pragmas when possible.

    *Why:* if a module lists the `CPP`, `AllowAmbiguousTypes`,
    `UndecidableInstances`, or any other suspicious extension, it triggers an
    unnecessary red flag. Even for harmless extensions, it is good practice to
    remove unused ones.  If such extensions are needed, it's good to document
    why they are need (with an exception of `CPP`).

    *Tip:* HLint can warn you about some unused pragmas.

## Guidelines

There are more general guidelines on how we write and structure code.

1. __Scope__: We try to be careful about scope, clarifying where a variable is
   relevant and where it is not. For example, in

   ```haskell
   foo x y z =
       ..
     where
       ..
   ```

   all of `x, y, z` will be in scope in the `where` clause. If they aren't
   relevant, limit their scope:

   ```haskell
   foo x = \y z ->
       ..
     where
       ..
   ```

   this also can help to avoid awkward variable names.

   Similarly, choosing `where` over `let` can help to clarify which variables
   are in scope in those definitions. Writing

   ```haskell
   foo x = do
       ..
     where
       y = ...
   ```

   makes it very clear that the definition of `y` does not depend on anything
   that has happened within the `do` block (it depends only on the formal
   parameters of the function). The flipside of this is that `y` is then scoped
   over the entire function body; that typically is less likely to result in
   confusion (especially for local function definitions), but if it is useful to
   emphasise that `y` is only used in a small part of the function body, or that
   this avoids awkward naming, then feel free to use `let` to express that. Use
   your judgement: use scope wisely.

2. __Tuples__: We generally prefer records over tuples with lots of arguments;
   positional arguments (in tuples or as arguments to constructors) provide
   less clues what the arguments are used for.

3. __Orphans__: Orphans are generally considered bad practice, but unfortunately
   avoiding orphans at all cost often means being unable to split a module into
   smaller parts. The reason orphans are considered bad practice is that they
   might lead to incoherence; we prefer the ability to split modules into
   smaller parts and accept the loss of the help of the compiler to avoid
   incoherence as an acceptable compromise.

   Orphans in test suites are also acceptable.

4. __Assertions__: If it helps to explain what a function does, we try to be
   clear about preconditions, postconditions, and invariants. When possible, it
   is useful to reinforce such invariants with assertions so that if our
   reasoning turns out to be invalid, we will notice. The use of
   `Ouroboros.Consensus.Util.Assert.assertWithMsg` is preferred over `assert`,
   so that if the assertion fails, we get some kind of informative error message
   rather than just a Prolog-like "no".

5. __Test packages__: In order to make test code from test suite A available
   in test suite B, we define test suites as a test suite library which is
   then used by a test suite executable; the test suite library can then
   be reused by other test suites as well.

   When there are multiple test suites within a single package, it is possible
   to share some code between them by making the same source code directory
   available to all of them. However, doing so would make it impossible to
   use that shared code in a test suite defined in another package. To avoid
   this problem, we avoid sharing source directories in `cabal` files.

[consensus-style-guide]: ../ouroboros-consensus/docs/StyleGuide.md
