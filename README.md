The [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule
[`Review.Action`](https://package.elm-lang.org/packages/lue-bird/elm-review-action/1.0.0/Review-Action)
transforms your code as specified by comments like `--!inline`.

### try it

```bash
elm-review --template lue-bird/elm-review-action/example
```

### add to your project

```elm
module ReviewConfig exposing (config)

import Review.Action
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ Review.Action.rule
    ]
```

### this should be a language server code action

But I can't be bothered to write ts.
The user experience won't be much worse
since elm-review is supported by the language server.

### future action candidates

  - ❇ [your most common refactor kind](https://github.com/lue-bird/elm-review-action/issues/new). Make sure an actual review rule isn't better suited
  - inline selected core implementations like Maybe.map, Result.withDefault or List.head
  - `declaration = {-!remove-} ...`: remove it's expose, it's @docs tag and it's implementation
  - inline values, types
      - where to put the comment so it can be recognized? On the same line as a single-line comment?
  - "backwards inline" a value/function/alias type (inline it in all places it's used)
  - `functionDeclaration argumentA {-!swap-} argumentB`: move arguments a and b in the declaration and all uses
      - elm-review does not have cross-file fixes :(
  - maybe invert if (negate condition, swap on true and on false branches)
