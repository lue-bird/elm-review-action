The [`elm-review`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review/latest/) rule
[`Review.Action`](https://dark.elm.dmy.fr/packages/elm-review-action/1.0.1/Review-Action)
transforms your code as specified by comments like `--!inline` or `--!remove`.

### try it

```bash
elm-review --template lue-bird/elm-review-action/example
```

### add it to your project

```elm
module ReviewConfig exposing (config)

import Review.Action
import Review.Rule

config : List Review.Rule.Rule
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
  - "backwards inline" a value/function/alias type (inline it in all places it's used)
  - inline values, types
      - where to put the comment so it can be recognized? On the same line as a single-line comment?
  - `| Variant {-!remove-} ...`: remove the variant declaration and the cases matching on it
      - elm-review does not have cross-file fixes
  - `functionDeclaration argumentA {-!swap-} argumentB`: move arguments a and b in the declaration and all uses
      - elm-review does not have cross-file fixes
