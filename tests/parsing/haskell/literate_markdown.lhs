# A literate module in Markdown

This file follows the markdown-unlit convention used by servant's
cookbook: only the fenced `haskell` blocks are compiled.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module LiterateMarkdown where

import Data.Text (Text)
```

Some prose with > a quote and a `#` heading below.

## Definitions

```haskell
greet :: Text -> Text
greet name = "hello " <> name
```

``` haskell ignore
this block is not compiled = = =
```

``` haskell
shout :: Text -> Text
shout = greet
```

```shell
$ runghc LiterateMarkdown.lhs
```
