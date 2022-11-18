---
title: "Texty: Composable, partial, fused, parse-transform-render traversals"
author: Guo Liang Oon
tags: haskell
description: "Part 2: How it works"
---

## how it works
### let's start with the types
Even though we are also *rendering*, we will still be *parsing*, so we need to keep track of unconsumed input.
```haskell
data Context = Context Text
```

Since we want optics composability for `lens` we need to keep track of unconsumed input in the completely polymorphic type parameters. So something like:
```haskell
type P p f s a = Optic' p f (s, Context) (a, Context)
type Optic' p f s a = Optic p f s s a a
type Optic p f s t a b = p a (f b) -> p s (f t)
```

Now which `lens` type correspond to a parse-render? We know 2 things:

- a parse may fail
- we can render a parsed item back into `Text`

That's a `Prism`! A prism can `preview` to parse, which fails with `Nothing`, and `review` to render. So we define:
```haskell
type PPrism s a = forall p f. (Choice p, Applicative f) => P p f s a
```

Why not specialise `s` to `Text` since we are parsing from `Text`? Well, this is because of the partial nature and nested structure of our parsing. Say, if we parse a `Text` into `Header Int Text`, we might be interested in further parsing *inside* the `Text` of the `Header`. So we would want to be able to compose `PPrism Text Header` with `PPrism Header Italic`, where in the latter `s ~ Header`.

But a prism is too specific. A common combinator in parser combinator libraries is `many` which promotes a `Parser a` into `Parser [a]`, repeatedly parsing the same thing in sequence.

Now we can do `PPrism s [a]`, but is that really correct? If the list is empty the prism should fail on `preview`, not return an empty list. Also to compose properly, let's say to do
```haskell
PPrism Text [Header] -> PPrism Header Italic -> PPrism Text [Italic]
```

requires more than just the `(.)` of function composition, and "contaminates" the type of the italic parser with lists, making *it* in turn less composable.

But there is a more natural `lens` type to represent having multiple targets: `Traversal`. So we can define:
```haskell
type PTraversal s a = forall f. (Applicative f) => P (->) f s a
type P p f s a = Optic' p f (s, Context) (a, Context)
```

Note that by typeclass constraint polymorphism, a prism is a special case of a traversal, so whatever combinators we define for a traversal can also handle a prism. But in certain situations where we know something to be a prism, we may be to do more. More on this in a later.

### combinatorial APIs
There is a question of [covariance/contravariance](https://www.fpcomplete.com/blog/2016/11/covariance-contravariance/) and how it relates to combinatorial APIs.

With such an API you want to compose `(a,a) -> a`. But what if it is not always exactly `a`? Then it matters what is on the left or right of the function arrow. If we do
```haskell
(PPrism, PPrism) -> PPrism
```
then we cannot take `PTraversal` as argument, but we return a stronger type, since a `PPrism` can be then passed along to another combinator which takes `PTraversal`s. So ideally we would want to take a weaker type and return a stronger type:
```haskell
(PTraversal, PTraversal) -> PPrism
```
But that is nonsensical as traversals can have multiple targets and prisms have at most one. So the best option then seems to be
```haskell
(PTraversal, PTraversal) -> PTraversal
```

So our primitive parser-renderers are prisms, but everything becomes a traversal once you use it with a combinator. But that's fine because all our combinators can work with traversals. All good.

### primitives
Before we have anything to compose we need to define our primitive parser-renderers.
```haskell
-- lifts a `Parser a` and a builder `a -> Text` for it to a `PPrism Text a`
pPrism :: Parser a -> (a -> Text) -> PPrism Text a
pPrism parse render = prism' build match
  where
    match = parseInContext parse
    build (a, ctx) = (render a, ctx)

-- lifts a `Parser a` to a matcher for a `PPrism Text a`
parseInContext :: Parser a -> (Text, Context) -> Maybe (a, Context)
parseInContext p (input, (Context after above lvl)) = eitherToMaybe $
  parse (contextualise <$> p <*> getInput) "" input
  where
    contextualise parsed unconsumed = (parsed, Context (unconsumed <> after) above lvl)
    eitherToMaybe e = case e of
            Left _ -> Nothing
            Right x -> Just x
```
- Lifting a `Parser a` simply involves taking care to preserve its unconsumed input in the `Context`. 
- The inverse operation of rendering simply involves printing the parsed value back into `Text`.
- We discard error messages. Integrating them is a future project.
- Nevermind the two additional arguments to `Context` for now. We'll get to that later.

### composition
Now on to composition. Of which there are two kinds:

- "horizontal" left-to-right composition
	- this is the same as ordinary parser composition, except of course we need to take care of the inverse render operation too
- "vertical" top-down composition
	- this is where the "partial" in partial parsing comes in
	- e.g. we can parse-render a header, or we can parse-render the italic inside the header

Note that vertical composition is a case of recursion. We parse from `Text` into an `a`. But the parse may be partial, in which case `a` also has within itself some `Text`, which is again a recursive instance of our parse-render problem.

Similarly to how top-level horizontal composition needs to keep track of unconsumed input, so does child-level horizontal composition. But here is where things get a little complicated.

Call our horizontal left-to-right combinator `||>`. To construct `left ||> right`, we must pass `left`'s unconsumed input to `right`. And in order to get `left`'s unconsumed input we need to "run"/parse it, which in the case of our design is running a prism or a traversal to produce their "child" targets. 

Here is the problem with vertical composition, which we will call `focus`. If we do `up . focus target . down` then down has its own horizontal unconsumed input context, but not its parent's or ancestors'. So we must define `Context Text [Text]`, where `[Text]` is a stack of ancestor unconsumed inputs.

Actually even that is not enough, since we have the option of doing `||>` at multiple levels. Consider

- `(header 1 . focus title . italic) ||> strikethrough` 
	- which would parse `# *i*\n~~s~~`
- `header 1 . focus title . (italic ||> strikethrough)`
	- which would parse `# *i ~~s~~\n`

So additionally, we also need to keep track of which level we are `focus`ed at when we apply `||>`.

So we arrive at our final definition of `Context` as:
```haskell
data Context = Context Text (Vector Text) Int
```
where the components are respectively:

- unconsumed input at current level
- stack of unconsumed inputs at ancestor levels
- index of current level

### vertical composition: focus
Let's start with the easier definition, for vertical composition:
```haskell
focus :: Traversal' s Text -> PTraversal s Text
focus at afb s@(_, ctx@(Context unconsumed above lvl)) =
  ...
```
As a reminder the API usage looks like: `header 1 . focus title . italic`, where
```haskell
data Header = Header {
  _level :: Int,
  _title :: Text
} 
newtype Italic = Italic { _unItalic :: Text }

makeLenses ''Header
makeLenses ''Italic
```

The first argument `at` tells us where to focus, e.g. `_title` inside `Header`. 

The following arguments are `afb` and `s` because an abstract `Traversal` is defined in continuation-passing style.

As a setter it has type
```haskell
(a -> Identity b) -> (s -> Identity t)
```
This can be understood as "if you give me a function for turning `a` into `b`, I can then give you a function from `s` into `t`". Here `s` and `t` are the "big" types of the traversal, and `a`, `b` are the focused "small" types. But our `PPrism` and `PTraversal` have `s ~ t` and `a ~ b`. So a setter is simply
```haskell
(a -> Identity a) -> (s -> Identity s)
```
Which reads "tell me how to transform  `a` , and since I know how to deal with (parse/render) their occurences in `s`, I can give you back a function which transforms `s`". 

A getter/fold is the same except it uses `Const` as the `Applicative` for the higher-kinded `f` so that it ignores the output of the function `afb` and simply "collects" the "targets" of the traversal.
```haskell
(a -> Const r a) -> (s -> Const r s)
```
where `r` is a `Monoid` so that `Const r` can be an `Applicative` while ignoring its second phantom argument.

Think: "if i can transform `a`, then surely I can simply return those `a` to you directly (and ignore the transform your gave me)".

So the type signature
```haskell
focus :: Traversal' s Text -> PTraversal s Text
```
says we have to promote a type of `Traversal' s Text` to a type of `PTraversal s Text`. Which is to say we promote a type which does not deal with `Context`s to one which does.

```haskell
focus :: Traversal' s Text -> PTraversal s Text
focus at afb s@(_, ctx@(Context unconsumed above lvl)) =
  let afbsft = _1
         . at
         . textAtLevel -- prepare new Context for text at `focus`
            (lvl+1) -- keep track of the number of times we focus
            (V.snoc above unconsumed) -- save current-level unconsumed to `Context`
  in afbsft afb s

-- prepare fresh `Context`
textAtLevel :: Int -> Vector Text -> Iso' Text (Text, Context)
textAtLevel lvl unconsumeds = iso
  (\txt -> (txt, Context "" unconsumeds lvl))
  (\(txt, Context rest _ _) -> txt <> rest)
```

First we do `_1` to "focus" on the value at which we want to `focus`. The second component of the `(a, Context)` tuple we leave untouched, since that deals with the context of the parent, not the child. To see this, note that
```
# *this is* a header\n unconsumed
```
when parsed gives
```haskell
(Header 1 "*this is* a header", Context " unconsumed" V.empty 0)
```
and that context does not change, even after `focus`ing at the header's content `"*this is* a header"`, which should be given a *fresh* context of its own for the recursive case, i.e. further parsing italics inside the header should yield
```haskell
(Italic "this is", Context " a header" (V.fromList [" unconsumed"]) 1)
```

So what remains is to prepare the context for the child:

- `textAtLevel`
	- the focused text will be in the left of `(Text, Context)` hence unconsumed is empty `""`
- `lvl+1`
	- we go one level deeper, so increment the level of the context
- `V.snoc above unconsumed`
	- preserve the parent unconsumed by adding to our stack 

Remember that with parse-rendering we need to do things in both directions, parse and render. Using `Iso`s wherever we can to chain `lens` combinators automatically does this. Whence
```haskell
textAtLevel :: Int -> Vector Text -> Iso' Text (Text, Context)
```

Finally, we call our promoted `afbsft :: PTraversal s Text` on the continuation `afb` and apply it to the parse input `s`. Done.

### horizontal composition: choice
As a warm up to working with horizontal sequential left-to-right composition, let's work with `<||>` which tries one parser and if it fails tries the other. It will suggest some tricks which will also help in defining `||>`.

Note that `lens` has a combinator `failing` which seems to do what we want. Its type specialised to `Traversal'` is
```haskell
failing :: Traversal' s a -> Traversal' s a -> Traversal s a
```

But often we have to have the "small" `a` type be different. For example, if we want to do `italic <||> bold` in which case `italic` and `bold` would have different "small" types `Italic` and `Bold`.
```haskell
<||> :: Traversal' s Bold -> Traversal' s Italic -> Traversal s ?
```

So we need something like:
```haskell
(<||>) :: PTraversal s a -> PTraversal s b -> PTraversal s (Either a b)
(<||>) afbst afbst' afb'' s =
  let Pair constt ft = afbst aConstfb s
  in case getConst constt of
       Any True -> ft
       Any False -> afbst' afb' s
  where aConstfb  (a,ctx) = onlyIfLeft a ctx <$> Pair (Const (Any True)) (afb'' (Left a, ctx))
        afb' (a,ctx) = onlyIfRight a ctx <$> afb'' (Right a, ctx)

onlyIfRight _ _ (Right b, ctx') = (b, ctx')
onlyIfRight a ctx (Left _, _) = (a, ctx)

onlyIfLeft _ _ (Left b, ctx') = (b, ctx')
onlyIfLeft a ctx (Right _, _) = (a, ctx)
```

The small type of the return value is `Either a b` corresponding to whether the left or the right branch succeeded.

How do we tell if the left branch has succeeded? We can follow the definition of `failing` and use the `Bazaar` type which is some kind of maximally general list-with-context traversal. But a simpler solution is possible.

Recall the types of our optics:
```haskell
type PPrism s a = forall p f. (Choice p, Applicative f) => P p f s a
type PTraversal s a = forall f. (Applicative f) => P (->) f s a

type P p f s a = Optic' p f (s, Context) (a, Context)
```

Due to the occurence of the `forall`, they are a rank-2 types. Which means when used with a function *the caller* gets to decide what `p` and `f` are.

We need to somehow "run" the left `Traversal`. Usually this is done by fixing `f` to `Const`, but *the caller* of the function gets to decide `f`, not us, so that's not an option.

What we can do instead is to run the traversal "in parallel" with `f`, using the product applicative:
```haskell
data Product f g a = Pair (f a) (g a)

instance (Applicative f, Applicative g) => Applicative (Product f g) where
    pure x = Pair (pure x) (pure x)
    Pair f g <*> Pair x y = Pair (f <*> x) (g <*> y)
    liftA2 f (Pair a b) (Pair x y) = Pair (liftA2 f a x) (liftA2 f b y)
```
With `Pair Const f`, `f` is still free to be chosen by the caller, while we use `Const`  to inspect the contents of the traversal.

We use `Const` with the `Any` monoid, since we are interested in only the binary yes/no question of whether or not the left branch succeeded, i.e. if it has any traversal targets.

- if it does, then we simply return the traversal result of running with the original applicative `f`, extracted from the right component of our product `Pair (Const Any) f t`
- if it doesn't then we simply run the traversal for the right branch.

`onlyIfLeft` and `onlyIfRight` are important in the inverse render direction. We render with the left only if it was the left which parsed, and with the right only if it was the right which parsed. This is to respect traversal laws, but more on this later.

### horizontal composition: sequential
Now that we are warmed up, and with the product applicative trick under our belts, let's implement sequential composition:
```haskell
andThen :: Bool -> PTraversal s a -> PTraversal Text b -> PTraversal s (Either a b)
andThen rightMustSucceed afbsft afbsft' afb'' s@(_, Context _ above lvl_s) =
  -- run left
  let Pair constt ft = afbsft aConstfb s
  in case getConst constt of
       Last (Just (unconsumed, isFocused)) ->
         -- run right, on unconsumed from left
         let Pair constt' ft' = afbsft' aConstfb' (unconsumed, Context "" above lvl_s)
         in case getConst constt' of
           Any True ->
             -- merge results of both left/right
             let merge (a, Context ctx _ _) (txt, Context ctx' _ _) =
                   -- if focused, then everything consumed will be rebuilt into 'a',
                   --   so discard 'ctx' which consists entirely of unconsumed
                   -- if not focused, we have already discarded unconsumed, so just use rebuilt 'ctx'
                   let rebuilt = if isFocused then "" else ctx
                   in (a, Context (rebuilt <> txt <> ctx') above lvl_s)
             in merge <$> ft <*> ft'
           Any False -> if rightMustSucceed
                        then pure s
                        else let replaceUnconsumed (t, Context rebuilt abv l) =
                                   (t, Context (rebuilt <> unconsumed) abv l)
                             in replaceUnconsumed <$> ft
       Last Nothing -> pure s

  where aConstfb  (a,ctx@(Context unconsumed abv lvl)) =
          let isFocused = lvl > 0
              -- if not focused, discard the parent top-level 'unconsumed',
              --  since it is now the responsibility of afbsft'
              --  we do it here since doing it at parent will rebuild child unconsumed into it
              --  and we would have to disentangle child/parent unconsumed
              -- if focused, 'unconsumed' is bottom-level and local to afbst,
              --  so will be rebuilt into parent of focus
              ctx' = Context (if isFocused then unconsumed else "") abv lvl
              unconsumed_top = fromMaybe unconsumed (abv !? lvl_s)
          in onlyIfLeft a ctx' <$> Pair
              (Const (Last (Just (unconsumed_top, isFocused))))
              (afb'' (Left a, ctx'))

        aConstfb' (a,ctx) = onlyIfRight a ctx <$> Pair (Const (Any True)) (afb'' (Right a, ctx))
```
Let's be clear on what we need

- we must allow the possibility of the right failing 
	- this is to be able to define `many`, more on this later
	- whence the `Boolean` argument `rightMustSucceed`
- we must run left first and then pass along its unconsumed input to the right
	- the left may be focused, in which case we need to pass along one of its ancestor unconsumed input instead, depending on the level at which the right is applied
		- recall the cases
			- `(header 1 . focus title . italic) ||> strikethrough` 
				- which would parse `# *i*\n~~s~~`
			- `header 1 . focus title . (italic ||> strikethrough)`
				- which would parse `# *i ~~s~~\n`
		- this is what

            unconsumed_top = fromMaybe unconsumed (abv !? lvl_s)

		  does. Where `lvl_s` indexes into our stack of parent unconsumed `abv`.
- if the right has consumed additional input, we must remove this portion from the unconsumed input of the left
	- otherwise more text would be rebuilt in the render phase for the left than is correct, consider:

		        (header 1 . focus title . italic) ||> strikethrough

		- the left `header 1 . focus title . italic` parses
			- from

			      (# *i* h\n~~s~~ unconsumed", Context "" [] 0)

			- into

			      (Italic "i", Context " h" ["~~s~~" unconsumed] 1)

		- the right `strikethrough` should consume `~~s~~`
		- *without* `strikethrough` the left's unconsumed would be `~~s~~ unconsumed`
		- but *with* `strikethrough` the left's unconsumed should be empty
		- we want an identity traversal (one which performs the no-op transform of doing nothing) to produce

		      # *i* h\n~~s~~ unconsumed

		- not

		      # *i* h\n~~s~~ unconsumed unconsumed

			- here incorrectly, the unconsumed of left and right overlap at ` unconsumed`
	- this is what the lines involving `isFocused` do
		- it is complicated by the discarding happening in different places, depending on whether the left is focused or not

We use the `Pair` product applicative as before to gather information we need:

- in the left case we need unconsumed input information
	- we automatically get parse-success information by just the `Last` monoid
	- it returns the "rightmost" last element of the traversal
	- why the rightmost?
		- in the case the left is not focused, it might still be a shallow traversal of multiple parse targets
		- for example `many italic` parses many adjacent occurences of italics, and

			    ("*i1**i2* unconsumed", Context "" [] 0)

          gets parsed into

              (Italic "i1", Context "*i2* unconsumed" [] 0)

          and

              (Italic "i2", Context " unconsumed" [] 0)

		- each occurence of which was passed the unconsumed input of the previous on the left
		- so to get the unconsumed of the parent, in this case `many italic`, we want to look at the unconsumed of the last child, in this case `" unconsumed" ` for `Italic "i2"`
- in the right case we just need to know if it succeeded, so we use `Const Any` like in the `<||>` case

The `merge` helper function is for the render phase: it makes sure the parent `Text` of the right and its unconsumed input are properly concatenated after the text that has been rendered for the left.

### many
```haskell
many :: PTraversal Text a -> PTraversal Text a
many p = failing (some' p) ignored
  where
    some :: PTraversal Text a -> PTraversal Text a
    some p = (p ||>? many' p) . alongside chosen id

(||>), (||>?) :: PTraversal s a -> PTraversal Text b -> PTraversal s (Either a b)
(||>) = andThen True
(||>?) = andThen False
```

Parsec defers to its `MonadPlus` instance for `<|>` which is simply the `Alternative` instance for a `Monad`, which is a certain "failure"/"choice" monoid for higher kinded types.
```haskell
class (Alternative m, Monad m) => MonadPlus m where
...

class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a

    some :: f a -> f [a]
    some v = some_v
      where
        many_v = some_v <|> pure []
        some_v = liftA2 (:) v many_v

    many :: f a -> f [a]
    many v = many_v
      where
        many_v = some_v <|> pure []
        some_v = liftA2 (:) v many_v
```
It defines `many` and `some` mutually recursively. 

We follow its example, except we have no `Applicative` instance for `PPrism s`  or `PTraversal`. This is because our parse-renderers are bidirectional. So if we want to map `a -> b` into `f a` to get `f b`, we need the inverse `b -> a`. This is an exponential functor, but that is not relevant here. This means we cannot just parse a single value of `a` first, parse the rest of `[a]`, and then `liftA2` to concatenate with `(:)`.

The fact that we use a traversal of `a` instead of a prism of `[a]` also means we have to be careful with our base case. Which is when there are no more `a` to consume. This is when the component parser for a single `a` fails. This means when we sequence `single ||> more` we must allow `single` to succeed and `more` to fail. This is where the `rightMustSucceed` argument to `andThen` comes in, where we use`||>?` to allow our base case to succeed, and terminate the recursion.

Note that we can use `failing` here instead of `<||>` because for `many` the types are the same for the multiple items we sequence. Similarly, since types are the same, we use `chosen` with `alongside` to eliminate the `Either` returned by `||>?`.
```haskell
failing :: Traversal s t a b -> Traversal s t a b -> Traversal s t a b
(<||>) :: PTraversal s a -> PTraversal s b -> PTraversal s (Either a b)

chosen :: Lens' (Either a a) a
alongside :: Lens' s a -> Lens s' a' -> Lens (s,s') (a,a')
(||>?) :: PTraversal s a -> PTraversal Text b -> PTraversal s (Either a b)
```

Lastly, we don't export `some` because a `Traversal` can always have no elements, hence they only make sense with `many`.
