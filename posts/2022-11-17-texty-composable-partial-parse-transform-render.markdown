---
title: "texty: composable, partial, fused, parse-transform-render traversals"
author: Guo Liang Oon
tags: haskell
description: "part 1: what it does"
---

## teaser
![](https://media.giphy.com/media/B5KgFe6s0rGUjSkMtL/giphy.mp4){loop=""}

Do you sometimes feel you have added too many bullet points, and that each top level bullet should be its own heading, with child bullets a new section under that heading? And of course they should be *subheadings* of the heading all the bullets were originally a section of.

Here's the code which does the transformation of the markdown:
```haskell
unindentBulletIntoSubheader :: Text -> Text
unindentBulletIntoSubheader = execState $
  zoom (text . many' headerTitleContent . _1 . _HeaderTitleContent) $ do
    (headerLevel, _, _) <- get
    zoom (_3 . text . many' (bullet <%> (header headerLevel)) . _1) $ do
       let f (Left (Bullet bulletLevel content)) =
             if bulletLevel==0
             then Right (Header (headerLevel+1) content)
             else Left  (Bullet (bulletLevel-1) content)
           f (Right x) = Right x
       modify f
```
## what is texty?
Composable parsers, like with parser combinators, but

- **partial**: no full parse if not necessary
	- e.g. in markdown: don't parse the italic inside the header if you are only interested in the raw text inside, but otherwise do if you need to transform it
```
"# *i*\n"
```

- **bidirectional**: parse and render
- **fuses** parse/transform/render passes into one
- **optics**-based: define traversal targets and modifications orthogonally

### naive approaches
If you were to do partial parsing in a naive way, you might define
```haskell
data Markdown = Markdown [ Line ]
data Line = Header Int [Inline] | Line [Inline]
data Inline = Plain Text | Italic Text
```
if you were interested in italics inside headers, or
```haskell
data Markdown = Markdown [ Line ]
data Line = Header Int Text | Line Text
```
if you were only interested in headers, not italics.

So unless you are prepared to define a separate such type for each specific markdown transformation task you want to do, in order to handle all possible scenarios, you would need to instead define a type upfront for the full markdown syntax. And you would be forced to work with other parts of the markdown tree you are not interested in for each specific task.

A full parse would also represent more data in memory than is necessary for the task at hand, say if you were only interested in indenting headers, why would you bother representing `Bullet Int Text` or `Italic Text` in memory?
Not only that, your parse, transform, and render traversals would each have to walk through extraneous structure in the syntax tree even when performing no modifications.

So a naive approach would be to do:
```haskell
data Markdown = Markdown [ Line ]
data Line = Header Int [Inline] | Line [Inline]
data Inline = Plain Text | Italic Text

parseItalic :: Parser Text
parseHeader :: Parser (Int, Inline)
parseInline :: Parser Inline
parseInline = Italic <$> parseItalic <|> parsePlainText
parseLine :: Parser Line
parseLine = (uncurry Header <$> parseHeader) <|> (Line <$> parseInline)
parseMarkdown = Markdown <$> many parseLine
...
```
with even more definitions for functions transforming and rendering the parse tree, each of which is coupled to the specific definition of `Markdown` for the specific the markdown elements you were interested in only because of some specific transformation you wanted to perform.

You could use recursion schemes or lenses/optics to factor out the folds of transformation and rendering, that saves some code and decouples the rendering/transform from traversals. But you would sill need to make separate passes for parse, transform, and render.

And this is assuming there *is* a task-neutral universal representation of markdown, which is doubtful. Consider for example, what is needed if you wanted to associate body text to the header it is under? Your definitions would have to be changed to
```haskell
data Markdown = Markdown [ Section ]
data Section = Section Int [Inline] [Line]
data Line = Line [Inline]
data Inline = Plain Text | Italic Text
```
Notice how except for `Inline` all the other data definitions are affected as well.

And what if you wanted to do *both* in the same task? Then you would have to pick the stronger representation and settle for it being harder to work with for the simpler operation! 

For example, if you wanted to unindent top-level bullets into subheaders of their own, in order to decide their sub-level you need to know the level of the header for their section, and use 1 more than that, so that your new headers fall under the same section: 

- for getting the level, the content-associated header representation works best, 
```haskell
data Section = Section Int [Inline] [Line]
```
- but for the unindent operation itself, the header-as-simply-a-special-line representation works best
```haskell
data Line = Header Int [Inline] | Line [Inline]
```
### a better way
What if, instead, the only non-coupled data types you need are
```haskell
newtype Header = Header Int Text
newtype Italic = Italic Text
```
? And you can just add more for your specific use case, and it won't affect the others?

And for the parsing/transforming you can simply define
```haskell
i :: PPrism Text Italic
i = pPrism parse render
  where
    parse = Italic . pack <$> withinMany (char '*') (noneOf (['*']))
    render (Italic txt) = ("*" <> txt <> "*")

h :: Int ->  PPrism Text Header
h n = pPrism parse render
-- similarly parse/render definitions for header ...
```
? And if you are only interested in transforming the header, then you can do
```haskell
λ set (text . h 1 . _1 . content) "hello" "# *i*\n"
"# hello
"
```
? And if you are interested in the italic inside of the header,  you can just do
```haskell
λ set (text . h 1 . focus content . i . _1 . unItalic) "hello" "# *i*\n"
"# *hello*
"
```
?

Composability! And all this in a one-pass traversal of the text!
