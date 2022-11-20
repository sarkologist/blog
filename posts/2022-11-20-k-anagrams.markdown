---
title: "k-anagrams"
author: Guo Liang Oon
tags: haskell
description: "group structure of the integers, and a small rant on coding interviews"
---

## coding interviews
I am not a fan of coding interviews. 

When I was hiring, I gave candidates a take home test, and then in the live interview discussed their solution and have them work through a modification to that solution. This is to relieve them of time pressure and allow them the space to thoroughly familiarise themselves with the problem. The subsequent live discussion and modification then serves only as a casual check that they did not just plagiarise someone else's work.

We often deliver code according to a deadline it is true, but this deadline is not the short amount of time of a coding interview. Most of doing well in coding interviews comes down to being familiar with the problems and meaningless coding micro-fluency, than from any real problem solving or deep programming ability.

My opinions on coding interviews aside, I found the k-anagram problem to be quite interesting, and not for the usual reasons.
 
## k-anagrams
Here is the problem statement:

> Given two strings of lowercase alphabets and a value k, the task is to find if two strings are k-anagrams of each other or not. 
> 
> Two strings are k-anagrams of each other if they can become anagrams by replacing at most k characters in a string.
> 
> Implement a function `isKAnagram` that returns true if the two strings are k-anagram, or false if they are not.

Not everything in a problem statement is relevant to the heart of the problem itself:

- lowercase/uppercase doesn't matter: the problem is about whether two `Char` are equal
	- we can always normalise to lowercase first before running the algorithm
- an "anagram" is a special case of a `k`-anagram: a `0`-anagram
- we can first compute how many replacements are necessary, then check if it is less than or equal to the `k` we want
- symmetry
	- if `a` takes `k` replacements to be an anagram of `b`, then so does `b` take `k` replacements to be an anagram of `a`

So we can focus on counting replacements necessary.

Replacements are necessary either when `a` has a letter not present in `b` or vice versa (recall symmetry). Note that a word may contain more than one occurence of the same latter, so we are checking for presence-with-multiplicity.

Sometimes the essence of the problem becomes clearer when we focus on simplifications of it.

Assume the strings `a` and `b` comes pre-sorted according alphabetical ordering of `Char`. Then counting for replacements necessary becomes easier:

- `anagram` sorted becomes `aaagmnr`
- `grammar` sorted becomes `aagmmrr`
- read through both strings from left to right
	- we can match up 2 `a`s from each, leaving 1 extra `a`
		- `agmnr`
		- `gmmrr`
	- discard the extra `a` 
		- `gmnr`
		- `gmmrr`
	- match 1 `g`, nothing extra, nothing missing
		- `mnr`
		- `mmrr`
	- match 1 `m`, 1 missing `m`
		- `nr`
		- `rr`
	- match 0 `n`, 1 extra `n`
		- `r`
		- `rr`
	- match 1 `r`, 1 missing `r`

So we have extras `a` and `n` and missings `m` and `r`. But recall that the situation is symmetric. We can either replace the letters of the first string to match that of the right, or vice versa.

So our problem is really about matching the `Char`s of both strings. But what is matching but a form of iterative "cancelling"? To put it in more visually stark terms, take the 3 `a`s from `anagram` and 2 `a`s from `grammar`
```
--> aaa | aa <--
```
We can imagine the left string moving to the right, and the right string moving to the left, and if they match they "cancel out". So the next step becomes
```
aa | a
```
And the next
```
a |
```
So we have 1 `a` remaining, and that means we have 1 extra `a`.

Similarly, for `m`
```
--> m | mm <--
```
becomes
```
| m
```
Which means we have 1 missing `m`.

Now because of the symmetry of the problem, we don't really care whether it is "missing" or "extra". But before we are done with the counting, we cannot know ahead of time if we will have remaining letters on the left or on the right. So we have to be consistent, and always place letters from the second string on the right, and letters from the first string on the left.

More abstractly, this is simply the group structure of the integers! "Extra" corresponds to the positive integers, and "missing" corresponds to the negative integers.

So we can implement
```haskell
unmatchedCount :: String -> String -> Maybe (Map Char Int)
unmatchedCount a b = go a b M.empty
  where
    go (a:as) (b:bs) acc = go as bs
       $ M.insertWith (+) a 1
       $ M.insertWith (+) b (-1)
       $ acc
    go [] [] acc = Just acc
    go _ _ _ = Nothing
```
which counts for each letter their *surplus* or *deficit* in the left vs. the right string.

We arbitrarily pick `b` (remember: symmetry) to be negative and let the letter counts cancel each other out.

So `unmatchedCount "anagram" "grammar"` gives
```haskell
Just (fromList [('a',1),('g',0),('m',-1),('n',1),('r',-1)])
```

The
```haskell
go _ _ _ = Nothing
```
case simply gives up ASAP if we know the strings are of unequal length, since we know they cannot be anagrams, let alone k-anagrams.

To count the number of replacements needed we simply sum across all the counts, but taking their absolute value with `abs`, since again by symmetry of the problem we don't care if it is a surplus or deficit. 
```haskell
replacementsNeeded :: String -> String -> Maybe Int
replacementsNeeded a b =
  case unmatchedCount a b of
    Just d -> 
      let Sum count = foldMap (Sum . abs) d
          (q, r) = quotRem count 2
      in if r == 0 then Just q else Nothing
    Nothing -> Nothing
```

Again by symmetry, we will double-count replacements needed, so we divide by 2. We know that the strings are of equal length if we get a `Just` case since `unmatchedCount` checks it for us, but just to be safe with use Euclid's algorithm and work with the quotient and remainder. 

We could have also simply summed over only the positive values, since again by symmetry for every *excess* there is a corresponding *lack* of equal magnitude, but filtering is no simpler or more efficient than simply taking the absolute value of everything and dividing by 2.

Finally checking for `k`-anagram status is simply a matter of checking if the replacements needed are within the `k` quota provided
```haskell
isKAnagram :: String -> String -> Int -> Bool
isKAnagram a b k = maybe False (<= k) $ replacementsNeeded a b
```

We can also make the group structure a bit more explicit by doing
```haskell
class Monoid k => Group k where
  invert :: k -> k
  -- law: invert . invert = id
  -- law: (\x -> x <> invert x) a == mempty

instance Num a => Group (Sum a) where
  invert (Sum a) = Sum (negate a)

newtype MergeMap k v = MergeMap (Map k v) deriving (Eq, Show)

instance (Ord k, Semigroup v) => Semigroup (MergeMap k v) where
  MergeMap l <> MergeMap k = MergeMap $ M.unionWith (<>) l k

isKAnagram :: String -> String -> Int -> Bool
isKAnagram a b k = maybe False (<= k) $ replacementsNeeded a b

replacementsNeeded :: String -> String -> Maybe Int
replacementsNeeded a b =
  case unmatchedCount a b of
    Just (MergeMap d) -> 
      let Sum count = foldMap (fmap abs) d
          (q, r) = quotRem count 2
      in if r == 0 then Just q else Nothing
    Nothing -> Nothing

unmatchedCount :: String -> String -> Maybe (MergeMap Char (Sum Int))
unmatchedCount = go (MergeMap M.empty)
  where
    go acc (a:as) (b:bs) =
        let updated = acc
              <> MergeMap (M.singleton a (Sum 1))
              <> MergeMap (M.singleton b (invert (Sum 1)))
        in go updated as bs
      
    go acc [] [] = Just acc
    go _   _  _  = Nothing
```

