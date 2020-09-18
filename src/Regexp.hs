module Regexp where

import Prelude hiding (seq)

data Regexp = Empty
            | Epsilon
            | Char Char
            | Seq Regexp Regexp
            | Alt Regexp Regexp
            | Star Regexp
            deriving (Eq, Ord)

match :: Regexp -> String -> Bool
match r s = nullable (foldl (flip derivative) r s)

nullable :: Regexp -> Bool
nullable Empty     = False
nullable Epsilon   = True
nullable (Char _)  = False
nullable (Alt p q) = nullable p || nullable q
nullable (Seq p q) = nullable p && nullable q
nullable (Star _)  = True

derivative :: Char -> Regexp -> Regexp
derivative _ Empty = empty
derivative _ Epsilon = empty
derivative c (Char r) | c == r = epsilon
derivative c (Char r) = empty
derivative c (Alt p q) = alt (derivative c p) (derivative c q)
derivative c (Seq p q) | nullable p = alt (seq (derivative c p) q) (derivative c q)
derivative c (Seq p q) = seq (derivative c p) q
derivative c (Star r) = seq (derivative c r) (star r)

empty = Empty
epsilon = Epsilon
char = Char

seq Empty _ = Empty
seq _ Empty = Empty
seq Epsilon r = r
seq r Epsilon = r
seq r q = Seq r q

alt Empty r = r
alt r Empty = r
alt r Epsilon = if nullable r then r else Alt Epsilon r
alt Epsilon r = if nullable r then r else Alt Epsilon r
alt r q | r == q = r
alt r q = Alt r q

star Empty = Epsilon
star Epsilon = Epsilon
-- r** = r*
star (Star r) = Star r
star r = Star r
