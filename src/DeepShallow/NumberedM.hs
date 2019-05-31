-- http://okmij.org/ftp/Haskell/numbered-monad.txt

{-# LANGUAGE DeriveFunctor #-}

{-
From posting-system@google.com Wed Oct 10 01:42:11 2001
Date: Tue, 9 Oct 2001 22:42:03 -0700
Reply-To: oleg@pobox.com
From: oleg@pobox.com (oleg@pobox.com)
Newsgroups: comp.lang.functional
Subject: Re: How to do this functionally?
References: <a1eba1e1.0110091309.d5c8e35@posting.google.com>
Message-ID: <7eb8ac3e.0110092142.3fef83e3@posting.google.com>
X-Comment: added more comments
Status: OR

dr_pibb@hotmail.com (Dr Pibb) wrote in message news:<a1eba1e1.0110091309.d5c8e35@posting.google.com>...

> An obvious way to implement it in this way is to have a collection of
> state which is passed along all the time to every function.  But this
> seems rather tedious.  I could also use the ST "lazy state thread"
> module, which would perhaps elminate some of the tedium...

You can build your own monad -- that will effectively hide the counter
(which you use to tag every node)

Here's an example
-}

module DeepShallow.NumberedM where

--First, some framework
type Numbered a = (Int,a)

newtype NumberedM a = NumberedM (Int -> Numbered a) deriving Functor

instance Applicative NumberedM where
  f <*> a = undefined
  pure = undefined

instance Monad NumberedM where
    NumberedM m >>= f = NumberedM $ \n -> let (n1,v) = (m n)
                                              NumberedM m' = f v
                                              (n1',v') = (m' n1)
                                          in (n1',v')
    return x = NumberedM $ \n -> (n,x)

-- additional monad morphisms

-- get the current id and increment it
incr:: NumberedM Int
incr = NumberedM $ \n -> (n+1,n)

run_numberedM:: NumberedM a -> Int -> Numbered a
run_numberedM (NumberedM m) init_count = m init_count

-- Now, on to applications: trees

-- The basic tree datatype
data Tree a = Nd a (Forest a) deriving Show
type Forest a = [Tree a]

-- Here's a function that creates a new node and tags its value
-- with a unique number
make_node val kids = do {
        n <- incr;
        return (Nd (n,val) kids)
        }

-- Finally, let's try to build a binary tree
make_btree:: Int -> NumberedM (Tree (Numbered Int))

-- Note the algorithm does not explicitly mention any counter
-- at all: it looks like a regular algorithm for building a binary tree.
-- There is an important difference: the left branch is constructed
-- strictly _before_ the right branch. The parent node of two branches
-- is constructed strictly after. The Algorithm is _serialized_: the
-- consequence of using the monad.

make_btree 0 = make_node 0 []
make_btree depth = do {
      left <- make_btree (depth -1);
      right <- make_btree (depth -1);
      make_node depth [left, right]
      }

-- Let's see what we've got

{-

Main> run_numberedM (make_btree 1) 100
(103,Nd (102,1)
     [Nd (100,0) [],
      Nd (101,0) []])

The result is a pair (the_resulting_count,tree)
We tag each node with a unique counter, an integer that starts at 100
and counts forward.

Main> run_numberedM (make_btree 3) 100
(115,Nd (114,3)
        [Nd (106,2)
            [Nd (102,1)
                [Nd (100,0) [],
                 Nd (101,0) []],
             Nd (105,1)
                [Nd (103,0) [],
                 Nd (104,0) []]],
         Nd (113,2)
            [Nd (109,1)
                [Nd (107,0) [],
                 Nd (108,0) []],
             Nd (112,1)
                [Nd (110,0) [],
                 Nd (111,0) []]]])

-- It does seem to work, don't you think?

-}
