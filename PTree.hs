-- PTree.hs
-- An implementation of patricia trees

{-# LANGUAGE GADTs, TupleSections, ScopedTypeVariables #-}

import Prelude hiding (lookup)
import qualified Data.List as L
import Data.Either
import Control.Monad
import Control.Monad.State

type Key k = [k]
type PChild k v = ([k], PTree k v)
type ReplPolicy v = v -> v -> v

data PTree k v where
  PNode :: Eq k => [([k],PTree k v)] -> PTree k v
  PLeaf :: Eq k => v -> PTree k v

instance (Show k, Show v) => Show (PTree k v) where
  show t = case t of
    PLeaf val -> show val
    PNode children ->
      "Node (" ++ (L.intercalate ", " (map showChild children)) ++ ")"
      where showChild (key,child) = (show key) ++ " => " ++ (show child)

empty :: Eq k => PTree k v
empty = PNode []

-- returns (a,b,c) where
-- a is the common prefix between the two keys
-- b, c respectively are the suffixes which when concatenated with
-- the common prefix form the original keys
consumeKey :: Eq k => Key k-> Key k -> (Key k, Key k, Key k)
consumeKey xxs yys
  | [] <- xxs, [] <- yys      = ([], [], [])
  | _:_ <- xxs, [] <- yys     = ([], xxs, [])
  | [] <- xxs, _:_ <- yys     = ([], [], yys)
  | x:xs <- xxs, y:ys <- yys  =
    if x == y
    then let (ps,qs,rs) = consumeKey xs ys in (y:ps,qs,rs)
    else ([],xxs,yys)
  
-- insert new node in patricia tree
-- f is the replacement policy if there is an existing value
-- with the same key
insertWith :: Eq k => ReplPolicy v -> Key k -> v -> PTree k v -> PTree k v
insertWith f key val (PLeaf tval) =
  insertWith f key val (PNode [([], PLeaf tval)])
insertWith f key val (PNode children) =
  let (newChildren, inserted) = insertChildren f key val children in
  if inserted
  -- the new node was inserted in one of the existing children
  then PNode newChildren
  -- the new node cannot be inserted in any existing child;
  -- make it a new child tree
  else let newChild = (key,PLeaf val) in PNode (newChild:children)

  where insertChildren :: Eq k => ReplPolicy v -> Key k -> v -> [PChild k v] -> ([PChild k v], Bool)
        insertChildren f key val []     = ([], False)
        insertChildren f key val (c:cs) =
          let (newc, inserted) = insertChild f key val c in
          if inserted
          -- found a subtree to insert the new node in!
          then ((newc:cs), True)
          else let (newcs, tailInserted) = insertChildren f key val cs in
               ((c:newcs), False || tailInserted)

        insertChild :: Eq k => ReplPolicy v -> Key k -> v -> PChild k v -> (PChild k v, Bool)
        insertChild f key val child@(ckey,ctree) =
          let (keyPre, ckeySuf, keySuf) = consumeKey ckey key in
          if length keyPre == 0 && (length ckeySuf > 0 || length keySuf > 0)
          then (child, False)
          else case ctree of
                 -- add new leaf to existing node
                 PNode _ -> 
                   let newTree = insertWith f keySuf val ctree in
                   ((ckey, newTree), True)

                 PLeaf cval ->
                   if key == ckey
                   -- trying to insert value with existing key;
                   -- apply replacement policy
                   then ((ckey, PLeaf (f cval val)), True)
                   -- create new intermediate node
                   else let child1 = (ckeySuf, ctree) in
                        let child2 = (keySuf, PLeaf val) in
                        ((keyPre, PNode [child1, child2]), True)


-- insert with replacement policy: clobber old value
insert :: Eq k => Key k -> v -> PTree k v -> PTree k v
insert = insertWith (\x _ -> x)

-- find value, if any, associated with key
lookup :: Eq k => Key k -> PTree k v -> Maybe v
lookup key t =
  case lookup_ key t of
    Left val  -> Just val
    Right _   -> Nothing
  -- we use the Either monad because we can use "failure"
  -- to short-circuit the search when we find a match
  -- i.e. "failure is success"
  where lookup_ :: Key k -> PTree k v -> Either v ()
        lookup_ key (PLeaf _)        = return ()
        lookup_ key (PNode children) = mapM_ (lookupChild key) children
        lookupChild key (ckey,ctree)
          | PLeaf cval <- ctree =
            if key == ckey then Left cval else return ()

          | PNode children <- ctree = do
            let (keyPre, ckeySuf, keySuf) = consumeKey ckey key
            if length keyPre > 0
            then mapM_ (lookupChild keySuf) children
            else return ()

main = do
  let pairs = [("actor",1),("acts",2),("acting",3),("act",4),("act",100),("beam",5)]
  let tree = foldr (uncurry insert) empty pairs
  print tree
  print $ lookup "acting" tree
  print $ lookup "act" tree
  print $ lookup "actoring" tree


      
