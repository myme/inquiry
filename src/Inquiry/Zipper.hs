{-# LANGUAGE DeriveFunctor #-}

module Inquiry.Zipper
  ( Zipper(..)
  , emptyZipper
  , append
  , end
  , insert
  , next
  , peek
  , pop
  , prev
  , start
  ) where

data Zipper a = Zipper { zipperCurrent :: Maybe a
                       , zipperPrev :: [a]
                       , zipperNext :: [a]
                       } deriving (Eq, Functor, Show)

instance Foldable Zipper where
  foldMap f (Zipper Nothing ps ns) = foldMap f (reverse ps) <> foldMap f ns
  foldMap f (Zipper (Just c) ps ns) = foldMap f (reverse ps) <> f c <> foldMap f ns

emptyZipper :: Zipper a
emptyZipper = Zipper { zipperCurrent = Nothing
                     , zipperPrev = []
                     , zipperNext = []
                     }

append :: a -> Zipper a -> Zipper a
append x (Zipper c ps ns) = Zipper c ps (ns <> [x])

insert :: a -> Zipper a -> Zipper a
insert x z@(Zipper Nothing _ _)   = z { zipperCurrent = Just x }
insert x z@(Zipper (Just c) xs _) = z { zipperCurrent = Just x
                                      , zipperPrev = c : xs
                                      }

start :: Zipper a -> Zipper a
start (Zipper Nothing ps ns)  = Zipper Nothing [] (reverse ps <> ns)
start (Zipper (Just c) ps ns) = Zipper Nothing [] (reverse ps <> (c : ns))

end :: Zipper a -> Zipper a
end (Zipper Nothing ps ns)  = Zipper Nothing (reverse ns <> ps) []
end (Zipper (Just c) ps ns) = Zipper Nothing (reverse ns <> (c : ps)) []

peek :: Zipper a -> Maybe a
peek (Zipper c _ _) = c

prev :: Zipper a -> Zipper a
prev z@(Zipper Nothing [] _) = z
prev   (Zipper (Just c) [] ns) = Zipper Nothing [] (c:ns)
prev   (Zipper Nothing (p:ps) ns) = Zipper (Just p) ps ns
prev   (Zipper (Just c) (p:ps) ns) = Zipper (Just p) ps (c:ns)

next :: Zipper a -> Zipper a
next z@(Zipper Nothing _ []) = z
next   (Zipper (Just c) ps []) = Zipper Nothing (c:ps) []
next   (Zipper Nothing ps (n:ns)) = Zipper (Just n) ps ns
next   (Zipper (Just c) ps (n:ns)) = Zipper (Just n) (c:ps) ns

pop :: Zipper a -> (Maybe a, Zipper a)
pop z@(Zipper Nothing _ _) = (Nothing, z)
pop   (Zipper c [] []) = (c, emptyZipper)
pop   (Zipper c (p:ps) []) = (c, Zipper (Just p) ps [])
pop   (Zipper c ps (n:ns)) = (c, Zipper (Just n) ps ns)
