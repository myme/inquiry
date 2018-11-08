{-# LANGUAGE DeriveFunctor #-}

module Inquiry.Zipper
  ( Zipper
  , emptyZipper
  , insertZipper
  , nextZipper
  , peekZipper
  , popZipper
  , prevZipper
  ) where

data Zipper a = Zipper { zipperCurrent :: Maybe a
                       , zipperPrev :: [a]
                       , zipperNext :: [a]
                       } deriving (Eq, Functor, Show)

instance Foldable Zipper where
  foldMap _ (Zipper Nothing _ _) = mempty
  foldMap f (Zipper (Just c) p n) = foldMap f (reverse p) <> f c <> foldMap f n

emptyZipper :: Zipper a
emptyZipper = Zipper { zipperCurrent = Nothing
                     , zipperPrev = []
                     , zipperNext = []
                     }

insertZipper :: a -> Zipper a -> Zipper a
insertZipper x z@(Zipper Nothing _ _)   = z { zipperCurrent = Just x }
insertZipper x z@(Zipper (Just c) xs _) = z { zipperCurrent = Just x
                                            , zipperPrev = c : xs
                                            }

peekZipper :: Zipper a -> Maybe a
peekZipper (Zipper c _ _) = c

prevZipper :: Zipper a -> Zipper a
prevZipper z@(Zipper Nothing _ _) = z
prevZipper z@(Zipper (Just _) [] _) = z
prevZipper   (Zipper (Just c) (p:ps) ns) = Zipper (Just p) ps (c:ns)

nextZipper :: Zipper a -> Zipper a
nextZipper z@(Zipper Nothing _ _) = z
nextZipper z@(Zipper (Just _) _ []) = z
nextZipper   (Zipper (Just c) ps (n:ns)) = Zipper (Just n) (c:ps) ns

popZipper :: Zipper a -> (Maybe a, Zipper a)
popZipper z@(Zipper Nothing _ _) = (Nothing, z)
popZipper   (Zipper c [] []) = (c, emptyZipper)
popZipper   (Zipper c (p:ps) []) = (c, Zipper (Just p) ps [])
popZipper   (Zipper c ps (n:ns)) = (c, Zipper (Just n) ps ns)
