{-# LANGUAGE RankNTypes #-}

import Data.Functor.Const
import Control.Monad.Identity

type Lens a b = forall f. Functor f => (b -> f b) -> (a -> f a)

over :: Traversal a b -> (b -> b) -> a -> a
over lens f = runIdentity . lens (\b -> Identity $ f b)

view :: Lens a b -> a -> b
view lens = getConst . lens (\b -> Const b)

type Traversal a b = forall f . Applicative f => (b -> f b) -> (a -> f a)

trav :: Traversable t => Traversal (t a) a
trav f = \ta -> traverse f ta

data Point = Point { _x :: Int, _y :: Int} deriving (Show)

data Rect = Rect { _a :: Point, _b :: Point} deriving (Show)

data Shape = Shape { _points :: [Point] } deriving (Show)

a :: Lens Rect Point
a f = \rect -> fmap (\newa -> rect { _a = newa}) (f (_a rect))

b :: Lens Rect Point
b f = \rect -> fmap (\newb -> rect { _b = newb}) (f (_b rect))

x :: Lens Point Int
x f = \point -> fmap (\newx -> point { _x = newx}) (f (_x point))

y :: Lens Point Int
y f = \point -> fmap (\newy -> point { _y = newy}) (f (_y point))

points :: Lens Shape [Point]
points f = \shape -> fmap (\newpoints -> shape {_points = newpoints}) (f (_points shape))

r = Rect { _a = Point { _x = 0, _y = 0}, _b = Point { _x = 2, _y = 2}}
s = Shape { _points = [Point {_x = 0, _y = 0}, Point {_x = 1, _y = 3}, Point {_x = 3, _y = 2}]}

