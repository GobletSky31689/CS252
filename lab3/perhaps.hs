data Perhaps a =
    PerhapsSo a
  | PerhapsNot
  deriving Show

instance Functor Perhaps where
    fmap f (PerhapsSo x) = PerhapsSo (f x)
    fmap f PerhapsNot = PerhapsNot

