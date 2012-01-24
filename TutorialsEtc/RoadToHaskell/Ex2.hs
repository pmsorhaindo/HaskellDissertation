class Fluffy f where
  furry :: (a -> b) -> f a -> f b
 
-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry = error "todo"
