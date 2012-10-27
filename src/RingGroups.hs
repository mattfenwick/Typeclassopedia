module RingGroups (


) where



-- 1. closure
-- 2. associativity
class Semigroup' a where
  (<|>) :: a -> a -> a


-- 3. identity element
class (Semigroup' a) => Monoid' a where
  empty :: a


-- 4. inverse element
class (Semigroup' a) => Group' a where
  inverse :: a -> a


-- hmm, this is wrong:  commutativity needs its own class
--   i.e. you can have commutative monoids that aren't groups or abelians ... I think
--   then an Abelian group would just be something that's both a group and commutative

-- 5. commutativity
class (Monoid' a) => Abelian' a where
  (how do I express that this is empty?)




-- 1. commutative monoid under addition
-- 2. monoid under multiplication
-- 3. multiplication distributes over addition
-- 4. x * 0 = 0 * x = 0
-- so ... how can I get this to subclass monoid in two different ways ???
class Semiring' a where
  zero :: a
  <++> :: a -> a -> a
  one  :: a
  <**> :: a -> a -> a
  distribute :: a -> a -> a -> a -- um, take this out.  or find examples where it's useful (has to better than just numbers)


-- 1. a is a group under addition
-- 2. a is a semigroup under multiplication
-- 3. right distribution of multiplication over addition
class Nearring' a where
  ???


-- 1. Abelian group under addition
-- 2. monoid under multiplication
-- 3. distribution of * over +
class Ring' a where
  ???


-- same as Ring', but * also commutative
class Commutativering' a where
  ???


