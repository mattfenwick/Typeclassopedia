

# functor:
#  fmap :: (a -> b) -> f a -> f b

# pointed:
#  pure :: a -> f a

# applicative:
#  (<*>) :: f (a -> b) -> f a -> f b

# monad:
#  join :: m (m a) -> m a


class List:

  def __init__(self, items):
    self.items = items

  def fmap(self, f):
    return List(map(f, self.items))

  @staticmethod
  def pure(x):
    return List([x])

  def ap(self, arg):
    return [f(x) for f in self.items for x in arg.items]

  def join(self):
    out = []
    for l in self.items:
      out.extend(l)
    return List(out)

  def __repr__(self):
    return repr(self.items)


class Maybe:

  def __init__(self, value = None):
    if value:
      self.value = value
      self.isPresent = True
    else:
      self.isPresent = False

  @staticmethod
  def Just(x):
    return Maybe(x)

  Nothing = []Maybe()

  def fmap(self, f):
    if self.isPresent:
      return Just(f(self.value))
    else:
      return Nothing

  @staticmethod
  def pure(x):
    return Just(x)

  def ap(self, arg):
    if self.isPresent and arg.isPresent:
      return Just(self.value(arg.value))
    else:
      return Nothing

  def join(self):
    if self.isPresent:
      return self.value
    else:
      return Nothing
