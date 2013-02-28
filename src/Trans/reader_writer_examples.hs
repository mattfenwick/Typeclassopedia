{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
import Trans.MTrans
import Classes
import Datums
import Prelude hiding ((>>=), (>>), fail, foldr, foldl, fmap)

    

doIt = getState . getWriterT


-- no logging
fac2 :: MonadState (Int, Int) m => m Int
fac2 =
  get >>= \(total, n) ->
  put (total * n, n + 1) >>
  pure (total * n)


-- 'lift'ing the state operations
fac2''' :: MonadState (Int, Int) m => WriterT String m Int
fac2''' =
  lift get                               >>= \(total, n) ->
  let newt = total * n in
  lift (put (newt, n + 1))               >>
  (say ("well, looky: " ++ show newt))   >>
  pure newt


-- manual logging
fac2'' :: MonadState (Int, Int) m => m (String, Int)
fac2'' = 
  get                      >>= \(total, n) ->
  put (total * n, n + 1)   >>
  pure ("well, looky: " ++ show (total * n), total * n)


-- no lifting, no manual logging
fac2' :: (MonadState (Int, Int) m, MonadWriter String m) => m Int
fac2' =
  get                                    >>= \(total, n) ->
  let newt = total * n in
  put (newt, n + 1)                      >>
  write ("well, looky: " ++ show newt)   >>
  pure newt


-- 'lift'ing the state operations
s' :: MonadState Int m => WriterT [String] m Int
s' =
  say ["getting state"]     >>
  lift get                  >>= \n ->
  say ["got " ++ show n]    >>
  let sq = n * n in
  say ["putting state"]     >>
  lift (put sq)             >>
  say ["put " ++ show n]    >>
  pure sq


-- manual logging
s'' :: MonadState Int m => m ([String], Int)
s'' = 
  let l1 = "getting state"    in
  get                         >>= \n ->
  let l2 = "got " ++ show n   in
  let sq = n * n              in
  let l3 = "putting state"    in
  put sq                      >>
  let l4 = "put " ++ show n   in
  pure ([l1,l2,l3,l4], sq)


-- no lifting, no manual logging
s''' :: (MonadState Int m, MonadWriter [String] m) => m Int
s''' =
  write ["getting state"]   >>
  get                       >>= \n ->
  write ["got " ++ show n]  >>
  let sq = n * n in
  write ["putting state"]   >>
  put sq                    >>
  write ["put " ++ show n]  >>
  pure sq
