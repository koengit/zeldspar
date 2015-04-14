{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Zeldspar where

import Data.Typeable

infix  6 :=
infixr 5 :>
infixr 4 >>>

type VarId = Integer

data Ref a = Ref VarId

data Statement exp inp out where
  Emit    :: exp out -> Statement exp inp out
  Receive :: Typeable inp => Ref inp -> Statement exp inp out
  (:=)    :: Typeable a   => Ref a -> exp a -> Statement exp inp out

data Program exp inp out where
  (:>)   :: Statement exp inp out -> Program exp inp out -> Program exp inp out
  Loop   :: Program exp inp out -> Program exp inp out
  Return :: Program exp inp out
  EndL   :: Program exp inp out -> Program exp inp out

(>:) :: Program exp inp out -> Statement exp inp out -> Program exp inp out
p >: s = p >>: (s :> Return)

(>>:) :: Program exp inp out -> Program exp inp out -> Program exp inp out
(s :> p) >>: q = s :> (p >>: q)
Loop p   >>: _ = Loop p
Return   >>: q = q

(>>>) :: Program exp inp msg -> Program exp msg out -> Program exp inp out

-- termination
Return >>> q      = Return
p      >>> Return = Return

-- connect!
(Emit m :> p) >>> (Receive v :> q) = (v := m) :> (p >>> q)

((v := e) :> p) >>> q               = (v := e) :> (p >>> q)
p               >>> ((v := e) :> q) = (v := e) :> (p >>> q)

-- loop...
Loop p >>> Loop q = Loop ((p >>: EndL p) >>> (q >>: EndL q))

Loop Return >>> q = blockInp q
Loop p      >>> q = unloop p >>> q

p >>> Loop Return = blockOut p
p >>> Loop q      = p >>> unloop q

-- outside actions
(Receive v :> p) >>> q             = Receive v :> (p >>> q)
p                >>> (Emit m :> q) = Emit m :> (p >>> q)

-- end loop
EndL _ >>> EndL _ = Return
EndL p >>> q      = (p >>: EndL p) >>> q
p      >>> EndL q = p >>> (q >>: EndL q)

unloop :: Program exp inp out -> Program exp inp out
unloop (s :> p) = s :> Loop (p >: s)
unloop (Loop p) = unloop p

blockInp :: Program exp inp out -> Program exp xxx out
blockInp (Receive _ :> _) = Loop Return
blockInp (Emit m :> p)    = Emit m :> blockInp p
blockInp ((v := e) :> p)  = (v := e) :> blockInp p
blockInp (Loop p)         = Loop (blockInp p)
blockInp Return           = Return
blockInp (EndL p)         = blockInp (Loop p)

blockOut :: Program exp inp out -> Program exp inp xxx
blockOut (Emit _ :> _)    = Loop Return
blockOut (Receive v :> p) = Receive v :> blockOut p
blockOut ((v := e) :> p)  = (v := e) :> blockOut p
blockOut (Loop p)         = Loop (blockOut p)
blockOut Return           = Return
blockOut (EndL p)         = blockOut (Loop p)

-- | Interface for creating variables
class VarExp exp
  where
    varExp :: Typeable a => VarId -> exp a

-- | Interface for evaluating expressions
class EvalExp exp m
  where
    eval :: exp a -> m a
