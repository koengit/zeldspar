{-# LANGUAGE GADTs #-}
module Zeldspar where

infix  6 :=
infixr 5 :>
infixr 4 >>>

data Statement var inp out a where
  Emit    :: out -> Statement var inp out ()
  Receive :: var inp -> Statement var inp out ()
  Fresh   :: a -> Statement var inp out (var a)
  (:=)    :: var a -> a -> Statement var inp out ()

data Program var inp out a where
  (:>)   :: Statement var inp out a -> Program var inp out () -> Program var inp out ()
  Loop   :: Program var inp out () -> Program var inp out ()
  Return :: Program var inp out ()
  EndL   :: Program var inp out () -> Program var inp out ()

(>:) :: Program var inp out () -> Statement var inp out a -> Program var inp out ()
p >: s = p >>: (s :> Return)

(>>:) :: Program var inp out () -> Program var inp out () -> Program var inp out ()
(s :> p) >>: q = s :> (p >>: q)
Loop p   >>: _ = Loop p
Return   >>: q = q

(>>>) :: Program var inp msg a -> Program var msg out a -> Program var inp out a

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

EndL _ >>> EndL _ = Return
EndL p >>> q      = (p >>: EndL p) >>> q
p      >>> EndL q = p >>> (q >>: EndL q)

-- outside actions
(Receive v :> p) >>> q             = Receive v :> (p >>> q)
p                >>> (Emit m :> q) = Emit m :> (p >>> q)

unloop :: Program var inp out () -> Program var inp out ()
unloop (s :> p) = s :> Loop (p >: s)
unloop (Loop p) = unloop p

blockInp :: Program var inp out () -> Program var xxx out ()
blockInp (Receive _ :> _) = Loop Return
blockInp (Emit m :> p)    = Emit m :> blockInp p
blockInp ((v := e) :> p)  = (v := e) :> blockInp p
blockInp (Loop p)         = Loop (blockInp p)
blockInp Return           = Return
blockInp (EndL p)         = blockInp (Loop p)

blockOut :: Program var inp out () -> Program var inp xxx ()
blockOut (Emit _ :> _)    = Loop Return
blockOut (Receive v :> p) = Receive v :> blockOut p
blockOut ((v := e) :> p)  = (v := e) :> blockOut p
blockOut (Loop p)         = Loop (blockOut p)
blockOut Return           = Return
blockOut (EndL p)         = blockOut (Loop p)
