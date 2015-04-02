{-# LANGUAGE GADTs #-}
module Ziria where

infix  6 :=
infixr 5 :>
infixr 4 >>>

data Statement var inp out where
  Emit    :: out -> Statement var inp out
  Receive :: var inp -> Statement var inp out
  (:=)    :: var a -> a -> Statement var inp out

data Program var inp out a where
  (:>)   :: Statement var inp out -> Program var inp out a -> Program var inp out a
  Loop   :: Program var inp out () -> Program var inp out a
  Return :: a -> Program var inp out a
  EndL   :: Program var inp out () -> Program var inp out ()

(>:) :: Program var inp out a -> Statement var inp out -> Program var inp out ()
p >: s = p >>: (s :> Return ())

(>>:) :: Program var inp out a -> Program var inp out b -> Program var inp out b
(s :> p) >>: q = s :> (p >>: q)
Loop p   >>: _ = Loop p
Return _ >>: q = q

(>>>) :: Program var inp msg a -> Program var msg out a -> Program var inp out a

-- termination
Return x >>> q        = Return x
p        >>> Return x = Return x

-- connect!
(Emit m :> p) >>> (Receive v :> q) = (v := m) :> (p >>> q)

((v := e) :> p) >>> q               = (v := e) :> (p >>> q)
p               >>> ((v := e) :> q) = (v := e) :> (p >>> q)

-- loop...
Loop p >>> Loop q = Loop ((p >>: EndL p) >>> (q >>: EndL q))

Loop (Return _) >>> q = blockInp q
Loop p          >>> q = unloop p >>> q

p >>> Loop (Return _) = blockOut p
p >>> Loop q          = p >>> unloop q

EndL _ >>> EndL _ = Return ()
EndL p >>> q      = (p >>: EndL p) >>> q
p      >>> EndL q = p >>> (q >>: EndL q)

-- outside actions
(Receive v :> p) >>> q             = Receive v :> (p >>> q)
p                >>> (Emit m :> q) = Emit m :> (p >>> q)

unloop :: Program var inp out () -> Program var inp out a
unloop (s :> p) = s :> Loop (p >: s)
unloop (Loop p) = unloop p

blockInp :: Program var inp out a -> Program var xxx out a
blockInp (Receive _ :> _) = Loop (Return ())
blockInp (Emit m :> p)    = Emit m :> blockInp p
blockInp ((v := e) :> p)  = (v := e) :> blockInp p
blockInp (Loop p)         = Loop (blockInp p)
blockInp (Return x)       = Return x
blockInp (EndL p)         = blockInp (Loop p)

blockOut :: Program var inp out a -> Program var inp xxx a
blockOut (Emit _ :> _)    = Loop (Return ())
blockOut (Receive v :> p) = Receive v :> blockOut p
blockOut ((v := e) :> p)  = (v := e) :> blockOut p
blockOut (Loop p)         = Loop (blockOut p)
blockOut (Return x)       = Return x
blockOut (EndL p)         = blockOut (Loop p)
