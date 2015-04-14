{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Backend where


import Language.Embedded.Imperative hiding (Program (..), Ref (..))
import qualified Language.Embedded.Imperative as Imp

import Zeldspar
import Frontend



type CMD exp
    =   RefCMD (VarPred exp) exp
    :+: ControlCMD exp
    :+: ConsoleCMD exp
    :+: TimeCMD exp

type Code exp = Imp.Program (CMD exp)

assign :: VarPred exp a => Ref a -> exp a -> Code exp ()
assign (Ref v) = Imp.setRef (Imp.RefComp v)

compile :: forall exp inp out . (VarPred exp inp, VarPred exp Bool, EvalExp exp) =>
    Prog exp inp out () -> Code exp (exp inp) -> (exp out -> Code exp ()) -> Code exp ()
compile p get put = go $ snd $ runProg p
  where
    go :: Program exp inp out -> Code exp ()
    go (Emit a    :> p) = put a >> go p
    go (Receive r :> p) = (get >>= assign r) >> go p
    go (r := a    :> p) = assign r a >> go p
    go (Loop p)         = while (return $ litExp True) (go p)
    go Return           = return ()
    -- EndL should not appear here

