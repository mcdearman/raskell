module Runtime.Env where

import Data.Text
import Syntax.SExpr

type Env = [(Text, SExpr)]