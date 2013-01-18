-----------------------------------------------------------------------------
-- |
-- Module      :  Sortie.Command.Deploy
-- Copyright   :  (c) Bitbase 2013
-- License     :  AllRightsReserved
--
-- Maintainer  :  trevor@bitba.se
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Sortie.Command.Deploy
    ( deploy )
where

import Sortie.Context          (Context(..))
import Sortie.Project          (Environment)

deploy :: Context -> Environment -> IO ()
deploy = undefined