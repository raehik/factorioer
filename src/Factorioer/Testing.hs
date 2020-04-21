module Factorioer.Testing where

import Factorioer.Types
import qualified Factorioer.Core.Recipes as FC.Recipes
import qualified Factorioer.Mod.FactorioBase.All as Mod.Base

modConfigBase = ModConfig {
    modConfigRecipes = Mod.Base.recipes
}
