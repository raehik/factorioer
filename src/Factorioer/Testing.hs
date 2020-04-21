{-# LANGUAGE OverloadedStrings #-}

module Factorioer.Testing where

import Factorioer.Types
import qualified Factorioer.Core.Recipes as FC.Recipes
import qualified Factorioer.Mod.FactorioBase.All as Mod.Base
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

modConfigBase = ModConfig {
    modConfigRecipes = Mod.Base.recipes
}

--------------------------------------------------------------------------------

modConfigTest = ModConfig {
    modConfigRecipes = [rWoodenPony, rDecoratedWoodenPony]
}

spriteAt :: Text -> Sprite
spriteAt = Sprite

crfsAny = [Crafter {crafterSpeedFactor=1}]

iWood = Item {itemName="wood", itemDetails=Nothing, itemSprite=spriteAt "dummy"}
iWoodenPony = Item {itemName="wooden-pony", itemDetails=Nothing, itemSprite=spriteAt "dummy"}
iWoodenShavings = Item {itemName="wooden-shavings", itemDetails=Nothing, itemSprite=spriteAt "dummy"}
iDecoratedWoodenPony = Item {itemName="decorated-wooden-pony", itemDetails=Nothing, itemSprite=spriteAt "dummy"}

rWoodenPony = Recipe {
    recipeIngredients = Map.singleton iWood 1,
    recipeProducts = RecipeProductsMany "Wooden pony" (spriteAt "dummy") (Map.insert iWoodenShavings 4 (Map.singleton iWoodenPony 1)),
    recipeCraftTime = CraftTimeTicks 300,
    recipeCrafters = crfsAny
}
rDecoratedWoodenPony = Recipe {
    recipeIngredients = Map.insert iWoodenShavings 2 (Map.singleton iWoodenPony 1),
    recipeProducts = RecipeProductsOne (iDecoratedWoodenPony, 1),
    recipeCraftTime = CraftTimeTicks 60,
    recipeCrafters = crfsAny
}
