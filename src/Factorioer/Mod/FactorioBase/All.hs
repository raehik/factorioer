{-# LANGUAGE OverloadedStrings #-}

module Factorioer.Mod.FactorioBase.All where

import Factorioer.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map

recipes = [rIronPlate, rIronGearWheel, rBeltYellow, rCopperPlate, rCopperCable, rGreenCircuit]

--------------------------------------------------------------------------------
-- Helper functions.

spriteAt :: Text -> Sprite
spriteAt = Sprite

recipeXOfItem :: Int -> Item -> Map Item Int -> CraftTime -> [Crafter] -> Recipe
recipeXOfItem num item ingrs craftTime crafters =
    Recipe {
        recipeIngredients = ingrs,
        recipeProducts = RecipeProductsOne (item, num),
        recipeCraftTime = craftTime,
        recipeCrafters = crafters
    }

recipeOneOfItem :: Item -> Map Item Int -> CraftTime -> [Crafter] -> Recipe
recipeOneOfItem = recipeXOfItem 1

--------------------------------------------------------------------------------
-- Crafter definitions.

crfPlayer = Crafter {crafterSpeedFactor=1}
crfsPlayerAndAssemblers = crfPlayer:crfsAssemblers
crfsAssemblers = [crfAssm1, crfAssm2, crfAssm3]
crfsFluidAssemblers = [crfAssm2, crfAssm3]
crfsFurnaces = [crfFurnaceIron, crfFurnaceSteel, crfFurnaceElectric]

crfAssm1 = Crafter {crafterSpeedFactor=0.5}
crfAssm2 = Crafter {crafterSpeedFactor=0.75}
crfAssm3 = Crafter {crafterSpeedFactor=1.25}
crfFurnaceIron = Crafter {crafterSpeedFactor=1}
crfFurnaceSteel = Crafter {crafterSpeedFactor=2}
crfFurnaceElectric = Crafter {crafterSpeedFactor=2}

--------------------------------------------------------------------------------
-- Item definitions.

iIronOre = Item {itemName="iron-ore", itemDetails=Nothing, itemSprite=spriteAt "ore/iron"}
iIronPlate = Item {itemName="iron-plate", itemDetails=Nothing, itemSprite=spriteAt "intm/iron-plate"}
iIronGearWheel = Item {itemName="iron-gear-wheel", itemDetails=Nothing, itemSprite=spriteAt "intm/iron-gear-wheel"}
iBeltYellow = Item {itemName="belt-yellow", itemDetails=Just "belt speed 15 items/s", itemSprite=spriteAt "log/belt/yellow/over"}

iCopperOre = Item {itemName="copper-ore", itemDetails=Nothing, itemSprite=spriteAt "ore/copper"}
iCopperPlate = Item {itemName="copper-plate", itemDetails=Nothing, itemSprite=spriteAt "intm/copper-plate"}
iCopperCable = Item {itemName="copper-cable", itemDetails=Nothing, itemSprite=spriteAt "intm/copper-cable"}

iGreenCircuit = Item {itemName="green-circuit", itemDetails=Nothing, itemSprite=spriteAt "intm/green-circuit"}

--------------------------------------------------------------------------------
-- Recipe definitions.

rIronPlate =
    let input = Map.singleton iIronOre 1 in
    recipeOneOfItem iIronPlate input (CraftTimeTicks 192) crfsFurnaces

rIronGearWheel =
    let input = Map.singleton iIronPlate 2 in
    recipeOneOfItem iIronGearWheel input (CraftTimeTicks 30) crfsPlayerAndAssemblers

rBeltYellow =
    let input = Map.insert iIronPlate 1 (Map.singleton iIronGearWheel 1) in
    recipeXOfItem 2 iBeltYellow input (CraftTimeTicks 30) crfsPlayerAndAssemblers

rCopperPlate =
    let input = Map.singleton iCopperOre 1 in
    recipeOneOfItem iCopperPlate input (CraftTimeTicks 192) crfsFurnaces

rCopperCable =
    let input = Map.singleton iCopperPlate 1 in
    recipeXOfItem 2 iCopperCable input (CraftTimeTicks 30) crfsPlayerAndAssemblers

rGreenCircuit =
    let input = Map.insert iCopperCable 3 (Map.singleton iIronPlate 1) in
    recipeOneOfItem iGreenCircuit input (CraftTimeTicks 30) crfsPlayerAndAssemblers

--------------------------------------------------------------------------------
-- Other.

-- Cheat item.
iMagicPutty = Item {itemName="magic-putty", itemDetails=Just "free item, for testing", itemSprite=spriteAt "etc/magic-putty"}
rMagicPutty = recipeOneOfItem iMagicPutty Map.empty InstantCraftTime crfsPlayerAndAssemblers
