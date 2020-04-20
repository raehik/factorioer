{-# LANGUAGE OverloadedStrings #-}

module Factorioer.Definitions.FactorioBase.All where

import Factorioer.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map

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

iIronOre = Item {itemName="iron-ore", itemDetails=Nothing}
iIronPlate = Item {itemName="iron-plate", itemDetails=Nothing}
iIronGearWheel = Item {itemName="iron-gear-wheel", itemDetails=Nothing}
iBeltYellow = Item {itemName="belt-yellow", itemDetails=Just "belt speed 15 items/s"}

--------------------------------------------------------------------------------
-- Recipe definitions.

rIronPlate =
    let input = [(recipePartOneOfItem iIronOre)] in
    recipeOneOfItem iIronPlate input (CraftTimeTicks 192) crfsFurnaces

rIronGearWheel =
    let input = [(recipePartXOfItem 2 iIronPlate)] in
    recipeOneOfItem iIronGearWheel input (CraftTimeTicks 30) crfsPlayerAndAssemblers

rBeltYellow =
    let input = [
            (recipePartOneOfItem iIronPlate),
            (recipePartOneOfItem iIronGearWheel)
            ] in
    recipeXOfItem 2 iBeltYellow input (CraftTimeTicks 30) crfsPlayerAndAssemblers

--------------------------------------------------------------------------------
-- Other.

-- Cheat item.
iMagicPutty = Item {itemName="magic-putty", itemDetails=Just "free item, for testing"}
rMagicPutty = recipeOneOfItem iMagicPutty [] InstantCraftTime crfsPlayerAndAssemblers
