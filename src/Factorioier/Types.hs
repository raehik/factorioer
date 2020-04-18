-- Main goal: Model movement of items on a conveyor belt.

module Factorioier.Types where

import Data.Text (Text)

type Ticks = Int

-- Something which can be:
--   - Transported via a Medium.
--   - Created.
--   - Placed in a Tile.
data Item = Item {
    itemRecipe :: Recipe
} deriving (Show)

data Recipe = Recipe {
    recipePermittedCrafters :: [Crafter],
    recipeCraftTime :: CraftTime,
    recipeIngredients :: [Item],
    recipeOutputNumber :: Int
} deriving (Show)

data Crafter = Crafter {
    crafterSpeedFactor :: Double
} deriving (Show)

data CraftTime = InstantCraftTime | CraftTimeTicks Ticks deriving (Show)

data Medium = Medium {
    mediumItem :: Item,
    mediumDimensions :: Dimensions,
    mediumCapacity :: MediumCapacity,
    mediumSpeed :: MediumSpeed
} deriving (Show)

data MediumCapacity
    = FiniteCapacity Int
    | InfiniteCapacity
    deriving (Show)

data MediumSpeed
    = FiniteSpeed Double
    | InfiniteSpeed
    deriving (Show)

-- True = used, False = empty
data Dimensions = Dimensions [[Bool]] deriving (Show)

--------------------------------------------------------------------------------

crfPlayer = Crafter { crafterSpeedFactor = 1 }
crfFurnace = Crafter { crafterSpeedFactor = 0.5 }

iIronPlate = Item {
    itemRecipe = Recipe {
        recipeOutputNumber = 1,
        recipeIngredients = [iMagicPutty],
        recipeCraftTime = CraftTimeTicks 60,
        recipePermittedCrafters = [crfFurnace]
    }
}

iConveyorBeltYellow = Item {
    itemRecipe = Recipe {
        recipeOutputNumber = 2,
        recipeIngredients = [iMagicPutty],
        recipeCraftTime = CraftTimeTicks 60,
        recipePermittedCrafters = [crfPlayer]
    }
}

mConveyorBeltYellowOneLane = Medium {
    mediumItem = iConveyorBeltYellow,
    mediumDimensions = Dimensions [[True]],
    mediumCapacity = FiniteCapacity 4,
    mediumSpeed = FiniteSpeed 7.5
}

--------------------------------------------------------------------------------
-- Tests, playing.

class HasItem a where
    getItem :: a -> Item
instance HasItem Item where
    getItem = id
instance HasItem Medium where
    getItem = mediumItem

-- Cheat item.
iMagicPutty = Item {
    itemRecipe = Recipe {
        recipeOutputNumber = 1,
        recipeIngredients = [],
        recipeCraftTime = InstantCraftTime,
        recipePermittedCrafters = [crfPlayer]
    }
}
