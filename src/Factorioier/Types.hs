{-# LANGUAGE OverloadedStrings #-}

-- Main goal: Model movement of items on a belt.
-- 2020-04-18T02:29:25+0100:
--   * Model placing things in the world, and some belt mechanics (if it end of
--     belt and there's a belt following, move in that tick
--   * Manage things via an TickMonad, which provides wrapping for applying
--     single ticks.
--   * Game executes state in TickMonad using original universe only (cellular
--     automaton-style)
-- World state is probably a list of tiles, and items with their owning tiles
-- (and location inside that tile).

module Factorioier.Types where

import Data.Text (Text)
import Data.Map (Map)

data Item = Item {
    itemName :: ItemName
} deriving (Show)

type ItemName = Text

data Recipe = Recipe {
    recipeItem :: Item,
    recipeIngredients :: [Item],
    recipeCraftTime :: CraftTime,
    recipeOutputNumber :: Int,
    recipePermittedCrafters :: [Crafter]
} deriving (Show)

data Crafter = Crafter {
    crafterSpeedFactor :: Double
} deriving (Show)

data CraftTime = InstantCraftTime | CraftTimeTicks Ticks deriving (Show)

type Ticks = Int

data Medium = Medium {
    mediumItem :: Item,
    --mediumDimensions :: Dimensions,
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

data World = World {
    worldTiles :: Map WorldLocation WorldTile,
    worldConfig :: WorldConfig
} deriving (Show)

-- True = used, False = empty
data WorldLocation = WorldLocation {
    worldLocationX :: Int,
    worldLocationY :: Int
} deriving (Show)

data WorldTile = WorldTile {
    worldTileTile :: Tile,
    worldTileContents :: TileContents
} deriving (Show)

data Tile = Tile {
    tileName :: TileName
} deriving (Show)

type TileName = Text

data TileContents = TileContents {
    tileContentsItems :: [Item]
} deriving (Show)

data WorldConfig = WorldConfig {
    worldConfigMaxItemsPerTile :: Int
} deriving (Show)

--------------------------------------------------------------------------------

crfPlayer = Crafter { crafterSpeedFactor = 1 }
crfFurnace = Crafter { crafterSpeedFactor = 0.5 }

iIronPlate = Item { itemName = "iron-plate" }
rIronPlate = Recipe {
    recipeItem = iIronPlate,
    recipeOutputNumber = 1,
    recipeIngredients = [iMagicPutty],
    recipeCraftTime = CraftTimeTicks 60,
    recipePermittedCrafters = [crfFurnace]
}

iBeltYellow = Item { itemName = "belt-yellow" }
rBeltYellow = Recipe {
    recipeItem = iBeltYellow,
    recipeOutputNumber = 2,
    recipeIngredients = [iMagicPutty],
    recipeCraftTime = CraftTimeTicks 60,
    recipePermittedCrafters = [crfPlayer]
}

mBeltYellowOneLane = Medium {
    mediumItem = iBeltYellow,
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
iMagicPutty = Item { itemName = "magic-putty" }
rMagicPutty = Recipe {
    recipeItem = iMagicPutty,
    recipeOutputNumber = 1,
    recipeIngredients = [],
    recipeCraftTime = InstantCraftTime,
    recipePermittedCrafters = [crfPlayer]
    }
