module Factorioer.Types where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map

-- The game configuration produced by the mods in use.
data ModConfig = ModConfig {
    modConfigRecipes :: [Recipe]
} deriving (Eq, Ord, Show)

data Item = Item {
    itemName :: ItemName,
    itemDetails :: Maybe Text,
    itemSprite :: Sprite
} deriving (Eq, Ord, Show)

type ItemName = Text

data Sprite = Sprite {
    spriteResPath :: ResPath
} deriving (Eq, Ord, Show)

type ResPath = Text

data Recipe = Recipe {
    recipeIngredients :: [RecipePart],
    recipeProducts :: RecipeProducts,
    recipeCraftTime :: CraftTime,
    recipeCrafters :: [Crafter]
} deriving (Eq, Ord, Show)

-- Recipe parts are integers only.
data RecipePart = RecipePart {
    recipePartItem :: Item,
    recipePartNumber :: Int
} deriving (Eq, Ord, Show)

-- Recipes with a single product get their name and icon from that product. For
-- recipes with multiple products, you need to provide a name and icon..
data RecipeProducts
    = RecipeProductsOne RecipePart
    | RecipeProductsMany Text Sprite [RecipePart]
    deriving (Eq, Ord, Show)

data Crafter = Crafter {
    crafterSpeedFactor :: Double
} deriving (Eq, Ord, Show)

data CraftTime
    = InstantCraftTime
    | CraftTimeTicks Ticks
    deriving (Eq, Ord, Show)

type Ticks = Int

--------------------------------------------------------------------------------

recipeXOfItem :: Int -> Item -> [RecipePart] -> CraftTime -> [Crafter] -> Recipe
recipeXOfItem num item ingrs craftTime crafters =
    Recipe {
        recipeIngredients = ingrs,
        recipeProducts = RecipeProductsOne $ recipePartXOfItem num item,
        recipeCraftTime = craftTime,
        recipeCrafters = crafters
    }

recipeOneOfItem :: Item -> [RecipePart] -> CraftTime -> [Crafter] -> Recipe
recipeOneOfItem = recipeXOfItem 1

recipePartXOfItem :: Int -> Item -> RecipePart
recipePartXOfItem num item =
    RecipePart {recipePartItem=item, recipePartNumber=num}

recipePartOneOfItem :: Item -> RecipePart
recipePartOneOfItem = recipePartXOfItem 1
