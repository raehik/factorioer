module Factorioer.Types where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map

data Item = Item {
    itemName :: ItemName,
    itemDetails :: Maybe Text
} deriving (Show)

type ItemName = Text

data Recipe = Recipe {
    recipeIngredients :: [RecipePart],
    recipeProducts :: RecipeProducts,
    recipeCraftTime :: CraftTime,
    recipeCrafters :: [Crafter]
} deriving (Show)

-- Recipe parts are integers only.
data RecipePart = RecipePart {
    recipePartItem :: Item,
    recipePartNumber :: Int
} deriving (Show)

-- One has auto recipe title. Many requires a recipe title.
data RecipeProducts
    = RecipeProductsOne RecipePart
    | RecipeProductsMany Text [RecipePart]
    deriving (Show)

data Crafter = Crafter {
    crafterSpeedFactor :: Double
} deriving (Show)

data CraftTime = InstantCraftTime | CraftTimeTicks Ticks deriving (Show)

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
