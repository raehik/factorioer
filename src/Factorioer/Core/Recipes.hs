{-# LANGUAGE OverloadedStrings #-}

module Factorioer.Core.Recipes where

import Factorioer.Types
import Factorioer.Utils
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- Supporting types and functions.

-- Recipe parts are integers, but base ingredients may be fractions depending on
-- ingredient/product ratios.
data RecipeBaseIngredient = RecipeBaseIngredient {
    recipeBaseIngredientItem :: Item,
    recipeBaseIngredientAmount :: Double
} deriving (Show)

-- Return all recipes in the current ModConfig which produce this item.
itemRecipes :: ModConfig -> Item -> [Recipe]
itemRecipes mc item =
    filter (recipeProducesItem item) (modConfigRecipes mc)

-- Return True if the given recipe produces the given item.
recipeProducesItem :: Item -> Recipe -> Bool
recipeProducesItem item recipe =
    case recipeProducts recipe of
        RecipeProductsOne part ->
            recipePartsIncludesItem item [part]
        RecipeProductsMany _ _ parts ->
            recipePartsIncludesItem item parts

-- Return True if the given recipe parts include the given item.
recipePartsIncludesItem :: Item -> [RecipePart] -> Bool
recipePartsIncludesItem item [] = False
recipePartsIncludesItem item (part:parts) =
    if recipePartItem part == item
    then True
    else recipePartsIncludesItem item parts

--------------------------------------------------------------------------------
-- Functions.

-- Return the name of a recipe.
--
-- For recipes with single products, the name is determined by that product.
-- Else, it must be provided.
--
-- Cacheable per mod config.
recipeName :: Recipe -> Text
recipeName recipe =
    case recipeProducts recipe of
        RecipeProductsMany name _ _ -> name
        RecipeProductsOne part ->
            let productName = (itemName . recipePartItem) part in
            case recipePartNumber part of
                1 -> productName
                x -> tshow x <> " x " <> productName

-- Return the icon of a recipe.
--
-- For recipes with single products, the icon is the product's sprite. Else, it
-- must be provided.
--
-- Cacheable per mod config.
recipeIcon :: Recipe -> Sprite
recipeIcon recipe =
    case recipeProducts recipe of
        RecipeProductsMany _ sprite _ -> sprite
        RecipeProductsOne part -> (itemSprite . recipePartItem) part

-- Return the base ingredients of a recipe in the given ModConfig.
--
-- A base ingredient is one which has either 0 or more than 1 producing recipes.
-- If 0, it's a raw ingredient, like mined items (ore) or other non-craftables.
-- If >1, we'd have to provide different possible paths for the recipe's base
-- ingredients, which we're not interested in since we just want to show a
-- useful overview.
--
-- Cacheable per mod config.
recipeBaseIngredients :: ModConfig -> Recipe -> [RecipeBaseIngredient]
recipeBaseIngredients mc recipe =
    let ingrs = recipeIngredients recipe in
    concatMap (recipePartBaseIngredients mc) ingrs

recipePartBaseIngredients :: ModConfig -> RecipePart -> [RecipeBaseIngredient]
recipePartBaseIngredients mc part =
    let item = recipePartItem part in
    case itemRecipes mc item of
        -- Exactly 1 recipe: base ingredients of that recipe
        [recipe] -> recipeBaseIngredients mc recipe

        -- 0 or many recipes: this is a base ingredient, return as-is
        _ -> [recipePartToBaseIngredient part]

recipePartToBaseIngredient :: RecipePart -> RecipeBaseIngredient
recipePartToBaseIngredient part =
    RecipeBaseIngredient {
        recipeBaseIngredientItem=recipePartItem part,
        recipeBaseIngredientAmount=fromIntegral (recipePartNumber part)
    }
