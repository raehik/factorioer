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
data BaseIngredient = BaseIngredient {
    baseIngredientItem :: Item,
    baseIngredientAmount :: Double
} deriving (Show)

-- Return all recipes in the current ModConfig which produce this item.
itemRecipes :: ModConfig -> Item -> [Recipe]
itemRecipes mc item =
    filter (recipeProducesItem item) (modConfigRecipes mc)

-- Return True if the given recipe produces the given item.
recipeProducesItem :: Item -> Recipe -> Bool
recipeProducesItem item recipe =
    case recipeProducts recipe of
        RecipeProductsOne (product, _) ->
            if product == item then True else False
        RecipeProductsMany _ _ products ->
            Map.member item products

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
        RecipeProductsOne (product, num) ->
            let productName = itemName product in
            if num == 1
            then productName
            else tshow num <> " x " <> productName

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
        RecipeProductsOne (product, _) -> itemSprite product

recipeBaseIngrs mc recipe =
    let ingrs = recipeIngredients recipe
        ingrBaseIngrs = Map.foldrWithKey (itemBaseIngrs mc) [] ingrs in
    mergeBaseIngrs ingrBaseIngrs

itemBaseIngrs :: ModConfig -> Item -> Int -> [Map Item Double] -> [Map Item Double]
itemBaseIngrs mc item num ingrBaseIngrs = baseIngrs:ingrBaseIngrs
    where
        baseIngrs =
            case itemRecipes mc item of
                -- Exactly 1 recipe: base ingredients of that recipe, multiplied
                -- by (needed/produced)
                [recipe] ->
                    let produced = recipeNumOfProduct recipe item
                        productionFactor =
                            (fromIntegral num) / (fromIntegral produced) in
                    Map.map (* productionFactor) (recipeBaseIngrs mc recipe)

                -- 0 or multiple recipes: this is a base ingredient, combine
                -- with previous value
                _ -> Map.singleton item (fromIntegral num)

-- Concatenates a list of base ingredient maps, summing duplicate ingredient
-- amounts.
mergeBaseIngrs :: [Map Item Double] -> Map Item Double
mergeBaseIngrs m = foldl (Map.unionWith (+)) Map.empty m

-- TODO: code smell
recipeNumOfProduct recipe item =
    case recipeProducts recipe of
        RecipeProductsOne (_, num) -> num
        RecipeProductsMany _ _ products -> products Map.! item
