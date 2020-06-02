{-# LANGUAGE OverloadedStrings #-}

module Factorioer.Core.Recipes where

import Factorioer.Types
import Factorioer.Utils
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

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

type RecipeBase = ([Map Item Double], RecipeIngredients)

recipeBaseIngrs :: ModConfig -> Recipe -> RecipeBase
recipeBaseIngrs mc recipe = recipeBaseIngrs' mc recipe Map.empty Set.empty

recipeBaseIngrs' :: ModConfig -> Recipe -> RecipeIngredients -> Set Item -> RecipeBase
recipeBaseIngrs' mc recipe products =
    let ingrs = recipeIngredients recipe
        (ingrBaseIngrs, byproducts) = Map.foldrWithKey (itemBaseIngrs mc) ([], products) ingrs in
   (mergeBaseIngrs ingrBaseIngrs, byproducts)

itemBaseIngrs :: ModConfig -> Item -> Int -> RecipeBase -> RecipeBase
itemBaseIngrs mc item num (ingrBaseIngrs, products)
    | numToCraft == 0 = (ingrBaseIngrs, products')
    | otherwise =
        case itemRecipes mc item of
            -- Exactly 1 recipe: base ingredients of that recipe, multiplied
            -- by (needed/produced) of wanted product
            [recipe] ->
                case recipeProducts recipe of
                    RecipeProductsOne (_, produced) ->
                        let itemProductionFactor =
                                (fromIntegral num) / (fromIntegral produced)
                            (itemBaseIngrs, products'') =
                                recipeBaseIngrs' mc recipe products'
                            itemBaseIngrs' = Map.map (* itemProductionFactor) itemBaseIngrs
                        in ((itemBaseIngrs':ingrBaseIngrs), products'')
                    RecipeProductsMany _ _ rProducts ->
                        let produced = rProducts Map.! item
                            itemProductionFactor =
                                (fromIntegral num) / (fromIntegral produced)
                            products'' =
                                Map.unionWith (+) products' (Map.delete item rProducts)
                            (itemBaseIngrs, products''') =
                                recipeBaseIngrs' mc recipe products''
                            itemBaseIngrs' = Map.map (* itemProductionFactor) itemBaseIngrs
                        in ((itemBaseIngrs':ingrBaseIngrs), products''')

            -- 0 or multiple recipes: this is a base ingredient, combine
            -- with previous value
            _ ->
                let itemBaseIngrs = Map.singleton item (fromIntegral num) in
                ((itemBaseIngrs:ingrBaseIngrs), products')
    where
        (numToCraft, products') = calculateNeededCrafts item num products

calculateNeededCrafts :: Item -> Int -> Map Item Int -> (Int, Map Item Int)
calculateNeededCrafts item num products =
    case Map.lookup item products of
        Nothing -> (num, products)
        Just present ->
            let remaining = present - num in
            if remaining > 0 then
                (0, Map.insert item remaining products)
            else if remaining == 0 then
                (0, Map.delete item products)
            else
                ((-remaining), Map.delete item products)

-- Concatenates a list of base ingredient maps, summing duplicate ingredient
-- amounts.
mergeBaseIngrs :: [Map Item Double] -> Map Item Double
mergeBaseIngrs m = foldl (Map.unionWith (+)) Map.empty m

-- TODO: code smell
recipeNumOfProduct recipe item =
    case recipeProducts recipe of
        RecipeProductsOne (_, num) -> num
        RecipeProductsMany _ _ products -> products Map.! item
