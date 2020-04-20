{-# LANGUAGE OverloadedStrings #-}

module Factorioer.Core.Recipes where

import Factorioer.Types
import Factorioer.Utils
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map

-- Retrieve the name of a recipe.
--
-- For recipes with single products, the name is determined by that product.
-- Else, it must be provided.
recipeName :: Recipe -> Text
recipeName recipe =
    case recipeProducts recipe of
        RecipeProductsMany name _ -> name
        RecipeProductsOne part ->
            let productName = (itemName . recipePartItem) part in
            case recipePartNumber part of
                1 -> productName
                x -> tshow x <> " x " <> productName
