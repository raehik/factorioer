{-# LANGUAGE OverloadedStrings #-}

module Factorioer.UI where

import Factorioer.Types
import qualified Factorioer.Core.Recipes as Core.Recipes
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map

tshow :: Show a => a -> Text
tshow = T.pack . show

-- A popup window, intended for the ones that show up in your inventory when you
-- scroll over an item or recipe.
-- TODO: May be useable for other things too. Perhaps refactor and rename when
-- its usage is clearer.
type UIPopup = [UIPopupSegment]

-- A segment of a popup window, with its own title and body.
data UIPopupSegment = UIPopupSegment {
    uiPopupSegmentTitle :: Text,
    uiPopupSegmentBody :: Maybe Text
} deriving (Show)

-- Display a recipe's UIPopup.
uiPopupRecipe :: Recipe -> UIPopup
uiPopupRecipe recipe =
    case optionalItemSegment of
        Nothing -> [recipeSegment]
        Just itemSegment -> [recipeSegment, itemSegment]
    where
        recipeSegment = uiPopupSegmentRecipe recipe
        optionalItemSegment = uiPopupSegmentMaybeItemDetails recipe

uiPopupSegmentRecipe :: Recipe -> UIPopupSegment
uiPopupSegmentRecipe recipe =
    let title = "Recipe: " <> recipeName
        recipeName = Core.Recipes.recipeName recipe in
    UIPopupSegment {
        uiPopupSegmentTitle = title,
        uiPopupSegmentBody = Just (uiPartRecipeDetails recipe)
    }

uiPopupSegmentMaybeItemDetails :: Recipe -> Maybe UIPopupSegment
uiPopupSegmentMaybeItemDetails recipe =
    case recipeProducts recipe of
        RecipeProductsMany _ _ _ -> Nothing
        RecipeProductsOne part ->
            case (itemDetails . recipePartItem) part of
                Nothing -> Nothing
                Just details ->
                    Just $ UIPopupSegment {
                        uiPopupSegmentTitle = (itemName . recipePartItem) part,
                        uiPopupSegmentBody = Just details
                    }

uiPartRecipeDetails :: Recipe -> Text
uiPartRecipeDetails _ = "(recipe details TODO)"
