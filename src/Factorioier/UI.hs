{-# LANGUAGE OverloadedStrings #-}

module Factorioier.UI where

import Factorioier.Types
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
        recipeName = uiPartRecipeName recipe in
    UIPopupSegment {
        uiPopupSegmentTitle = title,
        uiPopupSegmentBody = Just (uiPartRecipeDetails recipe)
    }

uiPopupSegmentMaybeItemDetails :: Recipe -> Maybe UIPopupSegment
uiPopupSegmentMaybeItemDetails recipe =
    case recipeOutput recipe of
        RecipeOutputMany _ _ -> Nothing
        RecipeOutputOne part ->
            case (itemDetails . recipePartItem) part of
                Nothing -> Nothing
                Just details ->
                    Just $ UIPopupSegment {
                        uiPopupSegmentTitle = (itemName . recipePartItem) part,
                        uiPopupSegmentBody = Just details
                    }

uiPartRecipeName :: Recipe -> Text
uiPartRecipeName recipe =
    case recipeOutput recipe of
        RecipeOutputMany name _ -> name
        RecipeOutputOne part ->
            let outputItemName = (itemName . recipePartItem) part in
            case recipePartNumber part of
                1 -> outputItemName
                x -> tshow x <> " x " <> outputItemName

uiPartRecipeDetails :: Recipe -> Text
uiPartRecipeDetails _ = "(recipe details TODO)"
