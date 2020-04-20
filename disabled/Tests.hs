{-# LANGUAGE OverloadedStrings #-}

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
    worldTiles :: Map WorldPosition WorldTile,
    worldConfig :: WorldConfig
} deriving (Show)

data WorldPosition = WorldPosition {
    worldPositionX :: Int,
    worldPositionY:: Int
} deriving (Show, Eq, Ord)

data WorldTile = WorldTile {
    worldTileTile :: Tile,
    worldTileDirection :: TileDirection,
    worldTileContents :: TileContents
} deriving (Show)

data Tile = Tile {
    tileName :: TileName
} deriving (Show)

type TileName = Text

data TileDirection
    = TileDirectionUp
    | TileDirectionRight
    | TileDirectionDown
    | TileDirectionLeft
    deriving (Show)

data TileContents = TileContents {
    tileContentsItems :: [Item]
} deriving (Show)

data WorldConfig = WorldConfig {
    worldConfigMaxItemsPerTile :: Int
} deriving (Show)

--------------------------------------------------------------------------------

-- Apply a single world tick.
tick :: World -> World
tick world =
    let tickedTiles = Map.mapWithKey (tickHelper world) (worldTiles world) in
    world { worldTiles = tickedTiles }

tickHelper :: World -> WorldPosition -> WorldTile -> WorldTile
tickHelper world pos tile =
    case getAdjacentTile world pos (worldTileDirection tile) of
        Nothing -> tile
        Just _ -> tile

-- Get adjacent tile in a given direction.
getAdjacentTile :: World -> WorldPosition -> TileDirection -> Maybe WorldTile
getAdjacentTile world (WorldPosition x y) dir =
    let (adjX, adjY) =
            case dir of
                TileDirectionUp     -> (x   , y+1   )
                TileDirectionRight  -> (x+1 , y     )
                TileDirectionDown   -> (x   , y-1   )
                TileDirectionLeft   -> (x-1 , y     )
        adjPos = WorldPosition adjX adjY in
    Map.lookup adjPos (worldTiles world)

--------------------------------------------------------------------------------

tBeltYellowOneLane = Tile {tileName="belt-yellow"}

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
