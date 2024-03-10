module AocLib where

import Data.Array.IArray (IArray, array)
import Data.Ix (Ix)
import Data.List.Extra (splitOn)
import Relude

-- | Read a value and fail in case of errors
readFail :: (Read a) => String -> a
readFail s =
    case readEither s of
        Left e -> error e
        Right a -> a

{- | Read a 2D array into an immutable array with index starting at
top left corner (0, 0). All lines should have the same length.
-}
to2dArray :: forall i a. (Num i, Enum i, Ix i, IArray a Char) => String -> a (i, i) Char
to2dArray str =
    let lns :: [String]
        lns = filter ((> 0) . length) $ splitOn "\n" str

        toChars :: (i, String) -> [((i, i), Char)]
        toChars (row, s) = zipWith (\col c -> ((row, col), c)) [0 ..] s

        list2d :: [((i, i), Char)]
        list2d = concatMap toChars $ zip [0 ..] lns

        rowCount = length lns
        colCount = maybe 0 length $ viaNonEmpty head lns
     in array ((0, 0), (toEnum rowCount - 1, toEnum colCount - 1)) list2d
