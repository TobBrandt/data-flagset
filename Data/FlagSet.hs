{-# LANGUAGE DeriveDataTypeable #-}
-- | 
-- = Introduction
-- This module provides a data type for a set of flags. Flags are stored
-- efficiently as bits of an unsigned integer.
--
-- This module is meant to be imported qualified:
--
-- @
-- import qualified Data.FlagSet as FlagSet
-- import Data.FlagSet (FlagSet)
-- @
--
-- The API is basically the same as that of "Data.Set" from the @containers@ package.
--
-- The functions `fromList` and `member` also have aliases that can be used 
-- unqualified:
-- 
-- @
-- import Data.FlagSet (flags, hasFlag)
-- @
--
-- = Examples
--
-- @
-- import qualified Data.FlagSet as FlagSet
-- import Data.FlagSet (FlagSet, flags, hasFlag)
--
-- data WeekDay = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
--      deriving (Eq, Show, Enum, Bounded)
--
-- workDays = flags [Monday .. Friday]
-- weekEndDays = flags [Saturday, Sunday]
-- allDays = FlagSet.full
--
-- worksOnTheWeekEnd :: FlagSet WeekDay -> Bool
-- worksOnTheWeekEnd = not . FlagSet.null . FlagSet.intersection weekEndDays
--
-- worksOnSunday :: FlagSet WeekDay -> Bool
-- worksOnSunday = hasFlag Sunday
--
-- workDayPay :: Rational
-- workDayPay = ...
-- weekEndDayPay :: Rational
-- weekEndDayPay = ...
--
-- pay :: FlagSet WeekDay -> Rational
-- pay daysWorking = workDayPay * countDays workDays + weekEndDayPay * countDays weekEndDays
--     where
--         countDays = fromIntegral . FlagSet.size . FlagSet.intersection daysWorking
-- @
module Data.FlagSet
    ( -- * Types
      FlagSet
      -- * Construction
    , fromList, flags, empty, singleton, full
      -- * Modification
    , insert, delete, union, unions, difference, intersection
      -- * Tests
    , member, hasFlag, null
      -- * Other
    , toList, size
    ) where

import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Word (Word32)
import Data.Bits (setBit, testBit, clearBit, (.|.), (.&.), complement, popCount)
import Data.Monoid (Monoid(..))
import Prelude hiding (null)

-- | A set of flags.
--
--   For most functions operating on flag sets, there must be an `Enum`
--   instance for @a@. Some functions also require `Bounded`.
--
--   You must ensure that `fromEnum` only returns values in the range
--   @[0 .. 31]@, otherwise an error can occur.
-- 
--   In the `Monoid` instance, `mempty` is `empty` and `mappend`/`<>` is `union`.
newtype FlagSet a = MkFlagSet Word32 deriving (Data, Typeable)

instance Eq (FlagSet a) where
    MkFlagSet x == MkFlagSet y = x == y

instance Ord (FlagSet a) where
    compare (MkFlagSet x) (MkFlagSet y) = compare x y

instance (Show a, Enum a) => Show (FlagSet a) where
    showsPrec p fs = showParen (p > 10) $
        showString "fromList " . shows (toList fs)

instance Monoid (FlagSet a) where
    mempty = empty
    mappend = union

-- | Alias for `fromList` that can be imported unqualified.
flags :: Enum a => [a] -> FlagSet a
flags = fromList

-- | Alias for `member` that can be imported unqualified.
hasFlag :: Enum a => a -> FlagSet a -> Bool
hasFlag = member

-- | Create a flag set from a list of flags. Input list can contain duplicates.
fromList :: Enum a => [a] -> FlagSet a
fromList vals = go vals 0
    where
        go [] acc = MkFlagSet acc
        go (v:vs) acc = withBit "fromList" v (go vs . setBit acc)

-- | Test whether a flag set contains a value.
member :: Enum a => a -> FlagSet a -> Bool
member v (MkFlagSet bits) = withBit "member" v (testBit bits)

withBit :: Enum a => String -> a -> (Int -> b) -> b
withBit fun v cont =
    let
        n = fromEnum v
    in
        if n < 0 || n > 31
            then error $ "Data.FlagSet." ++ fun ++ ": enum out of range"
            else cont n

-- | Convert a flag set to a list of values. The values will be ordered
--   according to the order defined by `fromEnum` and there will not be any
--   duplicates.
toList :: Enum a => FlagSet a -> [a]
toList (MkFlagSet bits) = map toEnum $ filter (testBit bits) [0 .. 31]

-- | The empty flag set.
empty :: FlagSet a
empty = MkFlagSet 0

-- | A flag set containing a single value.
singleton :: Enum a => a -> FlagSet a
singleton v = withBit "singleton" v (MkFlagSet . setBit 0)

-- | The union of two flag sets.
union :: FlagSet a -> FlagSet a -> FlagSet a
union (MkFlagSet x) (MkFlagSet y) = MkFlagSet (x .|. y)

-- | The union of multiple flag sets.
unions :: [FlagSet a] -> FlagSet a
unions = foldl union empty

-- | The difference of two flag sets.
difference :: FlagSet a -> FlagSet a -> FlagSet a
difference (MkFlagSet x) (MkFlagSet y) = MkFlagSet (x .&. complement y)

-- | The intersection of two flag sets.
intersection :: FlagSet a -> FlagSet a -> FlagSet a
intersection (MkFlagSet x) (MkFlagSet y) = MkFlagSet (x .&. y)

-- | The flag set that contains every value.
full :: (Enum a, Bounded a) => FlagSet a
full = fromList [minBound .. maxBound]

-- | Insert a value into a flag set.
insert :: Enum a => a -> FlagSet a -> FlagSet a
insert v (MkFlagSet bits) = withBit "insert" v (MkFlagSet . setBit bits)

-- | Remove a value from a flag set.
delete :: Enum a => a -> FlagSet a -> FlagSet a
delete v (MkFlagSet bits) = withBit "delete" v (MkFlagSet . clearBit bits)

-- | Test whether a flag set is empty.
null :: FlagSet a -> Bool
null = (== empty)

-- | The number of values in a flag set.
size :: FlagSet a -> Int
size (MkFlagSet bits) = popCount bits
