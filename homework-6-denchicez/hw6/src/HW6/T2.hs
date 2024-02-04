{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module HW6.T2
  ( TSet

  , Contains
  , Add
  , Delete
  ) where

import GHC.TypeLits

type TSet = [Symbol]

-- | Check if name in set
type family Contains (name :: Symbol) (set :: TSet) :: Bool where
  Contains el '[] = 'False
  Contains el (el: xs) = 'True
  Contains el (_: xs) = Contains el xs

-- | Delete name from set
type family Delete (name :: Symbol) (set :: TSet) :: TSet where
  Delete el '[] = '[]
  Delete el (el: xs) = xs
  Delete el (x: xs) = x : Delete el xs

-- | Add name to set
type family Add (v :: Symbol) (set :: TSet) :: TSet where
  Add el arr = el : Delete el arr
