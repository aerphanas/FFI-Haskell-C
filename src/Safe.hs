{-# LANGUAGE ForeignFunctionInterface #-}

module Safe where

import Foreign.C.Types

foreign export ccall fibonacci_hs :: CInt -> CInt

fibonacci :: Int -> Int
fibonacci n = fibs !! n
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibonacci_hs :: CInt -> CInt
fibonacci_hs = fromIntegral . fibonacci . fromIntegral
