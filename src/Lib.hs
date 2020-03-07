module Lib
    ( someFunc
    ) where

import           Data.Complex
import           Generator

dft :: [Float] -> Int -> Complex Float
dft xs k = sum $ zipWith (\element index -> toComplex element * tc index) xs [0.0..]
  where tc n = toComplex (cos $ arg n) - toComplex (arg n) * imOne
        arg n = 2.0 * pi * fromIntegral k * n / fromIntegral (length xs)

toComplex :: Num a => a -> Complex a
toComplex x = x :+ 0

imOne :: Complex Float
imOne = 0.0 :+ 1.0

someFunc :: IO ()
someFunc = do
  let harmonics = 8
  let amplitude = 100
  let frequency = 1200
  let phase = 100
  let time = 5

  let signal =
             [ randomSignal harmonics
                            t
                            amplitude
                            frequency
                            phase
               | t <- [0 .. time -  1]
             ]

  let spectral = [dft signal k | k <- [0 .. length signal]]
  putStrLn . unlines . map show $ signal
  putStr . unlines . map show $ spectral
