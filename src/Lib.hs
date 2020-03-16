module Lib
  ( lab3
  ) where

import Data.Complex
import Data.List
import Generator
import Graphics.Gnuplot.Simple
import qualified Graphics.Gnuplot.Terminal.PNG as PNG

dft :: [Float] -> Int -> Complex Float
dft xs k =
  sum $ zipWith (\element index -> toComplex element * tc index) xs [0.0 ..]
  where
    tc n = toComplex (cos $ arg n) - toComplex (sin $ arg n) * imOne
    arg n = 2.0 * pi * fromIntegral k * n / fromIntegral (length xs)

fft :: [Float] -> Int -> Bool -> Complex Float
fft xs k lower
  | lower = evenPart + expPart * oddPart
  | otherwise = evenPart - expPart * oddPart
  where
    evenPart = dft (evens xs) k
    oddPart = dft (odds xs) k
    expPart = exp $ -2 * pi * fromIntegral k * imOne / fromIntegral (length xs)

evens :: [a] -> [a]
evens (x:xs) = x : odds xs
evens _ = []

odds :: [a] -> [a]
odds (_:xs) = evens xs
odds _ = []

toComplex :: Num a => a -> Complex a
toComplex x = x :+ 0

imOne :: Complex Float
imOne = 0.0 :+ 1.0

printTwoLists :: Show a => [a] -> [a] -> IO ()
printTwoLists xs ys =
  putStrLn . unlines . map (intercalate "      \t\t" . map show) . transpose $
  xs : [ys]

lab3 :: IO ()
lab3 = do
  let sHarmonics = 8
  let sAmplitude = 100
  let sFrequency = 1200
  let sPhase = 100
  let sTime = 60
  let signal =
        [ randomSignal sHarmonics t sAmplitude sFrequency sPhase
        | t <- [0 .. sTime - 1]
        ]
  let spectral_dft = [dft signal k | k <- [0 .. sTime - 1]]
  let spectral_fft =
        [fft signal k True | k <- [0 .. n1 - 1]] ++
        [fft signal k False | k <- [0 .. n2 - 1]]
        where
          n1 = (ceiling :: Float -> Int) $ fromIntegral (length signal) / 2
          n2 = (floor :: Float -> Int) $ fromIntegral (length signal) / 2
  putStrLn . unlines . map show $ signal
  printTwoLists spectral_dft spectral_fft
  plotLists
    [ Key Nothing
    , XLabel "Time"
    , YLabel "Value"
    , Title "Random signal"
    , terminal (PNG.cons "signal.png")
    ]
    [signal]
  plotLists
    [ Key Nothing
    , XLabel "Time"
    , YLabel "Value"
    , Title "DFT"
    , terminal (PNG.cons "dft.png")
    ]
    [map realPart spectral_dft, map imagPart spectral_dft]
  plotLists
    [ Key Nothing
    , XLabel "Time"
    , YLabel "Value"
    , Title "FFT"
    , terminal (PNG.cons "fft.png")
    ]
    [map realPart spectral_fft, map imagPart spectral_fft]
