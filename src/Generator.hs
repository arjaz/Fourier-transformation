module Generator
        ( randomSignal
        )
where

import           Data.List
import           System.Random

-- someFunc :: IO ()
-- someFunc = do
--         let harmonic_number = 8
--         let max_amplitude   = 100
--         let max_frequency   = 1200
--         let max_phase       = 100
--         let max_time        = 1024

--         amplitude <- randomRIO (0, max_amplitude)
--         phase     <- randomRIO (0, max_phase)

--         start_time1 <- time
--         let signal1 =
--                     [ randomSignal harmonic_number
--                                    t
--                                    amplitude1
--                                    max_frequency
--                                    phase1
--                     | t <- [0 .. max_time - 1]
--                     ]

--         let signal =
--                    [ randomSignal harmonic_number
--                                   t
--                                   amplitude1
--                                   max_frequency
--                                   phase1
--                    | t <- [0 .. max_time - 1]
--                    ]
--         return signal

harmonic :: Int -> Float -> Float -> Float -> Float
harmonic time amplitude frequency phase =
        amplitude * sin (fromIntegral time * frequency - phase)

randomSignal :: Int -> Int -> Float -> Float -> Float -> Float
randomSignal harmonics time amplitude max_frequency phase = sum
        [ harmonic time amplitude new_max_frequency phase
        | new_max_frequency <-
                [0, max_frequency / fromIntegral (harmonics - 1) .. max_frequency]
        ]
