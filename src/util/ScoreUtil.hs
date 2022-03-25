module ScoreUtil where
import Database.MongoDB (Value(Float))

calculateScoreModifier :: Int -> Float
calculateScoreModifier seconds = min 32 (2 ** (seconds' / 600))
    where seconds' = fromIntegral seconds