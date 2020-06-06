import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable
import System.Process

asdr :: Float -> Float
asdr t
  | t < 0.2     = 5.0 * t
  | t < 1.1/3.0 = (-3.0) * t + 1.6
  | t < 0.8     = 0.5
  | t < 1.0     = (-4.0) * t + 3.7
  | otherwise  = 0

sampleRate :: Float
sampleRate = 48000

music = concat $ take 20 $ repeat $ note 440 0.25

note :: Float -> Float -> [Float]
note freq duration = zipWith (*) volumeMap $ map sin $ map (* (freq * 2 * pi / sampleRate)) $ [0.0 .. sampleRate * duration]
  where
    volumeMap = map asdr [0.0, 1.0/(sampleRate * duration) ..]

save :: [Float] -> IO ()
save sound = B.writeFile "output.bin" $ B.toLazyByteString $ fold $ map B.floatLE sound

play :: [Float] -> IO ()
play sound = do
  save sound
  _ <- runCommand ("ffplay -f f32le -ar 48000 output.bin")
  return ()
