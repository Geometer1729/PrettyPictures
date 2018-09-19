import Data.List
import Data.Bits
import Data.Word
import Data.ByteString (pack) 
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss
import System.Environment
--import Debug.Trace
{-
display :: [[Bool]] -> String
display [] = ""
display (xs:xss) = concat [dispRow xs,"\n",display xss]
-}
dispRow::[Bool]->String
dispRow [] = "|"
dispRow (x:xs) = (if x then 'X' else ' '):(dispRow xs)


mkGrid :: Int -> [[Bool]] -- grid size 2^n - 1
mkGrid 1 = [[True]]
mkGrid n = joinV [top,middle,bottom]
	where
		small = mkGrid (n-1)
		inSmall = invert small
		fliped = transpose small
		top = joinH [small,consGrid 1 (2^(n-1)-1) False,fliped]
		middle = joinH [consGrid (2^(n-1)) 1 True,consGrid (2^(n-1)-1) 1 False]
		bottom = joinH [inSmall,consGrid 1 (2^(n-1)-1) True,fliped]

joinH :: [[[a]]] -> [[a]]
joinH ([]:_) = []
joinH xss = (concat $ map head xss):(joinH $ map tail xss)

joinV :: [[[a]]] -> [[a]]
joinV = concat

invert = map $ map not

consGrid :: Int -> Int -> a -> [[a]]
consGrid n m x = take m $ cycle [ take n $ cycle [x]]

signs:: Int -> [[Bool]]
signs n = joinV [consGrid (2^n) 1 False,joinH [consGrid 1 (2^n-1) False,mkGrid n]]

index:: Int -> [[Int]]
index n = [[xor x y | x <- [0..(2^n-1)] ] | y <- [0..(2^n-1)]]

mapedZip :: [[a]] -> [[b]] -> [[(a,b)]]
mapedZip = zipWith zip

table:: Int -> [[(Int,Bool)]]
table n = mapedZip (index n) (signs n)


tableWNegs :: Int -> [[(Int,Bool)]]
tableWNegs n = colapse [[reg,inv],[inv,reg]]
	where
		reg = table n
		inv = (map (map (fmap not))) (table n)

colapse :: [[[[a]]]] -> [[a]]
colapse = joinV . (map joinH)

smallGrid:: Int -> (Int,Bool) -> [[[Word8]]]
smallGrid base (n,s) = [[c,c,c],[c,if s then w else c,c],[c,c,c]]
	where
		tau = 2*pi
		phase0 = (fromIntegral n)*(2^(8-(fromIntegral base)))*tau/256
		phase1 = phase0 + tau/3
		phase2 = phase0 + 2*tau/3
		r = round $ 255*(sin phase0)
		g = round $ 255*(sin phase1)
		b = round $ 255*(sin phase2)
		c = [r,g,b,255] :: [Word8]
		w = [255,255,255,255]

pixelData :: Int -> [[[Word8]]]
pixelData n = colapse $ map (map (smallGrid n)) (tableWNegs n)

asPic :: Int -> Picture --PxRGBA
asPic n = bitmapOfByteString (6*2^n) (6*2^n) (BitmapFormat TopToBottom PxRGBA) (pack ((concat . concat) (pixelData n))) True

main :: IO()
main = do 
	[n] <- fmap (map read) getArgs
	display (InWindow "Finna Woke" (6*2^n,6*2^n) (1,1) ) black (asPic n)


