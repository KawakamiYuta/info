main :: IO ()
main = do
		newton
		steep

{-
- 求めたい連立方程式
-}
e = exp 1
f1 x1 x2  = x1 - ( e ** x2 )
f2 x1 x2  = x1 - ( x2 ** 3 )

{-
- ニュートン法
-}
newton :: IO ()
newton = do
		--loop <閾値> <初期値> <ループカウンタ>--
		loop (0.000001 :: Double) (3.7 :: Double) 0
		where
			_newton old = old - ((f old) / (f' old))
			f x2 = (x2 ** 3) - (e ** x2) --f1(x1,x2) - f2(x1,x2)
			f' x2 = (3 * (x2 ** 2)) - ( e ** x2)
			loop thrshld ans cnt = if ( abs( f(ans) ) > thrshld ) 
								  			then loop thrshld (_newton(ans)) (cnt + 1)
								 			else do
												print "cnt, x1, x2, check"
												print cnt
												print another
												print ans
												print check
												where
													another = e ** ans
													check = f1 another ans
{-
- 再急降下法
-}

steep :: IO ()
steep = do
		--loop <閾値> <初期値:x1> <初期値:x2> <ループカウンタ>--
		loop (0.000001 :: Double) (10 :: Double) (100 :: Double) 0
		where
			loop :: Double -> Double -> Double -> Integer -> IO ()
			loop thrshld x1 x2 cnt = if ( abs( cost(x1 x2) ) > thrshld )
											then do
												print x1
												print x2
												loop thrshld (_steepx1(x1,x2)) (_steepx2(x1,x2)) (cnt +1)
											else do
												print "cnt, x1, x2, check"
												print cnt
												print x1
												print x2
			_steepx1 :: Double -> Double -> Double
			_steepx1 oldx1 oldx2 = oldx1 - 
				(cost''_px1x2(oldx1,oldx2) * cost'_px1(oldx1, oldx2)) - 
				(cost''_px1x2(oldx1, oldx2) * cost''_px2x2(oldx1,oldx2)) / 
				((cost''_px1x2(oldx1,oldx2) * cost''_px2x2(oldx1,oldx2)) - (cost''_px1x2(oldx1,oldx2) ** 2.0))
			_steepx2 :: Double -> Double -> Double
			_steepx2 oldx1 oldx2 = oldx2 - 
				(- (cost''_px1x2(oldx1,oldx2) * cost'_px1(oldx1, oldx2))) +
				(cost''_px1x1(oldx1, oldx2) * cost''_px1x2(oldx1,oldx2)) / 
				((cost''_px1x2(oldx1,oldx2) * cost''_px2x2(oldx1,oldx2)) - (cost''_px1x2(oldx1,oldx2) ** 2))
			cost :: Double -> Double -> Double
			cost x1 x2 = (2 * (x1 ** 2)) - (2 * x1 * (e ** (2 * x2))) - (2 * x1 * (x2 ** 3)) + (x2 ** 6)

			cost''_px1x1 x1 x2 = 4
			cost'_px1 x1 x2 = (4 * x1) - (2 * (e ** x2)) - (2 * (x2 ** 3))
			cost'_px2 x1 x2 = 2 * (e ** x2) - (2 * x1 * (e ** x2)) - (6 * x1 * (x2 ** 2)) + (6 * (x2 ** 5))
			cost''_px1x2 x1 x2 = - (2 * (e ** x2)) - (6 * (x2 ** 2))
			cost''_px2x2 x1 x2 = - (2 * x1 * (e ** x2)) + (4 * (e ** (2 * x2))) - (12 * x1 * x2) + 30 * (x2 ** 4)

