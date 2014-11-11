main = newton

{-
- 求めたい連立方程式
-}
e = exp 1
f1 x1 x2  = x1 - ( e ** x2 )
f2 x1 x2  = x1 - ( x2 ** 3 )

{-
- ニュートン法
-}
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

steep = do
		print "not implemented"
