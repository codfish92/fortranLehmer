MODULE Lehmer
	IMPLICIT NONE
	PRIVATE INTEGER:: n, g
	PUBLIC INTEGER:: SEED
	n = 2**31-1
	g = 7**5
	
	public seed
	public randint
	contains 
		subroutine seed(x)
			SEED = x
		end subroutine
		function randint() 
			randint = g*SEED % n
			call seed(randint)
		end

end 