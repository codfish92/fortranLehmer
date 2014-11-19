MODULE Lehmer
	IMPLICIT NONE
	PRIVATE n, g, mod_seed
	INTEGER::n=2147483647, g=7**5, mod_seed
	contains 
		subroutine seed(x)
			INTEGER::mod_seed, x
			mod_seed = x
		end subroutine
		function randint() 
			INTEGER::randint
			randint = mod(g*mod_seed, n) 
			call seed(randint)
		end function

end module 