MODULE Lehmer
	IMPLICIT NONE
	PRIVATE n, g, mod_seed
	INTEGER*8::n=2147483647, g=48271, mod_seed = 0
	contains 
		subroutine seed(x)
			INTEGER::x
			mod_seed = x
		end subroutine
		function randint() Result(randomint)
			INTEGER::randomint
			randomint = mod(g*mod_seed, n)
			call seed(randomint)
		end function
		function uniform() Result(randomuni)
			REAL:: randomuni
			randomuni = randint()/(n*1.0)
		end function
		function exponential(lambda) Result(x)
			REAL:: x
			REAL::lambda
			x = -1*lambda*log(uniform())
		end function
		

end module 
