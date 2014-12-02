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
		function randintRange(a, b) Result(randomint)
			INTEGER::a, b
			INTEGER::randomint
			randomint = mod(randint(), (b-a)+1)+a 
		end function
		function uniformRange(a, b) Result (randomuni)
			INTEGER:: a, b
			REAL::randomuni
			randomuni=uniform()*(b-a)+a
		end function
		

end module 
