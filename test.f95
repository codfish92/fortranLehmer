program test

use Lehmer
implicit none
INTEGER::x, i, bio, ber
REAL::uni, expon, gauss
call seed(1000)
DO i =1,10
	x = randintRange(10, 30)
	uni=uniformRange(15,18)
	expon=exponential(2.)
	gauss = gaussApprox()
	bio = binomial(100, .5)
	ber = bernoulli(.2)

	write(*,*) x, uni, expon, gauss, bio, ber
end do
end program 
