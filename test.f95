program test

use Lehmer
implicit none
INTEGER::x, i
REAL::uni, expon, gauss
call seed(100)
DO i =1,10
	x = randintRange(10, 30)
	uni=uniformRange(15,18)
	expon=exponential(2.)
	gauss = gaussApprox()

	write(*,*) x, uni, expon, gauss
end do
end program 
