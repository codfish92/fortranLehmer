program test

use Lehmer
implicit none
INTEGER::x, i
REAL::uni, expon
call seed(100)
DO i =1,10
	x = randintRange(10, 30)
	uni=uniformRange(15,18)
	expon=exponential(2.)

	write(*,*) x, uni, expon
end do
end program 
