program test

use Lehmer
implicit none
INTEGER::x, i
REAL::uni, expon
call seed(100)
DO i =1,10
	x = randint()
	uni=uniform()
	expon=exponential(2.)

	write(*,*) x, uni, expon
end do
end program 
