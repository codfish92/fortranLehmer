program test

use Lehmer
implicit none
INTEGER::x, i
call seed(100)
DO i =1,10
	x = randint()

	write(*,*) x
end do
end program 