MODULE Lehmer
    IMPLICIT NONE
    PRIVATE n, g, mod_seed, z1
    INTEGER*8::n=2147483647, g=48271, mod_seed = 0
    REAL::z1 = -1000000.0
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

        function gaussApprox() Result(gauss)
            REAL::u1, u2
            REAL::pi = 4.*atan(1.)
            REAL::gauss
            !check if there is a value from the last pair 
            if(z1== -1000000.0) then 
                u1 = uniform()
                u2 = uniform()
                gauss = ((-2*log(u1))**.5) * sin(2*pi*u2)
                z1 = ((-2*log(u1))**.5) * cos(2*pi*u2)
            else
                gauss = z1;
                z1 = -1000000.0;
                !just use the second value from the last genration
            end if
        end function

        function binomial(n, p) Result(success)
            INTEGER::n, success, i
            REAL::p, uni
            success = 0
            do i = 1,n
                uni = uniform()
                if(uni <= p) then
                    success = success+1
                end if
            end do
        end function

        function bernoulli(p) Result(success)
            INTEGER::success
            REAL::uni, p
            success = 0
            uni = uniform()
            if(uni <= p) then
                success = 1
            end if
        end function

        function chisquare(n) Result(chi)
            INTEGER::n, i
            REAL::chi, gauss, sumation
            sumation = 0
            do i = 1,n
                gauss = gaussApprox()
                sumation = sumation + gauss**2
            end do
            chi = sumation
        end function

        function lognormal() Result(lognorm)
            REAL::lognorm
            lognorm = log(gaussApprox())

        end function

        function geometric(p) Result(n)
            REAL::p
            INTEGER::n
            n = 0
            do
                n = n+1
                if(bernoulli(p)==1) then
                    exit
                end if
            end do

        end function


end module 
