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

            x = log(1-uniform())/(-lambda)
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

        function gaussApprox(v) Result(gauss)
            REAL::v
            REAL::u1, u2
            REAL::pi = 4.*atan(1.)
            REAL::gauss

            ! Uses Box-Muller Transform
            !check if there is a value from the last pair 
            if(z1== -1000000.0) then 
                u1 = -2 * log(uniform())
                u2 = uniform() * 2 * pi

                gauss = sqrt(v * u1) * cos(u2)
                z1    = sqrt(v * u1) * sin(u2)
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
                success = success + bernoulli(p)
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
                gauss = gaussApprox(1.0)
                sumation = sumation + gauss**2
            end do
            chi = sumation
        end function

        function lognormal() Result(lognorm)
            REAL::lognorm

            lognorm = exp(gaussApprox(1.0))
        end function

        function geometric(p) Result(n)
            REAL::p
            INTEGER::n

            n = ceiling(log(1-uniform())/log(1-p))
        end function

        function poisson(lambda) Result(n)
            INTEGER::n
            REAL::acc, lambda, limit
            limit = exp(-1*lambda)
            acc = 1
            n = 0
            do while(acc > limit) 
                n = n+1
                acc = acc*uniform()
            end do
            n = n-1
        end function

end module 
