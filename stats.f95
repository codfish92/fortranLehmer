program stats

  use Lehmer
  implicit none
  INTEGER::aseed, iters, i

  ! All s_* (stats) vars are just the following
  ! s*(1) - Min
  ! s*(2) - Max
  ! s*(3) - Mean
  ! s*(4) - Old_Mean
  ! s*(5) - Variance * (N-1)
  ! s*(6) - Old_Variance
  REAL::sbin(6), sber(6), sgeo(6), schi(6)
  REAL::spoi(6), sgau(6), sexp(6)

  ! rand_int, rand_real -- Used to store random draws
  INTEGER::ri
  REAL::rr


  WRITE(*,*) "Enter a seed"
  READ(*,*) aseed
  WRITE(*,*) "---------------------------------"
  WRITE(*,*) "--- Running 100000 iterations ---"
  WRITE(*,*) "---------------------------------"

  call seed(aseed)

  iters = 100000
  do i=1,iters
     !Bernoulli(0.25) -- 1/4 of the time it should succeed (eg. return 1)
     ri = bernoulli(0.25)
     rr = REAL(ri)
     if (i == 1) then
        sber = (/rr,rr,rr,rr,0.0,0.0/)
     else
        sber(1) = MIN(sber(1), rr)
        sber(2) = MAX(sber(2), rr)
        sber(3) = sber(4)+(ri-sber(4))/i
        sber(5) = sber(6)+(ri-sber(4))*(ri-sber(3))

        sber(4) = sber(3)
        sber(6) = sber(5)
     end if

     !Binomial(100, 0.5) -- 100 Coin Flips
     ri = binomial(100, 0.5)
     rr = REAL(ri)
     if (i == 1) then
        sbin = (/rr,rr,rr,rr,0.0,0.0/)
     else
        sbin(1) = MIN(sbin(1), rr)
        sbin(2) = MAX(sbin(2), rr)
        sbin(3) = sbin(4)+(ri-sbin(4))/i
        sbin(5) = sbin(6)+(ri-sbin(4))*(ri-sbin(3))

        sbin(4) = sbin(3)
        sbin(6) = sbin(5)
     end if

     !Geometric(0.33) -- Number of attempts for 1 == bernoulli(0.33)
     ri = geometric(0.33)
     rr = REAL(ri)
     if (i == 1) then
        sgeo = (/rr,rr,rr,rr,0.0,0.0/)
     else
        sgeo(1) = MIN(sgeo(1), rr)
        sgeo(2) = MAX(sgeo(2), rr)
        sgeo(3) = sgeo(4)+(ri-sgeo(4))/i
        sgeo(5) = sgeo(6)+(ri-sgeo(4))*(ri-sgeo(3))

        sgeo(4) = sgeo(3)
        sgeo(6) = sgeo(5)
     end if

     !Poisson(mu) - Mean = mu, Var = mu
     ri = poisson(50.0)
     rr = REAL(ri)
     if (i == 1) then
        spoi = (/rr,rr,rr,rr,0.0,0.0/)
     else
        spoi(1) = MIN(spoi(1), rr)
        spoi(2) = MAX(spoi(2), rr)
        spoi(3) = spoi(4)+(ri-spoi(4))/i
        spoi(5) = spoi(6)+(ri-spoi(4))*(ri-spoi(3))

        spoi(4) = spoi(3)
        spoi(6) = spoi(5)
     endif

     !Exponential(lambda) - Mean = 1/lambda, SDev = 1/lambda
     rr = exponential(0.1)
     if (i == 1) then
        sexp = (/rr,rr,rr,rr,0.0,0.0/)
     else
        sexp(1) = MIN(sexp(1), rr)
        sexp(2) = MAX(sexp(2), rr)
        sexp(3) = sexp(4)+(rr-sexp(4))/i
        sexp(5) = sexp(6)+(rr-sexp(4))*(rr-sexp(3))

        sexp(4) = sexp(3)
        sexp(6) = sexp(5)
     endif

     !Gaussian-Approximation
     ! Mean = 0
     ! StdDev = 1
     rr = gaussApprox(1.0)
     if (i == 1) then
        sgau = (/rr,rr,rr,rr,0.0,0.0/)
     else
        sgau(1) = MIN(sgau(1), rr)
        sgau(2) = MAX(sgau(2), rr)
        sgau(3) = sgau(4)+(rr-sgau(4))/i
        sgau(5) = sgau(6)+(rr-sgau(4))*(rr-sgau(3))

        sgau(4) = sgau(3)
        sgau(6) = sgau(5)
     endif

     !Chi-Squared
     rr = chisquare(50)
     if (i == 1) then
        schi = (/rr,rr,rr,rr,0.0,0.0/)
     else
        schi(1) = MIN(schi(1), rr)
        schi(2) = MAX(schi(2), rr)
        schi(3) = schi(4)+(rr-schi(4))/i
        schi(5) = schi(6)+(rr-schi(4))*(rr-schi(3))

        schi(4) = schi(3)
        schi(6) = schi(5)
     endif
  end do

  ! Write out Stats
  100 FORMAT(A15,A15,A15,A15,A15)
  200 FORMAT(F15.4,F15.4,F15.4,F15.4,F15.4)

  WRITE(*,*)
  WRITE(*,*)"/======================================\"
  WRITE(*,*)"| Bernoulli(P), 0 <= P <= 1            |"
  WRITE(*,*)"|  Mean     = P                        |"
  WRITE(*,*)"|  Variance = (1-P)P                   |"
  WRITE(*,*)"\--------------------------------------/"
  WRITE(*,*)"Tested: Bernoulli(0.25)"
  WRITE(*,*)"  Expected:"
  WRITE(*,*)"    Mean     = 0.25"
  WRITE(*,*)"    Variance = 0.1875"
  WRITE(*,*)"    StdDev   = 0.4330"
  WRITE(*,*)"  Actual:"
  WRITE(*,100)"MIN","MAX","MEAN","VAR","ST.DEV"
  WRITE(*,200)sber(1),sber(2),sber(3),sber(5)/(iters-1),sqrt(sber(5)/(iters-1))

  WRITE(*,*)
  WRITE(*,*)"/======================================\"
  WRITE(*,*)"| Binomial(n,P), 0 <= P <= 1           |"
  WRITE(*,*)"|  Mean     = nP                       |"
  WRITE(*,*)"|  Variance = n(1-P)P                  |"
  WRITE(*,*)"\--------------------------------------/"
  WRITE(*,*)"Tested: Binomial(100,0.5)"
  WRITE(*,*)"  Expected:"
  WRITE(*,*)"    Mean     = 50"
  WRITE(*,*)"    Variance = 25"
  WRITE(*,*)"    StdDev   = 5"
  WRITE(*,*)"  Actual:"
  WRITE(*,100)"MIN","MAX","MEAN","VAR","ST.DEV"
  WRITE(*,200)sbin(1),sbin(2),sbin(3),sbin(5)/(iters-1),sqrt(sbin(5)/(iters-1))

  WRITE(*,*)
  WRITE(*,*)"/======================================\"
  WRITE(*,*)"| Geometric(P), 0 <= P <= 1            |"
  WRITE(*,*)"|  Mean     = 1/P - 1                  |"
  WRITE(*,*)"|  Variance = (1-P)/P^2                |"
  WRITE(*,*)"\--------------------------------------/"
  WRITE(*,*)"Tested: Geometric(0.33)"
  WRITE(*,*)"  Expected:"
  WRITE(*,*)"    Mean     = 2.0303"
  WRITE(*,*)"    Variance = 6.1524"
  WRITE(*,*)"    StdDev   = 2.4804"
  WRITE(*,*)"  Actual:"
  WRITE(*,100)"MIN","MAX","MEAN","VAR","ST.DEV"
  WRITE(*,200)sgeo(1),sgeo(2),sgeo(3),sgeo(5)/(iters-1),sqrt(sgeo(5)/(iters-1))

  WRITE(*,*)
  WRITE(*,*)"/======================================\"
  WRITE(*,*)"| Poisson(mu), 0 < mu                  |"
  WRITE(*,*)"|  Mean     = mu                       |"
  WRITE(*,*)"|  Variance = mu                       |"
  WRITE(*,*)"\--------------------------------------/"
  WRITE(*,*)"Tested: Poisson(50.0)"
  WRITE(*,*)"  Expected:"
  WRITE(*,*)"    Mean     = 50.0"
  WRITE(*,*)"    Variance = 50.0"
  WRITE(*,*)"    StdDev   = 7.0711"
  WRITE(*,*)"  Actual:"
  WRITE(*,100)"MIN","MAX","MEAN","VAR","ST.DEV"
  WRITE(*,200)spoi(1),spoi(2),spoi(3),spoi(5)/(iters-1),sqrt(spoi(5)/(iters-1))

  WRITE(*,*)
  WRITE(*,*)"/======================================\"
  WRITE(*,*)"| Exponential(lambda), 0 < lambda      |"
  WRITE(*,*)"|  Mean     = 1/lambda                 |"
  WRITE(*,*)"|  StdDev   = 1/lambda                 |"
  WRITE(*,*)"\--------------------------------------/"
  WRITE(*,*)"Tested: Exponential(0.1)"
  WRITE(*,*)"  Expected:"
  WRITE(*,*)"    Mean     = 10.0"
  WRITE(*,*)"    Variance = 100.0"
  WRITE(*,*)"    StdDev   = 10.0"
  WRITE(*,*)"  Actual:"
  WRITE(*,100)"MIN","MAX","MEAN","VAR","ST.DEV"
  WRITE(*,200)sexp(1),sexp(2),sexp(3),sexp(5)/(iters-1),sqrt(sexp(5)/(iters-1))

  WRITE(*,*)
  WRITE(*,*)"/======================================\"
  WRITE(*,*)"| Gaussian(var), 0 < var               |"
  WRITE(*,*)"|  Mean     = 0                        |"
  WRITE(*,*)"|  Variance = var                      |"
  WRITE(*,*)"\--------------------------------------/"
  WRITE(*,*)"Tested: Gaussian(1.0)"
  WRITE(*,*)"  Expected:"
  WRITE(*,*)"    Mean     = 0.0"
  WRITE(*,*)"    Variance = 1.0"
  WRITE(*,*)"    StdDev   = 1.0"
  WRITE(*,*)"  Actual:"
  WRITE(*,100)"MIN","MAX","MEAN","VAR","ST.DEV"
  WRITE(*,200)sgau(1),sgau(2),sgau(3),sgau(5)/(iters-1),sqrt(sgau(5)/(iters-1))

  WRITE(*,*)
  WRITE(*,*)"/======================================\"
  WRITE(*,*)"| Chi-Squared(k), 0 < k                |"
  WRITE(*,*)"|  Mean     = k                        |"
  WRITE(*,*)"|  Variance = 2k                       |"
  WRITE(*,*)"\--------------------------------------/"
  WRITE(*,*)"Tested: Chi-Squared(5.0)"
  WRITE(*,*)"  Expected:"
  WRITE(*,*)"    Mean     = 50.0"
  WRITE(*,*)"    Variance = 100.0"
  WRITE(*,*)"    StdDev   = 10.0"
  WRITE(*,*)"  Actual:"
  WRITE(*,100)"MIN","MAX","MEAN","VAR","ST.DEV"
  WRITE(*,200)schi(1),schi(2),schi(3),schi(5)/(iters-1),sqrt(schi(5)/(iters-1))

end program stats
