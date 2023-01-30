program evaleps
    
    use thermev2_subs

    implicit none

    real (kind=dp) :: x,y
    real (kind=dp), parameter :: xin = 0.0001d0
    real (kind=dp), parameter :: dx = 0.01d0

    call leer_oblicuidad()

    print *,'nmax = ',nmax

    open(unit=11,file='../out/evaleps.out')

    x = xin

    do while (x.le.tf)
        y = oblicuidad(x)
        write(11,*) x,y
        x = x + dx
    end do

end program evaleps