program evaleps
    
    use thermev2_subs

    implicit none
    
    integer :: l,kmax

    real (kind=dp) :: x,y
    real (kind=dp), parameter :: xin = 0.0001d0
    real (kind=dp), parameter :: dx = 0.01d0

    call leer_oblicuidad(kmax)

    print *,'kmax = ',kmax

    open(unit=11,file='../out/evaleps.out')

    x = xin
    l = 3

    do while (x.le.tf)
        y = oblicuidad(x,kmax,l)
        write(11,*) x,y
        x = x + dx
    end do

!100 format('El valor t =',1x,f4.2,1x,'Ga se encuentra entre los índices',1x,i2,1x,'y',1x,i2,'.')
!    print *,' '
!    print 100,x,j,j+1

end program evaleps