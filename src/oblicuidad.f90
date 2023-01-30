program prueba_oblicuidad
    
    use thermev2_subs

    implicit none

    real (kind=8) :: t,a1,a2,eps,da,p,lod1,lod2,thp,n,epsdeg
    real (kind=8) :: h1,h2,h3,h4,da2,da6
    real (kind=8), parameter :: ti = 0.d0
    real (kind=8), parameter :: dt = 0.1d0
    real (kind=8), parameter :: tol = 1.d-6
    real (kind=8), parameter :: ae = 1.495978707e8
    real (kind=8), parameter :: GMs = 2.959122082853813556248d-4*ae**3/dd**2
    real (kind=8), parameter :: GMl = 4902.800d9
    real (kind=8), parameter :: Ed = 0.003243d0
    real (kind=8), parameter :: dt2 = 0.5d0*dt
    integer, parameter :: demid = 1
    
    open(unit=10,file='../out/oblicuidad.out')

10  format(f4.2,1x,f7.4,1x,f7.4,1x,f15.6,1x,f10.8,1x,f10.8)

    ldem1 = demid.eq.1

    t = ti
    eps = eps0
    epsdeg = eps*180.d0/pi
    p = p0

    do while (t.le.tf+0.5d0)
        call modelo_dinamico(t,a1,n,lod1,thp)
        call modelo_dinamico(t+dt,a2,n,lod2,thp)
        da = a2 - a1
        da2 = 0.5d0*da
        da6 = da/6.d0
!                    1  2    3              4                5       6
        write(10,10) t,eps,epsdeg,p*(180.d0/pi)*3600.d0*aa,a1/a0,lod1/LOD0
!       -----------------------------------------------------------------------
!       Integración de la ec. (A.6) de Farhat et al. (2020) usando el método
!       de Runge-Kutta de orden 4.
!       -----------------------------------------------------------------------
        h1 = depsda(t,eps)
        h2 = depsda(t+dt2,eps+h1*da2)
        h3 = depsda(t+dt2,eps+h2*da2)
        h4 = depsda(t+dt,eps+h3*da)
        eps = eps + da6*(h1 + 2.d0*(h2 + h3) + h4)
!       -----------------------------------------------------------------------
        epsdeg = eps*180.d0/pi
        t = t + dt
        p = 1.5d0*(GMs/(ae**3) + GMl/(a2**3))*Ed*thp*dcos(eps)/(thp0**2)
    end do
    
end program prueba_oblicuidad