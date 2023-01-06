!==============================================================================
program prueba_resolvente
!   ---------------------------------------------------------------------------
    implicit none
!   ---------------------------------------------------------------------------
    integer, parameter :: dp = 8
    real (kind=dp) :: tcmb,ric
    real (kind=dp), parameter ::   rc = 3.480d6
    real (kind=dp), parameter ::  glm = 10.d0         ! lower mantle gravity
    real (kind=dp), parameter :: rhoc = 1.3d4         ! core density
    real (kind=dp), parameter ::  pec = 363.85d0      ! pressure at the earth's centre (in gpa)
    real (kind=dp), parameter :: pcmb = 135.75d0      ! pressure at the cmb (in gpa)
!   ---------------------------------------------------------------------------
    interface
        subroutine resolvente(a,b,c,r1,r2)
        !----------------------------------------------------------------------
            implicit none
        !----------------------------------------------------------------------
            integer, parameter :: d = 8
            real (kind=d), intent(in) :: a,b,c
            real (kind=d), intent(out) :: r1,r2
            real (kind=d) :: q
        !----------------------------------------------------------------------
        end subroutine resolvente
!       -----------------------------------------------------------------------
        function pio(tcmb)
        !----------------------------------------------------------------------
            implicit none
        !   -------------------------------------------------------------------
            integer, parameter :: dp = 8
            real (kind=dp), intent(in) :: tcmb
            real (kind=dp) :: xi,c1,c2,c3,x1,x2,pio
            real (kind=dp), parameter ::  pec = 363.85d0      ! pressure at the earth's centre (in gpa)
            real (kind=dp), parameter :: pcmb = 135.75d0      ! pressure at the cmb (in gpa)
            real (kind=dp), parameter :: tlc0 = 1980.d0, tlc1 = 6.14d-3, tlc2 = -4.5d-6
            real (kind=dp), parameter ::  ta1 = 3.96d-3, ta2 = -3.3d-6
            real (kind=dp), parameter ::   x0 = 0.105d0
        !   -------------------------------------------------------------------
        end function pio
    end interface
!   ---------------------------------------------------------------------------
10  format(a6,1x,f8.2)
!   ---------------------------------------------------------------------------
    print *,'Introducir el valor de Tcmb'
    read *,tcmb
!   ---------------------------------------------------------------------------
    ric = dsqrt(2.d0*(pec-pio(tcmb))*1.d9*rc/(rhoc*glm))
!   ---------------------------------------------------------------------------
    print *,' '
    print 10,'Tcmb =',tcmb
    print 10,' Pio =',pio(tcmb)
    print 10,' Ric =',ric/1000.d0
    print *,' '
!   ---------------------------------------------------------------------------
end program prueba_resolvente
!==============================================================================
    subroutine resolvente(a,b,c,x1,x2)
!------------------------------------------------------------------------------
        implicit none
!------------------------------------------------------------------------------
        integer, parameter :: dp = 8
        real (kind=dp), intent(in) :: a,b,c
        real (kind=dp), intent(out) :: x1,x2
        real (kind=dp) :: q
!------------------------------------------------------------------------------
        q = -0.5d0*(b+dsign(1.d0,b)*dsqrt(b*b-4.d0*a*c))
        x1 = q/a
        x2 = c/q
!------------------------------------------------------------------------------
    end subroutine resolvente
!==============================================================================
    function pio(tcmb)
!   ---------------------------------------------------------------------------
        implicit none
!       -----------------------------------------------------------------------
        integer, parameter :: dp = 8
        real (kind=dp), intent(in) :: tcmb
        real (kind=dp) :: xi,c1,c2,c3,x1,x2,pio
        real (kind=dp), parameter ::  pec = 363.85d0      ! pressure at the earth's centre (in gpa)
        real (kind=dp), parameter :: pcmb = 135.75d0      ! pressure at the cmb (in gpa)
        real (kind=dp), parameter :: tlc0 = 1980.d0, tlc1 = 6.14d-3, tlc2 = -4.5d-6
        real (kind=dp), parameter ::  ta1 = 3.96d-3, ta2 = -3.3d-6
        real (kind=dp), parameter ::   x0 = 0.1d0
!       -----------------------------------------------------------------------
        xi = tlc0*(1.d0 - 2.d0*x0)*(1.d0 + (ta1 + ta2*pcmb)*pcmb)
        c1 = tcmb*ta2 - xi*tlc2
        c2 = tcmb*ta1 - xi*tlc1
        c3 = tcmb - xi
        call resolvente(c1,c2,c3,x1,x2)
!       -----------------------------------------------------------------------
        if ((x1-pcmb)*(pec-x1).gt.0.d0) then
            pio = x1
        else if ((x2-pcmb)*(pec-x2).gt.0.d0) then
            pio = x2
        else 
            pio = pec
        end if
!       -----------------------------------------------------------------------
    end function pio
!==============================================================================