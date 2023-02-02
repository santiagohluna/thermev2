!=======================================================================
module thermev2_subs
!   --------------------------------------------------------------------
    use, intrinsic:: iso_fortran_env, only: stdin=>input_unit
!   --------------------------------------------------------------------
!   bloque de declaración
!   --------------------------------------------------------------------
    implicit none
!   --------------------------------------------------------------------
    integer, parameter :: dp = 8
!   --------------------------------------------------------------------
!   parametros generales
!   --------------------------------------------------------------------
    real (kind=dp), parameter ::   pi = 4.d0*datan(1.d0)
    real (kind=dp), parameter ::  CGU = 6.67408d-11
    real (kind=dp), parameter ::   Me = 5.9722d24
    real (kind=dp), parameter ::   Rt = 6.3781366d6
    real (kind=dp), parameter ::   Rc = 3.480d6
    real (kind=dp), parameter ::   Ml = 7.342d22
    real (kind=dp), parameter ::   mu = CGU*(Me+Ml)
    real (kind=dp), parameter :: mred = Me*Ml/(Me+Ml)
    real (kind=dp), parameter :: rhot = 5515.d0
    real (kind=dp), parameter ::   dd = 86400.d0
    real (kind=dp), parameter ::   aa = 365.25d0*dd
    real (kind=dp), parameter ::   ga = aa*1.d9
    real (kind=dp), parameter ::   t0 = 4.5d0
    real (kind=dp), parameter ::   tf = 4.5d0
    real (kind=dp), parameter ::   a0 = 60.142611d0*Rt
    real (kind=dp), parameter :: LOD0 = 23.934468d0
    real (kind=dp), parameter :: eps0 = 23.2545d0*pi/180.d0
    real (kind=dp), parameter ::   p0 = 50.467718d0*pi/(180.d0*3600.d0*aa)
!   --------------------------------------------------------------------
!   parametros del modelo de driscoll & bercovici (2014)
!   tabla 3
!   --------------------------------------------------------------------
    real (kind=dp), parameter ::     at = 4.d0*pi*Rt**2 ! earth surface area
    real (kind=dp), parameter ::     ac = 4.d0*pi*Rc**2 ! core surface area
    real (kind=dp), parameter ::  alfam = 2.d-5         ! thermal expansivity of mantle
    real (kind=dp), parameter ::  alfac = 1.d-5         ! thermal expansivity of core
    real (kind=dp), parameter ::   beta = 0.3d0         ! thermal boundary layer exponent
    real (kind=dp), parameter ::     cm = 1100.d0       ! specific heat of mantle
    real (kind=dp), parameter ::     cc = 800.d0        ! specific heat of core
    real (kind=dp), parameter :: denVm = (Rt**3 - Rc**3)
    real (kind=dp), parameter ::     dm = 2891.d3       ! mantle depth
    real (kind=dp), parameter ::    dfe = 7000.d3       ! iron solidus length scale
    real (kind=dp), parameter ::     dn = 6340.d3       ! core adiabatic length scale
    real (kind=dp), parameter :: dvldtm = 8.d17         ! liquid volume gradient
    real (kind=dp), parameter ::     eg = 3.d5          ! gravitational energy density release at icb
    real (kind=dp), parameter ::   epsc = 1.2d0         ! ratio of the average core temperature to the 
                                                        ! temperature at the cmb
    real (kind=dp), parameter ::   epsm = 1.3d0         ! ratio of the average mantle temperature to the 
                                                        ! temperature at the base of the upper thermal 
                                                        ! boundary layer 
    real (kind=dp), parameter ::  erupt = 0.2d0         ! efficiency of magma eruption to the surface
    real (kind=dp), parameter ::  etaum = 0.7d0         ! upper mantle adiabatic temperature drop
    real (kind=dp), parameter ::  etalm = 1.3d0         ! lower mantle adiabatic temperature jump
    real (kind=dp), parameter ::   etac = 0.8d0         ! upper core adiabatic temperature drop
    real (kind=dp), parameter ::  fvol0 = 1.d-3         ! reference volumetric melt fraction
    real (kind=dp), parameter ::    gum = 9.8d0         ! upper mantle gravity
    real (kind=dp), parameter ::    glm = 10.5d0        ! lower mantle gravity
    real (kind=dp), parameter ::     gc = 10.5d0        ! cmb gravity
    real (kind=dp), parameter :: gammad = 1.d-3         ! magma adiabatic gradient
    real (kind=dp), parameter ::   grun = 1.3d0         ! core gruneisen parameter
    real (kind=dp), parameter ::  gammz = 3.9d-3        ! mantle solidus gradient
    real (kind=dp), parameter ::    kum = 4.d0          ! upper mantle thermal conductivity
    real (kind=dp), parameter ::    klm = 4.d0          ! lower mantle thermal conductivity
    real (kind=dp), parameter ::   kapm = 1.d-6         ! mantle thermal diffusivity
    real (kind=dp), parameter ::    lfe = 750.d3        ! latent heat of inner core crystallization
    real (kind=dp), parameter ::  Lmelt = 6.d5          ! latent heat of mantle melting
    real (kind=dp), parameter ::     mm = 4.06d24       ! mantle mass
    real (kind=dp), parameter ::     mc = 1.95d24       ! core mass
    real (kind=dp), parameter ::    pec = 363.85d0      ! pressure at the earth's centre (in gpa)
    real (kind=dp), parameter ::   pcmb = 135.75d0      ! pressure at the cmb (in gpa)
    real (kind=dp), parameter ::  qrad0 = 13.d12        ! present-day mantle radiogenic heat flow
    real (kind=dp), parameter :: qrad0c = 2.d12         ! present-day core radiogenic heat flow
    real (kind=dp), parameter ::     rm = 4.925d6       ! radius to average mantle temperature
    real (kind=dp), parameter ::   Racr = 500.d0        ! critical rayleigh number
    real (kind=dp), parameter ::   rhoc = 11900.d0      ! core density
    real (kind=dp), parameter ::  rhoic = 13000.d0      ! inner core density
    real (kind=dp), parameter ::   rhom = 4500.d0       ! mantle density
    real (kind=dp), parameter :: rhomel = 2700.d0       ! mantle melt density
    real (kind=dp), parameter :: rhosol = 3300.d0       ! mantle upwelling solid density
    real (kind=dp), parameter ::   tfe0 = 5600.d0       ! iron solidus coefficient
    real (kind=dp), parameter ::  tsol0 = 1244.d0       ! mantle solidus at surface
    real (kind=dp), parameter :: taurad = 2.94d0        ! mantle radioactive decay time scale
    real (kind=dp), parameter :: taurdc = 1.2d0         ! core radioactive decay time scale
    real (kind=dp), parameter :: phidis = 0.6d0         ! disgregation point
    real (kind=dp), parameter :: denric = 2.d0*(1.d0 - 1.d0/(3.d0*grun))*(dn/dfe)**2 - 1.d0
    real (kind=dp), parameter :: tlc0 = 1980.d0
    real (kind=dp), parameter :: tlc1 = 6.14d-3
    real (kind=dp), parameter :: tlc2 = -4.5d-6
    real (kind=dp), parameter :: ta1 = 3.96d-3
    real (kind=dp), parameter :: ta2 = -3.3d-6
    real (kind=dp), parameter :: u0 = 2.d-12            ! Convection velocity scalex
    real (kind=dp), parameter :: x0 = 0.1d0             ! initial concetration of light constituent in the core
!   --------------------------------------------------------------------
!   parametros de los modelos dinamicos
!   --------------------------------------------------------------------
!   demid = 1
!   ---------
    real (kind=dp), parameter, dimension(4) :: ca = (/ -0.0130526d0, 0.0565061d0, -0.116111d0, 1.d0 /)
    real (kind=dp), parameter, dimension(4) :: clod = (/ -0.0203081d0, 0.103267d0, -0.23361d0, 1.d0 /)
!   --------------------------------------------------------------------
!   demid = 2
!   ---------
    real (kind=dp), parameter :: dadt0 = 3.82d-2*ga/aa  ! Bills & Ray (1999)
    real (kind=dp), parameter :: dLODdt0 = 6.653d0      ! Williams & Boggs (2016)
!   --------------------------------------------------------------------
!   demid = 3
!   ---------
    real (kind=dp) :: ai,af,LODi,LODf,ma,mLOD
!   --------------------------------------------------------------------
!   Valor actual de la velocidad angular de rotación terrestre
!   --------------------------------------------------------------------
    real (kind=dp), parameter :: thp0 = 2.d0*pi/(LOD0*3600.d0)
!   --------------------------------------------------------------------
!   parametros de elementos radiogenicos
!   tabla 4.2 de turcotte & schubert (2014)
!   --------------------------------------------------------------------
    real (kind=dp), parameter, dimension(4) :: hi = (/ 9.46d-5, 5.69d-4, 2.64d-5, 2.92d-5 /)
    real (kind=dp), parameter, dimension(4) :: tau = (/ 4.47d9*aa/ga, 7.04d8*aa/ga, 1.40d10*aa/ga, 1.25d9*aa/ga /)
    real (kind=dp), parameter, dimension(4) :: lam = (/ dlog(2.d0)/tau(1), dlog(2.d0)/tau(2), & 
                                                        log(2.d0)/tau(3), dlog(2.d0)/tau(4) /)
    real (kind=dp), parameter, dimension(4) :: c0 = (/ 31.d-9, 31.d-9, 124.d-9, 31.d-5 /)
    real (kind=dp), parameter, dimension(4) :: fc = (/ 0.9928d0, 0.0071d0, 1.d0, 1.19d-4 /)
!   --------------------------------------------------------------------
!   parametros reologicos
!   --------------------------------------------------------------------
    real (kind=dp), parameter ::  rigdz = 8.0e10
    real (kind=dp), parameter ::   flex = 1.d0/rigdz
    real (kind=dp), parameter ::  ksubb = Rt/(CGU*Me*rhot)
    real (kind=dp), parameter ::  kflex = 0.2d0
    real (kind=dp), parameter ::   keta = 0.02d0
    real (kind=dp), parameter ::  alpha = 0.2
    real (kind=dp), parameter :: zandr0 = 1.d0
    real (kind=dp) :: gamalf,gammac,gammas
    real (kind=dp), parameter ::     k2 = 1.d0
    real (kind=dp), parameter ::     qf = 1.d0
    real (kind=dp), parameter :: deltat = 0.d0
    real (kind=dp), parameter :: epsmay = 0.d0
    logical, dimension(8) :: lreo
!   --------------------------------------------------------------------
!   Parámetros de la viscosidad
!   --------------------------------------------------------------------
    real (kind=dp), parameter ::   Vact = 4.d-6     ! Activation volume
    real (kind=dp), parameter ::  visc0 = 6.127d10  ! Viscosity prefactor (Tosi et al. 2017)
    real (kind=dp), parameter ::   rgas = 8.31447d0 ! gas constant
    real (kind=dp), parameter ::   eact = 3.35d5    ! viscosity activation energy
    real (kind=dp), parameter :: etaref = 5.d20     ! reference viscosity (stamenkovic et al, 2012)
    real (kind=dp), parameter ::   tref = 1600.d0   ! reference temperature
!   --------------------------------------------------------------------
!   Switch globales
!   --------------------------------------------------------------------
    logical ldem1,ldem2,ldem3,ltide,lradc,lradm,ltherm,lcore,lRic
!   --------------------------------------------------------------------
    integer, parameter :: lmaxp = 3
    integer, parameter :: qmaxp = 10
    integer, parameter :: mpol = 3
    integer :: nmax
    real (kind=dp) :: ffi(lmaxp,0:lmaxp,0:lmaxp), &
                      gge(lmaxp,0:lmaxp,-qmaxp:qmaxp),alm(lmaxp,0:lmaxp)
!   --------------------------------------------------------------------
    character (len=50) algo
!   --------------------------------------------------------------------
    real (kind=dp) :: Tsup,e,i
    integer :: lmax,qmax,thermfl, &
               corefl,nterms
!   --------------------------------------------------------------------
    real (kind=dp), allocatable :: asuma(:)
    real (kind=dp), dimension(1:60) :: xa,ya
    real(kind=dp), dimension(11) :: printout
!   --------------------------------------------------------------------
!   bloque de procedimientos
!   --------------------------------------------------------------------

    contains

!=======================================================================
    subroutine dTdt(t,y,dydt)
!   --------------------------------------------------------------------
    implicit none
!   --------------------------------------------------------------------
    real (kind=dp),intent(in) :: t,y(2)
    real (kind=dp),intent(out) :: dydt(2)
    real (kind=dp) :: dtubl,Tubl,ric,dricdt,dlbl,dubl,aic, &
                      dtlbl,dtmelt,mmeltp,qcmb,qconv,qmelt,qradm, &
                      qradc,Tcmb,Tlbl,dvupdt,qtidal, &
                      ur,urtot,radic,num,etavg, &
                      tmelt,zmelt,zum,dpiodtc,ftvf,Pio,St
    !real(kind=dp) :: delt,a1,a2,int
!    real (kind=dp) :: asumaq(4)
!   --------------------------------------------------------------------
    Tcmb = y(1)
    Tubl = y(2)
!   --------------------------------------------------------------------
!   Cálculo de los saltos de temperatura en y espesores de las capas
!   límite inferior y superior
!   --------------------------------------------------------------------
    call capas_limites(Tcmb,Tubl,Tlbl,Dtubl,DTlbl,dubl,dlbl)
!   --------------------------------------------------------------------
!   calculo de qcmb
!   --------------------------------------------------------------------
    Qcmb = Ac*klm*DTlbl/dlbl
!   --------------------------------------------------------------------
!   calculo de qconv
!   --------------------------------------------------------------------
    Qconv = At*kum*DTubl/dubl
!   --------------------------------------------------------------------
!   calculo de qmelt
!   --------------------------------------------------------------------
    dvupdt = 1.16d0*kapm*at/dubl
    tmelt = 0.5d0*(Tubl + tsol0)
    zmelt = 0.5d0*(Tubl-tsol0)/gammz
    dtmelt = tmelt - Tsup - zmelt*gammad
    zum = Rt - dubl
    mmeltp = dvupdt*rhosol*fmeltm(Tubl,dubl,zum)
    Qmelt = erupt*mmeltp*(Lmelt + cm*dtmelt)
!   --------------------------------------------------------------------
!   calculo de qradm
!   --------------------------------------------------------------------
    if (lradm) then
        !do k=1,4
        !    asumaq(k) = mm*fc(k)*c0(k)*hi(k)*dexp(lam(k)*(t0-t))
        !end do
        !qradm = sumar(4,asumaq)
        Qradm = Qrad0*dexp((t0-t)/taurad)
    else
        Qradm = 0.d0
    end if
!   --------------------------------------------------------------------
!   calculo de qradc
!   --------------------------------------------------------------------
    if (lradc) then
        Qradc = Qrad0c*dexp((t0-t)/taurdc)
    else
        Qradc = 0.d0
    end if
!   --------------------------------------------------------------------
!   Cálculo de la fraccion de volumen del manto activo para la 
!   interaccion de mareas
!   --------------------------------------------------------------------
    ftvf = (Rphim(Tubl,dubl)/Rt)**3-(Rc/Rt)**3
!   --------------------------------------------------------------------
!   calculo de qtidal
!   --------------------------------------------------------------------
    if (ltide) then
        ! a1 = Tlbl
        ! a2 = Tubl
        ! delt = Tubl - Tlbl
        ! call qromb(visc,a1,a2,int)
        ! etavg = int/delt
        etavg = 0.5*(visc(Tubl) + visc(Tlbl))
        Qtidal = ftvf*pm(t0-t,etavg)
    else
        Qtidal = 0.d0
    end if
!   --------------------------------------------------------------------
!   calculo del número de Stefan
!   --------------------------------------------------------------------
    St = stefan(Tubl,dubl)
!   --------------------------------------------------------------------
!   calculo de las razones de urey
!   --------------------------------------------------------------------
    ur = qradm/qconv
    urtot = (qradm+qtidal)/(qconv+qmelt)
!   --------------------------------------------------------------------
    if (lcore) then
        lRic = fPio(Pcmb,Tcmb,x0)*fPio(Pec,Tcmb,x0).lt.0.d0
        if(lRic) then 
            Pio = rtbis(fPio,Tcmb,x0,Pcmb,Pec,1.d-6)
            dPiodTc = (1.d0 + (ta1 + ta2*Pio)*Pio)/(xi(x0)*(tlc1 + 2.d0*tlc2) - &
                       Tcmb*(ta1 + 2.d0*ta2*Pio))
            Ric = dsqrt(2.d0*(Pec-Pio)*1.d9*Rc/(rhoc*glm))
            dydt(1) = (Qradc - Qcmb)*Ga/(Mc*Cc*epsc + Ac*Ric*dPiodTc*(Lfe+Eg)/(glm*Rc))
        else
            Ric = 0.d0
            dydt(1) = (Qradc - Qcmb)*Ga/(Mc*Cc*epsc)
        end if
    else
    !   --------------------------------------------------------------------
    !   calculo del radio del nucleo interno
    !   --------------------------------------------------------------------
        num = dlog(tfe0/Tcmb)*(dn/Rc)**2 - 1.d0
        if (num.ge.0.d0) then
            radic = num/denric
            ric = Rc*dsqrt(radic)
        else 
            ric = 0.d0
        end if
    !   --------------------------------------------------------------------
    !   calculo del area del nucleo interno
    !   --------------------------------------------------------------------
        aic = 4.d0*pi*ric**2
    !   --------------------------------------------------------------------
    !   calculo de dr_ic / dt_cmb
    !   --------------------------------------------------------------------
        dricdt = - (dn**2)/(2.d0*Rc*Tcmb*num)
    !   --------------------------------------------------------------------
        dydt(1) = (qradc - qcmb)*ga/(mc*cc - aic*rhoic*etac*dricdt*(lfe+eg))
    !   --------------------------------------------------------------------
    end if

    if (ltherm) then
        dydt(2) = (qcmb + qradm - qconv - qmelt)*ga/(epsm*mm*cm)
    else
        dydt(2) = (qcmb + qradm + qtidal - qconv - qmelt)*ga/ &
                  (mm*cm*(epsm + St))
    end if
!   --------------------------------------------------------------------
    printout(1) = ur 
    printout(2) = urtot
    printout(3) = Qcmb
    printout(4) = Qconv
    printout(5) = Qmelt
    printout(6) = Qradm
    printout(7) = Qradc
    printout(8) = Qtidal
    printout(9) = St
    printout(10) = Ric
!   --------------------------------------------------------------------
    end subroutine dTdt
!=======================================================================
    subroutine conveccion(Tcmb,Tubl,Tlbl,DTubl,DTlbl,dubl,dlbl)
!   --------------------------------------------------------------------
        implicit none 
!       ----------------------------------------------------------------
        real(kind=dp), intent(in) :: Tcmb, Tubl, Tlbl
        real(kind=dp), intent(out) :: DTubl, DTlbl, dubl, dlbl
        real(kind=dp) :: vlbl, vubl, Ra, Publ
!        real(kind=dp) :: Racrcmb
        real (kind=dp), parameter :: fpd = 1.d0
        real (kind=dp), parameter :: Racrcmb = 500.d0      ! critical rayleigh number at the cmb
!       ----------------------------------------------------------------
!       Cálculo de los saltos de temperatura
!       ----------------------------------------------------------------
        DTlbl = Tcmb - Tlbl
        DTubl = Tubl - Tsup
!       ----------------------------------------------------------------
!       Cálculo del espesor de la capa límite superior
!       ----------------------------------------------------------------
        Publ = pdr(Rt-dubl)*1.d9
        vubl = visc(Tubl)
        Ra = rhom*gum*alfam*(dTubl + dTlbl)*(Rt - Rc)**3/(kapm*vubl)
        dubl = (Rt - Rc)*(Racr/Ra)**beta
!       ----------------------------------------------------------------
!       Cálculo del espesor de la capa límite inferior
!       ----------------------------------------------------------------
!        Racrcmb = 0.28d0*Ra**0.21d0
        vlbl = fpd*visc(0.5d0*(Tlbl + Tcmb))
        dlbl = ((Racrcmb*kapm*vlbl)/(rhom*glm*alfam*DTlbl))**(1.d0/3.d0)
!       ----------------------------------------------------------------
        printout(11) = Ra
!   --------------------------------------------------------------------        
    end subroutine conveccion
!=======================================================================
    subroutine capas_limites(Tcmb,Tubl,Tlbl,DTubl,DTlbl,dubl,dlbl)
!   --------------------------------------------------------------------  
        implicit none
!       ----------------------------------------------------------------
        integer :: k
        integer, parameter :: kmax = 10
        real(kind=dp), intent(in) :: Tcmb,Tubl
        real(kind=dp), intent(out) :: Tlbl,DTubl,DTlbl,dubl,dlbl
        real(kind=dp) :: Tlbl0
!       ----------------------------------------------------------------    
        Tlbl = (2.d0*epsm - 1.d0)*Tubl
!       ----------------------------------------------------------------
        k = 1
!       ----------------------------------------------------------------    
        do
!       ----------------------------------------------------------------    
            Tlbl0 = Tlbl
!           ------------------------------------------------------------
            call conveccion(Tcmb,Tubl,Tlbl,DTubl,DTlbl,dubl,dlbl)
!           ------------------------------------------------------------    
            Tlbl = tm(Tubl,dubl,Rc+dlbl)
!           ------------------------------------------------------------    
            if((dabs(Tlbl-Tlbl0).lt.1.d-4).or.(k.eq.kmax)) exit
!           ------------------------------------------------------------    
            k = k + 1
!       ----------------------------------------------------------------    
        end do
!       ----------------------------------------------------------------
        call conveccion(Tcmb,Tubl,Tlbl,DTubl,DTlbl,dubl,dlbl)
!   --------------------------------------------------------------------          
    end subroutine capas_limites
!=======================================================================
    function pm(t,eta)
!   --------------------------------------------------------------------
!   esta función calcula el calor generado por interacción de mareas
!   usando la expresión derivada por efroimsky y makarov (2014)
!   --------------------------------------------------------------------
    implicit none
!   --------------------------------------------------------------------
    integer :: j,l,m,p,q
    real (kind=dp),intent(in) :: t,eta
    real (kind=dp) :: rsa,wlmpq,xlmpq,sumapm,rsal
    real (kind=dp) :: pm,kr,ki,a,n,lod,thp
    real (kind=dp), parameter :: dr = 1.d2
!   --------------------------------------------------------------------
    call modelo_dinamico(t,a,n,lod,thp)
    rsa = Rt/a
!   --------------------------------------------------------------------
!   Evaluación de las funciones de la inclinación
!   --------------------------------------------------------------------
    i = oblicuidad(t)
    call evalfi(i,ffi)
!   --------------------------------------------------------------------
!   calculo de la tasa de produccion de calor por mareas
!   --------------------------------------------------------------------
    j = 1
    do l=2,lmax
        rsal = rsa**(2*l+1)
        do m = 0,l
            do p = 0,l
                do q = -qmax,qmax
                    wlmpq = dble(l-2*p+q)*n - dble(m)*thp
                    xlmpq = dabs(wlmpq)
                    call reologia(l,wlmpq,eta,kr,ki)
                    asuma(j) = rsal*alm(l,m)*ffi(l,m,p)*gge(l,p,q)*wlmpq*ki*dsign(1.d0,wlmpq)
                    j = j + 1
                end do
            end do
        end do
    end do
    sumapm = sumar(j-1,asuma)
    pm = ((CGU*Ml)*(Ml/a))*sumapm
!   --------------------------------------------------------------------
    end function pm
!=======================================================================
    function afit(t)
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------
        real (kind=dp), intent(in) :: t
        real (kind=dp) :: afit
!       ----------------------------------------------------------------
        afit = (t*(t*(ca(1)*t + ca(2)) + ca(3)) + ca(4))*a0
!   --------------------------------------------------------------------
    end function afit
!=======================================================================
    function LODfit(t)
!   --------------------------------------------------------------------        
        implicit none
!       ----------------------------------------------------------------
        real (kind=dp), intent(in) :: t
        real (kind=dp) :: LODfit
!       ----------------------------------------------------------------
        LODfit = (t*(t*(clod(1)*t + clod(2)) + clod(3)) + clod(4))*LOD0
!   --------------------------------------------------------------------    
    end function LODfit
!=======================================================================
    subroutine modelo_dinamico(t,a,n,lod,thp)
!   --------------------------------------------------------------------
!   calculo de los valores de a y de lod
!   --------------------------------------------------------------------
    implicit none
!   --------------------------------------------------------------------
    real (kind=dp), intent(in) :: t
    real (kind=dp), intent(out) :: a,n,lod,thp
!   --------------------------------------------------------------------
    if (ldem1) then
!   --------------------------------------------------------------------
          a = afit(t)
        lod = LODfit(t)
!  ---------------------------------------------------------------------
    else if (ldem2) then
!   --------------------------------------------------------------------
          a = dadt0*t + a0
        lod = dLODdt0*t + lodf
!   --------------------------------------------------------------------
    else if (ldem3) then
!   --------------------------------------------------------------------
          a = ma*t + a0
        lod = mlod*t + lodf
!   --------------------------------------------------------------------
    else
        print *,'error en el identificador del modelo dinamico'
        return
    end if
!    
      n = dsqrt(mu/a)/a
    thp = 2.d0*pi/(lod*3600.d0)
!   --------------------------------------------------------------------
    end subroutine modelo_dinamico
!=======================================================================
    subroutine leer_entrada(tc0,tm0,DTcmb,DTlbl0,linit,demid,tidefl, &
                            radcfl,radmfl,dtprint)
!   --------------------------------------------------------------------
    implicit none
!   --------------------------------------------------------------------
    real(kind=dp), intent(out) :: tc0,tm0,DTcmb,DTlbl0,dtprint
    logical, dimension(2), intent(out) :: linit
    integer, intent(out) :: demid,tidefl,radcfl,radmfl
    integer :: k,initfl,idreo
!   --------------------------------------------------------------------
!   archivo de entrada
!   --------------------------------------------------------------------
    open(unit=10,file='../in/THERMEV2.IN',status='unknown')
!   --------------------------------------------------------------------
!     inicializacion de variables
!   --------------------------------------------------------------------
    read(10,*) algo
    read(10,*) Tsup,tc0,tm0,DTcmb,DTlbl0
    read(10,*) algo
    read(10,*) initfl
    read(10,*) algo
    read(10,*) e
    read(10,*) algo
    read(10,*) tidefl,radcfl,radmfl
    read(10,*) algo
    read(10,*) thermfl
    read(10,*) algo
    read(10,*) corefl
    read(10,*) algo
    read(10,*) idreo
    read(10,*) algo
    read(10,*) demid
    read(10,*) algo
    read(10,*) lmax,qmax
    read(10,*) algo
    read(10,*) dtprint
!   --------------------------------------------------------------------
    linit(1) = initfl.eq.1
    linit(2) = initfl.eq.2
    ltide = tidefl.eq.1
    lradc = radcfl.eq.1
    lradm = radmfl.eq.1
    ltherm = thermfl.eq.1
    lcore = corefl.eq.1
    do k = 1,8
        lreo(k) = idreo.eq.k
    end do
    ldem1 = demid.eq.1
    ldem2 = demid.eq.2
    ldem3 = demid.eq.3
!   --------------------------------------------------------------------
    close(unit=10)
!   --------------------------------------------------------------------
    end subroutine leer_entrada
!=======================================================================
    subroutine inicializar_temperaturas(linit,tc0,tm0,DTcmb,DTlbl0,Tcmb0,Tubl0)

        implicit none

        logical, dimension(2), intent(in) :: linit
        integer :: k
        integer, parameter :: kmax = 10
        real(kind=dp), intent(in) :: tc0,tm0,DTcmb,DTlbl0
        real(kind=dp), intent(out) :: Tcmb0,Tubl0
        real(kind=dp) :: Tubl,Tlbl,DTubl,DTlbl,dubl,dlbl

        if(linit(1)) then 
            Tcmb0 = Tc0/epsc
            Tubl0 = Tm0/epsm
        else if (linit(2)) then
            Tcmb0 = xi(x0)*(1.d0 + (tlc1 + tlc2*pec)*pec)/ &
                    (1.d0 + (ta1 + ta2*pec)*pec) + DTcmb
            Tlbl = Tcmb0 - DTlbl0
            Tubl = Tlbl/(2.d0*epsm - 1.d0)
!           ------------------------------------------------------------
            k = 1
!           ------------------------------------------------------------   
            do
!           -------------------------------------------------------------  
                Tubl0 = Tubl
                print *,Tubl
!               --------------------------------------------------------
                call conveccion(Tcmb0,Tubl,Tlbl,DTubl,DTlbl,dubl,dlbl)
!               --------------------------------------------------------  
                Tubl = Tlbl/(1.d0 + alfam*gum*(Rt-dubl-Rc-dlbl)/cm)
!               --------------------------------------------------------  
                if((dabs(Tubl-Tubl0).lt.1.d-4).or.(k.eq.kmax)) exit
!               -------------------------------------------------------- 
                k = k + 1
!           ------------------------------------------------------------ 
            end do
!   --------------------------------------------------------------------
        end if
        
    end subroutine inicializar_temperaturas
!=======================================================================
    subroutine timestamp(chanio,chmes,chdia,chhora,chmins,chsegs)
!   --------------------------------------------------------------------
!   esta subrutina imprime el año, mes, dia, hora, minutos y segundos
!   como caracteres para incluirlos en los nombres del archivo de salida
!   y del perfil de temperaturas.
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------
        integer :: tvals(8),anio,mes,dia,hora,mins,segs
        character (len=4), intent(out) :: chanio,chmes,chdia,chhora, &
                                          chmins,chsegs
!       ---------------------------------------------------------------
        call date_and_time(values=tvals)
        anio = tvals(1)
         mes = tvals(2)
         dia = tvals(3)
        hora = tvals(5)
        mins = tvals(6)
        segs = tvals(7)
!       ---------------------------------------------------------------
        write(chanio,'(i4)') anio
!       ---------------------------------------------------------------
        if (mes.lt.10) then
            write(chmes,'(i1)') mes
            chmes = trim('0'//chmes)
        else
            write(chmes,'(i2)') mes
        end if
!       ---------------------------------------------------------------
        if (dia.lt.10) then
            write(chdia,'(i1)') dia
            chdia = trim('0'//chdia)
        else
            write(chdia,'(i2)') dia
        end if
!       ---------------------------------------------------------------
        if (hora.lt.10) then
            write(chhora,'(i1)') hora
            chhora = trim('0'//chhora)
        else
            write(chhora,'(i2)') hora
        end if
!       ---------------------------------------------------------------
        if (mins.lt.10) then
            write(chmins,'(i1)') mins
            chmins = trim('0'//chmins)
        else
        write(chmins,'(i2)') mins
        end if
!       ---------------------------------------------------------------
        if (segs.lt.10) then
            write(chsegs,'(i1)') segs
            chsegs = trim('0'//chsegs)
        else
            write(chsegs,'(i2)') segs
        end if
!   --------------------------------------------------------------------
    end subroutine timestamp
!=======================================================================
    subroutine crear_archivo_salida(demid,tidefl,radcfl,radmfl)
!   -------------------------------------------------------------------
        implicit none
!       ---------------------------------------------------------------
        integer, intent(in) :: demid,tidefl,radcfl,radmfl
        character (len=10) :: reo,chidfit,chtide,chradc,chradm
        character (len=4) :: chanio,chmes,chdia,chhora,chmins,chsegs
!       ---------------------------------------------------------------
!       archivo de salida
!       ---------------------------------------------------------------
        call timestamp(chanio,chmes,chdia,chhora,chmins,chsegs)
!       ----------------------------------------------------------------
        if (lreo(1)) then
            reo = 'elast'
        else if (lreo(2)) then
            reo = 'visc'
        else if (lreo(3)) then
            reo = 'max'
        else if (lreo(4)) then
            reo = 'bur'
        else if (lreo(5)) then
            reo = 'and'
        else if (lreo(6)) then
            reo = 'qconst'
        else if (lreo(7)) then
            reo = 'dtconst'
        else if (lreo(8)) then
            reo = 'e&l2007'
        end if
        write(chidfit,'(i1)') demid
        write(chtide,'(i1)') tidefl
        write(chradc,'(i1)') radcfl
        write(chradm,'(i1)') radmfl
        open(unit=11,file='../out/thermev2_'//trim(reo)//'_demid_'// &
            trim(chidfit)//'_tide_'//trim(chtide)//'_radc_'//trim(chradc)// &
            '_radm_'//trim(chradm)//'_'//trim(chanio)//'-'//trim(chmes)//'-' &
            //trim(chdia)//'_'//trim(chhora)//'_'//trim(chmins)//'_' &
            //trim(chsegs)//'.out',status='unknown')
!   --------------------------------------------------------------------
    end subroutine crear_archivo_salida
!=======================================================================
    subroutine imprimir_perfil(tprint,Tcmb,Tubl)
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------
        real (kind=dp), intent(in) :: tprint,Tcmb,Tubl
        character (len=4) :: chanio,chmes,chdia,chhora,chmins,chsegs
        real (kind=dp) :: r,tdr,Tlbl,dlm,dum,Dtubl,Dtlbl
        character (len=4) :: chtprint
        real (kind=dp), parameter ::   dr = 1.d2
!       ----------------------------------------------------------------
!       impresión de los perfiles de temperatura a lo largo de la 
!       integración
!       ----------------------------------------------------------------
        call timestamp(chanio,chmes,chdia,chhora,chmins,chsegs)
!       ----------------------------------------------------------------
        if (tprint.eq.0.d0) then
            write(chtprint,'(i1)') int(tprint)
        else if(tprint.lt.1.d0) then
            write(chtprint,'(i3)') int(tprint*1.d3)
        else
            write(chtprint,'(i4)') int(tprint*1.d3)
        end if
!       ----------------------------------------------------------------
        open(unit=12,file='../out/temprofile'//trim(chtprint)//'_'//trim(chanio)// &
        '-'//trim(chmes)//'-'//trim(chdia)//'_'//trim(chhora)//'_'//trim(chmins)//'_' &
        //trim(chsegs)//'.out',status='unknown')
!       ----------------------------------------------------------------
        call capas_limites(Tcmb,Tubl,Tlbl,Dtubl,Dtlbl,dum,dlm)
!       ----------------------------------------------------------------
        r = 0.d0
        do while (r.le.Rt)
            tdr = Temprof(Tcmb,Tlbl,Tubl,dlm,dum,r)
            write(12,*) r,tdr,visc(tdr)
            r = r + dr
        end do
!       ----------------------------------------------------------------
        close(12)
!   --------------------------------------------------------------------
    end subroutine imprimir_perfil
!=======================================================================
    subroutine imprimir_Tliqc(tprint,chi)
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------
        real(kind=dp), intent(in) :: chi,tprint
        character (len=4) :: chanio,chmes,chdia,chhora,chmins,chsegs
        character (len=4) :: chtprint
        real (kind=dp), parameter :: dr = 1.d2
        real (kind=dp) :: r
!       ----------------------------------------------------------------
        call timestamp(chanio,chmes,chdia,chhora,chmins,chsegs)
!       ----------------------------------------------------------------
        if (tprint.eq.0.d0) then
            write(chtprint,'(i1)') int(tprint)
        else if(tprint.lt.1.d0) then
            write(chtprint,'(i3)') int(tprint*1.d3)
        else
            write(chtprint,'(i4)') int(tprint*1.d3)
        end if
!       ----------------------------------------------------------------
        open(unit=20,file='../out/Tliqc'//trim(chtprint)//'_'//trim(chanio)// &
        '-'//trim(chmes)//'-'//trim(chdia)//'_'//trim(chhora)//'_'//trim(chmins)//'_' &
        //trim(chsegs)//'.out',status='unknown')
!       ----------------------------------------------------------------
        r = 0.d0
        do while (r.le.Rc)
            write(20,*) r,Tliqc(chi,r)
            r = r + dr
        end do
!   --------------------------------------------------------------------        
    end subroutine imprimir_Tliqc
!=======================================================================
    subroutine reologia(l,w,eta,kr,ki)
!   --------------------------------------------------------------------
    implicit none
!   --------------------------------------------------------------------
    real (kind=dp), intent(in) :: w,eta
    real (kind=dp), intent(out) :: kr,ki
    real (kind=dp) :: x,zeta,bsubl,facl,denkl,numki,numkr,xpr,xpi,lag
    integer,intent(in) :: l
!   --------------------------------------------------------------------
    x = dabs(w)
    zeta = zandr0*(100.d0*dexp(-x/0.2d0) + 1.d0)
    bsubl = dble(2*l**2+4*l+3)*ksubb/dble(l)
    facl = 1.5d0/dble(l-1)
    if (lreo(1)) then
!    puramente elastico
      kr = facl*flex/(flex + bsubl)
      ki = 0.d0
      return
    else if (lreo(2)) then
!      puramente viscoso
      denkl = (bsubl*x*eta)**2 + 1.d0
      numkr = 1.d0
      numki = -bsubl*x*eta
    else if (lreo(3)) then
!      maxwell
      denkl = ((eta*x)**2)*(flex + bsubl)**2 + 1.d0
      numkr = (flex + bsubl)*flex*(eta*x)**2 + 1.d0
      numki = -bsubl*eta*x
    else if (lreo(4)) then
!      burgers
      xpr = flex*eta*x*(1.d0 + kflex/((kflex*keta*flex*eta*x)**2 + 1.d0))
      xpi = - (1.d0 + keta*(kflex*flex*eta*x)**2/((kflex*keta*flex*eta*x)**2 + 1.d0))         
      numkr = (xpr + bsubl*eta*x)*xpr + xpi**2
      numki = bsubl*eta*x*xpi
      denkl = (xpr + bsubl*eta*x)**2 + xpi**2             
    else if (lreo(5)) then
!     andrade
      xpr = flex*eta*x + ((flex*eta*x)**(1.d0-alpha))*gammac/(zeta**alpha)
      xpi = - (1.d0 + ((flex*eta*x)**(1.d0-alpha))*gammas/(zeta**alpha))
      numkr = (xpr + bsubl*eta*x)*xpr + xpi**2
      numki = bsubl*eta*x*xpi
      denkl = (xpr + bsubl*eta*x)**2 + xpi**2
    else if (lreo(6)) then
!      kaula (1964)
      kr = 0.d0
      ki = k2/qf
      return
    else if (lreo(7)) then
!      singer-mignard
      kr = 0.d0
      ki = k2*x*deltat
      return
    else if (lreo(8)) then
!      efroimsky & lainey (2007)
      lag = epsmay*(epsmay*x)**(-alpha-1.0d0)
      kr = 0.d0
      ki = k2*lag*x
      return
    else
      print *,'error en el identificador de la reologia'
      call abort
    end if
    kr =  facl*numkr/denkl
    ki = -facl*numki/denkl
!   --------------------------------------------------------------------
    return
!   --------------------------------------------------------------------
    end subroutine reologia
!=======================================================================
    subroutine evalklm()
!   --------------------------------------------------------------------
      implicit none
!     ------------------------------------------------------------------
!     evaluacion de las funciones k_lm
!     ------------------------------------------------------------------
!     l=2
      alm(2,0) = 1.0d0
      alm(2,1) = 0.333333333333333d0
      alm(2,2) = 0.0833333333333333d0
!     l=3
      alm(3,0) = 1.0d0
      alm(3,1) = 0.166666666666667d0
      alm(3,2) = 0.0166666666666667d0
      alm(3,3) = 0.00277777777777778d0
!   --------------------------------------------------------------------
    end subroutine evalklm
!=======================================================================
    subroutine evalfi(inc,f2i)
!   --------------------------------------------------------------------
      implicit none
!   --------------------------------------------------------------------
      real (kind=dp), intent(in) :: inc
      real (kind=dp) :: ci
      real (kind=dp), intent(out) :: f2i(lmaxp,0:lmaxp,0:lmaxp)
!   --------------------------------------------------------------------
      ci = dcos(inc)
!     l=2
      f2i(2,0,0) = 0.140625d0*(-1.0d0 + ci**2)**2
      f2i(2,0,1) = 0.5625d0*(-0.333333333333333d0 + ci**2)**2
      f2i(2,0,2) = 0.140625d0*(-1.0d0 + ci**2)**2
      f2i(2,1,0) = -0.5625d0*(-1.0d0 + ci)*(1.0d0 + ci)**3
      f2i(2,1,1) = 2.25d0*ci**2*(1.0d0 - ci**2)
      f2i(2,1,2) = -0.5625d0*(-1.0d0 + ci)**3*(1.0d0 + ci)
      f2i(2,2,0) = 0.5625d0*(1.0d0 + ci)**4
      f2i(2,2,1) = 2.25d0*(-1.0d0 + ci)**2*(1.0d0 + ci)**2
      f2i(2,2,2) = 0.5625d0*(-1.0d0 + ci)**4
!     l=3
      f2i(3,0,0) = -0.09765625d0*(-1.0d0 + ci**2)**3
      f2i(3,0,1) = 0.87890625d0*(1.0d0 - ci**2)*(-0.2d0 + ci**2)**2
      f2i(3,0,2) = 0.87890625d0*(1.0d0 - ci**2)*(-0.2d0 + ci**2)**2
      f2i(3,0,3) = -0.09765625d0*(-1.0d0 + ci**2)**3
      f2i(3,1,0) = 3.515625d0*(1.0d0 + ci)**4*(0.5d0 - ci + 0.5d0*ci**2) &
      **2/(-1.0d0 + ci)**2
      f2i(3,1,1) = 7.91015625d0*(1.0d0 + ci)**2*(0.0666666666666667d0 + &
      0.666666666666667d0*ci - ci**2)**2
      f2i(3,1,2) = 7.91015625d0*(-1.0d0 + ci)**2*(-0.0666666666666667d0 &
      + 0.666666666666667d0*ci + ci**2)**2
      f2i(3,1,3) = 3.515625d0*(-1.0d0 + ci)**4*(0.5d0 + ci + &
      0.5d0*ci**2)**2/(1.0d0 + ci)**2
      f2i(3,2,0) = -3.515625d0*(-1.0d0 + ci)*(1.0d0 + ci)**5
      f2i(3,2,1) = 31.640625d0*(1.0d0 - ci)*(-0.333333333333333d0 + ci) &
      **2*(1.0d0 + ci)**3
      f2i(3,2,2) = -31.640625d0*(-1.0d0 + ci)**3*(0.333333333333333d0 + &
      ci)**2*(1.0d0 + ci)
      f2i(3,2,3) = -3.515625d0*(-1.0d0 + ci)**5*(1.0d0 + ci)
      f2i(3,3,0) = 3.515625d0*(1.0d0 + ci)**6
      f2i(3,3,1) = 31.640625d0*(-1.0d0 + ci)**2*(1.0d0 + ci)**4
      f2i(3,3,2) = 31.640625d0*(-1.0d0 + ci)**4*(1.0d0 + ci)**2
      f2i(3,3,3) = 3.515625d0*(-1.0d0 + ci)**6
!   --------------------------------------------------------------------
    end subroutine evalfi
!=======================================================================
    subroutine evalge(exc,g2e)
!   --------------------------------------------------------------------
    implicit none
!   --------------------------------------------------------------------
    real (kind=dp), intent(in) :: exc
    real (kind=dp) :: e2,e3,e4,e5,e6,e7,e8,e9,e10
    real (kind=dp), intent(out) :: g2e(lmaxp,0:lmaxp,-qmaxp:qmaxp)
!   --------------------------------------------------------------------
       e2 = exc*exc
       e3 = e2*exc
       e4 = e2*e2
       e5 = e4*exc
       e6 = e4*e2
       e7 = e6*exc
       e8 = e6*e2
       e9 = e8*exc
      e10 = e8*e2
!   --------------------------------------------------------------------
!     l=2
!     ---
      g2e(2,0,-10) = 0.d0
      g2e(2,0,-9) = 0.d0
      g2e(2,0,-8) = 0.d0
      g2e(2,0,-7) = 0.d0
      g2e(2,0,-6) = 0.d0
      g2e(2,0,-5) = 0.0040045166015625d0*e10
      g2e(2,0,-4) = e8*(0.00173611111111111d0 + 0.00243055555555556d0*e2)
      g2e(2,0,-3) = e6*(0.000434027777777778d0 + e2*( &
      0.000596788194444444d0 + 0.000629679361979166d0*e2))
      g2e(2,0,-2) = 0.d0
      g2e(2,0,-1) = e2*(0.25d0 + e2*(-0.0625d0 + e2*( &
     0.0169270833333333d0 + e2*(0.00613064236111114d0 + &
     0.0053690592447917d0*e2))))
      g2e(2,0,0) = 1.0d0 + e2*(-5.0d0 + e2*(7.875d0 + e2*( &
      -4.30555555555556d0 + e2*(1.25043402777778d0 - 0.181302083333333d0*e2))))
      g2e(2,0,1) = e2*(12.25d0 + e2*(-53.8125d0 + e2*( &
      85.83984375d0 + e2*(-64.76318359375d0 + 28.4081359863281d0*e2))))
      g2e(2,0,2) = e4*(72.25d0 + e2*(-325.833333333333d0 + e2*( &
      580.215277777778d0 - 547.1625d0*e2)))
      g2e(2,0,3) = e6*(309.906684027778d0 + e2*(-1491.08208550347d0 &
      + 2986.78270975749d0*e2))
      g2e(2,0,4) = e8*(1109.72265625d0 - 5757.64921875d0*e2)
      g2e(2,0,5) = 3536.12958502876d0*e10
      g2e(2,0,6) = 0.d0
      g2e(2,0,7) = 0.d0
      g2e(2,0,8) = 0.d0
      g2e(2,0,9) = 0.d0
      g2e(2,0,10) = 0.d0
      g2e(2,1,-10) = 0.d0
      g2e(2,1,-9) = 0.d0
      g2e(2,1,-8) = 0.d0
      g2e(2,1,-7) = 0.d0
      g2e(2,1,-6) = 0.d0
      g2e(2,1,-5) = 47.9664459228516d0*e10
      g2e(2,1,-4) = e8*(23.16015625d0 + 7.76015625d0*e2)
      g2e(2,1,-3) = e6*(10.97265625d0 + e2*(10.17041015625d0 + 18.3712188720703d0*e2))
      g2e(2,1,-2) = e4*(5.0625d0 + e2*(7.875d0 + e2*(12.9765625d0 + 18.7921875d0*e2)))
      g2e(2,1,-1) = e2*(2.25d0 + e2*(5.0625d0 + e2*(8.96484375d0 + &
      e2*(13.86865234375d0 + 19.7799133300781d0*e2))))
      g2e(2,1,0) = 1.0d0 + e2*(3.0d0 + e2*(6.0d0 + e2*(10.0d0 + exc &
      **2*(15.0d0 + 21.0d0*e2))))
      g2e(2,1,1) = e2*(2.25d0 + e2*(5.0625d0 + e2*(8.96484375d0 + &
      e2*(13.86865234375d0 + 19.7799133300781d0*e2))))
      g2e(2,1,2) = e4*(5.0625d0 + e2*(7.875d0 + e2*(12.9765625d0 + 18.7921875d0*e2)))
      g2e(2,1,3) = e6*(10.97265625d0 + e2*(10.17041015625d0 + 18.3712188720703d0*e2))
      g2e(2,1,4) = e8*(23.16015625d0 + 7.76015625d0*e2)
      g2e(2,1,5) = 47.9664459228516d0*e10
      g2e(2,1,6) = 0.d0
      g2e(2,1,7) = 0.d0
      g2e(2,1,8) = 0.d0
      g2e(2,1,9) = 0.d0
      g2e(2,1,10) = 0.d0
      g2e(2,2,-10) = 0.d0
      g2e(2,2,-9) = 0.d0
      g2e(2,2,-8) = 0.d0
      g2e(2,2,-7) = 0.d0
      g2e(2,2,-6) = 0.d0
      g2e(2,2,-5) = 3536.12958502876d0*e10
      g2e(2,2,-4) = e8*(1109.72265625d0 - 5757.64921875d0*e2)
      g2e(2,2,-3) = e6*(309.906684027778d0 + e2*(-1491.08208550347d0 &
      + 2986.78270975749d0*e2))
      g2e(2,2,-2) = e4*(72.25d0 + e2*(-325.833333333333d0 + e2*( &
      580.215277777778d0 - 547.1625d0*e2)))
      g2e(2,2,-1) = e2*(12.25d0 + e2*(-53.8125d0 + e2*( &
      85.83984375d0 + e2*(-64.76318359375d0 + 28.4081359863281d0*e2))))
      g2e(2,2,0) = 1.0d0 + e2*(-5.0d0 + e2*(7.875d0 + e2*( &
      -4.30555555555556d0 + e2*(1.25043402777778d0 - &
      0.181302083333333d0*e2))))
      g2e(2,2,1) = e2*(0.25d0 + e2*(-0.0625d0 + e2*( &
      0.0169270833333333d0 + e2*(0.00613064236111114d0 + 0.0053690592447917d0*e2))))
      g2e(2,2,2) = 0.d0
      g2e(2,2,3) = e6*(0.000434027777777778d0 + e2*( &
      0.000596788194444444d0 + 0.000629679361979166d0*e2))
      g2e(2,2,4) = e8*(0.00173611111111111d0 + 0.00243055555555556d0*exc**2)
      g2e(2,2,5) = 0.0040045166015625d0*e10
      g2e(2,2,6) = 0.d0
      g2e(2,2,7) = 0.d0
      g2e(2,2,8) = 0.d0
      g2e(2,2,9) = 0.d0
      g2e(2,2,10) = 0.d0
!     ---
!     l=3
!     ---
      g2e(3,0,-10) = 0.d0
      g2e(3,0,-9) = 0.d0
      g2e(3,0,-8) = 0.d0
      g2e(3,0,-7) = 0.d0
      g2e(3,0,-6) = 0.d0
      g2e(3,0,-5) = 6.94444444444445d-5*e10
      g2e(3,0,-4) = e8*(6.7816840277778d-6 + 1.35633680555556d-5*e2)
      g2e(3,0,-3) = 0.d0
      g2e(3,0,-2) = e4*(0.015625d0 + e2*(0.00520833333333334d0 + exc** &
      2*(0.00490993923611111d0 + 0.00393880208333333d0*e2)))
      g2e(3,0,-1) = e2*(1.0d0 + e2*(-2.5d0 + e2*( &
      1.85416666666667d0 + e2*(-0.524305555555555d0 + 0.136197916666667d0*e2))))
      g2e(3,0,0) = 1.0d0 + e2*(-12.0d0 + e2*(49.21875d0 + e2*( &
      -83.21875d0 + e2*(68.0408935546875d0 - 31.233955078125d0*e2))))
      g2e(3,0,1) = e2*(25.0d0 + e2*(-220.0d0 + e2*( &
      736.916666666667d0 + e2*(-1221.72222222222d0 + 1147.2109375d0*exc**2))))
      g2e(3,0,2) = e4*(252.015625d0 + e2*(-2027.36979166667d0 + e2 &
      *(6597.1491156684d0 - 11511.4770507813d0*e2)))
      g2e(3,0,3) = e6*(1660.5625d0 + e2*(-13126.59375d0 + 43691.82890625d0*e2))
      g2e(3,0,4) = e8*(8504.77816433377d0 - 68154.558710395d0*e2)
      g2e(3,0,5) = 36828.8084027778d0*e10
      g2e(3,0,6) = 0.d0
      g2e(3,0,7) = 0.d0
      g2e(3,0,8) = 0.d0
      g2e(3,0,9) = 0.d0
      g2e(3,0,10) = 0.d0
      g2e(3,1,-10) = 0.d0
      g2e(3,1,-9) = 0.d0
      g2e(3,1,-8) = 0.d0
      g2e(3,1,-7) = 0.d0
      g2e(3,1,-6) = 0.d0
      g2e(3,1,-5) = 14.0312673611111d0*e10
      g2e(3,1,-4) = e8*(7.18072509765625d0 + 23.6063720703125d0*e2)
      g2e(3,1,-3) = e6*(3.67361111111111d0 + e2*(14.2152777777778d0 + 36.3644097222222d0*e2))
      g2e(3,1,-2) = e4*(1.890625d0 + e2*(8.421875d0 + e2*( &
      23.4019368489583d0 + 51.6582790798611d0*e2)))
      g2e(3,1,-1) = e2*(1.0d0 + e2*(5.0d0 + e2*(15.0d0 + e2*(35.0d0 + 70.0d0*e2))))
      g2e(3,1,0) = 1.0d0 + e2*(4.0d0 + e2*(11.46875d0 + e2*( &
      26.4756944444444d0 + e2*(53.2151557074653d0 + 96.8403244357639d0*e2))))
      g2e(3,1,1) = e2*(9.0d0 + e2*(16.5d0 + e2*(38.1875d0 + e2*( &
      71.4791666666667d0 + 124.026996527778d0*e2))))
      g2e(3,1,2) = e4*(43.890625d0 + e2*(32.296875d0 + e2*( &
      97.048095703125d0 + 149.09169921875d0*e2)))
      g2e(3,1,3) = e6*(164.694444444444d0 + e2*(-13.3680555555555d0 + 254.317795138889d0*e2))
      g2e(3,1,4) = e8*(532.960510253906d0 - 416.388549804688d0*e2)
      g2e(3,1,5) = 1567.17015625d0*e10
      g2e(3,1,6) = 0.d0
      g2e(3,1,7) = 0.d0
      g2e(3,1,8) = 0.d0
      g2e(3,1,9) = 0.d0
      g2e(3,1,10) = 0.d0
      g2e(3,2,-10) = 0.d0
      g2e(3,2,-9) = 0.d0
      g2e(3,2,-8) = 0.d0
      g2e(3,2,-7) = 0.d0
      g2e(3,2,-6) = 0.d0
      g2e(3,2,-5) = 1567.17015625d0*e10
      g2e(3,2,-4) = e8*(532.960510253906d0 - 416.388549804688d0*e2)
      g2e(3,2,-3) = e6*(164.694444444444d0 + e2*(-13.3680555555555d0 + 254.317795138889d0*e2))
      g2e(3,2,-2) = e4*(43.890625d0 + e2*(32.296875d0 + e2*( &
      97.048095703125d0 + 149.09169921875d0*e2)))
      g2e(3,2,-1) = e2*(9.0d0 + e2*(16.5d0 + e2*(38.1875d0 + e2* &
      (71.4791666666667d0 + 124.026996527778d0*e2))))
      g2e(3,2,0) = 1.0d0 + e2*(4.0d0 + e2*(11.46875d0 + e2*( &
      26.4756944444444d0 + e2*(53.2151557074653d0 + 96.8403244357639d0*e2))))
      g2e(3,2,1) = e2*(1.0d0 + e2*(5.0d0 + e2*(15.0d0 + e2*( 35.0d0 + 70.0d0*e2))))
      g2e(3,2,2) = e4*(1.890625d0 + e2*(8.421875d0 + e2*( &
      23.4019368489583d0 + 51.6582790798611d0*e2)))
      g2e(3,2,3) = e6*(3.67361111111111d0 + e2*(14.2152777777778d0 + 36.3644097222222d0*e2))
      g2e(3,2,4) = e8*(7.18072509765625d0 + 23.6063720703125d0*e2)
      g2e(3,2,5) = 14.0312673611111d0*e10
      g2e(3,2,6) = 0.d0
      g2e(3,2,7) = 0.d0
      g2e(3,2,8) = 0.d0
      g2e(3,2,9) = 0.d0
      g2e(3,2,10) = 0.d0
      g2e(3,3,-10) = 0.d0
      g2e(3,3,-9) = 0.d0
      g2e(3,3,-8) = 0.d0
      g2e(3,3,-7) = 0.d0
      g2e(3,3,-6) = 0.d0
      g2e(3,3,-5) = 36828.8084027778d0*e10
      g2e(3,3,-4) = e8*(8504.77816433377d0 - 68154.558710395d0*e2)
      g2e(3,3,-3) = e6*(1660.5625d0 + e2*(-13126.59375d0 + 43691.82890625d0*e2))
      g2e(3,3,-2) = e4*(252.015625d0 + e2*(-2027.36979166667d0 + exc** &
      2*(6597.1491156684d0 - 11511.4770507813d0*e2)))
      g2e(3,3,-1) = e2*(25.0d0 + e2*(-220.0d0 + e2*( &
      736.916666666667d0 + e2*(-1221.72222222222d0 + 1147.2109375d0*exc**2))))
      g2e(3,3,0) = 1.0d0 + e2*(-12.0d0 + e2*(49.21875d0 + e2*( &
      -83.21875d0 + e2*(68.0408935546875d0 - 31.233955078125d0*e2))))
      g2e(3,3,1) = e2*(1.0d0 + e2*(-2.5d0 + e2*(1.85416666666667d0 &
     + e2*(-0.524305555555555d0 + 0.136197916666666d0*e2))))
      g2e(3,3,2) = e4*(0.015625d0 + e2*(0.00520833333333334d0 + e2 &
      *(0.00490993923611111d0 + 0.00393880208333333d0*e2)))
      g2e(3,3,3) = 0.d0
      g2e(3,3,4) = e8*(6.7816840277778d-6 + 1.35633680555556d-5*e2)
      g2e(3,3,5) = 6.94444444444445d-5*e10
      g2e(3,3,6) = 0.d0
      g2e(3,3,7) = 0.d0
      g2e(3,3,8) = 0.d0
      g2e(3,3,9) = 0.d0
      g2e(3,3,10) = 0.d0
!   --------------------------------------------------------------------
    end subroutine evalge
!=======================================================================
    function sumar(n,a)
!   --------------------------------------------------------------------
    implicit none
!   --------------------------------------------------------------------
    integer, intent(in) :: n
    integer :: k
    real (kind=dp) :: a(n),suma,c,t
    real (kind=dp) :: sumar
!   --------------------------------------------------------------------
      suma = a(1)
      c = 0.d0
      do k=2,n
       t = suma + a(k)
       if(dabs(suma).ge.dabs(a(k))) then
        c = c + ((suma - t) + a(k))
        else
        c = c + ((a(k) - t) + suma)
       end if
       suma = t
      enddo
      sumar = suma + c
!   --------------------------------------------------------------------
    end function sumar
!=======================================================================
    function pdr(r)
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------
        real (kind=dp), intent(in) :: r
        real (kind=dp) :: pdr,rkm
        real (kind=dp), parameter, dimension(3) :: coefm = (/ 4.35256d-6, -0.0901277d0, 397.012d0 /)
        real (kind=dp), parameter, dimension(3) :: coefc = (/ -1.52163d-5, -0.0144178d0, 367.767d0 /)
!       ----------------------------------------------------------------
        rkm = r*1.d-3
        if (r.le.Rc) then
            pdr = (coefc(1)*rkm + coefc(2))*rkm + coefc(3)
        else
            pdr = (coefm(1)*rkm + coefm(2))*rkm + coefm(3)
        end if
!   --------------------------------------------------------------------
    end function pdr
!=======================================================================
    function tsol(r)
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------
        real (kind=dp), intent(in) :: r
        real (kind=dp) :: tsol
!       ----------------------------------------------------------------
        if (pdr(r).le.20.d0) then
            tsol = 1661.2d0*dexp(dlog(pdr(r)/1.336d0 + 1.d0)/7.437d0)
        else
            tsol = 2081.8d0*dexp(dlog(pdr(r)/101.69d0 + 1.d0)/1.226d0)
        end if
!   --------------------------------------------------------------------
    end function tsol
!=======================================================================
    function tliq(r)
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------
        real (kind=dp), intent(in) :: r
        real (kind=dp) :: tliq
!       ----------------------------------------------------------------
        if (pdr(r).le.20.d0) then
            tliq = 1982.1d0*dexp(dlog(pdr(r)/6.594d0 + 1.d0)/5.374d0)
        else
            tliq = 2006.8d0*dexp(dlog(pdr(r)/34.65d0 + 1.d0)/1.844d0)
        end if
!   --------------------------------------------------------------------
    end function tliq
!=======================================================================
    function tcondubl(Tubl,dum,r)
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------
        real (kind=dp), intent(in) ::Tubl,dum,r
        real (kind=dp) :: tcondubl
!       ----------------------------------------------------------------
        tcondubl = Tsup + (Tubl-Tsup)*(Rt-r)/dum
!   --------------------------------------------------------------------
    end function tcondubl
!=======================================================================
    function tcondlbl(Tcmb,Tlbl,dlm,r)
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------
        real (kind=dp), intent(in) :: Tcmb,Tlbl,dlm,r
        real (kind=dp) :: tcondlbl
!       ----------------------------------------------------------------
        tcondlbl = Tcmb + (Tcmb-Tlbl)*(Rc-r)/dlm
!   --------------------------------------------------------------------
    end function tcondlbl
!=======================================================================
    function Temprof(Tcmb,Tlbl,Tubl,dlm,dum,r)
!   --------------------------------------------------------------------
    implicit none
!   --------------------------------------------------------------------
    real (kind=dp), intent(in) :: r,dlm,dum,Tcmb,Tlbl,Tubl
    real (kind=dp) :: Temprof
    logical :: incore,inlbl,inmantle,inubl
!   --------------------------------------------------------------------
    incore = r*(Rc-r).ge.0.d0
    inlbl = (r-Rc)*((Rc+dlm)-r).ge.0d0
    inmantle = (r-(Rc+dlm))*((Rt-dum)-r).ge.0d0
    inubl = (r-(Rt-dum))*(Rt-r).ge.0d0
!   --------------------------------------------------------------------
    if (incore) then
        Temprof = tc(Tcmb,r)
    else if (inlbl) then
        Temprof = tcondlbl(Tcmb,Tlbl,dlm,r)
    else if (inmantle) then
        Temprof = tm(Tubl,dum,r)
    else if (inubl) then
        Temprof = tcondubl(Tubl,dum,r)
    else
        print *,'Error in Temprof: r is out of domain.'
        call abort
    end if
!   --------------------------------------------------------------------
    end function Temprof
!=======================================================================
    function visc(T,P)
!   --------------------------------------------------------------------
    implicit none
!   --------------------------------------------------------------------
    real (kind=dp), intent(in) :: T
    real (kind=dp), intent(in), optional :: P
    real (kind=dp) :: visc
!   --------------------------------------------------------------------
    if(present(P)) then
        visc = visc0*dexp((Eact+P*Vact)/(Rgas*T))
    else
        visc = etaref*dexp(Eact*(1.d0/T - 1.d0/Tref)/Rgas)
    end if
!   --------------------------------------------------------------------
    end function visc
!=======================================================================
    function tsoldb(r)
!   --------------------------------------------------------------------
    implicit none
!   --------------------------------------------------------------------
    real (kind=dp) :: tsoldb
    real (kind=dp), intent(in) :: r
    real (kind=dp), parameter :: asol = -1.160d-16
    real (kind=dp), parameter :: bsol = 1.708d-9
    real (kind=dp), parameter :: csol = -9.074d-3
    real (kind=dp), parameter :: dsol = 1.993d4
!   --------------------------------------------------------------------
    tsoldb = ((asol*r + bsol)*r + csol)*r + dsol
!   --------------------------------------------------------------------
    end function tsoldb
!=======================================================================
    function tliqdb(r)
!   --------------------------------------------------------------------
    implicit none
!   --------------------------------------------------------------------
    real (kind=dp) :: tliqdb
    real (kind=dp), intent(in) :: r
!   --------------------------------------------------------------------
    tliqdb = tsol(r) + 500.d0
!   --------------------------------------------------------------------
    end function tliqdb
!=======================================================================
    function tliqc(x,r)
!   --------------------------------------------------------------------
!   perfil de temperatura del liquidus del nucleo (stevenson et al 1983)
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------
        real (kind=dp), intent(in) :: r,x
        real (kind=dp) :: tliqc
!       ----------------------------------------------------------------
        tliqc = tlc0*(1.d0 - 2.d0*x)*(1.d0 + (tlc1 + tlc2*pdr(r))*pdr(r))
!   --------------------------------------------------------------------
    end
!=======================================================================
    function tsolc(r)
!   --------------------------------------------------------------------
    implicit none
!   --------------------------------------------------------------------
    real (kind=dp), intent(in) :: r
    real (kind=dp) :: tsolc
!   --------------------------------------------------------------------
    tsolc = tfe0*dexp(-2.d0*(1.d0 - 1.d0/(3.d0*grun))*(r**2)/(dfe**2))
!   --------------------------------------------------------------------
    end function tsolc
!=======================================================================
    function tc(Tcmb,r)
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------
        real (kind=dp), intent(in) :: Tcmb,r
        real (kind=dp) :: tc
!       ----------------------------------------------------------------
        tc = Tcmb*(1.d0 + (ta1 + ta2*pdr(r))*pdr(r))/ &
                      (1.d0 + (ta1 + ta2*pcmb)*pcmb)
!   --------------------------------------------------------------------
    end function tc
!=======================================================================
    function tcdb(Tcmb,r)
!   --------------------------------------------------------------------
!   perfil adiabático de temperatura en el núcleo
!   --------------------------------------------------------------------
    implicit none
!   --------------------------------------------------------------------
    real (kind=dp), intent(in) :: r,Tcmb
    real (kind=dp) :: tcdb
!   --------------------------------------------------------------------
    tcdb = Tcmb*dexp((Rc**2 - r**2)/(dn**2))
!   --------------------------------------------------------------------
    end function tcdb
!=======================================================================
    function tm(Tubl,dum,r)
!   --------------------------------------------------------------------
!   perfil adiabático de temperatura del manto
!   --------------------------------------------------------------------
    implicit none
!   --------------------------------------------------------------------
    real (kind=dp), intent(in) :: r,dum,Tubl
    real (kind=dp) :: tm
!   --------------------------------------------------------------------
    tm = Tubl*(1.d0 + alfam*gum*(Rt-dum-r)/cm)
!   --------------------------------------------------------------------
    end function tm
!=======================================================================
    function fmeltubl(r,Tubl,dum)
!   --------------------------------------------------------------------
    implicit none
!   --------------------------------------------------------------------
    real (kind=dp), intent(in) :: r,dum,Tubl
    real (kind=dp) :: fmeltubl,tdr
!   --------------------------------------------------------------------
    tdr = tcondubl(Tubl,dum,r)
    if((tdr-tsol(r))*(tliq(r)-tdr).gt.0.d0) then 
        fmeltubl = (tdr - tsol(r))/(tliq(r) - tsol(r))
    else if (tdr.ge.tliq(r)) then
        fmeltubl = 1.d0
    else 
        fmeltubl = 0.d0
    end if
!   --------------------------------------------------------------------
    end function fmeltubl
!=======================================================================
    function fmeltm(r,Tubl,dum)
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------
        real (kind=dp), intent(in) :: r,dum,Tubl
        real (kind=dp) :: fmeltm,tdr
!       ----------------------------------------------------------------
        tdr = Tm(Tubl,dum,r)
        if((tdr-tsol(r))*(tliq(r)-tdr).gt.0.d0) then 
            fmeltm = (tdr - tsol(r))/(tliq(r) - tsol(r))
        else if (tdr.ge.tliq(r)) then
            fmeltm = 1.d0
        else 
            fmeltm = 0.d0
        end if
!       ----------------------------------------------------------------
    end function fmeltm
!=======================================================================
    function fmeltmr2(r,Tubl,dum)
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------
        real (kind=dp), intent(in) :: Tubl,dum,r
        real (kind=dp) :: fmeltmr2
!       ----------------------------------------------------------------
        fmeltmr2 = fmeltm(r,Tubl,dum)*r**2
!   --------------------------------------------------------------------
    end function fmeltmr2
!=======================================================================
    function fmeltublr2(r,Tubl,dum)
!   --------------------------------------------------------------------
    implicit none
!   --------------------------------------------------------------------
    real (kind=dp), intent(in) :: Tubl,dum,r
    real (kind=dp) :: fmeltublr2
!   --------------------------------------------------------------------
    fmeltublr2 = fmeltubl(r,Tubl,dum)*r**2
!   --------------------------------------------------------------------
    end function fmeltublr2
!=======================================================================
    function avgfmelt(Tcmb,Tubl)
!   --------------------------------------------------------------------
!   Cálculo de la fracción de material fundido promediado en el manto
!   --------------------------------------------------------------------        
        implicit none

        real(kind=dp), intent(in) :: Tcmb,Tubl
        real(kind=dp) :: avgfmelt,int1,int2,rint, &
                         Tlbl,DTubl,DTlbl,dubl,dlbl

        call capas_limites(Tcmb,Tubl,Tlbl,DTubl,DTlbl,dubl,dlbl)

        rint = Rt-dubl
    
        call qromb(fmeltmr2,Rc,rint,int1,Tubl,dubl)

        call qromb(fmeltublr2,rint,Rt,int2,Tubl,dubl)
    
        avgfmelt = 3.d0*(int1+int2)/denVm

    end function avgfmelt
!=======================================================================
    function stefan(Tubl,dubl)
!   --------------------------------------------------------------------
!   Cálculo del número de stefan
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------
        real (kind=dp), intent(in) :: Tubl,dubl
        real (kind=dp) :: stefan
        !err,dphidT
        real (kind=dp) :: r1,r2,rint,denVa,int
!       ----------------------------------------------------------------
!        dphidT = dfridr(avgfmelt,Tcmb,Tubl,h,err)
!        stefan = (Lmelt/cm)*(3.d0/denVm)*dphidT
!       ----------------------------------------------------------------
        r1 = Rast(Tubl,dubl)
        rint = Rt - dubl
        r2 = Rlit(Tubl,dubl)
!       ----------------------------------------------------------------
        if(r2-r1.gt.1.d0) then
!       ----------------------------------------------------------------
            denVa = r2**3 - r1**3
!           ------------------------------------------------------------
            call qromb(dfmeltmdTubl,r1,rint,int,dubl)
            stefan = (Lmelt/cm)*(3.d0/denVm)*int
!           ------------------------------------------------------------
            call qromb(dfmeltmdTubl,rint,r2,int,dubl)
            stefan = stefan + (Lmelt/cm)*(3.d0/denVm)*int
!       ----------------------------------------------------------------
        else
!       ----------------------------------------------------------------
            stefan = 0.d0
!       ----------------------------------------------------------------
        end if
!   --------------------------------------------------------------------
    end function stefan
!=======================================================================
    function dfmeltmdTubl(r,dubl)
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------        
        real(kind=dp), intent(in) :: r,dubl
        real(kind=dp) :: dfmeltmdTubl
!       ----------------------------------------------------------------
        dfmeltmdTubl = (1.d0 + alfam*gum*(Rt-dubl-r)/cm)*r*r/(Tliq(r)-Tsol(r))
!   --------------------------------------------------------------------
    end function dfmeltmdTubl
!=======================================================================
    function dfmeltubldTubl(r,dubl)
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------        
        real(kind=dp), intent(in) :: r,dubl
        real(kind=dp) :: dfmeltubldTubl
!       ----------------------------------------------------------------
        dfmeltubldTubl = ((Rt-r)/dubl)*r*r  /(Tliq(r)-Tsol(r))
!   --------------------------------------------------------------------
    end function dfmeltubldTubl
!=======================================================================
!
!=======================================================================
    function dTmTsol(r,Tubl,dubl)
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------
        real (kind=dp), intent(in) :: r,Tubl,dubl
        real (kind=dp) :: dTmTsol
!       ----------------------------------------------------------------
        dTmTsol = tm(Tubl,dubl,r)-tsol(r)
!   --------------------------------------------------------------------
    end function dTmTsol
!=======================================================================
    function Rast(Tubl,dubl)
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------
        real (kind=dp), intent(in) :: Tubl,dubl
        real (kind=dp) :: rast
!       ----------------------------------------------------------------
!       determinacion de la distancia entre el centro de la tierra y el
!       limite inferior de la astenosfera
!       ----------------------------------------------------------------
        if(Tubl.gt.tsol(Rt-dubl)) then
            Rast = rtbis(dTmTsol,Tubl,dubl,Rc,Rt-dubl,1.d-3)
        else 
            Rast = Rt - dubl
        end if
!   --------------------------------------------------------------------
    end function rast
!=======================================================================
    function dTcndTsol(r,Tubl,dubl)
!   --------------------------------------------------------------------        
        implicit none
!       ----------------------------------------------------------------        
        real(kind=dp), intent(in) :: r,Tubl,dubl
        real(kind=dp) :: dTcndTsol
!       ----------------------------------------------------------------
        dTcndTsol = tcondubl(Tubl,dubl,r)-tsol(r)
!   --------------------------------------------------------------------        
    end function dTcndTsol
!=======================================================================
    function Rlit(Tubl,dubl)
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------
        real (kind=dp), intent(in) :: Tubl,dubl
        real (kind=dp) :: Rlit
!       ----------------------------------------------------------------
!       Determinación de la distancia radial entre el centro de la 
!       tierra yla base de la litósfera o límite superior de la astenósfera
!       ----------------------------------------------------------------
        if(Tubl.gt.tsol(Rt-dubl)) then
            Rlit = rtbis(dTcndTsol,Tubl,dubl,Rt-dubl,Rt,1.d-3)
        else 
            Rlit = Rt - dubl
        end if
!   --------------------------------------------------------------------
    end function Rlit
!=======================================================================
    function tsolr2(r)
!   --------------------------------------------------------------------
    implicit none
!   --------------------------------------------------------------------
    real (kind=dp), intent(in) :: r
    real (kind=dp) :: tsolr2
!   --------------------------------------------------------------------
    tsolr2 = tsol(r)*r*r
!   --------------------------------------------------------------------
    end function tsolr2
!=======================================================================
    function tliqr2(r)
!   --------------------------------------------------------------------
    implicit none
!   --------------------------------------------------------------------
    real (kind=dp), intent(in) :: r
    real (kind=dp) :: tliqr2
!   --------------------------------------------------------------------
    tliqr2 = tliq(r)*r*r
!   --------------------------------------------------------------------
    end
!=======================================================================
    function meltfdism(r,Tubl,dubl)
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------
        real (kind=dp), intent(in) :: r,Tubl,dubl
        real (kind=dp) :: meltfdism
!       ----------------------------------------------------------------
        meltfdism = fmeltm(Tubl,dubl,r) - phidis
!   --------------------------------------------------------------------
    end function meltfdism
!=======================================================================
    function Rphim(Tubl,dubl)
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------
        real (kind=dp), intent(in) :: Tubl,dubl
        real (kind=dp) :: Rphim
!       ----------------------------------------------------------------
        if (fmeltm(Tubl,dubl,Rt-dubl).ge.phidis) then 
            Rphim = rtbis(meltfdism,Tubl,dubl,Rc,Rt-dubl,1.d-3)
        else 
            Rphim = Rt - dubl
        end if
!   --------------------------------------------------------------------        
    end function Rphim
!=======================================================================
    function meltfdisubl(r,Tubl,dubl)
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------
        real (kind=dp), intent(in) :: r,Tubl,dubl
        real (kind=dp) :: meltfdisubl
!       ----------------------------------------------------------------
        meltfdisubl = fmeltubl(Tubl,dubl,r) - phidis
!   --------------------------------------------------------------------
    end function meltfdisubl
!=======================================================================
    function Rphiubl(Tubl,dubl)
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------
        real (kind=dp), intent(in) :: Tubl,dubl
        real (kind=dp) :: Rphiubl
!       ----------------------------------------------------------------
        Rphiubl = rtbis(meltfdisubl,Tubl,dubl,Rt-dubl,Rt,1.d-3)
!   --------------------------------------------------------------------        
    end function Rphiubl
!=======================================================================
    function xi(x)

        implicit none

        real(kind=dp), intent(in) :: x
        real(kind=dp) :: xi

        xi = tlc0*(1.d0 - 2.d0*x)*(1.d0 + (ta1 + ta2*pcmb)*pcmb)
        
    end function xi
!=======================================================================
    function fPio(x,Tcmb,chi)
        
        implicit none
        
        real(kind=dp), intent(in) :: x,chi,Tcmb
        real(kind=dp) :: fPio,cof0,cof1,cof2

        cof0 = Tcmb - xi(chi)
        cof1 = Tcmb*ta1 - xi(chi)*tlc1
        cof2 = Tcmb*ta2 - xi(chi)*tlc2

        fPio = cof0 + (cof1 + cof2*x)*x
        
    end function fPio
!==============================================================================
    subroutine qromb(func,a,b,ss,par1,par2)
    real (kind=dp), intent(in) :: a,b
    real(kind=dp),intent(in), optional :: par1,par2
    real (kind=dp), intent(out) :: ss
    real (kind=dp), parameter :: eps=1.d-6
    real(kind=dp), external :: func
    integer, parameter :: jmax=20, jmaxp=jmax+1, k=5, km=k-1
    integer :: j
!   uses polint,trapzd
    real*8 dss,h(jmaxp),s(jmaxp)
    h(1)=1.
    do j=1,jmax
        if(present(par1)) then 
            if(present(par2)) then 
                call trapzd(func,a,b,s(j),j,par1,par2)
            else
                call trapzd(func,a,b,s(j),j,par1)
            end if
        else
            call trapzd(func,a,b,s(j),j)
        end if 
      if (j.ge.k) then
        call polint(h(j-km),s(j-km),k,0.d0,ss,dss)
        if (abs(dss).le.eps*abs(ss)) return
      endif
      s(j+1)=s(j)
      h(j+1)=0.25d0*h(j)
    end do
    print *, 'too many steps in qromb'
    end subroutine qromb
!=======================================================================
      subroutine polint(xp,yp,n,x,y,dy)
      integer :: n,k,m,ns
      real (kind=dp), intent(in) :: x,xp(n),yp(n)
      real (kind=dp), intent(out) :: y,dy
      integer, parameter :: kmax=10
      real (kind=dp) :: dlen,dif,dift,ho,hp,w,c(kmax),d(kmax)
      ns=1
      dif=dabs(x-xp(1))
      do k=1,n
        dift=dabs(x-xp(k))
        if (dift.lt.dif) then
          ns=k
          dif=dift
        endif
        c(k)=yp(k)
        d(k)=yp(k)
      end do
      y=yp(ns)
      ns=ns-1
      do m=1,n-1
        do k=1,n-m
          ho=xp(k)-x
          hp=xp(k+m)-x
          w=c(k+1)-d(k)
          dlen=ho-hp
          if(dlen.eq.0.d0) print *,'failure in polint'
          dlen=w/dlen
          d(k)=hp*dlen
          c(k)=ho*dlen
        end do
        if (2*ns.lt.n-m)then
          dy=c(ns+1)
        else
          dy=d(ns)
          ns=ns-1
        endif
        y=y+dy
      end do
      return
      end subroutine polint
!=======================================================================
      subroutine trapzd(func,a,b,s,n,par1,par2)
      integer, intent(in) :: n
      real (kind=dp), intent(in) :: a,b
      real(kind=dp),intent(in), optional :: par1,par2
      real (kind=dp), intent(out) :: s
      integer :: it,j
      real (kind=dp) :: del,sum,tnm,x,func,fval
      if (n.eq.1) then
        if(present(par1)) then 
            if(present(par2)) then 
                s=0.5d0*(b-a)*(func(a,par1,par2)+func(b,par1,par2))
            else 
                s=0.5d0*(b-a)*(func(a,par1)+func(b,par1))
            end if
        else
            s=0.5d0*(b-a)*(func(a)+func(b))
        end if
      else
        it=2**(n-2)
        tnm=it
        del=(b-a)/tnm
        x=a+0.5d0*del
        sum=0.d0
        do j=1,it
            if(present(par1)) then 
                if(present(par2)) then 
                    fval = func(x,par1,par2)
                else 
                    fval = func(x,par1)
                end if
            else
                fval = func(x)
            end if
            sum=sum + fval
            x=x+del
        end do
        s=0.5d0*(s+(b-a)*sum/tnm)
      end if
      return
      end subroutine trapzd
!=======================================================================
    function gammln(xx)
      implicit none
      real(kind=dp) :: gammln
      real(kind=dp), intent(in) :: xx
      integer :: j
      real(kind=dp) :: ser,stp,tmp,x,y,cof(6)
      save cof,stp
      cof(1) = 76.18009172947146d0
      cof(2) = -86.50532032941677d0
      cof(3) = 24.01409824083091d0
      cof(4) = -1.231739572450155d0
      cof(5) = .1208650973866179d-2
      cof(6) = -.5395239384953d-5
      stp = 2.5066282746310005d0
      x=xx
      y=x
      tmp=x+5.5d0
      tmp=(x+0.5d0)*dlog(tmp)-tmp
      ser=1.000000000190015d0
      do j=1,6
        y=y+1.d0
        ser=ser+cof(j)/y
      end do
      gammln=tmp+dlog(stp*ser/x)
      return
    end function gammln
!=======================================================================
    subroutine odeint(ystart,nvar,x1,x2,eps,h1,hmin,nok,nbad,derivs,intsch)
    integer nbad,nok,nvar,kmaxx,maxstp,nmaxx
    real*8 eps,h1,hmin,x1,x2,ystart(nvar),tiny
    external :: derivs,intsch
    parameter (maxstp=10000,nmaxx=50,kmaxx=200,tiny=1.e-30)
    integer k,kmax,kount,nstp
    real*8 dxsav,h,hdid,hnext,x,xsav,dydx(nmaxx),xp(kmaxx),y(nmaxx), &
           yp(nmaxx,kmaxx),yscal(nmaxx)
    common /path/ kmax,kount,dxsav,xp,yp
    x=x1
    h=sign(h1,x2-x1)
    nok=0
    nbad=0
    kount=0
    do k=1,nvar
      y(k)=ystart(k)
    end do
    if (kmax.gt.0) xsav=x-2.*dxsav
    do nstp=1,maxstp
      call derivs(x,y,dydx)
      do k=1,nvar
        yscal(k)=abs(y(k))+abs(h*dydx(k))+tiny
      end do
      if(kmax.gt.0)then
        if(abs(x-xsav).gt.abs(dxsav)) then
          if(kount.lt.kmax-1)then
            kount=kount+1
            xp(kount)=x
            do k=1,nvar
              yp(k,kount)=y(k)
            end do
            xsav=x
          endif
        endif
      endif
      if((x+h-x2)*(x+h-x1).gt.0.) h=x2-x
      call intsch(y,dydx,nvar,x,h,eps,yscal,hdid,hnext,derivs)
      if(hdid.eq.h)then
        nok=nok+1
      else
        nbad=nbad+1
      endif
      if((x-x2)*(x2-x1).ge.0.)then
        do k=1,nvar
          ystart(k)=y(k)
        end do
        if(kmax.ne.0)then
          kount=kount+1
          xp(kount)=x
          do k=1,nvar
            yp(k,kount)=y(k)
          end do
        endif
        return
      endif
      if(abs(hnext).lt.hmin) then 
        print *,'stepsize smaller than minimum in odeint'
        print *, 'Waiting for Enter.'
        read(stdin,*)
      end if
      h=hnext
    end do
    print *,'too many steps in odeint'
    return
    end
!=======================================================================
    subroutine bsstep(y,dydx,nv,x,htry,eps,yscal,hdid,hnext,derivs)
    integer nv,nmaxx,kmaxx,imax
    real*8 eps,hdid,hnext,htry,x,dydx(nv),y(nv),yscal(nv),safe1,safe2, &
           redmax,redmin,tiny,scalmx
    parameter (nmaxx=50,kmaxx=8,imax=kmaxx+1,safe1=.25d0,safe2=.7d0, &
               redmax=1.d-5,redmin=.7d0,tiny=1.d-30,scalmx=.1d0)
!   uses derivs,mmid,pzextr
    external :: derivs
    integer k,iq,j,kk,km,kmax,kopt,nseq(imax)
    real*8 eps1,epsold,errmax,fact,h,red,scale,work,wrkmin,xest,xnew, &
           a(imax),alf(kmaxx,kmaxx),err(kmaxx),yerr(nmaxx),ysav(nmaxx),yseq(nmaxx)
    logical first,reduct
    save a,alf,epsold,first,kmax,kopt,nseq,xnew
    data first/.true./,epsold/-1.d0/
    data nseq /2,4,6,8,10,12,14,16,18/
    if(eps.ne.epsold)then
      hnext=-1.d29
      xnew=-1.d29
      eps1=safe1*eps
      a(1)=nseq(1)+1
      do k=1,kmaxx
        a(k+1)=a(k)+nseq(k+1)
      end do
      do iq=2,kmaxx
        do k=1,iq-1
          alf(k,iq)=eps1**((a(k+1)-a(iq+1))/((a(iq+1)-a(1)+1.d0)*(2*k+1)))
        end do
      end do
      epsold=eps
      do kopt=2,kmaxx-1
        if(a(kopt+1).gt.a(kopt)*alf(kopt-1,kopt))goto 1
      end do
1     kmax=kopt
    endif
    h=htry
    do k=1,nv
      ysav(k)=y(k)
    end do
    if(h.ne.hnext.or.x.ne.xnew)then
      first=.true.
      kopt=kmax
    endif
    reduct=.false.
2   do k=1,kmax
      xnew=x+h
      if(xnew.eq.x) then 
        print *,'step size underflow in bsstep'
        print *, 'Waiting for Enter.'
        read(stdin,*)
      end if
      call mmid(ysav,dydx,nv,x,h,nseq(k),yseq,derivs)
      xest=(h/nseq(k))**2
      call pzextr(k,xest,yseq,y,yerr,nv)
      if(k.ne.1)then
        errmax=tiny
        do j=1,nv
          errmax=max(errmax,abs(yerr(j)/yscal(j)))
        end do
        errmax=errmax/eps
        km=k-1
        err(km)=(errmax/safe1)**(1./(2*km+1))
      endif
      if(k.ne.1.and.(k.ge.kopt-1.or.first))then
        if(errmax.lt.1.)goto 4
        if(k.eq.kmax.or.k.eq.kopt+1)then
          red=safe2/err(km)
          goto 3
        else if(k.eq.kopt)then
          if(alf(kopt-1,kopt).lt.err(km))then
            red=1./err(km)
            goto 3
          endif
        else if(kopt.eq.kmax)then
          if(alf(km,kmax-1).lt.err(km))then
            red=alf(km,kmax-1)*safe2/err(km)
            goto 3
          endif
        else if(alf(km,kopt).lt.err(km))then
          red=alf(km,kopt-1)/err(km)
          goto 3
        endif
      endif
    end do
3   red=min(red,redmin)
    red=max(red,redmax)
    h=h*red
    reduct=.true.
    goto 2
4   x=xnew
    hdid=h
    first=.false.
    wrkmin=1.e35
    do kk=1,km
      fact=max(err(kk),scalmx)
      work=fact*a(kk+1)
      if(work.lt.wrkmin)then
        scale=fact
        wrkmin=work
        kopt=kk+1
      endif
    end do
    hnext=h/scale
    if(kopt.ge.k.and.kopt.ne.kmax.and..not.reduct)then
      fact=max(scale/alf(kopt-1,kopt),scalmx)
      if(a(kopt+1)*fact.le.wrkmin)then
        hnext=h/fact
        kopt=kopt+1
      endif
    endif
    return
    end
!=======================================================================
    subroutine mmid(y,dydx,nvar,xs,htot,nstep,yout,derivs)
    integer nstep,nvar,nmaxx
    real*8 htot,xs,dydx(nvar),y(nvar),yout(nvar)
    external :: derivs
    parameter (nmaxx=50)
    integer k,n
    real*8 h,h2,swap,x,ym(nmaxx),yn(nmaxx)
    h=htot/nstep
    do k=1,nvar
      ym(k)=y(k)
      yn(k)=y(k)+h*dydx(k)
    end do
    x=xs+h
    call derivs(x,yn,yout)
    h2=2.*h
    do n=2,nstep
      do k=1,nvar
        swap=ym(k)+h2*yout(k)
        ym(k)=yn(k)
        yn(k)=swap
      end do
      x=x+h
      call derivs(x,yn,yout)
    end do
    do k=1,nvar
      yout(k)=0.5*(ym(k)+yn(k)+h*yout(k))
    end do
    return
    end
!=======================================================================
    subroutine pzextr(iest,xest,yest,yz,dy,nv)
    integer iest,nv,imax,nmaxx
    real*8 xest,dy(nv),yest(nv),yz(nv)
    parameter (imax=13,nmaxx=50)
    integer j,k1
    real*8 delta,f1,f2,q,d(nmaxx),qcol(nmaxx,imax),x(imax)
    save qcol,x
    x(iest)=xest
    do j=1,nv
      dy(j)=yest(j)
      yz(j)=yest(j)
    end do
    if(iest.eq.1) then
      do j=1,nv
        qcol(j,1)=yest(j)
      end do
    else
      do j=1,nv
        d(j)=yest(j)
      end do
      do k1=1,iest-1
        delta=1./(x(iest-k1)-xest)
        f1=xest*delta
        f2=x(iest-k1)*delta
        do j=1,nv
          q=qcol(j,k1)
          qcol(j,k1)=dy(j)
          delta=d(j)-q
          dy(j)=f1*delta
          d(j)=f2*delta
          yz(j)=yz(j)+dy(j)
        end do
      end do
      do j=1,nv
        qcol(j,iest)=dy(j)
      end do
    endif
    return
    end
!=======================================================================
    subroutine rkqs(y,dydx,n,x,htry,eps,yscal,hdid,hnext,derivs)
    integer n,nmaxx
    real*8 eps,hdid,hnext,htry,x,dydx(n),y(n),yscal(n)
    procedure(dTdt) :: derivs
    parameter (nmaxx=50)
!   uses derivs,rkck
    integer k
    real*8 errmax,h,xnew,yerr(nmaxx),ytemp(nmaxx),safety,pgrow,pshrnk,errcon
    parameter (safety=0.9,pgrow=-.2,pshrnk=-.25,errcon=1.89e-4)
    h=htry
1   call rkck(y,dydx,n,x,h,ytemp,yerr,derivs)
    errmax=0.
    do k=1,n
      errmax=max(errmax,abs(yerr(k)/yscal(k)))
    end do
    errmax=errmax/eps
    if(errmax.gt.1.)then
      h=safety*h*(errmax**pshrnk)
      if(h.lt.0.1*h)then
        h=.1*h
      endif
      xnew=x+h
      if(xnew.eq.x) then 
        print *,'stepsize underflow in rkqs'
        print *, 'Waiting for Enter.'
        read(stdin,*)
      end if
      goto 1
    else
      if(errmax.gt.errcon)then
        hnext=safety*h*(errmax**pgrow)
      else
        hnext=5.*h
      endif
      hdid=h
      x=x+h
      do k=1,n
        y(k)=ytemp(k)
      end do
      return
    endif
    end
!======================================================================
    subroutine rkck(y,dydx,n,x,h,yout,yerr,derivs)
    integer n,nmaxx
    real*8 h,x,dydx(n),y(n),yerr(n),yout(n)
    external derivs
    parameter (nmaxx=50)
!   uses derivs
    integer k
    real*8 ak2(nmaxx),ak3(nmaxx),ak4(nmaxx),ak5(nmaxx),ak6(nmaxx), &
    ytemp(nmaxx),a2,a3,a4,a5,a6,b21,b31,b32,b41,b42,b43,b51,b52,b53, &
    b54,b61,b62,b63,b64,b65,c1,c3,c4,c6,dc1,dc3,dc4,dc5,dc6
    parameter (a2=.2,a3=.3,a4=.6,a5=1.,a6=.875,b21=.2,b31=3./40., &
    b32=9./40.,b41=.3,b42=-.9,b43=1.2,b51=-11./54.,b52=2.5, &
    b53=-70./27.,b54=35./27.,b61=1631./55296.,b62=175./512., &
    b63=575./13824.,b64=44275./110592.,b65=253./4096.,c1=37./378., &
    c3=250./621.,c4=125./594.,c6=512./1771.,dc1=c1-2825./27648., &
    dc3=c3-18575./48384.,dc4=c4-13525./55296.,dc5=-277./14336., &
    dc6=c6-.25)
    do k=1,n
        ytemp(k)=y(k)+b21*h*dydx(k)
    end do
    call derivs(x+a2*h,ytemp,ak2)
    do k=1,n
      ytemp(k)=y(k)+h*(b31*dydx(k)+b32*ak2(k))
    end do
    call derivs(x+a3*h,ytemp,ak3)
    do k=1,n
      ytemp(k)=y(k)+h*(b41*dydx(k)+b42*ak2(k)+b43*ak3(k))
    end do
    call derivs(x+a4*h,ytemp,ak4)
    do k=1,n
      ytemp(k)=y(k)+h*(b51*dydx(k)+b52*ak2(k)+b53*ak3(k)+b54*ak4(k))
    end do
    call derivs(x+a5*h,ytemp,ak5)
    do k=1,n
      ytemp(k)=y(k)+h*(b61*dydx(k)+b62*ak2(k)+b63*ak3(k)+b64*ak4(k)+b65*ak5(k))
    end do
    call derivs(x+a6*h,ytemp,ak6)
    do k=1,n
      yout(k)=y(k)+h*(c1*dydx(k)+c3*ak3(k)+c4*ak4(k)+c6*ak6(k))
    end do
    do k=1,n
      yerr(k)=h*(dc1*dydx(k)+dc3*ak3(k)+dc4*ak4(k)+dc5*ak5(k)+dc6*ak6(k))
    end do
    return
    end
!=======================================================================
    subroutine resolvente(a,b,c,x1,x2)
!-----------------------------------------------------------------------
        implicit none
!-----------------------------------------------------------------------
        real (kind=dp), intent(in) :: a,b,c
        real (kind=dp), intent(out) :: x1,x2
        real (kind=dp) :: q
!-----------------------------------------------------------------------
        q = -0.5d0*(b+dsign(1.d0,b)*dsqrt(b*b-4.d0*a*c))
        x1 = q/a
        x2 = c/q
!-----------------------------------------------------------------------
    end subroutine resolvente
!=======================================================================
    function depsda(t, x)
!   --------------------------------------------------------------------
!   Esta subrutina calcula la derivada temporal de la oblicuidad
!   terrestre utilizando la expresión dada por la ec. (A.6) del trabajo
!   de Farhat et al. (2022).
!   --------------------------------------------------------------------
        implicit none
!       ----------------------------------------------------------------
        real (kind=8), intent(in) :: t,x
        real (kind=8) :: depsda
        real (kind=8) :: a,lod,n,thp,Cthp,frac1,frac2
        real (kind=8), parameter :: Cthp0 = 0.3306947357075918999972d0*Me*Rt**2
        real (kind=8), parameter :: kf2 = 0.93d0
!       ----------------------------------------------------------------    
        call modelo_dinamico(t,a,n,lod,thp)
!       ----------------------------------------------------------------
        Cthp = Cthp0 + 2.d0*kf2*(Rt**5)*(thp**2 - thp0**2)/(9.d0*CGU)
!       ----------------------------------------------------------------
        frac1 = 0.25d0*mred*n*a/(Cthp*thp)
        frac2 = (thp*dcos(x) - 2.d0*n)/(thp*dcos(x) - n)
!       ----------------------------------------------------------------
        depsda = frac1*dsin(x)*frac2
!   --------------------------------------------------------------------
    end function depsda
!=======================================================================
    SUBROUTINE hunt(xx,n,x,jlo)
        INTEGER :: jlo,n
        REAL (kind=dp) x,xx(n)
        INTEGER :: inc,jhi,jm
        LOGICAL :: ascnd
        ascnd=xx(n).gt.xx(1)
        if(jlo.le.0.or.jlo.gt.n)then
          jlo=0
          jhi=n+1
          goto 3
        endif
        inc=1
        if(x.ge.xx(jlo).eqv.ascnd)then
  1       jhi=jlo+inc
          if(jhi.gt.n)then
            jhi=n+1
          else if(x.ge.xx(jhi).eqv.ascnd)then
            jlo=jhi
            inc=inc+inc
            goto 1
          endif
        else
          jhi=jlo
  2       jlo=jhi-inc
          if(jlo.lt.1)then
            jlo=0
          else if(x.lt.xx(jlo).eqv.ascnd)then
            jhi=jlo
            inc=inc+inc
            goto 2
          endif
        endif
  3     if(jhi-jlo.eq.1)return
        jm=(jhi+jlo)/2
        if(x.gt.xx(jm).eqv.ascnd)then
          jlo=jm
        else
          jhi=jm
        endif
        goto 3
        END SUBROUTINE hunt
!=======================================================================
    subroutine leer_oblicuidad()

        implicit none

        integer :: feof,k
        real (kind=dp) :: t,eps,epsdeg,p,a,lod

        open(unit=10,file='../out/oblicuidad.out')

        feof = 0
        k = 1
        
        do while(feof.eq.0)
        
            read(10,*,iostat=feof) t,eps,epsdeg,p,a,lod
!           ------------------------------------------------------------
            if (feof.gt.0) then
                print *,'revisar archivo de entrada'
                exit
            else if (feof.lt.0) then
                print *,  'listo!'
                exit
            else 
                xa(k) = t
                ya(k) = eps
                k = k + 1
            end if
!           ------------------------------------------------------------
        end do

        nmax = k - 1
  
    end subroutine leer_oblicuidad
!=======================================================================
    function oblicuidad(x)

        real(kind=dp), intent(in) :: x
        real(kind=dp) :: y,dy,oblicuidad
        integer :: j,k
    
        call hunt(xa,nmax,x,j)
        k = min(max(j-(mpol-1)/2,1),nmax+1-mpol)
        call polint(xa(k),ya(k),mpol,x,y,dy)
        
        oblicuidad = y

    end function oblicuidad
!=======================================================================
    FUNCTION dfridr(fun,par,x,h,err)

        real (kind=dp), intent(in) :: par,x,h
        REAL (kind=dp) :: dfridr,err
        real(kind=dp), external :: fun
        real (kind=dp), parameter :: CON=1.4d0
        real (kind=dp), parameter :: CON2=CON*CON
        real (kind=dp), parameter :: BIG=1.d30
        real (kind=dp), parameter :: SAFE=2.d0
        integer, parameter :: NTAB=10
!       USES func
        INTEGER :: k,j
        REAL (kind=dp) :: errt,fac,hh,a(NTAB,NTAB)
        if(h.eq.0.) print *, 'h must be nonzero in dfridr'
        hh=h
        a(1,1)=(fun(par,x+hh)-fun(par,x-hh))/(2.0*hh)
        err=BIG
        do k=2,NTAB
            hh=hh/CON
            a(1,k)=(fun(par,x+hh)-fun(par,x-hh))/(2.0*hh)
            fac=CON2
            do j=2,k
                a(j,k)=(a(j-1,k)*fac-a(j-1,k-1))/(fac-1.)
                fac=CON2*fac
                errt=max(abs(a(j,k)-a(j-1,k)),abs(a(j,k)-a(j-1,k-1)))
                if (errt.le.err) then
                err=errt
                dfridr=a(j,k)
                endif
            end do
            if(abs(a(k,k)-a(k-1,k-1)).ge.SAFE*err)return
        end do

        return

    END
!=======================================================================
    FUNCTION rtbis(func,par1,par2,x1,x2,xacc)

        implicit none

        INTEGER :: JMAX
        real(kind=dp) :: x1,x2,xacc,par1,par2
        REAL(kind=dp) :: rtbis,func
        PARAMETER (JMAX=40)
        INTEGER :: j
        REAL(kind=dp) :: dx,f,fmid,xmid
        fmid=func(x2,par1,par2)
        f=func(x1,par1,par2)
        if(f*fmid.ge.0.) then 
            print *, 'root must be bracketed in rtbis'
            print *, 'Waiting for Enter.'
            read(stdin,*)
        end if
        if(f.lt.0.)then
            rtbis=x1
            dx=x2-x1
        else
            rtbis=x2
            dx=x1-x2
        endif
        do 11 j=1,JMAX
            dx=dx*.5
            xmid=rtbis+dx
            fmid=func(xmid,par1,par2)
            if(fmid.le.0.)rtbis=xmid
            if(abs(dx).lt.xacc .or. fmid.eq.0.) return
    11    continue
        print *, 'too many bisections in rtbis'
    END
!=======================================================================

end module thermev2_subs