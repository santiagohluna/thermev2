!=======================================================================
program thermev_db
!-----------------------------------------------------------------------
    use thermev2_subs
!-----------------------------------------------------------------------
    implicit none
!-----------------------------------------------------------------------
    integer, parameter :: nvar = 2
    integer :: nbad,nok,l,m,p,q,ok,demid,tidefl,radcfl,radmfl
    character cr
    real(kind=dp) :: t,tprint,y(nvar),dydt(nvar),a,ric,&
                      ur,urtot,dlit,qcmb,qconv,qmelt,qradm, & 
                      qradc,tcmb,tubl,qtidal,tpot,Ra,St, &
                      Tlbl,DTubl,DTlbl,dubl,dlbl,dtprint
    real(kind=dp) :: tc0,tm0,DTcmb,DTlbl0,Tcmb0,Tubl0
    logical, dimension(2) :: linit
    real(kind=dp), parameter :: tol = 1.d-6
    real(kind=dp), parameter :: dt = 1.d-3
    real(kind=dp), parameter :: tera = 1.d-12
    real(kind=dp), parameter :: km = 1.d-3
!-----------------------------------------------------------------------
!   cálculos preliminares
!-----------------------------------------------------------------------
    print *,'Realizando calculos preliminares...'
    gamalf = dexp(gammln(alpha + 1.d0))
    gammac = gamalf*dcos(0.5d0*alpha*pi)
    gammas = gamalf*dsin(0.5d0*alpha*pi)
    ai = afit(0.d0)
    af = afit(t0)
    ma = (af-ai)/4.5d0
    mlod = (lodf-lodi)/4.5d0
!-----------------------------------------------------------------------
    cr = achar(13)
    print *,'... listo!'
!-----------------------------------------------------------------------
!   formatos de salida
!-----------------------------------------------------------------------
!10    format (a6,1x,f7.2,1x,a6,1x,f7.2,1x,a3,1x,i2)
!11  format(f5.3,1x,f7.2,1x,f7.2,1x,f7.2,1x,f6.2,1x,f4.2,1x,f4.2,1x, &
!            f8.6,1x,f10.3,1x,f6.2,1x,f6.2)
12  format(a,'Progreso =',1x,f5.1,1x,a1)
13  format(a9,1x,f7.2,1x,a2)
!-----------------------------------------------------------------------
!   lectura de archivo de entrada
!-----------------------------------------------------------------------
    print *,'Leyendo archivo de entrada...'
    call leer_entrada(tc0,tm0,DTcmb,DTlbl0,linit,demid,tidefl, &
    radcfl,radmfl,dtprint)
    print *,'... listo!'
    nterms = 0
    do l=2,lmax
        do m = 0,l
            do p = 0,l
                do q = -qmax,qmax
                    nterms = nterms + 1
                end do
            end do
        end do
    end do
    allocate(asuma(nterms),stat=ok)
    if (ok.ne.0) then
        print *,'Error en la asignación de memoria de asuma'
        stop
    endif
!-----------------------------------------------------------------------
!   evaluación de los factores k_lm, funciones de la inclinación y de la
!   excentricidad
!-----------------------------------------------------------------------
    print *,'Evaluando factores k_lm ...'
    call evalklm()
    print *,'... listo!'
    print *,'Leyendo datos para la evaluación de la oblicuidad ...'
    call leer_oblicuidad()
    print *,'... listo!'
    print *,'Evaluando funciones de la excentricidad ...'
    call evalge(e,gge)
    print *,'... listo!'
!-----------------------------------------------------------------------
!   creación de archivo de salida
!-----------------------------------------------------------------------
    print *,'Creando archivo de salida...'
    call crear_archivo_salida(demid,tidefl,radcfl,radmfl)
    print *,'... listo!'
!-----------------------------------------------------------------------
!   inicialización de las temperaturas medias del manto y del núcleo
!-----------------------------------------------------------------------
    print *,'Incializando variables...'
         t = 0.d0
    tprint = 0.d0
    dtprint = dtprint/1000.d0
    call inicializar_temperaturas(linit,tc0,tm0,DTcmb,DTlbl0,Tcmb0,Tubl0)
      y(1) = Tcmb0
      y(2) = Tubl0
    call dTdt(t,y,dydt)
    Tcmb = y(1)
    Tubl = y(2)
    Ur = printout(1)
    Urtot = printout(2)
     Qcmb = printout(3)
    Qconv = printout(4)
    Qmelt = printout(5)
    Qradm = printout(6)
    Qradc = printout(7)
   Qtidal = printout(8)
       St = printout(9)
      Ric = printout(10)
       Ra = printout(11)
    call capas_limites(Tcmb,Tubl,Tlbl,DTubl,DTlbl,dubl,dlbl)
    print *,'dTcmb/dt = ',dydt(1)
    print *,'dTm/dt = ',dydt(2)
    dlit = (rt - Rlit(tubl,dubl))*km
    tpot = tubl*dexp(-gum*alfam*dubl/cm)
    print *,'... listo!'
    print *,' '
    print *,'==================='
    print *,'  Estado inicial:  '
    print *,'==================='
    print 13,'    T_c =',Tc0,'K'
    print 13,'  T_cmb =',Tcmb,'K'
    print 13,'    T_m =',Tm0,'K'
    print 13,'  T_ubl =',Tubl,'K'
    print 13,'  T_lbl =',Tlbl,'K'
    print 13,'  T_pot = ',Tpot,'K'
    print 13,'   R_ic =',ric*km,'km'
    print 13,'    Lit =',dlit,'km'
    print 13,'  Q_cmb =',qcmb*tera,'TW'
    print 13,' Q_conv =',qconv*tera,'TW'
    print 13,' Q_melt =',qmelt*tera,'TW'
    print 13,'Q_rad,m =',qradm*tera,'TW'
    print 13,'Q_rad,c =',qradc*tera,'TW'
    print 13,'Q_tidal =',qtidal*tera,'TW'
    print 13,' Ur_tot =',urtot,'nd'
    print *,' '
!-----------------------------------------------------------------------
!   integracion de las ecuaciones diferenciales
!-----------------------------------------------------------------------
    print *,'Integrando ecuaciones diferenciales...'
    do while (t+dt.le.tf)
!       ----------------------------------------------------------------
!       impresión del perfil de temperatura
!       ----------------------------------------------------------------
        if (t.ge.tprint) then
            call imprimir_perfil(tprint,Tcmb,Tubl)
            tprint = tprint + dtprint
        end if
!       ----------------------------------------------------------------
        call capas_limites(Tcmb,Tubl,Tlbl,DTubl,DTlbl,dubl,dlbl)
!       ----------------------------------------------------------------
!       calculo del espesor de la litosfera
!       ----------------------------------------------------------------
        dlit = (rt - Rlit(tubl,dubl))*km
!       ----------------------------------------------------------------
!       calculo de la temperatura potencial
!       ----------------------------------------------------------------
        tpot = tubl*dexp(-gum*alfam*dubl/cm)
!       ----------------------------------------------------------------
            Ur = printout(1)
         Urtot = printout(2)
          Qcmb = printout(3)
         Qconv = printout(4)
         Qmelt = printout(5)
         Qradm = printout(6)
         Qradc = printout(7)
        Qtidal = printout(8)
            St = printout(9)
           Ric = printout(10)
            Ra = printout(11)
!       ----------------------------------------------------------------
!       escritura de resultados en el archivo de salida
!       ----------------------------------------------------------------
!                   1   2   3    4   5    6
        write(11,*) t,Tcmb,Tubl,Tpot,ur,urtot, &
!                    7     8       9     10  
                    dlit,Ric*km,dubl*km,dlbl*km, &
        !                11       12         13         14
                    qcmb*tera,qconv*tera,qmelt*tera,qradm*tera, &
        !               15          16        17      18 19
                    qradc*tera,qtidal*tera,visc(Tubl),Ra,St
!       ----------------------------------------------------------------
        call odeint(y,nvar,t,t+dt,tol,dt,0.d0,nok,nbad,dTdt,bsstep)    
        Tcmb = y(1)
        Tubl = y(2)
!       ----------------------------------------------------------------
        write(*,12,advance='no') cr,t*100.d0/tf,'%'
        t = t + dt
    end do
!   --------------------------------------------------------------------
    print *,' '
    print *,'... listo!'
    print *,' '
    print *,'==================='
    print *,'   Estado final:   '
    print *,'==================='
    print 13,'    T_c = ',epsc*Tcmb,'K'
    print 13,'  T_cmb = ',Tcmb,'K'
    print 13,'    T_m = ',epsm*Tubl,'K'
    print 13,'  T_ubl = ',Tubl,'K'
    print 13,'  T_lbl =',Tlbl,'K'
    print 13,'  T_pot = ',Tpot,'K'
    print 13,'   R_ic = ',Ric/1000.d0,'km'
    print 13,'    Lit = ',dlit,'km'
    print 13,'  Q_cmb = ',qcmb*tera,'TW'
    print 13,' Q_conv = ',qconv*tera,'TW'
    print 13,' Q_melt = ',qmelt*tera,'TW'
    print 13,'Q_rad,m = ',qradm*tera,'TW'
    print 13,'Q_rad,c = ',qradc*tera,'TW'
    print 13,'Q_tidal =',qtidal*tera,'TW'
    print 13,' Ur_tot = ',urtot,'nd'
    print 13,' '
!   --------------------------------------------------------------------
!   perfil de temperatura final
!   --------------------------------------------------------------------
    print *,'Imprimiendo perfil de temperatura final...'
    call imprimir_perfil(tprint,Tcmb,Tubl)
    print *,'... listo!'
!   --------------------------------------------------------------------
    print *,' '
!-----------------------------------------------------------------------
end program thermev_db
!=======================================================================