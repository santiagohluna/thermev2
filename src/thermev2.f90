!=======================================================================
program thermev_db
!-----------------------------------------------------------------------
    use thermev2_subs
!-----------------------------------------------------------------------
    implicit none
!-----------------------------------------------------------------------
    integer, parameter :: nvar = 3
    integer :: nbad,nok,l,m,p,q,ok
    character cr
    real (kind=dp) :: t,tprint,y(nvar),dydt(nvar),avgtc,avgtm,a,ric,&
                      ur,urtot,dum,dlm,dlit,qcmb,qconv,qmelt,qradm, & 
                      qradc,vm,num,tcmb,tubl,meltf,qtidal,tpot
    real (kind=dp), parameter :: eps = 1.d-6
    real (kind=dp), parameter :: dt = 1.d-3
    real (kind=dp), parameter :: tera = 1.d-12
    real (kind=dp), parameter :: km = 1.d-3
    common /printout/ dum,dlm,ur,urtot,qcmb,qconv,qmelt, &
                      qradm,qradc,qtidal,vm,num,tcmb,tubl,meltf
!-----------------------------------------------------------------------
!   cálculos preliminares
!-----------------------------------------------------------------------
    print *,'realizando calculos preliminares...'
    gamalf = dexp(gammln(alpha + 1.d0))
    gammac = gamalf*dcos(0.5d0*alpha*pi)
    gammas = gamalf*dsin(0.5d0*alpha*pi)
!   --------------------------------------------------------------------
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
    print *,'leyendo archivo de entrada...'
    call leer_entrada()
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
    call crear_archivo_salida()
    print *,'... listo!'
!-----------------------------------------------------------------------
!   inicialización de las temperaturas medias del manto y del núcleo
!-----------------------------------------------------------------------
    print *,'Incializando variables...'
         t = 0.d0
    tprint = 0.d0
    dtprint = dtprint/1000.d0
    avgtc = tc0
    avgtm = tm0
      y(1) = tc0
      y(2) = 0.d0
      Ric = dsqrt(y(2))
      y(3) = tm0
    call derivs(t,y,dydt)
    dlit = rt - rlit
    print *,'... listo!'
    print *,' '
    print *,'==================='
    print *,'  Estado inicial:  '
    print *,'==================='
    print 13,'    T_c =',avgtc,'K'
    print 13,'  T_cmb =',etac*avgtc,'K'
    print 13,'    T_m =',avgtm,'K'
    print 13,'  T_ubl =',etaum*avgtm,'K'
    print 13,'   R_ic =',ric*km,'km'
    print 13,'    Lit =',dlit,'km'
    print 13,'  Q_cmb =',qcmb*tera,'TW'
    print 13,' Q_conv =',qconv*tera,'TW'
    print 13,' Q_melt =',qmelt*tera,'TW'
    print 13,'Q_rad,m =',qradm*tera,'TW'
    print 13,'Q_rad,c =',qradc*tera,'TW'
    print 13,' Ur_tot =',urtot,'nd'
    print *,' '
!-----------------------------------------------------------------------
!   integracion de las ecuaciones diferenciales
!-----------------------------------------------------------------------
    print *,'integrando ecuaciones diferenciales...'
    do while (t+dt.le.tf)
!       ----------------------------------------------------------------
!       impresión del perfil de temperatura
!       ----------------------------------------------------------------
        if (t.ge.tprint) then
            call imprimir_perfil(tprint,avgtc,dlm,avgtm,dum)
            tprint = tprint + dtprint
        end if
!       ----------------------------------------------------------------
!       calculo del espesor de la litosfera
!       ----------------------------------------------------------------
        dlit = rt - rlit
!       ----------------------------------------------------------------
!       calculo de la temperatura potencial
!       ----------------------------------------------------------------
        tpot = tubl*dexp(-gum*alfam*dum/cm)
!       ----------------------------------------------------------------
!       escritura de resultados en el archivo de salida
!       ----------------------------------------------------------------
!                   1   2     3    4    5    6   7    8
        write(11,*) t,avgtc,avgtm,Tcmb,Tubl,Tpot,ur,urtot, &
!                    9     10     11     12  
                    dlit,ric*km,dum*km,dlm*km, &
        !                13       14         15         16
                    qcmb*tera,qconv*tera,qmelt*tera,qradm*tera, &
        !               17          18        19            
                    qradc*tera,qtidal*tera,visc(avgtm)
!       ----------------------------------------------------------------
        call odeint(y,nvar,t,t+dt,eps,dt,0.d0,nok,nbad,derivs,bsstep)    
        avgtc = y(1)
        Ric = dsqrt(y(2))
        avgtm = y(3)
!       ----------------------------------------------------------------
        write(*,12,advance='no') cr,t*100.d0/tf,'%'
        t = t + dt
    end do
    print *,' '
    print *,'... listo!'
    print *,' '
    print *,'==================='
    print *,'   Estado final:   '
    print *,'==================='
    print 13,'    T_c = ',avgtc,'K'
    print 13,'  T_cmb = ',etac*avgtc,'K'
    print 13,'    T_m = ',avgtm,'K'
    print 13,'  T_ubl = ',etaum*avgtm,'K'
    print 13,'   R_ic = ',ric/1000.d0,'km'
    print 13,'    Lit = ',dlit,'km'
    print 13,'  Q_cmb = ',qcmb*tera,'TW'
    print 13,' Q_conv = ',qconv*tera,'TW'
    print 13,' Q_melt = ',qmelt*tera,'TW'
    print 13,'Q_rad,m = ',qradm*tera,'TW'
    print 13,'Q_rad,c = ',qradc*tera,'TW'
    print 13,' Ur_tot = ',urtot,'nd'
    print 13,' '
!   --------------------------------------------------------------------
!   perfil de temperatura final
!   --------------------------------------------------------------------
    print *,'imprimiendo perfil de temperatura final...'
    call imprimir_perfil(tprint,avgtc,dlm,avgtm,dum)
    print *,'... listo!'
!   --------------------------------------------------------------------
    print *,' '
!-----------------------------------------------------------------------
end program thermev_db
!=======================================================================