!=======================================================================
program thermev_db
!-----------------------------------------------------------------------
    use thermev2_subs
!-----------------------------------------------------------------------
    implicit none
!-----------------------------------------------------------------------
    integer, parameter :: nvar = 2
    integer :: nbad,nok,l,m,p,q,ok
    character cr
    real (kind=dp) :: t,tprint,y(nvar),dydt(nvar),avgtc,avgtm,a,lod,ric,&
                      ur,urtot,dum,dlm,dlit,qcmb,qconv,qmelt,qradm, & 
                      qradc,vm,num,tcmb,tubl,meltf,qtidal,tpot
    real (kind=dp), parameter :: eps = 1.d-6
    real (kind=dp), parameter :: dt = 1.d-3
    common /printout/ a,lod,dum,dlm,ur,urtot,qcmb,qconv,qmelt, &
                      qradm,qradc,qtidal,vm,ric,num,tcmb,tubl,meltf
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
12  format(a,'progreso =',1x,f5.1,1x,a1)
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
    print *,'evaluando factores k_lm...'
    call evalklm()
    print *,'... listo!'
    print *,'evaluando funciones de la inclinación...'
    call evalfi(i,ffi)
    print *,'... listo!'
    print *,'evaluando funciones de la excentricidad...'
    call evalge(e,gge)
    print *,'... listo!'
!-----------------------------------------------------------------------
!   creación de archivo de salida
!-----------------------------------------------------------------------
    print *,'creando archivo de salida...'
    call crear_archivo_salida()
    print *,'... listo!'
!-----------------------------------------------------------------------
!   inicialización de las temperaturas medias del manto y del núcleo
!-----------------------------------------------------------------------
    print *,'incializando variables...'
         t = 0.d0
    tprint = 0.d0
    dtprint = dtprint/1000.d0
    avgtc = tc0
    avgtm = tm0
      y(1) = tc0
      y(2) = tm0
    call derivs(t,y,dydt)
    dlit = rt - rlit
    print *,'... listo!'
    print *,' '
    print *,'==================='
    print *,'  estado inicial:  '
    print *,'==================='
    print 13,'    t_c =',avgtc,'k'
    print 13,'  t_cmb =',etac*avgtc,'k'
    print 13,'    t_m =',avgtm,'k'
    print 13,'  t_ubl =',etaum*avgtm,'k'
    print 13,'   r_ic =',ric/1000.d0,'km'
    print 13,'    lit =',dlit,'km'
    print 13,'  q_cmb =',qcmb*1.d-12,'tw'
    print 13,' q_conv =',qconv*1.d-12,'tw'
    print 13,' q_melt =',qmelt*1.d-12,'tw'
    print 13,'q_rad,m =',qradm*1.d-12,'tw'
    print 13,'q_rad,c =',qradc*1.d-12,'tw'
    print 13,' ur_tot =',urtot,'nd'
    print *,' '
!   --------------------------------------------------------------------
!   escritura del estado inicial en el archivo de salida
!   --------------------------------------------------------------------
    print *,'escribiendo estado inicial en archivo de salida...'     
    write(11,*) t,avgtc,avgtm,dlit,a/a0,lod/lod0,ric/1.d3,ur,urtot,dum/1000.d0,dlm/1000.d0, &
                qcmb*1.d-12,qconv*1.d-12,qmelt*1.d-12,qradm*1.d-12,qradc*1.d-12,qtidal*1d-12, & 
                visc(avgtm),tcmb,meltf
    print *,'... listo!'
!   --------------------------------------------------------------------
!   perfil de temperatura inicial
!   --------------------------------------------------------------------
    print *,'imprimiendo perfil de temperatura inicial...'
    call imprimir_perfil(tprint,avgtc,dlm,avgtm,dum)
    print *,'... listo!'
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
        call odeint(y,nvar,t,t+dt,eps,dt,0.d0,nok,nbad,derivs,bsstep)    
        avgtc = y(1)
        avgtm = y(2)
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
!                    1   2     3   4    5      6      7       8     9     10          11
        write(11,*) t,avgtc,avgtm,dlit,a/a0,lod/lod0,ric/1.d3,ur,urtot,dum/1000.d0,dlm/1000.d0, &
!             12           13          14           15           16            17           18
             qcmb*1.d-12,qconv*1.d-12,qmelt*1.d-12,qradm*1.d-12,qradc*1.d-12,qtidal*1.d-12,visc(avgtm), &
!              19  20 
             tcmb,meltf
        write(*,12,advance='no') cr,t*100.d0/tf,'%'
        t = t + dt
    end do
    print *,' '
    print *,'... listo!'
    print *,' '
    print *,'==================='
    print *,'   estado final:   '
    print *,'==================='
    print 13,'    t_c = ',avgtc,'k'
    print 13,'  t_cmb = ',etac*avgtc,'k'
    print 13,'    t_m = ',avgtm,'k'
    print 13,'  t_ubl = ',etaum*avgtm,'k'
    print 13,'   r_ic = ',ric/1000.d0,'km'
    print 13,'    lit = ',dlit,'km'
    print 13,'  q_cmb = ',qcmb*1.d-12,'tw'
    print 13,' q_conv = ',qconv*1.d-12,'tw'
    print 13,' q_melt = ',qmelt*1.d-12,'tw'
    print 13,'q_rad,m = ',qradm*1.d-12,'tw'
    print 13,'q_rad,c = ',qradc*1.d-12,'tw'
    print 13,' ur_tot = ',urtot,'nd'
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