!==============================================================================
    program prueba_init
        
        use thermev2_subs
        
        implicit none

        integer :: demid,tidefl,radcfl,radmfl
        real(kind=dp) :: tc0,tm0,DTcmb,DTlbl0,dtprint,Tcmb0,Tubl0,tprint
        real(kind=dp) :: Tcmb,Ric,Pio
        real(kind=dp), parameter :: dT = 1.d-2
        real(kind=dp), parameter :: dr = 1.d3
        real(kind=dp), parameter :: Ric0 = 1221.d3
        logical, dimension(2) :: linit

        print *,'Leyendo archivo de entrada...'
        call leer_entrada(tc0,tm0,DTcmb,DTlbl0,linit,demid,tidefl, &
        radcfl,radmfl,dtprint)
        print *,'... listo!'

        print *,' '

        print *,'Incializando variables...'
        call inicializar_temperaturas(linit,tc0,tm0,DTcmb,DTlbl0,Tcmb0,Tubl0)
        print *,'... listo!'

        print *,' '

10      format(a11,1x,f7.2)

        print 10,'T_cmb (0) = ',Tcmb0
        print 10,'T_ubl (0) = ',Tubl0

        Tcmb = Tcmb0
        Ric = 0.d0

        do while ((Ric-Ric0+dr)*(Ric0-Ric+dr).lt.0.d0)

            lRic = fPio(Pcmb,Tcmb,x0)*fPio(Pec,Tcmb,x0).lt.0.d0
            if(lRic) then 
                Pio = rtbis(fPio,Tcmb,x0,Pcmb,Pec,1.d-6)
                Ric = dsqrt(2.d0*(Pec-Pio)*1.d9*Rc/(rhoc*glm))
            else
                Ric = 0.d0
            end if

            Tcmb = Tcmb - dT

        end do

        print *,' '

        DTcmb = Tcmb - Tcmb0
        print 10,'DTcmb = ',DTcmb
        print 10,'R_ic = ',Ric*1.d-3

        print *,' '

        print *,'Incializando variables...'
        call inicializar_temperaturas(linit,tc0,tm0,DTcmb,DTlbl0,Tcmb0,Tubl0)
        print *,'... listo!'

        print 10,'T_cmb (0) = ',Tcmb
        print 10,'T_ubl (0) = ',Tubl0

        print *,' '

        tprint = 0.d0

        print *,'Imprimiendo perfil de temperatura inicial...'
        call imprimir_perfil(tprint,Tcmb,Tubl0)
        print *,'... listo!'

        print *,' '
        
    end program prueba_init
!==============================================================================