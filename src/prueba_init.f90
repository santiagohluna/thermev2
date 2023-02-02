!==============================================================================
    program prueba_init
        
        use thermev2_subs
        
        implicit none

        integer :: demid,tidefl,radcfl,radmfl
        real(kind=dp) :: tc0,tm0,DTcmb,DTlbl0,dtprint,Tcmb0,Tubl0
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

        print *,'T_cmb (0) = ',Tcmb0
        print *,'T_ubl (0) = ',Tubl0
        
    end program prueba_init
!==============================================================================