program prueba_st

    use thermev2_subs

    implicit none

    real(kind=dp) :: Tubl,avgTc,avgTm,Tcmb,Tlbl,DTubl,DTlbl, &
                     dubl,dlbl,r1,r2,Tcmb0,St
    real(kind=dp), parameter :: km = 1.d-3
    real(kind=dp), parameter :: tprint = 0.d0
    real(kind=dp), parameter :: h = 10.d0

!   --------------------------------------------------------------------
!   calculo del numero de stefan
!   --------------------------------------------------------------------
10  format(a11,1x,f7.2)

    Tsup = 300.d0
    avgTc = 6000.d0
    avgTm = 3000.d0

    print 10,'Tc =',avgTc
    print 10,'Tm =',avgTm

    Tcmb = avgTc/epsc
    Tubl = avgTm/epsm

    Tcmb0 = xi(x0)*(1.d0 + (tlc1 + tlc2*pec)*pec)/(1.d0 + (ta1 + ta2*pec)*pec)

    call capas_limites(Tcmb,Tubl,Tlbl,DTubl,DTlbl,dubl,dlbl)

    print *,' '
    print 10,'Tcmb0 =',Tcmb0
    print 10,'Tcmb =',Tcmb
    print 10,'Tc0 =',epsc*Tcmb0
    print 10,'Tubl =',Tubl
    print 10,'Tlbl =',Tlbl
    print 10,'Tsol =',tsol(Rt-dubl)
    print *,' '
    print 10,'DTubl =',DTubl
    print 10,'DTlbl =',DTlbl
    print *,' '
    print 10,'dubl =',dubl*km
    print 10,'dlbl =',dlbl*km
    print *,' '

!    call imprimir_perfil(tprint,Tcmb,Tubl)
    
!    if ((Tubl-tsol(Rt-dubl))*(tliq(Rt-dubl)-Tubl).gt.0.d0) then 

        r1 = Rast(Tubl,dubl)
        r2 = Rlit(Tubl,dubl)

        print 10,'Rast =',r1*km
        print 10,'Rlit =',r2*km
        print 10,'dlit =',(Rt - r2)*km
        print 10,'Rt - dubl =',(Rt - dubl)*km
        print *,' '
        print 10,'avg(fmelt) =',avgfmelt(Tcmb,Tubl)

        St = stefan(Tubl,dubl)

        print *,'St = ',St
        print *,' '

!    end if

end program prueba_st