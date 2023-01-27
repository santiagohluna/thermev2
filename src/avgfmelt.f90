program avg_fmelt

    use thermev2_subs

    implicit none

    real(kind=dp) :: avgTc,avgTm,Tcmb,Tubl,Tlbl,DTlbl,DTubl,dubl,dlbl, &
                     St,err,damfdT
    real(kind=dp), parameter :: h = 10.d0
    
    open(unit=20,file='../out/avgfmelt.out',status='unknown')

    Tsup = 300.d0
    avgTc = 6000.d0
    avgTm = 2350.d0

    Tcmb = avgTc/epsc
    Tubl = avgTm/epsm

    do while(avgTm.le.3000.d0)
        Tubl = avgTm/epsm

        call capas_limites(Tcmb,Tubl,Tlbl,DTubl,DTlbl,dubl,dlbl)

        St = stefan(Tubl,dubl)
        damfdT = dfridr(avgfmelt,Tcmb,Tubl,h,err)

        write(20,*) avgTm,Tubl,avgfmelt(Tcmb,Tubl),(Lmelt/cm)*damfdT,St

        avgTm = avgTm + 10.d0
    end do

end program avg_fmelt