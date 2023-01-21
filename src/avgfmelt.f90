program avg_fmelt

    use thermev2_subs

    implicit none

    real(kind=dp) :: avgTc,avgTm,Tcmb,Tubl,Tlbl,DTlbl,DTubl,dubl,dlbl
    
    open(unit=20,file='../out/avgfmelt.out',status='unknown')

    Tsup = 300.d0
    avgTc = 6000.d0
    avgTm = 2350.d0

    Tcmb = avgTc/epsc

    do while(avgTm.le.3000.d0)
        Tubl = avgTm/epsm

        call capas_limites(Tcmb,Tubl,Tlbl,DTubl,DTlbl,dubl,dlbl)

        St = stefan(Tcmb,Tubl)

        write(20,*) avgTm,Tubl,3.d0*avgfmelt(Tcmb,Tubl)/denVm,St

        avgTm = avgTm + 10.d0
    end do

end program avg_fmelt