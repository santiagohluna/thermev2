!=======================================================================
MODULE THERMEV2_SUBS
!   --------------------------------------------------------------------
!   BLOQUE DE DECLARACIÓN
!   --------------------------------------------------------------------
    IMPLICIT NONE
!   --------------------------------------------------------------------
    INTEGER, PARAMETER :: DP = 8
!   --------------------------------------------------------------------
!   PARAMETROS GENERALES
!   --------------------------------------------------------------------
    REAL (KIND=DP), PARAMETER ::   PI = 4.D0*DATAN(1.D0)
    REAL (KIND=DP), PARAMETER ::  CGU = 6.67408D-11
    REAL (KIND=DP), PARAMETER ::   MT = 5.9722D24
    REAL (KIND=DP), PARAMETER ::   RT = 6.371D6
    REAL (KIND=DP), PARAMETER ::   RC = 3.480D6
    REAL (KIND=DP), PARAMETER ::   ML = 7.342D22
    REAL (KIND=DP), PARAMETER ::   MU = CGU*(MT+ML)
    REAL (KIND=DP), PARAMETER :: RHOT = 5515.D0
    REAL (KIND=DP), PARAMETER ::   DD = 86400.D0
    REAL (KIND=DP), PARAMETER ::   AA = 365.25D0*DD
    REAL (KIND=DP), PARAMETER ::   GA = AA*1.D9
    REAL (KIND=DP), PARAMETER ::   T0 = 4.5D0
    REAL (KIND=DP), PARAMETER ::   TF = 4.5D0
    REAL (KIND=DP), PARAMETER ::   DR = 1.D2
!   --------------------------------------------------------------------
!   PARAMETROS DEL MODELO DE DRISCOLL & BERCOVICI (2014)
!   TABLA 3
!   --------------------------------------------------------------------
    REAL (KIND=DP), PARAMETER ::     AT = 4.D0*PI*RT**2 ! EARTH SURFACE AREA
    REAL (KIND=DP), PARAMETER ::     AC = 4.D0*PI*RC**2 ! CORE SURFACE AREA
    REAL (KIND=DP), PARAMETER ::  ALFAM = 3.D-5         ! THERMAL EXPANSIVITY OF MANTLE
    REAL (KIND=DP), PARAMETER ::  ALFAC = 1.D-5         ! THERMAL EXPANSIVITY OF CORE
    REAL (KIND=DP), PARAMETER ::   BETA = 1.D0/3.D0     ! THERMAL BOUNDARY LAYER EXPONENT
    REAL (KIND=DP), PARAMETER ::     CM = 1265.D0       ! SPECIFIC HEAT OF MANTLE
    REAL (KIND=DP), PARAMETER ::     CC = 840.D0        ! SPECIFIC HEAT OF CORE
    REAL (KIND=DP), PARAMETER ::     DM = 2891.D3       ! MANTLE DEPTH
    REAL (KIND=DP), PARAMETER ::    DFE = 7000.D3       ! IRON SOLIDUS LENGTH SCALE
    REAL (KIND=DP), PARAMETER ::     DN = 6340.D3       ! CORE ADIABATIC LENGTH SCALE
    REAL (KIND=DP), PARAMETER :: DVLDTM = 8.D17         ! LIQUID VOLUME GRADIENT
    REAL (KIND=DP), PARAMETER ::     EG = 3.D5          ! GRAVITATIONAL ENERGY DENSITY RELEASE AT ICB
    REAL (KIND=DP), PARAMETER ::  ERUPT = 0.2D0         ! EFFICIENCY OF MAGMA ERUPTION TO THE SURFACE
    REAL (KIND=DP), PARAMETER ::  ETAUM = 0.7D0         ! UPPER MANTLE ADIABATIC TEMPERATURE DROP
    REAL (KIND=DP), PARAMETER ::  ETALM = 1.3D0         ! LOWER MANTLE ADIABATIC TEMPERATURE JUMP
    REAL (KIND=DP), PARAMETER ::   ETAC = 0.8D0         ! UPPER CORE ADIABATIC TEMPERATURE DROP
    REAL (KIND=DP), PARAMETER ::  FVOL0 = 1.D-3         ! REFERENCE VOLUMETRIC MELT FRACTION
    REAL (KIND=DP), PARAMETER ::    GUM = 9.8D0         ! UPPER MANTLE GRAVITY
    REAL (KIND=DP), PARAMETER ::    GLM = 10.5D0        ! LOWER MANTLE GRAVITY
    REAL (KIND=DP), PARAMETER ::     GC = 10.5D0        ! CMB GRAVITY
    REAL (KIND=DP), PARAMETER :: GAMMAD = 1.D-3         ! MAGMA ADIABATIC GRADIENT
    REAL (KIND=DP), PARAMETER ::   GRUN = 1.3D0         ! CORE GRUNEISEN PARAMETER
    REAL (KIND=DP), PARAMETER ::  GAMMZ = 3.9D-3        ! MANTLE SOLIDUS GRADIENT
    REAL (KIND=DP), PARAMETER ::    KUM = 4.2D0         ! UPPER MANTLE THERMAL CONDUCTIVITY
    REAL (KIND=DP), PARAMETER ::    KLM = 10.D0         ! LOWER MANTLE THERMAL CONDUCTIVITY
    REAL (KIND=DP), PARAMETER ::   KAPM = 1.D-6         ! MANTLE THERMAL DIFFUSIVITY
    REAL (KIND=DP), PARAMETER ::    LFE = 750.D3        ! LATENT HEAT OF INNER CORE CRYSTALLIZATION
    REAL (KIND=DP), PARAMETER ::  LMELT = 320.D3        ! LATENT HEAT OF MANTLE MELTING
    REAL (KIND=DP), PARAMETER ::     MM = 4.06D24       ! MANTLE MASS
    REAL (KIND=DP), PARAMETER ::     MC = 1.95D24       ! CORE MASS
    REAL (KIND=DP), PARAMETER ::  FVISC = 3.D0          ! VISCOSITY JUMP FROM UPPER TO LOWER MANTLE
    REAL (KIND=DP), PARAMETER ::  VMVUM = 10.D0         ! VISCOSITY JUMP FROM UPPER TO MID-MANTLE
    REAL (KIND=DP), PARAMETER ::  QRAD0 = 13.D12        ! PRESENT-DAY MANTLE RADIOGENIC HEAT FLOW
    REAL (KIND=DP), PARAMETER :: QRAD0C = 2.D12         ! PRESENT-DAY MANTLE RADIOGENIC HEAT FLOW
    REAL (KIND=DP), PARAMETER ::     RM = 4.925D6       ! RADIUS TO AVERAGE MANTLE TEMPERATURE
    REAL (KIND=DP), PARAMETER ::   RACR = 660.D0        ! CRITICAL RAYLEIGH NUMBER
    REAL (KIND=DP), PARAMETER :: RACRCMB = 2000.D0      ! CRITICAL RAYLEIGH NUMBER AT THE CMB
    REAL (KIND=DP), PARAMETER ::   RHOC = 11900.D0      ! CORE DENSITY
    REAL (KIND=DP), PARAMETER ::  RHOIC = 13000.D0      ! INNER CORE DENSITY
    REAL (KIND=DP), PARAMETER ::   RHOM = 4800.D0       ! MANTLE DENSITY
    REAL (KIND=DP), PARAMETER :: RHOMEL = 2700.D0       ! MANTLE MELT DENSITY
    REAL (KIND=DP), PARAMETER :: RHOSOL = 3300.D0       ! MANTLE UPWELLING SOLID DENSITY
    REAL (KIND=DP), PARAMETER ::   TFE0 = 5600.D0       ! IRON SOLIDUS COEFFICIENT
    REAL (KIND=DP), PARAMETER ::  TSOL0 = 1244.D0       ! MANTLE SOLIDUS AT SURFACE
    REAL (KIND=DP), PARAMETER :: TAURAD = 2.94D0        ! MANTLE RADIOACTIVE DECAY TIME SCALE
    REAL (KIND=DP), PARAMETER :: TAURDC = 1.2D0         ! CORE RADIOACTIVE DECAY TIME SCALE
    REAL (KIND=DP), PARAMETER :: PHIDIS = 0.6D0         ! DISGREGATION POINT
    REAL (KIND=DP), PARAMETER :: DENRIC = 2.D0*(1.D0 - 1.D0/(3.D0*GRUN))*(DN/DFE)**2 - 1.D0
!   --------------------------------------------------------------------
!   PARAMETROS DE INTEGRACION
!   --------------------------------------------------------------------
    REAL (KIND=DP) :: TC0,TM0
!   --------------------------------------------------------------------
!   PARAMETROS DE LOS MODELOS DINAMICOS
!   --------------------------------------------------------------------
!   DEMID = 1
!   ---------
    REAL (KIND=DP), PARAMETER, DIMENSION(4) :: CA = (/ -0.0731737, 0.270575, -0.387178, 3.844 /)
    REAL (KIND=DP), PARAMETER, DIMENSION(4) :: CLOD = (/ -0.586687, 3.41316, -7.4227, 23.93 /)
!   --------------------------------------------------------------------
!   DEMID = 2
!   ---------
    REAL (KIND=DP), PARAMETER ::    A0 = 3.84402D8
    REAL (KIND=DP), PARAMETER :: DADT0 = 3.82D-2
    REAL (KIND=DP), PARAMETER :: LODI1 = 6.15D0
    REAL (KIND=DP), PARAMETER ::  LODF = 23.93
    REAL (KIND=DP), PARAMETER ::   MA1 = DADT0*GA/AA
    REAL (KIND=DP), PARAMETER :: MLOD1 = (LODF-LODI1)/4.5D0
!   --------------------------------------------------------------------
!   DEMID = 3
!   ---------
    REAL (KIND=DP), PARAMETER ::    AI = 20.D0*RT
    REAL (KIND=DP), PARAMETER ::    AF = A0
    REAL (KIND=DP), PARAMETER :: LODI2 = 2.53D0
    REAL (KIND=DP), PARAMETER ::   MA2 = (AF-AI)/4.5D0
    REAL (KIND=DP), PARAMETER :: MLOD2 = (LODF-LODI2)/4.5D0
!   --------------------------------------------------------------------
!   PARAMETROS DE ELEMENTOS RADIOGENICOS
!   TABLA 4.2 DE TURCOTTE & SCHUBERT (2014)
!   --------------------------------------------------------------------
    REAL (KIND=DP), PARAMETER, DIMENSION(4) :: HI = (/ 9.46D-5, 5.69D-4, 2.64D-5, 2.92D-5 /)
    REAL (KIND=DP), PARAMETER, DIMENSION(4) :: TAU = (/ 4.47D9*AA/GA, 7.04D8*AA/GA, 1.40D10*AA/GA, 1.25D9*AA/GA /)
    REAL (KIND=DP), PARAMETER, DIMENSION(4) :: LAM = (/ DLOG(2.D0)/TAU(1), DLOG(2.D0)/TAU(2), & 
                                                        LOG(2.D0)/TAU(3), DLOG(2.D0)/TAU(4) /)
    REAL (KIND=DP), PARAMETER, DIMENSION(4) :: C0 = (/ 31.D-9, 31.D-9, 124.D-9, 31.D-5 /)
    REAL (KIND=DP), PARAMETER, DIMENSION(4) :: FC = (/ 0.9928D0, 0.0071D0, 1.D0, 1.19D-4 /)
!   --------------------------------------------------------------------
!   PARAMETROS REOLOGICOS
!   --------------------------------------------------------------------
    REAL (KIND=DP), PARAMETER ::  RIGDZ = 8.0E10
    REAL (KIND=DP), PARAMETER ::   FLEX = 1.D0/RIGDZ
    REAL (KIND=DP), PARAMETER ::  KSUBB = RT/(CGU*MT*RHOT)
    REAL (KIND=DP), PARAMETER ::  KFLEX = 0.2D0
    REAL (KIND=DP), PARAMETER ::   KETA = 0.02D0
    REAL (KIND=DP), PARAMETER ::  ALPHA = 0.2
    REAL (KIND=DP), PARAMETER :: ZANDR0 = 1.D0
    REAL (KIND=DP) :: GAMALF,GAMMAC,GAMMAS
    REAL (KIND=DP), PARAMETER ::     K2 = 1.D0
    REAL (KIND=DP), PARAMETER ::     QF = 1.D0
    REAL (KIND=DP), PARAMETER :: DELTAT = 0.D0
    REAL (KIND=DP), PARAMETER :: EPSMAY = 0.D0
!   --------------------------------------------------------------------
    LOGICAL LDEM1,LDEM2,LDEM3,LTIDE,LRADC,LRADM,LTHERM,LSTRUC
!   --------------------------------------------------------------------
    INTEGER, PARAMETER :: LMAXP = 3,QMAXP = 10
    REAL (KIND=DP) :: FFI(LMAXP,0:LMAXP,0:LMAXP), &
                      GGE(LMAXP,0:LMAXP,-QMAXP:QMAXP),ALM(LMAXP,0:LMAXP)
!   --------------------------------------------------------------------
    CHARACTER (LEN=50) ALGO
!   --------------------------------------------------------------------
    REAL (KIND=DP) :: TSUP,E,I,DTPRINT,RAST,RLIT
    INTEGER :: IDREO,DEMID,LMAX,QMAX,TIDEFL,RADCFL,RADMFL,THERMFL, &
               STRUCFL,NTERMS
!   --------------------------------------------------------------------
    REAL (KIND=DP), ALLOCATABLE :: ASUMA(:)
!   --------------------------------------------------------------------
!   BLOQUE DE PROCEDIMIENTOS
!   --------------------------------------------------------------------

    CONTAINS

!=======================================================================
    SUBROUTINE DERIVS(T,Y,DYDT)
!   --------------------------------------------------------------------
    IMPLICIT NONE
!   --------------------------------------------------------------------
    INTEGER :: K
    REAL (KIND=DP),INTENT(IN) :: T,Y(2)
    REAL (KIND=DP),INTENT(OUT) :: DYDT(2)
    REAL (KIND=DP) :: AVGTC,AVGTM,DTUBL,TUBL,RIC,DRICDT,DLBL,DUBL,AIC, &
                      DTLBL,DTMELT,MMELTP,QCMB,QCONV,QMELT,QRADM, &
                      QRADC,TCMB,TLBL,VM,MELTFM,DVUPDT,QTIDAL, &
                      A,LOD,UR,URTOT,RADIC,NUM,DELT,A1,A2,INT,ETAVG, &
                      RA,ST,TSOLM,TLIQM,TMELT,ZMELT,ZUM
    REAL (KIND=DP) :: ASUMAQ(4)
    COMMON /PRINTOUT/ A,LOD,DUBL,DLBL,UR,URTOT,QCMB,QCONV,QMELT, &
                      QRADM,QRADC,QTIDAL,VM,RIC,NUM,TCMB,MELTFM
!   --------------------------------------------------------------------
    AVGTC = Y(1)
    AVGTM = Y(2)
!   --------------------------------------------------------------------
!   CALCULO DE TCMB, TUBL Y TLBL
!   --------------------------------------------------------------------
    TCMB = ETAC*AVGTC
    TLBL = ETALM*AVGTM
    TUBL = ETAUM*AVGTM
!   --------------------------------------------------------------------
!   CALCULO DE LOS SALTOS DE TEMPERATURA
!   --------------------------------------------------------------------
    DTLBL = TCMB - TLBL
    DTUBL = TUBL - TSUP
!   --------------------------------------------------------------------
!   CALCULO DE QCMB
!   --------------------------------------------------------------------
    DLBL = (RACRCMB*KAPM*VISC(0.5D0*(TLBL + TCMB))/(GLM*ALPHA*DTLBL))**(1.D0/3.D0)
    QCMB = AC*KLM*DTLBL/DLBL
!   --------------------------------------------------------------------
!   CALCULO DE QCONV
!   --------------------------------------------------------------------    
    RA = GUM*ALPHA*(DTUBL + DTLBL)*(RT - RC)**3/(KAPM*VISC(TUBL))
    DUBL = (RT - RC)*(RACR/RA)**BETA
    QCONV = AT*KUM*ETAUM*DTUBL/DUBL
!   --------------------------------------------------------------------
!   CALCULO DEL NUMERO DE STEFAN
!   --------------------------------------------------------------------
    ST = STEFAN(TUBL,DUBL)
!   PRINT '(A4,1X,F7.2)','ST =',ST
!   --------------------------------------------------------------------
!   CALCULO DE QMELT
!   --------------------------------------------------------------------
    DVUPDT = 1.16D0*KAPM*AT/DUBL
    TMELT = 0.5D0*(TUBL + TSOL0)
    ZMELT = 0.5D0*(TUBL-TSOL0)/GAMMZ
    DTMELT = TMELT - TSUP - ZMELT*GAMMAD
    ZUM = RT - DUBL
    MMELTP = DVUPDT*RHOSOL*FMELT(ZUM,DUBL,TUBL)
    QMELT = ERUPT*MMELTP*(LMELT + CM*DTMELT)
!   --------------------------------------------------------------------
!   CALCULO DE QRADM
!   --------------------------------------------------------------------
    IF (LRADM) THEN
        DO K=1,4
            ASUMAQ(K) = MM*FC(K)*C0(K)*HI(K)*DEXP(LAM(K)*(T0-T))
        END DO
        QRADM = SUMAR(4,ASUMAQ)
    ELSE
        QRADM = 0.D0
    END IF
!   --------------------------------------------------------------------
!   CALCULO DE QRADC
!   --------------------------------------------------------------------
    IF (LRADC) THEN
        QRADC = QRAD0C*DEXP((T0-T)/TAURDC)
    ELSE
        QRADC = 0.D0
    END IF
!   --------------------------------------------------------------------
!   CALCULO DE QTIDAL
!   --------------------------------------------------------------------
    CALL MODELO_DINAMICO(T,A,LOD)
    IF (LTIDE) THEN
        A1 = TLBL
        A2 = TUBL
        DELT = TUBL - TLBL
        CALL QROMB(VISC,A1,A2,INT)
        ETAVG = RHOM*INT/DELT
        QTIDAL = PM(ETAVG,TUBL,DUBL,A,LOD)
    ELSE
        QTIDAL = 0.D0
    END IF
!   --------------------------------------------------------------------
!   CALCULO DEL RADIO DEL NUCLEO INTERNO
!   --------------------------------------------------------------------
    NUM = DLOG(TFE0/TCMB)*(DN/RC)**2 - 1.D0
    IF (NUM.GE.0.D0) THEN
        RADIC = NUM/DENRIC
        RIC = RC*DSQRT(RADIC)
    ELSE 
        RIC = 0.D0
    END IF
!   --------------------------------------------------------------------
!   CALCULO DEL AREA DEL NUCLEO INTERNO
!   --------------------------------------------------------------------
    AIC = 4.D0*PI*RIC**2
!   --------------------------------------------------------------------
!   CALCULO DE DR_IC / DT_CMB
!   --------------------------------------------------------------------
    DRICDT = - (DN**2)/(2.D0*RC*TCMB*NUM)
!   --------------------------------------------------------------------
!   CALCULO DE LAS RAZONES DE UREY
!   --------------------------------------------------------------------
       UR = QRADM/QCONV
    URTOT = QRADM/(QCONV+QMELT)
!   --------------------------------------------------------------------
    IF (LSTRUC) THEN
        DYDT(1) = 0.D0
    ELSE
        DYDT(1) = (QRADC - QCMB)*GA/(MC*CC - AIC*RHOIC*ETAC*DRICDT*(LFE+EG))
    END IF
    IF (LTHERM) THEN
        DYDT(2) = (QCMB + QRADM - QCONV - QMELT)*GA/(MM*CM)
    ELSE
        DYDT(2) = (QCMB + QRADM + QTIDAL - QCONV - QMELT)*GA/ &
                  (MM*CM*(1.D0 + ST))
    END IF
!   --------------------------------------------------------------------
    END SUBROUTINE DERIVS
!=======================================================================
    FUNCTION PM(ETA,TUBL,DUBL,A,LOD)
!   --------------------------------------------------------------------
!   ESTA FUNCIÓN CALCULA EL CALOR GENERADO POR INTERACCIÓN DE MAREAS
!   USANDO LA EXPRESIÓN DERIVADA POR EFROIMSKY Y MAKAROV (2014)
!   --------------------------------------------------------------------
    IMPLICIT NONE
!   --------------------------------------------------------------------
    INTEGER :: J,L,M,P,Q
    REAL (KIND=DP),INTENT(IN) :: ETA,TUBL,A,LOD,DUBL
    REAL (KIND=DP) :: N,THP,RSA,WLMPQ,XLMPQ,SUMAPM,FTVF,RSAL
    REAL (KIND=DP) :: RPHI,PM,KR,KI
!   --------------------------------------------------------------------
!   CALCULO DE LA FRACCION DE VOLUMEN DEL MANTO ACTIVO PARA LA 
!   INTERACCION DE MAREAS
!   --------------------------------------------------------------------
    RPHI = RAST
    DO WHILE (FMELT(RPHI,DUBL,TUBL).LT.PHIDIS)
        RPHI = RPHI + DR
    END DO
    FTVF = (RPHI/RT)**3-(RC/RT)**3
!   --------------------------------------------------------------------
    RSA = RT/A
      N = DSQRT(MU/A)/A
    THP = 2.D0*PI/(LOD*3600.D0)
!   --------------------------------------------------------------------
!   CALCULO DE LA TASA DE PRODUCCION DE CALOR POR MAREAS
!   --------------------------------------------------------------------
    J = 1
    DO L=2,LMAX
        RSAL = RSA**(2*L+1)
        DO M = 0,L
            DO P = 0,L
                DO Q = -QMAX,QMAX
                    WLMPQ = DBLE(L-2*P+Q)*N - DBLE(M)*THP
                    XLMPQ = DABS(WLMPQ)
                    CALL REOLOGIA(L,WLMPQ,IDREO,ETA,KR,KI)
                    ASUMA(J) = RSAL*ALM(L,M)*FFI(L,M,P)*GGE(L,P,Q)*WLMPQ*KI*DSIGN(1.D0,WLMPQ)
                    J = J + 1
                END DO
            END DO
        END DO
    END DO
    SUMAPM = SUMAR(J-1,ASUMA)
    PM = FTVF*((CGU*ML)*(ML/A))*SUMAPM
!   --------------------------------------------------------------------
    END FUNCTION PM
!=======================================================================
    SUBROUTINE MODELO_DINAMICO(T,A,LOD)
!   --------------------------------------------------------------------
!   CALCULO DE LOS VALORES DE A Y DE LOD
!   --------------------------------------------------------------------
    IMPLICIT NONE
!   --------------------------------------------------------------------
    REAL (KIND=DP), INTENT(IN) :: T
    REAL (KIND=DP), INTENT(OUT) :: A,LOD
!   --------------------------------------------------------------------
     IF (LDEM1) THEN
!     -------------------------------------------------------------------
        A = (T0-T)*((T0-T)*(CA(1)*(T0-T) + CA(2)) + CA(3)) + CA(4)
        A = A*1.D8
      LOD = (T0-T)*((T0-T)*(CLOD(1)*(T0-T) + CLOD(2)) + CLOD(3)) + CLOD(4)
!     ------------------------------------------------------------------
     ELSE IF (LDEM2) THEN
!    -------------------------------------------------------------------
        A = MA1*(T-T0) + A0
      LOD = MLOD1*(T-T0) + LODF
!    -------------------------------------------------------------------
     ELSE IF (LDEM3) THEN
!    -------------------------------------------------------------------
        A = MA2*(T-T0) + A0
      LOD = MLOD2*(T-T0) + LODF
!    -------------------------------------------------------------------
     ELSE
        PRINT *,'ERROR EN EL IDENTIFICADOR DEL MODELO DINAMICO'
        RETURN
     END IF
!   --------------------------------------------------------------------
    END SUBROUTINE MODELO_DINAMICO
!=======================================================================
    SUBROUTINE LEER_ENTRADA()
!   --------------------------------------------------------------------
    IMPLICIT NONE
!   --------------------------------------------------------------------
!   ARCHIVO DE ENTRADA
!   --------------------------------------------------------------------
    OPEN(UNIT=10,FILE='../entrada/THERMEV2DB.IN',STATUS='UNKNOWN')
!   --------------------------------------------------------------------
!     INICIALIZACION DE VARIABLES
!   --------------------------------------------------------------------
    READ(10,*) ALGO
    READ(10,*) TSUP,TC0,TM0
    READ(10,*) ALGO
    READ(10,*) E,I
    READ(10,*) ALGO
    READ(10,*) TIDEFL,RADCFL,RADMFL
    READ(10,*) ALGO
    READ(10,*) THERMFL
    READ(10,*) ALGO
    READ(10,*) STRUCFL
    READ(10,*) ALGO
    READ(10,*) IDREO
    READ(10,*) ALGO
    READ(10,*) DEMID
    READ(10,*) ALGO
    READ(10,*) LMAX,QMAX
    READ(10,*) ALGO
    READ(10,*) DTPRINT
!   --------------------------------------------------------------------
    LTIDE = TIDEFL.EQ.1
    LRADC = RADCFL.EQ.1
    LRADM = RADMFL.EQ.1
    LTHERM = THERMFL.EQ.1
    LSTRUC = STRUCFL.EQ.1
    LDEM1 = DEMID.EQ.1
    LDEM2 = DEMID.EQ.2
    LDEM3 = DEMID.EQ.3
    I = I*PI/180.D0
!   --------------------------------------------------------------------
    CLOSE(UNIT=10)
!   --------------------------------------------------------------------
    END SUBROUTINE LEER_ENTRADA
!=======================================================================
    SUBROUTINE TIMESTAMP(CHANIO,CHMES,CHDIA,CHHORA,CHMINS,CHSEGS)
!   --------------------------------------------------------------------
!   ESTA SUBRUTINA IMPRIME EL AÑO, MES, DIA, HORA, MINUTOS Y SEGUNDOS
!   COMO CARACTERES PARA INCLUIRLOS EN LOS NOMBRES DEL ARCHIVO DE SALIDA
!   Y DEL PERFIL DE TEMPERATURAS.
!   --------------------------------------------------------------------
        IMPLICIT NONE
!       ----------------------------------------------------------------
        INTEGER :: TVALS(8),ANIO,MES,DIA,HORA,MINS,SEGS
        CHARACTER (LEN=4), INTENT(OUT) :: CHANIO,CHMES,CHDIA,CHHORA, &
                                          CHMINS,CHSEGS
!       ---------------------------------------------------------------
        CALL DATE_AND_TIME(VALUES=TVALS)
        ANIO = TVALS(1)
         MES = TVALS(2)
         DIA = TVALS(3)
        HORA = TVALS(5)
        MINS = TVALS(6)
        SEGS = TVALS(7)
!       ---------------------------------------------------------------
        WRITE(CHANIO,'(I4)') ANIO
!       ---------------------------------------------------------------
        IF (MES.LT.10) THEN
            WRITE(CHMES,'(I1)') MES
            CHMES = TRIM('0'//CHMES)
        ELSE
            WRITE(CHMES,'(I2)') MES
        END IF
!       ---------------------------------------------------------------
        IF (DIA.LT.10) THEN
            WRITE(CHDIA,'(I1)') DIA
            CHDIA = TRIM('0'//CHDIA)
        ELSE
            WRITE(CHDIA,'(I2)') DIA
        END IF
!       ---------------------------------------------------------------
        IF (HORA.LT.10) THEN
            WRITE(CHHORA,'(I1)') HORA
            CHHORA = TRIM('0'//CHHORA)
        ELSE
            WRITE(CHHORA,'(I2)') HORA
        END IF
!       ---------------------------------------------------------------
        IF (MINS.LT.10) THEN
            WRITE(CHMINS,'(I1)') MINS
            CHMINS = TRIM('0'//CHMINS)
        ELSE
        WRITE(CHMINS,'(I2)') MINS
        END IF
!       ---------------------------------------------------------------
        IF (SEGS.LT.10) THEN
            WRITE(CHSEGS,'(I1)') SEGS
            CHSEGS = TRIM('0'//CHSEGS)
        ELSE
            WRITE(CHSEGS,'(I2)') SEGS
        END IF
!   --------------------------------------------------------------------
    END SUBROUTINE TIMESTAMP
!=======================================================================
    SUBROUTINE CREAR_ARCHIVO_SALIDA()
!   -------------------------------------------------------------------
        IMPLICIT NONE
!       ---------------------------------------------------------------
        CHARACTER (LEN=10) :: REO,CHIDFIT,CHTIDE,CHRADC,CHRADM
        CHARACTER (LEN=4) :: CHANIO,CHMES,CHDIA,CHHORA,CHMINS,CHSEGS
!       ---------------------------------------------------------------
!       ARCHIVO DE SALIDA
!       ---------------------------------------------------------------
        CALL TIMESTAMP(CHANIO,CHMES,CHDIA,CHHORA,CHMINS,CHSEGS)
!       ----------------------------------------------------------------
        IF (IDREO.EQ.1) THEN
            REO = 'ELAST'
        ELSE IF (IDREO.EQ.2) THEN
            REO = 'VISC'
        ELSE IF (IDREO.EQ.3) THEN
            REO = 'MAX'
        ELSE IF (IDREO.EQ.4) THEN
            REO = 'BUR'
        ELSE IF (IDREO.EQ.5) THEN
            REO = 'AND'
        ELSE IF (IDREO.EQ.6) THEN
            REO = 'QCONST'
        ELSE IF (IDREO.EQ.7) THEN
            REO = 'DTCONST'
        ELSE IF (IDREO.EQ.8) THEN
            REO = 'E&L2007'
        END IF
        WRITE(CHIDFIT,'(I1)') DEMID
        WRITE(CHTIDE,'(I1)') TIDEFL
        WRITE(CHRADC,'(I1)') RADCFL
        WRITE(CHRADM,'(I1)') RADMFL
        OPEN(UNIT=11,FILE='../out/THERMEVDB_'//TRIM(REO)//'_DEMID_'// &
            TRIM(CHIDFIT)//'_TIDE_'//TRIM(CHTIDE)//'_RADC_'//TRIM(CHRADC)// &
            '_RADM_'//TRIM(CHRADM)//'_'//TRIM(CHANIO)//'-'//TRIM(CHMES)//'-' &
            //TRIM(CHDIA)//'_'//TRIM(CHHORA)//'_'//TRIM(CHMINS)//'_' &
            //TRIM(CHSEGS)//'.OUT',STATUS='UNKNOWN')
!   --------------------------------------------------------------------
    END SUBROUTINE CREAR_ARCHIVO_SALIDA
!=======================================================================
    SUBROUTINE IMPRIMIR_PERFIL(TPRINT,AVGTC,DLM,AVGTM,DUM)
!   --------------------------------------------------------------------
        IMPLICIT NONE
!       ----------------------------------------------------------------
        REAL (KIND=DP), INTENT(IN) :: TPRINT,AVGTC,DLM,AVGTM,DUM
        CHARACTER (LEN=4) :: CHANIO,CHMES,CHDIA,CHHORA,CHMINS,CHSEGS
        REAL (KIND=DP) :: R,TDR
        CHARACTER (LEN=4) :: CHTPRINT
!       ----------------------------------------------------------------
!       IMPRESIÓN DE LOS PERFILES DE TEMPERATURA A LO LARGO DE LA 
!       INTEGRACIÓN
!       ----------------------------------------------------------------
        CALL TIMESTAMP(CHANIO,CHMES,CHDIA,CHHORA,CHMINS,CHSEGS)
!       ----------------------------------------------------------------
        IF (TPRINT.EQ.0.D0) THEN
            WRITE(CHTPRINT,'(I1)') INT(TPRINT)
        ELSE IF(TPRINT.LT.1.D0) THEN
            WRITE(CHTPRINT,'(I3)') INT(TPRINT*1.D3)
        ELSE
            WRITE(CHTPRINT,'(I4)') INT(TPRINT*1.D3)
        END IF
!       ----------------------------------------------------------------
        OPEN(UNIT=12,FILE='../out/TEMPROFILE'//TRIM(CHTPRINT)//'_'//TRIM(CHANIO)// &
        '-'//TRIM(CHMES)//'-'//TRIM(CHDIA)//'_'//TRIM(CHHORA)//'_'//TRIM(CHMINS)//'_' &
        //TRIM(CHSEGS)//'.OUT',STATUS='UNKNOWN')
!       ----------------------------------------------------------------
        R = 0.D0
        DO WHILE (R.LE.RT)
            TDR = TPROF(AVGTC,AVGTM,DLM,DUM,R)
            WRITE(12,*) R,TDR,VISC(TDR)
            R = R + DR
        END DO
    !   --------------------------------------------------------------------
        CLOSE(12)
    !   --------------------------------------------------------------------
    END SUBROUTINE IMPRIMIR_PERFIL
!=======================================================================
    SUBROUTINE REOLOGIA(L,W,ID,ETA,KR,KI)
!   -------------------------------------------------------------------
    IMPLICIT NONE
!   --------------------------------------------------------------------
    REAL (KIND=DP), INTENT(IN) :: W,ETA
    REAL (KIND=DP), INTENT(OUT) :: KR,KI
    REAL (KIND=DP) :: X,ZETA,BSUBL,FACL,DENKL,NUMKI,NUMKR,XPR,XPI,LAG
    INTEGER,INTENT(IN) :: L,ID
!   --------------------------------------------------------------------
    X = DABS(W)
    ZETA = ZANDR0*(100.D0*DEXP(-X/0.2D0) + 1.D0)
    BSUBL = DBLE(2*L**2+4*L+3)*KSUBB/DBLE(L)
    FACL = 1.5D0/DBLE(L-1)
    IF (ID.EQ.1) THEN
!    PURAMENTE ELASTICO
      KR = FACL*FLEX/(FLEX + BSUBL)
      KI = 0.D0
      RETURN
    ELSE IF (ID.EQ.2) THEN
!      PURAMENTE VISCOSO
      DENKL = (BSUBL*X*ETA)**2 + 1.D0
      NUMKR = 1.D0
      NUMKI = -BSUBL*X*ETA
    ELSE IF (ID.EQ.3) THEN
!      MAXWELL
      DENKL = ((ETA*X)**2)*(FLEX + BSUBL)**2 + 1.D0
      NUMKR = (FLEX + BSUBL)*FLEX*(ETA*X)**2 + 1.D0
      NUMKI = -BSUBL*ETA*X
    ELSE IF (ID.EQ.4) THEN
!      BURGERS
      XPR = FLEX*ETA*X*(1.D0 + KFLEX/((KFLEX*KETA*FLEX*ETA*X)**2 + 1.D0))
      XPI = - (1.D0 + KETA*(KFLEX*FLEX*ETA*X)**2/((KFLEX*KETA*FLEX*ETA*X)**2 + 1.D0))         
      NUMKR = (XPR + BSUBL*ETA*X)*XPR + XPI**2
      NUMKI = BSUBL*ETA*X*XPI
      DENKL = (XPR + BSUBL*ETA*X)**2 + XPI**2             
    ELSE IF (ID.EQ.5) THEN
!     ANDRADE
      XPR = FLEX*ETA*X + ((FLEX*ETA*X)**(1.D0-ALPHA))*GAMMAC/(ZETA**ALPHA)
      XPI = - (1.D0 + ((FLEX*ETA*X)**(1.D0-ALPHA))*GAMMAS/(ZETA**ALPHA))
      NUMKR = (XPR + BSUBL*ETA*X)*XPR + XPI**2
      NUMKI = BSUBL*ETA*X*XPI
      DENKL = (XPR + BSUBL*ETA*X)**2 + XPI**2
    ELSE IF (ID.EQ.6) THEN
!      KAULA (1964)
      KR = 0.D0
      KI = K2/QF
      RETURN
    ELSE IF (ID.EQ.7) THEN
!      SINGER-MIGNARD
      KR = 0.D0
      KI = K2*X*DELTAT
      RETURN
    ELSE IF (ID.EQ.8) THEN
!      EFROIMSKY & LAINEY (2007)
      LAG = EPSMAY*(EPSMAY*X)**(-ALPHA-1.0D0)
      KR = 0.D0
      KI = K2*LAG*X
      RETURN
    ELSE
      PRINT *,'ERROR EN EL IDENTIFICADOR DE LA REOLOGIA',IDREO
      RETURN
    END IF
    KR =  FACL*NUMKR/DENKL
    KI = -FACL*NUMKI/DENKL
!   --------------------------------------------------------------------
    RETURN
!   --------------------------------------------------------------------
    END SUBROUTINE REOLOGIA
!=======================================================================
    SUBROUTINE EVALKLM()
!   --------------------------------------------------------------------
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     EVALUACION DE LAS FUNCIONES K_LM
!     ------------------------------------------------------------------
!     L=2
      ALM(2,0) = 1.0D0
      ALM(2,1) = 0.333333333333333D0
      ALM(2,2) = 0.0833333333333333D0
!     L=3
      ALM(3,0) = 1.0D0
      ALM(3,1) = 0.166666666666667D0
      ALM(3,2) = 0.0166666666666667D0
      ALM(3,3) = 0.00277777777777778D0
!   --------------------------------------------------------------------
    END SUBROUTINE EVALKLM
!=======================================================================
    SUBROUTINE EVALFI(INC,F2I)
!   --------------------------------------------------------------------
      IMPLICIT NONE
!   --------------------------------------------------------------------
      REAL (KIND=DP), INTENT(IN) :: INC
      REAL (KIND=DP) :: CI
      REAL (KIND=DP), INTENT(OUT) :: F2I(LMAXP,0:LMAXP,0:LMAXP)
!   --------------------------------------------------------------------
      CI = DCOS(INC)
!     L=2
      F2I(2,0,0) = 0.140625D0*(-1.0D0 + CI**2)**2
      F2I(2,0,1) = 0.5625D0*(-0.333333333333333D0 + CI**2)**2
      F2I(2,0,2) = 0.140625D0*(-1.0D0 + CI**2)**2
      F2I(2,1,0) = -0.5625D0*(-1.0D0 + CI)*(1.0D0 + CI)**3
      F2I(2,1,1) = 2.25D0*CI**2*(1.0D0 - CI**2)
      F2I(2,1,2) = -0.5625D0*(-1.0D0 + CI)**3*(1.0D0 + CI)
      F2I(2,2,0) = 0.5625D0*(1.0D0 + CI)**4
      F2I(2,2,1) = 2.25D0*(-1.0D0 + CI)**2*(1.0D0 + CI)**2
      F2I(2,2,2) = 0.5625D0*(-1.0D0 + CI)**4
!     L=3
      F2I(3,0,0) = -0.09765625D0*(-1.0D0 + CI**2)**3
      F2I(3,0,1) = 0.87890625D0*(1.0D0 - CI**2)*(-0.2D0 + CI**2)**2
      F2I(3,0,2) = 0.87890625D0*(1.0D0 - CI**2)*(-0.2D0 + CI**2)**2
      F2I(3,0,3) = -0.09765625D0*(-1.0D0 + CI**2)**3
      F2I(3,1,0) = 3.515625D0*(1.0D0 + CI)**4*(0.5D0 - CI + 0.5D0*CI**2) &
      **2/(-1.0D0 + CI)**2
      F2I(3,1,1) = 7.91015625D0*(1.0D0 + CI)**2*(0.0666666666666667D0 + &
      0.666666666666667D0*CI - CI**2)**2
      F2I(3,1,2) = 7.91015625D0*(-1.0D0 + CI)**2*(-0.0666666666666667D0 &
      + 0.666666666666667D0*CI + CI**2)**2
      F2I(3,1,3) = 3.515625D0*(-1.0D0 + CI)**4*(0.5D0 + CI + &
      0.5D0*CI**2)**2/(1.0D0 + CI)**2
      F2I(3,2,0) = -3.515625D0*(-1.0D0 + CI)*(1.0D0 + CI)**5
      F2I(3,2,1) = 31.640625D0*(1.0D0 - CI)*(-0.333333333333333D0 + CI) &
      **2*(1.0D0 + CI)**3
      F2I(3,2,2) = -31.640625D0*(-1.0D0 + CI)**3*(0.333333333333333D0 + &
      CI)**2*(1.0D0 + CI)
      F2I(3,2,3) = -3.515625D0*(-1.0D0 + CI)**5*(1.0D0 + CI)
      F2I(3,3,0) = 3.515625D0*(1.0D0 + CI)**6
      F2I(3,3,1) = 31.640625D0*(-1.0D0 + CI)**2*(1.0D0 + CI)**4
      F2I(3,3,2) = 31.640625D0*(-1.0D0 + CI)**4*(1.0D0 + CI)**2
      F2I(3,3,3) = 3.515625D0*(-1.0D0 + CI)**6
!   --------------------------------------------------------------------
    END SUBROUTINE EVALFI
!=======================================================================
    SUBROUTINE EVALGE(EXC,G2E)
!   --------------------------------------------------------------------
    IMPLICIT NONE
!   --------------------------------------------------------------------
    REAL (KIND=DP), INTENT(IN) :: EXC
    REAL (KIND=DP) :: E2,E3,E4,E5,E6,E7,E8,E9,E10
    REAL (KIND=DP), INTENT(OUT) :: G2E(LMAXP,0:LMAXP,-QMAXP:QMAXP)
!   --------------------------------------------------------------------
       E2 = EXC*EXC
       E3 = E2*EXC
       E4 = E2*E2
       E5 = E4*EXC
       E6 = E4*E2
       E7 = E6*EXC
       E8 = E6*E2
       E9 = E8*EXC
      E10 = E8*E2
!   --------------------------------------------------------------------
!     L=2
!     ---
      G2E(2,0,-10) = 0.D0
      G2E(2,0,-9) = 0.D0
      G2E(2,0,-8) = 0.D0
      G2E(2,0,-7) = 0.D0
      G2E(2,0,-6) = 0.D0
      G2E(2,0,-5) = 0.0040045166015625D0*E10
      G2E(2,0,-4) = E8*(0.00173611111111111D0 + 0.00243055555555556D0*E2)
      G2E(2,0,-3) = E6*(0.000434027777777778D0 + E2*( &
      0.000596788194444444D0 + 0.000629679361979166D0*E2))
      G2E(2,0,-2) = 0.D0
      G2E(2,0,-1) = E2*(0.25D0 + E2*(-0.0625D0 + E2*( &
     0.0169270833333333D0 + E2*(0.00613064236111114D0 + &
     0.0053690592447917D0*E2))))
      G2E(2,0,0) = 1.0D0 + E2*(-5.0D0 + E2*(7.875D0 + E2*( &
      -4.30555555555556D0 + E2*(1.25043402777778D0 - 0.181302083333333D0*E2))))
      G2E(2,0,1) = E2*(12.25D0 + E2*(-53.8125D0 + E2*( &
      85.83984375D0 + E2*(-64.76318359375D0 + 28.4081359863281D0*E2))))
      G2E(2,0,2) = E4*(72.25D0 + E2*(-325.833333333333D0 + E2*( &
      580.215277777778D0 - 547.1625D0*E2)))
      G2E(2,0,3) = E6*(309.906684027778D0 + E2*(-1491.08208550347D0 &
      + 2986.78270975749D0*E2))
      G2E(2,0,4) = E8*(1109.72265625D0 - 5757.64921875D0*E2)
      G2E(2,0,5) = 3536.12958502876D0*E10
      G2E(2,0,6) = 0.D0
      G2E(2,0,7) = 0.D0
      G2E(2,0,8) = 0.D0
      G2E(2,0,9) = 0.D0
      G2E(2,0,10) = 0.D0
      G2E(2,1,-10) = 0.D0
      G2E(2,1,-9) = 0.D0
      G2E(2,1,-8) = 0.D0
      G2E(2,1,-7) = 0.D0
      G2E(2,1,-6) = 0.D0
      G2E(2,1,-5) = 47.9664459228516D0*E10
      G2E(2,1,-4) = E8*(23.16015625D0 + 7.76015625D0*E2)
      G2E(2,1,-3) = E6*(10.97265625D0 + E2*(10.17041015625D0 + 18.3712188720703D0*E2))
      G2E(2,1,-2) = E4*(5.0625D0 + E2*(7.875D0 + E2*(12.9765625D0 + 18.7921875D0*E2)))
      G2E(2,1,-1) = E2*(2.25D0 + E2*(5.0625D0 + E2*(8.96484375D0 + &
      E2*(13.86865234375D0 + 19.7799133300781D0*E2))))
      G2E(2,1,0) = 1.0D0 + E2*(3.0D0 + E2*(6.0D0 + E2*(10.0D0 + EXC &
      **2*(15.0D0 + 21.0D0*E2))))
      G2E(2,1,1) = E2*(2.25D0 + E2*(5.0625D0 + E2*(8.96484375D0 + &
      E2*(13.86865234375D0 + 19.7799133300781D0*E2))))
      G2E(2,1,2) = E4*(5.0625D0 + E2*(7.875D0 + E2*(12.9765625D0 + 18.7921875D0*E2)))
      G2E(2,1,3) = E6*(10.97265625D0 + E2*(10.17041015625D0 + 18.3712188720703D0*E2))
      G2E(2,1,4) = E8*(23.16015625D0 + 7.76015625D0*E2)
      G2E(2,1,5) = 47.9664459228516D0*E10
      G2E(2,1,6) = 0.D0
      G2E(2,1,7) = 0.D0
      G2E(2,1,8) = 0.D0
      G2E(2,1,9) = 0.D0
      G2E(2,1,10) = 0.D0
      G2E(2,2,-10) = 0.D0
      G2E(2,2,-9) = 0.D0
      G2E(2,2,-8) = 0.D0
      G2E(2,2,-7) = 0.D0
      G2E(2,2,-6) = 0.D0
      G2E(2,2,-5) = 3536.12958502876D0*E10
      G2E(2,2,-4) = E8*(1109.72265625D0 - 5757.64921875D0*E2)
      G2E(2,2,-3) = E6*(309.906684027778D0 + E2*(-1491.08208550347D0 &
      + 2986.78270975749D0*E2))
      G2E(2,2,-2) = E4*(72.25D0 + E2*(-325.833333333333D0 + E2*( &
      580.215277777778D0 - 547.1625D0*E2)))
      G2E(2,2,-1) = E2*(12.25D0 + E2*(-53.8125D0 + E2*( &
      85.83984375D0 + E2*(-64.76318359375D0 + 28.4081359863281D0*E2))))
      G2E(2,2,0) = 1.0D0 + E2*(-5.0D0 + E2*(7.875D0 + E2*( &
      -4.30555555555556D0 + E2*(1.25043402777778D0 - &
      0.181302083333333D0*E2))))
      G2E(2,2,1) = E2*(0.25D0 + E2*(-0.0625D0 + E2*( &
      0.0169270833333333D0 + E2*(0.00613064236111114D0 + 0.0053690592447917D0*E2))))
      G2E(2,2,2) = 0.D0
      G2E(2,2,3) = E6*(0.000434027777777778D0 + E2*( &
      0.000596788194444444D0 + 0.000629679361979166D0*E2))
      G2E(2,2,4) = E8*(0.00173611111111111D0 + 0.00243055555555556D0*EXC**2)
      G2E(2,2,5) = 0.0040045166015625D0*E10
      G2E(2,2,6) = 0.D0
      G2E(2,2,7) = 0.D0
      G2E(2,2,8) = 0.D0
      G2E(2,2,9) = 0.D0
      G2E(2,2,10) = 0.D0
!     ---
!     L=3
!     ---
      G2E(3,0,-10) = 0.D0
      G2E(3,0,-9) = 0.D0
      G2E(3,0,-8) = 0.D0
      G2E(3,0,-7) = 0.D0
      G2E(3,0,-6) = 0.D0
      G2E(3,0,-5) = 6.94444444444445D-5*E10
      G2E(3,0,-4) = E8*(6.7816840277778D-6 + 1.35633680555556D-5*E2)
      G2E(3,0,-3) = 0.D0
      G2E(3,0,-2) = E4*(0.015625D0 + E2*(0.00520833333333334D0 + EXC** &
      2*(0.00490993923611111D0 + 0.00393880208333333D0*E2)))
      G2E(3,0,-1) = E2*(1.0D0 + E2*(-2.5D0 + E2*( &
      1.85416666666667D0 + E2*(-0.524305555555555D0 + 0.136197916666667D0*E2))))
      G2E(3,0,0) = 1.0D0 + E2*(-12.0D0 + E2*(49.21875D0 + E2*( &
      -83.21875D0 + E2*(68.0408935546875D0 - 31.233955078125D0*E2))))
      G2E(3,0,1) = E2*(25.0D0 + E2*(-220.0D0 + E2*( &
      736.916666666667D0 + E2*(-1221.72222222222D0 + 1147.2109375D0*EXC**2))))
      G2E(3,0,2) = E4*(252.015625D0 + E2*(-2027.36979166667D0 + E2 &
      *(6597.1491156684D0 - 11511.4770507813D0*E2)))
      G2E(3,0,3) = E6*(1660.5625D0 + E2*(-13126.59375D0 + 43691.82890625D0*E2))
      G2E(3,0,4) = E8*(8504.77816433377D0 - 68154.558710395D0*E2)
      G2E(3,0,5) = 36828.8084027778D0*E10
      G2E(3,0,6) = 0.D0
      G2E(3,0,7) = 0.D0
      G2E(3,0,8) = 0.D0
      G2E(3,0,9) = 0.D0
      G2E(3,0,10) = 0.D0
      G2E(3,1,-10) = 0.D0
      G2E(3,1,-9) = 0.D0
      G2E(3,1,-8) = 0.D0
      G2E(3,1,-7) = 0.D0
      G2E(3,1,-6) = 0.D0
      G2E(3,1,-5) = 14.0312673611111D0*E10
      G2E(3,1,-4) = E8*(7.18072509765625D0 + 23.6063720703125D0*E2)
      G2E(3,1,-3) = E6*(3.67361111111111D0 + E2*(14.2152777777778D0 + 36.3644097222222D0*E2))
      G2E(3,1,-2) = E4*(1.890625D0 + E2*(8.421875D0 + E2*( &
      23.4019368489583D0 + 51.6582790798611D0*E2)))
      G2E(3,1,-1) = E2*(1.0D0 + E2*(5.0D0 + E2*(15.0D0 + E2*(35.0D0 + 70.0D0*E2))))
      G2E(3,1,0) = 1.0D0 + E2*(4.0D0 + E2*(11.46875D0 + E2*( &
      26.4756944444444D0 + E2*(53.2151557074653D0 + 96.8403244357639D0*E2))))
      G2E(3,1,1) = E2*(9.0D0 + E2*(16.5D0 + E2*(38.1875D0 + E2*( &
      71.4791666666667D0 + 124.026996527778D0*E2))))
      G2E(3,1,2) = E4*(43.890625D0 + E2*(32.296875D0 + E2*( &
      97.048095703125D0 + 149.09169921875D0*E2)))
      G2E(3,1,3) = E6*(164.694444444444D0 + E2*(-13.3680555555555D0 + 254.317795138889D0*E2))
      G2E(3,1,4) = E8*(532.960510253906D0 - 416.388549804688D0*E2)
      G2E(3,1,5) = 1567.17015625D0*E10
      G2E(3,1,6) = 0.D0
      G2E(3,1,7) = 0.D0
      G2E(3,1,8) = 0.D0
      G2E(3,1,9) = 0.D0
      G2E(3,1,10) = 0.D0
      G2E(3,2,-10) = 0.D0
      G2E(3,2,-9) = 0.D0
      G2E(3,2,-8) = 0.D0
      G2E(3,2,-7) = 0.D0
      G2E(3,2,-6) = 0.D0
      G2E(3,2,-5) = 1567.17015625D0*E10
      G2E(3,2,-4) = E8*(532.960510253906D0 - 416.388549804688D0*E2)
      G2E(3,2,-3) = E6*(164.694444444444D0 + E2*(-13.3680555555555D0 + 254.317795138889D0*E2))
      G2E(3,2,-2) = E4*(43.890625D0 + E2*(32.296875D0 + E2*( &
      97.048095703125D0 + 149.09169921875D0*E2)))
      G2E(3,2,-1) = E2*(9.0D0 + E2*(16.5D0 + E2*(38.1875D0 + E2* &
      (71.4791666666667D0 + 124.026996527778D0*E2))))
      G2E(3,2,0) = 1.0D0 + E2*(4.0D0 + E2*(11.46875D0 + E2*( &
      26.4756944444444D0 + E2*(53.2151557074653D0 + 96.8403244357639D0*E2))))
      G2E(3,2,1) = E2*(1.0D0 + E2*(5.0D0 + E2*(15.0D0 + E2*( 35.0D0 + 70.0D0*E2))))
      G2E(3,2,2) = E4*(1.890625D0 + E2*(8.421875D0 + E2*( &
      23.4019368489583D0 + 51.6582790798611D0*E2)))
      G2E(3,2,3) = E6*(3.67361111111111D0 + E2*(14.2152777777778D0 + 36.3644097222222D0*E2))
      G2E(3,2,4) = E8*(7.18072509765625D0 + 23.6063720703125D0*E2)
      G2E(3,2,5) = 14.0312673611111D0*E10
      G2E(3,2,6) = 0.D0
      G2E(3,2,7) = 0.D0
      G2E(3,2,8) = 0.D0
      G2E(3,2,9) = 0.D0
      G2E(3,2,10) = 0.D0
      G2E(3,3,-10) = 0.D0
      G2E(3,3,-9) = 0.D0
      G2E(3,3,-8) = 0.D0
      G2E(3,3,-7) = 0.D0
      G2E(3,3,-6) = 0.D0
      G2E(3,3,-5) = 36828.8084027778D0*E10
      G2E(3,3,-4) = E8*(8504.77816433377D0 - 68154.558710395D0*E2)
      G2E(3,3,-3) = E6*(1660.5625D0 + E2*(-13126.59375D0 + 43691.82890625D0*E2))
      G2E(3,3,-2) = E4*(252.015625D0 + E2*(-2027.36979166667D0 + EXC** &
      2*(6597.1491156684D0 - 11511.4770507813D0*E2)))
      G2E(3,3,-1) = E2*(25.0D0 + E2*(-220.0D0 + E2*( &
      736.916666666667D0 + E2*(-1221.72222222222D0 + 1147.2109375D0*EXC**2))))
      G2E(3,3,0) = 1.0D0 + E2*(-12.0D0 + E2*(49.21875D0 + E2*( &
      -83.21875D0 + E2*(68.0408935546875D0 - 31.233955078125D0*E2))))
      G2E(3,3,1) = E2*(1.0D0 + E2*(-2.5D0 + E2*(1.85416666666667D0 &
     + E2*(-0.524305555555555D0 + 0.136197916666666D0*E2))))
      G2E(3,3,2) = E4*(0.015625D0 + E2*(0.00520833333333334D0 + E2 &
      *(0.00490993923611111D0 + 0.00393880208333333D0*E2)))
      G2E(3,3,3) = 0.D0
      G2E(3,3,4) = E8*(6.7816840277778D-6 + 1.35633680555556D-5*E2)
      G2E(3,3,5) = 6.94444444444445D-5*E10
      G2E(3,3,6) = 0.D0
      G2E(3,3,7) = 0.D0
      G2E(3,3,8) = 0.D0
      G2E(3,3,9) = 0.D0
      G2E(3,3,10) = 0.D0
!   --------------------------------------------------------------------
    END SUBROUTINE EVALGE
!=======================================================================
    FUNCTION SUMAR(N,A)
!   --------------------------------------------------------------------
    IMPLICIT NONE
!   --------------------------------------------------------------------
    INTEGER, INTENT(IN) :: N
    INTEGER :: K
    REAL (KIND=DP) :: A(N),SUMA,C,T
    REAL (KIND=DP) :: SUMAR
!   --------------------------------------------------------------------
      SUMA = A(1)
      C = 0.D0
      DO K=2,N
       T = SUMA + A(K)
       IF(DABS(SUMA).GE.DABS(A(K))) THEN
        C = C + ((SUMA - T) + A(K))
        ELSE
        C = C + ((A(K) - T) + SUMA)
       END IF
       SUMA = T
      ENDDO
      SUMAR = SUMA + C
!   --------------------------------------------------------------------
    END FUNCTION SUMAR
!=======================================================================
    FUNCTION PDR(R)
!   --------------------------------------------------------------------
        IMPLICIT NONE
!       ----------------------------------------------------------------
        REAL (KIND=DP), INTENT(IN) :: R
        REAL (KIND=DP) :: PDR
        REAL (KIND=DP), PARAMETER, DIMENSION(3) :: COEFM = (/ 4.35256D-6, -0.0901277D0, 397.012D0 /)
        REAL (KIND=DP), PARAMETER, DIMENSION(3) :: COEFC = (/ -1.52163D-5, -0.0144178D0, 367.767D0 /)
!       ----------------------------------------------------------------        
        IF (R.LE.RC) THEN
            PDR = (COEFC(1)*R + COEFC(2))*R + COEFC(3)
        ELSE
            PDR = (COEFM(1)*R + COEFM(2))*R + COEFM(3)
        END IF
!   --------------------------------------------------------------------
    END FUNCTION PDR
!=======================================================================
    FUNCTION TSOL(R)
!   --------------------------------------------------------------------
        IMPLICIT NONE
!       ----------------------------------------------------------------
        REAL (KIND=DP), INTENT(IN) :: R
        REAL (kind=DP) :: TSOL
!       ----------------------------------------------------------------
        IF (PDR(R).LE.20.D0) THEN
            TSOL = 1661.2D0*DEXP(DLOG(PDR(R)/1.336D0 + 1.D0)/7.437D0)
        ELSE
            TSOL = 2081.8D0*DEXP(DLOG(PDR(R)/101.69D0 + 1.D0)/1.226D0)
        END IF
!   --------------------------------------------------------------------
    END FUNCTION TSOL
!=======================================================================
    FUNCTION TLIQ(R)
!   --------------------------------------------------------------------
        IMPLICIT NONE
!       ----------------------------------------------------------------
        REAL (KIND=DP), INTENT(IN) :: R
        REAL (KIND=DP) :: TLIQ
!       ----------------------------------------------------------------
        IF (PDR(R).LE.20.D0) THEN
            TLIQ = 1982.1D0*DEXP(DLOG(PDR(R)/6.594D0 + 1.D0)/5.374D0)
        ELSE
            TLIQ = 2006.8D0*DEXP(DLOG(PDR(R)/34.65D0 + 1.D0)/1.844D0)
        END IF
!   --------------------------------------------------------------------
    END FUNCTION TLIQ
!=======================================================================
    FUNCTION TCONDUBL(TUBL,DUM,R)
!   --------------------------------------------------------------------
        IMPLICIT NONE
!       ----------------------------------------------------------------
        REAL (KIND=DP), INTENT(IN) ::TUBL,DUM,R
        REAL (KIND=DP) :: TCONDUBL
!       ----------------------------------------------------------------
        TCONDUBL = TSUP + (TUBL-TSUP)*ERF(2.D0*(RT-R)/DUM)
!   --------------------------------------------------------------------
    END FUNCTION TCONDUBL
!=======================================================================
    FUNCTION TCONDLBL(TCMB,TLBL,DLM,R)
!   --------------------------------------------------------------------
        IMPLICIT NONE
!       ----------------------------------------------------------------
        REAL (KIND=DP), INTENT(IN) :: TCMB,TLBL,DLM,R
        REAL (KIND=DP) :: TCONDLBL
!       ----------------------------------------------------------------
        TCONDLBL = TCMB + (TCMB-TLBL)*ERF(2.D0*(RC-R)/DLM)
!   --------------------------------------------------------------------
    END FUNCTION TCONDLBL
!=======================================================================
    FUNCTION TPROF(TCAVG,TMAVG,DLM,DUM,R)
!   --------------------------------------------------------------------
    IMPLICIT NONE
!   --------------------------------------------------------------------
    REAL (KIND=DP), INTENT(IN) :: R,DLM,DUM,TCAVG,TMAVG
    REAL (KIND=DP) :: TPROF,TCMB,TUBL,TLBL
!   --------------------------------------------------------------------
    TCMB = ETAC*TCAVG
    TUBL = ETAUM*TMAVG
    TLBL = ETALM*TMAVG
!   --------------------------------------------------------------------
    IF (R.LE.RC) THEN
        TPROF = TC(TCMB,R)
    ELSE IF ((R.GT.RC).AND.(R.LE.(RC+DLM))) THEN
        TPROF = TCONDLBL(TCMB,TLBL,DLM,R)
    ELSE IF ((R.GE.(RC+DLM)).AND.(R.LE.(RT-DUM))) THEN
        TPROF = TM(TUBL,DUM,R)
    ELSE
        TPROF = TCONDUBL(TUBL,DUM,R)
    END IF
!   --------------------------------------------------------------------
    END FUNCTION TPROF
!=======================================================================
    FUNCTION VISC(T)
!   --------------------------------------------------------------------
    IMPLICIT NONE
!   --------------------------------------------------------------------
    REAL (KIND=DP), INTENT(IN) :: T
    REAL (KIND=DP) :: VISC
!   --------------------------------------------------------------------
    REAL (KIND=DP), PARAMETER ::   ACT0 = 5.2D4     ! ACTIVATION TEMPERATURE
    REAL (KIND=DP), PARAMETER ::  VISC0 = 4.D3      ! REFERENCE VISCOSITY (SCHUBERT ET AL, 2001)
!   --------------------------------------------------------------------
    REAL (KIND=DP), PARAMETER ::   RGAS = 8.31447D0 ! GAS CONSTANT
    REAL (KIND=DP), PARAMETER ::   EACT = 3.D5      ! VISCOSITY ACTIVATION ENERGY
    REAL (KIND=DP), PARAMETER :: ETAREF = 1.D21     ! REFERENCE VISCOSITY (STAMENKOVIC ET AL, 2012)
    REAL (KIND=DP), PARAMETER ::   TREF = 1600.D0   ! REFERENCE TEMPERATURE
!   --------------------------------------------------------------------
    VISC = VISC0*DEXP(ACT0/T)
    ! VISC = ETAREF*DEXP(EACT*(1.D0/T - 1.D0/TREF)/RGAS)
!   --------------------------------------------------------------------
    END FUNCTION VISC
!=======================================================================
    FUNCTION TSOLDB(R)
!   --------------------------------------------------------------------
    IMPLICIT NONE
!   --------------------------------------------------------------------
    REAL (KIND=DP) :: TSOLDB
    REAL (KIND=DP), INTENT(IN) :: R
    REAL (KIND=DP), PARAMETER :: ASOL = -1.160D-16
    REAL (KIND=DP), PARAMETER :: BSOL = 1.708D-9
    REAL (KIND=DP), PARAMETER :: CSOL = -9.074D-3
    REAL (KIND=DP), PARAMETER :: DSOL = 1.993D4
!   --------------------------------------------------------------------
    TSOLDB = ((ASOL*R + BSOL)*R + CSOL)*R + DSOL
!   --------------------------------------------------------------------
    END FUNCTION TSOLDB
!=======================================================================
    FUNCTION TLIQDB(R)
!   --------------------------------------------------------------------
    IMPLICIT NONE
!   --------------------------------------------------------------------
    REAL (KIND=DP) :: TLIQDB
    REAL (KIND=DP), INTENT(IN) :: R
!   --------------------------------------------------------------------
    TLIQDB = TSOL(R) + 500.D0
!   --------------------------------------------------------------------
    END FUNCTION TLIQDB
!=======================================================================
    FUNCTION TLIQC(X,R)
!   --------------------------------------------------------------------
!   PERFIL DE TEMPERATURA DEL LIQUIDUS DEL NUCLEO (STEVENSON ET AL 1983)
!   --------------------------------------------------------------------
        IMPLICIT NONE
!       ----------------------------------------------------------------
        REAL (KIND=DP) :: TLIQC,R,X
        REAL (KIND=DP), PARAMETER :: TLC0 = 1980.D0, TLC1 = 6.14D-3, &
                                     TLC2 = -4.5D-6
!       ----------------------------------------------------------------
        TLIQC = TLC0*(1.D0 - 2.D0*X)*(1.D0 + (TLC1 + TLC2*PDR(R))*PDR(R))
!   --------------------------------------------------------------------
    END
!=======================================================================
    FUNCTION TSOLC(R)
!   --------------------------------------------------------------------
    IMPLICIT NONE
!   --------------------------------------------------------------------
    REAL (KIND=DP), INTENT(IN) :: R
    REAL (KIND=DP) :: TSOLC
!   --------------------------------------------------------------------
    TSOLC = TFE0*DEXP(-2.D0*(1.D0 - 1.D0/(3.D0*GRUN))*(R**2)/(DFE**2))
!   --------------------------------------------------------------------
    END FUNCTION TSOLC
!=======================================================================
    FUNCTION TC(TCMB,R)
!   --------------------------------------------------------------------
!   PERFIL ADIABÁTICO DE TEMPERATURA EN EL NÚCLEO
!   --------------------------------------------------------------------
    IMPLICIT NONE
!   --------------------------------------------------------------------
    REAL (KIND=DP), INTENT(IN) :: R,TCMB
    REAL (KIND=DP) :: TC
!   --------------------------------------------------------------------
    TC = TCMB*DEXP((RC**2 - R**2)/(DN**2))
!   --------------------------------------------------------------------
    END FUNCTION TC
!=======================================================================
    FUNCTION TM(TUBL,DUM,R)
!   --------------------------------------------------------------------
!   PERFIL ADIABÁTICO DE TEMPERATURA DEL MANTO
!   --------------------------------------------------------------------
    IMPLICIT NONE
!   --------------------------------------------------------------------
    REAL (KIND=DP), INTENT(IN) :: R,DUM,TUBL
    REAL (KIND=DP) :: TM
!   --------------------------------------------------------------------
    TM = TUBL + 0.5D-3*(RT-DUM-R)
!   --------------------------------------------------------------------
    END FUNCTION TM
!=======================================================================
    FUNCTION FMELT(R,DUM,TUBL)
!   --------------------------------------------------------------------
    IMPLICIT NONE
!   --------------------------------------------------------------------
    REAL (KIND=DP), INTENT(IN) :: R,DUM,TUBL
    REAL (KIND=DP) :: FMELT
!   --------------------------------------------------------------------
    FMELT = (TM(TUBL,DUM,R) - TSOL(R))/(TLIQ(R) - TSOL(R))
!   --------------------------------------------------------------------
    END FUNCTION FMELT
!=======================================================================
    FUNCTION FMELTR2(R,DUM,TUBL)
!   --------------------------------------------------------------------
    IMPLICIT NONE
!   --------------------------------------------------------------------
    REAL (KIND=DP), INTENT(IN) :: R,DUM,TUBL
    REAL (KIND=DP) :: FMELTR2
!   --------------------------------------------------------------------
    FMELTR2 = FMELT(R,DUM,TUBL)*R**2
!   --------------------------------------------------------------------
    END FUNCTION FMELTR2
!=======================================================================
    FUNCTION STEFAN(TUBL,DUBL)
!   --------------------------------------------------------------------
!   Esta función calcula el número de Stefan
!   --------------------------------------------------------------------
        IMPLICIT NONE
!       ----------------------------------------------------------------
        REAL (KIND=DP), INTENT(IN) :: TUBL,DUBL
        REAL (KIND=DP) :: STEFAN,R1,R2,DEN,TSOLM,TLIQM,INT
!       ----------------------------------------------------------------
!       DETERMINACION DE LA DISTANCIA ENTRE EL CENTRO DE LA TIERRA Y EL
!       LIMITE INFERIOR DE LA ASTENOSFERA
!       ----------------------------------------------------------------
        RAST = RT - DUBL
        DO WHILE ((TM(TUBL,DUBL,RAST)-TSOL(RAST)).GT.0.D0)
            RAST = RAST - DR
        END DO
!       ----------------------------------------------------------------
!       DETERMINACIÓN DE LA DISTANCIA RADIAL ENTRE EL CENTRO DE LA 
!       TIERRA YLA BASE DE LA LITÓSFERA O LÍMITE SUPERIOR DE LA ASTENÓSFERA
!       ----------------------------------------------------------------
        RLIT = RT - DUBL
        DO WHILE ((TCONDUBL(TUBL,DUBL,RLIT)-TSOL(RLIT)).GT.0.D0)
            RLIT = RLIT + DR
        END DO
!       ----------------------------------------------------------------
!       CALCULO DE LOS VALORES MEDIOS DE LAS TEMPERATURAS DE SOLIDUS Y 
!       LIQUIDUS
!       ----------------------------------------------------------------
        R1 = RAST
        R2 = RLIT
        DEN = (R2**3 - R1**3)
        CALL QROMB(TSOLR2,R1,R2,INT)
        TSOLM = 3.D0*INT/DEN
        CALL QROMB(TLIQR2,R1,R2,INT)
        TLIQM = 3.D0*INT/DEN
!       ----------------------------------------------------------------
        STEFAN = LMELT/(CM*(TLIQM - TSOLM))
!   --------------------------------------------------------------------
    END FUNCTION STEFAN
!=======================================================================
    FUNCTION TSOLR2(R)
!   --------------------------------------------------------------------
    IMPLICIT NONE
!   --------------------------------------------------------------------
    REAL (KIND=DP), INTENT(IN) :: R
    REAL (KIND=DP) :: TSOLR2
!   --------------------------------------------------------------------
    TSOLR2 = TSOL(R)*R*R
!   --------------------------------------------------------------------
    END FUNCTION TSOLR2
!=======================================================================
    FUNCTION TLIQR2(R)
!   --------------------------------------------------------------------
    IMPLICIT NONE
!   --------------------------------------------------------------------
    REAL (KIND=DP), INTENT(IN) :: R
    REAL (KIND=DP) :: TLIQR2
!   --------------------------------------------------------------------
    TLIQR2 = TLIQ(R)*R*R
!   --------------------------------------------------------------------
    END
!=======================================================================
    SUBROUTINE qromb(func,a,b,ss)
    REAL (KIND=DP), INTENT(IN) :: a,b
    REAL (KIND=DP), INTENT(OUT) :: ss
    REAL (KIND=DP), PARAMETER :: EPS=1.D-6
    REAL (KIND=DP) :: func
    INTEGER, PARAMETER :: JMAX=20, JMAXP=JMAX+1, K=5, KM=K-1
    INTEGER :: j
    EXTERNAL func
!   USES polint,trapzd
    REAL*8 dss,h(JMAXP),s(JMAXP)
    h(1)=1.
    do j=1,JMAX
      call trapzd(func,a,b,s(j),j)
      if (j.ge.K) then
        call polint(h(j-KM),s(j-KM),K,0.d0,ss,dss)
        if (abs(dss).le.EPS*abs(ss)) return
      endif
      s(j+1)=s(j)
      h(j+1)=0.25d0*h(j)
    end do
    PRINT *, 'too many steps in qromb'
    END SUBROUTINE qromb
!=======================================================================
      SUBROUTINE polint(xa,ya,n,x,y,dy)
      INTEGER :: n,k,m,ns
      REAL (KIND=DP), INTENT(IN) :: x,xa(n),ya(n)
      REAL (KIND=DP), INTENT(OUT) :: y,dy
      INTEGER, PARAMETER :: NMAX=10
      REAL (KIND=DP) :: dlen,dif,dift,ho,hp,w,c(NMAX),d(NMAX)
      ns=1
      dif=dabs(x-xa(1))
      do k=1,n
        dift=dabs(x-xa(k))
        if (dift.lt.dif) then
          ns=k
          dif=dift
        endif
        c(k)=ya(k)
        d(k)=ya(k)
      end do
      y=ya(ns)
      ns=ns-1
      do m=1,n-1
        do k=1,n-m
          ho=xa(k)-x
          hp=xa(k+m)-x
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
      END SUBROUTINE polint
!=======================================================================
      SUBROUTINE trapzd(func,a,b,s,n)
      INTEGER, INTENT(IN) :: n
      REAL (KIND=DP), INTENT(IN) :: a,b
      REAL (KIND=DP), INTENT(OUT) :: s
      INTEGER :: it,j
      REAL (KIND=DP) :: del,sum,tnm,x,func
      if (n.eq.1) then
        s=0.5D0*(b-a)*(func(a)+func(b))
      else
        it=2**(n-2)
        tnm=it
        del=(b-a)/tnm
        x=a+0.5D0*del
        sum=0.D0
        do j=1,it
          sum=sum+func(x)
          x=x+del
        end do
        s=0.5d0*(s+(b-a)*sum/tnm)
      end if
      return
      END SUBROUTINE trapzd
!=======================================================================
    FUNCTION GAMMLN(XX)
      IMPLICIT NONE
      REAL(KIND=DP) :: GAMMLN
      REAL(KIND=DP), INTENT(IN) :: XX
      INTEGER :: J
      REAL(KIND=DP) :: SER,STP,TMP,X,Y,COF(6)
      SAVE COF,STP
      COF(1) = 76.18009172947146D0
      COF(2) = -86.50532032941677D0
      COF(3) = 24.01409824083091D0
      COF(4) = -1.231739572450155D0
      COF(5) = .1208650973866179D-2
      COF(6) = -.5395239384953D-5
      STP = 2.5066282746310005D0
      X=XX
      Y=X
      TMP=X+5.5D0
      TMP=(X+0.5D0)*DLOG(TMP)-TMP
      SER=1.000000000190015D0
      DO J=1,6
        Y=Y+1.D0
        SER=SER+COF(J)/Y
      END DO
      GAMMLN=TMP+DLOG(STP*SER/X)
      RETURN
    END FUNCTION GAMMLN
!=======================================================================
    SUBROUTINE ODEINT(YSTART,NVAR,X1,X2,EPS,H1,HMIN,NOK,NBAD,DERIVS,RKQS)
    INTEGER NBAD,NOK,NVAR,KMAXX,MAXSTP,NMAX
    REAL*8 EPS,H1,HMIN,X1,X2,YSTART(NVAR),TINY
    EXTERNAL DERIVS,RKQS
    PARAMETER (MAXSTP=10000,NMAX=50,KMAXX=200,TINY=1.E-30)
    INTEGER K,KMAX,KOUNT,NSTP
    REAL*8 DXSAV,H,HDID,HNEXT,X,XSAV,DYDX(NMAX),XP(KMAXX),Y(NMAX), &
           YP(NMAX,KMAXX),YSCAL(NMAX)
    COMMON /PATH/ KMAX,KOUNT,DXSAV,XP,YP
    X=X1
    H=SIGN(H1,X2-X1)
    NOK=0
    NBAD=0
    KOUNT=0
    DO K=1,NVAR
      Y(K)=YSTART(K)
    END DO
    IF (KMAX.GT.0) XSAV=X-2.*DXSAV
    DO NSTP=1,MAXSTP
      CALL DERIVS(X,Y,DYDX)
      DO K=1,NVAR
        YSCAL(K)=ABS(Y(K))+ABS(H*DYDX(K))+TINY
      END DO
      IF(KMAX.GT.0)THEN
        IF(ABS(X-XSAV).GT.ABS(DXSAV)) THEN
          IF(KOUNT.LT.KMAX-1)THEN
            KOUNT=KOUNT+1
            XP(KOUNT)=X
            DO K=1,NVAR
              YP(K,KOUNT)=Y(K)
            END DO
            XSAV=X
          ENDIF
        ENDIF
      ENDIF
      IF((X+H-X2)*(X+H-X1).GT.0.) H=X2-X
      CALL RKQS(Y,DYDX,NVAR,X,H,EPS,YSCAL,HDID,HNEXT,DERIVS)
      IF(HDID.EQ.H)THEN
        NOK=NOK+1
      ELSE
        NBAD=NBAD+1
      ENDIF
      IF((X-X2)*(X2-X1).GE.0.)THEN
        DO K=1,NVAR
          YSTART(K)=Y(K)
        END DO
        IF(KMAX.NE.0)THEN
          KOUNT=KOUNT+1
          XP(KOUNT)=X
          DO K=1,NVAR
            YP(K,KOUNT)=Y(K)
          END DO
        ENDIF
        RETURN
      ENDIF
      IF(ABS(HNEXT).LT.HMIN) PRINT *,'STEPSIZE SMALLER THAN MINIMUM IN ODEINT'
      H=HNEXT
    END DO
    PRINT *,'TOO MANY STEPS IN ODEINT'
    RETURN
    END
!=======================================================================
    SUBROUTINE BSSTEP(Y,DYDX,NV,X,HTRY,EPS,YSCAL,HDID,HNEXT,DERIVS)
    INTEGER NV,NMAX,KMAXX,IMAX
    REAL*8 EPS,HDID,HNEXT,HTRY,X,DYDX(NV),Y(NV),YSCAL(NV),SAFE1,SAFE2, &
           REDMAX,REDMIN,TINY,SCALMX
    PARAMETER (NMAX=50,KMAXX=8,IMAX=KMAXX+1,SAFE1=.25,SAFE2=.7, &
               REDMAX=1.E-5,REDMIN=.7,TINY=1.E-30,SCALMX=.1)
!   USES DERIVS,MMID,PZEXTR
    INTEGER K,IQ,J,KK,KM,KMAX,KOPT,NSEQ(IMAX)
    REAL*8 EPS1,EPSOLD,ERRMAX,FACT,H,RED,SCALE,WORK,WRKMIN,XEST,XNEW, &
           A(IMAX),ALF(KMAXX,KMAXX),ERR(KMAXX),YERR(NMAX),YSAV(NMAX),YSEQ(NMAX)
    LOGICAL FIRST,REDUCT
    SAVE A,ALF,EPSOLD,FIRST,KMAX,KOPT,NSEQ,XNEW
    EXTERNAL :: DERIVS
    DATA FIRST/.TRUE./,EPSOLD/-1./
    DATA NSEQ /2,4,6,8,10,12,14,16,18/
    IF(EPS.NE.EPSOLD)THEN
      HNEXT=-1.E29
      XNEW=-1.E29
      EPS1=SAFE1*EPS
      A(1)=NSEQ(1)+1
      DO K=1,KMAXX
        A(K+1)=A(K)+NSEQ(K+1)
      END DO
      DO IQ=2,KMAXX
        DO K=1,IQ-1
          ALF(K,IQ)=EPS1**((A(K+1)-A(IQ+1))/((A(IQ+1)-A(1)+1.)*(2*K+1)))
        END DO
      END DO
      EPSOLD=EPS
      DO KOPT=2,KMAXX-1
        IF(A(KOPT+1).GT.A(KOPT)*ALF(KOPT-1,KOPT))GOTO 1
      END DO
1     KMAX=KOPT
    ENDIF
    H=HTRY
    DO K=1,NV
      YSAV(K)=Y(K)
    END DO
    IF(H.NE.HNEXT.OR.X.NE.XNEW)THEN
      FIRST=.TRUE.
      KOPT=KMAX
    ENDIF
    REDUCT=.FALSE.
2   DO K=1,KMAX
      XNEW=X+H
      IF(XNEW.EQ.X) PRINT *,'STEP SIZE UNDERFLOW IN BSSTEP'
      CALL MMID(YSAV,DYDX,NV,X,H,NSEQ(K),YSEQ,DERIVS)
      XEST=(H/NSEQ(K))**2
      CALL PZEXTR(K,XEST,YSEQ,Y,YERR,NV)
      IF(K.NE.1)THEN
        ERRMAX=TINY
        DO J=1,NV
          ERRMAX=MAX(ERRMAX,ABS(YERR(J)/YSCAL(J)))
        END DO
        ERRMAX=ERRMAX/EPS
        KM=K-1
        ERR(KM)=(ERRMAX/SAFE1)**(1./(2*KM+1))
      ENDIF
      IF(K.NE.1.AND.(K.GE.KOPT-1.OR.FIRST))THEN
        IF(ERRMAX.LT.1.)GOTO 4
        IF(K.EQ.KMAX.OR.K.EQ.KOPT+1)THEN
          RED=SAFE2/ERR(KM)
          GOTO 3
        ELSE IF(K.EQ.KOPT)THEN
          IF(ALF(KOPT-1,KOPT).LT.ERR(KM))THEN
            RED=1./ERR(KM)
            GOTO 3
          ENDIF
        ELSE IF(KOPT.EQ.KMAX)THEN
          IF(ALF(KM,KMAX-1).LT.ERR(KM))THEN
            RED=ALF(KM,KMAX-1)*SAFE2/ERR(KM)
            GOTO 3
          ENDIF
        ELSE IF(ALF(KM,KOPT).LT.ERR(KM))THEN
          RED=ALF(KM,KOPT-1)/ERR(KM)
          GOTO 3
        ENDIF
      ENDIF
    END DO
3   RED=MIN(RED,REDMIN)
    RED=MAX(RED,REDMAX)
    H=H*RED
    REDUCT=.TRUE.
    GOTO 2
4   X=XNEW
    HDID=H
    FIRST=.FALSE.
    WRKMIN=1.E35
    DO KK=1,KM
      FACT=MAX(ERR(KK),SCALMX)
      WORK=FACT*A(KK+1)
      IF(WORK.LT.WRKMIN)THEN
        SCALE=FACT
        WRKMIN=WORK
        KOPT=KK+1
      ENDIF
    END DO
    HNEXT=H/SCALE
    IF(KOPT.GE.K.AND.KOPT.NE.KMAX.AND..NOT.REDUCT)THEN
      FACT=MAX(SCALE/ALF(KOPT-1,KOPT),SCALMX)
      IF(A(KOPT+1)*FACT.LE.WRKMIN)THEN
        HNEXT=H/FACT
        KOPT=KOPT+1
      ENDIF
    ENDIF
    RETURN
    END
!=======================================================================
    SUBROUTINE MMID(Y,DYDX,NVAR,XS,HTOT,NSTEP,YOUT,DERIVS)
    INTEGER NSTEP,NVAR,NMAX
    REAL*8 HTOT,XS,DYDX(NVAR),Y(NVAR),YOUT(NVAR)
    EXTERNAL DERIVS
    PARAMETER (NMAX=50)
    INTEGER K,N
    REAL*8 H,H2,SWAP,X,YM(NMAX),YN(NMAX)
    H=HTOT/NSTEP
    DO K=1,NVAR
      YM(K)=Y(K)
      YN(K)=Y(K)+H*DYDX(K)
    END DO
    X=XS+H
    CALL DERIVS(X,YN,YOUT)
    H2=2.*H
    DO N=2,NSTEP
      DO K=1,NVAR
        SWAP=YM(K)+H2*YOUT(K)
        YM(K)=YN(K)
        YN(K)=SWAP
      END DO
      X=X+H
      CALL DERIVS(X,YN,YOUT)
    END DO
    DO K=1,NVAR
      YOUT(K)=0.5*(YM(K)+YN(K)+H*YOUT(K))
    END DO
    RETURN
    END
!=======================================================================
    SUBROUTINE PZEXTR(IEST,XEST,YEST,YZ,DY,NV)
    INTEGER IEST,NV,IMAX,NMAX
    REAL*8 XEST,DY(NV),YEST(NV),YZ(NV)
    PARAMETER (IMAX=13,NMAX=50)
    INTEGER J,K1
    REAL*8 DELTA,F1,F2,Q,D(NMAX),QCOL(NMAX,IMAX),X(IMAX)
    SAVE QCOL,X
    X(IEST)=XEST
    DO J=1,NV
      DY(J)=YEST(J)
      YZ(J)=YEST(J)
    END DO
    IF(IEST.EQ.1) THEN
      DO J=1,NV
        QCOL(J,1)=YEST(J)
      END DO
    ELSE
      DO J=1,NV
        D(J)=YEST(J)
      END DO
      DO K1=1,IEST-1
        DELTA=1./(X(IEST-K1)-XEST)
        F1=XEST*DELTA
        F2=X(IEST-K1)*DELTA
        DO J=1,NV
          Q=QCOL(J,K1)
          QCOL(J,K1)=DY(J)
          DELTA=D(J)-Q
          DY(J)=F1*DELTA
          D(J)=F2*DELTA
          YZ(J)=YZ(J)+DY(J)
        END DO
      END DO
      DO J=1,NV
        QCOL(J,IEST)=DY(J)
      END DO
    ENDIF
    RETURN
    END
!=======================================================================
    SUBROUTINE RKQS(Y,DYDX,N,X,HTRY,EPS,YSCAL,HDID,HNEXT,DERIVS)
    INTEGER N,NMAX
    REAL*8 EPS,HDID,HNEXT,HTRY,X,DYDX(N),Y(N),YSCAL(N)
    EXTERNAL DERIVS
    PARAMETER (NMAX=50)
!   USES DERIVS,RKCK
    INTEGER K
    REAL*8 ERRMAX,H,XNEW,YERR(NMAX),YTEMP(NMAX),SAFETY,PGROW,PSHRNK,ERRCON
    PARAMETER (SAFETY=0.9,PGROW=-.2,PSHRNK=-.25,ERRCON=1.89E-4)
    H=HTRY
1   CALL RKCK(Y,DYDX,N,X,H,YTEMP,YERR,DERIVS)
    ERRMAX=0.
    DO K=1,N
      ERRMAX=MAX(ERRMAX,ABS(YERR(K)/YSCAL(K)))
    END DO
    ERRMAX=ERRMAX/EPS
    IF(ERRMAX.GT.1.)THEN
      H=SAFETY*H*(ERRMAX**PSHRNK)
      IF(H.LT.0.1*H)THEN
        H=.1*H
      ENDIF
      XNEW=X+H
      IF(XNEW.EQ.X)PRINT *,'STEPSIZE UNDERFLOW IN RKQS'
      GOTO 1
    ELSE
      IF(ERRMAX.GT.ERRCON)THEN
        HNEXT=SAFETY*H*(ERRMAX**PGROW)
      ELSE
        HNEXT=5.*H
      ENDIF
      HDID=H
      X=X+H
      DO K=1,N
        Y(K)=YTEMP(K)
      END DO
      RETURN
    ENDIF
    END
!======================================================================
    SUBROUTINE RKCK(Y,DYDX,N,X,H,YOUT,YERR,DERIVS)
    INTEGER N,NMAX
    REAL*8 H,X,DYDX(N),Y(N),YERR(N),YOUT(N)
    EXTERNAL DERIVS
    PARAMETER (NMAX=50)
!   USES DERIVS
    INTEGER K
    REAL*8 AK2(NMAX),AK3(NMAX),AK4(NMAX),AK5(NMAX),AK6(NMAX), &
    YTEMP(NMAX),A2,A3,A4,A5,A6,B21,B31,B32,B41,B42,B43,B51,B52,B53, &
    B54,B61,B62,B63,B64,B65,C1,C3,C4,C6,DC1,DC3,DC4,DC5,DC6
    PARAMETER (A2=.2,A3=.3,A4=.6,A5=1.,A6=.875,B21=.2,B31=3./40., &
    B32=9./40.,B41=.3,B42=-.9,B43=1.2,B51=-11./54.,B52=2.5, &
    B53=-70./27.,B54=35./27.,B61=1631./55296.,B62=175./512., &
    B63=575./13824.,B64=44275./110592.,B65=253./4096.,C1=37./378., &
    C3=250./621.,C4=125./594.,C6=512./1771.,DC1=C1-2825./27648., &
    DC3=C3-18575./48384.,DC4=C4-13525./55296.,DC5=-277./14336., &
    DC6=C6-.25)
    DO K=1,N
        YTEMP(K)=Y(K)+B21*H*DYDX(K)
    END DO
    CALL DERIVS(X+A2*H,YTEMP,AK2)
    DO K=1,N
      YTEMP(K)=Y(K)+H*(B31*DYDX(K)+B32*AK2(K))
    END DO
    CALL DERIVS(X+A3*H,YTEMP,AK3)
    DO K=1,N
      YTEMP(K)=Y(K)+H*(B41*DYDX(K)+B42*AK2(K)+B43*AK3(K))
    END DO
    CALL DERIVS(X+A4*H,YTEMP,AK4)
    DO K=1,N
      YTEMP(K)=Y(K)+H*(B51*DYDX(K)+B52*AK2(K)+B53*AK3(K)+B54*AK4(K))
    END DO
    CALL DERIVS(X+A5*H,YTEMP,AK5)
    DO K=1,N
      YTEMP(K)=Y(K)+H*(B61*DYDX(K)+B62*AK2(K)+B63*AK3(K)+B64*AK4(K)+B65*AK5(K))
    END DO
    CALL DERIVS(X+A6*H,YTEMP,AK6)
    DO K=1,N
      YOUT(K)=Y(K)+H*(C1*DYDX(K)+C3*AK3(K)+C4*AK4(K)+C6*AK6(K))
    END DO
    DO K=1,N
      YERR(K)=H*(DC1*DYDX(K)+DC3*AK3(K)+DC4*AK4(K)+DC5*AK5(K)+DC6*AK6(K))
    END DO
    RETURN
    END
!=======================================================================

END MODULE THERMEV2_SUBS