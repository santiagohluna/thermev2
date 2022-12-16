!=======================================================================
PROGRAM THERMEV_DB
!-----------------------------------------------------------------------
    USE THERMEV2_SUBS
!-----------------------------------------------------------------------
    IMPLICIT NONE
!-----------------------------------------------------------------------
    INTEGER, PARAMETER :: NVAR = 2
    INTEGER :: NBAD,NOK,L,M,P,Q
    CHARACTER CR
    REAL (KIND=DP) :: T,TPRINT,Y(NVAR),DYDT(NVAR),AVGTC,AVGTM,A,LOD,RIC,&
                      UR,URTOT,DUM,DLM,DLIT,QCMB,QCONV,QMELT,QRADM, & 
                      QRADC,VM,NUM,TCMB,MELTF,QTIDAL
    REAL (KIND=DP), PARAMETER :: EPS = 1.D-6
    REAL (KIND=DP), PARAMETER :: DT = 1.D-3
    COMMON /PRINTOUT/ A,LOD,DUM,DLM,UR,URTOT,QCMB,QCONV,QMELT, &
                      QRADM,QRADC,QTIDAL,VM,RIC,NUM,TCMB,MELTF
!-----------------------------------------------------------------------
!   CÁLCULOS PRELIMINARES
!-----------------------------------------------------------------------
    PRINT *,'REALIZANDO CALCULOS PRELIMINARES...'
    GAMALF = DEXP(GAMMLN(ALPHA + 1.D0))
    GAMMAC = GAMALF*DCOS(0.5D0*ALPHA*PI)
    GAMMAS = GAMALF*DSIN(0.5D0*ALPHA*PI)
!   --------------------------------------------------------------------
    CR = ACHAR(13)
    PRINT *,'... LISTO!'
!-----------------------------------------------------------------------
!   FORMATOS DE SALIDA
!-----------------------------------------------------------------------
!10    FORMAT (A6,1X,F7.2,1X,A6,1X,F7.2,1X,A3,1X,I2)
11  FORMAT(F5.3,1X,F7.2,1X,F7.2,1X,F7.2,1X,F6.2,1X,F4.2,1X,F4.2,1X, &
             F8.6,1X,F10.3,1X,F6.2,1X,F6.2)
12  FORMAT(A,'PROGRESO =',1X,F5.1,1X,A1)
13  FORMAT(A9,1X,F7.2,1X,A2)
!-----------------------------------------------------------------------
!   LECTURA DE ARCHIVO DE ENTRADA
!-----------------------------------------------------------------------
    PRINT *,'LEYENDO ARCHIVO DE ENTRADA...'
    CALL LEER_ENTRADA()
    PRINT *,'... LISTO!'
    NTERMS = 0
    DO L=2,LMAX
        DO M = 0,L
            DO P = 0,L
                DO Q = -QMAX,QMAX
                    NTERMS = NTERMS + 1
                END DO
            END DO
        END DO
    END DO
!-----------------------------------------------------------------------
!   EVALUACIÓN DE LOS FACTORES K_LM, FUNCIONES DE LA INCLINACIÓN Y DE LA
!   EXCENTRICIDAD
!-----------------------------------------------------------------------
    PRINT *,'EVALUANDO FACTORES K_LM...'
    CALL EVALKLM()
    PRINT *,'... LISTO!'
    PRINT *,'EVALUANDO FUNCIONES DE LA INCLINACIÓN...'
    CALL EVALFI(I,FFI)
    PRINT *,'... LISTO!'
    PRINT *,'EVALUANDO FUNCIONES DE LA EXCENTRICIDAD...'
    CALL EVALGE(E,GGE)
    PRINT *,'... LISTO!'
!-----------------------------------------------------------------------
!   CREACIÓN DE ARCHIVO DE SALIDA
!-----------------------------------------------------------------------
    PRINT *,'CREANDO ARCHIVO DE SALIDA...'
    CALL CREAR_ARCHIVO_SALIDA()
    PRINT *,'... LISTO!'
!-----------------------------------------------------------------------
!   INICIALIZACIÓN DE LAS TEMPERATURAS MEDIAS DEL MANTO Y DEL NÚCLEO
!-----------------------------------------------------------------------
    PRINT *,'INCIALIZANDO VARIABLES...'
         T = 0.D0
    TPRINT = 0.D0
    DTPRINT = DTPRINT/1000.D0
    AVGTC = TC0
    AVGTM = TM0
      Y(1) = TC0
      Y(2) = TM0
    CALL DERIVS(T,Y,DYDT)
    DLIT = LIT(TC0,TM0,DUM,DLM)
    PRINT *,'... LISTO!'
    PRINT *,' '
    PRINT *,'==================='
    PRINT *,'  ESTADO INICIAL:  '
    PRINT *,'==================='
    PRINT 13,'    T_C =',AVGTC,'K'
    PRINT 13,'  T_CMB =',ETAC*AVGTC,'K'
    PRINT 13,'    T_M =',AVGTM,'K'
    PRINT 13,'  T_UBL =',ETAUM*AVGTM,'K'
    PRINT 13,'   R_IC =',RIC/1000.D0,'KM'
    PRINT 13,'    LIT =',DLIT,'KM'
    PRINT 13,'  Q_CMB =',QCMB*1.D-12,'TW'
    PRINT 13,' Q_CONV =',QCONV*1.D-12,'TW'
    PRINT 13,' Q_MELT =',QMELT*1.D-12,'TW'
    PRINT 13,'Q_RAD,M =',QRADM*1.D-12,'TW'
    PRINT 13,'Q_RAD,C =',QRADC*1.D-12,'TW'
    PRINT 13,' UR_TOT =',URTOT,'ND'
    PRINT *,' '
!   --------------------------------------------------------------------
!   ESCRITURA DEL ESTADO INICIAL EN EL ARCHIVO DE SALIDA
!   --------------------------------------------------------------------
    PRINT *,'ESCRIBIENDO ESTADO INICIAL EN ARCHIVO DE SALIDA...'     
    WRITE(11,*) T,AVGTC,AVGTM,DLIT,A/A0,LOD/LODF,RIC/1.D3,UR,URTOT,DUM/1000.D0,DLM/1000.D0, &
                QCMB*1.D-12,QCONV*1.D-12,QMELT*1.D-12,QRADM*1.D-12,QRADC*1.D-12,QTIDAL*1D-12, & 
                VISC(AVGTM),TCMB,MELTF
    PRINT *,'... LISTO!'
!   --------------------------------------------------------------------
!   PERFIL DE TEMPERATURA INICIAL
!   --------------------------------------------------------------------
    PRINT *,'IMPRIMIENDO PERFIL DE TEMPERATURA INICIAL...'
    CALL IMPRIMIR_PERFIL(TPRINT,AVGTC,DLM,AVGTM,DUM)
    PRINT *,'... LISTO!'
!-----------------------------------------------------------------------
!   INTEGRACION DE LAS ECUACIONES DIFERENCIALES
!-----------------------------------------------------------------------
    PRINT *,'INTEGRANDO ECUACIONES DIFERENCIALES...'
    DO WHILE (T+DT.LE.TF)
!       ----------------------------------------------------------------
!       IMPRESIÓN DEL PERFIL DE TEMPERATURA
!       ----------------------------------------------------------------
        IF (T.GE.TPRINT) THEN
            CALL IMPRIMIR_PERFIL(TPRINT,AVGTC,DLM,AVGTM,DUM)
            TPRINT = TPRINT + DTPRINT
        END IF
!       ----------------------------------------------------------------
        CALL ODEINT(Y,NVAR,T,T+DT,EPS,DT,0.D0,NOK,NBAD,DERIVS,RKQS)    
        AVGTC = Y(1)
        AVGTM = Y(2)
!       ----------------------------------------------------------------
!       ESCRITURA DE RESULTADOS EN EL ARCHIVO DE SALIDA
!       ----------------------------------------------------------------
        DLIT = LIT(TC0,TM0,DUM,DLM)
!                    1   2     3   4    5      6      7       8     9     10          11
        WRITE(11,*) T,AVGTC,AVGTM,DLIT,A/A0,LOD/LODF,RIC/1.D3,UR,URTOT,DUM/1000.D0,DLM/1000.D0, &
!             12           13          14           15           16            17           18
             QCMB*1.D-12,QCONV*1.D-12,QMELT*1.D-12,QRADM*1.D-12,QRADC*1.D-12,QTIDAL*1.D-12,VISC(AVGTM), &
!              19  20 
             TCMB,MELTF
        WRITE(*,12,ADVANCE='NO') CR,T*100.D0/TF,'%'
        T = T + DT
    END DO
    PRINT *,' '
    PRINT *,'... LISTO!'
    PRINT *,' '
    PRINT *,'==================='
    PRINT *,'   ESTADO FINAL:   '
    PRINT *,'==================='
    PRINT 13,'    T_C = ',AVGTC,'K'
    PRINT 13,'  T_CMB = ',ETAC*AVGTC,'K'
    PRINT 13,'    T_M = ',AVGTM,'K'
    PRINT 13,'  T_UBL = ',ETAUM*AVGTM,'K'
    PRINT 13,'   R_IC = ',RIC/1000.D0,'KM'
    PRINT 13,'    LIT = ',DLIT,'KM'
    PRINT 13,'  Q_CMB = ',QCMB*1.D-12,'TW'
    PRINT 13,' Q_CONV = ',QCONV*1.D-12,'TW'
    PRINT 13,' Q_MELT = ',QMELT*1.D-12,'TW'
    PRINT 13,'Q_RAD,M = ',QRADM*1.D-12,'TW'
    PRINT 13,'Q_RAD,C = ',QRADC*1.D-12,'TW'
    PRINT 13,' UR_TOT = ',URTOT,'ND'
    PRINT 13,' '
!   --------------------------------------------------------------------
!   PERFIL DE TEMPERATURA FINAL
!   --------------------------------------------------------------------
    PRINT *,'IMPRIMIENDO PERFIL DE TEMPERATURA FINAL...'
    CALL IMPRIMIR_PERFIL(TPRINT,AVGTC,DLM,AVGTM,DUM)
    PRINT *,'... LISTO!'
!   --------------------------------------------------------------------
    PRINT *,' '
!-----------------------------------------------------------------------
END PROGRAM THERMEV_DB
!=======================================================================