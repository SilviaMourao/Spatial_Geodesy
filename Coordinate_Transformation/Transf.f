      PROGRAM TRANSFHELM
      IMPLICIT NONE
      REAL*8 X_14(3,1),XDOT_14(3,1),X_08(3,1),XDOT_08(3,1),EPOCA
      REAL*8 X_05(3,1),XDOT_05(3,1),X_00(3,1),XDOT_00(3,1)
      REAL*8 X_E00(3,1),XDOT_E00(3,1)
      REAL*8 XX,YY,ZZ,XXDOT,YYDOT,ZZDOT

	INTEGER OPTION,I
      CHARACTER LINE*80,ID*4

	WRITE(6,*)'Welcome to the Coordinate Transformation Program'
	WRITE(6,*)'Please Select an Option'
	WRITE(6,*)'1- Read File (ITRF2014 - IGN)'
	WRITE(6,*)'2- Manual Input (1 point)'
	READ(5,*)OPTION
	WRITE(6,*)'Please Input Epoch of Observation:'
	READ(5,*)EPOCA
	
     	IF(OPTION.EQ.1)THEN
c	Please enter your filename and extension on line 24
c	FILE='yourfilename.txt'
c	reads files with format IGS - ITRF2014
c	 read file
		OPEN(10,FILE='ITRF2014.txt',STATUS='OLD')
		DO I=1,7
	    READ(10,'(A)')LINE
	    END DO
  500		READ(10,100,END=900)ID,XX,YY,ZZ
		READ(10,101,END=900)XXDOT,YYDOT,ZZDOT


		X_14(1,1)=XX
		X_14(2,1)=YY
		X_14(3,1)=ZZ

		XDOT_14(1,1)= XXDOT
		XDOT_14(2,1)= YYDOT
		XDOT_14(3,1)= ZZDOT

 
c	Conversao 14 para 08
		CALL I14_I08(X_14,XDOT_14,EPOCA,X_08,XDOT_08)
	    OPEN(11, FILE='Transf14_08.OUT')
 		WRITE(11,1000)ID,X_08(1,1),X_08(2,1),X_08(3,1),
	1		XDOT_08(1,1),XDOT_08(2,1),XDOT_08(3,1)

c 	Conversao 08 para 05
		CALL I08_I05(X_08,XDOT_08,EPOCA,X_05,XDOT_05)
	    OPEN(12, FILE='Transf08_05.OUT')
 		WRITE(12,1000)ID,X_05(1,1),X_05(2,1),X_05(3,1),
	1		XDOT_05(1,1),XDOT_05(2,1),XDOT_05(3,1)

c	Conversao 05 para 00
		CALL I05_I00(X_05,XDOT_05,EPOCA,X_00,XDOT_00)
	    OPEN(13, FILE='Transf05_00.OUT')
 		WRITE(13,1000)ID,X_00(1,1),X_00(2,1),X_00(3,1),
	1		XDOT_00(1,1),XDOT_00(2,1),XDOT_00(3,1)

c	Atualizacao Coordenadas
		CALL I00_E00(X_00,XDOT_00,EPOCA,X_E00,XDOT_E00)
	    OPEN(14, FILE='Transf00_ETRF.OUT')
      	WRITE(14,1000)ID,X_E00(1,1),X_E00(2,1),X_E00(3,1),
	1		XDOT_E00(1,1),XDOT_E00(2,1),XDOT_E00(3,1)

 
  100		FORMAT(32X,A4,3(F14.4))
  101		FORMAT(36X,3(F14.5))
 1000		FORMAT(A6,2X,3(F16.4,2X),3(F8.5,2X))
 1001		FORMAT(A9,2X,3(F16.4,2X),3(F8.5,2X))
 1002		FORMAT(A9,2X,3(A16,2X),3(A8,2X))

	
c 102  FORMAT(A4,6F14.5)
		GOTO 500
      
  900		CONTINUE
     
      ELSE
c     --------------------------------------------------------------------
c     Input de Coordenadas Iniciais
c     --------------------------------------------------------------------
		WRITE(6,*)'Helmert Transf 14 Parameters'
		WRITE(6,*)'Insert X,Y,Z ITRF2014'					 
		READ(5,*)XX,YY,ZZ
		WRITE(6,*)'Insert XDOT,YDOT,ZDOT ITRF2014'
		READ(5,*)XXDOT,YYDOT,ZZDOT

		X_14(1,1)=XX
		X_14(2,1)=YY
		X_14(3,1)=ZZ

		XDOT_14(1,1)= XXDOT
		XDOT_14(2,1)= YYDOT
		XDOT_14(3,1)= ZZDOT
	
c    --------------------------------------------------------------------
c    Resultados - Subrotinas
c    --------------------------------------------------------------------
c	Coordenadas Iniciais'
	    WRITE(6,1002)'Ref','X','Y','Z','X_DOT','Y_DOT','Z_DOT'
	    WRITE(6,1001)'ITRF2014',X_14(1,1),X_14(2,1),X_14(3,1),
     1	XDOT_14(1,1),XDOT_14(2,1),XDOT_14(3,1)

c	Conversao 14 para 08
		CALL I14_I08(X_14,XDOT_14,EPOCA,X_08,XDOT_08)
	    WRITE(6,1001)'ITRF2008',X_08(1,1),X_08(2,1),X_08(3,1),
     1	XDOT_08(1,1),XDOT_08(2,1),XDOT_08(3,1)

c 	Conversao 08 para 05
		CALL I08_I05(X_08,XDOT_08,EPOCA,X_05,XDOT_05)
	    WRITE(6,1001)'ITRF2005',X_05(1,1),X_05(2,1),X_05(3,1),
     1	XDOT_05(1,1),XDOT_05(2,1),XDOT_05(3,1)

c	Conversao 05 para 00
		CALL I05_I00(X_05,XDOT_05,EPOCA,X_00,XDOT_00)
	    WRITE(6,1001)'ITRF2000',X_00(1,1),X_00(2,1),X_00(3,1),
     1	XDOT_00(1,1),XDOT_00(2,1),XDOT_00(3,1)

c	Atualizacao Coordenadas
		CALL I00_E00(X_00,XDOT_00,EPOCA,X_E00,XDOT_E00)
	    WRITE(6,1001)'ETRF2005',X_E00(1,1),X_E00(2,1),X_E00(3,1),
     1	XDOT_E00(1,1),XDOT_E00(2,1),XDOT_E00(3,1)

c      ELSE
c		WRITE(6,*)'Wrong input, please try again'

	END IF
	END
      
      

c    --------------------------------------------------------------------
c     Subrotinas
c    --------------------------------------------------------------------


      SUBROUTINE I14_I08(X_14,XDOT_14,EPOCA,X_08,XDOT_08)                            
  
      IMPLICIT NONE
  
      REAL*8 X_14(3,1),XDOT_14(3,1),X_08(3,1),XDOT_08(3,1),EPOCA
      REAL*8 T(3,1),D,R(3,3),TDOT(3,1),DDOT,RDOT(3,3),DIFE
      REAL*8 AUX1(3,1),AUX2(3,1),AUX3(3,1),AUX4(3,1),CONV,PI
      REAL*8 AUX1DOT(3,1),AUX2DOT(3,1),AUX3DOT(3,1),AUX4DOT(3,1) 

      PI= 4.0D0*DATAN(1.0D0)
      CONV=(1.0D-3/3600)*(PI/180)
      DIFE=EPOCA-2010.0D0

c     CORRECAO AOS PARAMETROS DE TRANSFORMACAO     
      T(1,1)=1.6D-3+DIFE*TDOT(1,1)
      T(2,1)=1.9D-3+DIFE*TDOT(2,1)
      T(3,1)=2.4D-3+DIFE*TDOT(3,1)
      D=-0.02D-9+DIFE*DDOT   
      R(1,1)=0.0D0*CONV+DIFE*RDOT(1,1)
      R(1,2)=0.0D0*CONV+DIFE*RDOT(1,2)
      R(1,3)=0.0D0*CONV+DIFE*RDOT(1,3)
      R(2,1)=0.0D0*CONV+DIFE*RDOT(2,1)
      R(2,2)=0.0D0*CONV+DIFE*RDOT(2,2)
      R(2,3)=0.0D0*CONV+DIFE*RDOT(2,3) 
      R(3,1)=0.0D0*CONV+DIFE*RDOT(3,1)
      R(3,2)=0.0D0*CONV+DIFE*RDOT(3,2)
      R(3,3)=0.0D0*CONV+DIFE*RDOT(3,3)
 
C     PARAMETROS VELOCIDADES
      TDOT(1,1)=0.0D-3
      TDOT(2,1)=0.0D-3
      TDOT(3,1)=-0.1D-3
      DDOT=0.03D-9
      RDOT(1,1)=0.0D0*CONV
      RDOT(1,2)=0.0D0*CONV
      RDOT(1,3)=0.0D0*CONV
      RDOT(2,1)=0.0D0*CONV
      RDOT(2,2)=0.0D0*CONV
      RDOT(2,3)=0.0D0*CONV
      RDOT(3,1)=0.0D0*CONV
      RDOT(3,2)=0.0D0*CONV
      RDOT(3,3)=0.0D0*CONV
  
c     CALCULO DAS COORDENADAS    
c     X_08=X_14+T+D*X_14+R*X_14  
      CALL ADDAB(X_14,T,AUX1,3,1,3,1)
      CALL MATSCA(D,X_14,AUX2,3,1,3,1)  
      CALL AB(R,X_14,AUX3,3,3,1,3,3,1)
      CALL ADDAB(AUX1,AUX2,AUX4,3,1,3,1)
      CALL ADDAB(AUX4,AUX3,X_08,3,1,3,1)
c     CALCULO DAS VELOCIDADES  
c     XDOT_08=XDOT_14+TDOT+DDOT*X_14+RDOT*X_14 
      CALL ADDAB(XDOT_14,TDOT,AUX1DOT,3,1,3,1) 
      CALL MATSCA(DDOT,X_14,AUX2DOT,3,1,3,1) 
      CALL AB(RDOT,X_14,AUX3DOT,3,3,1,3,3,1)
      CALL ADDAB(AUX1DOT,AUX2DOT,AUX4DOT,3,1,3,1)
      CALL ADDAB(AUX4DOT,AUX3DOT,XDOT_08,3,1,3,1)
        
      END                             


      SUBROUTINE I08_I05(X_08,XDOT_08,EPOCA,X_05,XDOT_05)                          
      IMPLICIT NONE
      REAL*8 X_08(3,1),XDOT_08(3,1),X_05(3,1),XDOT_05(3,1),EPOCA
      REAL*8 T(3,1),D,R(3,3),TDOT(3,1),DDOT,RDOT(3,3),DIFE
      REAL*8 AUX1(3,1),AUX2(3,1),AUX3(3,1),AUX4(3,1),CONV,PI
      REAL*8 AUX1DOT(3,1),AUX2DOT(3,1),AUX3DOT(3,1),AUX4DOT(3,1) 
      
      PI= 4.0D0*DATAN(1.0D0)
      CONV=(1.0D-3/3600)*(PI/180)
      DIFE=EPOCA-2005.0D0

	  
C     PARAMETROS VELOCIDADES
      TDOT(1,1)=0.3D-3
      TDOT(2,1)=0.0D-3
      TDOT(3,1)=0.0D-3
      DDOT=0.0D-9
      RDOT(1,1)=0.0D0*CONV
      RDOT(1,2)=0.0D0*CONV
      RDOT(1,3)=0.0D0*CONV
      RDOT(2,1)=0.0D0*CONV
      RDOT(2,2)=0.0D0*CONV
      RDOT(2,3)=0.0D0*CONV
      RDOT(3,1)=0.0D0*CONV
      RDOT(3,2)=0.0D0*CONV
      RDOT(3,3)=0.0D0*CONV

c     CORRECAO AOS PARAMETROS DE TRANSFORMACAO        
      T(1,1)=-0.5D-3+DIFE*TDOT(1,1)
      T(2,1)=-0.9D-3+DIFE*TDOT(2,1)
      T(3,1)=-4.7D-3+DIFE*TDOT(3,1)
      D=0.94D-9+DIFE*DDOT   
      R(1,1)=0.0D0*CONV+DIFE*RDOT(1,1)
      R(1,2)=0.0D0*CONV+DIFE*RDOT(1,2)
      R(1,3)=0.0D0*CONV+DIFE*RDOT(1,3)
      R(2,1)=0.0D0*CONV+DIFE*RDOT(2,1)
      R(2,2)=0.0D0*CONV+DIFE*RDOT(2,2)
      R(2,3)=0.0D0*CONV+DIFE*RDOT(2,3) 
      R(3,1)=0.0D0*CONV+DIFE*RDOT(3,1)
      R(3,2)=0.0D0*CONV+DIFE*RDOT(3,2)
      R(3,3)=0.0D0*CONV+DIFE*RDOT(3,3)  

c     CALCULO DAS COORDENADAS   
c     X_05=X_08+T+D*X_08+R*X_08    
      CALL ADDAB(X_08,T,AUX1,3,1,3,1)
      CALL MATSCA(D,X_08,AUX2,3,1,3,1)  
      CALL AB(R,X_08,AUX3,3,3,1,3,3,1)
      CALL ADDAB(AUX1,AUX2,AUX4,3,1,3,1)
      CALL ADDAB(AUX4,AUX3,X_05,3,1,3,1)
c     CALCULO DAS VELOCIDADES  
c     XDOT_05=XDOT_08+TDOT+DDOT*X_08+RDOT*X_08 
      CALL ADDAB(XDOT_08,TDOT,AUX1DOT,3,1,3,1) 
      CALL MATSCA(DDOT,X_08,AUX2DOT,3,1,3,1) 
      CALL AB(RDOT,X_08,AUX3DOT,3,3,1,3,3,1)
      CALL ADDAB(AUX1DOT,AUX2DOT,AUX4DOT,3,1,3,1)
      CALL ADDAB(AUX4DOT,AUX3DOT,XDOT_05,3,1,3,1)
      
      END


      SUBROUTINE I05_I00(X_05,XDOT_05,EPOCA,X_00,XDOT_00)                          
      IMPLICIT NONE
      REAL*8 X_05(3,1),XDOT_05(3,1),X_00(3,1),XDOT_00(3,1),EPOCA
      REAL*8 T(3,1),D,R(3,3),TDOT(3,1),DDOT,RDOT(3,3),DIFE
      REAL*8 AUX1(3,1),AUX2(3,1),AUX3(3,1),AUX4(3,1),CONV,PI
      REAL*8 AUX1DOT(3,1),AUX2DOT(3,1),AUX3DOT(3,1),AUX4DOT(3,1) 
      
      PI= 4.0D0*DATAN(1.0D0)
      CONV=(1.0D-3/3600)*(PI/180)
      DIFE=EPOCA-2000.0D0

	  
C     PARAMETROS VELOCIDADES
      TDOT(1,1)=-0.2D-3
      TDOT(2,1)=0.1D-3
      TDOT(3,1)=-1.8D-3
      DDOT=0.08D-9
      RDOT(1,1)=0.0D0*CONV
      RDOT(1,2)=0.0D0*CONV
      RDOT(1,3)=0.0D0*CONV
      RDOT(2,1)=0.0D0*CONV
      RDOT(2,2)=0.0D0*CONV
      RDOT(2,3)=0.0D0*CONV
      RDOT(3,1)=0.0D0*CONV
      RDOT(3,2)=0.0D0*CONV
      RDOT(3,3)=0.0D0*CONV

	  
c     CORRECAO AOS PARAMETROS DE TRANSFORMACAO       
      T(1,1)=0.1D-3+DIFE*TDOT(1,1)
      T(2,1)=-0.8D-3+DIFE*TDOT(2,1)
      T(3,1)=-5.8D-3+DIFE*TDOT(3,1)
      D=0.40D-9+DIFE*DDOT   
      R(1,1)=0.0D0*CONV+DIFE*RDOT(1,1)
      R(1,2)=0.0D0*CONV+DIFE*RDOT(1,2)
      R(1,3)=0.0D0*CONV+DIFE*RDOT(1,3)
      R(2,1)=0.0D0*CONV+DIFE*RDOT(2,1)
      R(2,2)=0.0D0*CONV+DIFE*RDOT(2,2)
      R(2,3)=0.0D0*CONV+DIFE*RDOT(2,3) 
      R(3,1)=0.0D0*CONV+DIFE*RDOT(3,1)
      R(3,2)=0.0D0*CONV+DIFE*RDOT(3,2)
      R(3,3)=0.0D0*CONV+DIFE*RDOT(3,3)  	  
	  



c     CALCULO DAS COORDENADAS  
c     X_00=X_05+T+D*X_05+R*X_05   
      CALL ADDAB(X_05,T,AUX1,3,1,3,1)
      CALL MATSCA(D,X_05,AUX2,3,1,3,1)  
      CALL AB(R,X_05,AUX3,3,3,1,3,3,1)
      CALL ADDAB(AUX1,AUX2,AUX4,3,1,3,1)
      CALL ADDAB(AUX4,AUX3,X_00,3,1,3,1)
c     CALCULO DAS VELOCIDADES 
c     XDOT_00=XDOT_05+TDOT+DDOT*X_05+RDOT*X_05   
      CALL ADDAB(XDOT_05,TDOT,AUX1DOT,3,1,3,1) 
      CALL MATSCA(DDOT,X_05,AUX2DOT,3,1,3,1) 
      CALL AB(RDOT,X_05,AUX3DOT,3,3,1,3,3,1)
      CALL ADDAB(AUX1DOT,AUX2DOT,AUX4DOT,3,1,3,1)
      CALL ADDAB(AUX4DOT,AUX3DOT,XDOT_00,3,1,3,1)

      END
      
      
      SUBROUTINE I00_E00(X_00,XDOT_00,EPOCA,X_E00,XDOT_E00)                        
      IMPLICIT NONE
      REAL*8 X_00(3,1),XDOT_00(3,1),X_E00(3,1),XDOT_E00(3,1),EPOCA
      REAL*8 T(3,1),D,R(3,3),TDOT(3,1),DDOT,RDOT(3,3),DIFE
      REAL*8 AUX1(3,1),AUX2(3,1),AUX3(3,1),AUX4(3,1),CONV,PI,R1,R2,R3
      REAL*8 AUX1DOT(3,1),AUX2DOT(3,1),AUX3DOT(3,1),AUX4DOT(3,1) 
     
      PI= 4.0D0*DATAN(1.0D0)
      CONV=(1.0D-3/3600)*(PI/180)
      DIFE=EPOCA-1989.0D0
   

C     PARAMETROS VELOCIDADES
      TDOT(1,1)=0.0D-3
      TDOT(2,1)=0.0D-3
      TDOT(3,1)=0.0D-3
      DDOT=0.00D-9
      R1=0.081D0
      R2=0.490D0
      R3=-0.792D0
      RDOT(1,1)=0.0D0*CONV
      RDOT(1,2)=-R3*CONV
      RDOT(1,3)=R2*CONV
      RDOT(2,1)=R3*CONV
      RDOT(2,2)=0.0D0*CONV
      RDOT(2,3)=-R1*CONV
      RDOT(3,1)=-R2*CONV
      RDOT(3,2)=R1*CONV
      RDOT(3,3)=0.0D0*CONV
      
c     CORRECAO AOS PARAMETROS DE TRANSFORMACAO      
      T(1,1)=54.D-3+DIFE*TDOT(1,1)
      T(2,1)=51.0D-3+DIFE*TDOT(2,1)
      T(3,1)=-48.0D-3+DIFE*TDOT(3,1)
      D=0.00D-9+DIFE*DDOT   
      R(1,1)=0.0D0*CONV+DIFE*RDOT(1,1)
      R(1,2)=0.0D0*CONV+DIFE*RDOT(1,2)
      R(1,3)=0.0D0*CONV+DIFE*RDOT(1,3)
      R(2,1)=0.0D0*CONV+DIFE*RDOT(2,1)
      R(2,2)=0.0D0*CONV+DIFE*RDOT(2,2)
      R(2,3)=0.0D0*CONV+DIFE*RDOT(2,3) 
      R(3,1)=0.0D0*CONV+DIFE*RDOT(3,1)
      R(3,2)=0.0D0*CONV+DIFE*RDOT(3,2)
      R(3,3)=0.0D0*CONV+DIFE*RDOT(3,3)     

c     CALCULO DAS COORDENADAS 
c     X_E00=X_00+T+D*X_00+R*X_00    
      CALL ADDAB(X_00,T,AUX1,3,1,3,1)
      CALL MATSCA(D,X_00,AUX2,3,1,3,1)  
      CALL AB(R,X_00,AUX3,3,3,1,3,3,1)
      CALL ADDAB(AUX1,AUX2,AUX4,3,1,3,1)
      CALL ADDAB(AUX4,AUX3,X_E00,3,1,3,1)
c     CALCULO DAS VELOCIDADES  
c     XDOT_E00=XDOT_00+TDOT+DDOT*X_00+RDOT*X_00 
      CALL ADDAB(XDOT_00,TDOT,AUX1DOT,3,1,3,1) 
      CALL MATSCA(DDOT,X_00,AUX2DOT,3,1,3,1) 
      CALL AB(RDOT,X_00,AUX3DOT,3,3,1,3,3,1)
      CALL ADDAB(AUX1DOT,AUX2DOT,AUX4DOT,3,1,3,1)
      CALL ADDAB(AUX4DOT,AUX3DOT,XDOT_E00,3,1,3,1)

      END



C   SUBROTINA PARA EFECTUAR O PRODUTO DAS MATRIZES A(L,M)
C   E B(M,N), SENDO O RESULTADO COLOCADO NA MATRIZ R(L,N).
C   AS DIMENSOES MAXIMAS DAS MATRIZES (NL,NM,NN) 
C   BEM COMO AS EFECTIVAS (L,M,N), SAO PASSADAS DO PROGRAMA PRINCIPAL.
C   AS MATRIZES A E B PODEM SER IGUAIS. R TEM DE SER DIFERENTE
C   DE A E DE B, AS QUAIS VOLTAM INALTERADAS AO PROGRAMA PRINCIPAL.

      SUBROUTINE AB(A,B,R,NL,NM,NN,L,M,N)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 A(NL,NM),B(NM,NN),R(NL,NN)
      DO 5 I=1,L
      DO 5 J=1,N
      R(I,J)=0
      DO 5 K=1,M
    5 R(I,J)=R(I,J)+A(I,K)*B(K,J)
      RETURN
      END           
      
C   SUBROTINA PARA MULTIPLICAR A MATRIZ B PELO ESCALAR A                
C   E COLOCA O RESULTADO EM C.                                          
C                                                                       
C   AS MATRIZES B E C PODEM SER IGUAIS.                                 
C                                                                       
C   AS DIMENSOES MAXIMAS DAS MATRIZES (NM,NN),           
C   BEM COMO AS EFECTIVAS (M,N), SAO PASSADAS DO PROGRAMA PRINCIPAL.          
C                                                                       
      SUBROUTINE MATSCA(A,B,C,NM,NN,M,N)                                
      IMPLICIT REAL*8(A-C)                                              
      REAL*8 B(NM,NN),C(NM,NN)                                       
      DO 5 I=1,M                                                        
      DO 5 J=1,N                                                        
    5 C(I,J) = A * B(I,J)                                               
      RETURN                                                            
      END                                                                        
      
      
C   SUBROTINA PARA FORMAR A MATRIZ SOMA R=A+B.                          
C   AS MATRIZES A,B E R PODEM SER IGUAIS.                               
C                                                                       
C   AS DIMENSOES MAXIMAS DAS MATRIZES (NL,NM),          
C   BEM COMO AS EFECTIVAS (L,M), SAO PASSADAS DO 
C   PROGRAMA PRINCIPAL.          
C                                                                       
      SUBROUTINE ADDAB(A,B,R,NL,NM,L,M)                                 
      IMPLICIT REAL*8(A-H,O-Z)                                          
      REAL*8 A(NL,NM),B(NL,NM),R(NL,NM)                              
      DO 5 J=1,M                                                        
      DO 5 I=1,L                                                        
    5 R(I,J)=A(I,J)+B(I,J)                                              
      RETURN                                                            
      END                                                               
      

