#include <stdint.h>
#include <math.h>

// inputs material element components
/*
	  SUBROUTINE SPEC(SUBST,NFORM,MMAX,JZ,WT,JING,EADD,NELL,SOURCE)
C     2 Apr 87. Reads various input parameters.
	  CHARACTER*72 FORMLA,FRM(100),SUBST
	  CHARACTER*30 ENGIN,SOURCE
	  CHARACTER  RESP*1                                                 SMS
	  PARAMETER (MEA=1200)                                              SMS
	  DIMENSION JZ(100),WT(100),JZ1(100),WT1(100),LH(100),WATE(100),
	 1 FRAC(100),EADD(MEA)
	  NFORM=3
   10 FORMAT(1H )
   12 FORMAT(A)
	  PRINT *,' Enter name of substance: '
	  READ 12, SUBST
	  PRINT 10
	  PRINT *,' Options for characterization of substance:'
	  PRINT *,'    1. Elemental substance, specified by atomic number'
	  PRINT *,'    2. Elemental substance, specified by chemical symbol'
	  PRINT *,'    3. Compound, specified by chemical formula'
	  PRINT *,'    4. Mixture of elements and/or compounds'
	  PRINT *,' Enter choice: '
	  READ *,NSUB
	  PRINT 10
	  GO TO(15,20,50,55),NSUB
   15 PRINT *,' Enter atomic number of element: '
	  READ *, JZ(1)
	  MMAX=1
	  WT(1)=1.0
	  GO TO 22
   20 PRINT *,' Enter chemical symbol for element: '
	  READ 12,FORMLA
	  PRINT 10
	  CALL FORM(FORMLA,MMAX,JZ,WT)
   22 PRINT *,' Options for output quantities:'
	  PRINT *,'     1. Cross sections in barns/atom'
	  PRINT *,'     2. Cross sections in barns/atom, and'
	  PRINT *,'        attenuation coefficients in cm2/g'
	  PRINT *,'     3. Partial interaction coefficients and'
	  PRINT *,'        attenuation coefficients in cm2/g'
	  PRINT *,' Enter choice: '
	  READ *,NFORM
	  PRINT 10
   25 MAX=MMAX-1
	  DO 40 M=1,MAX
	  MP1=M+1
	  DO 40 N=MP1,MMAX
	  IF(JZ(M)-JZ(N))40,40,30
   30 JZTEMP=JZ(M)
	  WTTEMP=WT(M)
	  JZ(M)=JZ(N)
	  WT(M)=WT(N)
	  JZ(N)=JZTEMP
	  WT(N)=WTTEMP
   40 CONTINUE
C     GO TO 150                                                         SMS
C        NOW THAT IN ASCENDING ORDER, COMBINE IDENTICAL Zs              SMS
	  M=0                                                               SMS
   42 M=M+1                                                             SMS
	  IF (M.GE.MMAX) GO TO 150                                          SMS
   44 IF (JZ(M).NE.JZ(M+1)) GO TO 42                                    SMS
	  WT(M)=WT(M)+WT(M+1)                                               SMS
	  MMAX=MMAX-1                                                       SMS
	  IF (M.EQ.MMAX) GO TO 150                                          SMS
	  DO 46 MM=M+1,MMAX                                                 SMS
	  JZ(MM)=JZ(MM+1)                                                   SMS
   46 WT(MM)=WT(MM+1)                                                   SMS
	  GO TO 44                                                          SMS
   50 PRINT *,'Enter chemical formula for compound: '
	  READ 12,FORMLA
	  PRINT 10
	  CALL FORM(FORMLA,MMAX,JZ,WT)
	  GO TO 25
   55 PRINT *,' How many components in mixture? Enter number: '
	  READ *,NCOMP
	  PRINT 10
	  DO 65 N=1,NCOMP
	  PRINT 60,N
   60 FORMAT(' Enter chemical symbol or formula for component',
	 1 I3,': ')
	  READ 12, FRM(N)
	  PRINT 61,N
   61 FORMAT(' Enter fraction by weight for component',I3,': ')
	  READ *, FRAC(N)
   65 CONTINUE
	  PRINT 10
	  SUMF=0.0
	  DO 70 N=1,NCOMP
   70 SUMF=SUMF+FRAC(N)
	  PRINT 72
   72 FORMAT('   Component    Fraction')
	  PRINT 73
   73 FORMAT('               by Weight')
	  PRINT 10
	  DO 75 N=1,NCOMP
	  PRINT 74,N,FRAC(N),FRM(N)
   74 FORMAT(I12,F12.6,6X,A)
   75 CONTINUE
	  PRINT 10
	  PRINT 80, SUMF
   80 FORMAT(6X,'Sum = ',F12.6)
	  PRINT 10
	  PRINT *,' Options for accepting or rejecting composition data:'
	  PRINT *,'     1. Accept, but let program normalize fractions'
	  PRINT *,'        by weight so that their sum is unity'
	  PRINT *,'     2. Reject, and enter different set of fractions'
	  PRINT *,' Enter choice: '
	  READ *,MSUMGO
	  PRINT 10
	  GO TO (85,55), MSUMGO
   85 DO 90 N=1,NCOMP
   90 FRAC(N)=FRAC(N)/SUMF
	  DO 95 L=1,100
   95 LH(L)=0
	  DO 120 N=1,NCOMP
	  CALL FORM (FRM(N),MAX,JZ1,WT1)
	  DO 120 M=1,MAX
	  IN=JZ1(M)
	  IF(LH(IN))100,100,110
  100 LH(IN)=1
	  WATE(IN)=FRAC(N)*WT1(M)
	  GO TO 120
  110 WATE(IN)=WATE(IN)+FRAC(N)*WT1(M)
  120 CONTINUE
	  LL=0
	  DO 140 L=1,100
	  IF(LH(L))140,140,130
  130 LL=LL+1
	  JZ(LL)=L
	  WT(LL)=WATE(L)
  140 CONTINUE
	  MMAX=LL
  150 PRINT *,' Options for energy list for output data:'
	  PRINT *,'     1. Standard energy grid only'
	  PRINT *,'     2. Standard grid plus additional energies'
	  PRINT *,'     3. Additional energies only'
	  PRINT *,' Enter choice: '
	  READ *,NELL
	  PRINT 10
	  GO TO (240,190,190), NELL
  190 PRINT *,' Modes of entering additional energies:'
	  PRINT *,'     1. Entry from keyboard'
	  PRINT *,'     2. Entry from prepared input file'
	  PRINT *,' Enter choice: '
	  READ *, INEN
	  GO TO (200,210), INEN
  200 PRINT *,' How many additional energies are wanted? '
	  READ *,JING
	  PRINT 10
	  DO 205 J=1,JING
	  IF(J-1)201,201,202
  201 PRINT *,' Enter first energy (in MeV): '
	  GO TO 203
  202 PRINT *,' Enter next energy (in MeV): '
  203 READ *, EADD(J)
  205 CONTINUE
	  PRINT 10
	  PRINT *,' Save additional energies in file? (Y/N): '              SMS
	  READ *,RESP                                                       SMS
	  IF (RESP.NE.'Y'.AND.RESP.NE.'y') GO TO 220                        SMS
	  PRINT *,' Specify file name to save this energy list.'            SMS
	  PRINT *,' (Specification can include drive and path): '           SMS
	  READ 12, ENGIN                                                    SMS
	  PRINT 10                                                          SMS
	  OPEN (UNIT=7,FILE=ENGIN)                                          SMS
	  WRITE (7,207) JING                                                SMS
  207 FORMAT (I6)                                                       SMS
	  WRITE (7,208) (EADD(J),J=1,JING)                                  SMS
  208 FORMAT (1P6E12.5)                                                 SMS
	  CLOSE (7)                                                         SMS
	  GO TO 220
  210 PRINT *,' Specify file that contains input energy list.'
	  PRINT *,' (Specification can include drive and path): '
	  READ 12, ENGIN
	  PRINT 10
	  OPEN (UNIT=7,FILE=ENGIN)
	  READ (7,*) JING,(EADD(J),J=1,JING)
	  CLOSE (7)
  220 DO 230 J=1,JING
  230 EADD(J)=EADD(J)*1.0E+06
	  CALL SORT (JING,EADD)                                             SMS
  240 CONTINUE                                                          SMS
C 240 PRINT *,' Options for entry of Database files:'
C     PRINT *,'    1. Read files from floppy-disk drive A'
C     PRINT *,'    2. Read files from floppy-disk drive B'
C     PRINT *,'    3. Read files from CURRENT directory on hard disk'
C     PRINT *,' Enter choice: '
C     READ *, INDAT
C     GO TO (241,242,243),INDAT
C 241 SOURCE='A:MDATX3.'
C     RETURN
C 242 SOURCE='B:MDATX3.'
C     RETURN
  243 SOURCE='MDATX3.'
	  RETURN
	  END
*/

// decomposes chemical formulas
/*
	  SUBROUTINE FORM (W,MMAX,JZ,WT)
C     24 Mar 87. Reads element symbols or chemical formulas.
	  CHARACTER*72,W
	  DIMENSION MASH1(26),MASH2(418),IC(72),K(72),JZ(100),NZ(100),
	 1 MS(100),ATWTS(100),WT(100)
	  INCLUDE 'HASH1.DAT'
	  INCLUDE 'HASH2.DAT'
	  INCLUDE 'ATWTS.DAT'
	  DO 116 L=1,72
	  IC(L)=ICHAR(W(L:L))
	  IF(IC(L)-32)101,102,103
  101 K(L)=1
	  GO TO 116
  102 K(L)=2
	  GO TO 116
  103 IF(IC(L)-48)104,105,105
  104 K(L)=1
	  GO TO 116
  105 IF(IC(L)-58)106,107,107
  106 K(L)=3
	  GO TO 116
  107 IF(IC(L)-65)108,109,109
  108 K(L)=1
	  GO TO 116
  109 IF(IC(L)-91)110,111,111
  110 K(L)=4
	  GO TO 116
  111 IF(IC(L)-97)112,113,113
  112 K(L)=1
	  GO TO 116
  113 IF(IC(L)-123)114,115,115
  114 K(L)=5
	  GO TO 116
  115 K(L)=1
  116 CONTINUE
	  L=1
	  M=0
  117 IF(K(L)-2)118,118,119
  118 L=L+1
	  GO TO 117
  119 LMIN=L
  120 KG=K(L)
	  IF(L-LMIN)130,130,140
  130 GO TO (150,150,150,160,150), KG
  140 GO TO (150,470,150,160,150), KG
  150 STOP 1
  160 KG1=K(L+1)
	  GO TO (170,180,180,180,240), KG1
  170 STOP 2
  180 ICC=IC(L)-64
	  JT=MASH1(ICC)
	  IF(JT)190,190,200
  190 STOP 3
  200 M=M+1
	  JZ(M)=JT
	  GO TO (170,210,230,220,240), KG1
  210 NZ(M)=1
	  GO TO 470
  220 NZ(M)=1
	  L=L+1
	  GO TO 120
  230 IN=L+1
	  GO TO 390
  240 ICC=9*IC(L+1)-10*IC(L)+9
	  IF(ICC-1)310,250,250
  250 IF(ICC-418)260,260,310
  260 IF(ICC-208)300,270,300
  270 M=M+1
	  IF(IC(L)-71)290,280,290
  280 JZ(M)=32
	  GO TO 330
  290 JZ(M)=84
	  GO TO 330
  300 JT=MASH2(ICC)
	  IF(JT)310,310,320
  310 STOP 4
  320 M=M+1
	  JZ(M)=JT
  330 KG2=K(L+2)
	  GO TO (340,350,380,360,370), KG2
  340 STOP 5
  350 NZ(M)=1
	  GO TO 470
  360 NZ(M)=1
	  L=L+2
	  GO TO 120
  370 STOP 6
  380 IN=L+2
  390 INN=IN
	  IS=0
	  NZ(M)=0
  400 IF(K(INN)-3)420,410,420
  410 IS=IS+1
	  MS(IS)=IC(INN)-48
	  INN=INN+1
	  GO TO 400
  420 ISM=IS
	  KFAC=1
  430 NZ(M)=NZ(M)+KFAC*MS(IS)
	  KFAC=10*KFAC
	  IS=IS-1
	  IF(IS)440,440,430
  440 IF(NZ(M))450,450,460
  450 STOP 7
  460 L=IN+ISM
	  GO TO 120
  470 MMAX=M
	  ASUM=0.0
	  DO 480 M=1,MMAX
	  JM=JZ(M)
  480 ASUM=ASUM+ATWTS(JM)*REAL(NZ(M))
	  DO 490 M=1,MMAX
	  JM=JZ(M)
  490 WT(M)=ATWTS(JM)*REAL(NZ(M))/ASUM
	  RETURN
	  END
*/

// sorts energies
/*
	  SUBROUTINE SORT (NMAX,E)                                          SMS
C     16 Jun 99. Sorts into monotonically increasing order.             SMS
	  DIMENSION E(1)                                                    SMS
	  DATA  EBIG/1.0E20/                                                SMS
	  DO 20 M=1,NMAX-1                                                  SMS
	  EMIN=EBIG                                                         SMS
	  DO 10 N=M,NMAX                                                    SMS
	  IF (E(N).GT.EMIN) GO TO 10                                        SMS
	  EMIN=E(N)                                                         SMS
	  NS=N                                                              SMS
   10 CONTINUE                                                          SMS
	  E(NS)=E(M)                                                        SMS
	  E(M)=EMIN                                                         SMS
   20 CONTINUE                                                          SMS
	  RETURN                                                            SMS
	  END                                                               SMS

	  SUBROUTINE MERGE(E1,K1,L1,MMAX,E2,K2,L2,NMAX)
C     24 Mar 87. Merges energy lists.
	  DIMENSION E1(1),K1(1),L1(1),E2(1),K2(1),L2(1)
	  DATA MLIM/200/
	  DO 50 N=1,NMAX
	  M=2
   10 IF(E2(N)-E1(M))30,30,20
   20 M=M+1
	  GO TO 10
   30 MC=M
	  MF=M+1
	  MMAX=MMAX+1
	  IF(MMAX-MLIM)35,35,32
   32 PRINT 33,MMAX,MLIM
   33 FORMAT(6H MMAX=,I3,3X,6H MLIM=,I3)
	  STOP
   35 DO 40 M=MMAX,MF,-1
	  E1(M)=E1(M-1)
	  K1(M)=K1(M-1)
   40 L1(M)=L1(M-1)
	  E1(MC)=E2(N)
	  K1(MC)=K2(N)
   50 L1(MC)=L2(N)
	  RETURN
	  END

	  SUBROUTINE REV(NMAX,X)
C     24 Mar 87. Reverses the order of lists.
	  DIMENSION X(1)
	  NH=NMAX/2
	  DO 10 N=1,NH
	  N1=NMAX-N+1
	  T=X(N1)
	  X(N1)=X(N)
   10 X(N)=T
	  RETURN
	  END
*/

// fit
/*
	  SUBROUTINE SCOF(X,F,NMAX,A,B,C,D)
C     22 Feb 83. Fits F as a function of X, and calculates
C                cubic spline coefficients A,B,C and D.
	  DIMENSION X(1),F(1),A(1),B(1),C(1),D(1)
	  M1=2
	  M2=NMAX-1
	  S=0.0
	  DO 10 M=1,M2
	  D(M)=X(M+1)-X(M)
	  R=(F(M+1)-F(M))/D(M)
	  C(M)=R-S
   10 S=R
	  S=0.0
	  R=0.0
	  C(1)=0.0
	  C(NMAX)=0.0
	  DO 20 M=M1,M2
	  C(M)=C(M)+R*C(M-1)
	  B(M)=(X(M-1)-X(M+1))*2.0-R*S
	  S=D(M)
   20 R=S/B(M)
	  MR=M2
	  DO 30 M=M1,M2
	  C(MR)=(D(MR)*C(MR+1)-C(MR))/B(MR)
   30 MR=MR-1
	  DO 40 M=1,M2
	  S=D(M)
	  R=C(M+1)-C(M)
	  D(M)=R/S
	  C(M)=C(M)*3.0
	  B(M)=(F(M+1)-F(M))/S-(C(M)+R)*S
   40 A(M)=F(M)
	  RETURN
	  END
*/

void XCOM_calculate_cubic_spline(size_t numpoints,  const double* x, const double* f, double* B, double* C, double* D)
{
	double S = 0.0;
	double R = 0.0;
	for (size_t i = 0; i < numpoints - 1; ++i) {
		double d = x[i + 1] - x[i];
		D[i] = d;
		double R = (f[i + 1] - f[i]) / d;
		C[i] = R - S;
		S = R;
	}
	S = 0.0;
	R - 0.0;
	C[0] = 0.0;
	C[numpoints-1] = 0;
	for (size_t i = 1; i < numpoints - 1; ++i) {
		C[i] = C[i]+R*C[i-1];
		double b = (x[i - 1] - x[i + 1]) * 2.0 - R * S;
		B[i] = b;
		S = D[i];
		R = S / b;
	}

	for (size_t i = numpoints - 2; i > 0; --i) {
		C[i] = (D[i] * C[i+1] - C[i])/B[i];
	}

	for (size_t i = 0; i < numpoints - 1; ++i) {
		S = D[i];
		R = C[i+1]-C[i];
		D[i]=R/S;
		C[i]*=3.0;
		B[i]=(f[i+1]-f[i])/S-(C[i]+R)*S;
	}
}


/*
	  SUBROUTINE BSPOL(S,X,A,B,C,D,N,G)
C     22 Feb 83. Evaluates cubic spline as function of S, to obtain
C                fitted result G.
	  DIMENSION X(1),A(1),B(1),C(1),D(1)
	  IF (X(1).GT.X(N)) GO TO 10
	  IDIR=0
	  MLB=0
	  MUB=N
	  GO TO 20
   10 IDIR=1
	  MLB=N
	  MUB=0
   20 IF (S.GE.X(MUB+IDIR)) GO TO 60
	  IF (S.LE.X(MLB+1-IDIR)) GO TO 70
	  ML=MLB
	  MU=MUB
	  GO TO 40
   30 IF (IABS(MU-ML).LE.1) GO TO 80
   40 MAV=(ML+MU)/2
	  IF (S.LT.X(MAV)) GO TO 50
	  ML=MAV
	  GO TO 30
   50 MU=MAV
	  GO TO 30
   60 MU=MUB+2*IDIR-1
	  GO TO 90
   70 MU=MLB-2*IDIR+1
	  GO TO 90
   80 MU=MU+IDIR-1
   90 Q=S-X(MU)
	  G=((D(MU)*Q+C(MU))*Q+B(MU))*Q+A(MU)
	  RETURN
	  END
*/

inline static size_t XCOM_find_bin(double t, size_t numpoints, const double* X)
{

	int inverse_dir;
	int64_t upper_bound, lower_bound;
	if (X[0] > X[numpoints - 1]) {
		inverse_dir = 1;
		upper_bound = 0;
		lower_bound = numpoints;
	}
	else {
		inverse_dir = 0;
		upper_bound = numpoints;
		lower_bound = 0;
	}
	size_t position;

	if (t >= X[upper_bound - 1 + inverse_dir]) {
		position = upper_bound - 1 + inverse_dir;
	}

	else if (t <= X[lower_bound - inverse_dir]) {
		position = lower_bound - inverse_dir;
	}
	else {
		int64_t upper = upper_bound, lower = lower_bound;
		do {
			position = (upper + lower) / 2;
			if (t >= X[position]) {
				lower = position;
			}
			else {
				upper = position;
			}
		} while (llabs(upper - lower) > 1);
		position = upper - 1 + inverse_dir;
	}

	return position;
}

static double XCOM_cubic_pline_interpolate(double t, size_t numpoints, const double* X, const double* A, const double* B, const double* C, const double* D)
{
	size_t position = XCOM_find_bin(t, numpoints, X);
	double Q = t - X[position];
	return ((D[position] * Q + C[position]) * Q + B[position])* Q + A[position];
}

/*
	  SUBROUTINE BLIN(S,X,Y,N,T)
C     12 Apr 87. Linear interpolation routine
	  DIMENSION X(1000),Y(1000)
	  IF (X(1).GT.X(N)) GO TO 10
	  IDIR=0
	  MLB=0
	  MUB=N
	  GO TO 20
   10 IDIR=1
	  MLB=N
	  MUB=0
   20 IF (S.GE.X(MUB+IDIR)) GO TO 60
	  IF (S.LE.X(MLB+1-IDIR)) GO TO 70
	  ML=MLB
	  MU=MUB
	  GO TO 40
   30 IF (IABS(MU-ML).LE.1) GO TO 80
   40 MAV=(ML+MU)/2
	  IF (S.LT.X(MAV)) GO TO 50
	  ML=MAV
	  GO TO 30
   50 MU=MAV
	  GO TO 30
   60 MU=MUB+2*IDIR-1
	  GO TO 90
   70 MU=MLB-2*IDIR+1
	  GO TO 90
   80 MU=MU+IDIR-1
   90 Q=S-X(MU)
	  T=Y(MU)+Q*(Y(MU+1)-Y(MU))/(X(MU+1)-X(MU))
	  RETURN
	  END

*/

static double XCOM_linear_interpolate(double t, size_t numpoints, const double* X, const double* Y)
{
	size_t position = XCOM_find_bin(t, numpoints, X);
	double Q = t - X[position];
	return Y[position] + Q * (Y[position + 1] - Y[position]) / (X[position + 1] - X[position]);
}


#define XCOM_EPAIR_NUCLEUS 1.022007E+06
#define XCOM_EPAIR_SHELL 2.044014E+06
#define XCOM_AVOGADRO 0.60221367

#include "XCOM.h"


#include <stdio.h>

#include <errno.h>
XCOM_API int XCOM_load_data(XCOM_element_info* elem, const char* filename)
{
	int error = 0;
	FILE* file = fopen(filename, "r");
	if(!file) {
		error = EACCES;
		goto Error;
	}
	if(fscanf(file, "%6d%"/*12.6*/"lf\n", &elem->Z, &elem->atomic_weight)!=2) {
		error = EIO;
		goto Error;
	}
	if (fscanf(file, "%6d%6d\n", &elem->shell_count, &elem->energy_count) != 2) {
		error = EIO;
		goto Error;
	}

	if(elem->shell_count > XCOM_MAX_SHELL_COUNT || elem->energy_count > XCOM_MAX_ENERGY_POINTS) {
		error = E2BIG;
		goto Error;
	}

	// basic shell info
	{
		// shell indexes
		{
			for (int i = 0; i < elem->shell_count; ++i) {
				if (i % 12 == 0 && i != 0) {
					if (fscanf(file, "\n") != 0) {
						error = EIO;
						goto Error;
					}
				}
				if (fscanf(file, "%6d", &elem->shell_indexes[i]) != 1) {
					error = EIO;
					goto Error;
				}
				if (elem->shell_indexes[i] >= elem->energy_count) {
					error = E2BIG;
					goto Error;
				}
				elem->shell_indexes[i] = elem->energy_count - elem->shell_indexes[i];
			}
			if (fscanf(file, "\n") != 0) {
				error = EIO;
				goto Error;
			}
		}
		// shell names
		{
			for (int i = 0; i < elem->shell_count; ++i) {
				if (fscanf(file, " %2s", &elem->shell_names[i]) != 1) {
					error = EIO;
					goto Error;
				}
			}
			if (fscanf(file, "\n") != 0) {
				error = EIO;
				goto Error;
			}
		}
		// shell energies
		{
			for (int i = 0; i < elem->shell_count; ++i) {
				if (i % 8 == 0 && i != 0) {
					if (fscanf(file, "\n") != 0) {
						error = EIO;
						goto Error;
					}
				}
				if (fscanf(file, "%"/*9.1*/"lf", &elem->shell_energies[i]) != 1) {
					error = EIO;
					goto Error;
				}
			}
			if (fscanf(file, "\n") != 0) {
				error = EIO;
				goto Error;
			}
		}
	}

	// energy and normal absorption lists
	{
		// energy list
		{
			for (int i = elem->energy_count - 1; i >=0 ; --i) {
				if (elem->energy_count - 1 - i % 6 == 0 && i != elem->energy_count - 1) {
					if (fscanf(file, "\n") != 0) {
						error = EIO;
						goto Error;
					}
				}
				if (fscanf(file, "%"/*13.5*/"lE", &elem->energies[i]) != 1) {
					error = EIO;
					goto Error;
				}
			}
			if (fscanf(file, "\n") != 0) {
				error = EIO;
				goto Error;
			}
		}
		// absorption lists
		{
			// coherent scatter
			{
				for (int i = elem->energy_count - 1; i >= 0; --i) {
					if (elem->energy_count - 1 - i % 8 == 0 && i != elem->energy_count - 1) {
						if (fscanf(file, "\n") != 0) {
							error = EIO;
							goto Error;
						}
					}
					if (fscanf(file, "%"/*10.3*/"lE", &elem->coherent_scatter[i]) != 1) {
						error = EIO;
						goto Error;
					}
				}
				if (fscanf(file, "\n") != 0) {
					error = EIO;
					goto Error;
				}
			}
			// incoherent scatter
			{
				for (int i = elem->energy_count - 1; i >= 0; --i) {
					if (elem->energy_count - 1 - i % 8 == 0 && i != elem->energy_count - 1) {
						if (fscanf(file, "\n") != 0) {
							error = EIO;
							goto Error;
						}
					}
					if (fscanf(file, "%"/*10.3*/"lE", &elem->incoherent_scatter[i]) != 1) {
						error = EIO;
						goto Error;
					}
				}
				if (fscanf(file, "\n") != 0) {
					error = EIO;
					goto Error;
				}
			}
			// photoelectric
			{
				for (int i = elem->energy_count - 1; i >= 0; --i) {
					if (elem->energy_count - 1 - i % 8 == 0 && i != elem->energy_count - 1) {
						if (fscanf(file, "\n") != 0) {
							error = EIO;
							goto Error;
						}
					}
					if (fscanf(file, "%"/*10.3*/"lE", &elem->photoelectric[i]) != 1) {
						error = EIO;
						goto Error;
					}
				}
				if (fscanf(file, "\n") != 0) {
					error = EIO;
					goto Error;
				}
			}
			// nucleus pair production
			{
				double value;
				for (int i = elem->energy_count - 1; i >= 0; --i) {
					if (elem->energy_count - 1 - i % 8 == 0 && i != elem->energy_count - 1) {
						if (fscanf(file, "\n") != 0) {
							error = EIO;
							goto Error;
						}
					}
					if (fscanf(file, "%"/*10.3*/"lE", &value) != 1) {
						error = EIO;
						goto Error;
					}
					if(i >= XCOM_PAIR_NUCLEUS_POINTS) {
						if (value != 0) {
							error = EIO;
							goto Error;
						}
					}
					else {
						elem->nucleus_pair_production[i] = value;
					}
				}
				if (fscanf(file, "\n") != 0) {
					error = EIO;
					goto Error;
				}
			}
			// electron pair production
			{
				double value;
				for (int i = elem->energy_count - 1; i >= 0; --i) {
					if (elem->energy_count - 1 - i % 8 == 0 && i != elem->energy_count - 1) {
						if (fscanf(file, "\n") != 0) {
							error = EIO;
							goto Error;
						}
					}
					if (fscanf(file, "%"/*10.3*/"lE", &value) != 1) {
						error = EIO;
						goto Error;
					}
					if (i >= XCOM_PAIR_SHELL_POINTS) {
						if (value != 0) {
							error = EIO;
							goto Error;
						}
					}
					else {
						elem->shell_pair_production[i] = value;
					}
				}
				if (fscanf(file, "\n") != 0) {
					error = EIO;
					goto Error;
				}
			}
		}
	}

	// shell characterization
	if(elem->shell_count > 0) {
		if (fscanf(file, "%d\n", &elem->shell_known_count) != 1) {
			error = EIO;
			goto Error;
		}
		if (elem->shell_known_count != elem->shell_count) {
			error = EFAULT;
			goto Error;
		}
		// lax
		{
			for (int i = elem->shell_known_count - 1; i >= 0; --i) {
				if (fscanf(file, "%d", &elem->shell_energy_points[i]) != 1) {
					error = EIO;
					goto Error;
				}
				if (elem->shell_energy_points[i] > XCOM_MAX_SHELL_POINTS) {
					error = EFAULT;
					goto Error;
				}
			}
			if (fscanf(file, "\n") != 0) {
				error = EIO;
				goto Error;
			}
		}
		//	eng
		{
			for (int shell = elem->shell_known_count - 1; shell >= 0; --shell) {
				for(int point = 0; point < elem->shell_energy_points[shell]; ++point) {
					if (fscanf(file, "%lf", &elem->shell_energy_levels[shell][point]) != 1) {
						error = EIO;
						goto Error;
					}
				}
				if (fscanf(file, "\n") != 0) {
					error = EIO;
					goto Error;
				}
			}
		}
		// phc
		{
			for (int shell = elem->shell_known_count - 1; shell >= 0; --shell) {
				for (int point = 0; point < elem->shell_energy_points[shell]; ++point) {
					if (fscanf(file, "%lf", &elem->shell_photon_capture[shell][point]) != 1) {
						error = EIO;
						goto Error;
					}
				}
				if (fscanf(file, "\n") != 0) {
					error = EIO;
					goto Error;
				}
			}
		}

	}

	for(int i = 0; i<elem->shell_count; ++i) {
		elem->shell_peak_difference[i] = 
			elem->photoelectric[elem->shell_indexes[i]]
			- elem->photoelectric[elem->shell_indexes[i] + 1];
	}

	// remove the points before peak?
	/*
	int new_item = 0;
	for(int i = 0; i<elem->energy_count; ++i) {
		int found = 0;
		for(int j = 0; j<elem->shell_count; ++j) {
			if(i == elem->shell_indexes[j] + 1) {
				found = 1;
				break;
			}
		}
		if(found) {
			continue;
		}
		elem->energies[new_item] = elem->energies[i];
		elem->coherent_scatter[new_item] = elem->coherent_scatter[i];
		elem->incoherent_scatter[new_item] = elem->incoherent_scatter[i];
		elem->photoelectric[new_item] = elem->photoelectric[i];
		elem->nucleus_pair_production[new_item] = elem->nucleus_pair_production[i];
		elem->shell_pair_production[new_item] = elem->shell_pair_production[i];
		new_item++;
	}
	elem->energy_count = new_item;
	*/


	// populate spline
	for(int i = 0; i<elem->energy_count; ++i) {
		elem->log_energies[i] = log(elem->energies[i]);
		elem->coherent_scatter_spline_A[i] = log(elem->coherent_scatter[i]);
		elem->incoherent_scatter_spline_A[i] = log(elem->incoherent_scatter[i]);
		elem->photoelectric_spline_A[i] = log(elem->photoelectric[i]);
	}
	for(int i = 0; i<XCOM_PAIR_NUCLEUS_POINTS; ++i) {
		double term = elem->energies[i];
		term = (term - XCOM_EPAIR_NUCLEUS) / term;
		elem->nucleus_pair_spline_A[i] = log(elem->nucleus_pair_production[i] / (term * term * term));
	}
	elem->nucleus_pair_spline_A[XCOM_PAIR_NUCLEUS_POINTS] 
		= 3.006275 * elem->nucleus_pair_spline_A[XCOM_PAIR_NUCLEUS_POINTS - 1]
		- 2.577757 * elem->nucleus_pair_spline_A[XCOM_PAIR_NUCLEUS_POINTS - 2]
		+ 0.571482 * elem->nucleus_pair_spline_A[XCOM_PAIR_NUCLEUS_POINTS - 3];

	for (int i = 0; i < XCOM_PAIR_SHELL_POINTS; ++i) {
		double term = elem->energies[i];
		term = (term - XCOM_EPAIR_SHELL) / term;
		elem->shell_pair_spline_A[i] = log(elem->shell_pair_production[i] / (term * term * term));
	}
	elem->shell_pair_spline_A[XCOM_PAIR_SHELL_POINTS]
		= 3.006275 * elem->shell_pair_spline_A[XCOM_PAIR_SHELL_POINTS - 1]
		- 2.577757 * elem->shell_pair_spline_A[XCOM_PAIR_SHELL_POINTS - 2]
		+ 0.571482 * elem->shell_pair_spline_A[XCOM_PAIR_SHELL_POINTS - 3];

	XCOM_calculate_cubic_spline(elem->energy_count, elem->log_energies, elem->coherent_scatter_spline_A,
		elem->coherent_scatter_spline_B, elem->coherent_scatter_spline_C, elem->coherent_scatter_spline_D);
	XCOM_calculate_cubic_spline(elem->energy_count, elem->log_energies, elem->incoherent_scatter_spline_A,
		elem->incoherent_scatter_spline_B, elem->incoherent_scatter_spline_C, elem->incoherent_scatter_spline_D);
	XCOM_calculate_cubic_spline(elem->energy_count, elem->log_energies, elem->photoelectric_spline_A,
		elem->photoelectric_spline_B, elem->photoelectric_spline_C, elem->photoelectric_spline_D);
	XCOM_calculate_cubic_spline(XCOM_PAIR_NUCLEUS_POINTS, elem->log_energies, elem->nucleus_pair_spline_A,
		elem->nucleus_pair_spline_B, elem->nucleus_pair_spline_C, elem->nucleus_pair_spline_D);
	XCOM_calculate_cubic_spline(XCOM_PAIR_SHELL_POINTS, elem->log_energies, elem->shell_pair_spline_A,
		elem->shell_pair_spline_B, elem->shell_pair_spline_C, elem->shell_pair_spline_D);

	for(int shell = 0; shell < elem->shell_known_count; ++shell) {
		for(int point = 0; point < elem->shell_energy_points[shell]; ++point) {
			elem->log_shell_energy_points[shell][point] 
				= log(1e6) + log(elem->shell_energy_levels[shell][point]);
			elem->adjusted_shell_photon_capture[shell][point]
				= log(elem->shell_photon_capture[shell][point]);
		}
	}

Error:
	if (file) {
		fclose(file);
	}
	return error;
}

XCOM_API double XCOM_coherent_scatter_cross_section(const XCOM_element_info* elem, double E)
{
	return 
		exp(XCOM_cubic_pline_interpolate(log(E), elem->energy_count, elem->log_energies,
			elem->coherent_scatter_spline_A, elem->coherent_scatter_spline_B,
			elem->coherent_scatter_spline_C, elem->coherent_scatter_spline_D));
}

XCOM_API double XCOM_coherent_scatter_attenuation(const XCOM_element_info* elem, double E)
{
	return XCOM_AVOGADRO / elem->atomic_weight *
		XCOM_coherent_scatter_cross_section(elem, E);
}

XCOM_API double XCOM_incoherent_scatter_cross_section(const XCOM_element_info* elem, double E)
{
	return 
		exp(XCOM_cubic_pline_interpolate(log(E), elem->energy_count, elem->log_energies,
			elem->incoherent_scatter_spline_A, elem->incoherent_scatter_spline_B,
			elem->incoherent_scatter_spline_C, elem->incoherent_scatter_spline_D));
}

XCOM_API double XCOM_incoherent_scatter_attenuation(const XCOM_element_info* elem, double E)
{
	return XCOM_AVOGADRO / elem->atomic_weight *
		XCOM_incoherent_scatter_cross_section(elem, E);
}

XCOM_API double XCOM_photoelectric_cross_section(const XCOM_element_info* elem, double E)
{
	// photon capture
	if(elem->shell_known_count > 0 && E < elem->shell_energies[0]) {
		int idx = 0;
		for (; E < elem->shell_energies[idx] && idx < elem->shell_known_count; ++idx);
		idx -= 1;
		double pdif = 0;
		if(E == elem->shell_energies[idx + 1]) {
			pdif = elem->shell_peak_difference[idx + 1];
		}
		return 
			exp(XCOM_linear_interpolate( log(E), elem->shell_energy_points[idx],
				elem->log_shell_energy_points[idx], elem->adjusted_shell_photon_capture[idx]))
				- pdif;
	}
	double pdif = 0;
	if(E == elem->shell_energies[0]) {
		pdif = elem->shell_peak_difference[0];
	}
	return 
		exp(XCOM_cubic_pline_interpolate(log(E), elem->energy_count, elem->log_energies,
			elem->photoelectric_spline_A, elem->photoelectric_spline_B,
			elem->photoelectric_spline_C, elem->photoelectric_spline_D))
			- pdif;
}

XCOM_API double XCOM_photoelectric_attenuation(const XCOM_element_info* elem, double E)
{
	return XCOM_AVOGADRO / elem->atomic_weight *
		XCOM_photoelectric_cross_section(elem, E);
}

XCOM_API double XCOM_nucleus_pair_production_cross_section(const XCOM_element_info* elem, double E)
{
	if (E < XCOM_EPAIR_NUCLEUS) {
		return 0;
	}
	double term = (E - XCOM_EPAIR_NUCLEUS) / E;
	return 
		exp(XCOM_cubic_pline_interpolate(log(E), XCOM_PAIR_NUCLEUS_POINTS + 1, elem->log_energies,
			elem->nucleus_pair_spline_A, elem->nucleus_pair_spline_B,
			elem->nucleus_pair_spline_C, elem->nucleus_pair_spline_D))
		* term * term * term;
}

XCOM_API double XCOM_nucleus_pair_production_attenuation(const XCOM_element_info* elem, double E)
{
	return XCOM_AVOGADRO / elem->atomic_weight *
		XCOM_nucleus_pair_production_cross_section(elem, E);
}

XCOM_API double XCOM_shell_pair_production_cross_section(const XCOM_element_info* elem, double E)
{
	if (E < XCOM_EPAIR_SHELL) {
		return 0;
	}
	double term = (E - XCOM_EPAIR_SHELL) / E;
	return 
		exp(XCOM_cubic_pline_interpolate(log(E), XCOM_PAIR_SHELL_POINTS + 1, elem->log_energies,
			elem->shell_pair_spline_A, elem->shell_pair_spline_B,
			elem->shell_pair_spline_C, elem->shell_pair_spline_D))
		* term * term * term;
}

XCOM_API double XCOM_shell_pair_production_attenuation(const XCOM_element_info* elem, double E)
{
	return XCOM_AVOGADRO / elem->atomic_weight *
		XCOM_shell_pair_production_cross_section(elem, E);
}

XCOM_API double XCOM_total_cross_section(const XCOM_element_info* elem, double E)
{
	return XCOM_coherent_scatter_cross_section(elem, E) 
		+ XCOM_incoherent_scatter_cross_section(elem, E)
		+ XCOM_photoelectric_cross_section(elem, E)
		+ XCOM_nucleus_pair_production_cross_section(elem, E)
		+ XCOM_shell_pair_production_cross_section(elem, E);
}
XCOM_API double XCOM_total_attenuation(const XCOM_element_info* elem, double E)
{
	return XCOM_AVOGADRO / elem->atomic_weight *
		XCOM_total_cross_section(elem, E);
}

XCOM_API double XCOM_scatter_cross_section(const XCOM_element_info* elem, double E)
{
	return XCOM_coherent_scatter_cross_section(elem, E)
		+ XCOM_incoherent_scatter_cross_section(elem, E);
}
XCOM_API double XCOM_scatter_attenuation(const XCOM_element_info* elem, double E)
{
	return XCOM_AVOGADRO / elem->atomic_weight*
		XCOM_scatter_cross_section(elem, E);
}

// index = Z - 1;
XCOM_element_info xcom_elements[XCOM_ELEMENT_COUNT] = { 0 };
double atomic_weights[XCOM_ELEMENT_COUNT] = {
		1.00794,       4.002602,      6.941,         9.012182,
		10.811,        12.011,        14.00674,      15.9994,
		18.9984032,    20.1797,       22.989768,     24.3050,
		26.981539,     28.0855,       30.973762,     32.066,
		35.4527,       39.948,        39.0983,       40.078,
		44.955910,     47.88,         50.9415,       51.9961,
		54.93805,      55.847,        58.93320,      58.69,
		63.546,        65.39,         69.723,        72.61,
		74.92159,      78.96,         79.904,        83.80,
		85.4678,       87.62,         88.90585,      91.224,
		92.90638,      95.94,         97.9072,      101.07,
	   102.9055,      106.42,        107.8682,      112.411,
	   114.82,        118.710,       121.75,        127.60,
	   126.90447,     131.29,        132.90543,     137.327,
	   138.9055,      140.115,       140.90765,     144.24,
	   144.9127,      150.36,        151.965,       157.25,
	   158.92534,     162.50,        164.93032,     167.26,
	   168.93421,     173.04,        174.967,       178.49,
	   180.9479,      183.85,        186.207,       190.2,
	   192.22,        195.08,        196.96654,     200.59,
	   204.3833,      207.2,         208.98037,     208.9824,
	   209.9871,      222.0176,      223.0197,      226.0254,
	   227.0278,      232.0381,      231.03588,     238.0289,
	   237.0482,      239.0522,      243.0614,      247.0703,
	   247.0703,      251.0796,      252.083,       257.0951
};


XCOM_API double XCOM_material_coherent_scatter_attenuation(const XCOM_material_info* mat, double E)
{
	double result = 0;
	for(size_t i = 0; i<mat->element_count; ++i) {
		int idx = mat->Z[i] - 1;
		result +=  mat->weight_portion[i]* XCOM_coherent_scatter_cross_section(&xcom_elements[idx], E) / atomic_weights[idx];
	}
	return result * XCOM_AVOGADRO;
}

XCOM_API double XCOM_material_incoherent_scatter_attenuation(const XCOM_material_info* mat, double E)
{
	double result = 0;
	for (size_t i = 0; i < mat->element_count; ++i) {
		int idx = mat->Z[i] - 1;
		result += mat->weight_portion[i] * XCOM_incoherent_scatter_cross_section(&xcom_elements[idx], E) / atomic_weights[idx];
	}
	return result * XCOM_AVOGADRO;
}

XCOM_API double XCOM_material_photoelectric_attenuation(const XCOM_material_info* mat, double E)
{
	double result = 0;
	for (size_t i = 0; i < mat->element_count; ++i) {
		int idx = mat->Z[i] - 1;
		result += mat->weight_portion[i] * XCOM_photoelectric_cross_section(&xcom_elements[idx], E) / atomic_weights[idx];
	}
	return result * XCOM_AVOGADRO;
}

XCOM_API double XCOM_material_nucleus_pair_production_attenuation(const XCOM_material_info* mat, double E)
{
	double result = 0;
	for (size_t i = 0; i < mat->element_count; ++i) {
		int idx = mat->Z[i] - 1;
		result += mat->weight_portion[i] * XCOM_nucleus_pair_production_cross_section(&xcom_elements[idx], E) / atomic_weights[idx];
	}
	return result * XCOM_AVOGADRO;
}

XCOM_API double XCOM_material_shell_pair_production_attenuation(const XCOM_material_info* mat, double E)
{
	double result = 0;
	for (size_t i = 0; i < mat->element_count; ++i) {
		int idx = mat->Z[i] - 1;
		result += mat->weight_portion[i] * XCOM_shell_pair_production_cross_section(&xcom_elements[idx], E) / atomic_weights[idx];
	}
	return result * XCOM_AVOGADRO;
}

XCOM_API double XCOM_material_total_attenuation(const XCOM_material_info* mat, double E)
{
	double result = 0;
	for (size_t i = 0; i < mat->element_count; ++i) {
		int idx = mat->Z[i] - 1;
		result += mat->weight_portion[i] * XCOM_total_cross_section(&xcom_elements[idx], E) / atomic_weights[idx];
	}
	return result * XCOM_AVOGADRO;
}

XCOM_API double XCOM_material_scatter_attenuation(const XCOM_material_info* mat, double E)
{
	double result = 0;
	for (size_t i = 0; i < mat->element_count; ++i) {
		int idx = mat->Z[i] - 1;
		result += mat->weight_portion[i] * XCOM_scatter_cross_section(&xcom_elements[idx], E) / atomic_weights[idx];
	}
	return result * XCOM_AVOGADRO;
}




enum XCOM_token_type {
	XCOM_token_type_invalid,
	XCOM_token_type_null,
	XCOM_token_type_number,
	XCOM_token_type_upper,
	XCOM_token_type_lower
};

static const char XCOM_char_type_table[256] = {
	1,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,2,2,
	2,2,2,2,2,2,2,2,0,0,
	0,0,0,0,0,3,3,3,3,3,
	3,3,3,3,3,3,3,3,3,3,
	3,3,3,3,3,3,3,3,3,3,
	3,0,0,0,0,0,0,4,4,4,
	4,4,4,4,4,4,4,4,4,4,
	4,4,4,4,4,4,4,4,4,4,
	4,4,4,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0
};

static int XCOM_char_type(char ch) 
{
	return XCOM_char_type_table[(unsigned char)ch];
}

static const char XCOM_single_char_element_table[26] = {
	0,5,6,0,0,9,0,1,53,0,19,0,0,7,8,15,0,0,16,0,92,23,74,0,39,0
};

static const char XCOM_double_char_element_table[418] = {
	70,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,54,0,0,0,73,0,0,0,0,0,0,0,0,65,0,0,0,0,0,0,0,0,43,51,88,
	0,0,0,0,0,0,0,21,37,0,0,0,0,0,0,52,0,0,0,91,0,0,0,0,0,34,0,0,82,0,0,0,0,0,0,75,30,0,0,11,0,0,
	90,0,0,0,46,0,41,
	0,0,22,0,0,0,0,0,0,0,57,0,14,45,0,0,0,60,0,0,0,0,0,40,
	0,0,10,0,0,81,0,0,0,0,0,0,0,0,69,0,0,0,0,0,0,0,0,0,62,0,0,0,0,0,12,0,0,50,0,0,31,0,28,0,0,0,0,
	86,0,0,0,0,0,0,0,0,0,0,61,0,0,0,3,0,0,0,2,64,0,0,0,0,0,38,
	0,72,84,0,0,0,20,0,0,0,80,0,26,0,0,0,56,0,0,0,0,0,0,
	25,0,0,0,0,0,59,0,93,42,48,0,0,44,0,0,0,0,0,58,0,89,0,0,78,76,0,0,98,
	4,0,0,0,94,0,0,0,0,0,0,49,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,36,47,0,67,
	0,100,0,0,0,83,0,0,0,0,0,0,0,71,0,0,77,5 * 0,17,97,
	0,0,0,0,0,0,0,96,0,0,0,0,0,0,0,0,0,0,13,0,0,0,87,0,0,27,0,95,0,0,0,0,68,0,0,0,0,0,0,0,0,99,0,0,0,0,0,0,0,0,0,0,24,
	0,0,0,0,0,0,63,0,55,35,0,0,0,0,0,0,0,0,0,18,0,0,0,0,0,0,29,0,33,0,0,0,0,0,0,0,0,85,0,0,0,0,0,0,0,0,79,0,0,0,0,0,66
};

#include <ctype.h>

#include <string.h>

XCOM_API int XCOM_parse_formula(XCOM_material_info* mat, const char* formula)
{
	memset(mat, 0, sizeof(mat));
	size_t pos = 0;
	size_t elem_count = 0;
	int current_Z = 0;
	int current_count = 0;
	int XCOM_token_type;
	while(1) {
		XCOM_token_type = XCOM_char_type(formula[pos]);
		switch(XCOM_token_type) {
			default:
				return 0;
			case XCOM_token_type_null:
				goto DONE;
				break;
			case XCOM_token_type_number:
				if(pos == 0) {
					return 0;
				}
				current_count = 0;
				while (isdigit(formula[pos])) {
					current_count *= 10;
					current_count += formula[pos] - '0';
					++pos;
				};
				break;
			case XCOM_token_type_upper:
				switch(XCOM_char_type(formula[pos + 1])) {
					default:
						return 0;
					case XCOM_token_type_upper:
					case XCOM_token_type_null:
						current_Z = XCOM_single_char_element_table[formula[pos] - 'A'];
						current_count = 1;
						++pos;
						break;
					case XCOM_token_type_number:
						current_Z = XCOM_single_char_element_table[formula[pos] - 'A'];
						++pos;
						continue;
					case XCOM_token_type_lower:
						{
							int index = 9 * formula[pos + 1] - 10 * formula[pos] + 8;
							if (index < 0 || index >= 418) {
								return 0;
							}
							if (index == 207) {
								if (formula[pos] == 'G') {
									current_Z = 32;
								}
								else {
									current_Z = 84;
								}
							}
							else {
								current_Z = XCOM_double_char_element_table[index];
							}
						}
						pos += 2;
						switch (XCOM_char_type(formula[pos])) {
							default:
								return 0;
							case XCOM_token_type_null:
							case XCOM_token_type_upper:
								current_count = 1;
								break;
							case XCOM_token_type_number:
								continue;
						}
						break;
				}
				break;
		}
		if(current_Z == 0 || current_count == 0) {
			return 0;
		}
		int found = 0;
		for(size_t i = 0; i<elem_count; ++i) {
			if(mat->Z[i] == current_Z) {
				mat->counts[i] += current_count;
				found = 1;
				current_Z = 0;
				current_count = 0;
				break;
			}
		}
		if(!found) {
			if (elem_count + 1 >= XCOM_ELEMENT_COUNT) {
				break;
			}
			mat->Z[elem_count] = current_Z;
			mat->counts[elem_count] = current_count;
			current_Z = 0;
			current_count = 0;
			++elem_count;
		}
	}
DONE:
	// overflow return
	mat->element_count = elem_count;
	double weight = 0;
	for (size_t i = 0; i < elem_count; ++i) {
		weight += mat->counts[i] * atomic_weights[mat->Z[i] - 1];
	}
	mat->total_weight = weight;
	for (size_t i = 0; i < elem_count; ++i) {
		mat->weight_portion[i] = mat->counts[i] * atomic_weights[mat->Z[i] - 1]/weight;
	}
	return 1;
}


XCOM_API int XCOM_material_mix_into(XCOM_material_info* mat, double portion_2, const XCOM_material_info* mat2)
{
	size_t count1 = mat->element_count;
	size_t count2 = mat2->element_count;
	for (size_t i = 0; i < count1; ++i) {
		mat->weight_portion[i] *= 1 - portion_2;
	}

	for (size_t j = 0; j < count2; ++j) {
		int found = 0;
		for (size_t i = 0; i < count1; ++i) {
			if (mat2->Z[j] == mat->Z[i]) {
				mat->weight_portion[i] += mat2->weight_portion[j] * portion_2;
				found = 1;
				break;
			}
		}
		if (!found) {
			if (count1 + 1 >= XCOM_ELEMENT_COUNT) {
				return 0;
			}
			mat->Z[count1] = mat2->Z[j];
			mat->weight_portion[count1] = mat2->weight_portion[j] * portion_2;
			++count1;
		}
	}
	mat->element_count = count1;
	return 1;
}


XCOM_API int XCOM_material_mix(XCOM_material_info* mat, size_t portion_count, const XCOM_material_info* portions, double* weight_factor, int normalize) {

	if(normalize) {
		double weight_sum = 0;
		for(size_t i = 0; i< portion_count; ++i) {
			weight_sum += weight_factor[i];
		}
		for (size_t i = 0; i < portion_count; ++i) {
			weight_factor[i] /= weight_sum;
		}
	}
	memset(mat, 0, sizeof(mat));
	size_t elem_count = 0;
	for (size_t portion = 0; portion < portion_count; ++portion) {
		for (size_t element = 0; element < portions[portion].element_count; ++element) {
			int found = 0;
			for(size_t existing = 0; existing < elem_count; ++existing) {
				if (mat->Z[existing] == portions[portion].Z[element]) {
					mat->weight_portion[existing] += portions[portion].weight_portion[element] * weight_factor[portion];
					found = 1;
					break;
				}
			}
			if(!found) {
				if (elem_count + 1 >= XCOM_ELEMENT_COUNT) {
					return 0;
				}
				mat->Z[elem_count] = portions[portion].Z[element];
				mat->weight_portion[elem_count] = portions[portion].weight_portion[element] * weight_factor[portion];
				++elem_count;
			}
		}
	}
	mat->element_count = elem_count;
	return 1;
}

#define XCOM_DATA_PREFIX "XCOM/" /*"XCOM/"*/
#define XCOM_DATA_PREFIX_LEN sizeof(XCOM_DATA_PREFIX) - 1


XCOM_API const struct XCOM_element_info* XCOM_get_element(int Z)
{
	if (xcom_elements[Z - 1].Z != Z) {
		char filename_buffer[] = XCOM_DATA_PREFIX"MDATX3.000";
		filename_buffer[XCOM_DATA_PREFIX_LEN + 7] = '0' + Z / 100;
		filename_buffer[XCOM_DATA_PREFIX_LEN + 8] = '0' + (Z % 100) / 10;
		filename_buffer[XCOM_DATA_PREFIX_LEN + 9] = '0' + Z % 10;
		if (XCOM_load_data(&xcom_elements[Z - 1], filename_buffer) < 0) {
			return 0;
		}
	}
	return &xcom_elements[Z - 1];
}

XCOM_API int XCOM_load_material(const XCOM_material_info* mat)
{
	if (mat->element_count >= XCOM_ELEMENT_COUNT) {
		return 0;
	}

	for (size_t i = 0; i < mat->element_count; ++i) {
		int Z = mat->Z[i];
		if (!XCOM_get_element(Z)) {
			return 0;
		}
	}
	return 1;
}




