 PROGRAM SuperCairtical_Fluid_Drying
! C AUTHOR: Kunkun Guo
! !C DATE:   Aug 13, 2023
! C (�����ڴ���ȡ�����е��ܼ�Ũ���趨Ϊ0)
! C (���ڴ���ȡ�����е��ܼ�Ũ��������������ݶ�Ϊ0)!***��������
! C (���洦��Cv-Cs�йص��ȡ��ֵ)
! C PURPOSE
! C ���㳬�ٽ���ȡ�����У�Բ�������������ܼ��Ҵ����滻�Ĺ���
! C ���������Ҵ�Ũ�ȷֲ���ʱ��ı仯
! C ������������������ȣ������������Ũ���ݶ�
! C NUMERICAL METHODS
! C Բ������ϵ����ת�ԳƵ�����ά����
! C �̶������Ŀռ仮��
! C ���淽����ʽ������
! C DIFFUSIVITY MODEL
! C �ο����״�����ɢϵ�������Ũ�ȵ������Ժ��� 
! C ������ʱ�䲽������ƥ�䣬������ܳ���Ũ��Ϊ���������Σ�
! C ��һ��Ӧ�ÿ���Cv���㷽�������⣺��ʽ��⣿�޶�������
! C 20150819:����飬Ҳ����ΪCv���㲿�ֵĳ�����ִ������£���ʽ��⣬��©��ԭʼֵ������������������
! C INPUT DATA
! C Cai   ���������ܼ��ĳ�ʼŨ�� ��kmol/m3��16.5
! C De 	  ��Ч��ɢϵ����m2/s��4.7��4.9��5.1����10-9m2/s��
! C Kx 	  ��Ч��������ϵ����m s-1��4.92��8.58 ��10-4 ��m s-1��
! C vz    ������ʵ����٣�m s-1��0.0024-0.013
! C Rv&ra �������������İ뾶(m) 0.5&1.0
! C L     �������߶ȣ�m��5.0~10 cm
! C ������������
! C C��0��RN+1��0��HN+1��	 �Ҵ�Ũ�ȷֲ�
! C Ca��0��RN��1��HN�� 		 �������ڵ�����
! C Csl(0��RN) Csu(0��RN)	 �������¡��ϱ����Ũ��
! C CsLateral(0��HN+1)		 �����������Ũ��
! C Cvl(0��RN) Cvt(0��RN)	 ���������¡��ϱ���Ӵ�����ȡ�������Ҵ���Ũ��
! C CvLateral��0��HN+1�� 	 ������������Ӵ�����ȡ�������ܼ��Ҵ���Ũ��
! C Csl(0��RN)=C��0��RN��1��
! C Csu(0��RN)=C��0��RN��HN��
! C CsLateral(0��HN+1)=C��RN��1��HN��
! C
! C Cvl(0��RN)=C��0��RN��0��
! C Cvt(0��RN)=C��0��RN��HN+1��
! C CvLateral(0��HN+1)=C��RN+1��0��HN+1��
! C Dter2 			dt*De/dr/dr
! C Dtez2 			dt*De/dz/dz
! C Dtkxr			dt*Kx/dr
! C Dzkxde			dz*Kx/De
! C Drkxde			dr*Kx/De
! C Dtkxra2			dt*Kx*Ra/(Rv*Rv-Ra*Ra)/2
! C R(0:RN)			�������
! C********************************************************************
!C
!C		USE DFLIB
!		USE IMSL
!C		USE DFPORT 
	
		!Implicit Real*8 (a-h, o-z)

		INTEGER,PARAMETER :: RN  = 15
		INTEGER,PARAMETER :: HN  = 151
		INTEGER,PARAMETER :: RN1 = 16
		INTEGER,PARAMETER :: HN1 = 152 ! RN1 & HN1 must equal to RN+1 and HN+1
		Real*8, PARAMETER :: PAI = 3.1415926535897932384626433 
		
		CHARACTER*10 	CN
		CHARACTER*10 	FileName
		Character*80	FilePath
		 
		REAL*8, DIMENSION(0: RN1, 1: HN) :: C
		REAL*8, DIMENSION(0: RN,  1: HN) :: Ca
		
		REAL*8, DIMENSION(0: RN)  :: R, MULTIR
		REAL*8, DIMENSION(1: HN)  :: MULTIH
		
		INTEGER 	I,J,K,N_Iter, NIter, N_Pro, N_log
		REAL(4)		TimeI, TA(2),Monitor
		REAL*8 		De, Kx, Dt, Rv, Ra, Vz, L
		REAL*8 		Ti, Tf, Dr, Dz, Cai,DRDH2PI,Residual_C
		REAL*8 		Dter2, Dtez2, Dtkxr, Dzkxde,Drkxde, Dtkxra2
		REAL*8		AR0, BR0, CR0
		REAL*8		AH0, BH0, CH0, CvMax, CvMin, Volume
		
! �ж��Ƿ�Ũ�ȵ���
        Monitor = 1
! ��õ�ǰĿ¼λ��
	
! READ THE INPUT DATA
! ���ɲ����ļ�����ʾ���֣������������޸��ã�(��������1��ζ�������޸Ĳ�����)

		Call Parameter_ImPut(2)

!C
!C SET THE INITIAL CONDITIONS
!C
! integerate coefficient
			MULTIR = 1.0D0

			MULTIR(0) = 0.5D0;  MULTIR(RN) = 0.5D0

			MULTIH = 1.0D0

			MULTIH(1) = 0.5D0;  MULTIH(HN) = 0.5D0   
! space & time steps
			DR = Ra/Dble(RN)

			Dz = L/Dble(HN-1)

			DRDH2PI = DR * Dz * 2.0 * PAI

			Do I = 0, RN
				R(i)=dble(i)*Dr
			Enddo
			
			C(0:RN,1:HN)  = Cai    	! in alcohol-gel 
! solvent in fluids
			C(RN1, 1:HN) = 0.0d0	! lateral Surface

! ������ϵ�����
			Ca(0:RN,1:HN) = 1.0D0
			Call AVERAGE(Ca, Volume)
			write(*,*) "the volume of the aerogel cylinder is: ", Volume
			write(*,*) "dr,dz is:", dr, dz

! solvent in gel and on its surfaces

			Ca(0:RN,1:HN) = Cai ! (0:RN,1:HN)

! ��¼�������ܼ�������ʱ��仯		
			open(21,FILE='output\log.dat')  
				Write(21, '(2A30)') "Time(s)", "Residual_Solvent(Kg)"
				Call AVERAGE(Ca, Residual_C)
				Write(21, '(2F30.10)') 0.0d0, Residual_C/Volume/Cai 
			close(21)
		
			CALL COEFFICIENT	! ���Է������ϵ��
	
			Call OutPut(2)		! ���ģ�Ͳ���

! ��ѭ����ʼ��	
			
			Do N_Iter = 1, NIter
			    
				CAll DIFFUSION(C)
				
! ������ȡ��Ũ�ȱ仯�۲�
				IF((mod(N_Iter, N_log)==0) .and. (N_Iter .LT. N_Pro))Then
					write (CN, '(i10.10)') N_Iter
					Call OutPut(3) ! 3 ��ȡ��Ũ��
				EndIF

! ����������ܼ��ܶȷֲ�
				IF (mod(N_Iter, N_Pro)==0) then
				!	Ca(0:RN,1:HN) = C(0:RN,1:HN)
					write (CN, '(i10.10)') N_Iter  
					Call OutPut(0) ! 0 Ũ�ȷֲ�  !���ѡ��������������������Կ���
					Call OutPut(3) ! 3 ��ȡ��Ũ��
					write(*,*) "CN is:", CN
				! �۲����������ܼ�Ũ���Ƿ���ָ�����
					If(Monitor==1) then
						CvMax = 0.0D0; CvMin = Cai
						Do I = 0,RN
							DO J = 1, HN
								IF(CvMax.LT.C(I,J)) CvMax = C(I,J)
								IF(CvMin.GT.C(I,J)) CvMin = C(I,J)
							Enddo
						Enddo					
						Write(*,*) "N_Iter, CvMax, CvMin is:", N_Iter, CvMax, CvMin 
					EndIf			
				Endif
! �����������ܼ���������¼
				IF (mod(N_Iter, N_log)==0) then
					Ca(0:RN,1:HN) = C(0:RN,1:HN)
					Call AVERAGE(Ca, Residual_C)
					Call OutPut(1)	! ʣ���ܼ�	
				Endif

	IF(mod(N_Iter, N_Pro)==0)then 
	write(*,*) "N_Iter, Residual_C/Volume/Cai is:",N_Iter, Residual_C/Volume/Cai   ! ���������Ļ��ʾ
    endif
			Enddo

			TimeI = DTIME(TA)
			write(*,*) 'Program has been running for', TimeI, 'seconds.'
			write(*,*) ' This includes', TA(1), 'seconds of user time and', TA(2), 'seconds of system time.'


	 CONTAINS
!***************************************************************************
	!=======================================================================
	!======			SUBROUTINES										 ======!
	!=======================================================================
!===============================================================================
!===============================================================================
	SUBROUTINE COEFFICIENT
	!========================================================================!
	!	coefficent for diffusion equation							 !
	!========================================================================!
		Dter2	=	dt*De/dr/dr
		Dtez2	=	dt*De/dz/dz
		Dtkxr	=	dt*Kx/dr
		Dzkxde	=	dz*Kx/De
		Drkxde	=	dr*Kx/De
		Dtkxra2	=	dt*Kx*Ra/(Rv*Rv-Ra*Ra)

		AR0 = Dter2/2.0d0   !  Ar(i) = Ar0 * (1-0.5*dr/r(i))
		BR0 = -Dter2-1.0d0  ! Br(i) = Br0
		CR0 = Dter2/2.0d0   ! Ar(i) = Ar0 * (1+0.5*dr/r(i))

		AH0 = Dtez2/2.0d0   ! AH(j) = AH0 
		BH0 = -Dtez2 - 1.0d0
		CH0 = Dtez2/2.0d0
		
		write(*,*) "dter2,dtez2,dtkxr is:",dter2,dtez2,dtkxr
		write(*,*) "dzkxde,drkxde,dtkxra2 is:",dzkxde,drkxde,dtkxra2

	END SUBROUTINE COEFFICIENT	

!**********************************************************************************
!**********************************************************************************
		SUBROUTINE DIFFUSION(Q) 
		REAL*8, DIMENSION(0: RN1, 1:HN) :: Q, QH, QIN,QOUT
		REAL*8, DIMENSION(0: RN) :: AR, BR, CR, XR, UR
		REAL*8, DIMENSION(1: HN ) :: AH, BH, CH, XH, UH
		REAL*8 	TmpL,TmpR,TmpU
		INTEGER N, I, J, K  
	
		QH = 0.0D0			! ��ʱ�䲽��������ֵ
		QIN(:,:) = Q(:,:)	! ����ԭʼŨ�ȷֲ�
! ������ȡ���ܼ�Ũ�ȣ����ʱ�䲽��			
		I = RN1
		Do K = 1, HN		! ���ճ�������©����һ�+ Qin(I,K)
			IF(K==1) then  
QH(I,K) = 0.0 ! Dtkxra2*(Qin(I-1,K)-Qin(I,K))-0.50D0*Dt*Vz/Dz*(Qin(I,K+1)-Qin(I,K))+Qin(I,K) 
			ElseIf(K==HN) then
QH(I,K) = Dtkxra2*(Qin(I-1,K)-Qin(I,K))-0.50D0*Dt*Vz/Dz*(Qin(I,K)-Qin(I,K-1))+Qin(I,K)
			Else
QH(I,K) = Dtkxra2*(Qin(I-1,K)-Qin(I,K))-0.50D0*Dt*Vz/Dz*(Qin(I,K+1)-Qin(I,K-1))+Qin(I,K)
			EndIf
		Enddo
! SOLVING IN R DIRECTION
! ���������±��棬���ñ߽�����ȷ���Ҷ����еĻ���
! �뾶Ϊ0������Լ��������뾶Ra������Ҫ���öԳ��Լ��߽��������޸���β�����Է���
! �����������ܼ�Ũ�ȣ�����һ�����

		N = RN1		! �����Ϸ��̵ĸ�����

		DO J = 1, HN	! �������ڲ�
		! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
			I = 0
			AR(I) = 0.0d0
			BR(I) = BR0
			CR(I) = CR0*2.0d0
			IF(J==1) Then
				TmpL  = 0.0D0	!	
				XR(I) = -Qin(I,J)-AH0*(TmpL-2.0d0*Qin(I,J)+Qin(I,J+1))
			ElseIf(J==HN) then
				TmpU  = Qin(I,J-1) 
				XR(I) = -Qin(I,J)-AH0*(Qin(I,J-1) + TmpU - 2.0d0*Qin(I,J))
			Else
				XR(I) = -Qin(I,J)-AH0*(Qin(I,J-1) + Qin(I,J+1)-2.0d0*Qin(I,J))
			EndIf
			
			DO I = 1, RN-1
				AR(I) = AR0 *(1.0d0 - 0.50d0*Dr/R(I))
				BR(I) = BR0
				CR(I) = CR0 *(1.0d0 + 0.50d0*Dr/R(I))
				IF(J==1) THEN
					TmpL  = 0.0D0		!	�±���Ϊ����ȡҺ 
					XR(I) = -Qin(I,J)-AH0*(TmpL-2.0d0*Qin(I,J)+Qin(I,J+1))
				ELSEIf(J==HN) Then
					TmpU  = Qin(I,J-1)	!	
					XR(I) = -Qin(I,J)-AH0*(Qin(I,J-1) + TmpU - 2.0d0*Qin(I,J))
				ELSE
					XR(I) = -Qin(I,J)-AH0*(Qin(I,J-1) + Qin(I,J+1)-2.0d0*Qin(I,J))
				END IF
			ENDDO

! be careful ! �߽���������Cv-Cs��ϵ��Ҳ��������ϵ��	
! C(i+1) = C(i-1)+2*DrKxDe*(Cv-Cs) = C(i-1)+2*DrKxDe*(C(i+1)-C(i))
! AR(I) = AR0 *(1.0d0 - 0.50d0*Dr/R(I)); BR(I) = BR0; CR(I) = CR0 *(1.0d0 + 0.50d0*Dr/R(I))		
			I = RN		
			AR(I) = 2.0D0 * AR0
			BR(I) = BR0 - 2.0d0*AR0*Drkxde*(1.0d0+0.50d0*Dr/R(I))
			TmpR  = - 2.0d0*AR0*Drkxde*(1.0d0+0.50d0*Dr/R(I)) 
			! ��cv��Ϊ��֪������һʱ�䲽�Ľ�����
			CR(I) = 0.0D0
			IF(J==1) Then
				TmpL  = 0.0D0	
	XR(I) = -Qin(I,J)-AH0*(TmpL-2.0d0*Qin(I,J)+Qin(I,J+1))+TmpR*QH(I+1,J)
			ElseIf(J==HN) then
				TmpU  = Qin(I,J-1)
	XR(I) = -Qin(I,J)-AH0*(Qin(I,J-1) + TmpU - 2.0d0*Qin(I,J))+TmpR*QH(I+1,J)
			Else
	XR(I) = -Qin(I,J)-AH0*(Qin(I,J-1) + Qin(I,J+1)-2.0d0*Qin(I,J))+TmpR*QH(I+1,J)
			EndIf

		!	CALL DLSLTR (N, AR, BR, CR, XR)
			call tridagr(AR,BR,CR,XR,UR,RN)
	
			DO I = 0, RN
				QH(I,J) = UR(I)
			ENDDO		

	    ENDDO	

! �±���������Ũ��Ϊ0���ϱ������ݶ�Ϊ0��
		
! SOLVING IN HEIGHT DIRECTION ******************************** 

		N = HN
		
		QOUT = 0.0D0
		
! ������ȡ���ܼ�Ũ�ȣ����ʱ�䲽��			
		I = RN1
		Do K = 1, HN
			IF(K==1) then
Qout(I,K) = 0.0  ! Dtkxra2*(QH(I-1,K)-QH(I,K))-0.50D0*Dt*Vz/Dz*(QH(I,K+1)-QH(I,K)) + QH(I,K)
			ElseIf(K==HN) then
Qout(I,K) = Dtkxra2*(QH(I-1,K)-QH(I,K))-0.50D0*Dt*Vz/Dz*(QH(I,K)-QH(I,K-1)) + QH(I,K)
			Else
Qout(I,K) = Dtkxra2*(QH(I-1,K)-QH(I,K))-0.25D0*Dt*Vz/Dz*(QH(I,K+1)-QH(I,K-1)) + QH(I,K)
			EndIf
		Enddo
		
		N = HN  !	��ȥ���������ܼ�Ũ��δ֪֮�⣬�����Ϊ��֪��
		  
		DO I = 0, RN !  ! �����飬��������Ӱ����
			DO J = 1, HN 
			  IF(J==1) then
					AH(J) = 0.0d0
					BH(J) = BH0	! 
					CH(J) = 1.0D0 * CH0 
					IF(I==RN) then
TmpR  = 2.0d0 * Drkxde * (Qout(RN1,J) - QH(I,J) ) + QH(I-1, J) 
XH(J) = -QH(I,J)-0.50d0 * Dt * De * ( (TmpR-2.0d0*QH(I,J)+QH(I-1,J))/Dr/Dr+0.50D0*Dr*(TmpR-QH(I-1,J))/R(i) )
					ElseIf(I==0) Then
XH(J) = -QH(I,J)- Dt * De * ( (QH(I+1,J)-QH(I,J))/Dr/Dr )
  					Else
XH(J) = -QH(I,J)-0.50d0 * Dt * De * ((QH(I+1,J)-2.0d0*QH(I,J)+QH(I-1,J))/Dr/Dr+0.5D0*Dr*(QH(I+1,J)-QH(I-1,J))/R(i) )
					EndIf

			ElseIf(J==HN) Then
					AH(J) = 2.0D0 * AH0
					BH(J) = BH0 
					CH(J) = 0.0d0				
					IF(I==RN) then
	TmpR = -2.0d0 * Drkxde * (QH(I,J) - Qout(RN1,J)) + QH(I-1, J) ! 
XH(J) = -QH(I,J)-0.50d0 * Dt * De * ( (TmpR-2.0d0*QH(I,J)+QH(I-1,J))/Dr/Dr+0.5D0*Dr*(TmpR-QH(I-1,J))/R(i) )
					ElseIf(I==0) Then
XH(J) = -QH(I,J)- Dt * De * (QH(I+1,J)-QH(I,J))/Dr/Dr
					Else
XH(J) = -QH(I,J)-0.50d0 * Dt * De * ( (QH(I+1,J)-2.0d0*QH(I,J)+QH(I-1,J))/Dr/Dr+0.5D0*Dr*(QH(I+1,J)-QH(I-1,J))/R(i) )
					EndIf

			Else
					AH(J) = AH0
					BH(J) = BH0
					CH(J) = CH0 
				
					IF(I==RN) then
TmpR = -2.0d0 * Drkxde * (QH(I,J) - Qout(RN1,J)) + QH(I-1, J)  ! Cv-Cs
XH(J) = -QH(I,J)-0.50d0 * Dt * De * ( (TmpR-2.0d0*QH(I,J)+QH(I-1,J))/Dr/Dr+0.5D0*Dr*(TmpR-QH(I-1,J))/R(i) )
					ElseIf(I==0) Then
XH(J) = -QH(I,J) -  Dt * De * ( (QH(I+1,J)- QH(I,J))/Dr/Dr )
					Else
XH(J) = -QH(I,J) - 0.50d0 * Dt * De * (	(QH(I+1,J)-2.0d0*QH(I,J)+QH(I-1,J))/Dr/Dr+0.5D0*Dr*(QH(I+1,J)-QH(I-1,J))/R(i) )
					EndIf

				EndIf
			
			Enddo

		!	CALL DLSLTR (N, AH, BH, CH, XH)
			
			call tridagh(AH,BH,CH,XH,UH,N)
			DO K = 1, N
				QOUT(I, K) = UH(K)
			END DO
					
		ENDDO
		
		Q(:,:) = QOUT(:,:)
	
	END SUBROUTINE DIFFUSION
!========================================================================== 
!==========================================================================
	Subroutine output(IO)
	
	INTEGER :: IO

	if (IO == 0) then
		
		open(21,file='output\Profile'//CN//'.dat')

		Do I = 0, RN
			Do J = 1, HN
	
				Write(21, '(2I5,F30.10)') I, J, C(I,J)
	
			Enddo
		Enddo
		
		Close(21)
		
		open(21,file='output\press'//CN//'.dat')

		Do I = 1, RN
			Do J = 2, HN
	
				Write(21, '(2I5,2F30.10)') I, J, (C(i-1,j) - C(I,J)),(c(i,j)-c(i,j-1))
	
			Enddo
		Enddo
		
		Close(21)

	endif

	if (IO == 1) then
		open(21,FILE='output\log.dat', POSITION="APPEND",STATUS="OLD")
		Write(21, '(2F30.10)') Dble(N_Iter)*Dt, Residual_C/Volume/Cai
         close(21)
	endif

	IF(IO==2) then
			OPEN (Unit= 1, FILE='output\PARA_List.dat')
				Write (1,99) "File_Name"
				Write (1,*)  "Ethanol"
				WRITE (1,99) "�������뾶��ra&Rv, m��"
				Write (1,'(2F30.20)')  Ra, Rv
				WRITE (1,99) "�������߶ȣ�L, m��"
				Write (1,'(F30.20)')  L
				WRITE (1,99) "��ȡ��������(vz, m/s��"
				Write (1,'(F30.20)')  vz
				WRITE (1,99) "������ʼŨ�ȣ�Cai, kmol/m3��"
				Write (1,'(F30.20)')  Cai
				WRITE (1,99) "��Ч����ϵ����Kx, m/s��"
				Write (1,'(F30.20)')  Kx
				WRITE (1,99) "��Ч��ɢϵ��(De, m2/s)"
				Write (1,'(F30.20)')  De
				WRITE (1,99) "ʱ�䲽��(Dt,s)"
				Write (1,'(F30.20)')  Dt
				WRITE (1,99) "�ռ䲽����Dr&Dz,m��"
				Write (1,'(2F30.20)')  Dr, Dz
				WRITE (1,99) "��ʱ�䲽�����ܶȷֲ����Ƶ�ʣ��ܼ���ͳ��Ƶ��"
				Write (1,'(3I30)')  NIter, N_Pro, N_log
				WRITE (1,99) "��ϵ���"
				Write (1,'(F30.20)')  Volume
		
			CLOSE (UNIT=1)
	EndIf

	if (IO == 3) then
		
		open(21,file='output\Cv'//CN//'.dat')

		I = RN1
		Do J = 1, HN
	
			Write(21, '(I5,F30.10)') J, C(I,J)
	
		Enddo
				
		Close(21)

	endif

99		FORMAT (A)
	End Subroutine OutPut
!========================================================================== 
!==========================================================================
!*******************************************************************

	SUBROUTINE AVERAGE(DUMBA,DUMBB)

	REAL*8, DIMENSION(0: RN, 1: HN) :: DUMBA 

	REAL*8 DUMBB	

		DUMBB = 0.0D0

		DO I = 0, RN

			DO J = 1, HN

 DUMBB = DUMBB + DUMBA(I, J)* R(I)* MULTIR(I)*MULTIH(J) * DRDH2PI

			ENDDO

		ENDDO   

	END SUBROUTINE AVERAGE
!**********************************************************************************
		Subroutine Parameter_ImPut(Nx)
		Integer   Nx  ! ����д���Ʊ���	
		If(Nx==1) then
			OPEN (Unit= 1, FILE='input\PARAMETER.dat')
				Write (1,99) "File_Name"
				Write (1,*)  "Ethanol"
				WRITE (1,99) "�������뾶��ra&Rv, m��"
				Write (1,*)  0.015, 0.025
				WRITE (1,99) "�������߶ȣ�L, m��"
				Write (1,*)  0.15
				WRITE (1,99) "��ȡ��������(vz, m/s��"
				Write (1,*)  0.0025
				WRITE (1,99) "������ʼŨ�ȣ�Cai, kmol/m3��"
				Write (1,*)  16.5
				WRITE (1,99) "��Ч����ϵ����Kx, m/s��"
				Write (1,*)  8.58D-4
				WRITE (1,99) "��Ч��ɢϵ��(De, m2/s)"
				Write (1,*)  5.1D-9
				WRITE (1,99) "ʱ�䲽��(Dt,s)"
				Write (1,*)  0.0125
				WRITE (1,99) "�ռ䲽����Dr&Dz,m��"
				Write (1,*)  0.0005,0.0005
				WRITE (1,99) "��ʱ�䲽�����ܶȷֲ����Ƶ�ʣ��ܼ���ͳ��Ƶ��"
				Write (1,*)  1000000, 100000, 10000
		
			CLOSE (UNIT=1)
		endif		
		
		OPEN (Unit= 1, FILE='input\PARAMETER.dat')	
			READ (1,*) 
			READ (1,*) FileName
			READ (1,99)
			READ (1,*) Ra, Rv
			READ (1,99)
			READ (1,*) L
			READ (1,99)
			READ (1,*) vz
			READ (1,99)
			READ (1,*) Cai
			READ (1,99)
			READ (1,*) Kx
			READ (1,99)
			READ (1,*) De
			READ (1,99)
			READ (1,*) Dt
			READ (1,99)
			READ (1,*) Dr, Dz ! �������룬��R&L��RN&HN���㼴�ɣ�
			READ (1,99)
			READ (1,*) NIter, N_Pro, N_log
		CLOSE (UNIT=1)
99		FORMAT (A)
		End Subroutine Parameter_ImPut
!**********************************************************************************
SUBROUTINE tridagr(aa,bb,cc,rr,uu,nn)
      INTEGER nn
      REAL*8 aa(0:nn),bb(0:nn),cc(0:nn),rr(0:nn),uu(0:nn)
       REAL*8 bet,gam(0:nn)
 !     if(bb(0).eq.0.)pause 'tridag: rewrite equations'
      bet=bb(0)
      uu(0)=rr(0)/bet
      do j=1,nn
        gam(j)=cc(j-1)/bet
        bet=bb(j)-aa(j)*gam(j)
      !  if(bet.eq.0.) pause 'tridag failed'
        uu(j)=(rr(j)-aa(j)*uu(j-1))/bet
      end do
      do j=nn-1,0,-1
        uu(j)=uu(j)-gam(j+1)*uu(j+1)
      end do
      
 END subroutine

 SUBROUTINE tridagh(aa,bb,cc,rr,uu,nn)
	  integer nn
 	  REAL*8 aa(1:nn),bb(1:nn),cc(1:nn),rr(1:nn),uu(1:nn)
     
      REAL*8 bet,gam(1:nn)
      
      bet=bb(1)
      uu(1)=rr(1)/bet
      do j=2,nn
        gam(j)=cc(j-1)/bet
        bet=bb(j)-aa(j)*gam(j)
        
        uu(j)=(rr(j)-aa(j)*uu(j-1))/bet
      end do
      do j=nn-1,1,-1
        uu(j)=uu(j)-gam(j+1)*uu(j+1)
      end do
      return
      END
!**************************************************************************
	END PROGRAM
!**************************END OF THE PROGRAMM*****************************
!========================================================================== 
!==========================================================================

