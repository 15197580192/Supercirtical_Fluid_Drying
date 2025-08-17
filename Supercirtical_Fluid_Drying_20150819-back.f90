 PROGRAM SuperCairtical_Fluid_Drying
! C AUTHOR: Kunkun Guo
! !C DATE:   Aug 13, 2023
! C (将进口处萃取介质中的溶剂浓度设定为0)
! C (出口处萃取介质中的溶剂浓度在流动方向的梯度为0)!***不够合理？
! C (表面处于Cv-Cs有关的项，取负值)
! C PURPOSE
! C 计算超临界萃取过程中，圆柱形凝胶柱中溶剂乙醇被替换的过程
! C 凝胶柱内乙醇浓度分布随时间的变化
! C 凝胶柱外的流体侧向均匀，流动方向存在浓度梯度
! C NUMERICAL METHODS
! C 圆柱坐标系，旋转对称的拟三维问题
! C 固定步长的空间划分
! C 交替方向隐式差分求解
! C DIFFUSIVITY MODEL
! C 参考文献处理：扩散系数对组分浓度的依赖性忽略 
! C 流速与时间步长必须匹配，否则可能出现浓度为负数的情形！
! C 下一步应该考虑Cv计算方法的问题：显式求解？限定条件？
! C 20150819:经检查，也许因为Cv计算部分的程序出现错误所致！显式求解，遗漏了原始值，仅计算了增量（）
! C INPUT DATA
! C Cai   凝胶柱中溶剂的初始浓度 （kmol/m3）16.5
! C De 	  等效扩散系数（m2/s）4.7、4.9、5.1（×10-9m2/s）
! C Kx 	  等效质量交换系数（m s-1）4.92、8.58 ×10-4 （m s-1）
! C vz    流体介质的流速（m s-1）0.0024-0.013
! C Rv&ra 容器及凝胶柱的半径(m) 0.5&1.0
! C L     凝胶柱高度（m）5.0~10 cm
! C 其他变量定义
! C C（0：RN+1，0：HN+1）	 乙醇浓度分布
! C Ca（0：RN，1：HN） 		 凝胶柱内的区域
! C Csl(0：RN) Csu(0：RN)	 凝胶柱下、上表面的浓度
! C CsLateral(0：HN+1)		 凝胶柱侧面的浓度
! C Cvl(0：RN) Cvt(0：RN)	 与凝胶柱下、上表面接触的萃取流体中乙醇的浓度
! C CvLateral（0：HN+1） 	 与凝胶柱侧面接触的萃取流体中溶剂乙醇的浓度
! C Csl(0：RN)=C（0：RN，1）
! C Csu(0：RN)=C（0：RN，HN）
! C CsLateral(0：HN+1)=C（RN，1：HN）
! C
! C Cvl(0：RN)=C（0：RN，0）
! C Cvt(0：RN)=C（0：RN，HN+1）
! C CvLateral(0：HN+1)=C（RN+1，0：HN+1）
! C Dter2 			dt*De/dr/dr
! C Dtez2 			dt*De/dz/dz
! C Dtkxr			dt*Kx/dr
! C Dzkxde			dz*Kx/De
! C Drkxde			dr*Kx/De
! C Dtkxra2			dt*Kx*Ra/(Rv*Rv-Ra*Ra)/2
! C R(0:RN)			径向距离
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
		
! 判断是否浓度低于
        Monitor = 1
! 获得当前目录位置
	
! READ THE INPUT DATA
! 生成参数文件的提示文字，帮助参数表修改用！(参数等于1意味着重新修改参数！)

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

! 计算体系的体积
			Ca(0:RN,1:HN) = 1.0D0
			Call AVERAGE(Ca, Volume)
			write(*,*) "the volume of the aerogel cylinder is: ", Volume
			write(*,*) "dr,dz is:", dr, dz

! solvent in gel and on its surfaces

			Ca(0:RN,1:HN) = Cai ! (0:RN,1:HN)

! 记录凝胶中溶剂含量随时间变化		
			open(21,FILE='output\log.dat')  
				Write(21, '(2A30)') "Time(s)", "Residual_Solvent(Kg)"
				Call AVERAGE(Ca, Residual_C)
				Write(21, '(2F30.10)') 0.0d0, Residual_C/Volume/Cai 
			close(21)
		
			CALL COEFFICIENT	! 线性方程组的系数
	
			Call OutPut(2)		! 输出模型参数

! 主循环开始：	
			
			Do N_Iter = 1, NIter
			    
				CAll DIFFUSION(C)
				
! 早期萃取相浓度变化观察
				IF((mod(N_Iter, N_log)==0) .and. (N_Iter .LT. N_Pro))Then
					write (CN, '(i10.10)') N_Iter
					Call OutPut(3) ! 3 萃取相浓度
				EndIF

! 输出凝胶中溶剂密度分布
				IF (mod(N_Iter, N_Pro)==0) then
				!	Ca(0:RN,1:HN) = C(0:RN,1:HN)
					write (CN, '(i10.10)') N_Iter  
					Call OutPut(0) ! 0 浓度分布  !输出选项，可以设置其他变量加以控制
					Call OutPut(3) ! 3 萃取相浓度
					write(*,*) "CN is:", CN
				! 观察凝胶柱中溶剂浓度是否出现负数！
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
! 计算凝胶中溶剂含量并记录
				IF (mod(N_Iter, N_log)==0) then
					Ca(0:RN,1:HN) = C(0:RN,1:HN)
					Call AVERAGE(Ca, Residual_C)
					Call OutPut(1)	! 剩余溶剂	
				Endif

	IF(mod(N_Iter, N_Pro)==0)then 
	write(*,*) "N_Iter, Residual_C/Volume/Cai is:",N_Iter, Residual_C/Volume/Cai   ! 计算进度屏幕提示
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
	
		QH = 0.0D0			! 半时间步长的输入值
		QIN(:,:) = Q(:,:)	! 输入原始浓度分布
! 更新萃取相溶剂浓度（半个时间步）			
		I = RN1
		Do K = 1, HN		! 往日程序中遗漏了这一项！+ Qin(I,K)
			IF(K==1) then  
QH(I,K) = 0.0 ! Dtkxra2*(Qin(I-1,K)-Qin(I,K))-0.50D0*Dt*Vz/Dz*(Qin(I,K+1)-Qin(I,K))+Qin(I,K) 
			ElseIf(K==HN) then
QH(I,K) = Dtkxra2*(Qin(I-1,K)-Qin(I,K))-0.50D0*Dt*Vz/Dz*(Qin(I,K)-Qin(I,K-1))+Qin(I,K)
			Else
QH(I,K) = Dtkxra2*(Qin(I-1,K)-Qin(I,K))-0.50D0*Dt*Vz/Dz*(Qin(I,K+1)-Qin(I,K-1))+Qin(I,K)
			EndIf
		Enddo
! SOLVING IN R DIRECTION
! 凝胶柱上下表面，利用边界条件确定右端项中的积分
! 半径为0的左端以及凝胶柱半径Ra处，需要利用对称性及边界条件，修改首尾的线性方程
! 侧面流体中溶剂浓度，可以一并求解

		N = RN1		! 径向上方程的个数！

		DO J = 1, HN	! 凝胶柱内部
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
					TmpL  = 0.0D0		!	下表面为新萃取液 
					XR(I) = -Qin(I,J)-AH0*(TmpL-2.0d0*Qin(I,J)+Qin(I,J+1))
				ELSEIf(J==HN) Then
					TmpU  = Qin(I,J-1)	!	
					XR(I) = -Qin(I,J)-AH0*(Qin(I,J-1) + TmpU - 2.0d0*Qin(I,J))
				ELSE
					XR(I) = -Qin(I,J)-AH0*(Qin(I,J-1) + Qin(I,J+1)-2.0d0*Qin(I,J))
				END IF
			ENDDO

! be careful ! 边界条件处：Cv-Cs，系数也仅含传质系数	
! C(i+1) = C(i-1)+2*DrKxDe*(Cv-Cs) = C(i-1)+2*DrKxDe*(C(i+1)-C(i))
! AR(I) = AR0 *(1.0d0 - 0.50d0*Dr/R(I)); BR(I) = BR0; CR(I) = CR0 *(1.0d0 + 0.50d0*Dr/R(I))		
			I = RN		
			AR(I) = 2.0D0 * AR0
			BR(I) = BR0 - 2.0d0*AR0*Drkxde*(1.0d0+0.50d0*Dr/R(I))
			TmpR  = - 2.0d0*AR0*Drkxde*(1.0d0+0.50d0*Dr/R(I)) 
			! 将cv视为已知，用上一时间步的结果替代
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

! 下表面流体相浓度为0，上表面则梯度为0！
		
! SOLVING IN HEIGHT DIRECTION ******************************** 

		N = HN
		
		QOUT = 0.0D0
		
! 更新萃取相溶剂浓度（半个时间步）			
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
		
		N = HN  !	除去凝胶柱内溶剂浓度未知之外，其余均为已知。
		  
		DO I = 0, RN !  ! 经检验，迭代次序不影响结果
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
				WRITE (1,99) "凝胶柱半径（ra&Rv, m）"
				Write (1,'(2F30.20)')  Ra, Rv
				WRITE (1,99) "凝胶柱高度（L, m）"
				Write (1,'(F30.20)')  L
				WRITE (1,99) "萃取介质流速(vz, m/s）"
				Write (1,'(F30.20)')  vz
				WRITE (1,99) "凝胶初始浓度（Cai, kmol/m3）"
				Write (1,'(F30.20)')  Cai
				WRITE (1,99) "等效传质系数（Kx, m/s）"
				Write (1,'(F30.20)')  Kx
				WRITE (1,99) "等效扩散系数(De, m2/s)"
				Write (1,'(F30.20)')  De
				WRITE (1,99) "时间步长(Dt,s)"
				Write (1,'(F30.20)')  Dt
				WRITE (1,99) "空间步长（Dr&Dz,m）"
				Write (1,'(2F30.20)')  Dr, Dz
				WRITE (1,99) "总时间步数，密度分布输出频率，溶剂量统计频率"
				Write (1,'(3I30)')  NIter, N_Pro, N_log
				WRITE (1,99) "体系体积"
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
		Integer   Nx  ! 读、写控制变量	
		If(Nx==1) then
			OPEN (Unit= 1, FILE='input\PARAMETER.dat')
				Write (1,99) "File_Name"
				Write (1,*)  "Ethanol"
				WRITE (1,99) "凝胶柱半径（ra&Rv, m）"
				Write (1,*)  0.015, 0.025
				WRITE (1,99) "凝胶柱高度（L, m）"
				Write (1,*)  0.15
				WRITE (1,99) "萃取介质流速(vz, m/s）"
				Write (1,*)  0.0025
				WRITE (1,99) "凝胶初始浓度（Cai, kmol/m3）"
				Write (1,*)  16.5
				WRITE (1,99) "等效传质系数（Kx, m/s）"
				Write (1,*)  8.58D-4
				WRITE (1,99) "等效扩散系数(De, m2/s)"
				Write (1,*)  5.1D-9
				WRITE (1,99) "时间步长(Dt,s)"
				Write (1,*)  0.0125
				WRITE (1,99) "空间步长（Dr&Dz,m）"
				Write (1,*)  0.0005,0.0005
				WRITE (1,99) "总时间步数，密度分布输出频率，溶剂量统计频率"
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
			READ (1,*) Dr, Dz ! 无需输入，由R&L，RN&HN计算即可！
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

