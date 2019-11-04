call date_and_time (date, t, zone, dt_init)
! Open output file.
OPEN ( UNIT=7, FILE=filename, ACCESS='SEQUENTIAL', &
    FORM='FORMATTED', STATUS='replace',IOSTAT=ierror )
IF (ierror==0) THEN ! Open successful
    ! Print out the input data
    CALL input()
    ! *************************************
    ! Initialize the operational parameters
    ! *************************************
    depth_bio = L_rec-depth_char
    ! For mass balance
    ! ----------------
    rho_char(1) = rho_biomass * (1.D0-ash/100.D0)* X1_char
    rho_m(1) = rho_biomass * (moisture/100.D0)
    rho_volt(1) = rho_biomass * (1.D0-ash/100.D0)* (1.D0-X1_char)
    m_g(1) = air_rate*4.D0/(pi*D_rec**2.D0*3600.D0)
    Y_O2(1) = mf_O2
    Y_N2(1) = 1.D0-mf_O2

    Y_CO(1) = 0.D0
    Y_CO2(1) = 0.D0
    Y_H2O(1) = 0.D0
    Y_H2(1) = 0.D0
    Y_CH4(1) = 0.D0
    Y_C2H4(1) = 0.D0
    Y_tar1(1) = 0.D0
    Y_tar2(1) = 0.D0
    mole_g(1) = Y_O2(1)*mole_O2+Y_CO(1)*mole_CO +Y_CO2(1)*mole_CO2+Y_H2O(1)*mole_H2O+ &
    Y_H2(1)*mole_H2+Y_CH4(1)*mole_CH4+Y_C2H4(1)*mole_C2H4+Y_tar1(1)*mole_tar1+&
    Y_tar2(1)*mole_tar2+Y_N2(1)*mole_N2
    ! For Energy balance
    ! ------------------
    P_Ts(1) = 0.D0
    Q_Ts(1) = T_a
    P_Tg(1) = 0.D0
    Q_Tg(1) = T_gas
    CALL initial()
    DO
    time=time+delta_t
    CALL pretime()
    iteration = 0
    DO i=2,N
    gasheat(i)=0.D0
    END DO 
    DO
    logic = 0
    DO i=2,N
    CALL mh_coefficient()
    CALL reaction_rates()
    END DO
    DO i=N,2,-1
    CALL solid_velocity()
    END DO

    DO i=2,N
    CALL particle_diameter()
    END DO
    sec_check=1

    DO i=2,N
    CALL solid_balance()
    CALL gas_balance()
    END DO
    T_sold(1)=T_s(1)
    T_gold(1)=T_g(1)
    DO i=2,N
    CALL wallh()
    CALL thomasenergy_balance()
    T_sold(i)=T_s(i)
    T_gold(i)=T_g(i)
    END DO
    CALL thomas(N,P_Tg,Q_Tg,T_g)
    CALL thomas(N,P_Ts,Q_Ts,T_s)
    DO i=1,N
    IF (ABS((T_s(i)-T_sold(i))/T_s(i))>error.OR.&
    ABS((T_g(i)-T_gold(i))/T_g(i))>error) logic=1
    END DO
    iteration = iteration + 1
    IF (logic==0) EXIT
    IF (iteration>1000)THEN
    Write(*,*)'------- CONVERGENCE PROBLEM ----------'
    EXIT
    END IF
    END DO
    Write(*,"(' ',T2,A7,T10,I6,T18,A4,T25,A18,T45,I5)")&
    "time =",NINT(time),"sec,","no of iteration =", iteration
    CALL NON_diment_Tg ()
    CALL static_rezone ()
    CALL coldgas_efficiency()
    IF (NINT(time)==300) call Tg_peak()
    IF (NINT(time)==600) call Tg_peak()
    IF (NINT(time)==900) call Tg_peak()
    IF (NINT(time)==1200)call Tg_peak()
    IF (NINT(time)==1500)call Tg_peak()
    IF (NINT(time)==1800)call Tg_peak()
    IF (NINT(time)==output_time) EXIT
    END DO
    CALL date_and_time (date, t, zone, dt_final)
    CALL mass_balance()
    CALL gasifier_performance()
    CALL output_summary() 
    CALL READ_TIME (run_time,dt_init,dt_final)
    Write(*,'(T2, A15, T17, I5, T22, A4, T27, F6.3, T33, A4)') &
    "Running time =",INT(run_time/60.) ,"min", mod(run_time,60.),"sec"
    Write(*,'(/T2, A58, T63, A)') &
    "See the result details in your defined output file called",filename
    CALL output_profile()
    CALL rates_output()
    CALL output_summary7()
END IF
