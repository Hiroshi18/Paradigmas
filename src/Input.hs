module Input where

import Control.Monad
-- This is for the user to enter the input parameter.
user_input = do
    putStrLn " ------------------------------------------------------------------------------"
    putStrLn " TRANSIENT MODEL FOR STRATIFIED DOWNDRAFT WOOD GASIFIER "
    putStrLn " ------------------------------------------------------------------------------"
    putStrLn " This program is a transient model of a stratified downdraft gasifier. Model is"
    putStrLn " able to predict the transient behivour of reactor namely, solid and gas phase"
    putStrLn " temperarue profile, velocity profile, reaction rates profile, gas composition"
    putStrLn " profile and condensable tar profile along the axis of gasifer."
    putStrLn  " -------------------------------- "
    putStrLn  " User defined gasifier Dimensions "
    putStrLn  " -------------------------------- "
    putStrLn  "Enter the inner diameter of the reactor in metre (for base case = 0.206)"
    d_rec <- getDouble
    putStrLn  "Enter the effective length of the reactor in metre (for base case = 0.7)"
    l_rec <- getDouble
    putStrLn  " ---------------------------- "
    putStrLn " User defined fuel properties "
    putStrLn " ---------------------------- "
    putStrLn  "Enter the apparent density of biomass in [kg d.b/m3] (for base case = 950)"
    rho_biomass <- getDouble
    putStrLn  "Enter the moisture content of the fuel in [wt d.b %] (for base case = 10)"
    moisture <- getDouble
    putStrLn  "Enter the initial wood particle diameter in metre (for base case = 0.02)"
    d_pwood <- getDouble
    putStrLn  "Enter the bed void fraction (for base case = 0.46) "
    void <- getDouble
    putStrLn  " ----------------------------------- "
    putStrLn " User defined operational parameters "
    putStrLn " ----------------------------------- "
    putStrLn  "Model is able to run for single-stage and two-stage"

    menu

    putStrLn  "Enter the pressure of inlet air, [KPa] (atmosperic condtiion = 101.325)"
    pre <- getDouble
    putStrLn  "Enter the ambient temperature in K (for normal condition = 300)" 
    t_a <- getDouble
    putStrLn  "Enter the inlet air temperature in K (for base case = 300)"
    t_gas <- getDouble
    putStrLn  "Enter the ignition temperature in K"
    putStrLn  "(in the range of 900-1200 K, for base case = 1000)"
    t_ign <- getDouble
    putStrLn  "Enter the initial depth of the ignition in meter (for base case= 0.05)"
    depth_ign <- getDouble
    putStrLn  "Enter the location of the ignition from the reactor top in meter "
    putStrLn  "(for base case = 0.2)"
    loc_ign <- getDouble
    putStrLn  "Enter the depth of char bed at initial condition in meter"
    putStrLn  "(for base case = 0.5)"
    depth_char <- getDouble
    putStrLn  "Enter the time to write the output result in second"
    putStrLn  "It can be divided by 3 (e.g, 300, 999, 1200) (base case for 1800)"
    output_time <- getDouble
    
    putStrLn "Enter '1' for sensitivity analysis of model parameter"
    putStrLn  "Enter '0' for to run the program without sensitivity analysis"
    check_sensitivity <- readLn
    putStrLn ""
            
    -- Defined the model parameter for base run
    let zeta = 0.60 -- Reacting to non-reacting heat transfer coefficient ratio [-]
    let km_max = 0.045 -- Defined maximum mass transfer coefficient, [m/s]
    let av_mf = 1.0 -- Ratio to effective and actural transport exchanged area, [-]
    let ap1_mf = 1.00 -- Multiplication factor of primary pyrolysis rate, [-]
    let ap2_mf = 1.0 -- Multiplication factor of secondary pyrolysis rate, [-]
    let a6_mf = 1.0 -- Multiplication factor of secondary tar oxidation rate, [-]
    let keff_mf = 1.0 -- Multiplication factor to effictive thermal condutivity, [-]
    let wall_loss = 1.0 -- Multiplication factor of wall heat transfer coefficient, [-]

    if (check_sensitivity==0) then
        return ()
    else if (check_sensitivity==1) then
        print_option
    else
        putStrLn  "Error in enter value, it must be either '0' or '1'"

    --putStrLn "Enter result output file name: It must be less than 30 characters."
    --putStrLn "Please aware that your file name must be new in the folder.If your"
    --putStrLn "defined file name already exist in the folder, it will be replaced."
    --filename <- readLn
    putStrLn ""

print_option = do
    putStrLn "Effect of eight model parameter can be examined by changing the"
    putStrLn  "single parameter while all other parameter are kept constant."
    putStrLn "Effect of which parameter do you want to analyse?"
    putStrLn "Enter '1' to analyse the reacting to non-reacting"
    putStrLn "solid-to-gas heat transfer coefficient ratio"
    putStrLn  "Enter '2' to analyse defined maximum mass transfer coefficient"
    putStrLn  "Enter '3' to analyse the ratio to effective and actural"
    putStrLn "heat and mass transfer surface area"
    putStrLn  "Enter '4' to analyse the primary pyrolysis rate"
    putStrLn  "Enter '5' to analyse the secondary pyrolysis rate"
    putStrLn  "Enter '6' to analyse the secondary tar oxidation rate"
    putStrLn  "Enter '7' to analyse the effictive thermal condutivity"
    putStrLn "Enter '8' to analyse the wall heat transfer coefficient"
    analysis <- getDouble
    check_analysis analysis

read_check_analysis_1 = do
    putStrLn  "Enter the reacting to non-reacting heat transfer coefficient ratio"
    putStrLn  " it must be less than '1', (for base case = 0.6)"
    zeta <- getDouble
    putStrLn ""

read_check_analysis_2 = do
    putStrLn  "Enter the defined maximum mass transfer coefficient"
    putStrLn  " it must be in a range of 0.04-0.06 (for base case = 0.045)"
    km_max <- getDouble
    putStrLn ""

read_check_analysis_3 = do
    putStrLn  "Enter the ratio to effective and actural transport exchanged area"
    putStrLn  " it should be in a range of 0.8-1.2 (for base case = 1)"
    av_mf <- getDouble
    putStrLn ""

read_check_analysis_4 = do
    putStrLn  "Enter the multiplication factor of primary pyrolysis rate"
    putStrLn  " it should be in a range of 0.5-2 (for base case = 1)"
    ap1_mf <- getDouble
    putStrLn ""
    
read_check_analysis_5 = do
    putStrLn  "Enter the multiplication factor of secondary pyrolysis rate"
    putStrLn  " it should be in a range of 0.7-1.3 (for base case = 1)"
    ap2_mf <- getDouble
    putStrLn ""

read_check_analysis_6 = do
    putStrLn  "Enter the multiplication factor of secondary tar oxidation rate"
    putStrLn  " it should be in a range of 0.5-3 (for base case = 1)"
    a6_mf <- getDouble
    putStrLn ""

read_check_analysis_7 = do
    putStrLn  "Enter the multiplication factor to Effictive thermal condutivity"
    putStrLn  " it should be in a range of 0.5-3 (for base case = 1)"
    keff_mf <- getDouble
    putStrLn ""

read_check_analysis_8 = do
    putStrLn  "Enter the multiplication factor of wall heat transfer coefficient"
    putStrLn  " it should be in a range of 0.5-1.5 (for base case = 1)"
    wall_loss <- getDouble
    putStrLn ""

read_check_stage_1 = do
    putStrLn  "Enter the air supply rate, [kg/hr] (for base case = 15)";
    air_rate <- getDouble;
    let secondair = 0.0;
    let loc_secair = 0.0;
    putStrLn  "";

read_check_stage_2 = do
    putStrLn  "Enter the primary air supply rate, [kg/hr] (for base case = 15)";
    air_rate <- getDouble;
    putStrLn  "Enter the secondary air supply rate, [kg/hr]";
    putStrLn  "(in the range of 1-5 kg/hr, for base case = 5 kg/hr)";
    secondair <- getDouble;
    putStrLn "Enter the location of the secondary air supply from the reactor top in (m)";
    putStrLn  "(it must be between less than 0.5, for base case = 0.4 m)";
    loc_secair <- getDouble;
    putStrLn  "";

getDouble :: IO Double
getDouble = readLn

menu = do
    let loop = do
        putStrLn  "(Enter '1' for single stage or '2' for two-stage)"
        check_stage <- readLn
        if check_stage == 1 then
            read_check_stage_1
        else if (check_stage==2) then
            read_check_stage_2
        else
            putStrLn  "Error in enter value, it must be either 1 or 2"
        when (check_stage /= 1 && check_stage /= 2) loop
    loop

check_analysis check_analysis  
    |check_analysis == 1 = read_check_analysis_1
    |check_analysis == 2 = read_check_analysis_2
    |check_analysis == 3 = read_check_analysis_3
    |check_analysis == 4 = read_check_analysis_4
    |check_analysis == 5 = read_check_analysis_5
    |check_analysis == 6 = read_check_analysis_6
    |check_analysis == 7 = read_check_analysis_7
    |check_analysis == 8 = read_check_analysis_8
    |otherwise = putStrLn "Error in enter value, it must be any integer value from '1' to '8'"
