-- ****************************************
-- Constants values and physical properties
-- ****************************************
module Constants where
pi :: Double
pi = 3.141592654 -- Value of pi

r :: Double 
r = 8.3144 -- Universal gas constant, [J/mol K]

mole_C :: Double 
mole_C = 12.0 -- Molecular weight of Char,[g/mol]

mole_O2 :: Double 
mole_O2 = 32.0 -- Molecular weight of O2, [g/mol]

mole_CO2 :: Double 
mole_CO2 = 44.0 -- Molecular weight of CO2, [g/mol]

mole_CO :: Double 
mole_CO = 28.0 -- Molecular weight of CO, [g/mol]

mole_H2O :: Double 
mole_H2O = 18.0 -- Molecular weight of H2O, [g/mol]

mole_H2 :: Double 
mole_H2 = 2.0 -- Molecular weight of H2, [g/mol]

mole_CH4 :: Double 
mole_CH4 = 16.0 -- Molecular weight of CH4, [g/mol]

mole_C2H4 :: Double 
mole_C2H4 = 28.0 -- Molecular weight of C2H4, [g/mol]

mole_N2 :: Double 
mole_N2 = 28.0 -- Molecular weight of N2, [g/mol]

mole_tar1 :: Double 
mole_tar1 = 134.934 -- Molecular weight of primary tar, [g/mol]

mole_tar2 :: Double 
mole_tar2 = 78.0 -- Molecular weight of secondary tar, [g/mol]

mf_O2 :: Double 
mf_O2 = 0.21 -- Molar fraction of oxygen in air, [-]

mass_O2 :: Double 
mass_O2 = 0.233 -- Mass fraction of oxygen in air, [-]

sigma :: Double 
sigma = 5.6703e-08-- Stefan-Boltzmann constant, [W/m2K4]

emiss :: Double 
emiss = 0.85 -- Emissivity of char, [-]

cp_m :: Double 
cp_m = 4200.0 -- Specific heat of moisture,[J/kgK]

dV_O2 :: Double 
dV_O2 = 12.22 -- Diffusion volumes of O2

dV_N2 :: Double 
dV_N2 = 9.08 -- Diffusion volumes of N2 

dV_CO :: Double 
dV_CO = 22.01 -- Diffusion volumes of CO

dV_CO2 :: Double 
dV_CO2 = 28.12 -- Diffusion volumes of CO2

dV_H2 :: Double 
dV_H2 = 4.62 -- Diffusion volumes of H2

dV_H2O :: Double 
dV_H2O = 10.73 -- Diffusion volumes of H2O

dV_CH4 :: Double 
dV_CH4 = 25.14 -- Diffusion volumes of CH4

dV_C2H4 :: Double 
dV_C2H4 = 41.04 -- Diffusion volumes of C2H4

dV_tar1 :: Double 
dV_tar1 = 140.08314 -- Diffusion volumes of tar1

dV_tar2 :: Double 
dV_tar2 = 109.26 -- Diffusion volumes of tar2
