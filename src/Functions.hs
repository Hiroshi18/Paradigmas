import qualified Constants
module Functions where
ln :: Double -> Double 
ln value = log(value)/log(2.7182818281)
-- Calculate ln value

-- ****************************End FUNCTION ln**********************************************

h_C :: Double -> Double
-- Calculate absolute enthalpy of O2 at temperature T
h_C temp
    |temp > 1000 = (1.23870304+01*temp) + (6.91227177-03*temp**2) - (1.85438496-06*temp**3)+(2.68524262-10*temp**4)-(1.53215420-14*temp**5)-(5.88033398+03)
    |otherwise = -(5.59755420+00*temp) + (2.98573864-02*temp**2) - (1.54884907-05*temp**3)+(4.26136365-09*temp**4)-(6.08974359-13*temp**5)-(6.05750000+02)

-- *********************** END FUNCTION h_C ************************************************

h_O2 :: Double -> Double
h_O2 temp
-- Calculate absolute enthalpy of O2 at temperature T
    |temp > 1000 = Constants.r*((0.03697578e+02*temp)+(0.06135197e-02/2*temp**2)-(0.1258842e-06/3*temp**3)+(0.01775281e-09/4*temp**4)-(0.11364354e-14/5*temp**5)-(0.12339301e+04))
    |otherwise = Constants.r*((0.03212936e+02*temp)+(0.11274864e-02/2*temp**2)-(0.05756150e-05/3*temp**3)+(0.13138773e-08/4*temp**4)-(0.08768554e-11/5*temp**5)-(0.1005249e+04))
-- *********************** END FUNCTION h_O2 ***********************************************

h_CO2 :: Double -> Double
h_CO2 temp
-- Calculate absolute enthalpy of CO2 at temperature T
    |temp>1000 = Constants.r*((0.04453623e+02*temp)+(0.03140168e-01/2*temp**2)-(0.12784105e-05/3*temp**3)+(0.02393996e-08/4*temp**4)-(0.16690333e-13/5*temp**5)-(0.04896696e+06))
    |otherwise = Constants.r*((0.02275724e+02*temp)+(0.09922072e-01/2*temp**2)-(0.10409113e-04/3*temp**3)+(0.06866686e-07/4*temp**4)-(0.0211728e-10/5*temp**5)-(0.04837314e+06))
-- *********************** END FUNCTION h_CO2 **********************************************

h_CO :: Double -> Double
h_CO (temp)
--Calculate absolute enthalpy of CO at temperature T
    |temp>1000 = Constants.r*((0.03025078e+02*temp)+(0.14426885e-02/2*temp**2)-(0.05630827e-05/3*temp**3)+(0.10185813e-09/4*temp**4)-(0.06910951e-13/5*temp**5)-(0.1426835e+05))
    |otherwise = Constants.r*((0.03262451e+02*temp)+(0.15119409e-02/2*temp**2)-(0.03881755e-04/3*temp**3)+(0.05581944e-07/4*temp**4)-(0.02474951e-10/5*temp**5)-(0.14310539e+05))

-- *********************** END FUNCTION h_CO ***********************************************

h_H2 :: Double -> Double
h_H2(temp)
-- Calculate absolute enthalpy of H2 at temperature T
    |temp>1000 = Constants.r*((0.02991423e+02*temp)+(0.07000644e-02/2*temp**2)-(0.05633828e-06/3*temp**3)-(0.09231578e-10/4*temp**4)+(0.15827519e-14/5*temp**5)-(0.0835034e+04))
    |otherwise = Constants.r*((0.03298124e+02*temp)+(0.08249441e-02/2*temp**2)-(0.08143015e-05/3*temp**3)-(0.09475434e-09/4*temp**4)+(0.04134872e-11/5*temp**5)-(0.10125209e+04))
-- *********************** END FUNCTION h_H2 ***********************************************

h_H2O :: Double -> Double
h_H2O(temp)
-- Calculate absolute enthalpy of H2O at temperature T
    |temp>1000 = Constants.r*((0.02672145e+02*temp)+(0.03056293e-01/2*temp**2)-(0.08730260e-05/3*temp**3)+(0.12009964e-09/4*temp**4)-(0.06391618e-13/5*temp**5)-(0.02989921e+06))
    |otherwise = Constants.r*((0.03386842e+02*temp)+(0.03474982e-01/2*temp**2)-(0.06354696e-04/3*temp**3)+(0.06968581e-07/4*temp**4)-(0.02506588e-10/5*temp**5)-(0.03020811e+06))
-- *********************** END FUNCTION h_H2O **********************************************

h_CH4 :: Double -> Double
h_CH4 (temp) = (Constants.r*(((1.702)*(temp-298.15))+((9.081e-03/2)*(temp**2-298.15**2))-((2.164e-06/3)*(temp**3-298.15**3))))-74831

--Calculate absolute enthalpy of CH4 at temperature T

-- *********************** END FUNCTION h_CH4 **********************************************

h_C2H4 :: Double -> Double
h_C2H4 (temp) = (Constants.r*(((1.424)*(temp-298.15))+((14.394e-03/2)*(temp**2-298.15**2))-((4.392e-06/3)*(temp**3-298.15**3))))+52283
-- Calculate absolute enthalpy of C2H4 at temperature T

-- *********************** END FUNCTION h_C2H4 *********************************************
h_C6H6 :: Double -> Double
h_C6H6 (temp) = (Constants.r*(-((0.206)*(temp-298.15))+((39.064e-03/2)*(temp**2-298.15**2))-((13.301e-06/3)*(temp**3-298.15**3))))+82927
-- Calculate absolute enthalpy of C6H6 at temperature T

-- *********************** END FUNCTION h_C6H6 ********************************************* 

-- heat1 :: Double -> Double
-- heat1 (temp)
-- Calculate the heat of reaction of Reaction 1 [J/kg of Carbon]
--    heat1 = 1000*(h_C(temp)+gamma*h_O2(temp)-(2-2*gamma)*h_CO(temp)-&(2*gamma-1)*h_CO2(temp))/Constants.mole_C

-- *********************** END FUNCTION heat1 **********************************************

heat2 :: Double -> Double
-- Calculate the heat of reaction of Reaction 2 [J/kg of Carbon]
heat2 (temp) = 1000*(h_C(temp)+h_CO2(temp)-2*h_CO(temp))/Constants.mole_C

-- *********************** END FUNCTION heat2 **********************************************

heat3 :: Double -> Double
-- Calculate the heat of reaction of Reaction 3 [J/kg of Carbon]
heat3 (temp) = 1000*(h_C(temp)+h_H2O(temp)-h_H2(temp)-h_CO(temp))/Constants.mole_C

-- *********************** END FUNCTION heat3 **********************************************


heat4 :: Double -> Double
-- Calculate the heat of reaction of reaction 4 [J/kg of Carbon]
heat4(temp) = 1000*(h_C(temp)+2*h_H2(temp)-h_CH4(temp))/Constants.mole_C

-- *********************** END FUNCTION heat4 **********************************************

heat5 :: Double -> Double
--Calculate the heat of reaction of forward reaction 5 [J/kmol]
heat5 (temp) = 1000*(h_CO(temp)+h_H2O(temp)-h_H2(temp)-h_CO2(temp))

-- *********************** END FUNCTION heat5 **********************************************

heat5r :: Double -> Double
--Calculate the heat of reaction of reverse reaction 5 [J/kmol]
heat5r (temp) = 1000*(h_CO2(temp)+h_H2(temp)-h_H2O(temp)-h_CO(temp))

-- *********************** END FUNCTION heat5 **********************************************

heat6 :: Double -> Double
-- Calculate the heat of reaction of reaction 6
heat6 (temp) = 1000*(h_C6H6(temp)+3*h_O2(temp)-3*h_H2(temp)-6*h_CO(temp))

-- *********************** END FUNCTION heat6 **********************************************

heat7 :: Double -> Double
-- Calculate the heat of reaction of reaction 7 [J/kmol of CO]
heat7(temp) = 1000*(h_CO(temp)+0.5*h_O2(temp)-h_CO2(temp))

-- *********************** END FUNCTION heat7 **********************************************

heat8 :: Double -> Double
-- Calculate the heat of reaction of reaction 8 [J/kmol of CH4]
heat8 (temp) = 1000*(h_CH4(temp)+1.5*h_O2(temp)-2*h_H2O(temp)-h_CO(temp))

-- *********************** END FUNCTION heat8 **********************************************

heat9 :: Double -> Double
-- Calculate the heat of reaction of reaction 9 [J/kmol of C2H4]
heat9(temp) = 1000*(h_C2H4(temp)+ h_O2(temp)-2*h_H2(temp)-2*h_CO(temp))

-- *********************** END FUNCTION heat9 **********************************************

heat10 :: Double -> Double
-- Calculate the heat of reaction of reaction 10 [J/kmol of H2]
heat10 (temp) = 1000*(h_H2(temp)+0.5*h_O2(temp)-h_H2O(temp)) 

-- *********************** END FUNCTION heat10 *********************************************

k :: (Double,Double,Double) -> Double
-- Calculate K
k (a,e,temp) = a*exp(-e/(Constants.r*temp))

-- ****************************END FUNCTION k***********************************************

visco :: Double -> Double
-- Calculate viscosity of the gas
visco(temp)=1.98e-05*((temp/300)**(2/3))
-- *********************END FUNCTION visco**************************************************

cp_g :: Double -> Double
-- Calculate specific heat of gas mixture
cp_g(temp) = -3.63930e-8*temp**3+ 6.50858e-5*temp**2 + 1.66918e-1*temp + 9.41079e+2
-- *********************END FUNCTION Cp_g **************************************************

cp_char :: (Double,Double) -> Double
-- Calculate specific heat of char
cp_char(temp, t_a)=(1.39+0.00036*(temp+t_a)/2)*1000

-- *********************END FUNCTION Cp_char ***********************************************

cp_wood :: (Double, Double) -> Double
-- Calculate specific heat of wood
cp_wood(temp, t_a)=(1.39+0.00036*(temp+t_a)/2)*1000 

-- *********************END FUNCTION Cp_wood ***********************************************

k_wood :: Double -> Double
-- Calculate Thermal conductivity of wood
k_wood (temp)=0.13+3e-4*temp

-- *********************END FUNCTION K_wood ************************************************

k_g :: Double -> Double
-- Calculate Thermal conductivity of gas
k_g (temp)=4.77e-04*temp**0.717

-- *********************END FUNCTION k_g ***************************************************

h_rs :: (Double, Double) -> Double
-- Calculate the solid radiation coefficient
h_rs(temp,emiss)=4*Constants.sigma*(temp**3)*(emiss/(2-emiss))

-- *********************END FUNCTION h_rs **************************************************
h_rv :: (Double,Double,Double) -> Double
-- Calculate the void to void radiation coefficient
h_rv (temp,emiss,void)=4*Constants.sigma*(temp**3)/(1+((void/(2*(1-void)))*((1-emiss)/emiss)))

-- *********************END FUNCTION h_rv **************************************************


k_eff :: (Double,Double,Double,Double,Double,Double,Double,Double) -> Double

-- Calculate the effectivity thermal conductivity in case of gas filled voids,
k_eff(m_g,t_s,t_g,d_p,emiss,void,d_rec,keff_mf)=keff_mf* ((k_g(t_g)*((1-void)/((2/3)*(k_g(t_g)/k_wood(t_s))+
 (1/(10/void+(d_p*h_rs(t_s,emiss)/k_g(t_g)))))+
 (void*d_p*h_rv(t_g,emiss,void)/k_g(t_g))))
 +(k_g(t_g)*void)
 +((0.14/(1+46*(d_p/d_rec)))*(d_p*m_g/(visco(t_g)))*
 (cp_g(t_g)*(visco(t_g)))))

-- *********************END FUNCTION K_eff**************************************************

packp :: Double -> Double
-- Calculate packing parameter
packp (k)=(0.3525*(((k-1)/k)**2)/(((ln(k-0.5431*(k-1))))-(0.4569*(k-1)/k)))-(2/(3*k))

-- *********************END FUNCTION packp**************************************************

hV_biomass :: (Double,Double,Double,Double,Double,Double,Double) -> Double
-- Calculate Higher heating value of biomass in moisture and ash free basic [MJ/kg]
-- Calculate Net (Lower) heating value of biomass in moisture and ash free basic [MJ/kg]
hV_biomass(c,h,o,n,s,a,m) = ((34.1*c+132.2*h+6.8*s-1.53*a-12*(o+n))/(100))-2.442*(m/100+h/100*9)


-- *********************END FUNCTION HV_biomass*********************************************

hV_gas :: (Double,Double,Double,Double) -> Double
hV_gas(cO,h2,cH4,h2O)
-- Calculate Net (Lower) heating value of producer gas in dry basic [MJ/Nm3]
    |(cO < 0.00000001 && h2 < 0.00000001 && cH4<0.00000001) = 0
    |otherwise = ((cO*100/(1-h2O)*13.1)+(h2*100/(1-h2O)*11.2)+(cH4*100/(1-h2O)*37.1))/100

-- *********************END FUNCTION hV_gas*************************************************


kp_wg :: Double -> Double
kp_wg(temp) = 2.7182818281**((1/8.3144/temp)*(11321-31.08*temp+3*temp*ln(temp)-0.00028*temp**2-91500/temp))
-- Calculate the equilibrium constant of water gas shift reaction 
-- *****************************END FUNCTION kp_wg******************************************

diff_AB :: (Double,Double,Double,Double,Double,Double) -> Double
diff_AB (temp,pre,mole_A,mole_B,dV_A,dV_B) = (0.00143*temp**1.75)*sqrt(1/mole_A+1/mole_B)*1e-04 / (pre*sqrt(2)*(dV_A**(1/3)+dV_B**(1/3))**2)
-- Calculate diffusion O2efficient between O2 and gas mixture at temperature T

-- *********************** END FUNCTION diff_AB ********************************************