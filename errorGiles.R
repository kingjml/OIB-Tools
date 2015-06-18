#Giles et al. 2007 Altimeter Error Budget

#Eureka Values
fi = 0.13 #Ice freeboard in m
fs = 0.29 #Snow freeboard in m
hs = 0.16 #Snow depth in m
rhow = 1023.8 #Water density in kg/m^3
rhoi = 915.1 #Ice density in kg/m^3
rhos = 306.0 #Snow denisty in kg/m^3

#Error estimates
efi = 0.03 #Ice freeboard error in m
efs = 0.02 #Snow freeboard error in m
ehs = 0.062 #Snow depth error in m
epw = 0.5 #Water density error in kg/m^3
epi = 5 #Ice density error in kg/m^3
eps = 3 #Snow density errpr in kg/m^3



#################################################
#Typical values used in Giles et al. 2007
fi = 0.3 #Ice freeboard in m
fs = 0.6 #Snow freeboard in m
hs = 0.3 #Snow depth in m
rhow = 1023.8 #Water density in kg/m^3
rhoi = 915.1 #Ice density in kg/m^3
rhos = 319.5 #Snow denisty in kg/m^3

#Error estimates
efi = 0.03 #Ice freeboard error in m
efs = 0.02 #Snow freeboard error in m
ehs = 0.105 #Snow depth error in m
epw = 0.5 #Water density error in kg/m^3
epi = 5 #Ice density error in kg/m^3
eps = 3 #Snow density errpr in kg/m^3
################################################


epsR = sqrt((efi^2*(rhow/(rhow-rhoi))^2)+
	 (ehs^2*(rhos/(rhow-rhoi))^2)+
	 (eps^2*(hs/(rhow-rhoi))^2)+
	 (epw^2*((fi/(rhow-rhoi))-((fi*rhow)/(rhow-rhoi)^2)-((hs*rhos)/(rhow-rhoi)^2))^2)+
	 (epi^2*(((fi*rhow)/(rhow-rhoi)^2)+((hs*rhos)/(rhow-rhoi)^2))^2))

#epsL = (efs^2*(rhow/(rhow-rhoi))^2)+



#########################
#Gaussian error prophagation from Kern 2015
sigmaF = 0.03 #uncertainty of freeboard (0.03 - 0.15 m)
sigmaS = 0.01 #uncertainty of snow depth (0.01-0.2 m)
sigmaRhoI = 20 #uncertainty of sea ice density (20 kg m^3)
sigmaRhoS = 50 #uncertainty of snow density (50 kg m^3)

F= #total freeboard
S = #Snow depth

rhos = 300
rhoi = 915.1
rhow = 1023.9 

sigmaL = (sigmaF*(rhow/(rhow-rhoi)))^2 + (sigmaS*((rhos-rhow)/(rhow-rhoi)))^2+
         (sigmaRhoS*(S/(rhow-rhoi)))^2+
	   (sigmaRhoI*((rho)/()^2))^2