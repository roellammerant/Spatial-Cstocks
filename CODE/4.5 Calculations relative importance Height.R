##%######################################################%##
#                                                          #
####              adjust data for tests                 ####
#                                                          #
##%######################################################%##
library(stats)
Indices_height_Fixed$CWM_Height<- as.numeric(Indices_height_Fixed$CWM_Height) 
Indices_height_SpecificMinusFixedA$CWM_Height<- as.numeric(Indices_height_SpecificMinusFixedA$CWM_Height) 

Indices_height_Fixed$FDis_Height<- as.numeric(Indices_height_Fixed$FDis_Height) 
Indices_height_SpecificMinusFixedA$FDis_Height<- as.numeric(Indices_height_SpecificMinusFixedA$FDis_Height) 

Indices_height_Fixed$Exposure<- as.factor(Indices_height_Fixed$Exposure) 
Indices_height_SpecificMinusFixedA$Exposure<- as.factor(Indices_height_SpecificMinusFixedA$Exposure)

Indices_height_Allsites$CWM_Height<- as.numeric(Indices_height_Allsites$CWM_Height) 
Indices_height_Allsites$FDis_Height<- as.numeric(Indices_height_Allsites$FDis_Height) 
Indices_height_Allsites$Exposure<- as.factor(Indices_height_Allsites$Exposure) 

##%######################################################%##
#                                                          #
####      Non-parametric test + heteroscedasticity     #####
#                                                          #
##%######################################################%##

library(tidyverse)
library(vegan)

##%######################################################%##
#                                                          #
####            Non parametric permanova                ####   
####                                                    ####
#                                                          #
##%######################################################%##

# Permanova can´t handle zero`s
Indices_height_Fixed$FDis_Height[Indices_height_Fixed$FDis_Height == 0] <- 0.00001 
Shallow_Height_Fixed <- Indices_height_Fixed[ which(Indices_height_Fixed$Depth=='Shallow'), ]

Indices_height_SpecificMinusFixedA$FDis_Height[Indices_height_SpecificMinusFixedA$FDis_Height == 0] <- 0.00001 
Indices_height_SpecificMinusFixedA$CWM_Height <- abs(Indices_height_SpecificMinusFixedA$CWM_Height)
Indices_height_SpecificMinusFixedA$CWM_Height[Indices_height_SpecificMinusFixedA$CWM_Height == 0] <- 0.00001 
Shallow_Height_IntraA <- Indices_height_SpecificMinusFixedA[ which(Indices_height_SpecificMinusFixedA$Depth=='Shallow'), ]

Indices_height_Allsites$FDis_Height[Indices_height_Allsites$FDis_Height == 0] <- 0.00001 
Shallow_Height_SpecificA <- Indices_height_Allsites[ which(Indices_height_Allsites$Depth=='Shallow'), ]




#### FIXED ####

CWMHeight_Fixed <- adonis2(Shallow_Height_Fixed$CWM_Height ~ Exposure + Site_Exposure, data=Shallow_Height_Fixed, perm=999)
CWMHeight_Fixed # significant difference at exposure and site level

FDisHeight_Fixed <- adonis2(Shallow_Height_Fixed$FDis_Height ~ Exposure + Site_Exposure , data=Shallow_Height_Fixed, perm=999)
FDisHeight_Fixed # significant difference at exposure and site level

#### Intra A ####

CWMHeight_IntraA <- adonis2(Shallow_Height_IntraA$CWM_Height ~ Exposure + Site_Exposure, data=Shallow_Height_IntraA, perm=999)
CWMHeight_IntraA # significant difference at exposure and site level

FDisHeight_IntraA <- adonis2(Shallow_Height_IntraA$FDis_Height ~ Exposure + Site_Exposure , data=Shallow_Height_IntraA, perm=999)
FDisHeight_IntraA # significant difference at exposure and site level

#### Specific A ####

CWMHeight_SpecificA <- adonis2(Shallow_Height_SpecificA$CWM_Height ~ Exposure + Site_Exposure, data=Shallow_Height_SpecificA, perm=999)
CWMHeight_SpecificA # significant difference at exposure and site level

FDisHeight_SpecificA <- adonis2(Shallow_Height_SpecificA$FDis_Height ~ Exposure + Site_Exposure , data=Shallow_Height_SpecificA, perm=999)
FDisHeight_SpecificA # significant difference at exposure and site level






