##%######################################################%##
#                                                          #
####    Community indices Above-Belowground ratio       ####
#                                                          #
##%######################################################%##

##########################################
###########          #####################
########### EXPOSED ######################
###########         ######################
##########################################
Spatial_Campaign_data_exp <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                        sheet = "Exposed", na = "NA")

TraitData_expA <- Spatial_Campaign_data_exp[,c(1:4, 29,30)]
TraitData_expA$Ratio <- TraitData_expA$`Total belowground dry mass (mg)`/TraitData_expA$`Total aboveground dry mass (mg)`

AbundanceData_exp <- Spatial_Campaign_data_exp[,c(1:4, 33)]
names(AbundanceData_exp)[5]<-paste(c("TotalDryMass"))
names(AbundanceData_exp)[2]<-paste(c("Site_number"))

############################################################
####          Create species abundance matrix           ####
##%######################################################%##

### abundance based on Total biomass
Plot_Abun_exp <- with(AbundanceData_exp, tapply(TotalDryMass, list(Site, Species), sum))
Plot_Abun_exp[is.na(Plot_Abun_exp)] = 0

##### Abundance per site
Plot_Abun_exp1_deep <- Plot_Abun_exp[c(1:8),]
Plot_Abun_exp1_shallow <- Plot_Abun_exp[c(27:34),]
Plot_Abun_exp1_deep <- Plot_Abun_exp1_deep[,c(2,4,5)]
Plot_Abun_exp1_shallow <- Plot_Abun_exp1_shallow[,c(1,2,4,5)]

Plot_Abun_exp2_deep <- Plot_Abun_exp[c(9:14),]
Plot_Abun_exp2_shallow <- Plot_Abun_exp[c(35:39),]
Plot_Abun_exp2_deep <- data.matrix(Plot_Abun_exp2_deep[,c(6)])
colnames(Plot_Abun_exp2_deep) <- c("ZOSMAR")
Plot_Abun_exp2_shallow <- Plot_Abun_exp2_shallow[,c(3:6)]

Plot_Abun_exp3_deep <- Plot_Abun_exp[c(15:20),]
Plot_Abun_exp3_shallow <- Plot_Abun_exp[c(40:45),]
Plot_Abun_exp3_deep <- Plot_Abun_exp3_deep[,c(2,4:6)]
Plot_Abun_exp3_shallow <- Plot_Abun_exp3_shallow[,c(2:5)]

Plot_Abun_exp4_deep <- Plot_Abun_exp[c(21:26),]
Plot_Abun_exp4_shallow <- Plot_Abun_exp[c(46:51),]
Plot_Abun_exp4_deep <- Plot_Abun_exp4_deep[,c(1,4,6)]

Plot_Abun_exp5_shallow <- Plot_Abun_exp[c(52:57),]
Plot_Abun_exp5_shallow <- Plot_Abun_exp5_shallow[,c(1,3:5)]


##%##########################################
#########    Trait matrix      ########
##%#########################################

Sites_exp <- aggregate(x=TraitData_expA$Ratio, by=list(TraitData_expA$`Site number`,TraitData_expA$Depth, TraitData_expA$Species), FUN=median)
names(Sites_exp)[1:4] <- paste(c("Site","Depth","Species", "Ratio"))

Site1_exp_deep <- subset(Sites_exp, Site==1 & Depth=="D")
Site1_exp_deepB <- as.data.frame(Site1_exp_deep[,c(4)])
rownames(Site1_exp_deepB) <- Site1_exp_deep$Species
names(Site1_exp_deepB)[1]<-paste(c("Ratio"))

Site1_exp_shallow <- subset(Sites_exp, Site==1 & Depth=="S")
Site1_exp_shallowB <- as.data.frame(Site1_exp_shallow[,c(4)])
rownames(Site1_exp_shallowB) <- Site1_exp_shallow$Species
names(Site1_exp_shallowB)[1]<-paste(c("Ratio"))

Site2_exp_deep <- subset(Sites_exp, Site==2 & Depth=="D")
Site2_exp_deepB <- as.data.frame(Site2_exp_deep[,c(4)])
rownames(Site2_exp_deepB) <- Site2_exp_deep$Species
names(Site2_exp_deepB)[1]<-paste(c("Ratio"))

Site2_exp_shallow <- subset(Sites_exp, Site==2 & Depth=="S")
Site2_exp_shallowB <- as.data.frame(Site2_exp_shallow[,c(4)])
rownames(Site2_exp_shallowB) <- Site2_exp_shallow$Species
names(Site2_exp_shallowB)[1]<-paste(c("Ratio"))

Site3_exp_deep <- subset(Sites_exp, Site==3 & Depth=="D")
Site3_exp_deepB <- as.data.frame(Site3_exp_deep[,c(4)])
rownames(Site3_exp_deepB) <- Site3_exp_deep$Species
names(Site3_exp_deepB)[1]<-paste(c("Ratio"))

Site3_exp_shallow <- subset(Sites_exp, Site==3 & Depth=="S")
Site3_exp_shallowB <- as.data.frame(Site3_exp_shallow[,c(4)])
rownames(Site3_exp_shallowB) <- Site3_exp_shallow$Species
names(Site3_exp_shallowB)[1]<-paste(c("Ratio"))

Site4_exp_deep <- subset(Sites_exp, Site==4 & Depth=="D")
Site4_exp_deepB <- as.data.frame(Site4_exp_deep[,c(4)])
rownames(Site4_exp_deepB) <- Site4_exp_deep$Species
names(Site4_exp_deepB)[1]<-paste(c("Ratio"))

Site4_exp_shallow <- subset(Sites_exp, Site==4 & Depth=="S")
Site4_exp_shallowB <- as.data.frame(Site4_exp_shallow[,c(4)])
rownames(Site4_exp_shallowB) <- Site4_exp_shallow$Species
names(Site4_exp_shallowB)[1]<-paste(c("Ratio"))

Site5_exp_shallow <- subset(Sites_exp, Site==5 & Depth=="S")
Site5_exp_shallowB <- as.data.frame(Site5_exp_shallow[,c(4)])
rownames(Site5_exp_shallowB) <- Site5_exp_shallow$Species
names(Site5_exp_shallowB)[1]<-paste(c("Ratio"))

########################################################
### Community weighted mean value for Ratio trait ######
########################################################

Indices_exp1_deep <- FD::dbFD(Site1_exp_deepB,Plot_Abun_exp1_deep, w.abun = TRUE)
Indices_exp1_shallow <- FD::dbFD(Site1_exp_shallowB,Plot_Abun_exp1_shallow, w.abun = TRUE)

Indices_exp2_shallow <- FD::dbFD(Site2_exp_shallowB,Plot_Abun_exp2_shallow, w.abun = TRUE)
Indices_exp2_deep <- Indices_exp1_shallow
Indices_exp2_deep$sing.sp <-  c(rep("1",8))
Indices_exp2_deep$FDis <- c(rep("0",8))
Indices_exp2_deep$CWM <- c(rep("0.5978297",8))

Indices_exp3_deep <- FD::dbFD(Site3_exp_deepB,Plot_Abun_exp3_deep, w.abun = TRUE)
Indices_exp3_shallow <- FD::dbFD(Site3_exp_shallowB,Plot_Abun_exp3_shallow, w.abun = TRUE)

Indices_exp4_deep <- FD::dbFD(Site4_exp_deepB,Plot_Abun_exp4_deep, w.abun = TRUE)
Indices_exp4_shallow <- FD::dbFD(Site4_exp_shallowB,Plot_Abun_exp4_shallow, w.abun = TRUE)

Indices_exp5_shallow <- FD::dbFD(Site5_exp_shallowB,Plot_Abun_exp5_shallow, w.abun = TRUE)

########################################################
###          Combine exposed site indices         ######
########################################################

###### Community weighted mean
CWMExp1S <-as.data.frame(Indices_exp1_shallow$CWM)
CWMExp1D <-as.data.frame(Indices_exp1_deep$CWM)

CWMExp2S <-as.data.frame(Indices_exp2_shallow$CWM)
CWMExp2D <-as.data.frame(Indices_exp2_deep$CWM)
CWMExp2D <- as.data.frame(CWMExp2D[c(1:6),]) 
colnames(CWMExp2D) <- c("Ratio")

CWMExp3S <-as.data.frame(Indices_exp3_shallow$CWM)
CWMExp3D <-as.data.frame(Indices_exp3_deep$CWM)

CWMExp4S <-as.data.frame(Indices_exp4_shallow$CWM)
CWMExp4D <-as.data.frame(Indices_exp4_deep$CWM)

CWMExp5S <-as.data.frame(Indices_exp5_shallow$CWM)

Indices_Ratio_Expsites <-rbind.data.frame(CWMExp1S,CWMExp2S,CWMExp3S,CWMExp4S,CWMExp5S,
                                                      CWMExp1D,CWMExp2D,CWMExp3D,CWMExp4D)
colnames(Indices_Ratio_Expsites) <- c("CWM_Ratio")
Indices_Ratio_Expsites$Depth <- c(rep("Shallow",31),rep("Deep",26))
Indices_Ratio_Expsites$Exposure <- c(rep("Exposed",57))

###### Functional Dispersion

FDisExp1S <-as.data.frame(Indices_exp1_shallow$FDis)
colnames(FDisExp1S) <- c("FDis")
FDisExp1D <-as.data.frame(Indices_exp1_deep$FDis)
colnames(FDisExp1D) <- c("FDis")

FDisExp2S <-as.data.frame(Indices_exp2_shallow$FDis)
colnames(FDisExp2S) <- c("FDis")
FDisExp2D <-as.data.frame(Indices_exp2_deep$FDis)
FDisExp2D <- as.data.frame(FDisExp2D[c(1:6),]) 
colnames(FDisExp2D) <- c("FDis")

FDisExp3S <-as.data.frame(Indices_exp3_shallow$FDis)
colnames(FDisExp3S) <- c("FDis")
FDisExp3D <-as.data.frame(Indices_exp3_deep$FDis)
colnames(FDisExp3D) <- c("FDis")

FDisExp4S <-as.data.frame(Indices_exp4_shallow$FDis)
colnames(FDisExp4S) <- c("FDis")
FDisExp4D <-as.data.frame(Indices_exp4_deep$FDis)
colnames(FDisExp4D) <- c("FDis")

FDisExp5S <-as.data.frame(Indices_exp5_shallow$FDis)
colnames(FDisExp5S) <- c("FDis")

Indices_Ratio_Expsites$FDis_Ratio <-rbind.data.frame(FDisExp1S,FDisExp2S,FDisExp3S,FDisExp4S,FDisExp5S,
                                                       FDisExp1D,FDisExp2D,FDisExp3D,FDisExp4D)
Indices_Ratio_Expsites <- tibble::rownames_to_column(Indices_Ratio_Expsites,"Site")
rownames(Indices_Ratio_Expsites)<-c(1:57)

##########################################
###########          #####################
###########   SEMI  ######################
###########         ######################
##########################################

Spatial_Campaign_data_semi <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Semi", na = "NA")

TraitData_semiA <- Spatial_Campaign_data_semi[,c(1:4, 30,31)]
TraitData_semiA[is.na(TraitData_semiA)] = 0
TraitData_semiA$Ratio <- TraitData_semiA$`Total belowground dry mass (mg)`/ TraitData_semiA$`Total aboveground dry mass (mg)`  

AbundanceData_semi <- Spatial_Campaign_data_semi[,c(1:4, 34)]
names(AbundanceData_semi)[5]<-paste(c("TotalDryMass"))
names(AbundanceData_semi)[2]<-paste(c("Site_number"))

############################################################
####          Create species abundance matrix           ####
##%######################################################%##

### abundance based on Total biomass
Plot_Abun_semi <- with(AbundanceData_semi, tapply(TotalDryMass, list(Site, Species), sum))
Plot_Abun_semi[is.na(Plot_Abun_semi)] = 0

##### Abundance per site
Plot_Abun_semi1_deep <- Plot_Abun_semi[c(1:6),]
Plot_Abun_semi1_shallow <- Plot_Abun_semi[c(31:36),]
Plot_Abun_semi1_deep <- Plot_Abun_semi1_deep[,c(2,6,7,8)]
Plot_Abun_semi1_shallow <- Plot_Abun_semi1_shallow[,c(2,3,6,7,8)]

Plot_Abun_semi2_deep <- Plot_Abun_semi[c(7:12),]
Plot_Abun_semi2_shallow <- Plot_Abun_semi[c(37:42),]
Plot_Abun_semi2_deep <- Plot_Abun_semi2_deep[,c(2,3,7,8)]
Plot_Abun_semi2_shallow <- Plot_Abun_semi2_shallow[,c(3,6,7,8)]

Plot_Abun_semi3_deep <- Plot_Abun_semi[c(13:18),]
Plot_Abun_semi3_shallow <- Plot_Abun_semi[c(43:48),]
Plot_Abun_semi3_deep <- Plot_Abun_semi3_deep[,c(1,2)]
Plot_Abun_semi3_shallow <- Plot_Abun_semi3_shallow[,c(1,2,3,7)]

Plot_Abun_semi4_deep <- Plot_Abun_semi[c(19:24),]
Plot_Abun_semi4_shallow <- Plot_Abun_semi[c(49:54),]
Plot_Abun_semi4_deep <- Plot_Abun_semi4_deep[,c(1,2,6)]
Plot_Abun_semi4_shallow <- Plot_Abun_semi4_shallow[,c(1,2,3,5,7)]

Plot_Abun_semi5_deep <- Plot_Abun_semi[c(25:30),]
Plot_Abun_semi5_shallow <- Plot_Abun_semi[c(55:60),]
Plot_Abun_semi5_deep <- Plot_Abun_semi5_deep[,c(1,2,3,7,8,9)]
Plot_Abun_semi5_shallow <- Plot_Abun_semi5_shallow[,c(2,3,4,7,8)]


##%##########################################
#########    Trait matrix      ########
##%#########################################

Sites_semi <-aggregate(x=TraitData_semiA$Ratio, by=list(TraitData_semiA$`Site number`,TraitData_semiA$Depth, TraitData_semiA$Species), FUN=median)
names(Sites_semi)[1:4]<-paste(c("Site","Depth","Species", "Ratio"))

Site1_semi_deep<- subset(Sites_semi, Site==1 & Depth=="D")
Site1_semi_deepB <- as.data.frame(Site1_semi_deep[,c(4)])
rownames(Site1_semi_deepB) <- Site1_semi_deep$Species
names(Site1_semi_deepB)[1]<-paste(c("Ratio"))

Site1_semi_shallow <- subset(Sites_semi, Site==1 & Depth=="S")
Site1_semi_shallowB <- as.data.frame(Site1_semi_shallow[,c(4)])
rownames(Site1_semi_shallowB) <- Site1_semi_shallow$Species
names(Site1_semi_shallowB)[1]<-paste(c("Ratio"))

Site2_semi_deep <- subset(Sites_semi, Site==2 & Depth=="D")
Site2_semi_deepB <- as.data.frame(Site2_semi_deep[,c(4)])
rownames(Site2_semi_deepB) <- Site2_semi_deep$Species
names(Site2_semi_deepB)[1]<-paste(c("Ratio"))

Site2_semi_shallow <- subset(Sites_semi, Site==2 & Depth=="S")
Site2_semi_shallowB <- as.data.frame(Site2_semi_shallow[,c(4)])
rownames(Site2_semi_shallowB) <- Site2_semi_shallow$Species
names(Site2_semi_shallowB)[1]<-paste(c("Ratio"))

Site3_semi_deep <- subset(Sites_semi, Site==3 & Depth=="D")
Site3_semi_deepB <- as.data.frame(Site3_semi_deep[,c(4)])
rownames(Site3_semi_deepB) <- Site3_semi_deep$Species
names(Site3_semi_deepB)[1]<-paste(c("Ratio"))

Site3_semi_shallow <- subset(Sites_semi, Site==3 & Depth=="S")
Site3_semi_shallowB <- as.data.frame(Site3_semi_shallow[,c(4)])
rownames(Site3_semi_shallowB) <- Site3_semi_shallow$Species
names(Site3_semi_shallowB)[1]<-paste(c("Ratio"))

Site4_semi_deep <- subset(Sites_semi, Site==4 & Depth=="D")
Site4_semi_deepB <- as.data.frame(Site4_semi_deep[,c(4)])
rownames(Site4_semi_deepB) <- Site4_semi_deep$Species
names(Site4_semi_deepB)[1]<-paste(c("Ratio"))

Site4_semi_shallow <- subset(Sites_semi, Site==4 & Depth=="S")
Site4_semi_shallowB <- as.data.frame(Site4_semi_shallow[,c(4)])
rownames(Site4_semi_shallowB) <- Site4_semi_shallow$Species
names(Site4_semi_shallowB)[1]<-paste(c("Ratio"))

Site5_semi_deep <- subset(Sites_semi, Site==5 & Depth=="D")
Site5_semi_deepB <- as.data.frame(Site5_semi_deep[,c(4)])
rownames(Site5_semi_deepB) <- Site5_semi_deep$Species
names(Site5_semi_deepB)[1]<-paste(c("Ratio"))

Site5_semi_shallow <- subset(Sites_semi, Site==5 & Depth=="S")
Site5_semi_shallowB <- as.data.frame(Site5_semi_shallow[,c(4)])
rownames(Site5_semi_shallowB) <- Site5_semi_shallow$Species
names(Site5_semi_shallowB)[1]<-paste(c("Ratio"))

########################################################
### Community weighted mean value for ratio trait ######
########################################################

Indices_semi1_deep <- FD::dbFD(Site1_semi_deepB,Plot_Abun_semi1_deep, w.abun = TRUE)
Indices_semi1_shallow <- FD::dbFD(Site1_semi_shallowB,Plot_Abun_semi1_shallow, w.abun = TRUE)

Indices_semi2_deep <- FD::dbFD(Site2_semi_deepB,Plot_Abun_semi2_deep, w.abun = TRUE)
Indices_semi2_shallow <- FD::dbFD(Site2_semi_shallowB,Plot_Abun_semi2_shallow, w.abun = TRUE)

Indices_semi3_deep <- FD::dbFD(Site3_semi_deepB,Plot_Abun_semi3_deep, w.abun = TRUE)
Indices_semi3_shallow <- FD::dbFD(Site3_semi_shallowB,Plot_Abun_semi3_shallow, w.abun = TRUE)

Indices_semi4_deep <- FD::dbFD(Site4_semi_deepB,Plot_Abun_semi4_deep, w.abun = TRUE)
Indices_semi4_shallow <- FD::dbFD(Site4_semi_shallowB,Plot_Abun_semi4_shallow, w.abun = TRUE)

Indices_semi5_deep <- FD::dbFD(Site5_semi_deepB,Plot_Abun_semi5_deep, w.abun = TRUE)
Indices_semi5_shallow <- FD::dbFD(Site5_semi_shallowB,Plot_Abun_semi5_shallow, w.abun = TRUE)

########################################################
###           Combine semi site indices           ######
########################################################
###### Community weighted mean
CWMSemi1S <-as.data.frame(Indices_semi1_shallow$CWM)
CWMSemi1D <-as.data.frame(Indices_semi1_deep$CWM)

CWMSemi2S <-as.data.frame(Indices_semi2_shallow$CWM)
CWMSemi2D <-as.data.frame(Indices_semi2_deep$CWM)

CWMSemi3S <-as.data.frame(Indices_semi3_shallow$CWM)
CWMSemi3D <-as.data.frame(Indices_semi3_deep$CWM)

CWMSemi4S <-as.data.frame(Indices_semi4_shallow$CWM)
CWMSemi4D <-as.data.frame(Indices_semi4_deep$CWM)

CWMSemi5S <-as.data.frame(Indices_semi5_shallow$CWM)
CWMSemi5D <-as.data.frame(Indices_semi5_deep$CWM)

Indices_Ratio_Semisites <- rbind.data.frame(CWMSemi1S,CWMSemi2S,CWMSemi3S,CWMSemi4S,CWMSemi5S,
                                                       CWMSemi1D,CWMSemi2D,CWMSemi3D,CWMSemi4D,CWMSemi4D)

colnames(Indices_Ratio_Semisites) <- c("CWM_Ratio")
Indices_Ratio_Semisites$Depth <- c(rep("Shallow",30),rep("Deep",30))
Indices_Ratio_Semisites$Exposure <- c(rep("Semi",60))
###### Functional Dispersion

FDisSemi1S <-as.data.frame(Indices_semi1_shallow$FDis)
colnames(FDisSemi1S) <- c("FDis")
FDisSemi1D <-as.data.frame(Indices_semi1_deep$FDis)
colnames(FDisSemi1D) <- c("FDis")

FDisSemi2S <-as.data.frame(Indices_semi2_shallow$FDis)
colnames(FDisSemi2S) <- c("FDis")
FDisSemi2D <-as.data.frame(Indices_semi2_deep$FDis)
colnames(FDisSemi2D) <- c("FDis")

FDisSemi3S <-as.data.frame(Indices_semi3_shallow$FDis)
colnames(FDisSemi3S) <- c("FDis")
FDisSemi3D <-as.data.frame(Indices_semi3_deep$FDis)
colnames(FDisSemi3D) <- c("FDis")

FDisSemi4S <-as.data.frame(Indices_semi4_shallow$FDis)
colnames(FDisSemi4S) <- c("FDis")
FDisSemi4D <-as.data.frame(Indices_semi4_deep$FDis)
colnames(FDisSemi4D) <- c("FDis")

FDisSemi5S <-as.data.frame(Indices_semi5_shallow$FDis)
colnames(FDisSemi5S) <- c("FDis")
FDisSemi5D <-as.data.frame(Indices_semi5_deep$FDis)
colnames(FDisSemi5D) <- c("FDis")

Indices_Ratio_Semisites$FDis_Ratio <-rbind.data.frame(FDisSemi1S,FDisSemi2S,FDisSemi3S,FDisSemi4S,FDisSemi5S,
                                                        FDisSemi1D,FDisSemi2D,FDisSemi3D,FDisSemi4D, FDisSemi5D)
Indices_Ratio_Semisites <- tibble::rownames_to_column(Indices_Ratio_Semisites,"Site")
rownames(Indices_Ratio_Semisites)<-c(1:60)

##############################################
###########              #####################
###########  Sheltered   #####################
###########              #####################
##############################################

Spatial_Campaign_data_shel <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Sheltered", na = "NA")

TraitData_shelA <- Spatial_Campaign_data_shel[,c(1:4, 34,35)]
TraitData_shelA[is.na(TraitData_shelA)] = 0
TraitData_shelA$Ratio <- TraitData_shelA$`Total belowground dry mass (mg)` / TraitData_shelA$`Total aboveground dry mass (mg)`

AbundanceData_shel <- Spatial_Campaign_data_shel[,c(1:4, 38)]
names(AbundanceData_shel)[5]<-paste(c("TotalDryMass"))
names(AbundanceData_shel)[2]<-paste(c("Site_number"))

############################################################
####          Create species abundance matrix           ####
##%######################################################%##

### abundance based on Total biomass
Plot_Abun_shel <- with(AbundanceData_shel, tapply(TotalDryMass, list(Site, Species), sum))
Plot_Abun_shel[is.na(Plot_Abun_shel)] = 0

##%##########################################
#########     Trait matrix          ########
##%#########################################

Sites_shel_Ratio <- aggregate(x=TraitData_shelA$Ratio, by=list(TraitData_shelA$Depth, TraitData_shelA$Species), FUN=median)
names(Sites_shel_Ratio)[1:3]<-paste(c("Depth","Species", "Ratio"))

Sites_shel_RatioB <- as.data.frame(Sites_shel_Ratio[,c(3)])
rownames(Sites_shel_RatioB) <- Sites_shel_Ratio$Species
names(Sites_shel_RatioB)[1]<-paste(c("Ratio"))

########################################################
### Community weighted mean value for ratio trait ######
########################################################

Indices_shel_Ratio <- FD::dbFD(Sites_shel_RatioB,Plot_Abun_shel, w.abun = TRUE)

########################################################
###         Combine sheltered site indices        ######
########################################################

###### Community weighted mean
CWMshel_Ratio <-as.data.frame(Indices_shel_Ratio$CWM)

Indices_Ratio_shelsites <- CWMshel_Ratio
colnames(Indices_Ratio_shelsites) <- c("CWM_Ratio")
Indices_Ratio_shelsites$Depth <- c(rep("Shallow",30))
Indices_Ratio_shelsites$Exposure <- c(rep("Sheltered",30))

###### Functional Dispersion
FDisshel_Ratio <-as.data.frame(Indices_shel_Ratio$FDis)
colnames(FDisshel_Ratio) <- c("FDis")

Indices_Ratio_shelsites$FDis_Ratio <- FDisshel_Ratio

Indices_Ratio_shelsites <- tibble::rownames_to_column(Indices_Ratio_shelsites,"Site")
rownames(Indices_Ratio_shelsites)<-c(1:30)

##########################################
###########          #####################
########### POJO    ######################
###########         ######################
##########################################

Spatial_Campaign_data_pojo <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Pojo", na = "NA")

TraitData_pojoA <- Spatial_Campaign_data_pojo[-c(1,4,6,7),c(1:4, 32,33)]
TraitData_pojoA[is.na(TraitData_pojoA)] = 0
TraitData_pojoA$Ratio <- TraitData_pojoA$`Total belowground dry mass (mg)`/TraitData_pojoA$`Total aboveground dry mass (mg)`

AbundanceData_pojo <- Spatial_Campaign_data_pojo[-c(1,4,6,7),c(1:4, 36)]
names(AbundanceData_pojo)[5]<-paste(c("TotalDryMass"))
names(AbundanceData_pojo)[2]<-paste(c("Site_number"))

############################################################
####          Create species abundance matrix           ####
##%######################################################%##

### abundance based on Total biomass
Plot_Abun_pojo <- with(AbundanceData_pojo, tapply(TotalDryMass, list(Site, Species), sum))
Plot_Abun_pojo[is.na(Plot_Abun_pojo)] = 0

##### Abundance per depth
Plot_Abun_pojo_deep <- Plot_Abun_pojo[c(1:8),-c(3,5,6,8,10,12)]
Plot_Abun_pojo_shallow <- Plot_Abun_pojo[c(9:38),-c(1)]

##%##########################################
#########     Trait matrix          ########
##%#########################################

Sites_pojo_Ratio <- aggregate(x=TraitData_pojoA$Ratio, by=list(TraitData_pojoA$Depth, TraitData_pojoA$Species), FUN=median)
names(Sites_pojo_Ratio)[1:3]<-paste(c("Depth","Species", "Ratio"))

Sites_pojo_Ratio_D <- subset(Sites_pojo_Ratio, Depth=="D")
Sites_pojo_Ratio_S <- subset(Sites_pojo_Ratio, Depth=="S")

Sites_pojo_Ratio_DB <- as.data.frame(Sites_pojo_Ratio_D[,c(3)])
rownames(Sites_pojo_Ratio_DB) <- Sites_pojo_Ratio_D$Species
names(Sites_pojo_Ratio_DB)[1]<-paste(c("Ratio"))

Sites_pojo_Ratio_SB <- as.data.frame(Sites_pojo_Ratio_S[,c(3)])
rownames(Sites_pojo_Ratio_SB) <- Sites_pojo_Ratio_S$Species
names(Sites_pojo_Ratio_SB)[1]<-paste(c("Ratio"))

########################################################
### Community weighted mean value for ratio trait ######
########################################################

Indices_pojo_Ratio_D <- FD::dbFD(Sites_pojo_Ratio_DB,Plot_Abun_pojo_deep, w.abun = TRUE)
Indices_pojo_Ratio_S <- FD::dbFD(Sites_pojo_Ratio_SB,Plot_Abun_pojo_shallow, w.abun = TRUE)

########################################################
###          Combine pojo site indices            ######
########################################################

###### Community weighted mean
CWMpojo_Ratio_D <-as.data.frame(Indices_pojo_Ratio_D$CWM)
CWMpojo_Ratio_S <-as.data.frame(Indices_pojo_Ratio_S$CWM)

Indices_Ratio_pojosites <- rbind(CWMpojo_Ratio_D,CWMpojo_Ratio_S)
colnames(Indices_Ratio_pojosites) <- c("CWM_Ratio")
Indices_Ratio_pojosites$Depth <- c(rep("Deep",8),rep("Shallow",30))
Indices_Ratio_pojosites$Exposure <- c(rep("Pojo",38))

###### Functional Dispersion
FDispojo_Ratio_D <-as.data.frame(Indices_pojo_Ratio_D$FDis)
colnames(FDispojo_Ratio_D) <- c("FDis")
FDispojo_Ratio_S <-as.data.frame(Indices_pojo_Ratio_S$FDis)
colnames(FDispojo_Ratio_S) <- c("FDis")

Indices_Ratio_pojosites$FDis_Ratio <- rbind(FDispojo_Ratio_D,FDispojo_Ratio_S)

Indices_Ratio_pojosites <- tibble::rownames_to_column(Indices_Ratio_pojosites,"Site")
rownames(Indices_Ratio_pojosites)<-c(1:38)


##################################################
###########                 ######################
###########    Combined     ######################
###########                 ######################
##################################################

Indices_Ratio_Expsites <- as.matrix.data.frame(Indices_Ratio_Expsites)
Indices_Ratio_pojosites <- as.matrix.data.frame(Indices_Ratio_pojosites)
Indices_Ratio_Semisites <- as.matrix.data.frame(Indices_Ratio_Semisites)
Indices_Ratio_shelsites <- as.matrix.data.frame(Indices_Ratio_shelsites)
Indices_Ratio_Expsites <- as.data.frame(Indices_Ratio_Expsites)
Indices_Ratio_pojosites <- as.data.frame(Indices_Ratio_pojosites)
Indices_Ratio_Semisites <- as.data.frame(Indices_Ratio_Semisites)
Indices_Ratio_shelsites <- as.data.frame(Indices_Ratio_shelsites)

Indices_Ratio_AllsitesB<- rbind.data.frame(Indices_Ratio_Expsites,Indices_Ratio_Semisites,
                                           Indices_Ratio_pojosites,Indices_Ratio_shelsites)


Indices_Ratio_AllsitesB$Exposure_Depth <- factor(paste(Indices_Ratio_AllsitesB$Exposure, 
                                                       Indices_Ratio_AllsitesB$Depth, sep = "_"),
                                                levels = c(
                                                  "Exposed_Shallow", "Exposed_Deep",
                                                  "Semi_Shallow", "Semi_Deep",
                                                  "Pojo_Shallow", "Pojo_Deep",
                                                  "Sheltered_Shallow"
                                                ))
Indices_Ratio_AllsitesB$Site_number<- c(rep("1S",8),rep("2S",5),rep("3S",6),rep("4S",6),rep("5S",6),rep("1D",8),rep("2D",6),rep("3D",6),rep("4D",6),
                                         rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6),rep("1D",6),rep("2D",6),rep("3D",6),rep("4D",6),rep("5D",6),
                                         rep("2D",2),rep("3D",6),rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6),
                                         rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6))

Indices_Ratio_AllsitesB$Site_Exposure <- factor(paste(Indices_Ratio_AllsitesB$Site_number, 
                                                      Indices_Ratio_AllsitesB$Exposure, sep = "_"),
                                                 levels = c(
                                                   "1S_Exposed", "2S_Exposed", "3S_Exposed", "4S_Exposed", "5S_Exposed", "1D_Exposed", "2D_Exposed", "3D_Exposed", "4D_Exposed",
                                                   "1S_Semi", "2S_Semi", "3S_Semi", "4S_Semi", "5S_Semi", "1D_Semi", "2D_Semi", "3D_Semi", "4D_Semi", "5D_Semi",
                                                   "1S_Pojo", "2S_Pojo", "3S_Pojo", "4S_Pojo", "5S_Pojo", "2D_Pojo", "3D_Pojo",
                                                   "1S_Sheltered", "2S_Sheltered", "3S_Sheltered", "4S_Sheltered", "5S_Sheltered"
                                                 ))


