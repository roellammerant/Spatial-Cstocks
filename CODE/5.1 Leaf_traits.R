##%######################################################%##
#                                                          #
####            Community indices leaves                ####
#                                                          #
##%######################################################%##

##########################################
###########          #####################
########### EXPOSED ######################
###########         ######################
##########################################
Spatial_Campaign_data_exp <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                        sheet = "Exposed", na = "NA")

TraitData_expA <- Spatial_Campaign_data_exp[,c(1:4, 35,37)]
names(TraitData_expA)[5]<-paste(c("SLA"))
names(TraitData_expA)[6]<-paste(c("LAP"))
TraitData_expB <- Spatial_Campaign_data_exp[,c(1:4, 36,38)]
names(TraitData_expB)[5]<-paste(c("SLA"))
names(TraitData_expB)[6]<-paste(c("LAP"))
TraitData_exp <- rbind (TraitData_expA,TraitData_expB )

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

Sites_exp_SLA <-aggregate(x=TraitData_exp$SLA, by=list(TraitData_exp$`Site number`,TraitData_exp$Depth, TraitData_exp$Species), FUN=median)
names(Sites_exp_SLA)[1:4]<-paste(c("Site","Depth","Species", "SLA"))

Sites_exp_LAP <-aggregate(x=TraitData_exp$LAP, by=list(TraitData_exp$`Site number`,TraitData_exp$Depth, TraitData_exp$Species), FUN=median)
names(Sites_exp_LAP)[1:4]<-paste(c("Site","Depth","Species", "LAP"))

############ SLA  ############ 
Site1_exp_SLAD <- subset(Sites_exp_SLA, Site==1 & Depth=="D")
Site1_exp_SLADB <- as.data.frame(Site1_exp_SLAD[,c(4)])
rownames(Site1_exp_SLADB) <- Site1_exp_SLAD$Species
names(Site1_exp_SLADB)[1]<-paste(c("SLA"))

Site1_exp_SLAS <- subset(Sites_exp_SLA, Site==1 & Depth=="S")
Site1_exp_SLASB <- as.data.frame(Site1_exp_SLAS[,c(4)])
rownames(Site1_exp_SLASB) <- Site1_exp_SLAS$Species
names(Site1_exp_SLASB)[1]<-paste(c("SLA"))

Site2_exp_SLAD <- subset(Sites_exp_SLA, Site==2 & Depth=="D")
Site2_exp_SLADB <- as.data.frame(Site2_exp_SLAD[,c(4)])
rownames(Site2_exp_SLADB) <- Site2_exp_SLAD$Species
names(Site2_exp_SLADB)[1]<-paste(c("SLA"))

Site2_exp_SLAS <- subset(Sites_exp_SLA, Site==2 & Depth=="S")
Site2_exp_SLASB <- as.data.frame(Site2_exp_SLAS[,c(4)])
rownames(Site2_exp_SLASB) <- Site2_exp_SLAS$Species
names(Site2_exp_SLASB)[1]<-paste(c("SLA"))

Site3_exp_SLAD <- subset(Sites_exp_SLA, Site==3 & Depth=="D")
Site3_exp_SLADB <- as.data.frame(Site3_exp_SLAD[,c(4)])
rownames(Site3_exp_SLADB) <- Site3_exp_SLAD$Species
names(Site3_exp_SLADB)[1]<-paste(c("SLA"))

Site3_exp_SLAS <- subset(Sites_exp_SLA, Site==3 & Depth=="S")
Site3_exp_SLASB <- as.data.frame(Site3_exp_SLAS[,c(4)])
rownames(Site3_exp_SLASB) <- Site3_exp_SLAS$Species
names(Site3_exp_SLASB)[1]<-paste(c("SLA"))

Site4_exp_SlAD <- subset(Sites_exp_SLA, Site==4 & Depth=="D")
Site4_exp_SlADB <- as.data.frame(Site4_exp_SlAD[,c(4)])
rownames(Site4_exp_SlADB) <- Site4_exp_SlAD$Species
names(Site4_exp_SlADB)[1]<-paste(c("SLA"))

Site4_exp_SLAS <- subset(Sites_exp_SLA, Site==4 & Depth=="S")
Site4_exp_SLASB <- as.data.frame(Site4_exp_SLAS[,c(4)])
rownames(Site4_exp_SLASB) <- Site4_exp_SLAS$Species
names(Site4_exp_SLASB)[1]<-paste(c("SLA"))

Site5_exp_SLAS <- subset(Sites_exp_SLA, Site==5 & Depth=="S")
Site5_exp_SLASB <- as.data.frame(Site5_exp_SLAS[,c(4)])
rownames(Site5_exp_SLASB) <- Site5_exp_SLAS$Species
names(Site5_exp_SLASB)[1]<-paste(c("SLA"))

############ LAP  ############ 
Site1_exp_LAPD <- subset(Sites_exp_LAP, Site==1 & Depth=="D")
Site1_exp_LAPDB <- as.data.frame(Site1_exp_LAPD[,c(4)])
rownames(Site1_exp_LAPDB) <- Site1_exp_LAPD$Species
names(Site1_exp_LAPDB)[1]<-paste(c("LAP"))

Site1_exp_LAPS <- subset(Sites_exp_LAP, Site==1 & Depth=="S")
Site1_exp_LAPSB <- as.data.frame(Site1_exp_LAPS[,c(4)])
rownames(Site1_exp_LAPSB) <- Site1_exp_LAPS$Species
names(Site1_exp_LAPSB)[1]<-paste(c("LAP"))

Site2_exp_LAPD <- subset(Sites_exp_LAP, Site==2 & Depth=="D")
Site2_exp_LAPDB <- as.data.frame(Site2_exp_LAPD[,c(4)])
rownames(Site2_exp_LAPDB) <- Site2_exp_LAPD$Species
names(Site2_exp_LAPDB)[1]<-paste(c("LAP"))

Site2_exp_LAPS <- subset(Sites_exp_LAP, Site==2 & Depth=="S")
Site2_exp_LAPSB <- as.data.frame(Site2_exp_LAPS[,c(4)])
rownames(Site2_exp_LAPSB) <- Site2_exp_LAPS$Species
names(Site2_exp_LAPSB)[1]<-paste(c("LAP"))

Site3_exp_LAPD <- subset(Sites_exp_LAP, Site==3 & Depth=="D")
Site3_exp_LAPDB <- as.data.frame(Site3_exp_LAPD[,c(4)])
rownames(Site3_exp_LAPDB) <- Site3_exp_LAPD$Species
names(Site3_exp_LAPDB)[1]<-paste(c("LAP"))

Site3_exp_LAPS <- subset(Sites_exp_LAP, Site==3 & Depth=="S")
Site3_exp_LAPSB <- as.data.frame(Site3_exp_LAPS[,c(4)])
rownames(Site3_exp_LAPSB) <- Site3_exp_LAPS$Species
names(Site3_exp_LAPSB)[1]<-paste(c("LAP"))

Site4_exp_LAPD <- subset(Sites_exp_LAP, Site==4 & Depth=="D")
Site4_exp_LAPDB <- as.data.frame(Site4_exp_LAPD[,c(4)])
rownames(Site4_exp_LAPDB) <- Site4_exp_LAPD$Species
names(Site4_exp_LAPDB)[1]<-paste(c("LAP"))

Site4_exp_LAPS <- subset(Sites_exp_LAP, Site==4 & Depth=="S")
Site4_exp_LAPSB <- as.data.frame(Site4_exp_LAPS[,c(4)])
rownames(Site4_exp_LAPSB) <- Site4_exp_LAPS$Species
names(Site4_exp_LAPSB)[1]<-paste(c("LAP"))

Site5_exp_LAPS <- subset(Sites_exp_LAP, Site==5 & Depth=="S")
Site5_exp_LAPSB <- as.data.frame(Site5_exp_LAPS[,c(4)])
rownames(Site5_exp_LAPSB) <- Site5_exp_LAPS$Species
names(Site5_exp_LAPSB)[1]<-paste(c("LAP"))

########################################################
### Community weighted mean value for leaf traits ######
########################################################

############ SLA  ############ 
Indices_exp1_SLAD <- FD::dbFD(Site1_exp_SLADB,Plot_Abun_exp1_deep, w.abun = TRUE)
Indices_exp1_SLAS <- FD::dbFD(Site1_exp_SLASB,Plot_Abun_exp1_shallow, w.abun = TRUE)

Indices_exp2_SLAS <- FD::dbFD(Site2_exp_SLASB,Plot_Abun_exp2_shallow, w.abun = TRUE)
Indices_exp2_SLAD <- Indices_exp1_SLAS
Indices_exp2_SLAD$FDis <- c(rep("0",8))
Indices_exp2_SLAD$CWM <- c(rep("0.3375753",8))

Indices_exp3_SLAD <- FD::dbFD(Site3_exp_SLADB,Plot_Abun_exp3_deep, w.abun = TRUE)
Indices_exp3_SLAS <- FD::dbFD(Site3_exp_SLASB,Plot_Abun_exp3_shallow, w.abun = TRUE)

Indices_exp4_SLAD <- FD::dbFD(Site4_exp_SlADB,Plot_Abun_exp4_deep, w.abun = TRUE)
Indices_exp4_SLAS <- FD::dbFD(Site4_exp_SLASB,Plot_Abun_exp4_shallow, w.abun = TRUE)

Indices_exp5_SLAS <- FD::dbFD(Site5_exp_SLASB,Plot_Abun_exp5_shallow, w.abun = TRUE)

############ LAP  ############ 
Indices_exp1_LAPD <- FD::dbFD(Site1_exp_LAPDB,Plot_Abun_exp1_deep, w.abun = TRUE)
Indices_exp1_LAPS <- FD::dbFD(Site1_exp_LAPSB,Plot_Abun_exp1_shallow, w.abun = TRUE)

Indices_exp2_LAPS <- FD::dbFD(Site2_exp_LAPSB,Plot_Abun_exp2_shallow, w.abun = TRUE)
Indices_exp2_LAPD <- Indices_exp1_LAPS
Indices_exp2_LAPD$FDis <- c(rep("0",8))
Indices_exp2_LAPD$CWM <- c(rep("0.1719425",8))

Indices_exp3_LAPD <- FD::dbFD(Site3_exp_LAPDB,Plot_Abun_exp3_deep, w.abun = TRUE)
Indices_exp3_LAPS <- FD::dbFD(Site3_exp_LAPSB,Plot_Abun_exp3_shallow, w.abun = TRUE)

Indices_exp4_LAPD <- FD::dbFD(Site4_exp_LAPDB,Plot_Abun_exp4_deep, w.abun = TRUE)
Indices_exp4_LAPS <- FD::dbFD(Site4_exp_LAPSB,Plot_Abun_exp4_shallow, w.abun = TRUE)

Indices_exp5_LAPS <- FD::dbFD(Site5_exp_LAPSB,Plot_Abun_exp5_shallow, w.abun = TRUE)

########################################################
###         Combibine exposed site indices        ######
########################################################

###### Community weighted mean

############ SLA  ############ 
CWMExp1SLAS <-as.data.frame(Indices_exp1_SLAS$CWM)
CWMExp1SLAD <-as.data.frame(Indices_exp1_SLAD$CWM)

CWMExp2SLAS <-as.data.frame(Indices_exp2_SLAS$CWM)
colnames(CWMExp2SLAS) <- c("SLA")
CWMExp2SLAD <-as.data.frame(Indices_exp2_SLAD$CWM)
CWMExp2SLAD <- as.data.frame(CWMExp2SLAD[c(1:6),]) 
colnames(CWMExp2SLAD) <- c("SLA")

CWMExp3SLAS <-as.data.frame(Indices_exp3_SLAS$CWM)
CWMExp3SLAD <-as.data.frame(Indices_exp3_SLAD$CWM)

CWMExp4SLAS <-as.data.frame(Indices_exp4_SLAS$CWM)
CWMExp4SLAD <-as.data.frame(Indices_exp4_SLAD$CWM)

CWMExp5SLAS <-as.data.frame(Indices_exp5_SLAS$CWM)

Indices_leaf_Expsites <-rbind.data.frame(CWMExp1SLAS,CWMExp2SLAS,CWMExp3SLAS,CWMExp4SLAS,CWMExp5SLAS,
                                                      CWMExp1SLAD,CWMExp2SLAD,CWMExp3SLAD,CWMExp4SLAD)
colnames(Indices_leaf_Expsites) <- c("CWM_SLA")
Indices_leaf_Expsites$Depth <- c(rep("Shallow",31),rep("Deep",26))
Indices_leaf_Expsites$Exposure <- c(rep("Exposed",57))

############ LAP  ############ 
CWMExp1LAPS <-as.data.frame(Indices_exp1_LAPS$CWM)
CWMExp1LAPD <-as.data.frame(Indices_exp1_LAPD$CWM)

CWMExp2LAPS <-as.data.frame(Indices_exp2_LAPS$CWM)
colnames(CWMExp2LAPS) <- c("LAP")
CWMExp2LAPD <-as.data.frame(Indices_exp2_LAPD$CWM)
CWMExp2LAPD <- as.data.frame(CWMExp2LAPD[c(1:6),]) 
colnames(CWMExp2LAPD) <- c("LAP")

CWMExp3LAPS <-as.data.frame(Indices_exp3_LAPS$CWM)
CWMExp3LAPD <-as.data.frame(Indices_exp3_LAPD$CWM)

CWMExp4LAPS <-as.data.frame(Indices_exp4_LAPS$CWM)
CWMExp4LAPD <-as.data.frame(Indices_exp4_LAPD$CWM)

CWMExp5LAPS <-as.data.frame(Indices_exp5_LAPS$CWM)

Indices_leaf_Expsites$CWM_LAP <-rbind.data.frame(CWMExp1LAPS,CWMExp2LAPS,CWMExp3LAPS,CWMExp4LAPS,CWMExp5LAPS,
                                         CWMExp1LAPD,CWMExp2LAPD,CWMExp3LAPD,CWMExp4LAPD)
###### Functional Dispersion

############ SLA  ############ 
FDisExp1SLAS <-as.data.frame(Indices_exp1_SLAS$FDis)
colnames(FDisExp1SLAS) <- c("FDis")
FDisExp1SLAD <-as.data.frame(Indices_exp1_SLAD$FDis)
colnames(FDisExp1SLAD) <- c("FDis")

FDisExp2SLAS <-as.data.frame(Indices_exp2_SLAS$FDis)
colnames(FDisExp2SLAS) <- c("FDis")
FDisExp2SLAD <-as.data.frame(Indices_exp2_SLAD$FDis)
FDisExp2SLAD <- as.data.frame(FDisExp2SLAD[c(1:6),]) 
colnames(FDisExp2SLAD) <- c("FDis")

FDisExp3SLAS <-as.data.frame(Indices_exp3_SLAS$FDis)
colnames(FDisExp3SLAS) <- c("FDis")
FDisExp3SLAD <-as.data.frame(Indices_exp3_SLAD$FDis)
colnames(FDisExp3SLAD) <- c("FDis")

FDisExp4SLAS <-as.data.frame(Indices_exp4_SLAS$FDis)
colnames(FDisExp4SLAS) <- c("FDis")
FDisExp4SLAD <-as.data.frame(Indices_exp4_SLAD$FDis)
colnames(FDisExp4SLAD) <- c("FDis")

FDisExp5SLAS <-as.data.frame(Indices_exp5_SLAS$FDis)
colnames(FDisExp5SLAS) <- c("FDis")

Indices_leaf_Expsites$FDis_SLA <-rbind.data.frame(FDisExp1SLAS,FDisExp2SLAS,FDisExp3SLAS,FDisExp4SLAS,FDisExp5SLAS,
                                                       FDisExp1SLAD,FDisExp2SLAD,FDisExp3SLAD,FDisExp4SLAD)
############ LAP  ############ 
FDisExp1LAPS <-as.data.frame(Indices_exp1_LAPS$FDis)
colnames(FDisExp1LAPS) <- c("FDis")
FDisExp1LAPD <-as.data.frame(Indices_exp1_LAPD$FDis)
colnames(FDisExp1LAPD) <- c("FDis")

FDisExp2LAPS <-as.data.frame(Indices_exp2_LAPS$FDis)
colnames(FDisExp2LAPS) <- c("FDis")
FDisExp2LAPD <-as.data.frame(Indices_exp2_LAPD$FDis)
FDisExp2LAPD <- as.data.frame(FDisExp2LAPD[c(1:6),]) 
colnames(FDisExp2LAPD) <- c("FDis")

FDisExp3LAPS <-as.data.frame(Indices_exp3_LAPS$FDis)
colnames(FDisExp3LAPS) <- c("FDis")
FDisExp3LAPD <-as.data.frame(Indices_exp3_LAPD$FDis)
colnames(FDisExp3LAPD) <- c("FDis")

FDisExp4LAPS <-as.data.frame(Indices_exp4_LAPS$FDis)
colnames(FDisExp4LAPS) <- c("FDis")
FDisExp4LAPD <-as.data.frame(Indices_exp4_LAPD$FDis)
colnames(FDisExp4LAPD) <- c("FDis")

FDisExp5LAPS <-as.data.frame(Indices_exp5_LAPS$FDis)
colnames(FDisExp5LAPS) <- c("FDis")

Indices_leaf_Expsites$FDis_LAP <-rbind.data.frame(FDisExp1LAPS,FDisExp2LAPS,FDisExp3LAPS,FDisExp4LAPS,FDisExp5LAPS,
                                                       FDisExp1LAPD,FDisExp2LAPD,FDisExp3LAPD,FDisExp4LAPD)
Indices_leaf_Expsites <- tibble::rownames_to_column(Indices_leaf_Expsites,"Site")
rownames(Indices_leaf_Expsites)<-c(1:57)

##########################################
###########          #####################
###########   SEMI  ######################
###########         ######################
##########################################

Spatial_Campaign_data_semi <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Semi", na = "NA")
TraitData_semiA <- Spatial_Campaign_data_semi[-c(43),c(1:4, 36,38)]
names(TraitData_semiA)[5]<-paste(c("SLA"))
names(TraitData_semiA)[6]<-paste(c("LAP"))
TraitData_semiB <- Spatial_Campaign_data_semi[-c(43),c(1:4, 37,39)]
names(TraitData_semiB)[5]<-paste(c("SLA"))
names(TraitData_semiB)[6]<-paste(c("LAP"))
TraitData_semi <- rbind (TraitData_semiA,TraitData_semiB )

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

Sites_semi_SLA <-aggregate(x=TraitData_semi$SLA, by=list(TraitData_semi$`Site number`,TraitData_semi$Depth, TraitData_semi$Species), FUN=median)
names(Sites_semi_SLA)[1:4]<-paste(c("Site","Depth","Species", "SLA"))

Sites_semi_LAP <-aggregate(x=TraitData_semi$LAP, by=list(TraitData_semi$`Site number`,TraitData_semi$Depth, TraitData_semi$Species), FUN=median)
names(Sites_semi_LAP)[1:4]<-paste(c("Site","Depth","Species", "LAP"))

############ SLA  ############ 
Site1_semi_SLAD<- subset(Sites_semi_SLA, Site==1 & Depth=="D")
Site1_semi_SLADB <- as.data.frame(Site1_semi_SLAD[,c(4)])
rownames(Site1_semi_SLADB) <- Site1_semi_SLAD$Species
names(Site1_semi_SLADB)[1]<-paste(c("SLA"))

Site1_semi_SLAS <- subset(Sites_semi_SLA, Site==1 & Depth=="S")
Site1_semi_SLASB <- as.data.frame(Site1_semi_SLAS[,c(4)])
rownames(Site1_semi_SLASB) <- Site1_semi_SLAS$Species
names(Site1_semi_SLASB)[1]<-paste(c("SLA"))

Site2_semi_SLAD <- subset(Sites_semi_SLA, Site==2 & Depth=="D")
Site2_semi_SLADB <- as.data.frame(Site2_semi_SLAD[,c(4)])
rownames(Site2_semi_SLADB) <- Site2_semi_SLAD$Species
names(Site2_semi_SLADB)[1]<-paste(c("SLA"))

Site2_semi_SLAS <- subset(Sites_semi_SLA, Site==2 & Depth=="S")
Site2_semi_SLASB <- as.data.frame(Site2_semi_SLAS[,c(4)])
rownames(Site2_semi_SLASB) <- Site2_semi_SLAS$Species
names(Site2_semi_SLASB)[1]<-paste(c("SLA"))

Site3_semi_SLAD <- subset(Sites_semi_SLA, Site==3 & Depth=="D")
Site3_semi_SLADB <- as.data.frame(Site3_semi_SLAD[,c(4)])
rownames(Site3_semi_SLADB) <- Site3_semi_SLAD$Species
names(Site3_semi_SLADB)[1]<-paste(c("SLA"))

Site3_semi_SLAS <- subset(Sites_semi_SLA, Site==3 & Depth=="S")
Site3_semi_SLASB <- as.data.frame(Site3_semi_SLAS[,c(4)])
rownames(Site3_semi_SLASB) <- Site3_semi_SLAS$Species
names(Site3_semi_SLASB)[1]<-paste(c("SLA"))

Site4_semi_SLAD <- subset(Sites_semi_SLA, Site==4 & Depth=="D")
Site4_semi_SLADB <- as.data.frame(Site4_semi_SLAD[,c(4)])
rownames(Site4_semi_SLADB) <- Site4_semi_SLAD$Species
names(Site4_semi_SLADB)[1]<-paste(c("SLA"))

Site4_semi_SLAS <- subset(Sites_semi_SLA, Site==4 & Depth=="S")
Site4_semi_SLASB <- as.data.frame(Site4_semi_SLAS[,c(4)])
rownames(Site4_semi_SLASB) <- Site4_semi_SLAS$Species
names(Site4_semi_SLASB)[1]<-paste(c("SLA"))

Site5_semi_SLAD <- subset(Sites_semi_SLA, Site==5 & Depth=="D")
Site5_semi_SLADB <- as.data.frame(Site5_semi_SLAD[,c(4)])
rownames(Site5_semi_SLADB) <- Site5_semi_SLAD$Species
names(Site5_semi_SLADB)[1]<-paste(c("SLA"))

Site5_semi_SLAS <- subset(Sites_semi_SLA, Site==5 & Depth=="S")
Site5_semi_SLASB <- as.data.frame(Site5_semi_SLAS[,c(4)])
rownames(Site5_semi_SLASB) <- Site5_semi_SLAS$Species
names(Site5_semi_SLASB)[1]<-paste(c("SLA"))

############ LAP  ############ 
Site1_semi_LAPD<- subset(Sites_semi_LAP, Site==1 & Depth=="D")
Site1_semi_LAPDB <- as.data.frame(Site1_semi_LAPD[,c(4)])
rownames(Site1_semi_LAPDB) <- Site1_semi_LAPD$Species
names(Site1_semi_LAPDB)[1]<-paste(c("LAP"))

Site1_semi_LAPS <- subset(Sites_semi_LAP, Site==1 & Depth=="S")
Site1_semi_LAPSB <- as.data.frame(Site1_semi_LAPS[,c(4)])
rownames(Site1_semi_LAPSB) <- Site1_semi_LAPS$Species
names(Site1_semi_LAPSB)[1]<-paste(c("LAP"))

Site2_semi_LAPD <- subset(Sites_semi_LAP, Site==2 & Depth=="D")
Site2_semi_LAPDB <- as.data.frame(Site2_semi_LAPD[,c(4)])
rownames(Site2_semi_LAPDB) <- Site2_semi_LAPD$Species
names(Site2_semi_LAPDB)[1]<-paste(c("LAP"))

Site2_semi_LAPS <- subset(Sites_semi_LAP, Site==2 & Depth=="S")
Site2_semi_LAPSB <- as.data.frame(Site2_semi_LAPS[,c(4)])
rownames(Site2_semi_LAPSB) <- Site2_semi_LAPS$Species
names(Site2_semi_LAPSB)[1]<-paste(c("LAP"))

Site3_semi_LAPD <- subset(Sites_semi_LAP, Site==3 & Depth=="D")
Site3_semi_LAPDB <- as.data.frame(Site3_semi_LAPD[,c(4)])
rownames(Site3_semi_LAPDB) <- Site3_semi_LAPD$Species
names(Site3_semi_LAPDB)[1]<-paste(c("LAP"))

Site3_semi_LAPS <- subset(Sites_semi_LAP, Site==3 & Depth=="S")
Site3_semi_LAPSB <- as.data.frame(Site3_semi_LAPS[,c(4)])
rownames(Site3_semi_LAPSB) <- Site3_semi_LAPS$Species
names(Site3_semi_LAPSB)[1]<-paste(c("LAP"))

Site4_semi_LAPD <- subset(Sites_semi_LAP, Site==4 & Depth=="D")
Site4_semi_LAPDB <- as.data.frame(Site4_semi_LAPD[,c(4)])
rownames(Site4_semi_LAPDB) <- Site4_semi_LAPD$Species
names(Site4_semi_LAPDB)[1]<-paste(c("LAP"))

Site4_semi_LAPS <- subset(Sites_semi_LAP, Site==4 & Depth=="S")
Site4_semi_LAPSB <- as.data.frame(Site4_semi_LAPS[,c(4)])
rownames(Site4_semi_LAPSB) <- Site4_semi_LAPS$Species
names(Site4_semi_LAPSB)[1]<-paste(c("LAP"))

Site5_semi_LAPD <- subset(Sites_semi_LAP, Site==5 & Depth=="D")
Site5_semi_LAPDB <- as.data.frame(Site5_semi_LAPD[,c(4)])
rownames(Site5_semi_LAPDB) <- Site5_semi_LAPD$Species
names(Site5_semi_LAPDB)[1]<-paste(c("LAP"))

Site5_semi_LAPS <- subset(Sites_semi_LAP, Site==5 & Depth=="S")
Site5_semi_LAPSB <- as.data.frame(Site5_semi_LAPS[,c(4)])
rownames(Site5_semi_LAPSB) <- Site5_semi_LAPS$Species
names(Site5_semi_LAPSB)[1]<-paste(c("LAP"))

########################################################
### Community weighted mean value for leaf traits ######
########################################################

############ SLA  ############ 
Indices_semi1_SLAD <- FD::dbFD(Site1_semi_SLADB,Plot_Abun_semi1_deep, w.abun = TRUE)
Indices_semi1_SLAS <- FD::dbFD(Site1_semi_SLASB,Plot_Abun_semi1_shallow, w.abun = TRUE)

Indices_semi2_SLAD <- FD::dbFD(Site2_semi_SLADB,Plot_Abun_semi2_deep, w.abun = TRUE)
Indices_semi2_SLAS <- FD::dbFD(Site2_semi_SLASB,Plot_Abun_semi2_shallow, w.abun = TRUE)

Indices_semi3_SLAD <- FD::dbFD(Site3_semi_SLADB,Plot_Abun_semi3_deep, w.abun = TRUE)
Indices_semi3_SLAS <- FD::dbFD(Site3_semi_SLASB,Plot_Abun_semi3_shallow, w.abun = TRUE)

Indices_semi4_SLAD <- FD::dbFD(Site4_semi_SLADB,Plot_Abun_semi4_deep, w.abun = TRUE)
Indices_semi4_SLAS <- FD::dbFD(Site4_semi_SLASB,Plot_Abun_semi4_shallow, w.abun = TRUE)

Indices_semi5_SLAD <- FD::dbFD(Site5_semi_SLADB,Plot_Abun_semi5_deep, w.abun = TRUE)
Indices_semi5_SLAS <- FD::dbFD(Site5_semi_SLASB,Plot_Abun_semi5_shallow, w.abun = TRUE)

############ LAP  ############ 

Indices_semi1_LAPD <- FD::dbFD(Site1_semi_LAPDB,Plot_Abun_semi1_deep, w.abun = TRUE)
Indices_semi1_LAPS <- FD::dbFD(Site1_semi_LAPSB,Plot_Abun_semi1_shallow, w.abun = TRUE)

Indices_semi2_LAPD <- FD::dbFD(Site2_semi_LAPDB,Plot_Abun_semi2_deep, w.abun = TRUE)
Indices_semi2_LAPS <- FD::dbFD(Site2_semi_LAPSB,Plot_Abun_semi2_shallow, w.abun = TRUE)

Indices_semi3_LAPD <- FD::dbFD(Site3_semi_LAPDB,Plot_Abun_semi3_deep, w.abun = TRUE)
Indices_semi3_LAPS <- FD::dbFD(Site3_semi_LAPSB,Plot_Abun_semi3_shallow, w.abun = TRUE)

Indices_semi4_LAPD <- FD::dbFD(Site4_semi_LAPDB,Plot_Abun_semi4_deep, w.abun = TRUE)
Indices_semi4_LAPS <- FD::dbFD(Site4_semi_LAPSB,Plot_Abun_semi4_shallow, w.abun = TRUE)

Indices_semi5_LAPD <- FD::dbFD(Site5_semi_LAPDB,Plot_Abun_semi5_deep, w.abun = TRUE)
Indices_semi5_LAPS <- FD::dbFD(Site5_semi_LAPSB,Plot_Abun_semi5_shallow, w.abun = TRUE)


########################################################
###           Combine semi site indices           ######
########################################################

###### Community weighted mean

############ SLA  ############
CWMSemi1SLAS <-as.data.frame(Indices_semi1_SLAS$CWM)
CWMSemi1SLAD <-as.data.frame(Indices_semi1_SLAD$CWM)

CWMSemi2SLAS <-as.data.frame(Indices_semi2_SLAS$CWM)
CWMSemi2SLAD <-as.data.frame(Indices_semi2_SLAD$CWM)

CWMSemi3SLAS <-as.data.frame(Indices_semi3_SLAS$CWM)
CWMSemi3SLAD <-as.data.frame(Indices_semi3_SLAD$CWM)

CWMSemi4SLAS <-as.data.frame(Indices_semi4_SLAS$CWM)
CWMSemi4SLAD <-as.data.frame(Indices_semi4_SLAD$CWM)

CWMSemi5SLAS <-as.data.frame(Indices_semi5_SLAS$CWM)
CWMSemi5SLAD <-as.data.frame(Indices_semi5_SLAD$CWM)

Indices_leaf_Semisites <-rbind.data.frame(CWMSemi1SLAS,CWMSemi2SLAS,CWMSemi3SLAS,CWMSemi4SLAS,CWMSemi5SLAS,
                                          CWMSemi1SLAD,CWMSemi2SLAD,CWMSemi3SLAD,CWMSemi4SLAD,CWMSemi4SLAD)
colnames(Indices_leaf_Semisites) <- c("CWM_SLA")
Indices_leaf_Semisites$Depth <- c(rep("Shallow",30),rep("Deep",30))
Indices_leaf_Semisites$Exposure <- c(rep("Semi",60))

############ LAP  ############

CWMSemi1LAPS <-as.data.frame(Indices_semi1_LAPS$CWM)
CWMSemi1LAPD <-as.data.frame(Indices_semi1_LAPD$CWM)

CWMSemi2LAPS <-as.data.frame(Indices_semi2_LAPS$CWM)
CWMSemi2LAPD <-as.data.frame(Indices_semi2_LAPD$CWM)

CWMSemi3LAPS <-as.data.frame(Indices_semi3_LAPS$CWM)
CWMSemi3LAPD <-as.data.frame(Indices_semi3_LAPD$CWM)

CWMSemi4LAPS <-as.data.frame(Indices_semi4_LAPS$CWM)
CWMSemi4LAPD <-as.data.frame(Indices_semi4_LAPD$CWM)

CWMSemi5LAPS <-as.data.frame(Indices_semi5_LAPS$CWM)
CWMSemi5LAPD <-as.data.frame(Indices_semi5_LAPD$CWM)

Indices_leaf_Semisites$CWM_LAP <-rbind.data.frame(CWMSemi1LAPS,CWMSemi2LAPS,CWMSemi3LAPS,CWMSemi4LAPS,CWMSemi5LAPS,
                                                  CWMSemi1LAPD,CWMSemi2LAPD,CWMSemi3LAPD,CWMSemi4LAPD,CWMSemi5LAPD)

###### Functional Dispersion
############ SLA  ############
FDisSemi1SLAS <-as.data.frame(Indices_semi1_SLAS$FDis)
colnames(FDisSemi1SLAS) <- c("FDis")
FDisSemi1SLAD <-as.data.frame(Indices_semi1_SLAD$FDis)
colnames(FDisSemi1SLAD) <- c("FDis")

FDisSemi2SLAS <-as.data.frame(Indices_semi2_SLAS$FDis)
colnames(FDisSemi2SLAS) <- c("FDis")
FDisSemi2SLAD <-as.data.frame(Indices_semi2_SLAD$FDis)
colnames(FDisSemi2SLAD) <- c("FDis")

FDisSemi3SLAS <-as.data.frame(Indices_semi3_SLAS$FDis)
colnames(FDisSemi3SLAS) <- c("FDis")
FDisSemi3SLAD <-as.data.frame(Indices_semi3_SLAD$FDis)
colnames(FDisSemi3SLAD) <- c("FDis")

FDisSemi4SLAS <-as.data.frame(Indices_semi4_SLAS$FDis)
colnames(FDisSemi4SLAS) <- c("FDis")
FDisSemi4SLAD <-as.data.frame(Indices_semi4_SLAD$FDis)
colnames(FDisSemi4SLAD) <- c("FDis")

FDisSemi5SLAS <-as.data.frame(Indices_semi5_SLAS$FDis)
colnames(FDisSemi5SLAS) <- c("FDis")
FDisSemi5SLAD <-as.data.frame(Indices_semi5_SLAD$FDis)
colnames(FDisSemi5SLAD) <- c("FDis")

Indices_leaf_Semisites$FDis_SLA <-rbind.data.frame(FDisSemi1SLAS,FDisSemi2SLAS,FDisSemi3SLAS,FDisSemi4SLAS,FDisSemi5SLAS,
                                                        FDisSemi1SLAD,FDisSemi2SLAD,FDisSemi3SLAD,FDisSemi4SLAD, FDisSemi5SLAD)
############ LAP  ############

FDisSemi1LAPS <-as.data.frame(Indices_semi1_LAPS$FDis)
colnames(FDisSemi1LAPS) <- c("FDis")
FDisSemi1LAPD <-as.data.frame(Indices_semi1_LAPD$FDis)
colnames(FDisSemi1LAPD) <- c("FDis")

FDisSemi2LAPS <-as.data.frame(Indices_semi2_LAPS$FDis)
colnames(FDisSemi2LAPS) <- c("FDis")
FDisSemi2LAPD <-as.data.frame(Indices_semi2_LAPD$FDis)
colnames(FDisSemi2LAPD) <- c("FDis")

FDisSemi3LAPS <-as.data.frame(Indices_semi3_LAPS$FDis)
colnames(FDisSemi3LAPS) <- c("FDis")
FDisSemi3LAPD <-as.data.frame(Indices_semi3_LAPD$FDis)
colnames(FDisSemi3LAPD) <- c("FDis")

FDisSemi4LAPS <-as.data.frame(Indices_semi4_LAPS$FDis)
colnames(FDisSemi4LAPS) <- c("FDis")
FDisSemi4LAPD <-as.data.frame(Indices_semi4_LAPD$FDis)
colnames(FDisSemi4LAPD) <- c("FDis")

FDisSemi5LAPS <-as.data.frame(Indices_semi5_LAPS$FDis)
colnames(FDisSemi5LAPS) <- c("FDis")
FDisSemi5LAPD <-as.data.frame(Indices_semi5_LAPD$FDis)
colnames(FDisSemi5LAPD) <- c("FDis")

Indices_leaf_Semisites$FDis_LAP <-rbind.data.frame(FDisSemi1LAPS,FDisSemi2LAPS,FDisSemi3LAPS,FDisSemi4LAPS,FDisSemi5LAPS,
                                                        FDisSemi1LAPD,FDisSemi2LAPD,FDisSemi3LAPD,FDisSemi4LAPD, FDisSemi5LAPD)
Indices_leaf_Semisites <- tibble::rownames_to_column(Indices_leaf_Semisites,"Site")
rownames(Indices_leaf_Semisites)<-c(1:60)

##############################################
###########              #####################
###########  Sheltered   #####################
###########              #####################
##############################################

Spatial_Campaign_data_shel <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Sheltered", na = "NA")

TraitData_shelA <- Spatial_Campaign_data_shel[-c(2),c(1:4, 40,42)]
names(TraitData_shelA)[5]<-paste(c("SLA"))
names(TraitData_shelA)[6]<-paste(c("LAP"))
TraitData_shelB <- Spatial_Campaign_data_shel[-c(2),c(1:4, 41,43)]
names(TraitData_shelB)[5]<-paste(c("SLA"))
names(TraitData_shelB)[6]<-paste(c("LAP"))
TraitData_shel <- rbind (TraitData_shelA,TraitData_shelB )

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

Sites_shel_SLA <- aggregate(x=TraitData_shel$SLA, by=list(TraitData_shel$Depth, TraitData_shel$Species), FUN=median)
Sites_shel_LAP <- aggregate(x=TraitData_shel$LAP, by=list(TraitData_shel$Depth, TraitData_shel$Species), FUN=median)
names(Sites_shel_SLA)[1:3]<-paste(c("Depth","Species", "SLA"))
names(Sites_shel_LAP)[1:3]<-paste(c("Depth","Species", "LAP"))

Sites_shel_SLAB <- as.data.frame(Sites_shel_SLA[,c(3)])
rownames(Sites_shel_SLAB) <- Sites_shel_SLA$Species
names(Sites_shel_SLAB)[1]<-paste(c("SLA"))

Sites_shel_LAPB <- as.data.frame(Sites_shel_LAP[,c(3)])
rownames(Sites_shel_LAPB) <- Sites_shel_LAP$Species
names(Sites_shel_LAPB)[1]<-paste(c("LAP"))

########################################################
### Community weighted mean value for leaf traits ######
########################################################

Indices_shel_SLA <- FD::dbFD(Sites_shel_SLAB,Plot_Abun_shel, w.abun = TRUE)
Indices_shel_LAP <- FD::dbFD(Sites_shel_LAPB,Plot_Abun_shel, w.abun = TRUE)

########################################################
###         Combine sheltered site indices        ######
########################################################

###### Community weighted mean
CWMshel_SLA <-as.data.frame(Indices_shel_SLA$CWM)
CWMshel_LAP <-as.data.frame(Indices_shel_LAP$CWM)

Indices_Leaf_shelsites <- CWMshel_SLA
colnames(Indices_Leaf_shelsites) <- c("CWM_SLA")
Indices_Leaf_shelsites$CWM_LAP <- CWMshel_LAP
Indices_Leaf_shelsites$Depth <- c(rep("Shallow",30))
Indices_Leaf_shelsites$Exposure <- c(rep("Sheltered",30))

###### Functional Dispersion
FDisshel_SLA <-as.data.frame(Indices_shel_SLA$FDis)
colnames(FDisshel_SLA) <- c("FDis")
FDisshel_LAP <-as.data.frame(Indices_shel_LAP$FDis)
colnames(FDisshel_LAP) <- c("FDis")

Indices_Leaf_shelsites$FDis_SLA <- FDisshel_SLA
Indices_Leaf_shelsites$FDis_LAP <- FDisshel_LAP

Indices_Leaf_shelsites <- tibble::rownames_to_column(Indices_Leaf_shelsites,"Site")
rownames(Indices_Leaf_shelsites)<-c(1:30)
##########################################
###########          #####################
########### POJO    ######################
###########         ######################
##########################################

Spatial_Campaign_data_pojo <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Pojo", na = "NA")

TraitData_pojoA <- Spatial_Campaign_data_pojo[-c(1,4,6,7),c(1:4, 38,40)]
names(TraitData_pojoA)[5]<-paste(c("SLA"))
names(TraitData_pojoA)[6]<-paste(c("LAP"))
TraitData_pojoB <- Spatial_Campaign_data_pojo[-c(1,4,6,7),c(1:4, 39,41)]
names(TraitData_pojoB)[5]<-paste(c("SLA"))
names(TraitData_pojoB)[6]<-paste(c("LAP"))
TraitData_pojo <- rbind (TraitData_pojoA,TraitData_pojoB )

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

Sites_pojo_SLA <- aggregate(x=TraitData_pojo$SLA, by=list(TraitData_pojo$Depth, TraitData_pojo$Species), FUN=median)
Sites_pojo_LAP <- aggregate(x=TraitData_pojo$LAP, by=list(TraitData_pojo$Depth, TraitData_pojo$Species), FUN=median)
names(Sites_pojo_SLA)[1:3]<-paste(c("Depth","Species", "SLA"))
names(Sites_pojo_LAP)[1:3]<-paste(c("Depth","Species", "LAP"))

Site_pojo_SLA_D <- subset(Sites_pojo_SLA, Depth=="D")
Site_pojo_SLA_S <- subset(Sites_pojo_SLA, Depth=="S")

Site_pojo_LAP_D <- subset(Sites_pojo_LAP, Depth=="D")
Site_pojo_LAP_S <- subset(Sites_pojo_LAP, Depth=="S")

Site_pojo_SLA_DB <- as.data.frame(Site_pojo_SLA_D[,c(3)])
rownames(Site_pojo_SLA_DB) <- Site_pojo_SLA_D$Species
names(Site_pojo_SLA_DB)[1]<-paste(c("SLA"))

Site_pojo_SLA_SB <- as.data.frame(Site_pojo_SLA_S[,c(3)])
rownames(Site_pojo_SLA_SB) <- Site_pojo_SLA_S$Species
names(Site_pojo_SLA_SB)[1]<-paste(c("SLA"))

Site_pojo_LAP_DB <- as.data.frame(Site_pojo_LAP_D[,c(3)])
rownames(Site_pojo_LAP_DB) <- Site_pojo_LAP_D$Species
names(Site_pojo_LAP_DB)[1]<-paste(c("LAP"))

Site_pojo_LAP_SB <- as.data.frame(Site_pojo_LAP_S[,c(3)])
rownames(Site_pojo_LAP_SB) <- Site_pojo_LAP_S$Species
names(Site_pojo_LAP_SB)[1]<-paste(c("LAP"))


########################################################
### Community weighted mean value for leaf traits ######
########################################################

Indices_pojo_SLA_D <- FD::dbFD(Site_pojo_SLA_DB,Plot_Abun_pojo_deep, w.abun = TRUE)
Indices_pojo_SLA_S <- FD::dbFD(Site_pojo_SLA_SB,Plot_Abun_pojo_shallow, w.abun = TRUE)
Indices_pojo_LAP_D <- FD::dbFD(Site_pojo_LAP_DB,Plot_Abun_pojo_deep, w.abun = TRUE)
Indices_pojo_LAP_S <- FD::dbFD(Site_pojo_LAP_SB,Plot_Abun_pojo_shallow, w.abun = TRUE)

########################################################
###           Combine pojo site indices           ######
########################################################

###### Community weighted mean
CWMpojo_SLA_D <-as.data.frame(Indices_pojo_SLA_D$CWM)
CWMpojo_SLA_S <-as.data.frame(Indices_pojo_SLA_S$CWM)
CWMpojo_LAP_D <-as.data.frame(Indices_pojo_LAP_D$CWM)
CWMpojo_LAP_S <-as.data.frame(Indices_pojo_LAP_S$CWM)

Indices_Leaf_pojosites <- rbind(CWMpojo_SLA_D,CWMpojo_SLA_S)
colnames(Indices_Leaf_pojosites) <- c("CWM_SLA")
Indices_Leaf_pojosites$CWM_LAP <- rbind(CWMpojo_LAP_D,CWMpojo_LAP_S)
Indices_Leaf_pojosites$Depth <- c(rep("Deep",8),rep("Shallow",30))
Indices_Leaf_pojosites$Exposure <- c(rep("Pojo",38))

###### Functional Dispersion
FDispojo_SLA_D <-as.data.frame(Indices_pojo_SLA_D$FDis)
colnames(FDispojo_SLA_D) <- c("FDis")
FDispojo_SLA_S <-as.data.frame(Indices_pojo_SLA_S$FDis)
colnames(FDispojo_SLA_S) <- c("FDis")
FDispojo_LAP_D <-as.data.frame(Indices_pojo_LAP_D$FDis)
colnames(FDispojo_LAP_D) <- c("FDis")
FDispojo_LAP_S <-as.data.frame(Indices_pojo_LAP_S$FDis)
colnames(FDispojo_LAP_S) <- c("FDis")

Indices_Leaf_pojosites$FDis_SLA <- rbind(FDispojo_SLA_D,FDispojo_SLA_S)
Indices_Leaf_pojosites$FDis_LAP <- rbind(FDispojo_LAP_D,FDispojo_LAP_S)

Indices_Leaf_pojosites <- tibble::rownames_to_column(Indices_Leaf_pojosites,"Site")
rownames(Indices_Leaf_pojosites)<-c(1:38)
##################################################
###########                 ######################
###########    Combined     ######################
###########                 ######################
##################################################

Indices_leaf_Expsites <- as.matrix.data.frame(Indices_leaf_Expsites)
Indices_Leaf_pojosites <- as.matrix.data.frame(Indices_Leaf_pojosites)
Indices_leaf_Semisites <- as.matrix.data.frame(Indices_leaf_Semisites)
Indices_Leaf_shelsites <- as.matrix.data.frame(Indices_Leaf_shelsites)
Indices_leaf_Expsites <- as.data.frame(Indices_leaf_Expsites)
Indices_Leaf_pojosites <- as.data.frame(Indices_Leaf_pojosites)
Indices_leaf_Semisites <- as.data.frame(Indices_leaf_Semisites)
Indices_Leaf_shelsites <- as.data.frame(Indices_Leaf_shelsites)

Indices_leaf_AllsitesB<- rbind.data.frame(Indices_leaf_Expsites,Indices_leaf_Semisites,Indices_Leaf_pojosites,Indices_Leaf_shelsites)

Indices_leaf_AllsitesB$Site_number<- c(rep("1S",8),rep("2S",5),rep("3S",6),rep("4S",6),rep("5S",6),rep("1D",8),rep("2D",6),rep("3D",6),rep("4D",6),
                                      rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6),rep("1D",6),rep("2D",6),rep("3D",6),rep("4D",6),rep("5D",6),
                                      rep("2D",2),rep("3D",6),rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6),
                                      rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6))

Indices_leaf_AllsitesB$Exposure_Depth <- factor(paste(Indices_leaf_AllsitesB$Exposure, 
                                                     Indices_leaf_AllsitesB$Depth, sep = "_"),
                                               levels = c(
                                                 "Exposed_Shallow", "Exposed_Deep",
                                                 "Semi_Shallow", "Semi_Deep",
                                                 "Pojo_Shallow", "Pojo_Deep",
                                                 "Sheltered_Shallow"
                                               ))


Indices_leaf_AllsitesB$Site_Exposure <- factor(paste(Indices_leaf_AllsitesB$Site_number, 
                                                    Indices_leaf_AllsitesB$Exposure, sep = "_"),
                                              levels = c(
                                                "1S_Exposed", "2S_Exposed", "3S_Exposed", "4S_Exposed", "5S_Exposed", "1D_Exposed", "2D_Exposed", "3D_Exposed", "4D_Exposed",
                                                "1S_Semi", "2S_Semi", "3S_Semi", "4S_Semi", "5S_Semi", "1D_Semi", "2D_Semi", "3D_Semi", "4D_Semi", "5D_Semi",
                                                "1S_Pojo", "2S_Pojo", "3S_Pojo", "4S_Pojo", "5S_Pojo", "2D_Pojo", "3D_Pojo",
                                                "1S_Sheltered", "2S_Sheltered", "3S_Sheltered", "4S_Sheltered", "5S_Sheltered"
                                              ))



