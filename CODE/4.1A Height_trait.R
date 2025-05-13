##%######################################################%##
#                                                          #
####            Community indices height                ####
#                                                          #
##%######################################################%##

##########################################
###########          #####################
########### EXPOSED ######################
###########         ######################
##########################################
Spatial_Campaign_data_exp <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                        sheet = "Exposed", na = "NA")
TraitData_exp <- Spatial_Campaign_data_exp[,c(1:5)]
names(TraitData_exp)[5]<-paste(c("Height"))
names(TraitData_exp)[2]<-paste(c("Site_number"))

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

Sites_exp <-aggregate(x=TraitData_exp$Height, by=list(TraitData_exp$Site_number,TraitData_exp$Depth, TraitData_exp$Species), FUN=max)
names(Sites_exp)[1:4]<-paste(c("Site","Depth","Species", "Height"))

Site1_exp_deep <- subset(Sites_exp, Site==1 & Depth=="D")
Site1_exp_deepB <- as.data.frame(Site1_exp_deep[,c(4)])
rownames(Site1_exp_deepB) <- Site1_exp_deep$Species
names(Site1_exp_deepB)[1]<-paste(c("Height"))

Site1_exp_shallow <- subset(Sites_exp, Site==1 & Depth=="S")
Site1_exp_shallowB <- as.data.frame(Site1_exp_shallow[,c(4)])
rownames(Site1_exp_shallowB) <- Site1_exp_shallow$Species
names(Site1_exp_shallowB)[1]<-paste(c("Height"))

Site2_exp_deep <- subset(Sites_exp, Site==2 & Depth=="D")
Site2_exp_deepB <- as.data.frame(Site2_exp_deep[,c(4)])
rownames(Site2_exp_deepB) <- Site2_exp_deep$Species
names(Site2_exp_deepB)[1]<-paste(c("Height"))

Site2_exp_shallow <- subset(Sites_exp, Site==2 & Depth=="S")
Site2_exp_shallowB <- as.data.frame(Site2_exp_shallow[,c(4)])
rownames(Site2_exp_shallowB) <- Site2_exp_shallow$Species
names(Site2_exp_shallowB)[1]<-paste(c("Height"))

Site3_exp_deep <- subset(Sites_exp, Site==3 & Depth=="D")
Site3_exp_deepB <- as.data.frame(Site3_exp_deep[,c(4)])
rownames(Site3_exp_deepB) <- Site3_exp_deep$Species
names(Site3_exp_deepB)[1]<-paste(c("Height"))

Site3_exp_shallow <- subset(Sites_exp, Site==3 & Depth=="S")
Site3_exp_shallowB <- as.data.frame(Site3_exp_shallow[,c(4)])
rownames(Site3_exp_shallowB) <- Site3_exp_shallow$Species
names(Site3_exp_shallowB)[1]<-paste(c("Height"))

Site4_exp_deep <- subset(Sites_exp, Site==4 & Depth=="D")
Site4_exp_deepB <- as.data.frame(Site4_exp_deep[,c(4)])
rownames(Site4_exp_deepB) <- Site4_exp_deep$Species
names(Site4_exp_deepB)[1]<-paste(c("Height"))

Site4_exp_shallow <- subset(Sites_exp, Site==4 & Depth=="S")
Site4_exp_shallowB <- as.data.frame(Site4_exp_shallow[,c(4)])
rownames(Site4_exp_shallowB) <- Site4_exp_shallow$Species
names(Site4_exp_shallowB)[1]<-paste(c("Height"))

Site5_exp_shallow <- subset(Sites_exp, Site==5 & Depth=="S")
Site5_exp_shallowB <- as.data.frame(Site5_exp_shallow[,c(4)])
rownames(Site5_exp_shallowB) <- Site5_exp_shallow$Species
names(Site5_exp_shallowB)[1]<-paste(c("Height"))

########################################################
### Community weighted mean value for height trait######
########################################################

Indices_exp1_deep <- FD::dbFD(Site1_exp_deepB,Plot_Abun_exp1_deep, w.abun = TRUE)
Indices_exp1_shallow <- FD::dbFD(Site1_exp_shallowB,Plot_Abun_exp1_shallow, w.abun = TRUE)


Indices_exp2_shallow <- FD::dbFD(Site2_exp_shallowB,Plot_Abun_exp2_shallow, w.abun = TRUE)
Indices_exp2_deep <- Indices_exp1_shallow
Indices_exp2_deep$sing.sp <-  c(rep("1",8))
Indices_exp2_deep$FDis <- c(rep("0",8))
Indices_exp2_deep$CWM <- c(rep("55.5",8))


Indices_exp3_deep <- FD::dbFD(Site3_exp_deepB,Plot_Abun_exp3_deep, w.abun = TRUE)
Indices_exp3_shallow <- FD::dbFD(Site3_exp_shallowB,Plot_Abun_exp3_shallow, w.abun = TRUE)

Indices_exp4_deep <- FD::dbFD(Site4_exp_deepB,Plot_Abun_exp4_deep, w.abun = TRUE)
Indices_exp4_shallow <- FD::dbFD(Site4_exp_shallowB,Plot_Abun_exp4_shallow, w.abun = TRUE)

Indices_exp5_shallow <- FD::dbFD(Site5_exp_shallowB,Plot_Abun_exp5_shallow, w.abun = TRUE)

########################################################
###         Combibine exposed site indices        ######
########################################################

###### SpeciesRichness
SpRichnessExp1S <-as.data.frame(Indices_exp1_shallow$sing.sp)
colnames(SpRichnessExp1S) <- c("SpeciesRichness")
SpRichnessExp1D <-as.data.frame(Indices_exp1_deep$sing.sp)
colnames(SpRichnessExp1D) <- c("SpeciesRichness")

SpRichnessExp2S <-as.data.frame(Indices_exp2_shallow$sing.sp)
colnames(SpRichnessExp2S) <- c("SpeciesRichness")
SpRichnessExp2D <-as.data.frame(Indices_exp2_deep$sing.sp )
SpRichnessExp2D <- as.data.frame(SpRichnessExp2D[c(1:6),]) 
colnames(SpRichnessExp2D) <- c("SpeciesRichness")
rownames(SpRichnessExp2D) <- c("D2.1","D2.2","D2.3","D2.4","D2.5","D2.6")

SpRichnessExp3S <-as.data.frame(Indices_exp3_shallow$sing.sp)
colnames(SpRichnessExp3S) <- c("SpeciesRichness")
SpRichnessExp3D <-as.data.frame(Indices_exp3_deep$sing.sp)
colnames(SpRichnessExp3D) <- c("SpeciesRichness")

SpRichnessExp4S <-as.data.frame(Indices_exp4_shallow$sing.sp)
colnames(SpRichnessExp4S) <- c("SpeciesRichness")
SpRichnessExp4D <-as.data.frame(Indices_exp4_deep$sing.sp)
colnames(SpRichnessExp4D) <- c("SpeciesRichness")

SpRichnessExp5S <-as.data.frame(Indices_exp5_shallow$sing.sp)
colnames(SpRichnessExp5S) <- c("SpeciesRichness")

Indices_height_Expsites <-rbind(SpRichnessExp1S,SpRichnessExp2S,SpRichnessExp3S,SpRichnessExp4S,SpRichnessExp5S,
                                SpRichnessExp1D,SpRichnessExp2D, SpRichnessExp3D, SpRichnessExp4D)
Indices_height_Expsites$Depth <- c(rep("Shallow",31),rep("Deep",26))
Indices_height_Expsites$Exposure <- c(rep("Exposed",57))

###### Community weighted mean
CWMExp1S <-as.data.frame(Indices_exp1_shallow$CWM)
CWMExp1D <-as.data.frame(Indices_exp1_deep$CWM)

CWMExp2S <-as.data.frame(Indices_exp2_shallow$CWM)
CWMExp2D <-as.data.frame(Indices_exp2_deep$CWM)
CWMExp2D <- as.data.frame(CWMExp2D[c(1:6),]) 
colnames(CWMExp2D) <- c("Height")

CWMExp3S <-as.data.frame(Indices_exp3_shallow$CWM)
CWMExp3D <-as.data.frame(Indices_exp3_deep$CWM)

CWMExp4S <-as.data.frame(Indices_exp4_shallow$CWM)
CWMExp4D <-as.data.frame(Indices_exp4_deep$CWM)

CWMExp5S <-as.data.frame(Indices_exp5_shallow$CWM)

Indices_height_Expsites$CWM_Height <-rbind.data.frame(CWMExp1S,CWMExp2S,CWMExp3S,CWMExp4S,CWMExp5S,
                   CWMExp1D,CWMExp2D,CWMExp3D,CWMExp4D)

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

Indices_height_Expsites$FDis_Height <-rbind.data.frame(FDisExp1S,FDisExp2S,FDisExp3S,FDisExp4S,FDisExp5S,
                                                       FDisExp1D,FDisExp2D,FDisExp3D,FDisExp4D)
Indices_height_Expsites <- tibble::rownames_to_column(Indices_height_Expsites,"Site")
rownames(Indices_height_Expsites)<-c(1:57)

##########################################
###########          #####################
###########   SEMI  ######################
###########         ######################
##########################################

Spatial_Campaign_data_semi <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                        sheet = "Semi", na = "NA")
TraitData_semi <- Spatial_Campaign_data_semi[,c(1:5)]
names(TraitData_semi)[5]<-paste(c("Height"))
names(TraitData_semi)[2]<-paste(c("Site_number"))

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

Sites_semi <-aggregate(x=TraitData_semi$Height, by=list(TraitData_semi$Site_number,TraitData_semi$Depth, TraitData_semi$Species), FUN=max)
names(Sites_semi)[1:4]<-paste(c("Site","Depth","Species", "Height"))

Site1_semi_deep<- subset(Sites_semi, Site==1 & Depth=="D")
Site1_semi_deepB <- as.data.frame(Site1_semi_deep[,c(4)])
rownames(Site1_semi_deepB) <- Site1_semi_deep$Species
names(Site1_semi_deepB)[1]<-paste(c("Height"))

Site1_semi_shallow <- subset(Sites_semi, Site==1 & Depth=="S")
Site1_semi_shallowB <- as.data.frame(Site1_semi_shallow[,c(4)])
rownames(Site1_semi_shallowB) <- Site1_semi_shallow$Species
names(Site1_semi_shallowB)[1]<-paste(c("Height"))

Site2_semi_deep <- subset(Sites_semi, Site==2 & Depth=="D")
Site2_semi_deepB <- as.data.frame(Site2_semi_deep[,c(4)])
rownames(Site2_semi_deepB) <- Site2_semi_deep$Species
names(Site2_semi_deepB)[1]<-paste(c("Height"))

Site2_semi_shallow <- subset(Sites_semi, Site==2 & Depth=="S")
Site2_semi_shallowB <- as.data.frame(Site2_semi_shallow[,c(4)])
rownames(Site2_semi_shallowB) <- Site2_semi_shallow$Species
names(Site2_semi_shallowB)[1]<-paste(c("Height"))

Site3_semi_deep <- subset(Sites_semi, Site==3 & Depth=="D")
Site3_semi_deepB <- as.data.frame(Site3_semi_deep[,c(4)])
rownames(Site3_semi_deepB) <- Site3_semi_deep$Species
names(Site3_semi_deepB)[1]<-paste(c("Height"))

Site3_semi_shallow <- subset(Sites_semi, Site==3 & Depth=="S")
Site3_semi_shallowB <- as.data.frame(Site3_semi_shallow[,c(4)])
rownames(Site3_semi_shallowB) <- Site3_semi_shallow$Species
names(Site3_semi_shallowB)[1]<-paste(c("Height"))

Site4_semi_deep <- subset(Sites_semi, Site==4 & Depth=="D")
Site4_semi_deepB <- as.data.frame(Site4_semi_deep[,c(4)])
rownames(Site4_semi_deepB) <- Site4_semi_deep$Species
names(Site4_semi_deepB)[1]<-paste(c("Height"))

Site4_semi_shallow <- subset(Sites_semi, Site==4 & Depth=="S")
Site4_semi_shallowB <- as.data.frame(Site4_semi_shallow[,c(4)])
rownames(Site4_semi_shallowB) <- Site4_semi_shallow$Species
names(Site4_semi_shallowB)[1]<-paste(c("Height"))

Site5_semi_deep <- subset(Sites_semi, Site==5 & Depth=="D")
Site5_semi_deepB <- as.data.frame(Site5_semi_deep[,c(4)])
rownames(Site5_semi_deepB) <- Site5_semi_deep$Species
names(Site5_semi_deepB)[1]<-paste(c("Height"))

Site5_semi_shallow <- subset(Sites_semi, Site==5 & Depth=="S")
Site5_semi_shallowB <- as.data.frame(Site5_semi_shallow[,c(4)])
rownames(Site5_semi_shallowB) <- Site5_semi_shallow$Species
names(Site5_semi_shallowB)[1]<-paste(c("Height"))

########################################################
### Community weighted mean value for height trait######
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
###         Combine exposed site indices        ######
########################################################

###### SpeciesRichness
SpRichnessSemi1S <-as.data.frame(Indices_semi1_shallow$sing.sp)
colnames(SpRichnessSemi1S) <- c("SpeciesRichness")
SpRichnessSemi1D <-as.data.frame(Indices_semi1_deep$sing.sp)
colnames(SpRichnessSemi1D) <- c("SpeciesRichness")

SpRichnessSemi2S <-as.data.frame(Indices_semi2_shallow$sing.sp)
colnames(SpRichnessSemi2S) <- c("SpeciesRichness")
SpRichnessSemi2D <-as.data.frame(Indices_semi2_deep$sing.sp)
colnames(SpRichnessSemi2D) <- c("SpeciesRichness")

SpRichnessSemi3S <-as.data.frame(Indices_semi3_shallow$sing.sp)
colnames(SpRichnessSemi3S) <- c("SpeciesRichness")
SpRichnessSemi3D <-as.data.frame(Indices_semi3_deep$sing.sp)
colnames(SpRichnessSemi3D) <- c("SpeciesRichness")

SpRichnessSemi4S <-as.data.frame(Indices_semi4_shallow$sing.sp)
colnames(SpRichnessSemi4S) <- c("SpeciesRichness")
SpRichnessSemi4D <-as.data.frame(Indices_semi4_deep$sing.sp)
colnames(SpRichnessSemi4D) <- c("SpeciesRichness")

SpRichnessSemi5S <-as.data.frame(Indices_semi5_shallow$sing.sp)
colnames(SpRichnessSemi5S) <- c("SpeciesRichness")
SpRichnessSemi5D <-as.data.frame(Indices_semi5_deep$sing.sp)
colnames(SpRichnessSemi5D) <- c("SpeciesRichness")

Indices_height_Semisites <-rbind(SpRichnessSemi1S,SpRichnessSemi2S,SpRichnessSemi3S,SpRichnessSemi4S,SpRichnessSemi5S,
                                SpRichnessSemi1D,SpRichnessSemi2D, SpRichnessSemi3D, SpRichnessSemi4D,SpRichnessSemi5D)
Indices_height_Semisites$Depth <- c(rep("Shallow",30),rep("Deep",30))
Indices_height_Semisites$Exposure <- c(rep("Semi",60))

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

Indices_height_Semisites$CWM_Height <-rbind.data.frame(CWMSemi1S,CWMSemi2S,CWMSemi3S,CWMSemi4S,CWMSemi5S,
                                                      CWMSemi1D,CWMSemi2D,CWMSemi3D,CWMSemi4D,CWMSemi4D)

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

Indices_height_Semisites$FDis_Height <-rbind.data.frame(FDisSemi1S,FDisSemi2S,FDisSemi3S,FDisSemi4S,FDisSemi5S,
                                                       FDisSemi1D,FDisSemi2D,FDisSemi3D,FDisSemi4D, FDisSemi5D)
Indices_height_Semisites <- tibble::rownames_to_column(Indices_height_Semisites,"Site")
rownames(Indices_height_Semisites)<-c(1:60)

##############################################
###########             ######################
###########   SHELTERED ######################
###########             ######################
##############################################

Spatial_Campaign_data_shel <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Sheltered", na = "NA")
TraitData_shel <- Spatial_Campaign_data_shel[,c(1:5)]
names(TraitData_shel)[5]<-paste(c("Height"))
names(TraitData_shel)[2]<-paste(c("Site_number"))
TraitData_shel[is.na(TraitData_shel)] = 0

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
#########    Trait matrix      ########
##%#########################################

Sites_shel <-aggregate(x=TraitData_shel$Height, by=list(TraitData_shel$Species), FUN=max)
names(Sites_shel)[1:2]<-paste(c("Species", "Height"))
Sites_shelB <- as.data.frame(Sites_shel$Height)
rownames(Sites_shelB) <- Sites_shel$Species
names(Sites_shelB)[1]<-paste(c("Height"))

########################################################
### Community weighted mean value for height trait######
########################################################

Indices_shel_All <- FD::dbFD(Sites_shelB,Plot_Abun_shel, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### SpeciesRichness
SpRichnessShel <-as.data.frame(Indices_shel_All$sing.sp)
colnames(SpRichnessShel) <- c("SpeciesRichness")

Indices_height_Shelsites <- as.data.frame(SpRichnessShel)
Indices_height_Shelsites$Depth <- c(rep("Shallow",30))
Indices_height_Shelsites$Exposure <- c(rep("Sheltered",30))

###### Community weighted mean
CWMShel <-as.data.frame(Indices_shel_All$CWM)
Indices_height_Shelsites$CWM_Height <- CWMShel$Height

###### Functional Dispersion

FDisShel <-as.data.frame(Indices_shel_All$FDis)
colnames(FDisShel) <- c("FDis")
Indices_height_Shelsites$FDis_Height <- FDisShel$FDis
Indices_height_Shelsites <- tibble::rownames_to_column(Indices_height_Shelsites,"Site")

##############################################
###########             ######################
###########    Pojo     ######################
###########             ######################
##############################################

Spatial_Campaign_data_pojo <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Pojo", na = "NA")
TraitData_pojo <- Spatial_Campaign_data_pojo[,c(1:5)]
names(TraitData_pojo)[5]<-paste(c("Height"))
names(TraitData_pojo)[2]<-paste(c("Site_number"))
TraitData_pojo <- TraitData_pojo[-c(1,4,6,7),]
TraitData_pojo[is.na(TraitData_pojo)] = 0

AbundanceData_pojo <- Spatial_Campaign_data_pojo[,c(1:4, 36)]
names(AbundanceData_pojo)[5]<-paste(c("TotalDryMass"))
names(AbundanceData_pojo)[2]<-paste(c("Site_number"))
AbundanceData_pojo <- AbundanceData_pojo[-c(1,4,6,7),]

############################################################
####          Create species abundance matrix           ####
##%######################################################%##

### abundance based on Total biomass
Plot_Abun_pojo <- with(AbundanceData_pojo, tapply(TotalDryMass, list(Site, Species), sum))
Plot_Abun_pojo[is.na(Plot_Abun_pojo)] = 0

##%##########################################
#########    Trait matrix      ########
##%#########################################

Sites_pojo <-aggregate(x=TraitData_pojo$Height, by=list(TraitData_pojo$Species), FUN=max)
names(Sites_pojo)[1:2]<-paste(c("Species", "Height"))
Sites_pojoB <- as.data.frame(Sites_pojo$Height)
rownames(Sites_pojoB) <- Sites_pojo$Species
names(Sites_pojoB)[1]<-paste(c("Height"))

########################################################
### Community weighted mean value for height trait######
########################################################

Indices_pojo_All <- FD::dbFD(Sites_pojoB,Plot_Abun_pojo, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### SpeciesRichness
SpRichnessPojo <-as.data.frame(Indices_pojo_All$sing.sp)
colnames(SpRichnessPojo) <- c("SpeciesRichness")

Indices_height_Pojosites <- as.data.frame(SpRichnessPojo)
Indices_height_Pojosites$Depth <- c(rep("Deep",8),rep("Shallow",30))
Indices_height_Pojosites$Exposure <- c(rep("Pojo",38))

###### Community weighted mean
CWMPojo <-as.data.frame(Indices_pojo_All$CWM)
Indices_height_Pojosites$CWM_Height <- CWMPojo$Height

###### Functional Dispersion

FDisPojo <-as.data.frame(Indices_pojo_All$FDis)
colnames(FDisPojo) <- c("FDis")
Indices_height_Pojosites$FDis_Height <- FDisPojo$FDis
Indices_height_Pojosites <- tibble::rownames_to_column(Indices_height_Pojosites,"Site")

##################################################
###########                 ######################
###########    Combined     ######################
###########                 ######################
##################################################
Indices_height_Expsites <- as.matrix.data.frame(Indices_height_Expsites)
Indices_height_Pojosites <- as.matrix.data.frame(Indices_height_Pojosites)
Indices_height_Semisites <- as.matrix.data.frame(Indices_height_Semisites)
Indices_height_Shelsites <- as.matrix.data.frame(Indices_height_Shelsites)
Indices_height_Expsites <- as.data.frame(Indices_height_Expsites)
Indices_height_Pojosites <- as.data.frame(Indices_height_Pojosites)
Indices_height_Semisites <- as.data.frame(Indices_height_Semisites)
Indices_height_Shelsites <- as.data.frame(Indices_height_Shelsites)

Indices_height_Allsites<- rbind.data.frame(Indices_height_Expsites,Indices_height_Semisites,Indices_height_Pojosites,Indices_height_Shelsites)
Indices_height_Allsites$Site_number<- c(rep("1S",8),rep("2S",5),rep("3S",6),rep("4S",6),rep("5S",6),rep("1D",8),rep("2D",6),rep("3D",6),rep("4D",6),
                                         rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6),rep("1D",6),rep("2D",6),rep("3D",6),rep("4D",6),rep("5D",6),
                                         rep("2D",2),rep("3D",6),rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6),
                                         rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6))

Indices_height_Allsites$Exposure_Depth <- factor(paste(Indices_height_Allsites$Exposure, 
                                                        Indices_height_Allsites$Depth, sep = "_"),
                                                  levels = c(
                                                    "Exposed_Shallow", "Exposed_Deep",
                                                    "Semi_Shallow", "Semi_Deep",
                                                    "Pojo_Shallow", "Pojo_Deep",
                                                    "Sheltered_Shallow"
                                                  ))


Indices_height_Allsites$Site_Exposure <- factor(paste(Indices_height_Allsites$Site_number, 
                                                       Indices_height_Allsites$Exposure, sep = "_"),
                                                 levels = c(
                                                   "1S_Exposed", "2S_Exposed", "3S_Exposed", "4S_Exposed", "5S_Exposed", "1D_Exposed", "2D_Exposed", "3D_Exposed", "4D_Exposed",
                                                   "1S_Semi", "2S_Semi", "3S_Semi", "4S_Semi", "5S_Semi", "1D_Semi", "2D_Semi", "3D_Semi", "4D_Semi", "5D_Semi",
                                                   "1S_Pojo", "2S_Pojo", "3S_Pojo", "4S_Pojo", "5S_Pojo", "2D_Pojo", "3D_Pojo",
                                                   "1S_Sheltered", "2S_Sheltered", "3S_Sheltered", "4S_Sheltered", "5S_Sheltered"
                                                 ))

