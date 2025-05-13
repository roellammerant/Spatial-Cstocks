##########################################
###########          #####################
########### EXPOSED ######################
###########         ######################
##########################################

Spatial_Campaign_data_exp <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                        sheet = "Exposed", na = "NA")

TraitData_expA <- Spatial_Campaign_data_exp[-c(43),c(1:4, 35,37)]
names(TraitData_expA)[5]<-paste(c("SLA"))
names(TraitData_expA)[6]<-paste(c("LAP"))
TraitData_expB <- Spatial_Campaign_data_exp[-c(43),c(1:4, 36,38)]
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

##### Abundance per depth
Plot_Abun_exp_deep <- Plot_Abun_exp[c(1:26),-c(1,3)]
Plot_Abun_exp_shallow <- Plot_Abun_exp[c(27:57),]

##%##########################################
#########     Trait matrix          ########
##%#########################################

# Sites_exp <- aggregate(x=TraitData_exp$SLA, by=list(TraitData_exp$`Site number`,TraitData_exp$Depth, TraitData_exp$Species), FUN=median)
# names(Sites_exp)[1:4]<-paste(c("Site","Depth","Species", "Height"))
# Variation among sites is minimal for most species, aim plotting per depth

Sites_exp_SLA <- aggregate(x=TraitData_exp$SLA, by=list(TraitData_exp$Depth, TraitData_exp$Species), FUN=median)
Sites_exp_LAP <- aggregate(x=TraitData_exp$LAP, by=list(TraitData_exp$Depth, TraitData_exp$Species), FUN=median)
names(Sites_exp_SLA)[1:3]<-paste(c("Depth","Species", "SLA"))
names(Sites_exp_LAP)[1:3]<-paste(c("Depth","Species", "LAP"))

Site_exp_SLA_D <- subset(Sites_exp_SLA, Depth=="D")
Site_exp_SLA_S <- subset(Sites_exp_SLA, Depth=="S")

Site_exp_LAP_D <- subset(Sites_exp_LAP, Depth=="D")
Site_exp_LAP_S <- subset(Sites_exp_LAP, Depth=="S")

Site_exp_SLA_DB <- as.data.frame(Site_exp_SLA_D[,c(3)])
rownames(Site_exp_SLA_DB) <- Site_exp_SLA_D$Species
names(Site_exp_SLA_DB)[1]<-paste(c("SLA"))

Site_exp_SLA_SB <- as.data.frame(Site_exp_SLA_S[,c(3)])
rownames(Site_exp_SLA_SB) <- Site_exp_SLA_S$Species
names(Site_exp_SLA_SB)[1]<-paste(c("SLA"))

Site_exp_LAP_DB <- as.data.frame(Site_exp_LAP_D[,c(3)])
rownames(Site_exp_LAP_DB) <- Site_exp_LAP_D$Species
names(Site_exp_LAP_DB)[1]<-paste(c("LAP"))

Site_exp_LAP_SB <- as.data.frame(Site_exp_LAP_S[,c(3)])
rownames(Site_exp_LAP_SB) <- Site_exp_LAP_S$Species
names(Site_exp_LAP_SB)[1]<-paste(c("LAP"))


########################################################
### Community weighted mean value for height trait######
########################################################

Indices_exp_SLA_D <- FD::dbFD(Site_exp_SLA_DB,Plot_Abun_exp_deep, w.abun = TRUE)
Indices_exp_SLA_S <- FD::dbFD(Site_exp_SLA_SB,Plot_Abun_exp_shallow, w.abun = TRUE)
Indices_exp_LAP_D <- FD::dbFD(Site_exp_LAP_DB,Plot_Abun_exp_deep, w.abun = TRUE)
Indices_exp_LAP_S <- FD::dbFD(Site_exp_LAP_SB,Plot_Abun_exp_shallow, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### Community weighted mean
CWMexp_SLA_D <-as.data.frame(Indices_exp_SLA_D$CWM)
CWMexp_SLA_S <-as.data.frame(Indices_exp_SLA_S$CWM)
CWMexp_LAP_D <-as.data.frame(Indices_exp_LAP_D$CWM)
CWMexp_LAP_S <-as.data.frame(Indices_exp_LAP_S$CWM)

Indices_Leaf_expsites <- rbind(CWMexp_SLA_S,CWMexp_SLA_D)
colnames(Indices_Leaf_expsites) <- c("CWM_SLA")
Indices_Leaf_expsites$CWM_LAP <- rbind(CWMexp_LAP_S, CWMexp_LAP_D)
Indices_Leaf_expsites$Depth <- c(rep("Shallow",31),rep("Deep",26))
Indices_Leaf_expsites$Exposure <- c(rep("Exposed",57))

###### Functional Dispersion
FDisexp_SLA_D <-as.data.frame(Indices_exp_SLA_D$FDis)
colnames(FDisexp_SLA_D) <- c("FDis")
FDisexp_SLA_S <-as.data.frame(Indices_exp_SLA_S$FDis)
colnames(FDisexp_SLA_S) <- c("FDis")
FDisexp_LAP_D <-as.data.frame(Indices_exp_LAP_D$FDis)
colnames(FDisexp_LAP_D) <- c("FDis")
FDisexp_LAP_S <-as.data.frame(Indices_exp_LAP_S$FDis)
colnames(FDisexp_LAP_S) <- c("FDis")

Indices_Leaf_expsites$FDis_SLA <- rbind(FDisexp_SLA_S, FDisexp_SLA_D)
Indices_Leaf_expsites$FDis_LAP <- rbind(FDisexp_LAP_S, FDisexp_LAP_D)

Indices_Leaf_expsites <- tibble::rownames_to_column(Indices_Leaf_expsites,"Site")
rownames(Indices_Leaf_expsites)<-c(1:57)

##########################################
###########          #####################
########### SEMI    ######################
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

##### Abundance per depth
Plot_Abun_semi_deep <- Plot_Abun_semi[c(1:30),-c(4,5)]
Plot_Abun_semi_shallow <- Plot_Abun_semi[c(31:60),-c(9)]

##%##########################################
#########     Trait matrix          ########
##%#########################################

Sites_semi_SLA <- aggregate(x=TraitData_semi$SLA, by=list(TraitData_semi$Depth, TraitData_semi$Species), FUN=median)
Sites_semi_LAP <- aggregate(x=TraitData_semi$LAP, by=list(TraitData_semi$Depth, TraitData_semi$Species), FUN=median)
names(Sites_semi_SLA)[1:3]<-paste(c("Depth","Species", "SLA"))
names(Sites_semi_LAP)[1:3]<-paste(c("Depth","Species", "LAP"))

Site_semi_SLA_D <- subset(Sites_semi_SLA, Depth=="D")
Site_semi_SLA_S <- subset(Sites_semi_SLA, Depth=="S")

Site_semi_LAP_D <- subset(Sites_semi_LAP, Depth=="D")
Site_semi_LAP_S <- subset(Sites_semi_LAP, Depth=="S")

Site_semi_SLA_DB <- as.data.frame(Site_semi_SLA_D[,c(3)])
rownames(Site_semi_SLA_DB) <- Site_semi_SLA_D$Species
names(Site_semi_SLA_DB)[1]<-paste(c("SLA"))

Site_semi_SLA_SB <- as.data.frame(Site_semi_SLA_S[,c(3)])
rownames(Site_semi_SLA_SB) <- Site_semi_SLA_S$Species
names(Site_semi_SLA_SB)[1]<-paste(c("SLA"))

Site_semi_LAP_DB <- as.data.frame(Site_semi_LAP_D[,c(3)])
rownames(Site_semi_LAP_DB) <- Site_semi_LAP_D$Species
names(Site_semi_LAP_DB)[1]<-paste(c("LAP"))

Site_semi_LAP_SB <- as.data.frame(Site_semi_LAP_S[,c(3)])
rownames(Site_semi_LAP_SB) <- Site_semi_LAP_S$Species
names(Site_semi_LAP_SB)[1]<-paste(c("LAP"))


########################################################
### Community weighted mean value for height trait######
########################################################

Indices_semi_SLA_D <- FD::dbFD(Site_semi_SLA_DB,Plot_Abun_semi_deep, w.abun = TRUE)
Indices_semi_SLA_S <- FD::dbFD(Site_semi_SLA_SB,Plot_Abun_semi_shallow, w.abun = TRUE)
Indices_semi_LAP_D <- FD::dbFD(Site_semi_LAP_DB,Plot_Abun_semi_deep, w.abun = TRUE)
Indices_semi_LAP_S <- FD::dbFD(Site_semi_LAP_SB,Plot_Abun_semi_shallow, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
########################################################

###### Community weighted mean
CWMsemi_SLA_D <-as.data.frame(Indices_semi_SLA_D$CWM)
CWMsemi_SLA_S <-as.data.frame(Indices_semi_SLA_S$CWM)
CWMsemi_LAP_D <-as.data.frame(Indices_semi_LAP_D$CWM)
CWMsemi_LAP_S <-as.data.frame(Indices_semi_LAP_S$CWM)

Indices_Leaf_semisites <- rbind(CWMsemi_SLA_S,CWMsemi_SLA_D)
colnames(Indices_Leaf_semisites) <- c("CWM_SLA")
Indices_Leaf_semisites$CWM_LAP <- rbind(CWMsemi_LAP_S,CWMsemi_LAP_D)
Indices_Leaf_semisites$Depth <- c(rep("Shallow",30),rep("Deep",30))
Indices_Leaf_semisites$Exposure <- c(rep("semi",60))

###### Functional Dispersion
FDissemi_SLA_D <-as.data.frame(Indices_semi_SLA_D$FDis)
colnames(FDissemi_SLA_D) <- c("FDis")
FDissemi_SLA_S <-as.data.frame(Indices_semi_SLA_S$FDis)
colnames(FDissemi_SLA_S) <- c("FDis")
FDissemi_LAP_D <-as.data.frame(Indices_semi_LAP_D$FDis)
colnames(FDissemi_LAP_D) <- c("FDis")
FDissemi_LAP_S <-as.data.frame(Indices_semi_LAP_S$FDis)
colnames(FDissemi_LAP_S) <- c("FDis")

Indices_Leaf_semisites$FDis_SLA <- rbind(FDissemi_SLA_S,FDissemi_SLA_D)
Indices_Leaf_semisites$FDis_LAP <- rbind(FDissemi_LAP_S,FDissemi_LAP_D)

Indices_Leaf_semisites <- tibble::rownames_to_column(Indices_Leaf_semisites,"Site")
rownames(Indices_Leaf_semisites)<-c(1:60)

##############################################
###########              #####################
###########  Sheltered   #####################
###########              #####################
##############################################

Spatial_Campaign_data_shel <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Sheltered", na = "NA")

TraitData_shelA <- Spatial_Campaign_data_shel[-c(2),c(1:4, 39,41)]
names(TraitData_shelA)[5]<-paste(c("SLA"))
names(TraitData_shelA)[6]<-paste(c("LAP"))
TraitData_shelB <- Spatial_Campaign_data_shel[-c(2),c(1:4, 40,42)]
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
### Community weighted mean value for height trait######
########################################################

Indices_shel_SLA <- FD::dbFD(Sites_shel_SLAB,Plot_Abun_shel, w.abun = TRUE)
Indices_shel_LAP <- FD::dbFD(Sites_shel_LAPB,Plot_Abun_shel, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
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

TraitData_pojoA <- Spatial_Campaign_data_pojo[-c(1,4,6,7),c(1:4, 37,39)]
names(TraitData_pojoA)[5]<-paste(c("SLA"))
names(TraitData_pojoA)[6]<-paste(c("LAP"))
TraitData_pojoB <- Spatial_Campaign_data_pojo[-c(1,4,6,7),c(1:4, 38,40)]
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
### Community weighted mean value for height trait######
########################################################

Indices_pojo_SLA_D <- FD::dbFD(Site_pojo_SLA_DB,Plot_Abun_pojo_deep, w.abun = TRUE)
Indices_pojo_SLA_S <- FD::dbFD(Site_pojo_SLA_SB,Plot_Abun_pojo_shallow, w.abun = TRUE)
Indices_pojo_LAP_D <- FD::dbFD(Site_pojo_LAP_DB,Plot_Abun_pojo_deep, w.abun = TRUE)
Indices_pojo_LAP_S <- FD::dbFD(Site_pojo_LAP_SB,Plot_Abun_pojo_shallow, w.abun = TRUE)

########################################################
###         Combine exposed site indices        ######
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

Indices_Leaf_expsites <- as.matrix.data.frame(Indices_Leaf_expsites)
Indices_Leaf_pojosites <- as.matrix.data.frame(Indices_Leaf_pojosites)
Indices_Leaf_semisites <- as.matrix.data.frame(Indices_Leaf_semisites)
Indices_Leaf_shelsites <- as.matrix.data.frame(Indices_Leaf_shelsites)
Indices_Leaf_expsites <- as.data.frame(Indices_Leaf_expsites)
Indices_Leaf_pojosites <- as.data.frame(Indices_Leaf_pojosites)
Indices_Leaf_semisites <- as.data.frame(Indices_Leaf_semisites)
Indices_Leaf_shelsites <- as.data.frame(Indices_Leaf_shelsites)

Indices_leaf_Allsites<- rbind.data.frame(Indices_Leaf_expsites,Indices_Leaf_semisites,Indices_Leaf_pojosites,Indices_Leaf_shelsites)

Indices_leaf_Allsites$Site_number<- c(rep("1S",8),rep("2S",5),rep("3S",6),rep("4S",6),rep("5S",6),rep("1D",8),rep("2D",6),rep("3D",6),rep("4D",6),
                                      rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6),rep("1D",6),rep("2D",6),rep("3D",6),rep("4D",6),rep("5D",6),
                                      rep("2D",2),rep("3D",6),rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6),
                                      rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6))

Indices_leaf_Allsites$Exposure_Depth <- factor(paste(Indices_leaf_Allsites$Exposure, 
                                                     Indices_leaf_Allsites$Depth, sep = "_"),
                                               levels = c(
                                                 "Exposed_Shallow", "Exposed_Deep",
                                                 "semi_Shallow", "semi_Deep",
                                                 "Pojo_Shallow", "Pojo_Deep",
                                                 "Sheltered_Shallow"
                                               ))


Indices_leaf_Allsites$Site_Exposure <- factor(paste(Indices_leaf_Allsites$Site_number, 
                                                    Indices_leaf_Allsites$Exposure, sep = "_"),
                                              levels = c(
                                                "1S_Exposed", "2S_Exposed", "3S_Exposed", "4S_Exposed", "5S_Exposed", "1D_Exposed", "2D_Exposed", "3D_Exposed", "4D_Exposed",
                                                "1S_semi", "2S_semi", "3S_semi", "4S_semi", "5S_semi", "1D_semi", "2D_semi", "3D_semi", "4D_semi", "5D_semi",
                                                "1S_Pojo", "2S_Pojo", "3S_Pojo", "4S_Pojo", "5S_Pojo", "2D_Pojo", "3D_Pojo",
                                                "1S_Sheltered", "2S_Sheltered", "3S_Sheltered", "4S_Sheltered", "5S_Sheltered"
                                              ))