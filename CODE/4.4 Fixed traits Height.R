##%######################################################%##
#                                                          #
####     Fixed community indices for Root depth         ####
#                                                          #
##%######################################################%##

##########################################
###########          #####################
########### EXPOSED ######################
###########         ######################
##########################################
Spatial_Campaign_data_exp <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                        sheet = "Exposed", na = "NA")

TraitData_expA <- Spatial_Campaign_data_exp[-c(43),c(1:5)]
names(TraitData_expA)[5]<-paste(c("Height"))


##########################################
###########          #####################
###########   SEMI  ######################
###########         ######################
##########################################
Spatial_Campaign_data_semi <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Semi", na = "NA")

TraitData_semiA <- Spatial_Campaign_data_semi[,c(1:5)]
names(TraitData_semiA)[5]<-paste(c("Height")) 

##############################################
###########             ######################
###########   SHELTERED ######################
###########             ######################
##############################################
Spatial_Campaign_data_shel <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Sheltered", na = "NA")

TraitData_shelA <- Spatial_Campaign_data_shel[,c(1:5)]
names(TraitData_shelA)[5]<-paste(c("Height"))

##############################################
###########             ######################
###########    Pojo     ######################
###########             ######################
##############################################
Spatial_Campaign_data_pojo <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Pojo", na = "NA")

TraitData_pojoA <- Spatial_Campaign_data_pojo[-c(1,4,6,7),c(1:5)]
names(TraitData_pojoA)[5]<-paste(c("Height"))


#################################################
###########                ######################
###########    All triats  ######################
###########                ######################
#################################################

All_Height <- rbind (TraitData_expA, TraitData_pojoA, TraitData_semiA, TraitData_shelA)
Fixed_Height <- aggregate(x=All_Height$Height, by=list(All_Height$Species), FUN=max)

##%######################################################%##
#                                                          #
####                 Community indices                  ####
#                                                          #
##%######################################################%##

##########################################
###########          #####################
########### EXPOSED ######################
###########         ######################
##########################################

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
Plot_Abun_exp_deep <- Plot_Abun_exp[c(1:26),-c(3)]
Plot_Abun_exp_shallow <- Plot_Abun_exp[c(27:57),]


##%##########################################
#########    Trait matrix      ########
##%#########################################

Sites_exp <-aggregate(x=TraitData_exp$Height, by=list(TraitData_exp$Depth, TraitData_exp$Species), FUN=max)
names(Sites_exp)[1:3]<-paste(c("Depth","Species", "Height"))
### change for fixed heights
Sites_exp$Height <- as.numeric(c("200.4","200.4", "194.1","194.1", "44.2", "87.5", "87.5", "77.7", "77.7","55.5","55.5"))

Site_exp_Height_D <- subset(Sites_exp, Depth=="D")
Site_exp_Height_S <- subset(Sites_exp, Depth=="S")

Site_exp_Height_DB <- as.data.frame(Site_exp_Height_D[,c(3)])
rownames(Site_exp_Height_DB) <- Site_exp_Height_D$Species
names(Site_exp_Height_DB)[1]<-paste(c("Height"))

Site_exp_Height_SB <- as.data.frame(Site_exp_Height_S[,c(3)])
rownames(Site_exp_Height_SB) <- Site_exp_Height_S$Species
names(Site_exp_Height_SB)[1]<-paste(c("Height"))

########################################################
### Community weighted mean value for height trait######
########################################################

Indices_deep <- FD::dbFD(Site_exp_Height_DB,Plot_Abun_exp_deep, w.abun = TRUE)
Indices_shallow <- FD::dbFD(Site_exp_Height_SB,Plot_Abun_exp_shallow, w.abun = TRUE)


########################################################
###         Combibine exposed site indices        ######
########################################################

###### SpeciesRichness
SpRichnessExpS <-as.data.frame(Indices_shallow$sing.sp)
colnames(SpRichnessExpS) <- c("SpeciesRichness")
SpRichnessExpD <-as.data.frame(Indices_deep$sing.sp)
colnames(SpRichnessExpD) <- c("SpeciesRichness")


Indices_height_Expsites <-rbind(SpRichnessExpS,SpRichnessExpD)
Indices_height_Expsites$Depth <- c(rep("Shallow",31),rep("Deep",26))
Indices_height_Expsites$Exposure <- c(rep("Exposed",57))

###### Community weighted mean
CWMExpS <-as.data.frame(Indices_shallow$CWM)
CWMExpD <-as.data.frame(Indices_deep$CWM)

Indices_height_Expsites$CWM_Height <-rbind.data.frame(CWMExpS,CWMExpD)

###### Functional Dispersion
FDisExpS <-as.data.frame(Indices_shallow$FDis)
colnames(FDisExpS) <- c("FDis")

FDisExpD <-as.data.frame(Indices_deep$FDis)
colnames(FDisExpD) <- c("FDis")

Indices_height_Expsites$FDis_Height <-rbind.data.frame(FDisExpS,FDisExpD)
Indices_height_Expsites <- tibble::rownames_to_column(Indices_height_Expsites,"Site")
rownames(Indices_height_Expsites)<-c(1:57)

##########################################
###########          #####################
###########   SEMI  ######################
###########         ######################
##########################################

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
Plot_Abun_semi_deep <- Plot_Abun_semi[c(1:30),-c(4,5)]
Plot_Abun_semi_shallow <- Plot_Abun_semi[c(31:60),-c(9)]

##%##########################################
#########    Trait matrix      ########
##%#########################################

Sites_semi <-aggregate(x=TraitData_semi$Height, by=list(TraitData_semi$Depth, TraitData_semi$Species), FUN=max)
names(Sites_semi)[1:3]<-paste(c("Depth","Species", "Height"))
### change for fixed heights
Sites_semi$Height <- as.numeric(c("66.5","66.5", "200.4", "200.4", "194.1","194.1", "171.2", "32.6", 
                                 "44.2", "44.2","87.5","87.5", "77.7","77.7", "55.5" ))

Site_semi_Height_D <- subset(Sites_semi, Depth=="D")
Site_semi_Height_S <- subset(Sites_semi, Depth=="S")

Site_semi_Height_DB <- as.data.frame(Site_semi_Height_D[,c(3)])
rownames(Site_semi_Height_DB) <- Site_semi_Height_D$Species
names(Site_semi_Height_DB)[1]<-paste(c("Height"))

Site_semi_Height_SB <- as.data.frame(Site_semi_Height_S[,c(3)])
rownames(Site_semi_Height_SB) <- Site_semi_Height_S$Species
names(Site_semi_Height_SB)[1]<-paste(c("Height"))
########################################################
### Community weighted mean value for height trait######
########################################################

Indices_semi_deep <- FD::dbFD(Site_semi_Height_DB,Plot_Abun_semi_deep, w.abun = TRUE)
Indices_semi_shallow <- FD::dbFD(Site_semi_Height_SB,Plot_Abun_semi_shallow, w.abun = TRUE)

########################################################
###         Combibine exposed site indices        ######
########################################################

###### SpeciesRichness
SpRichnessSemiS <-as.data.frame(Indices_semi_shallow$sing.sp)
colnames(SpRichnessSemiS) <- c("SpeciesRichness")
SpRichnessSemiD <-as.data.frame(Indices_semi_deep$sing.sp)
colnames(SpRichnessSemiD) <- c("SpeciesRichness")

Indices_height_Semisites <-rbind(SpRichnessSemiS,SpRichnessSemiD)
Indices_height_Semisites$Depth <- c(rep("Shallow",30),rep("Deep",30))
Indices_height_Semisites$Exposure <- c(rep("Semi",60))

###### Community weighted mean
CWMSemiS <-as.data.frame(Indices_semi_shallow$CWM)
CWMSemiD <-as.data.frame(Indices_semi_deep$CWM)

Indices_height_Semisites$CWM_Height <-rbind.data.frame(CWMSemiS,CWMSemiD)

###### Functional Dispersion

FDisSemiS <-as.data.frame(Indices_semi_shallow$FDis)
colnames(FDisSemiS) <- c("FDis")
FDisSemiD <-as.data.frame(Indices_semi_deep$FDis)
colnames(FDisSemiD) <- c("FDis")

Indices_height_Semisites$FDis_Height <-rbind.data.frame(FDisSemiS,FDisSemiD)
Indices_height_Semisites <- tibble::rownames_to_column(Indices_height_Semisites,"Site")
rownames(Indices_height_Semisites)<-c(1:60)

##############################################
###########             ######################
###########   SHELTERED ######################
###########             ######################
##############################################

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
Sites_shelB$Height <- as.numeric(c("18.4","66.5", "9.3", "190.1", "200.4","44.3", "194.1", "32.6", "87.5"))


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
Sites_pojoB$Height <- as.numeric(c("18.4","66.5", "15.4", "9.3", "190.1", "200.4","44.3", "34.2", "31.0", "10.7",
                                   "32.6", "50.1"))

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

Indices_height_Fixed<- rbind.data.frame(Indices_height_Expsites,Indices_height_Semisites,Indices_height_Pojosites,Indices_height_Shelsites)
Indices_height_Fixed$Site_number<- c(rep("1S",8),rep("2S",5),rep("3S",6),rep("4S",6),rep("5S",6),rep("1D",8),rep("2D",6),rep("3D",6),rep("4D",6),
                                         rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6),rep("1D",6),rep("2D",6),rep("3D",6),rep("4D",6),rep("5D",6),
                                         rep("2D",2),rep("3D",6),rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6),
                                         rep("1S",6),rep("2S",6),rep("3S",6),rep("4S",6),rep("5S",6))

Indices_height_Fixed$Exposure_Depth <- factor(paste(Indices_height_Fixed$Exposure, 
                                                    Indices_height_Fixed$Depth, sep = "_"),
                                                  levels = c(
                                                    "Exposed_Shallow", "Exposed_Deep",
                                                    "Semi_Shallow", "Semi_Deep",
                                                    "Pojo_Shallow", "Pojo_Deep",
                                                    "Sheltered_Shallow"
                                                  ))


Indices_height_Fixed$Site_Exposure <- factor(paste(Indices_height_Fixed$Site_number, 
                                                   Indices_height_Fixed$Exposure, sep = "_"),
                                                 levels = c(
                                                   "1S_Exposed", "2S_Exposed", "3S_Exposed", "4S_Exposed", "5S_Exposed", "1D_Exposed", "2D_Exposed", "3D_Exposed", "4D_Exposed",
                                                   "1S_Semi", "2S_Semi", "3S_Semi", "4S_Semi", "5S_Semi", "1D_Semi", "2D_Semi", "3D_Semi", "4D_Semi", "5D_Semi",
                                                   "1S_Pojo", "2S_Pojo", "3S_Pojo", "4S_Pojo", "5S_Pojo", "2D_Pojo", "3D_Pojo",
                                                   "1S_Sheltered", "2S_Sheltered", "3S_Sheltered", "4S_Sheltered", "5S_Sheltered"
                                                 ))

