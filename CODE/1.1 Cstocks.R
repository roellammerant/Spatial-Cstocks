##%######################################################%##
#                                                          #
####                  Load R packages                   ####
#                                                          #
##%######################################################%##
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}
par(resetPar())

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c('readxl', 'data.table', 'vegan', 'reshape2', 'moments')
ipak(packages)

##%######################################################%##
#                                                          #
####             Exposed plant Cstocks                  ####
#                                                          #
##%######################################################%##

Spatial_Campaign_data_exp <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                        sheet = "Exposed", na = "NA")
Exposed_data <- Spatial_Campaign_data_exp[,c(1:4,29:33)]

#######################################                          
##Cstocks plants based on C/N Samples##
#######################################

C_N_Spatial <- read_excel("DATA/C-N.xlsx")
C_N_Exposed <- C_N_Spatial[ which(C_N_Spatial$Exposure=='Exposed'), ]
LEAF_Exp <- C_N_Exposed[ which(C_N_Exposed$Tissue=='Leaf'), ]
ROOT_Exp <- C_N_Exposed[ which(C_N_Exposed$Tissue=='Root'), ]
RHI_Exp <- C_N_Exposed[ which(C_N_Exposed$Tissue=='Rhi'), ]


### Mean carbon values of tissues
LEAF_C <- aggregate(x= LEAF_Exp$`C (µg/mg)`, by = list(LEAF_Exp$Species), FUN = median)
names(LEAF_C)[1:2]<-paste(c("Species", "LEAF_C"))
ROOT_C <- aggregate(x= ROOT_Exp$`C (µg/mg)`, by = list(ROOT_Exp$Species), FUN = median)
names(ROOT_C)[1:2]<-paste(c("Species", "ROOT_C"))
RHI_C <- aggregate(x= RHI_Exp$`C (µg/mg)`, by = list(RHI_Exp$Species), FUN = median)
names(RHI_C)[1:2]<-paste(c("Species", "RHI_C"))

### combine biomass and species carbon values
Exposed_C<- merge(Exposed_data,LEAF_C,by="Species",  all = TRUE)
Exposed_C<- merge(Exposed_C,ROOT_C,by="Species",  all = TRUE)
Exposed_C<- merge(Exposed_C,RHI_C,by="Species",  all = TRUE)

### get total carbon stock for each species in grams in a quadrat
Exposed_C$CarbonStockRoot <- (Exposed_C$`Total root dry mass (mg)`*(Exposed_C$ROOT_C/1000))/1000
Exposed_C$CarbonStockRhi <- (Exposed_C$`Total rhizome dry mass (mg)`*(Exposed_C$RHI_C/1000))/1000
Exposed_C$CarbonStockLeaf <- (Exposed_C$`Total aboveground dry mass (mg)`*(Exposed_C$LEAF_C/1000))/1000
Exposed_C[is.na(Exposed_C)] <- 0
Exposed_C$TotalC <- Exposed_C$CarbonStockLeaf + Exposed_C$CarbonStockRoot + Exposed_C$CarbonStockRhi

### Total  Cstock per quadrat
Plot_Abun_C <- with(Exposed_C, tapply(TotalC, list(Site, Species), sum))
Plot_Abun_C[is.na(Plot_Abun_C)] = 0
ExposedPlantCstock <- as.data.frame (rowSums(Plot_Abun_C))
setDT(ExposedPlantCstock, keep.rownames = "Plot")
names(ExposedPlantCstock)[2]<-paste("CarbonStock")
ExposedPlantCstock$Category <- c("Exposed")
ExposedPlantCstock <- ExposedPlantCstock[,-c(1)]
ExposedPlantCstock$site <- c("D_1","D_1","D_1","D_1","D_1","D_1","D_1","D_1",
                                 "D_2","D_2","D_2","D_2","D_2","D_2",
                                 "D_3","D_3","D_3","D_3","D_3","D_3",
                                 "D_4","D_4","D_4","D_4","D_4","D_4",
                                 "S_1","S_1","S_1","S_1","S_1","S_1","S_1","S_1",
                                 "S_2","S_2","S_2","S_2","S_2",
                                 "S_3","S_3","S_3","S_3","S_3","S_3",
                                 "S_4","S_4","S_4","S_4","S_4","S_4",
                                 "S_5","S_5","S_5","S_5","S_5","S_5")

### total Cstock per square meter
ExposedPlantCstock$CarbonStock <- ExposedPlantCstock$CarbonStock/(0.27*0.27)

ExposedPlantCstockB <- aggregate(x=ExposedPlantCstock$CarbonStock, by=list(ExposedPlantCstock$site), FUN=mean)
colnames(ExposedPlantCstockB)<-c("Site_number","Cstock_gm2")
Site_group = c("exposed", "exposed", "exposed","exposed","exposed", "exposed", "exposed","exposed", "exposed")
ExposedPlantCstockB <- cbind(ExposedPlantCstockB,Site_group)

Cstock_exp_5 <- data.frame(Site_number=c("D_5"),Cstock_gm2=c(0),Site_group=c("exposed"))
ExposedPlantCstockB <-rbind(ExposedPlantCstockB,Cstock_exp_5)
ExposedPlantCstockB$Depth <- c("Deep","Deep","Deep","Deep","Shallow",
                               "Shallow","Shallow","Shallow","Shallow","Deep")
ExposedPlantCstockB$Organism <- "Plant"


##%######################################################%##
#                                                          #
####             Exposed algae Cstocks                  ####
#                                                          #
##%######################################################%##
# get total carbon stock for each species in grams in a quadrat
Exposed_Algae <- Spatial_Campaign_data_exp[,c(1:3,25:26)]
Exposed_Algae[is.na(Exposed_Algae)] = 0

### Mean carbon values of tissues
Algae_Exp <- C_N_Exposed[ which(C_N_Exposed$Organism=='Algae'), ]
Algae_C <- aggregate(x= Algae_Exp$`C (µg/mg)`, by = list(Algae_Exp$Species), FUN = median)
names(Algae_C)[1:2]<-paste(c("Species", "Algae_C"))

Exposed_Algae$`Tolypella (mg)` <- (Exposed_Algae$`Tolypella (mg)` * 0.3036736)/1000
Exposed_Algae$`Chorda (mg)` <- (Exposed_Algae$`Chorda (mg)`  * 0.3348399)/1000
Exposed_Algae$TotalC <- Exposed_Algae$`Tolypella (mg)`+ Exposed_Algae$`Chorda (mg)`

### Total  Cstock per quadrat
Plot_Abun_C <- with(Exposed_Algae, tapply(TotalC, list(Site), sum))
Plot_Abun_C[is.na(Plot_Abun_C)] = 0
ExposedAlgaeCstock <- as.data.frame (Plot_Abun_C)
setDT(ExposedAlgaeCstock, keep.rownames = "Plot")
names(ExposedAlgaeCstock)[2]<-paste("CarbonStock")
ExposedAlgaeCstock$Category <- c("Exposed")
ExposedAlgaeCstock <- ExposedAlgaeCstock[,-c(1)]
ExposedAlgaeCstock$site <- c("D_1","D_1","D_1","D_1","D_1","D_1","D_1","D_1",
                             "D_2","D_2","D_2","D_2","D_2","D_2",
                             "D_3","D_3","D_3","D_3","D_3","D_3",
                             "D_4","D_4","D_4","D_4","D_4","D_4",
                             "S_1","S_1","S_1","S_1","S_1","S_1","S_1","S_1",
                             "S_2","S_2","S_2","S_2","S_2",
                             "S_3","S_3","S_3","S_3","S_3","S_3",
                             "S_4","S_4","S_4","S_4","S_4","S_4",
                             "S_5","S_5","S_5","S_5","S_5","S_5")

### total Cstock per square meter
ExposedAlgaeCstock$CarbonStock <- ExposedAlgaeCstock$CarbonStock/(0.27*0.27)

ExposedAlgaeCstockB <- aggregate(x=ExposedAlgaeCstock$CarbonStock, by=list(ExposedAlgaeCstock$site), FUN=mean)
colnames(ExposedAlgaeCstockB)<-c("Site_number","Cstock_gm2")
Site_group = c("exposed", "exposed", "exposed","exposed","exposed", "exposed", "exposed","exposed", "exposed")
ExposedAlgaeCstockB <- cbind(ExposedAlgaeCstockB,Site_group)

Cstock_exp_5 <- data.frame(Site_number=c("D_5"),Cstock_gm2=c(0),Site_group=c("exposed"))
ExposedAlgaeCstockB <-rbind(ExposedAlgaeCstockB,Cstock_exp_5)
ExposedAlgaeCstockB$Depth <- c("Deep","Deep","Deep","Deep","Shallow",
                               "Shallow","Shallow","Shallow","Shallow","Deep")
ExposedAlgaeCstockB$Organism <- "Algae"

##%######################################################%##
#                                                          #
####             Semi plant Cstocks                     ####
#                                                          #
##%######################################################%##

Spatial_Campaign_data_semi <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                        sheet = "Semi", na = "NA")
Semi_data <- Spatial_Campaign_data_semi[,c(1:4,30,32,33)]

#######################################                          
##Cstocks plants based on C/N Samples##
#######################################

C_N_Semi <- C_N_Spatial[ which(C_N_Spatial$Exposure=='Semi'), ]
LEAF_Semi <- C_N_Semi[ which(C_N_Semi$Tissue=='Leaf'), ]
ROOT_Semi <- C_N_Semi[ which(C_N_Semi$Tissue=='Root'), ]
RHI_Semi <- C_N_Semi[ which(C_N_Semi$Tissue=='Rhi'), ]


### Median carbon values of tissues
LEAF_C <- aggregate(x= LEAF_Semi$`C (µg/mg)`, by = list(LEAF_Semi$Species), FUN = median)
names(LEAF_C)[1:2]<-paste(c("Species", "LEAF_C"))
ROOT_C <- aggregate(x= ROOT_Semi$`C (µg/mg)`, by = list(ROOT_Semi$Species), FUN = median)
names(ROOT_C)[1:2]<-paste(c("Species", "ROOT_C"))
RHI_C <- aggregate(x= RHI_Semi$`C (µg/mg)`, by = list(RHI_Semi$Species), FUN = median)
names(RHI_C)[1:2]<-paste(c("Species", "RHI_C"))

### combine biomass and species carbon values
Semi_C<- merge(Semi_data,LEAF_C,by="Species",  all = TRUE)
Semi_C<- merge(Semi_C,ROOT_C,by="Species",  all = TRUE)
Semi_C<- merge(Semi_C,RHI_C,by="Species",  all = TRUE)
Semi_C[is.na(Semi_C)] = 0

### get total carbon stock for each species in grams in a quadrat
Semi_C$CarbonStockRoot <- (Semi_C$`Total root dry mass (mg)`*(Semi_C$ROOT_C/1000))/1000
Semi_C$CarbonStockRhi <- (Semi_C$`Total rhizome dry mass (mg)`*(Semi_C$RHI_C/1000))/1000
Semi_C$CarbonStockLeaf <- (Semi_C$`Total aboveground dry mass (mg)`*(Semi_C$LEAF_C/1000))/1000
Semi_C$TotalC <- Semi_C$CarbonStockLeaf + Semi_C$CarbonStockRoot + Semi_C$CarbonStockRhi

### Total  Cstock per quadrat
Plot_Abun_C <- with(Semi_C, tapply(TotalC, list(Site, Species), sum))
Plot_Abun_C[is.na(Plot_Abun_C)] = 0
SemiPlantCstock <- as.data.frame (rowSums(Plot_Abun_C))
setDT(SemiPlantCstock, keep.rownames = "Plot")
names(SemiPlantCstock)[2]<-paste("CarbonStock")
SemiPlantCstock$Category <- c("Semi")
SemiPlantCstock <- SemiPlantCstock[-c(1),-c(1)]
SemiPlantCstock$site <- c("D_1","D_1","D_1","D_1","D_1","D_1",
                             "D_2","D_2","D_2","D_2","D_2","D_2",
                             "D_3","D_3","D_3","D_3","D_3","D_3",
                             "D_4","D_4","D_4","D_4","D_4","D_4",
                             "D_5","D_5","D_5","D_5","D_5","D_5",
                             "S_1","S_1","S_1","S_1","S_1","S_1",
                             "S_2","S_2","S_2","S_2","S_2","S_2",
                             "S_3","S_3","S_3","S_3","S_3","S_3",
                             "S_4","S_4","S_4","S_4","S_4","S_4",
                             "S_5","S_5","S_5","S_5","S_5","S_5")

### total Cstock per square meter
SemiPlantCstock$CarbonStock <- SemiPlantCstock$CarbonStock/(0.27*0.27)

SemiPlantCstockB <- aggregate(x=SemiPlantCstock$CarbonStock, by=list(SemiPlantCstock$site), FUN=mean)
colnames(SemiPlantCstockB)<-c("Site_number","Cstock_gm2")
Site_group = c("Semi", "Semi", "Semi","Semi","Semi", "Semi", "Semi", "Semi","Semi","Semi")
SemiPlantCstockB <- cbind(SemiPlantCstockB,Site_group)
SemiPlantCstockB$Depth <- c("Deep","Deep","Deep","Deep","Deep",
                               "Shallow","Shallow","Shallow","Shallow","Shallow")
SemiPlantCstockB$Organism <- "Plant"


##%######################################################%##
#                                                          #
####             Semi algae Cstocks                     ####
#                                                          #
##%######################################################%##

# get total carbon stock for each species in grams in a quadrat
Semi_Algae <- Spatial_Campaign_data_semi[,c(1:3,25,27)]
Semi_Algae$`Tolypella (mg)` <- as.numeric(Semi_Algae$`Tolypella (mg)`)
Semi_Algae$`Chorda (mg)` <- as.numeric(Semi_Algae$`Chorda (mg)`)
Semi_Algae[is.na(Semi_Algae)] =  0

### Mean carbon values of tissues
Algae_Semi <- C_N_Semi[ which(C_N_Semi$Organism=='Algae'), ]
Algae_C <- aggregate(x= Algae_Semi$`C (µg/mg)`, by = list(Algae_Semi$Species), FUN = median)
names(Algae_C)[1:2]<-paste(c("Species", "Algae_C"))

Semi_Algae$`Tolypella (mg)` <- (Semi_Algae$`Tolypella (mg)` * 0.2870553)/1000
Semi_Algae$`Chorda (mg)` <- (Semi_Algae$`Chorda (mg)`  * 0.3113923)/1000
Semi_Algae$TotalC <- Semi_Algae$`Tolypella (mg)`+ Semi_Algae$`Chorda (mg)`

### Total  Cstock per quadrat
Plot_Abun_C <- with(Semi_Algae, tapply(TotalC, list(Site), sum))
Plot_Abun_C[is.na(Plot_Abun_C)] = 0
SemiAlgaeCstock <- as.data.frame (Plot_Abun_C)
setDT(SemiAlgaeCstock, keep.rownames = "Plot")
names(SemiAlgaeCstock)[2]<-paste("CarbonStock")
SemiAlgaeCstock$Category <- c("Semi")
SemiAlgaeCstock <- SemiAlgaeCstock[,-c(1)]
SemiAlgaeCstock$site <- c("D_1","D_1","D_1","D_1","D_1","D_1",
                             "D_2","D_2","D_2","D_2","D_2","D_2",
                             "D_3","D_3","D_3","D_3","D_3","D_3",
                             "D_4","D_4","D_4","D_4","D_4","D_4",
                             "D_5","D_5","D_5","D_5","D_5","D_5",
                             "S_1","S_1","S_1","S_1","S_1","S_1",
                             "S_2","S_2","S_2","S_2","S_2","S_2",
                             "S_3","S_3","S_3","S_3","S_3","S_3",
                             "S_4","S_4","S_4","S_4","S_4","S_4",
                             "S_5","S_5","S_5","S_5","S_5","S_5")

### total Cstock per square meter
SemiAlgaeCstock$CarbonStock <- SemiAlgaeCstock$CarbonStock/(0.27*0.27)

SemiAlgaeCstockB <- aggregate(x=SemiAlgaeCstock$CarbonStock, by=list(SemiAlgaeCstock$site), FUN=mean)
colnames(SemiAlgaeCstockB)<-c("Site_number","Cstock_gm2")
Site_group = c("Semi", "Semi", "Semi","Semi","Semi", "Semi", "Semi","Semi", "Semi","Semi")
SemiAlgaeCstockB <- cbind(SemiAlgaeCstockB,Site_group)
SemiAlgaeCstockB$Depth <- c("Deep","Deep","Deep","Deep","Deep",
                               "Shallow","Shallow","Shallow","Shallow","Shallow")
SemiAlgaeCstockB$Organism <- "Algae"

##%######################################################%##
#                                                          #
####           Sheltered plant Cstocks                  ####
#                                                          #
##%######################################################%##

Spatial_Campaign_data_Sheltered <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Sheltered", na = "NA")
Shel_data <- Spatial_Campaign_data_Sheltered[,c(1:4,34,36,37)]

#######################################                          
##Cstocks plants based on C/N Samples##
#######################################

C_N_Shel <- C_N_Spatial[ which(C_N_Spatial$Exposure=='Sheltered'), ]
LEAF_Shel <- C_N_Shel[ which(C_N_Shel$Tissue=='Leaf'), ]
ROOT_Shel <- C_N_Shel[ which(C_N_Shel$Tissue=='Root'), ]
RHI_Shel <- C_N_Shel[ which(C_N_Shel$Tissue=='Rhi'), ]


### Median carbon values of tissues
LEAF_C <- aggregate(x= LEAF_Shel$`C (µg/mg)`, by = list(LEAF_Shel$Species), FUN = median)
names(LEAF_C)[1:2]<-paste(c("Species", "LEAF_C"))
ROOT_C <- aggregate(x= ROOT_Shel$`C (µg/mg)`, by = list(ROOT_Shel$Species), FUN = median)
names(ROOT_C)[1:2]<-paste(c("Species", "ROOT_C"))
RHI_C <- aggregate(x= RHI_Shel$`C (µg/mg)`, by = list(RHI_Shel$Species), FUN = median)
names(RHI_C)[1:2]<-paste(c("Species", "RHI_C"))

### combine biomass and species carbon values
Shel_C<- merge(Shel_data,LEAF_C,by="Species",  all = TRUE)
Shel_C<- merge(Shel_C,ROOT_C,by="Species",  all = TRUE)
Shel_C<- merge(Shel_C,RHI_C,by="Species",  all = TRUE)
Shel_C[is.na(Shel_C)] = 0

### get total carbon stock for each species in grams in a quadrat
Shel_C$CarbonStockRoot <- (Shel_C$`Total root dry mass (mg)`*(Shel_C$ROOT_C/1000))/1000
Shel_C$CarbonStockRhi <- (Shel_C$`Total rhizome dry mass (mg)`*(Shel_C$RHI_C/1000))/1000
Shel_C$CarbonStockLeaf <- (Shel_C$`Total aboveground dry mass (mg)`*(Shel_C$LEAF_C/1000))/1000
Shel_C[is.na(Shel_C)] <- 0
Shel_C$TotalC <- Shel_C$CarbonStockLeaf + Shel_C$CarbonStockRoot + Shel_C$CarbonStockRhi

### Total  Cstock per quadrat
Plot_Abun_C <- with(Shel_C, tapply(TotalC, list(Site, Species), sum))
Plot_Abun_C[is.na(Plot_Abun_C)] = 0
ShelPlantCstock <- as.data.frame (rowSums(Plot_Abun_C))
setDT(ShelPlantCstock, keep.rownames = "Plot")
names(ShelPlantCstock)[2]<-paste("CarbonStock")
ShelPlantCstock$Category <- c("Shel")
ShelPlantCstock <- ShelPlantCstock[,-c(1)]
ShelPlantCstock$site <- c("S_1","S_1","S_1","S_1","S_1","S_1",
                          "S_2","S_2","S_2","S_2","S_2","S_2",
                          "S_3","S_3","S_3","S_3","S_3","S_3",
                          "S_4","S_4","S_4","S_4","S_4","S_4",
                          "S_5","S_5","S_5","S_5","S_5","S_5")

### total Cstock per square meter
ShelPlantCstock$CarbonStock <- ShelPlantCstock$CarbonStock/(0.27*0.27)

ShelPlantCstockB <- aggregate(x=ShelPlantCstock$CarbonStock, by=list(ShelPlantCstock$site), FUN=mean)
colnames(ShelPlantCstockB)<-c("Site_number","Cstock_gm2")
Site_group = c("Shel","Shel","Shel","Shel","Shel")
ShelPlantCstockB <- cbind(ShelPlantCstockB,Site_group)
ShelPlantCstockB$Depth <- c("Shallow","Shallow","Shallow","Shallow","Shallow")

#### add deep sites
Cstock_ShelD <- data.frame(Site_number=c("D_1", "D_2", "D_3","D_4","D_5"),
                           Cstock_gm2=c(0,0,0,0,0),
                           Site_group=c("Shel","Shel","Shel","Shel","Shel"),
                           Depth = c("Deep","Deep", "Deep", "Deep", "Deep"))
ShelPlantCstockB <-rbind(ShelPlantCstockB,Cstock_ShelD)
ShelPlantCstockB$Organism <- "Plant"

##%######################################################%##
#                                                          #
####              Shel algae Cstocks                    ####
#                                                          #
##%######################################################%##

# get total carbon stock for each species in grams in a quadrat
Shel_Algae <- Spatial_Campaign_data_Sheltered[,c(1:3,25:28, 31)]
Shel_Algae[is.na(Shel_Algae)] =  0

### Mean carbon values of tissues
Algae_Shel <- C_N_Shel[ which(C_N_Shel$Organism=='Algae'), ]
Algae_C <- aggregate(x= Algae_Shel$`C (µg/mg)`, by = list(Algae_Shel$Species), FUN = median)
names(Algae_C)[1:2]<-paste(c("Species", "Algae_C"))

Shel_Algae$`Green algae (mg)` <- (Shel_Algae$`Green algae (mg)`* 0.3174035)/1000
Shel_Algae$`Vauhcheria (mg)` <- (Shel_Algae$`Vauhcheria (mg)`* 0.2417111)/1000
Shel_Algae$`Chara globularis (mg)` <- (Shel_Algae$`Chara globularis (mg)`* 0.3468252)/1000
Shel_Algae$`Chara sp. (mg)` <- (Shel_Algae$`Chara sp. (mg)` * 0.3468252)/1000
Shel_Algae$`Chorda (mg)` <- (Shel_Algae$`Chorda (mg)`* 0.2759433)/1000
Shel_Algae$TotalC <- Shel_Algae$`Green algae (mg)` + Shel_Algae$`Vauhcheria (mg)` + Shel_Algae$`Chara globularis (mg)` +
                     Shel_Algae$`Chara sp. (mg)` + Shel_Algae$`Chorda (mg)` 

### Total  Cstock per quadrat
Plot_Abun_C <- with(Shel_Algae, tapply(TotalC, list(Site), sum))
Plot_Abun_C[is.na(Plot_Abun_C)] = 0
ShelAlgaeCstock <- as.data.frame (Plot_Abun_C)
setDT(ShelAlgaeCstock, keep.rownames = "Plot")
names(ShelAlgaeCstock)[2]<-paste("CarbonStock")
ShelAlgaeCstock$Category <- c("Shel")
ShelAlgaeCstock <- ShelAlgaeCstock[,-c(1)]
ShelAlgaeCstock$site <- c("S_1","S_1","S_1","S_1","S_1","S_1",
                          "S_2","S_2","S_2","S_2","S_2","S_2",
                          "S_3","S_3","S_3","S_3","S_3","S_3",
                          "S_4","S_4","S_4","S_4","S_4","S_4",
                          "S_5","S_5","S_5","S_5","S_5","S_5")

### total Cstock per square meter
ShelAlgaeCstock$CarbonStock <- ShelAlgaeCstock$CarbonStock/(0.27*0.27)

ShelAlgaeCstockB <- aggregate(x=ShelAlgaeCstock$CarbonStock, by=list(ShelAlgaeCstock$site), FUN=mean)
colnames(ShelAlgaeCstockB)<-c("Site_number","Cstock_gm2")
Site_group = c("Shel", "Shel", "Shel","Shel","Shel")
ShelAlgaeCstockB <- cbind(ShelAlgaeCstockB,Site_group)
ShelAlgaeCstockB$Depth <- c("Shallow","Shallow","Shallow","Shallow","Shallow")

#### add deep sites
Cstock_ShelD <- data.frame(Site_number=c("D_1", "D_2", "D_3","D_4","D_5"),
                           Cstock_gm2=c(0,0,0,0,0),
                           Site_group=c("Shel","Shel","Shel","Shel","Shel"),
                           Depth = c("Deep","Deep", "Deep", "Deep", "Deep"))
ShelAlgaeCstockB <-rbind(ShelAlgaeCstockB,Cstock_ShelD)
ShelAlgaeCstockB$Organism <- "Algae"



##%######################################################%##
#                                                          #
####            Pojo bay plant Cstocks                  ####
#                                                          #
##%######################################################%##

Spatial_Campaign_data_pojo <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                              sheet = "Pojo", na = "NA")
Pojo_data <- Spatial_Campaign_data_pojo[-c(1,4,6,7),c(1:4,32,34,35)]

#######################################                          
##Cstocks plants based on C/N Samples##
#######################################

C_N_Pojo <- C_N_Spatial[ which(C_N_Spatial$Exposure=='Pojo'), ]
LEAF_Pojo <- C_N_Pojo[ which(C_N_Pojo$Tissue=='Leaf'), ]
ROOT_Pojo <- C_N_Pojo[ which(C_N_Pojo$Tissue=='Root'), ]
RHI_Pojo <- C_N_Pojo[ which(C_N_Pojo$Tissue=='Rhi'), ]


### Median carbon values of tissues
LEAF_C <- aggregate(x= LEAF_Pojo$`C (µg/mg)`, by = list(LEAF_Pojo$Species), FUN = median)
names(LEAF_C)[1:2]<-paste(c("Species", "LEAF_C"))
ROOT_C <- aggregate(x= ROOT_Pojo$`C (µg/mg)`, by = list(ROOT_Pojo$Species), FUN = median)
names(ROOT_C)[1:2]<-paste(c("Species", "ROOT_C"))
RHI_C <- aggregate(x= RHI_Pojo$`C (µg/mg)`, by = list(RHI_Pojo$Species), FUN = median)
names(RHI_C)[1:2]<-paste(c("Species", "RHI_C"))

### Adjust species names for matching
Pojo_data$Species[Pojo_data$Species == 'Fontinalis'] <- 'FONTINALES'
Pojo_data$Species[Pojo_data$Species == 'Sparganium'] <- 'SPARGANIUM'

### combine biomass and species carbon values
Pojo_C<- merge(Pojo_data,LEAF_C,by="Species",  all = TRUE)
Pojo_C<- merge(Pojo_C,ROOT_C,by="Species",  all = TRUE)
Pojo_C<- merge(Pojo_C,RHI_C,by="Species",  all = TRUE)
Pojo_C[is.na(Pojo_C)] = 0

### get total carbon stock for each species in grams in a quadrat
Pojo_C$CarbonStockRoot <- (Pojo_C$`Total root dry mass (mg)`*(Pojo_C$ROOT_C/1000))/1000
Pojo_C$CarbonStockRhi <- (Pojo_C$`Total rhizome dry mass (mg)`*(Pojo_C$RHI_C/1000))/1000
Pojo_C$CarbonStockLeaf <- (Pojo_C$`Total aboveground dry mass (mg)`*(Pojo_C$LEAF_C/1000))/1000
Pojo_C[is.na(Pojo_C)] <- 0
Pojo_C$TotalC <- Pojo_C$CarbonStockLeaf + Pojo_C$CarbonStockRoot + Pojo_C$CarbonStockRhi

### Total  Cstock per quadrat
Plot_Abun_C <- with(Pojo_C, tapply(TotalC, list(Site, Species), sum))
Plot_Abun_C[is.na(Plot_Abun_C)] = 0
PojoPlantCstock <- as.data.frame (rowSums(Plot_Abun_C))
setDT(PojoPlantCstock, keep.rownames = "Plot")
names(PojoPlantCstock)[2]<-paste("CarbonStock")
PojoPlantCstock$Category <- c("Pojo")
PojoPlantCstock <- PojoPlantCstock[,-c(1)]
PojoPlantCstock$site <- c("D_2","D_2",
                          "D_3","D_3","D_3","D_3","D_3","D_3",
                          "S_1","S_1","S_1","S_1","S_1","S_1",
                          "S_2","S_2","S_2","S_2","S_2","S_2",
                          "S_3","S_3","S_3","S_3","S_3","S_3",
                          "S_4","S_4","S_4","S_4","S_4","S_4",
                          "S_5","S_5","S_5","S_5","S_5","S_5")

### total Cstock per square meter
PojoPlantCstock$CarbonStock <- PojoPlantCstock$CarbonStock/(0.27*0.27)

PojoPlantCstockB <- aggregate(x=PojoPlantCstock$CarbonStock, by=list(PojoPlantCstock$site), FUN=mean)
colnames(PojoPlantCstockB)<-c("Site_number","Cstock_gm2")
Site_group = c("Pojo","Pojo","Pojo","Pojo","Pojo","Pojo","Pojo")
PojoPlantCstockB <- cbind(PojoPlantCstockB,Site_group)
PojoPlantCstockB$Depth <- c("Deep","Deep",
                             "Shallow","Shallow","Shallow","Shallow","Shallow")

#### add deep sites
Cstock_PojoD <- data.frame(Site_number=c("D_1","D_4","D_5"),
                           Cstock_gm2=c(0,0,0),
                           Site_group=c("Pojo","Pojo","Pojo"),
                           Depth = c("Deep","Deep", "Deep"))
PojoPlantCstockB <-rbind(PojoPlantCstockB,Cstock_PojoD)
PojoPlantCstockB$Organism <- "Plant"

##%######################################################%##
#                                                          #
####              Pojo algae Cstocks                    ####
#                                                          #
##%######################################################%##

# get total carbon stock for each species in grams in a quadrat
Pojo_Algae <- Spatial_Campaign_data_pojo[,c(1:3,26:29)]
Pojo_Algae[is.na(Pojo_Algae)] =  0

### Mean carbon values of tissues
Algae_Pojo <- C_N_Pojo[ which(C_N_Pojo$Organism=='Algae'), ]
Algae_C <- aggregate(x= Algae_Pojo$`C (µg/mg)`, by = list(Algae_Pojo$Species), FUN = median)
names(Algae_C)[1:2]<-paste(c("Species", "Algae_C"))

Pojo_Algae$`Green algae (mg)` <- (Pojo_Algae$`Green algae (mg)`* 0.1551968)/1000
Pojo_Algae$`Vaucheria sp (mg)` <- (Pojo_Algae$`Vaucheria sp (mg)`* 0.2293567)/1000
Pojo_Algae$`Aegagrophila limnaeib (mg)` <- (Pojo_Algae$`Aegagrophila limnaeib (mg)`* 0.3022541)/1000
Pojo_Algae$`Chara sp (mg)`<- (Pojo_Algae$`Chara sp (mg)`* 0.3563516)/1000

Pojo_Algae$TotalC <- Pojo_Algae$`Green algae (mg)` + Pojo_Algae$`Vaucheria sp (mg)` + Pojo_Algae$`Aegagrophila limnaeib (mg)` +
  Pojo_Algae$`Chara sp (mg)`

### Total  Cstock per quadrat
Plot_Abun_C <- with(Pojo_Algae, tapply(TotalC, list(Site), sum))
Plot_Abun_C[is.na(Plot_Abun_C)] = 0
PojoAlgaeCstock <- as.data.frame (Plot_Abun_C)
setDT(PojoAlgaeCstock, keep.rownames = "Plot")
names(PojoAlgaeCstock)[2]<-paste("CarbonStock")
PojoAlgaeCstock$Category <- c("Pojo")
PojoAlgaeCstock <- PojoAlgaeCstock[,-c(1)]
PojoAlgaeCstock$site <- c("D_2","D_2","D_2","D_2","D_2","D_2",
                          "D_3","D_3","D_3","D_3","D_3","D_3",
                          "S_1","S_1","S_1","S_1","S_1","S_1",
                          "S_2","S_2","S_2","S_2","S_2","S_2",
                          "S_3","S_3","S_3","S_3","S_3","S_3",
                          "S_4","S_4","S_4","S_4","S_4","S_4",
                          "S_5","S_5","S_5","S_5","S_5","S_5")

### total Cstock per square meter
PojoAlgaeCstock$CarbonStock <- PojoAlgaeCstock$CarbonStock/(0.27*0.27)

PojoAlgaeCstockB <- aggregate(x=PojoAlgaeCstock$CarbonStock, by=list(PojoAlgaeCstock$site), FUN=mean)
colnames(PojoAlgaeCstockB)<-c("Site_number","Cstock_gm2")
Site_group = c("Pojo", "Pojo", "Pojo","Pojo","Pojo","Pojo","Pojo")
PojoAlgaeCstockB <- cbind(PojoAlgaeCstockB,Site_group)
PojoAlgaeCstockB$Depth <- c("Deep", "Deep",
                            "Shallow","Shallow","Shallow","Shallow","Shallow")

#### add deep sites
Cstock_PojoD <- data.frame(Site_number=c("D_1","D_4","D_5"),
                           Cstock_gm2=c(0,0,0),
                           Site_group=c("Pojo","Pojo","Pojo"),
                           Depth = c("Deep","Deep", "Deep"))
PojoAlgaeCstockB <-rbind(PojoAlgaeCstockB,Cstock_PojoD)
PojoAlgaeCstockB$Organism <- "Algae"

##%######################################################%##
#                                                          #
####                 Compiled datasets                  ####
#                                                          #
##%######################################################%##


Cstock_all<-rbind(PojoPlantCstockB,ShelPlantCstockB,SemiPlantCstockB,ExposedPlantCstockB,
                             PojoAlgaeCstockB,ShelAlgaeCstockB,SemiAlgaeCstockB,ExposedAlgaeCstockB)

Cstock_all$MacrophyteCategory <- factor(paste(Cstock_all$Site_group, 
                                              Cstock_all$Organism, sep = "_"),
                                                  levels = c(
                                                    "exposed_Plant", "exposed_Algae",
                                                    "Semi_Plant", "Semi_Algae",
                                                    "Shel_Plant", "Shel_Algae",
                                                    "Pojo_Plant", "Pojo_Algae"
                                                  ))

Cstock_Plant <- Cstock_all[ which(Cstock_all$Organism=='Plant'), ]

Cstock_Plant$MacrophyteCategoryDepth <- factor(paste(Cstock_Plant$MacrophyteCategory, 
                                                     Cstock_Plant$Depth, sep = "_"),
                                        levels = c(
                                          "exposed_Plant_Shallow", "exposed_Plant_Deep",
                                          "Semi_Plant_Shallow", "Semi_Plant_Deep",
                                          "Shel_Plant_Shallow", "Shel_Plant_Deep",
                                          "Pojo_Plant_Shallow", "Pojo_Plant_Deep"
                                        ))


##%######################################################%##
#                                                          #
####                  Plot figure                       ####
#                                                          #
##%######################################################%##


png(
  "output_plot/MainFigure_CarbonStocksPlant.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 900
)

par(mfcol=c(1,1))

par(mar = c(3, 5, 2, 3))

Cstock_Plant$MacrophyteCategoryDepth <- factor(Cstock_Plant$MacrophyteCategoryDepth , # Reorder factor levels
                                                     c("exposed_Plant_Shallow", "exposed_Plant_Deep", "Semi_Plant_Shallow", "Semi_Plant_Deep", 
                                                       "Shel_Plant_Shallow", "Shel_Plant_Deep", "Pojo_Plant_Shallow", "Pojo_Plant_Deep"))

boxplot(Cstock_gm2 ~ MacrophyteCategoryDepth, data = Cstock_Plant,
        boxwex = 0.5, col = c("bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "g C m-2", ylim = c(0, 100),at = c(1, 2, 4, 5, 7, 8, 10, 11),
        lex.order = TRUE, xaxt="n",cex.lab = 1.3)

stripchart(Cstock_gm2 ~ MacrophyteCategoryDepth,
           data = Cstock_Plant,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5, 6.5,7.5,9.5,10.5),
           pch = 17,
           col = c("bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(1.5 , 11 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)

legend("topleft", legend = c("Shallow", "Deep"), 
       col=c("bisque","azure2"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1.3,  horiz = F)


dev.off()

