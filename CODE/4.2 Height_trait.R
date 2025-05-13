##%######################################################%##
#                                                          #
####                  Normality tests                   ####
#                                                          #
##%######################################################%##
library(rstatix)

### Exposed_Shallow
Indices_height_Allsites[c(1:31),c(8)]
mshapiro_test(as.numeric(Indices_height_Allsites[c(1:31),c(5)])) # significant
mshapiro_test(as.numeric(Indices_height_Allsites[c(1:31),c(6)])) # marginally significant

### Exposed_Deep
Indices_height_Allsites[c(31:57),c(8)]
mshapiro_test(as.numeric(Indices_height_Allsites[c(31:57),c(5)])) # significant
mshapiro_test(as.numeric(Indices_height_Allsites[c(31:57),c(6)])) # significant

### Semi_Shallow
Indices_height_Allsites[c(58:87),c(8)]
mshapiro_test(as.numeric(Indices_height_Allsites[c(58:87),c(5)])) # significant
mshapiro_test(as.numeric(Indices_height_Allsites[c(58:87),c(6)])) # significant

### Semi_Deep
Indices_height_Allsites[c(88:117),c(8)]
mshapiro_test(as.numeric(Indices_height_Allsites[c(88:117),c(5)])) # significant
mshapiro_test(as.numeric(Indices_height_Allsites[c(88:117),c(6)])) # significant

### Pojo_Deep
Indices_height_Allsites[c(118:125),c(8)]
mshapiro_test(as.numeric(Indices_height_Allsites[c(118:125),c(5)])) # non-significant
mshapiro_test(as.numeric(Indices_height_Allsites[c(118:125),c(6)])) # significant

### Pojo_Shallow
Indices_height_Allsites[c(126:155),c(8)]
mshapiro_test(as.numeric(Indices_height_Allsites[c(126:155),c(5)])) # significant
mshapiro_test(as.numeric(Indices_height_Allsites[c(126:155),c(6)])) # significant

### Sheltered_Shallow
Indices_height_Allsites[c(156:185),c(8)]
mshapiro_test(as.numeric(Indices_height_Allsites[c(156:185),c(5)])) # significant
mshapiro_test(as.numeric(Indices_height_Allsites[c(156:185),c(6)])) # significant

### FDis and CWM are overall non-normal distributes

##%######################################################%##
#                                                          #
####                Homogeneity tests                   ####
#                                                          #
##%######################################################%##
library(stats)
Indices_height_Allsites$CWM_Height<- as.numeric(Indices_height_Allsites$CWM_Height) 
Indices_height_Allsites$FDis_Height<- as.numeric(Indices_height_Allsites$FDis_Height) 
Indices_height_Allsites$Exposure<- as.factor(Indices_height_Allsites$Exposure) 

fligner.test(Indices_height_Allsites$CWM_Height ~ Indices_height_Allsites$Exposure_Depth) # signifcant
fligner.test(Indices_height_Allsites$FDis_Height ~ Indices_height_Allsites$Exposure_Depth) # signifcant

### FDis and CWM show heteroscedasticity

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
Indices_height_Allsites$FDis_Height[Indices_height_Allsites$FDis_Height == 0] <- 0.00001 
Shallow_Height <- Indices_height_Allsites[ which(Indices_height_Allsites$Depth=='Shallow'), ]
Deep_Height <- Indices_height_Allsites[ which(Indices_height_Allsites$Depth=='Deep'), ]

######### Shallow sites #########
# https://uw.pressbooks.pub/appliedmultivariatestatistics/chapter/permanova/
# Doesn´t assume normality or homogeneity of variance

CWMHeight_S <- adonis2(Shallow_Height$CWM_Height ~ Exposure + Site_Exposure, data=Shallow_Height, perm=999)
CWMHeight_S # significant difference at exposure and site level

FDisHeight_S <- adonis2(Shallow_Height$FDis_Height ~ Exposure + Site_Exposure , data=Shallow_Height, perm=999)
FDisHeight_S # significant difference at exposure and site level

######### Deep sites #########

CWMHeight_D <- adonis2(Deep_Height$CWM_Height ~ Exposure + Site_Exposure, data=Deep_Height, perm=999)
CWMHeight_D # significant difference at exposure and site level

FDisHeight_D <- adonis2(Deep_Height$FDis_Height ~ Exposure + Site_Exposure , data=Deep_Height, perm=999)
FDisHeight_D # significant difference at exposure and site level

######### Between depths #########

CWMHeight_Depths <- adonis2(Indices_height_Allsites$CWM_Height ~ Depth + Site_Exposure, 
                            data=Indices_height_Allsites, perm=999)
CWMHeight_Depths # significant difference among exposure-depth and site level

FDisHeight_Depths <- adonis2(Indices_height_Allsites$FDis_Height ~ Depth + Site_Exposure, 
                             data=Indices_height_Allsites, perm=999)
FDisHeight_Depths # significant difference among exposure-depth and site level

##%######################################################%##
#                                                          #
####                 Post-hoc adonis test               ####   
#                                                          #
##%######################################################%##
library(devtools)
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)

######### Shallow sites #########

######### CWM
table(Shallow_Height$Exposure_Depth[c(1:61)])
dist_matrixA=vegdist(Shallow_Height$CWM_Height[c(1:61)],method="manhattan")
pairwise.adonis2(dist_matrixA ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_Height[c(1:61),])

table(Shallow_Height$Exposure_Depth[c(32:61, 92:121)])
dist_matrixB=vegdist(Shallow_Height$CWM_Height[c(32:61, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixB ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_Height[c(32:61, 92:121),])

table(Shallow_Height$Exposure_Depth[c(92:121, 62:91)])
dist_matrixC=vegdist(Shallow_Height$CWM_Height[c(92:121, 62:91)],method="manhattan")
pairwise.adonis2(dist_matrixC ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_Height[c(92:121, 62:91),])

######### FDis

table(Shallow_Height$Exposure_Depth[c(1:61)])
dist_matrixA2=vegdist(Shallow_Height$FDis_Height[c(1:61)],method="manhattan")
pairwise.adonis2(dist_matrixA2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_Height[c(1:61),])

table(Shallow_Height$Exposure_Depth[c(32:61, 92:121)])
dist_matrixB2=vegdist(Shallow_Height$FDis_Height[c(32:61, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixB2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_Height[c(32:61, 92:121),])

table(Shallow_Height$Exposure_Depth[c(92:121, 62:91)])
dist_matrixC2=vegdist(Shallow_Height$FDis_Height[c(92:121, 62:91)],method="manhattan")
pairwise.adonis2(dist_matrixC2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_Height[c(92:121, 62:91),])

table(Shallow_Height$Exposure_Depth[c(1:31, 92:121)])
dist_matrixD2=vegdist(Shallow_Height$FDis_Height[c(1:31, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixD2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_Height[c(1:31, 92:121),])

######### Deep sites #########

######### CWM
table(Deep_Height$Exposure_Depth[c(1:26,27:56)])
dist_matrixAD=vegdist(Deep_Height$CWM_Height[c(1:26,27:56)],method="manhattan")
pairwise.adonis2(dist_matrixAD ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_Height[c(1:26,27:56),])

table(Deep_Height$Exposure_Depth[c(27:64)])
dist_matrixAD2=vegdist(Deep_Height$CWM_Height[c(27:64)],method="manhattan")
pairwise.adonis2(dist_matrixAD2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_Height[c(27:64),])

######### FDis
table(Deep_Height$Exposure_Depth[c(1:26,27:56)])
dist_matrixBD=vegdist(Deep_Height$FDis_Heigh[c(1:26,27:56)],method="manhattan")
pairwise.adonis2(dist_matrixBD ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_Height[c(1:26,27:56),])

table(Deep_Height$Exposure_Depth[c(27:64)])
dist_matrixBD2=vegdist(Deep_Height$FDis_Heigh[c(27:64)],method="manhattan")
pairwise.adonis2(dist_matrixBD2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_Height[c(27:64),])


######### Between depths #########
ExposedDepths <-Indices_height_Allsites[c(1:57),]
SemiDepths <-Indices_height_Allsites[c(58:117),]
PojoDepths <-Indices_height_Allsites[c(118:155),]

#### Exposed

dist_matrixA_ExposedDepths=vegdist(ExposedDepths$CWM_Height,method="manhattan")
pairwise.adonis2(dist_matrixA_ExposedDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = ExposedDepths) #significant depth and site level

dist_matrixB_ExposedDepths=vegdist(ExposedDepths$FDis_Heigh,method="manhattan")
pairwise.adonis2(dist_matrixB_ExposedDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = ExposedDepths) #significant depth and site level

#### Semi

dist_matrixA_SemiDepths=vegdist(SemiDepths$CWM_Height,method="manhattan")
pairwise.adonis2(dist_matrixA_SemiDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = SemiDepths) #significant depth 

dist_matrixB_SemiDepths=vegdist(SemiDepths$FDis_Heigh,method="manhattan")
pairwise.adonis2(dist_matrixB_SemiDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = SemiDepths) #significant depth 
#### Pojo

dist_matrixA_PojoDepths=vegdist(PojoDepths$CWM_Height,method="manhattan")
pairwise.adonis2(dist_matrixA_PojoDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PojoDepths) #significant depth and site level

dist_matrixB_PojoDepths=vegdist(PojoDepths$FDis_Heigh,method="manhattan")
pairwise.adonis2(dist_matrixB_PojoDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PojoDepths) #significant site level




