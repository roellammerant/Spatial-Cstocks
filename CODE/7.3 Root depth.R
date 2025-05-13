##%######################################################%##
#                                                          #
####                  Normality tests                   ####
#                                                          #
##%######################################################%##
library(rstatix)

### Exposed_Shallow
Indices_RootDepth_AllsitesB[c(1:31),c(6)]
mshapiro_test(as.numeric(Indices_RootDepth_AllsitesB[c(1:31),c(2)])) # non-significant
mshapiro_test(as.numeric(Indices_RootDepth_AllsitesB[c(1:31),c(5)])) # significant

### Exposed_Deep
Indices_RootDepth_AllsitesB[c(32:57),c(6)]
mshapiro_test(as.numeric(Indices_RootDepth_AllsitesB[c(32:57),c(2)])) # significant
mshapiro_test(as.numeric(Indices_RootDepth_AllsitesB[c(32:57),c(5)])) # significant

### Semi_shallow
Indices_RootDepth_AllsitesB[c(58:87),c(6)]
mshapiro_test(as.numeric(Indices_RootDepth_AllsitesB[c(58:87),c(2)])) # non-significant
mshapiro_test(as.numeric(Indices_RootDepth_AllsitesB[c(58:87),c(5)])) # significant

### Semi_Deep
Indices_RootDepth_AllsitesB[c(88:117),c(6)]
mshapiro_test(as.numeric(Indices_RootDepth_AllsitesB[c(88:117),c(2)])) # significant
mshapiro_test(as.numeric(Indices_RootDepth_AllsitesB[c(88:117),c(5)])) # significant

### Pojo_Deep
Indices_RootDepth_AllsitesB[c(118:125),c(6)]
mshapiro_test(as.numeric(Indices_RootDepth_AllsitesB[c(118:125),c(2)])) # non-significant
mshapiro_test(as.numeric(Indices_RootDepth_AllsitesB[c(118:125),c(5)])) # non-significant

### Pojo_Shallow
Indices_RootDepth_AllsitesB[c(126:155),c(6)]
mshapiro_test(as.numeric(Indices_RootDepth_AllsitesB[c(126:155),c(2)])) # significant
mshapiro_test(as.numeric(Indices_RootDepth_AllsitesB[c(126:155),c(5)])) # significant

### Sheltered_Shallow
Indices_RootDepth_AllsitesB[c(156:185),c(6)]
mshapiro_test(as.numeric(Indices_RootDepth_AllsitesB[c(156:185),c(2)])) # significant
mshapiro_test(as.numeric(Indices_RootDepth_AllsitesB[c(156:185),c(5)])) # significant

### FDis and CWM are overall non-normal distributes

##%######################################################%##
#                                                          #
####                Homogeneity tests                   ####
#                                                          #
##%######################################################%##
library(stats)
Indices_RootDepth_AllsitesB$CWM_RootDepth<- as.numeric(Indices_RootDepth_AllsitesB$CWM_RootDepth) 
Indices_RootDepth_AllsitesB$FDis_RootDepth<- as.numeric(Indices_RootDepth_AllsitesB$FDis_RootDepth) 

fligner.test(Indices_RootDepth_AllsitesB$CWM_RootDepth ~ Indices_RootDepth_AllsitesB$Exposure_Depth) # signifcant
fligner.test(Indices_RootDepth_AllsitesB$FDis_RootDepth ~ Indices_RootDepth_AllsitesB$Exposure_Depth) # signifcant

### FDis and CWM show heteroscedasticity

##%######################################################%##
#                                                          #
####      Non-parametric test + heteroscedasticity     #####
#                                                          #
##%######################################################%##

##%######################################################%##
#                                                          #
####            Non parametric permanova                ####   
####                                                    ####
#                                                          #
##%######################################################%##

# Permanova can´t handle zero`s
Indices_RootDepth_AllsitesB$FDis_RootDepth[Indices_RootDepth_AllsitesB$FDis_RootDepth == 0] <- 0.00001 
Indices_RootDepth_AllsitesB$CWM_RootDepth <- Indices_RootDepth_AllsitesB$CWM_RootDepth +1 # problems with zero values
Shallow_RootDepth <- Indices_RootDepth_AllsitesB[ which(Indices_RootDepth_AllsitesB$Depth=='Shallow'), ]
Deep_RootDepth <- Indices_RootDepth_AllsitesB[ which(Indices_RootDepth_AllsitesB$Depth=='Deep'), ]

######### Shallow sites #########
CWMRootDepth_S <- adonis2(Shallow_RootDepth$CWM_RootDepth ~ Exposure + Site_Exposure, data=Shallow_RootDepth, perm=999)
CWMRootDepth_S # significant difference at exposure and site level

FDisRootDepth_S <- adonis2(Shallow_RootDepth$FDis_RootDepth ~ Exposure + Site_Exposure , data=Shallow_RootDepth, perm=999)
FDisRootDepth_S # significant difference at exposure and site level

######### Deep sites #########
CWMRootDepth_D <- adonis2(Deep_RootDepth$CWM_RootDepth ~ Exposure + Site_Exposure, data=Deep_RootDepth, perm=999)
CWMRootDepth_D # significant difference at exposure and site level

FDisRootDepth_D <- adonis2(Deep_RootDepth$FDis_RootDepth ~ Exposure + Site_Exposure , data=Deep_RootDepth, perm=999)
FDisRootDepth_D # significant difference at exposure and site level

######### Between depths #########
CWMRootDepth_Depths <- adonis2(Indices_RootDepth_AllsitesB$CWM_RootDepth ~ Depth + Site_Exposure, 
                            data=Indices_RootDepth_AllsitesB, perm=999)
CWMRootDepth_Depths # significant difference among exposure-depth and site level

FDisRootDepth_Depths <- adonis2(Indices_RootDepth_AllsitesB$FDis_RootDepth ~ Depth + Site_Exposure, 
                             data=Indices_RootDepth_AllsitesB, perm=999)
FDisRootDepth_Depths # significant difference among exposure-depth and site level

##%######################################################%##
#                                                          #
####                 Post-hoc adonis test               ####   
#                                                          #
##%######################################################%##

######### Shallow sites #########

######### CWM
table(Shallow_RootDepth$Exposure_Depth[c(1:61)])
dist_matrixA=vegdist(Shallow_RootDepth$CWM_RootDepth[c(1:61)],method="manhattan")
pairwise.adonis2(dist_matrixA ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_RootDepth[c(1:61),])

table(Shallow_RootDepth$Exposure_Depth[c(32:61, 92:121)])
dist_matrixB=vegdist(Shallow_RootDepth$CWM_RootDepth[c(32:61, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixB ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_RootDepth[c(32:61, 92:121),])

table(Shallow_RootDepth$Exposure_Depth[c(92:121, 62:91)])
dist_matrixC=vegdist(Shallow_RootDepth$CWM_RootDepth[c(92:121, 62:91)],method="manhattan")
pairwise.adonis2(dist_matrixC ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_RootDepth[c(92:121, 62:91),])

######### FDis

table(Shallow_RootDepth$Exposure_Depth[c(1:61)])
dist_matrixA2=vegdist(Shallow_RootDepth$FDis_RootDepth[c(1:61)],method="manhattan")
pairwise.adonis2(dist_matrixA2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_RootDepth[c(1:61),])

table(Shallow_RootDepth$Exposure_Depth[c(32:61, 92:121)])
dist_matrixB2=vegdist(Shallow_RootDepth$FDis_RootDepth[c(32:61, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixB2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_RootDepth[c(32:61, 92:121),])

table(Shallow_RootDepth$Exposure_Depth[c(92:121, 62:91)])
dist_matrixC2=vegdist(Shallow_RootDepth$FDis_RootDepth[c(92:121, 62:91)],method="manhattan")
pairwise.adonis2(dist_matrixC2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_RootDepth[c(92:121, 62:91),])

table(Shallow_RootDepth$Exposure_Depth[c(1:31, 92:121)])
dist_matrixD2=vegdist(Shallow_RootDepth$FDis_RootDepth[c(1:31, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixD2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_RootDepth[c(1:31, 92:121),])

######### Deep sites #########

######### CWM
table(Deep_RootDepth$Exposure_Depth[c(1:26,27:56)])
dist_matrixAD=vegdist(Deep_RootDepth$CWM_RootDepth[c(1:26,27:56)],method="manhattan")
pairwise.adonis2(dist_matrixAD ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_RootDepth[c(1:26,27:56),])

table(Deep_RootDepth$Exposure_Depth[c(27:64)])
dist_matrixBD=vegdist(Deep_RootDepth$CWM_RootDepth[c(27:64)],method="manhattan")
pairwise.adonis2(dist_matrixBD ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_RootDepth[c(27:64),])

######### FDis
table(Deep_RootDepth$Exposure_Depth[c(1:26,27:56)])
dist_matrixBD=vegdist(Deep_RootDepth$FDis_RootDepth[c(1:26,27:56)],method="manhattan")
pairwise.adonis2(dist_matrixBD ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_RootDepth[c(1:26,27:56),])

table(Deep_RootDepth$Exposure_Depth[c(27:64)])
dist_matrixBD2=vegdist(Deep_RootDepth$FDis_RootDepth[c(27:64)],method="manhattan")
pairwise.adonis2(dist_matrixBD2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_RootDepth[c(27:64),])

######### Between depths #########
ExposedDepths <-Indices_RootDepth_AllsitesB[c(1:57),]
SemiDepths <-Indices_RootDepth_AllsitesB[c(58:117),]
PojoDepths <-Indices_RootDepth_AllsitesB[c(118:155),]

### Exposed
dist_matrixA_ExposedDepths=vegdist(ExposedDepths$CWM_RootDepth,method="manhattan")
pairwise.adonis2(dist_matrixA_ExposedDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = ExposedDepths) #significant depth and site level

dist_matrixB_ExposedDepths=vegdist(ExposedDepths$FDis_RootDepth,method="manhattan")
pairwise.adonis2(dist_matrixB_ExposedDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = ExposedDepths) #significant site level

###Semi
dist_matrixA_SemiDepths=vegdist(SemiDepths$CWM_RootDepth,method="manhattan")
pairwise.adonis2(dist_matrixA_SemiDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = SemiDepths) #significant depth and site level

dist_matrixB_SemiDepths=vegdist(SemiDepths$FDis_RootDepth,method="manhattan")
pairwise.adonis2(dist_matrixB_SemiDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = SemiDepths) #significant depth 

### Pojo
dist_matrixA_PojoDepths=vegdist(PojoDepths$CWM_RootDepth,method="manhattan")
pairwise.adonis2(dist_matrixA_PojoDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PojoDepths) #significant depth and site level

dist_matrixB_PojoDepths=vegdist(PojoDepths$FDis_RootDepth,method="manhattan")
pairwise.adonis2(dist_matrixB_PojoDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PojoDepths) #significant site level







