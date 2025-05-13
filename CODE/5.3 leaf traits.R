##%######################################################%##
#                                                          #
####                  Normality tests                   ####
#                                                          #
##%######################################################%##
library(rstatix)

### Exposed_Shallow
Indices_leaf_AllsitesB[c(1:31),c(10)]

mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(1:31),c(2)])) # non-significant
mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(1:31),c(6)])) # non-significant

mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(1:31),c(5)])) # significant
mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(1:31),c(7)])) # significant

### Exposed_Deep
Indices_leaf_AllsitesB[c(32:57),c(10)]

mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(32:57),c(2)])) # significant
mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(32:57),c(6)])) # significant

mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(32:57),c(5)])) # significant
mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(32:57),c(7)])) # significant

### Semi_Shallow
Indices_leaf_AllsitesB[c(58:87),c(10)]

mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(58:87),c(2)])) # significant
mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(58:87),c(6)])) # significant

mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(58:87),c(5)])) # significant
mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(58:87),c(7)])) # significant

### Semi_Deep
Indices_leaf_AllsitesB[c(88:117),c(10)]

mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(88:117),c(2)])) # significant
mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(88:117),c(6)])) # significant

mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(88:117),c(5)])) # significant
mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(88:117),c(7)])) # significant

### Pojo_Deep
Indices_leaf_AllsitesB[c(118:125),c(10)]

mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(118:125),c(2)])) # significant
mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(118:125),c(6)])) # non-significant

mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(118:125),c(5)])) # marginally significant
mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(118:125),c(7)])) # significant

### Pojo_Shallow
Indices_leaf_AllsitesB[c(126:155),c(10)]

mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(126:155),c(2)])) # significant
mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(126:155),c(6)])) # significant

mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(126:155),c(5)])) # significant
mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(126:155),c(7)])) # significant

### Sheltered_Shallow
Indices_leaf_AllsitesB[c(156:185),c(10)]

mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(156:185),c(2)])) # significant
mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(156:185),c(6)])) # significant

mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(156:185),c(5)])) # significant
mshapiro_test(as.numeric(Indices_leaf_AllsitesB[c(156:185),c(7)])) # significant

### FDis and CWM are overall non-normal distributes for both SLA and LAP

##%######################################################%##
#                                                          #
####                Homogeneity tests                   ####
#                                                          #
##%######################################################%##
library(stats)
Indices_leaf_AllsitesB$CWM_SLA <- as.numeric(Indices_leaf_AllsitesB$CWM_SLA) 
Indices_leaf_AllsitesB$FDis_SLA<- as.numeric(Indices_leaf_AllsitesB$FDis_SLA) 
Indices_leaf_AllsitesB$CWM_LAP <- as.numeric(Indices_leaf_AllsitesB$CWM_LAP) 
Indices_leaf_AllsitesB$FDis_LAP<- as.numeric(Indices_leaf_AllsitesB$FDis_LAP) 
Indices_leaf_AllsitesB$Exposure<- as.factor(Indices_leaf_AllsitesB$Exposure) 

fligner.test(Indices_leaf_AllsitesB$CWM_SLA ~ Indices_leaf_AllsitesB$Exposure_Depth) # signifcant
fligner.test(Indices_leaf_AllsitesB$FDis_SLA ~ Indices_leaf_AllsitesB$Exposure_Depth) # signifcant

fligner.test(Indices_leaf_AllsitesB$CWM_LAP ~ Indices_leaf_AllsitesB$Exposure_Depth) # signifcant
fligner.test(Indices_leaf_AllsitesB$FDis_LAP ~ Indices_leaf_AllsitesB$Exposure_Depth) # signifcant

### FDis and CWM show heteroscedasticity for both SLA and LAP

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

Indices_leaf_AllsitesB$FDis_SLA[Indices_leaf_AllsitesB$FDis_SLA == 0] <- 0.00001 
Indices_leaf_AllsitesB$FDis_LAP[Indices_leaf_AllsitesB$FDis_LAP == 0] <- 0.00001 
Shallow_Leaf <- Indices_leaf_AllsitesB[ which(Indices_leaf_AllsitesB$Depth=='Shallow'), ]
Deep_Leaf <- Indices_leaf_AllsitesB[ which(Indices_leaf_AllsitesB$Depth=='Deep'), ]

##%######################################################%##
#                                                          #
####              Specific leaf area (SLA)             #####
#                                                          #
##%######################################################%##

######### Shallow sites #########
CWMSLA_S <- adonis2(Shallow_Leaf$CWM_SLA ~ Exposure + Site_Exposure, data=Shallow_Leaf, perm=999)
CWMSLA_S # significant difference at exposure and site level

FDisSLA_S <- adonis2(Shallow_Leaf$FDis_SLA ~ Exposure + Site_Exposure , data=Shallow_Leaf, perm=999)
FDisSLA_S # significant difference at exposure and site level

######### Deep sites #########
CWMSLA_D <- adonis2(Deep_Leaf$CWM_SLA ~ Exposure + Site_Exposure, data=Deep_Leaf, perm=999)
CWMSLA_D # significant difference at exposure and site level

FDisSLA_D <- adonis2(Deep_Leaf$FDis_SLA ~ Exposure + Site_Exposure , data=Deep_Leaf, perm=999)
FDisSLA_D # significant difference at exposure and site level

######### Between depths #########
CWMSLA_Depths <- adonis2(Indices_leaf_AllsitesB$CWM_SLA ~ Depth + Site_Exposure, 
                            data=Indices_leaf_AllsitesB, perm=999)
CWMSLA_Depths # significant difference among exposure-depth and site level

FDisSLA_Depths <- adonis2(Indices_leaf_AllsitesB$FDis_SLA ~ Depth + Site_Exposure, 
                             data=Indices_leaf_AllsitesB, perm=999)
FDisSLA_Depths # significant difference among exposure-depth and site level


##%######################################################%##
#                                                          #
####        Leaf area - perimeter ratio (LAP)          #####
#                                                          #
##%######################################################%##

######### Shallow sites #########
CWMLAP_S <- adonis2(Shallow_Leaf$CWM_LAP ~ Exposure + Site_Exposure, data=Shallow_Leaf, perm=999)
CWMLAP_S # significant difference at exposure and site level

FDisLAP_S <- adonis2(Shallow_Leaf$FDis_LAP ~ Exposure + Site_Exposure , data=Shallow_Leaf, perm=999)
FDisLAP_S # significant difference at exposure and site level

######### Deep sites #########
CWMLAP_D <- adonis2(Deep_Leaf$CWM_LAP ~ Exposure + Site_Exposure, data=Deep_Leaf, perm=999)
CWMLAP_D # significant difference at exposure and site level

FDisLAP_D <- adonis2(Deep_Leaf$FDis_LAP ~ Exposure + Site_Exposure , data=Deep_Leaf, perm=999)
FDisLAP_D # significant difference at exposure and site level

######### Between depths #########
CWMLAP_Depths <- adonis2(Indices_leaf_AllsitesB$CWM_LAP ~ Depth + Site_Exposure, 
                         data=Indices_leaf_AllsitesB, perm=999)
CWMLAP_Depths # significant difference among exposure-depth and site level

FDisLAP_Depths <- adonis2(Indices_leaf_AllsitesB$FDis_LAP ~ Depth + Site_Exposure, 
                          data=Indices_leaf_AllsitesB, perm=999)
FDisLAP_Depths # significant difference among exposure-depth and site level


##%######################################################%##
#                                                          #
####                 Post-hoc adonis test               ####   
#                                                          #
##%######################################################%##

##%######################################################%##
#                                                          #
####              Specific leaf area (SLA)             #####
#                                                          #
##%######################################################%##

######### Shallow sites #########

######### CWM
table(Shallow_Leaf$Exposure_Depth[c(1:61)])
dist_matrixA=vegdist(Shallow_Leaf$CWM_SLA[c(1:61)],method="manhattan")
pairwise.adonis2(dist_matrixA ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_Leaf[c(1:61),])

table(Shallow_Leaf$Exposure_Depth[c(32:61, 92:121)])
dist_matrixB=vegdist(Shallow_Leaf$CWM_SLA[c(32:61, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixB ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_Leaf[c(32:61, 92:121),])

table(Shallow_Leaf$Exposure_Depth[c(92:121, 62:91)])
dist_matrixC=vegdist(Shallow_Leaf$CWM_SLA[c(92:121, 62:91)],method="manhattan")
pairwise.adonis2(dist_matrixC ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_Leaf[c(92:121, 62:91),])

######### FDis

table(Shallow_Leaf$Exposure_Depth[c(1:61)])
dist_matrixA2=vegdist(Shallow_Leaf$FDis_SLA[c(1:61)],method="manhattan")
pairwise.adonis2(dist_matrixA2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_Leaf[c(1:61),])

table(Shallow_Leaf$Exposure_Depth[c(32:61, 92:121)])
dist_matrixB2=vegdist(Shallow_Leaf$FDis_SLA[c(32:61, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixB2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_Leaf[c(32:61, 92:121),])

table(Shallow_Leaf$Exposure_Depth[c(92:121, 62:91)])
dist_matrixC2=vegdist(Shallow_Leaf$FDis_SLA[c(92:121, 62:91)],method="manhattan")
pairwise.adonis2(dist_matrixC2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_Leaf[c(92:121, 62:91),])

table(Shallow_Leaf$Exposure_Depth[c(1:31, 92:121)])
dist_matrixD2=vegdist(Shallow_Leaf$FDis_SLA[c(1:31, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixD2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_Leaf[c(1:31, 92:121),])

######### Deep sites #########

######### CWM
table(Deep_Leaf$Exposure_Depth[c(1:26,27:56)])
dist_matrixAD=vegdist(Deep_Leaf$CWM_SLA[c(1:26,27:56)],method="manhattan")
pairwise.adonis2(dist_matrixAD ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_Leaf[c(1:26,27:56),])

table(Deep_Leaf$Exposure_Depth[c(27:64)])
dist_matrixBD=vegdist(Deep_Leaf$CWM_SLA[c(27:64)],method="manhattan")
pairwise.adonis2(dist_matrixBD ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_Leaf[c(27:64),])

######### FDis
table(Deep_Leaf$Exposure_Depth[c(1:26,27:56)])
dist_matrixBD=vegdist(Deep_Leaf$FDis_SLA[c(1:26,27:56)],method="manhattan")
pairwise.adonis2(dist_matrixBD ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_Leaf[c(1:26,27:56),])

table(Deep_Leaf$Exposure_Depth[c(27:64)])
dist_matrixBD2=vegdist(Deep_Leaf$FDis_SLA[c(27:64)],method="manhattan")
pairwise.adonis2(dist_matrixBD2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_Leaf[c(27:64),])


######### Between depths #########
ExposedDepths <-Indices_leaf_AllsitesB[c(1:57),]
SemiDepths <-Indices_leaf_AllsitesB[c(58:117),]
PojoDepths <-Indices_leaf_AllsitesB[c(118:155),]

###Exposed
dist_matrixA_ExposedDepths=vegdist(ExposedDepths$CWM_SLA,method="manhattan")
pairwise.adonis2(dist_matrixA_ExposedDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = ExposedDepths) #significant site level

dist_matrixB_ExposedDepths=vegdist(ExposedDepths$FDis_SLA,method="manhattan")
pairwise.adonis2(dist_matrixB_ExposedDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = ExposedDepths) #significant depth and site level

###Semi
dist_matrixA_SemiDepths=vegdist(SemiDepths$CWM_SLA,method="manhattan")
pairwise.adonis2(dist_matrixA_SemiDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = SemiDepths) #significant depth and site level

dist_matrixB_SemiDepths=vegdist(SemiDepths$FDis_SLA,method="manhattan")
pairwise.adonis2(dist_matrixB_SemiDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = SemiDepths) #significant depth and site level (marginally)

###Pojo
dist_matrixA_PojoDepths=vegdist(PojoDepths$CWM_SLA,method="manhattan")
pairwise.adonis2(dist_matrixA_PojoDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PojoDepths) #significant depth and site level (marginally)

dist_matrixB_PojoDepths=vegdist(PojoDepths$FDis_SLA,method="manhattan")
pairwise.adonis2(dist_matrixB_PojoDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PojoDepths) #non-significant

##%######################################################%##
#                                                          #
####        Leaf area - perimeter ratio (LAP)          #####
#                                                          #
##%######################################################%##

######### Shallow sites #########

######### CWM
table(Shallow_Leaf$Exposure_Depth[c(1:61)])
dist_matrixA=vegdist(Shallow_Leaf$CWM_LAP[c(1:61)],method="manhattan")
pairwise.adonis2(dist_matrixA ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_Leaf[c(1:61),])

table(Shallow_Leaf$Exposure_Depth[c(32:61, 92:121)])
dist_matrixB=vegdist(Shallow_Leaf$CWM_LAP[c(32:61, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixB ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_Leaf[c(32:61, 92:121),])

table(Shallow_Leaf$Exposure_Depth[c(92:121, 62:91)])
dist_matrixC=vegdist(Shallow_Leaf$CWM_LAP[c(92:121, 62:91)],method="manhattan")
pairwise.adonis2(dist_matrixC ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_Leaf[c(92:121, 62:91),])

######### FDis

table(Shallow_Leaf$Exposure_Depth[c(1:61)])
dist_matrixA2=vegdist(Shallow_Leaf$FDis_LAP[c(1:61)],method="manhattan")
pairwise.adonis2(dist_matrixA2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_Leaf[c(1:61),])

table(Shallow_Leaf$Exposure_Depth[c(32:61, 92:121)])
dist_matrixB2=vegdist(Shallow_Leaf$FDis_LAP[c(32:61, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixB2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_Leaf[c(32:61, 92:121),])

table(Shallow_Leaf$Exposure_Depth[c(92:121, 62:91)])
dist_matrixC2=vegdist(Shallow_Leaf$FDis_LAP[c(92:121, 62:91)],method="manhattan")
pairwise.adonis2(dist_matrixC2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_Leaf[c(92:121, 62:91),])

table(Shallow_Leaf$Exposure_Depth[c(1:31, 92:121)])
dist_matrixD2=vegdist(Shallow_Leaf$FDis_LAP[c(1:31, 92:121)],method="manhattan")
pairwise.adonis2(dist_matrixD2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Shallow_Leaf[c(1:31, 92:121),])

######### Deep sites #########

######### CWM
table(Deep_Leaf$Exposure_Depth[c(1:26,27:56)])
dist_matrixAD=vegdist(Deep_Leaf$CWM_LAP[c(1:26,27:56)],method="manhattan")
pairwise.adonis2(dist_matrixAD ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_Leaf[c(1:26,27:56),])

table(Deep_Leaf$Exposure_Depth[c(27:64)])
dist_matrixBD=vegdist(Deep_Leaf$CWM_LAP[c(27:64)],method="manhattan")
pairwise.adonis2(dist_matrixBD ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_Leaf[c(27:64),])

######### FDis
table(Deep_Leaf$Exposure_Depth[c(1:26,27:56)])
dist_matrixBD=vegdist(Deep_Leaf$FDis_LAP[c(1:26,27:56)],method="manhattan")
pairwise.adonis2(dist_matrixBD ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_Leaf[c(1:26,27:56),])

table(Deep_Leaf$Exposure_Depth[c(27:64)])
dist_matrixBD2=vegdist(Deep_Leaf$FDis_LAP[c(27:64)],method="manhattan")
pairwise.adonis2(dist_matrixBD2 ~ Exposure + Site_Exposure, p.adj = "hochberg", data = Deep_Leaf[c(27:64),])

######### Between depths #########
ExposedDepths <-Indices_leaf_AllsitesB[c(1:57),]
SemiDepths <-Indices_leaf_AllsitesB[c(58:117),]
PojoDepths <-Indices_leaf_AllsitesB[c(118:155),]

#Exposed
dist_matrixC_ExposedDepths=vegdist(ExposedDepths$CWM_LAP,method="manhattan")
pairwise.adonis2(dist_matrixC_ExposedDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = ExposedDepths) #significant depth and site level

dist_matrixD_ExposedDepths=vegdist(ExposedDepths$FDis_LAP,method="manhattan")
pairwise.adonis2(dist_matrixD_ExposedDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = ExposedDepths) #significant depth and site level

###Semi
dist_matrixC_SemiDepths=vegdist(SemiDepths$CWM_LAP,method="manhattan")
pairwise.adonis2(dist_matrixC_SemiDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = SemiDepths) #significant site level

dist_matrixD_SemiDepths=vegdist(SemiDepths$FDis_LAP,method="manhattan")
pairwise.adonis2(dist_matrixD_SemiDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = SemiDepths) #significant depth and site level 

###Pojo
dist_matrixC_PojoDepths=vegdist(PojoDepths$CWM_LAP,method="manhattan")
pairwise.adonis2(dist_matrixC_PojoDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PojoDepths) #significant site level (marginally)

dist_matrixD_PojoDepths=vegdist(PojoDepths$FDis_LAP,method="manhattan")
pairwise.adonis2(dist_matrixD_PojoDepths ~ Depth + Site_Exposure, p.adj = "hochberg", 
                 data = PojoDepths) #significant site level (marginally)

