##%######################################################%##
#                                                          #
####              Update data file for stats            ####
#                                                          #
##%######################################################%##
Cstock_all$Exposure_Depth <- factor(paste(Cstock_all$Site_group, 
                                          Cstock_all$Depth, sep = "_"),
                                                 levels = c(
                                                   "exposed_Shallow", "exposed_Deep",
                                                   "Semi_Shallow", "Semi_Deep",
                                                   "Pojo_Shallow", "Pojo_Deep",
                                                   "Shel_Shallow","Shel_Deep"
                                                 ))

Cstock_all$Exposure_Depth <- factor(Cstock_all$Exposure_Depth , # Reorder factor levels
                                            c("exposed_Shallow", "exposed_Deep", "Semi_Shallow", "Semi_Deep", 
                                              "Shel_Shallow", "Shel_Deep", "Pojo_Shallow", "Pojo_Deep"))
##%######################################################%##
#                                                          #
####                  Normality tests                   ####
#                                                          #
##%######################################################%##
library(rstatix)

### Exposed_Deep
Exp_dp <- subset(Cstock_all, Organism=="Plant" & Exposure_Depth=="exposed_Deep")
Exp_da <-  subset(Cstock_all, Organism=="Algae" & Exposure_Depth=="exposed_Deep")

mshapiro_test(as.numeric(Exp_dp[,c(2)])) #  marginally significant for plants
mshapiro_test(as.numeric(Exp_dp[c(1:4),c(2)])) # non-significant for plants
mshapiro_test(as.numeric(Exp_da[,c(2)])) # significant for algae

### Exposed_Shallow
Exp_sp <- subset(Cstock_all, Organism=="Plant" & Exposure_Depth=="exposed_Shallow")
Exp_sa <-  subset(Cstock_all, Organism=="Algae" & Exposure_Depth=="exposed_Shallow")

mshapiro_test(as.numeric(Exp_sp[,c(2)])) # non-significant for plants
mshapiro_test(as.numeric(Exp_sa[,c(2)])) # significant for algae

### Semi_Deep
Semi_dp <- subset(Cstock_all, Organism=="Plant" & Exposure_Depth=="Semi_Deep")
Semi_da <-  subset(Cstock_all, Organism=="Algae" & Exposure_Depth=="Semi_Deep")

mshapiro_test(as.numeric(Semi_dp[,c(2)])) # significant for plants
mshapiro_test(as.numeric(Semi_da[,c(2)])) # all zero so no results for algae

### Semi_Shallow
Semi_sp <- subset(Cstock_all, Organism=="Plant" & Exposure_Depth=="Semi_Shallow")
Semi_sa <-  subset(Cstock_all, Organism=="Algae" & Exposure_Depth=="Semi_Shallow")

mshapiro_test(as.numeric(Semi_sp[,c(2)])) # non-significant for plants
mshapiro_test(as.numeric(Semi_sa[,c(2)])) # non-significant for algae

### Sheltered_Deep
Shel_dp <- subset(Cstock_all, Organism=="Plant" & Exposure_Depth=="Shel_Deep")
Shel_da <-  subset(Cstock_all, Organism=="Algae" & Exposure_Depth=="Shel_Deep")

mshapiro_test(as.numeric(Shel_dp[,c(2)])) # all zero so no results for plants
mshapiro_test(as.numeric(Shel_da[,c(2)])) # all zero so no results for algae

### Sheltered_Shallow
Shel_sp <- subset(Cstock_all, Organism=="Plant" & Exposure_Depth=="Shel_Shallow")
Shel_sa <-  subset(Cstock_all, Organism=="Algae" & Exposure_Depth=="Shel_Shallow")

mshapiro_test(as.numeric(Shel_sp[,c(2)])) # significant for plants
mshapiro_test(as.numeric(Shel_sa[,c(2)])) # significant for algae

### Pojo_Deep
Pojo_dp <- subset(Cstock_all, Organism=="Plant" & Exposure_Depth=="Pojo_Deep")
Pojo_da <-  subset(Cstock_all, Organism=="Algae" & Exposure_Depth=="Pojo_Deep")

mshapiro_test(as.numeric(Pojo_dp[,c(2)])) # significant for plants
mshapiro_test(as.numeric(Pojo_da[,c(2)])) # significant for algae

### Pojo_Shallow
Pojo_sp <- subset(Cstock_all, Organism=="Plant" & Exposure_Depth=="Pojo_Shallow")
Pojo_sa <-  subset(Cstock_all, Organism=="Algae" & Exposure_Depth=="Pojo_Shallow")

mshapiro_test(as.numeric(Pojo_sp[,c(2)])) # significant for plants
mshapiro_test(as.numeric(Pojo_sa[,c(2)])) # marginally significant for algae

# Conclusion non-parametric tests for both depths and algae/plants

##%######################################################%##
#                                                          #
####                  Homogeneity tests                 ####
#                                                          #
##%######################################################%##

Cstock_all$Cstock_gm2<- as.numeric(Cstock_all$Cstock_gm2) 

Algae_Shallow <- subset(Cstock_all, Organism=="Algae" & Depth=="Shallow")
Algae_Deep <- subset(Cstock_all, Organism=="Algae" & Depth=="Deep")
Plant_Shallow <- subset(Cstock_all, Organism=="Plant" & Depth=="Shallow")
Plant_Deep <- subset(Cstock_all, Organism=="Plant" & Depth=="Deep")

fligner.test(Algae_Shallow$Cstock_gm2 ~ Algae_Shallow$Exposure) # non-signifcant
fligner.test(Algae_Deep$Cstock_gm2 ~ Algae_Deep$Exposure) # non-signifcant

fligner.test(Plant_Shallow$Cstock_gm2 ~ Plant_Shallow$Exposure) # non - signifcant
fligner.test(Plant_Deep$Cstock_gm2 ~ Plant_Deep$Exposure) # signifcant 

# Conclusion to test for gradients within a depth, we can assume mostly homogeneity of variance

##%######################################################%##
#                                                          #
####            Tests within the same depth             ####
#                                                          #
##%######################################################%##

kruskal.test(Cstock_gm2 ~ Exposure_Depth, data = Plant_Shallow) # non-significant
kruskal.test(Cstock_gm2 ~ Exposure_Depth, data = Plant_Deep) # significant differences


############## Mann-Whitney U Post hoc test ##############

### Deep plant C stocks

Plant_Deep <- rbind(Exp_dp, Semi_dp, Shel_dp, Pojo_dp)

Plant_DeepA <- Plant_Deep[c(1:10),]
Plant_DeepA2 <- Plant_Deep[c(1:4, 6:10),]
Plant_DeepB <- Plant_Deep[c(6:15),] 
Plant_DeepC <- Plant_Deep[c(11:20),] 

A <-wilcox.test(Cstock_gm2 ~ Exposure_Depth, data = Plant_DeepA, paired = FALSE, alternative = "greater")
A # Exposed to semi are marginally significant lower

A2 <-wilcox.test(Cstock_gm2 ~ Exposure_Depth, data = Plant_DeepA2, paired = FALSE, alternative = "greater")
A2 # Exposed to semi are significant lower if we take the 0 outlier out

B <-wilcox.test(Cstock_gm2 ~ Exposure_Depth, data = Plant_DeepB, paired = FALSE, alternative = "greater")
B # semi to sheltered are significant lower

B2 <-wilcox.test(Cstock_gm2 ~ Exposure_Depth, data = Plant_DeepC, paired = FALSE, alternative = "greater")
B2 # Sheltered to Pojo are margnially significant lower


##%######################################################%##
#                                                          #
####          Tests between different depths            ####
#                                                          #
##%######################################################%##

Plant_Exposure <- subset(Cstock_all, Organism=="Plant" & Site_group=="exposed")
Plant_DeepExposure2 <- Plant_Exposure[-c(10),]
Plant_Semi <- subset(Cstock_all, Organism=="Plant" & Site_group=="Semi")
Plant_Sheltered <- subset(Cstock_all, Organism=="Plant" & Site_group=="Shel")
Plant_Pojo <- subset(Cstock_all, Organism=="Plant" & Site_group=="Pojo")

A <-wilcox.test(Cstock_gm2 ~ Depth, data = Plant_Exposure, paired = FALSE, alternative = "less")
A # No significant difference between depths

A2 <-wilcox.test(Cstock_gm2 ~ Depth, data = Plant_DeepExposure2, paired = FALSE, alternative = "greater")
A2 # Goes further away from significance when taking out the 0 value

B <-wilcox.test(Cstock_gm2 ~ Depth, data = Plant_Semi, paired = FALSE, alternative = "less")
B # Deep sites have significantly lower C stocks

B2 <-wilcox.test(Cstock_gm2 ~ Depth, data = Plant_Sheltered, paired = FALSE, alternative = "less")
B2 # Deep sites have significantly lower C stocks

C <-wilcox.test(Cstock_gm2 ~ Depth, data = Plant_Pojo, paired = FALSE, alternative = "less")
C # Deep sites have significantly lower C stocks
