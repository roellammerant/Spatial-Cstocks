##%######################################################%##
#                                                          #
####              Update data file for stats            ####
#                                                          #
##%######################################################%##

Abundance_all_deep$Depth <- c(rep("Deep",14))
Abundance_all_shallow$Depth <- c(rep("Shallow",15))
Abundance_all<- rbind.data.frame(Abundance_all_deep,Abundance_all_shallow)
Abundance_all$Exposure_Depth <- factor(paste(Abundance_all$Site_group, 
                                            Abundance_all$Depth, sep = "_"),
                                          levels = c(
                                            "exposed_Shallow", "exposed_Deep",
                                            "semi_Shallow", "semi_Deep",
                                            "Pojo_Shallow", "Pojo_Deep",
                                            "sheltered_Shallow","sheltered_Deep"
                                          ))

##%######################################################%##
#                                                          #
####                  Normality tests                   ####
#                                                          #
##%######################################################%##

### Exposed_Deep
Abundance_all[c(1:4),c(5)]
mshapiro_test(as.numeric(Abundance_all[c(1:4),c(2)])) # significant

### Exposed_Shallow
Abundance_all[c(15:19),c(5)]
mshapiro_test(as.numeric(Abundance_all[c(15:19),c(2)])) # non-significant

### Semi_Deep
Abundance_all[c(5:9),c(5)]
mshapiro_test(as.numeric(Abundance_all[c(5:9),c(2)])) # non-significant

### Semi_Shallow
Abundance_all[c(20:24),c(5)]
mshapiro_test(as.numeric(Abundance_all[c(20:24),c(2)])) # non-significant

### Sheltered_Deep
Abundance_all[c(10:14),c(5)]
mshapiro_test(as.numeric(Abundance_all[c(10:14),c(2)])) # No result all zero values

### Sheltered_Shallow
Abundance_all[c(25:29),c(5)]
mshapiro_test(as.numeric(Abundance_all[c(25:29),c(2)])) # non-significant

# Conclusion parametric test for shallow sites and non-parametric for deep sites

##%######################################################%##
#                                                          #
####                  Homogeneity tests                 ####
#                                                          #
##%######################################################%##

Abundance_all_deep$Individual_count_per_m2 <- as.numeric(Abundance_all_deep$Individual_count_per_m2) 
Abundance_all_shallow$Individual_count_per_m2 <- as.numeric(Abundance_all_shallow$Individual_count_per_m2) 

fligner.test(Abundance_all_shallow$Individual_count_per_m2 ~ Abundance_all_shallow$Site_group) # marginally signifcant
fligner.test(Abundance_all_deep$Individual_count_per_m2 ~ Abundance_all_deep$Site_group) # signifcant


# Conclusion to test for gradients within a depth, we should assume heteroscedasticity

##%######################################################%##
#                                                          #
####            Tests within the same depth             ####
#                                                          #
##%######################################################%##

kruskal.test(Individual_count_per_m2 ~ Site_group, data = Abundance_all_shallow) # significant differences
kruskal.test(Individual_count_per_m2 ~ Site_group, data = Abundance_all_deep) # significant differences

############## Mann-Whitney U Post hoc test ##############

### Ind Count deep sites

Plant_DeepA <- Abundance_all_deep[c(1:9),]
Plant_DeepA2 <- Abundance_all_deep[c(5:14),]

A <-wilcox.test(Individual_count_per_m2 ~ Site_group, data = Plant_DeepA, 
                paired = FALSE, alternative = "greater")
A # Exposed to semi decreases significantly

A2 <-wilcox.test(Individual_count_per_m2 ~ Site_group, data = Plant_DeepA2, 
                 paired = FALSE, alternative = "greater")
A2 # semi to sheltered decreases significantly

### Ind Count shallow sites

Plant_ShallowA <- Abundance_all_shallow[c(1:10),]
Plant_ShallowA2 <- Abundance_all_shallow[c(6:15),]

A <-wilcox.test(Individual_count_per_m2 ~ Site_group, data = Plant_ShallowA, 
                paired = FALSE, alternative = "greater")
A # Exposed to semi decreases significantly

A2 <-wilcox.test(Individual_count_per_m2 ~ Site_group, data = Plant_ShallowA2, 
                 paired = FALSE, alternative = "greater")
A2 # semi to sheltered decreases significantly

##%######################################################%##
#                                                          #
####          Tests between different depths            ####
#                                                          #
##%######################################################%##

Plant_Exposure <- Abundance_all[c(1:4, 15:19),]
Plant_Semi <- Abundance_all[c(5:9, 20:24),] 
Plant_Sheltered <- Abundance_all[c(10:14, 25:29),] 

A <-wilcox.test(Individual_count_per_m2 ~ Exposure_Depth, data = Plant_Exposure, 
                paired = FALSE, alternative = "greater")
A # Ind count decreases significantly from shallow to deeper depths

B <-wilcox.test(Individual_count_per_m2 ~ Exposure_Depth, data = Plant_Semi, 
                paired = FALSE, alternative = "greater")
B # Ind count decreases significantly from shallow to deeper depths

C <-wilcox.test(Individual_count_per_m2 ~ Exposure_Depth, data = Plant_Sheltered, 
                paired = FALSE, alternative = "greater")
C # Ind count decreases significantly from shallow to deeper depths

