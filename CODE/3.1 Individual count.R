##%######################################################%##
#                                                          #
####       Boxplot abundance by individual count        ####
#                                                          #
##%######################################################%##

Spatial_Campaign_data_exp <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                        sheet = "Exposed", na = "NA")
Spatial_Campaign_data_semi <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Semi", na = "NA")
Spatial_Campaign_data_shel <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Sheltered", na = "NA")

Exposed_data <- Spatial_Campaign_data_exp[,c(1:6)]
Semi_data <-Spatial_Campaign_data_semi[,c(1:6)]
Shel_data <-Spatial_Campaign_data_shel[-c(1:14,17),c(1:6)]

###############################################
##        Total abundance per quadrat        ##
###############################################

Abundance_exp_quad <- aggregate(x=Exposed_data$`Individual count`,
                                by=list(Exposed_data$Site,Exposed_data$`Site number`), FUN=sum)
colnames(Abundance_exp_quad) <- c("Site","Site number","Individual count")
Abundance_exp_quad$Individual_count_m2 <- Abundance_exp_quad$`Individual count`/(0.27*0.27)
Abundance_exp_quad_Deep <- Abundance_exp_quad[-c(9:16,23:27,34:39,46:57),]
Abundance_exp_quad_Shallow <-Abundance_exp_quad[c(9:16,23:27,34:39,46:57),]

Abundance_semi_quad <- aggregate(x=Semi_data$`Individual count`,
                                 by=list(Semi_data$Site,Semi_data$`Site number`), FUN=sum)
colnames(Abundance_semi_quad) <- c("Site","Site number","Individual count")
Abundance_semi_quad$Individual_count_m2 <- Abundance_semi_quad$`Individual count`/(0.27*0.27)
Abundance_semi_quad_Deep <- Abundance_semi_quad[-c(7:12,19:24,31:36,43:48,55:60),]
Abundance_semi_quad_Shallow <-Abundance_semi_quad[c(7:12,19:24,31:36,43:48,55:60),]

Abundance_shel_quad_Shallow <- aggregate(x=Shel_data$`Individual count`,
                                         by=list(Shel_data$Site,Shel_data$`Site number`), FUN=sum)
colnames(Abundance_shel_quad_Shallow) <- c("Site","Site number","Individual count")
Abundance_shel_quad_Shallow$Individual_count_m2 <- Abundance_shel_quad_Shallow$`Individual count`/(0.27*0.27)


###############################################
##          mean abundance deep sites        ##
###############################################

Abundance_exp_deep <- aggregate(x=Abundance_exp_quad_Deep$Individual_count_m2, by=list(Abundance_exp_quad_Deep$`Site number`), FUN=mean)
colnames(Abundance_exp_deep)<-c("Site_number","Individual_count_per_m2")
Site_group = c("exposed", "exposed", "exposed","exposed")
Abundance_exp_deep <- cbind(Abundance_exp_deep,Site_group)


Abundance_semi_deep <- aggregate(x=Abundance_semi_quad_Deep$Individual_count_m2, by=list(Abundance_semi_quad_Deep$`Site number`), FUN=mean)
colnames(Abundance_semi_deep)<-c("Site_number","Individual_count_per_m2")
Site_group = c("semi", "semi", "semi","semi","semi")
Abundance_semi_deep <- cbind(Abundance_semi_deep,Site_group)

Abundance_shel_deep <-  Abundance_semi_deep
Abundance_shel_deep$Individual_count_per_m2 <- 0
Abundance_shel_deep$Site_group <- "sheltered"

Abundance_all_deep <-rbind(Abundance_exp_deep,Abundance_semi_deep, Abundance_shel_deep)

###############################################
##        mean abundance shallow sites       ##
###############################################

Abundance_exp_shallow <- aggregate(x=Abundance_exp_quad_Shallow$Individual_count_m2, 
                                   by=list(Abundance_exp_quad_Shallow$`Site number`), FUN=mean)
colnames(Abundance_exp_shallow)<-c("Site_number","Individual_count_per_m2")
Site_group = c("exposed", "exposed", "exposed","exposed","exposed")
Abundance_exp_shallow <- cbind(Abundance_exp_shallow,Site_group)


Abundance_semi_shallow <- aggregate(x=Abundance_semi_quad_Shallow$Individual_count_m2, 
                                    by=list(Abundance_semi_quad_Shallow$`Site number`), FUN=mean)
colnames(Abundance_semi_shallow)<-c("Site_number","Individual_count_per_m2")
Site_group = c("semi", "semi", "semi","semi","semi")
Abundance_semi_shallow<- cbind(Abundance_semi_shallow,Site_group)

Abundance_shel_shallow <- aggregate(x=Abundance_shel_quad_Shallow$Individual_count_m2,
                                    by=list(Abundance_shel_quad_Shallow$`Site number`),FUN=mean)
colnames(Abundance_shel_shallow) <-c("Site_number","Individual_count_per_m2")
Site_group= c("sheltered","sheltered","sheltered","sheltered","sheltered")
Abundance_shel_shallow <- cbind(Abundance_shel_shallow, Site_group)

Abundance_all_shallow <-rbind(Abundance_exp_shallow,Abundance_semi_shallow, Abundance_shel_shallow)

##########################################################################
######### Boxplot of mean data from both depths in same figure ##########
##########################################################################

##depth column and combining data frames
Abundance_all_deep$Depth <- c(rep("Deep",14))
Abundance_all_shallow$Depth <- c(rep("Shallow",15))
Abundance_All <- rbind(Abundance_all_deep, Abundance_all_shallow)
Abundance_All$Site_Depth <- factor(
  paste(Abundance_All$Site_group, Abundance_All$Depth, sep = "_"),
  levels = c(
    "exposed_Deep", "exposed_Shallow",
    "semi_Deep", "semi_Shallow",
    "sheltered_Deep", "sheltered_Shallow"
  )
)


Abundance_All$Site_Depth <- factor(Abundance_All$Site_Depth , # Reorder factor levels
                                                  c("exposed_Shallow", "exposed_Deep", "semi_Shallow", "semi_Deep", 
                                                    "sheltered_Shallow", "sheltered_Deep"))

png(
  "output_plot/Supplemental_IndividualCount.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 900
)

par(mfcol=c(1,1))

par(mar = c(3, 5, 2, 3))

boxplot(Individual_count_per_m2 ~ Site_Depth, data = Abundance_All,
        boxwex = 0.5, col = c("bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "Individual count m-2", ylim = c(0, 5500),at = c(1, 2, 4, 5, 7, 8),
        lex.order = TRUE, xaxt="n",cex.lab = 1.4)

stripchart(Individual_count_per_m2 ~ Site_Depth,
           data = Abundance_All,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5, 6.5,7.5),
           pch = 17,
           col = c("bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered", "Sheltered")

axis(1, 
     at = seq(1.5 , 8 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)

legend("topright", legend = c("Shallow", "Deep"), 
       col=c("bisque","azure2"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1.4,  horiz = F)

dev.off()

