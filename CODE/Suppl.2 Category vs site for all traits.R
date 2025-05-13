CategoryLevel_Height<- Indices_height_AllsitesB[c(1:117),]
CategoryLevel_Height$CWM_Height <- as.numeric(CategoryLevel_Height$CWM_Height)
CategoryLevel_Height$FDis_Height <- as.numeric(CategoryLevel_Height$FDis_Height)
CategoryLevel_Height$Exposure_Depth <- factor(CategoryLevel_Height$Exposure_Depth , # Reorder factor levels
                                              c("Exposed_Shallow","Exposed_Deep",
                                                "Semi_Shallow", "Semi_Deep"))
SiteLevel_Height<- Indices_height_Allsites[c(1:117),]
SiteLevel_Height$CWM_Height <- as.numeric(SiteLevel_Height$CWM_Height)
SiteLevel_Height$FDis_Height <- as.numeric(SiteLevel_Height$FDis_Height)
SiteLevel_Height$Exposure_Depth <- factor(SiteLevel_Height$Exposure_Depth , # Reorder factor levels
                                          c("Exposed_Shallow","Exposed_Deep",
                                            "Semi_Shallow", "Semi_Deep"))

CategoryLevel_Ratio<- Indices_Ratio_Allsites[c(1:117),]
CategoryLevel_Ratio$CWM_Ratio <- as.numeric(CategoryLevel_Ratio$CWM_Ratio)
CategoryLevel_Ratio$FDis_Ratio <- as.numeric(CategoryLevel_Ratio$FDis_Ratio)
CategoryLevel_Ratio$Exposure_Depth <- factor(CategoryLevel_Ratio$Exposure_Depth , # Reorder factor levels
                                             c("Exposed_Shallow","Exposed_Deep",
                                               "semi_Shallow", "semi_Deep"))
SiteLevel_Ratio<- Indices_Ratio_AllsitesB[c(1:117),]
SiteLevel_Ratio$CWM_Ratio <- as.numeric(SiteLevel_Ratio$CWM_Ratio)
SiteLevel_Ratio$FDis_Ratio <- as.numeric(SiteLevel_Ratio$FDis_Ratio)
SiteLevel_Ratio$Exposure_Depth <- factor(SiteLevel_Ratio$Exposure_Depth , # Reorder factor levels
                                         c("Exposed_Shallow","Exposed_Deep",
                                           "Semi_Shallow", "Semi_Deep"))

CategoryLevel_RootDepth<- Indices_RootDepth_Allsites[c(1:117),]
CategoryLevel_RootDepth$CWM_RootDepth <- as.numeric(CategoryLevel_RootDepth$CWM_RootDepth)
CategoryLevel_RootDepth$FDis_RootDepth <- as.numeric(CategoryLevel_RootDepth$FDis_RootDepth)
CategoryLevel_RootDepth$Exposure_Depth <- factor(CategoryLevel_RootDepth$Exposure_Depth , # Reorder factor levels
                                                 c("Exposed_Shallow","Exposed_Deep",
                                                   "semi_Shallow", "semi_Deep"))
SiteLevel_RootDepth<- Indices_RootDepth_AllsitesB[c(1:117),]
SiteLevel_RootDepth$CWM_RootDepth <- as.numeric(SiteLevel_RootDepth$CWM_RootDepth)
SiteLevel_RootDepth$FDis_RootDepth <- as.numeric(SiteLevel_RootDepth$FDis_RootDepth)
SiteLevel_RootDepth$Exposure_Depth <- factor(SiteLevel_RootDepth$Exposure_Depth , # Reorder factor levels
                                             c("Exposed_Shallow","Exposed_Deep",
                                               "Semi_Shallow", "Semi_Deep"))

CategoryLevel_SLA<- Indices_leaf_Allsites[c(1:117),]
CategoryLevel_SLA$CWM_SLA <- as.numeric(CategoryLevel_SLA$CWM_SLA)
CategoryLevel_SLA$FDis_SLA <- as.numeric(CategoryLevel_SLA$FDis_SLA)
CategoryLevel_SLA$Exposure_Depth <- factor(CategoryLevel_SLA$Exposure_Depth , # Reorder factor levels
                                           c("Exposed_Shallow","Exposed_Deep",
                                             "semi_Shallow", "semi_Deep"))
SiteLevel_SLA<- Indices_leaf_AllsitesB[c(1:117),]
SiteLevel_SLA$CWM_SLA <- as.numeric(SiteLevel_SLA$CWM_SLA)
SiteLevel_SLA$FDis_SLA <- as.numeric(SiteLevel_SLA$FDis_SLA)
SiteLevel_SLA$Exposure_Depth <- factor(SiteLevel_SLA$Exposure_Depth , # Reorder factor levels
                                       c("Exposed_Shallow","Exposed_Deep",
                                         "Semi_Shallow", "Semi_Deep"))

CategoryLevel_LAP<- Indices_leaf_Allsites[c(1:117),]
CategoryLevel_LAP$CWM_LAP <- as.numeric(CategoryLevel_LAP$CWM_LAP)
CategoryLevel_LAP$FDis_LAP <- as.numeric(CategoryLevel_LAP$FDis_LAP)
CategoryLevel_LAP$Exposure_Depth <- factor(CategoryLevel_LAP$Exposure_Depth , # Reorder factor levels
                                           c("Exposed_Shallow","Exposed_Deep",
                                             "semi_Shallow", "semi_Deep"))
SiteLevel_LAP<- Indices_leaf_AllsitesB[c(1:117),]
SiteLevel_LAP$CWM_LAP <- as.numeric(SiteLevel_LAP$CWM_LAP)
SiteLevel_LAP$FDis_LAP <- as.numeric(SiteLevel_LAP$FDis_LAP)
SiteLevel_LAP$Exposure_Depth <- factor(SiteLevel_LAP$Exposure_Depth , # Reorder factor levels
                                       c("Exposed_Shallow","Exposed_Deep",
                                         "Semi_Shallow", "Semi_Deep"))


##%######################################################%##
#                                                          #
####       Plotting Category versus site for cwm        ####
#                                                          #
##%######################################################%##

png(
  "output_plot/SupplementaL_Category VS Site CWM all.jpg",
  width = 16,
  height = 8,
  units = 'in',
  res = 600
)
par(mfcol=c(2,5))

par(mar = c(3, 4, 2, 3))

############## Height ##############

boxplot(CWM_Height ~ Exposure_Depth, data = CategoryLevel_Height,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = "Height",
        xlab = NA, ylab = "CWM at category level", at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(CWM_Height ~ Exposure_Depth,
           data = CategoryLevel_Height,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered")

axis(1, 
     at = seq(1.50 , 5 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)

legend("topleft", legend = c("Shallow", "Deep"), 
       col=c("bisque","azure2"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1,  horiz = F)

boxplot(CWM_Height ~ Exposure_Depth, data = SiteLevel_Height,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "CWM at site level", at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(CWM_Height ~ Exposure_Depth,
           data = SiteLevel_Height,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered")

axis(1, 
     at = seq(1.5 , 5 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)

############## Ratio ##############

boxplot(CWM_Ratio ~ Exposure_Depth, data = CategoryLevel_Ratio,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = "R-S Ratio",
        xlab = NA, ylab = NA, at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2, ylim = c(0, 1.2))

stripchart(CWM_Ratio ~ Exposure_Depth,
           data = CategoryLevel_Ratio,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered")

axis(1, 
     at = seq(1.50 , 5 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)

boxplot(CWM_Ratio ~ Exposure_Depth, data = SiteLevel_Ratio,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = NA, at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2, ylim = c(0, 1.2))

stripchart(CWM_Ratio ~ Exposure_Depth,
           data = SiteLevel_Ratio,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered")

axis(1, 
     at = seq(1.5 , 5 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)


############## RootDepth ##############

boxplot(CWM_RootDepth ~ Exposure_Depth, data = CategoryLevel_RootDepth,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = "Root depth",
        xlab = NA, ylab = NA, at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2, ylim = c(0, 40))

stripchart(CWM_RootDepth ~ Exposure_Depth,
           data = CategoryLevel_RootDepth,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered")

axis(1, 
     at = seq(1.50 , 5 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)

boxplot(CWM_RootDepth ~ Exposure_Depth, data = SiteLevel_RootDepth,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = NA, at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2, ylim = c(0, 40))

stripchart(CWM_RootDepth ~ Exposure_Depth,
           data = SiteLevel_RootDepth,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered")

axis(1, 
     at = seq(1.5 , 5 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)

############## SLA ##############

boxplot(CWM_SLA ~ Exposure_Depth, data = CategoryLevel_SLA,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = "SLA",
        xlab = NA, ylab = NA, at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2, ylim = c(0, 1.2))

stripchart(CWM_SLA ~ Exposure_Depth,
           data = CategoryLevel_SLA,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered")

axis(1, 
     at = seq(1.50 , 5 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)

boxplot(CWM_SLA ~ Exposure_Depth, data = SiteLevel_SLA,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = NA, at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2, ylim = c(0, 1.2))

stripchart(CWM_SLA ~ Exposure_Depth,
           data = SiteLevel_SLA,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered")

axis(1, 
     at = seq(1.5 , 5 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)

############## LAP ##############

boxplot(CWM_LAP ~ Exposure_Depth, data = CategoryLevel_LAP,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = "LAP",
        xlab = NA, ylab = NA, at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2, ylim = c(0, 0.8))

stripchart(CWM_LAP ~ Exposure_Depth,
           data = CategoryLevel_LAP,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered")

axis(1, 
     at = seq(1.50 , 5 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)

boxplot(CWM_LAP ~ Exposure_Depth, data = SiteLevel_LAP,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "CWM at site level", at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2, ylim = c(0, 0.8))

stripchart(CWM_LAP ~ Exposure_Depth,
           data = SiteLevel_LAP,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered")

axis(1, 
     at = seq(1.5 , 5 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)


dev.off()



##%######################################################%##
#                                                          #
####       Plotting Category versus site for FDis       ####
#                                                          #
##%######################################################%##

png(
  "output_plot/Suppelemental_Category VS Site FDis all.jpg",
  width = 16,
  height = 8,
  units = 'in',
  res = 600
)
par(mfcol=c(2,5))

par(mar = c(3, 4, 2, 2))

############## Height ##############

boxplot(FDis_Height ~ Exposure_Depth, data = CategoryLevel_Height,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = "Height",
        xlab = NA, ylab = "FDis at category level", at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(FDis_Height ~ Exposure_Depth,
           data = CategoryLevel_Height,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered")

axis(1, 
     at = seq(1.50 , 5 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)

legend("topleft", legend = c("Shallow", "Deep"), 
       col=c("bisque","azure2"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1,  horiz = F)

boxplot(FDis_Height ~ Exposure_Depth, data = SiteLevel_Height,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "FDis at site level", at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(FDis_Height ~ Exposure_Depth,
           data = SiteLevel_Height,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered")

axis(1, 
     at = seq(1.5 , 5 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)


############## Ratio ##############

boxplot(FDis_Ratio ~ Exposure_Depth, data = CategoryLevel_Ratio,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = "R-S Ratio",
        xlab = NA, ylab = NA, at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2, ylim = c(0, 1.4))

stripchart(FDis_Ratio ~ Exposure_Depth,
           data = CategoryLevel_Ratio,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered")

axis(1, 
     at = seq(1.50 , 5 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)


boxplot(FDis_Ratio ~ Exposure_Depth, data = SiteLevel_Ratio,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = NA, at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2, ylim = c(0, 1.4))

stripchart(FDis_Ratio ~ Exposure_Depth,
           data = SiteLevel_Ratio,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered")

axis(1, 
     at = seq(1.5 , 5 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)

############## RootDepth ##############

boxplot(FDis_RootDepth ~ Exposure_Depth, data = CategoryLevel_RootDepth,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = "Root Depth",
        xlab = NA, ylab = NA, at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2, ylim = c(0, 1.4))

stripchart(FDis_RootDepth ~ Exposure_Depth,
           data = CategoryLevel_RootDepth,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered")

axis(1, 
     at = seq(1.50 , 5 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)


boxplot(FDis_RootDepth ~ Exposure_Depth, data = SiteLevel_RootDepth,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = NA, at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2, ylim = c(0, 1.4))

stripchart(FDis_RootDepth ~ Exposure_Depth,
           data = SiteLevel_RootDepth,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered")

axis(1, 
     at = seq(1.5 , 5 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)


############## SLA ##############

boxplot(FDis_SLA ~ Exposure_Depth, data = CategoryLevel_SLA,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = "SLA",
        xlab = NA, ylab = NA, at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2, ylim = c(0, 1.4))

stripchart(FDis_SLA ~ Exposure_Depth,
           data = CategoryLevel_SLA,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered")

axis(1, 
     at = seq(1.50 , 5 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)


boxplot(FDis_SLA ~ Exposure_Depth, data = SiteLevel_SLA,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = NA, at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2, ylim = c(0, 1.4))

stripchart(FDis_SLA ~ Exposure_Depth,
           data = SiteLevel_SLA,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered")

axis(1, 
     at = seq(1.5 , 5 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)

############## LAP ##############

boxplot(FDis_LAP ~ Exposure_Depth, data = CategoryLevel_LAP,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = "LAP",
        xlab = NA, ylab = NA, at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2, ylim = c(0, 1.4))

stripchart(FDis_LAP ~ Exposure_Depth,
           data = CategoryLevel_LAP,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered")

axis(1, 
     at = seq(1.50 , 5 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)


boxplot(FDis_LAP ~ Exposure_Depth, data = SiteLevel_LAP,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = NA, at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2, ylim = c(0, 1.4))

stripchart(FDis_LAP ~ Exposure_Depth,
           data = SiteLevel_LAP,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered")

axis(1, 
     at = seq(1.5 , 5 , 3), 
     labels = label , 
     tick=FALSE , cex=1.2)


dev.off()

