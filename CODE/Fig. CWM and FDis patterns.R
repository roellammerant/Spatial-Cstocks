##%######################################################%##
#                                                          #
####        Plotting Community weighted mean            ####
#                                                          #
##%######################################################%##

####Height
Indices_height_Allsites$CWM_Height <- as.numeric(Indices_height_Allsites$CWM_Height)
Indices_height_Allsites$FDis_Height <- as.numeric(Indices_height_Allsites$FDis_Height)
Indices_height_Allsites$Exposure_Depth <- factor(Indices_height_Allsites$Exposure_Depth , # Reorder factor levels
                                                 c("Exposed_Shallow","Exposed_Deep",
                                                   "Semi_Shallow", "Semi_Deep", 
                                                   "Sheltered_Shallow",
                                                   "Pojo_Shallow", "Pojo_Deep"))
####Root depth

Indices_RootDepth_AllsitesB$CWM_RootDepth <- as.numeric(Indices_RootDepth_AllsitesB$CWM_RootDepth)
Indices_RootDepth_AllsitesB$FDis_RootDepth <- as.numeric(Indices_RootDepth_AllsitesB$FDis_RootDepth)
Indices_RootDepth_AllsitesB$Exposure_Depth <- factor(Indices_RootDepth_AllsitesB$Exposure_Depth , # Reorder factor levels
                                                 c("Exposed_Shallow","Exposed_Deep",
                                                   "Semi_Shallow", "Semi_Deep", 
                                                   "Sheltered_Shallow",
                                                   "Pojo_Shallow", "Pojo_Deep"))

####R-s ratio

Indices_Ratio_AllsitesB$CWM_Ratio <- as.numeric(Indices_Ratio_AllsitesB$CWM_Ratio)
Indices_Ratio_AllsitesB$FDis_Ratio <- as.numeric(Indices_Ratio_AllsitesB$FDis_Ratio)
Indices_Ratio_AllsitesB$Exposure_Depth <- factor(Indices_Ratio_AllsitesB$Exposure_Depth , # Reorder factor levels
                                                 c("Exposed_Shallow","Exposed_Deep",
                                                   "Semi_Shallow", "Semi_Deep", 
                                                   "Sheltered_Shallow",
                                                   "Pojo_Shallow", "Pojo_Deep"))

####Leaf

Indices_leaf_AllsitesB$CWM_SLA <- as.numeric(Indices_leaf_AllsitesB$CWM_SLA)
Indices_leaf_AllsitesB$CWM_LAP <- as.numeric(Indices_leaf_AllsitesB$CWM_LAP)
Indices_leaf_AllsitesB$FDis_SLA <- as.numeric(Indices_leaf_AllsitesB$FDis_SLA)
Indices_leaf_AllsitesB$FDis_LAP <- as.numeric(Indices_leaf_AllsitesB$FDis_LAP)
Indices_leaf_AllsitesB$Exposure_Depth <- factor(Indices_leaf_AllsitesB$Exposure_Depth , # Reorder factor levels
                                                 c("Exposed_Shallow","Exposed_Deep",
                                                   "Semi_Shallow", "Semi_Deep", 
                                                   "Sheltered_Shallow",
                                                   "Pojo_Shallow", "Pojo_Deep"))



png(
  "output_plot/MainFigure_CWM_All_Indices.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 600
)
par(mfcol=c(3,2))

par(mar = c(3, 5, 2, 3))

############## height ##############

boxplot(CWM_Height ~ Exposure_Depth, data = Indices_height_Allsites,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "CWM of Height",at = c(1, 2, 4, 5, 7, 9, 10),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(CWM_Height ~ Exposure_Depth,
           data = Indices_height_Allsites,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5, 6.5,8.5,9.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(1.80 , 10 , 2.5), 
     labels = label , 
     tick=FALSE , cex=1.2)

legend("topleft", legend = c("Shallow", "Deep"), 
       col=c("bisque","azure2"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1,  horiz = F)

############## root to shoot ratio ##############

boxplot(CWM_Ratio ~ Exposure_Depth, data = Indices_Ratio_AllsitesB,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "CWM of R:S ratio",at = c(1, 2, 4, 5, 7, 9, 10),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(CWM_Ratio ~ Exposure_Depth,
           data = Indices_Ratio_AllsitesB,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5, 6.5,8.5,9.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(1.80 , 10 , 2.5), 
     labels = label , 
     tick=FALSE , cex=1.2)

############## root depth ##############

boxplot(CWM_RootDepth ~ Exposure_Depth, data = Indices_RootDepth_AllsitesB,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "CWM of Root depth",at = c(1, 2, 4, 5, 7, 9, 10),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(CWM_RootDepth ~ Exposure_Depth,
           data = Indices_RootDepth_AllsitesB,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5, 6.5,8.5,9.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(1.80 , 10 , 2.5), 
     labels = label , 
     tick=FALSE , cex=1.2)

############## SLA ##############

boxplot(CWM_SLA ~ Exposure_Depth, data = Indices_leaf_AllsitesB,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "CWM of SLA",at = c(1, 2, 4, 5, 7, 9, 10),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(CWM_SLA ~ Exposure_Depth,
           data = Indices_leaf_AllsitesB,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5, 6.5,8.5,9.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(1.80 , 10 , 2.5), 
     labels = label , 
     tick=FALSE , cex=1.2)

############## LAP ##############

boxplot(CWM_LAP ~ Exposure_Depth, data = Indices_leaf_AllsitesB,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "CWM of LAP",at = c(1, 2, 4, 5, 7, 9, 10),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(CWM_LAP ~ Exposure_Depth,
           data = Indices_leaf_AllsitesB,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5, 6.5,8.5,9.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(1.80 , 10 , 2.5), 
     labels = label , 
     tick=FALSE , cex=1.2)

dev.off()

##%######################################################%##
#                                                          #
####           Plotting functional dispersion           ####
#                                                          #
##%######################################################%##

png(
  "output_plot/MainFigure_FDis_All_Indices.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 600
)
par(mfcol=c(3,2))

par(mar = c(3, 5, 2, 3))

############## height ##############

boxplot(FDis_Height ~ Exposure_Depth, data = Indices_height_Allsites,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "FDis of Height",at = c(1, 2, 4, 5, 7, 9, 10),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(FDis_Height ~ Exposure_Depth,
           data = Indices_height_Allsites,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5, 6.5,8.5,9.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(1.80 , 10 , 2.5), 
     labels = label , 
     tick=FALSE , cex=1.2)

legend("topleft", legend = c("Shallow", "Deep"), 
       col=c("bisque","azure2"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1,  horiz = F)

############## root to shoot ratio ##############

boxplot(FDis_Ratio ~ Exposure_Depth, data = Indices_Ratio_AllsitesB,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "FDis of R:S ratio",at = c(1, 2, 4, 5, 7, 9, 10),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(FDis_Ratio ~ Exposure_Depth,
           data = Indices_Ratio_AllsitesB,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5, 6.5,8.5,9.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(1.80 , 10 , 2.5), 
     labels = label , 
     tick=FALSE , cex=1.2)

############## root depth ##############

boxplot(FDis_RootDepth ~ Exposure_Depth, data = Indices_RootDepth_AllsitesB,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "FDis of Root depth",at = c(1, 2, 4, 5, 7, 9, 10),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(FDis_RootDepth ~ Exposure_Depth,
           data = Indices_RootDepth_AllsitesB,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5, 6.5,8.5,9.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(1.80 , 10 , 2.5), 
     labels = label , 
     tick=FALSE , cex=1.2)

############## SLA ##############

boxplot(FDis_SLA ~ Exposure_Depth, data = Indices_leaf_AllsitesB,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "FDis of SLA",at = c(1, 2, 4, 5, 7, 9, 10),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(FDis_SLA ~ Exposure_Depth,
           data = Indices_leaf_AllsitesB,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5, 6.5,8.5,9.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(1.80 , 10 , 2.5), 
     labels = label , 
     tick=FALSE , cex=1.2)

############## LAP ##############

boxplot(FDis_LAP ~ Exposure_Depth, data = Indices_leaf_AllsitesB,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "FDis of LAP",at = c(1, 2, 4, 5, 7, 9, 10),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(FDis_LAP ~ Exposure_Depth,
           data = Indices_leaf_AllsitesB,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5, 6.5,8.5,9.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-sheltered", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(1.80 , 10 , 2.5), 
     labels = label , 
     tick=FALSE , cex=1.2)

dev.off()

