##%######################################################%##
#                                                          #
####       Plotting Category versus site for height     ####
#                                                          #
##%######################################################%##

CategoryLevel_Height<- Indices_height_AllsitesB[c(1:117),]
CategoryLevel_Height$Exposure_Depth <- factor(CategoryLevel_Height$Exposure_Depth , # Reorder factor levels
                                                 c("Exposed_Shallow","Exposed_Deep",
                                                   "Semi_Shallow", "Semi_Deep"))
SiteLevel_Height<- Indices_height_Allsites[c(1:117),]
SiteLevel_Height$Exposure_Depth <- factor(SiteLevel_Height$Exposure_Depth , # Reorder factor levels
                                              c("Exposed_Shallow","Exposed_Deep",
                                                "Semi_Shallow", "Semi_Deep"))

png(
  "output_plot/Category VS Site for Height.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 300
)
par(mfcol=c(2,2))

par(mar = c(3, 5, 2, 3))

############## CWM ##############

boxplot(CWM_Height ~ Exposure_Depth, data = CategoryLevel_Height,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
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

############## FDis ##############

boxplot(FDis_Height ~ Exposure_Depth, data = CategoryLevel_Height,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
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

dev.off()

##%######################################################%##
#                                                          #
## Plotting Category versus site for root to shoot ratio  ##
#                                                          #
##%######################################################%##

CategoryLevel_Ratio<- Indices_Ratio_Allsites[c(1:117),]
CategoryLevel_Ratio$Exposure_Depth <- factor(CategoryLevel_Ratio$Exposure_Depth , # Reorder factor levels
                                              c("Exposed_Shallow","Exposed_Deep",
                                                "semi_Shallow", "semi_Deep"))
SiteLevel_Ratio<- Indices_Ratio_AllsitesB[c(1:117),]
SiteLevel_Ratio$Exposure_Depth <- factor(SiteLevel_Ratio$Exposure_Depth , # Reorder factor levels
                                          c("Exposed_Shallow","Exposed_Deep",
                                            "Semi_Shallow", "Semi_Deep"))

png(
  "output_plot/Category VS Site for R-S ratio.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 300
)
par(mfcol=c(2,2))

par(mar = c(3, 5, 2, 3))

############## CWM ##############

boxplot(CWM_Ratio ~ Exposure_Depth, data = CategoryLevel_Ratio,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "CWM at category level", at = c(1, 2, 4, 5),
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

legend("topleft", legend = c("Shallow", "Deep"), 
       col=c("bisque","azure2"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1,  horiz = F)

boxplot(CWM_Ratio ~ Exposure_Depth, data = SiteLevel_Ratio,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "CWM at site level", at = c(1, 2, 4, 5),
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

############## FDis ##############

boxplot(FDis_Ratio ~ Exposure_Depth, data = CategoryLevel_Ratio,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "FDis at category level", at = c(1, 2, 4, 5),
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
        xlab = NA, ylab = "FDis at site level", at = c(1, 2, 4, 5),
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

dev.off()


##%######################################################%##
#                                                          #
##     Plotting Category versus site for root depth       ##
#                                                          #
##%######################################################%##

CategoryLevel_RootDepth<- Indices_RootDepth_Allsites[c(1:117),]
CategoryLevel_RootDepth$Exposure_Depth <- factor(CategoryLevel_RootDepth$Exposure_Depth , # Reorder factor levels
                                             c("Exposed_Shallow","Exposed_Deep",
                                               "semi_Shallow", "semi_Deep"))
SiteLevel_RootDepth<- Indices_RootDepth_AllsitesB[c(1:117),]
SiteLevel_RootDepth$Exposure_Depth <- factor(SiteLevel_RootDepth$Exposure_Depth , # Reorder factor levels
                                         c("Exposed_Shallow","Exposed_Deep",
                                           "Semi_Shallow", "Semi_Deep"))

png(
  "output_plot/Category VS Site for Root depth.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 300
)
par(mfcol=c(2,2))

par(mar = c(3, 5, 2, 3))

############## CWM ##############

boxplot(CWM_RootDepth ~ Exposure_Depth, data = CategoryLevel_RootDepth,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "CWM at category level", at = c(1, 2, 4, 5),
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

legend("topright", legend = c("Shallow", "Deep"), 
       col=c("bisque","azure2"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1,  horiz = F)

boxplot(CWM_RootDepth ~ Exposure_Depth, data = SiteLevel_RootDepth,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "CWM at site level", at = c(1, 2, 4, 5),
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

############## FDis ##############

boxplot(FDis_RootDepth ~ Exposure_Depth, data = CategoryLevel_RootDepth,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "FDis at category level", at = c(1, 2, 4, 5),
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
        xlab = NA, ylab = "FDis at site level", at = c(1, 2, 4, 5),
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

dev.off()

##%######################################################%##
#                                                          #
##         Plotting Category versus site for SLA          ##
#                                                          #
##%######################################################%##

CategoryLevel_SLA<- Indices_leaf_Allsites[c(1:117),]
CategoryLevel_SLA$Exposure_Depth <- factor(CategoryLevel_SLA$Exposure_Depth , # Reorder factor levels
                                                 c("Exposed_Shallow","Exposed_Deep",
                                                   "semi_Shallow", "semi_Deep"))
SiteLevel_SLA<- Indices_leaf_AllsitesB[c(1:117),]
SiteLevel_SLA$Exposure_Depth <- factor(SiteLevel_SLA$Exposure_Depth , # Reorder factor levels
                                             c("Exposed_Shallow","Exposed_Deep",
                                               "Semi_Shallow", "Semi_Deep"))

png(
  "output_plot/Category VS Site for SLA.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 300
)
par(mfcol=c(2,2))

par(mar = c(3, 5, 2, 3))

############## CWM ##############

boxplot(CWM_SLA ~ Exposure_Depth, data = CategoryLevel_SLA,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "CWM at category level", at = c(1, 2, 4, 5),
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

legend("topleft", legend = c("Shallow", "Deep"), 
       col=c("bisque","azure2"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1,  horiz = F)

boxplot(CWM_SLA ~ Exposure_Depth, data = SiteLevel_SLA,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "CWM at site level", at = c(1, 2, 4, 5),
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

############## FDis ##############

boxplot(FDis_SLA ~ Exposure_Depth, data = CategoryLevel_SLA,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "FDis at category level", at = c(1, 2, 4, 5),
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
        xlab = NA, ylab = "FDis at site level", at = c(1, 2, 4, 5),
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

dev.off()

##%######################################################%##
#                                                          #
##         Plotting Category versus site for LAP          ##
#                                                          #
##%######################################################%##

CategoryLevel_LAP<- Indices_leaf_Allsites[c(1:117),]
CategoryLevel_LAP$Exposure_Depth <- factor(CategoryLevel_LAP$Exposure_Depth , # Reorder factor levels
                                           c("Exposed_Shallow","Exposed_Deep",
                                             "semi_Shallow", "semi_Deep"))
SiteLevel_LAP<- Indices_leaf_AllsitesB[c(1:117),]
SiteLevel_LAP$Exposure_Depth <- factor(SiteLevel_LAP$Exposure_Depth , # Reorder factor levels
                                       c("Exposed_Shallow","Exposed_Deep",
                                         "Semi_Shallow", "Semi_Deep"))

png(
  "output_plot/Category VS Site for LAP.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 300
)
par(mfcol=c(2,2))

par(mar = c(3, 5, 2, 3))

############## CWM ##############

boxplot(CWM_LAP ~ Exposure_Depth, data = CategoryLevel_LAP,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "CWM at category level", at = c(1, 2, 4, 5),
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

legend("topleft", legend = c("Shallow", "Deep"), 
       col=c("bisque","azure2"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1,  horiz = F)

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

############## FDis ##############

boxplot(FDis_LAP ~ Exposure_Depth, data = CategoryLevel_LAP,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "FDis at category level", at = c(1, 2, 4, 5),
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
        xlab = NA, ylab = "FDis at site level", at = c(1, 2, 4, 5),
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

