# Run files 7.1A and 7.1B again


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

CategoryLevel_RootDepth$CWM_RootDepth <- as.numeric(CategoryLevel_RootDepth$CWM_RootDepth)
CategoryLevel_RootDepth$FDis_RootDepth <- as.numeric(CategoryLevel_RootDepth$FDis_RootDepth)

SiteLevel_RootDepth$CWM_RootDepth <- as.numeric(SiteLevel_RootDepth$CWM_RootDepth)
SiteLevel_RootDepth$FDis_RootDepth <- as.numeric(SiteLevel_RootDepth$FDis_RootDepth)

png(
  "output_plot/MainFigure_Categor_VS_Site_RootDepth.jpg",
  width = 8,
  height = 8,
  units = 'in',
  res = 900
)
par(mfcol=c(2,2))

par(mar = c(3, 5, 2, 3))

############## CWM ##############

boxplot(CWM_RootDepth ~ Exposure_Depth, data = CategoryLevel_RootDepth,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "CWM at category level", at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.5, ylim = c(0, 40))

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
     tick=FALSE , cex.axis=1.5)


boxplot(CWM_RootDepth ~ Exposure_Depth, data = SiteLevel_RootDepth,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "CWM at site level", at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.5, ylim = c(0, 40))

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
     tick=FALSE , cex.axis=1.5)

############## FDis ##############

boxplot(FDis_RootDepth ~ Exposure_Depth, data = CategoryLevel_RootDepth,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "FDis at category level", at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.5, ylim = c(0, 1.4))


legend("topright", legend = c("Shallow", "Deep"), 
       col=c("bisque","azure2"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1.5,  horiz = F)

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
     tick=FALSE , cex.axis=1.5)


boxplot(FDis_RootDepth ~ Exposure_Depth, data = SiteLevel_RootDepth,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "FDis at site level", at = c(1, 2, 4, 5),
        lex.order = TRUE, xaxt="n",cex.lab = 1.5, ylim = c(0, 1.4))

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
     tick=FALSE , cex.axis=1.6)

dev.off()

