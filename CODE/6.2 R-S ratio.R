#%######################################################%##
#                                                          #
####        Plotting Community indices height           ####
#                                                          #
##%######################################################%##

Indices_Ratio_Allsites$CWM_Ratio <- as.numeric(Indices_Ratio_Allsites$CWM_Ratio)
Indices_Ratio_Allsites$FDis_Ratio <- as.numeric(Indices_Ratio_Allsites$FDis_Ratio)
Indices_Ratio_Allsites$Exposure_Depth <- factor(Indices_Ratio_Allsites$Exposure_Depth , # Reorder factor levels
                                               c("Exposed_Shallow","Exposed_Deep",
                                                 "semi_Shallow", "semi_Deep", 
                                                 "Sheltered_Shallow",
                                                 "Pojo_Shallow", "Pojo_Deep"))

png(
  "output_plot/Below-Aboveground ratio_Indices.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 300
)
par(mfcol=c(2,1))

par(mar = c(3, 5, 2, 3))
boxplot(CWM_Ratio ~ Exposure_Depth, data = Indices_Ratio_Allsites,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "CWM root to shoot ratio",at = c(1, 2, 4, 5, 7, 9, 10),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(CWM_Ratio ~ Exposure_Depth,
           data = Indices_Ratio_Allsites,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5, 6.5,8.5,9.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-exposed", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(1.80 , 10 , 2.5), 
     labels = label , 
     tick=FALSE , cex=1.2)

legend("topright", legend = c("Shallow", "Deep"), 
       col=c("bisque","azure2"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1,  horiz = F)


boxplot(FDis_Ratio ~ Exposure_Depth, data = Indices_Ratio_Allsites,
        boxwex = 0.5, col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
        main = NA,
        xlab = NA, ylab = "FDis root to shoot ratio",at = c(1, 2, 4, 5, 7, 9, 10),
        lex.order = TRUE, xaxt="n",cex.lab = 1.2)

stripchart(FDis_Ratio ~ Exposure_Depth,
           data = Indices_Ratio_Allsites,
           method = "jitter",
           at = c(0.5, 1.5, 3.5, 4.5, 6.5,8.5,9.5),
           pch = 17,
           col = c("bisque","azure2","bisque","azure2","bisque","bisque","azure2"),
           vertical = TRUE,
           add = TRUE)

label=c("Exposed", "Semi-exposed", "Sheltered", "Pojo bay")

axis(1, 
     at = seq(1.80 , 10 , 2.5), 
     labels = label , 
     tick=FALSE , cex=1.2)

dev.off()


