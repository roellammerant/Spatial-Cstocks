##%######################################################%##
#                                                          #
####            Plotting intra versus inter             ####
#                                                          #
##%######################################################%##

png(
  "output_plot/Main Figure 4.jpg",
  width = 16,
  height = 8,
  units = 'in',
  res = 300
)
par(mfcol=c(2,3))

par(mar = c(3, 5, 2, 3))

##%######################################################%##
#                                                          #
####                      Height                        ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("10.08","129.01","-115.47","23.62","6.70","21.43","-17.65","10.47")),
        col = c("azure2","bisque","aquamarine","lightsalmon","azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of Height", space = as.numeric(c("0.25","0.25","0.25","0.25", "2.0","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-175,175))


label=c("CWM", "FDis")

axis(1, 
     at = seq(2.5 , 10 , 6.5), 
     labels = label , 
     tick=FALSE)

legend("topright", legend = c("Species turnover", "Intraspecific variation", "Covariation", "Total variation"), 
       col=c("azure2","bisque","aquamarine","lightsalmon"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1.2,  horiz = F)

##%######################################################%##  
#                                                          #
####                    Root Depth                      ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("16.54", "50.91", "-51.50", "15.95","8.19", "13.21", "-14.88", "6.51")),
        col = c("azure2","bisque","aquamarine","lightsalmon","azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of Root depth", space = as.numeric(c("0.25","0.25","0.25","0.25", "2.0","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-60,60))

label=c("CWM", "FDis")

axis(1, 
     at = seq(2.5 , 10 , 6.5), 
     labels = label , 
     tick=FALSE)

##%######################################################%##
#                                                          #
####                      R-S ratio                     ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("25.14", "1.96", "8.54", "35.64","15.97", "8.65", "-16.80", "7.81")),
        col = c("azure2","bisque","aquamarine","lightsalmon","azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of R-S ratio", space = as.numeric(c("0.25","0.25","0.25","0.25", "2.0","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-60,60))

label=c("CWM", "FDis")

axis(1, 
     at = seq(2.5 , 10 , 6.5), 
     labels = label , 
     tick=FALSE)


##%######################################################%##
#                                                          #
####                        SLA                         ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("12.37", "17.94", "-13.72", "16.58","10.69", "10.99", "-14.12", "7.52")),
        col = c("azure2","bisque","aquamarine","lightsalmon","azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of SLA", space = as.numeric(c("0.25","0.25","0.25","0.25", "2.0","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-60,60))

label=c("CWM", "FDis")

axis(1, 
     at = seq(2.5 , 10 , 6.5), 
     labels = label , 
     tick=FALSE)

##%######################################################%##
#                                                          #
####                        LAP                         ####
#                                                          #
##%######################################################%##

barplot(as.numeric(c("12.87", "0.04", "-1.18", "11.74","12.28", "11.62", "-8.61", "15.29")),
        col = c("azure2","bisque","aquamarine","lightsalmon","azure2","bisque","aquamarine","lightsalmon"),
        ylab = "Explained variance of LAP", space = as.numeric(c("0.25","0.25","0.25","0.25", "2.0","0.25","0.25","0.25")),
        cex.lab = 1.5, ylim=c(-60,60))

label=c("CWM", "FDis")

axis(1, 
     at = seq(2.5 , 10 , 6.5), 
     labels = label , 
     tick=FALSE)

dev.off()



