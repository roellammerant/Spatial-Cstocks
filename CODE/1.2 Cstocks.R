##%######################################################%##
#                                                          #
####           Plant Quadrat C stocks per site          ####
#                                                          #
##%######################################################%##

################################
##      Plant quadrats      ##
################################
Cstock_all_Plant <-rbind(PojoPlantCstock,ShelPlantCstock,SemiPlantCstock,ExposedPlantCstock)
Cstock_all_Plant$Depth <- c(rep("Deep",8), rep("Shallow",60),rep("Deep",30),rep("Shallow",30),rep("Deep",26),rep("Shallow",31))

cstock_Plant_shallow <- Cstock_all_Plant[ which(Cstock_all_Plant$Depth=='Shallow'), ]
cstock_Plant_shallow$Exposure_Site <- factor(paste(cstock_Plant_shallow$Category, 
                                                   cstock_Plant_shallow$site, sep = "_"),
                                          levels = c(
                                            "Exposed_S_1", "Exposed_S_2","Exposed_S_3", "Exposed_S_4","Exposed_S_5", 
                                            "Semi_S_1", "Semi_S_2","Semi_S_3", "Semi_S_4","Semi_S_5",
                                            "Shel_S_1", "Shel_S_2","Shel_S_3", "Shel_S_4","Shel_S_5",
                                            "Pojo_S_1", "Pojo_S_2","Pojo_S_3", "Pojo_S_4","Pojo_S_5"
                                          ))


cstock_Plant_Deep <- Cstock_all_Plant[ which(Cstock_all_Plant$Depth=='Deep'), ]
Cstock_PlantD <- data.frame(site=c(rep("D_1",6),rep("D_2",4),rep("D_4",6), rep("D_5",6), rep("D_1",6), rep("D_2",6),
                                          rep("D_3",6), rep("D_4",6), rep("D_5",12)),
                            CarbonStock=c(rep("0",58)),
                           Category=c(rep("Pojo",22), rep("Shel",30), rep("Exposed",6)),
                           Depth = c(rep("Deep",58)))
cstock_Plant_Deep <-rbind(cstock_Plant_Deep,Cstock_PlantD)
cstock_Plant_Deep$Exposure_Site <- factor(paste(cstock_Plant_Deep$Category, 
                                                cstock_Plant_Deep$site, sep = "_"),
                                        levels = c(
                                          "Exposed_D_1", "Exposed_D_2","Exposed_D_3", "Exposed_D_4","Exposed_D_5", 
                                          "Semi_D_1", "Semi_D_2","Semi_D_3", "Semi_D_4","Semi_D_5",
                                          "Shel_D_1", "Shel_D_2","Shel_D_3", "Shel_D_4","Shel_D_5",
                                          "Pojo_D_1", "Pojo_D_2","Pojo_D_3", "Pojo_D_4","Pojo_D_5"
                                        ))
cstock_Plant_Deep$CarbonStock <- as.numeric(cstock_Plant_Deep$CarbonStock)



##%######################################################%##
#                                                          #
####      Figure Plant Quadrat C stocks per site        ####
#                                                          #
##%######################################################%##

CPlant_Shallow <- aggregate(cstock_Plant_shallow$CarbonStock,
                    by = list(cstock_Plant_shallow$Exposure_Site),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),
                                        n = length(x)))
CPlant_Shallow <- do.call(data.frame, CPlant_Shallow) 
CPlant_Shallow$se <- CPlant_Shallow$x.sd / sqrt(CPlant_Shallow$x.n)
colnames(CPlant_Shallow) <- c("Site", "mean", "sd", "n", "se")

CPlant_Deep <- aggregate(cstock_Plant_Deep$CarbonStock,
                            by = list(cstock_Plant_Deep$Exposure_Site),
                            FUN = function(x) c(mean = mean(x), sd = sd(x),
                                                n = length(x)))
CPlant_Deep <- do.call(data.frame, CPlant_Deep) 
CPlant_Deep$se <- CPlant_Deep$x.sd / sqrt(CPlant_Deep$x.n)
colnames(CPlant_Deep) <- c("Site", "mean", "sd", "n", "se")

png(
  "output_plot/SupplCstockPlantSite.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 900
)

par(mfcol=c(2,1))

par(mar = c(3, 5, 2, 3))

barCenters <- barplot(height = CPlant_Shallow$mean,
                      beside = true, las = 2,
                      ylim = c(0, 150),
                      cex.axis = 1.25, cex.lab =1.25, xaxt = "n",
                      ylab = "g C m-2 (1-2m)",
                      border = "black", axes = TRUE, 
                      col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
                              "palegreen","palegreen","palegreen","palegreen","palegreen",
                              "plum1","plum1","plum1","plum1","plum1",
                              "orange2","orange2","orange2","orange2","orange2"))

segments(barCenters, CPlant_Shallow$mean - CPlant_Shallow$se, barCenters,
         CPlant_Shallow$mean + CPlant_Shallow$se, lwd = 1.5)
arrows(barCenters, CPlant_Shallow$mean - CPlant_Shallow$se, barCenters,
       CPlant_Shallow$mean + CPlant_Shallow$se, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
legend("topleft", legend = c("Exposed", "Semi-exposed","Sheltered","Pojo bay"), 
       col=c("burlywood3","palegreen","plum1","orange2"),
       pch = 15, bty = "n", pt.cex = 3, cex = 1,  horiz = F)

barCenters <- barplot(height = CPlant_Deep$mean,
                      beside = true, las = 2,
                      ylim = c(0, 150),
                      cex.axis = 1.25, cex.lab =1.25, xaxt = "n",
                      ylab = "g C m-2 (3-4m)",
                      border = "black", axes = TRUE, 
                      col = c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
                              "palegreen","palegreen","palegreen","palegreen","palegreen",
                              "plum1","plum1","plum1","plum1","plum1",
                              "orange2","orange2","orange2","orange2","orange2"))

segments(barCenters, CPlant_Deep$mean - CPlant_Deep$se, barCenters,
         CPlant_Deep$mean + CPlant_Deep$se, lwd = 1.5)
arrows(barCenters, CPlant_Deep$mean - CPlant_Deep$se, barCenters,
       CPlant_Deep$mean + CPlant_Deep$se, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

dev.off()

