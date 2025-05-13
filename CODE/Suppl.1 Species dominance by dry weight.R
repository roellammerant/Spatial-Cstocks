##%######################################################%##
#                                                          #
####          Suppl. Dominance by Biomass               ####
#                                                          #
##%######################################################%##

Exposed <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                      sheet = "Exposed", na = "NA")
Semi <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                   sheet = "Semi", na = "NA")
Sheltered <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                        sheet = "Sheltered", na = "NA")
Pojo <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                   sheet = "Pojo", na = "NA")


png(
  "output_plot/Supplemental_DominanceSpecies.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 300
)

par(mfcol=c(2,2), mar=c(7,4,1,1) +1.0)


############################                          
##        Exposed         ##
############################
boxplot((`Total  dry mass (mg)`/1000)*16 ~Species,
        data=Exposed,
        main= "Exposed",
        xlab=NA,
        ylab="g dwt biomass m-2",
        xlim = c(0, 12),
        ylim = c(0, 250),
        col="bisque",
        border="brown",
        at = c(1, 3, 5, 7, 9, 11),
        names=c(expression(italic("M. spicatum")),expression(italic("P. perfoliatus")),
                expression(italic("R. cirrhosa")),expression(italic("S. pectinata")), 
                expression(italic("Z. major")), expression(italic("Z. marina"))),
        las =  2
)

stripchart((`Total  dry mass (mg)`/1000)*16 ~Species,
           data = Exposed,
           method = "jitter",
           at = c(0, 2, 4, 6, 8,10),
           pch = 17,
           col = c("bisque"),
           vertical = TRUE,
           add = TRUE)
  
############################                          
##      Sheltered        ##
############################

boxplot((`Total  dry mass (mg)`/1000)*16 ~Species,
        data=Sheltered,
        main= "Sheltered",
        xlab=NA,
        ylab="g dwt biomass m-2",
        xlim = c(0, 18),
        ylim = c(0, 600),
        col="bisque",
        border="brown",
        at = c(1, 3, 5, 7, 9, 11, 13, 15, 17),
        names=c(expression(italic("C. hermaphroditica")),expression(italic("C. demersum")),
                expression(italic("L. trisulca")), expression(italic("M. sibiricum")), 
                expression(italic("M. spicatum")), expression(italic("N. marina")),
                expression(italic("P. perfoliatus")), expression(italic("R. circinatus")), 
                expression(italic("S. pectinata"))),
        las =  2
)


stripchart((`Total  dry mass (mg)`/1000)*16 ~Species,
           data = Sheltered,
           method = "jitter",
           at = c(0, 2, 4, 6, 8,10,12,14,16),
           pch = 17,
           col = c("bisque"),
           vertical = TRUE,
           add = TRUE)

############################                          
##     Semi-exposed       ##
############################

boxplot((`Total  dry mass (mg)`/1000)*16 ~Species,
        data=Semi,
        main= "Semi-sheltered",
        xlab=NA,
        ylab="g dwt biomass m-2",
        xlim = c(0, 18),
        ylim = c(0, 180),
        col="bisque",
        border="brown",
        at = c(1, 3, 5, 7, 9, 11, 13, 15, 17),
        names=c(expression(italic("C. demersum")),expression(italic("M. spicatum")),
                expression(italic("P. perfoliatus")), expression(italic("R. baudotti")), 
                expression(italic("R. circinatus")), expression(italic("R. maritima")),
                expression(italic("S. pectinata")), expression(italic("Z. major")), 
                expression(italic("Z. marina"))),
        las =  2
)


stripchart((`Total  dry mass (mg)`/1000)*16 ~Species,
           data = Semi,
           method = "jitter",
           at = c(0, 2, 4, 6, 8,10, 12, 14, 16),
           pch = 17,
           col = c("bisque"),
           vertical = TRUE,
           add = TRUE)




############################                          
##         Pojo           ##
############################
boxplot((`Total  dry mass (mg)`/1000)*16 ~Species,
        data=Pojo,
        main= "Pojo",
        xlab=NA,
        ylab="g dwt biomass m-2",
        xlim = c(0, 18),
        ylim = c(0, 180),
        col="bisque",
        border="brown",
        at = c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23),
        names=c(expression(italic("C. hermaphroditica")),expression(italic("C. demersum")),
                expression(italic("Fontinalis sp.")), expression(italic("L. trisulca")), 
                expression(italic("M. sibiricum")), expression(italic("M. spicatum")),
                expression(italic("N. marina")), expression(italic("N. lutea")), 
                expression(italic("P. nitens")), expression(italic("P. obtusifolius")), 
                expression(italic("R. circinatus")), expression(italic("Sparganium sp."))),
        las =  2
)

stripchart((`Total  dry mass (mg)`/1000)*16 ~Species,
           data = Pojo,
           method = "jitter",
           at = c(0, 2, 4, 6, 8,10,12,14,16,18,20,22),
           pch = 17,
           col = c("bisque"),
           vertical = TRUE,
           add = TRUE)

dev.off()

