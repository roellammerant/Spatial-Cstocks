##%######################################################%##
#                                                          #
####          Prepare data environmental PCA            ####
#                                                          #
##%######################################################%##
library(vegan)
library(car)
library(tidyr)
library(ellipse)
library(imager)

Site_environmental <- read_excel("DATA/Site environmental.xlsx")

ord <- prcomp(~ Site_environmental$Salinity +
                Site_environmental$LOI +
                Site_environmental$`Percent Sand` +
                Site_environmental$`Percent Clay` 
              , center = TRUE, scale = TRUE, data=Site_environmental)

summary(ord) # percent of variance explained by each PC AXIS

data.scores = as.data.frame(ord$x[,1:2])
data.scores$Exposure <- Site_environmental$Exposure



##%######################################################%##
#                                                          #
####      Prepare data Community composition NMDS       ####
#                                                          #
##%######################################################%##

Spatial_Campaign_data_exp <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                        sheet = "Exposed", na = "NA")
Spatial_Campaign_data_semi <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Semi", na = "NA")
Spatial_Campaign_data_shel <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Sheltered", na = "NA")
Spatial_Campaign_data_pojo <- read_excel("DATA/Spatial_Campaign_data.xlsx", 
                                         sheet = "Pojo", na = "NA")

Exposed_data <- Spatial_Campaign_data_exp[,c(1:4,29:34)]
Semi_data <-Spatial_Campaign_data_semi[,c(1:4,30:35)]
Shel_data <-Spatial_Campaign_data_shel[,c(1:4,34:39)]
Pojo_data <-Spatial_Campaign_data_pojo[,c(1:4,32:37)]

Exposed_dry_mass <- aggregate(`Total  dry mass (mg)` ~ Site*Species,
                              FUN = sum,  
                              data = Exposed_data)

Semi_dry_mass <- aggregate(`Total  dry mass (mg)` ~ Site*Species,
                           FUN = sum,  
                           data = Semi_data)

Shel_dry_mass <- aggregate(`Total  dry mass (mg)` ~ Site*Species,
                           FUN = sum,  
                           data = Shel_data)

Pojo_dry_mass <- aggregate(`Total  dry mass (mg)` ~ Site*Species,
                           FUN = sum,  
                           data = Pojo_data)



Exposed<- pivot_wider(Exposed_dry_mass, names_from = Species, values_from = `Total  dry mass (mg)`)
Exposed[is.na(Exposed)] <- 0
Exposed$Site_group <- c("Exposed")
Exposed <-Exposed[,c(2:8)]

Semi<- pivot_wider(Semi_dry_mass, names_from = Species, values_from = `Total  dry mass (mg)`)
Semi[is.na(Semi)] <- 0
Semi$Site_group <- c("Semi")
Semi <-Semi[,c(2:11)]

Sheltered<- pivot_wider(Shel_dry_mass, names_from = Species, values_from = `Total  dry mass (mg)`)
Sheltered[is.na(Sheltered)] <- 0
Sheltered$Site_group <- c("Shel")
Sheltered <-Sheltered[,c(2:11)]

Pojo<- pivot_wider(Pojo_dry_mass, names_from = Species, values_from = `Total  dry mass (mg)`)
Pojo[is.na(Pojo)] <- 0
Pojo$Site_group <- c("Pojo")
Pojo <-Pojo[,c(2:14)]

Exposed$CALHER <-0
Exposed$CERDEM <-0
Exposed$Fontinalis <-0
Exposed$LEMTRI <-0
Exposed$MYRSIB <-0
Exposed$NAJMAR <- 0
Exposed$NUPLUT <- 0
Exposed$POTNIT <- 0
Exposed$POTOBT <-0
Exposed$RANBAU <-0
Exposed$RANCIR <-0
Exposed$Sparganium <-0
Exposed <-Exposed[,c(7,2,5,4,6,1,3,8:19)]

Semi$CALHER <-0
Semi$Fontinalis <-0
Semi$LEMTRI <-0
Semi$MYRSIB <-0
Semi$NAJMAR <- 0
Semi$NUPLUT <- 0
Semi$POTNIT <- 0
Semi$POTOBT <-0
Semi$Sparganium <-0
Semi <-Semi[,c(10,3,8,7,9,2,6,11,1,12:18,4,5,19)]

Sheltered$Fontinalis <-0
Sheltered$NUPLUT <- 0
Sheltered$POTNIT <- 0
Sheltered$POTOBT <-0
Sheltered$RANBAU <-0
Sheltered$RUPCIR <-0
Sheltered$Sparganium <-0
Sheltered$ZANMAJ <- 0
Sheltered$ZOSMAR <-0
Sheltered <-Sheltered[,c(10,7,18,9,19,5,16,1,2,11,3,4,6,12,13,14,15,8,17)]

Pojo$POTPER <-0
Pojo$RANBAU <-0
Pojo$RUPCIR <-0
Pojo$STUPEC <-0
Pojo$ZANMAJ <-0
Pojo$ZOSMAR <-0
Pojo <-Pojo[,c(13,14,18,17,19,6,16,1,2,3,4,5,7,8,9,10,15,11,12)]

All_sites_drymass <-rbind(Exposed,Semi)
All_sites_drymass <-rbind(All_sites_drymass,Sheltered)
All_sites_drymass <-rbind(All_sites_drymass,Pojo)

comB = All_sites_drymass[,2:19]
m_comB = as.matrix(comB)
set.seed(123)
nmdsB = metaMDS(m_comB, distance = "bray")
nmdsB
plot(nmdsB)

data.scoresB = as.data.frame(scores(nmdsB)$sites)
data.scoresB$Site_group <- All_sites_drymass$Site_group


##%######################################################%##
#                                                          #
####           Plot 4 panel figure of sites             ####
#                                                          #
##%######################################################%##
png(
  "output_plot/MainFigure_Sites_Environment_Species.jpg",
  width = 13,
  height = 8,
  units = 'in',
  res = 900
)

par(mfcol = c(2, 2), mar = c(1, 1, 0, 0.5))  

#######################
### FIGURE OF sites ###
#######################

image_data <- load.image("C:/Users/roell/OneDrive/Bureaublad/Github/Coastclim Spatial/Spatial-23-CoastClim/output_plot/Map_sitegroup_colors.png")
plot(image_data, axes = FALSE)

mtext("A", 3, -1.25, adj=0.05, font=2)


#######################
### Species NMDS    ###
#######################
par(mar = c(4, 4, 1.5, 0.5))
colors <- c("burlywood3","orange2","palegreen","plum1")

ordiplot(nmdsB, display = "species", cex = 1.5, xlim = c(-2.3,3.5), ylab="NMDS2", xlab="NMDS1")
dataEllipse(data.scoresB$NMDS1, data.scoresB$NMDS2, groups = as.factor(data.scoresB$Site_group), levels = c(0.60), 
            center.pch =FALSE,  plot.points = FALSE, group.labels = NA, col=colors)

text(1.064122079, 0.25366121, expression("C. demersum"))
text(0.000725299, -0.930, expression("M. spicatum"))
text(-0.845397347, 0.770, expression("R. cirrhosa"))
text(-0.874367214, -1, expression("R. baudotti"))
text(-1.074251583, 0.364, expression("Z. major"))
text(-0.316448835 , 1, expression("S. pectinata"))
text(-1.8, -0.45444449, expression("Z. marina"))
text(2.15,-0.63773488,expression("L. trisulca"))
text(1.495955489,0.00655396, expression("M. sibiricum"))
text(1.532480937,0.6, expression("N. lutea"))
text(-0.750562000, -0.36740262, expression("P. perfoliatus"))
text(2.27517475, -0.80189288, expression("Fontinalis Sp."))
text(2.35, -0.98839380, expression("Sparganium Sp."))
text( 1.0051941252, -0.65978410, expression("R. circunatus"))
text(3.240411228,  0.75350624, expression("P. nitens"))
text(1.946877033, -1.19134342, expression("P. obtusifolius"))
text(2.082468368, -0.27188921, expression("N. marina"))
text(2.913322221, -0.07091259, expression("C. hermaprhoditica"))

mtext("C", 3, 0.25, adj=0, font=2)

##################################################################
### FIGURE OF ORDINATION
##################################################################

colors <- c("burlywood3","orange2","palegreen","plum1")

plot(ord$x[,1:2], pch=16, col=c("burlywood3","burlywood3","burlywood3","burlywood3","burlywood3",
                                "palegreen","palegreen","palegreen","palegreen","palegreen",
                                "plum1","plum1","plum1","plum1","plum1",
                                "orange2","orange2","orange2","orange2","orange2"), 
     cex=0.75,ylim = c(-3,3), xlim = c(-2,4))

dataEllipse(data.scores$PC1, data.scores$PC2, groups = as.factor(data.scores$Exposure), levels = c(0.60), 
            center.pch =FALSE,  plot.points = FALSE, group.labels = NA, col=colors)

mtext("B", 3, 0.25, adj=0.0, font=2)
abline(v=0, h=0, lty=3)

l.x <- ord$rotation[,1]*1.5
l.y <- ord$rotation[,2]*1.5

arrows(rep(0,11), rep(0,11), l.x, l.y, len=0.1, lwd=1.25)


text(-0.82, -1.4, expression("Salinity"))
text(0.90, -0.72, expression("LOI"))
text(-0.99, 0.37, expression("Sand %"))
text(1.00, -0.33, expression("Clay %"))

legend("topleft",
       legend = c("Exposed", "Semi-sheltered", "Shelted", "Pojo bay"),
       pch = 19,
       col = c("#DEB887","#98FB98","#DDA0DD","#FFA500"),
       cex = 0.95)

###########################
### Species NMDS data   ###
###########################

plot(data.scoresB$NMDS1, data.scoresB$NMDS2,  col = colors[as.factor(data.scoresB$Site_group)],xlim=c(-2,3), ylab="NMDS2", xlab="NMDS1", pch = 19)
dataEllipse(data.scoresB$NMDS1, data.scoresB$NMDS2, groups = as.factor(data.scoresB$Site_group), levels = c(0.60), 
            center.pch =FALSE,  plot.points = FALSE, group.labels = NA, col=colors)

mtext("D", 3, 0.25, adj=0, font=2)

dev.off()