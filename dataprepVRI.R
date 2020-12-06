
#Libraries
library(spgwr)
library(spatstat)
library(tmap)
library(gstat)
library(sf)
library(raster)
library(rgdal)
library(e1071)
library(spdep)

#Set working directory
dir <- "/Users/emmabialobzyski/Desktop/Geog 418/Geog418Final"
setwd(dir)
getwd()

#Reading in datasets
elev <- readOGR("ElevSample.shp") #Read in data
elev <- spTransform(elev, CRS("+init=epsg:26910"))

victoria <- readOGR("Vic_Census_Join.shp")
victoria <- spTransform(victoria, CRS("+init=epsg:26910"))

vanisl <- readOGR("ABMS_PROV_polygon.shp")
vanisl <- spTransform(victoria, CRS("+init=epsg:26910"))

bound <-readOGR("LHA_2018.shp")
bound <- spTransform(bound, CRS("+init=epsg:26910"))

background <- readOGR("DissolveOutput.shp")
background <- spTransform(background, CRS("+init=epsg:26910"))

cities <- readOGR("VanIs_Pop.shp")
cities <- spTransform(cities, CRS("+init=epsg:26910"))
View(elev@data)

#Reading in VRI data
VRI <- readOGR("WatershedVRI.shp") #Read in shapefile
VRI <- spTransform(VRI, CRS("+init=epsg:26910"))
head(VRI@data)
View(vriClean@data)

vriCleanCols <- c("FID_VEG_CO", "POLYGON_ID", "PROJ_AGE_1",
                  "SITE_INDEX", "SPECIES__4", "SPECIES__5",
                  "PROJ_HEI_1", "SPECIES_PC", "SPECIES__6",
                  "VRI_LIVE_S", "BASAL_AREA", "WHOLE_STEM",
                  "CROWN_CL_1")

vriClean <- VRI[,vriCleanCols]


newNames <- c("FID", "PolyID", "Stand_Age", "Site_Index",
              "CoDom_Sp", "Dom_Sp", "Stand_HT", "DomSP_Perc", 
              "CDomSP_Perc", "Stand_Dens", "Stand_BA", "Stand_StemBio", "Stand_CrownCl")
colnames(vriClean@data) <- newNames

(vriClean$Stand_StemBio)
VRI
vriClean
VRI.no0$Stand_StemBio

#get rid of nas
vriClean <- vriClean[!is.na(vriClean@data$Stand_StemBio), ]
#make sure numeric
typeof(vriClean$Stand_StemBio)

#Create choropleth map of stand variable - with removed zero data from combine page
map_Bio <- tm_shape(VRI.no0) +
  tm_polygons(col = "Stand_StemBio",
              title = "Biomass [T/ha]",
              style = "jenks",
              palette = "Greens", n = 6,
              border.col = "lightgrey", 
              lwd = 0.3) +
  tm_legend(legend.position = c("LEFT", "BOTTOM"), title.size = 1.5, legend.text.size = 1.1, legend.stack = "horizontal")+
  tm_shape(elev) + tm_dots(col="grid_code", border.col = "black", palette = "Reds", contrast = c(0.33, 0.86), 
                               title="Elevation [m]", size=0.2, scale = 1)+
  tm_layout(panel.labels = "(B) Stand Stem Biomass in the Sooke Lake Watershed\nwith Elevation Sample",  panel.label.height = 2, panel.label.size = 2.2, inner.margins = c(0.15, 0.05, 0.03, 0.13))+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"))

map_Bio

#make a map of the study area, in victoria/vancouver island
newbound <- bound[bound$HSDA_CD %in% c(41, 42), ]

Map_background <- tm_shape(background) + tm_polygons(col = "lightyellow1") +
  tm_shape(VRI.no0) +
  tm_polygons(col = "palegreen3",border.col = "palegreen3") +
  tm_layout(bg.color = "lightblue", panel.labels = "(A) The Sooke Lake Watershed, Vancouver Island, BC", panel.label.height = 2, panel.label.size = 2.2, inner.margins = c(0.02, -1.1, -0.9, -0.2) )+
  tm_compass(position = c("left", "top")) +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_shape(cities) +tm_dots(size = 0.4, col = "black") +tm_text("Name", size = 1.3, bg.color = NA, bg.alpha = NA, xmod = 1.5, ymod = 0.5)


Map_background

newbound.coords <- coordinates(newbound)
bound.coords <- coordinates(bound)

newboundID <- cut(bound.coords[,1], quantile(bound.coords[,1]),include.lowest = TRUE)
newbound <- unionSpatialPolygons(bound, newboundID)


##################################################
#Descriptive statistics
#Mean
meanstem <- mean(VRI.no0$Stand_StemBio)
meanelev <- mean(elev$grid_code)

#Standard Deviation
sdstem <- sd(VRI.no0$Stand_StemBio) #Calculate the SD, ignoring NA values
sdelev <- sd(elev$grid_code) #Calculate the SD, ignoring NA values only for the summer months

#Mode
modeStem <- as.numeric(names(sort(table(VRI.no0$Stand_StemBio), decreasing = TRUE))[1]) #make frequency table of fire size variable and sort it in desending order and extract the first row (Most Frequent)
modeElev <- as.numeric(names(sort(table(elev$grid_code), decreasing = TRUE))[1])

#Median
medStem <- median(VRI.no0$Stand_StemBio)
medElev <- median(elev$grid_code)

library(e1071)
#Skewness
skewstem <- skewness(VRI.no0$Stand_StemBio, na.rm = TRUE)[1]
skewelev <- skewness(elev$grid_code, na.rm = TRUE)[1]

#Kurtosis
kurtstem <- kurtosis(VRI.no0$Stand_StemBio, na.rm = TRUE)[1]
kurtelev <- kurtosis(elev$grid_code, na.rm = TRUE)[1]

#CoV
CoVstem <- (sdstem / meanstem) * 100
CoVelev <- (sdelev / meanelev) * 100

#Normal distribution test
normStem_PVAL <- shapiro.test(VRI.no0$Stand_StemBio)$p.value
normelev_PVAL <- shapiro.test(elev$grid_code)$p.value

#####
#Create a table of descriptive stats

Sample = c("Stand Stem Biomass", "Elevation") #Create an object for the labels
means = c(meanstem, meanelev) #Create an object for the means
sd = c(sdstem, sdelev) #Create an object for the standard deviations
Median = c(medStem, medElev) #Create an object for the medians
Mode <- c(modeStem, modeElev) #Create an object for the modes
skewness <- c(skewstem, skewelev) #Create an object for the skewness
kurtosis <- c(kurtstem, kurtelev) #Create an object for the kurtosis
CoV <- c(CoVstem, CoVelev) #Create an object for the CoV
Normality <- c(normStem_PVAL, normelev_PVAL) #Create an object for the normality PVALUE

Normality <-signif(normality, digits =3)

Mean <- round(means, 2)
SD <- round(sd, 3)
Skewness <- round(skewness, 2)
Kurtosis <- round(kurtosis, 2)
CoV <- round(CoV, 2)

qqnorm(VRI.no0$Stand_StemBio, pch = 1, frame = FALSE)
qqline(VRI.no0$Stand_StemBio, col = "steelblue", lwd = 2)

#Make table
library(gridExtra)
library(grid)
library(ggplot2)
library(gtable)

data.for.table2 = data.frame(Sample, Mean,Median, Mode,Skewness, SD, Kurtosis, CoV, Normality)

table2 <- tableGrob(data.for.table2, rows = c("",""))
t2Caption <- textGrob("Table 1: Descriptive Statistics for Stand Stem Biomass\nand Elevation in the Sooke Lake Watershed", gp = gpar(fontsize = 12))
padding <- unit(5, "mm")

table2 <- gtable_add_rows(table2, 
                          heights = grobHeight(t2Caption) + padding, 
                          pos = 0)

table2 <- gtable_add_grob(table2,
                          t2Caption, t = 1, l = 2, r = ncol(data.for.table2) + 1)
grid.arrange(table2, newpage = TRUE)

# Elevation data
elevdata <- as.data.frame(elev)
range(elevdata$grid_code)
#62.780, 834.855

histelev <- ggplot(elevdata, aes(x = grid_code)) + #Create new GGplot object with data attached and fire size mapped to X axis
  geom_histogram(bins = 30, color = "black", fill = "white") + #make histogram with 30 bins, black outline, white fill
  labs(title = "Histogram of Elevation Sample, Sooke Lake Watershed, BC, 2020", x = "Elevation Above Sea Level [m]", y = "Frequency", caption = "Figure 1: Histogram of Fire Sizes for year 2020 to date (September 21), British Columbia") + #label plot, x axis, y axis
  theme_classic() + #set the theme to classic (removes background and borders etc.)
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.caption = element_text(hjust = 0.5)) + #set title to center and bold
  scale_y_continuous(breaks = seq(0, 700, by = 100)) # set y axis labels to 0 - 700 incrimenting by 100

print(histelev)

#Stand stem biomass data
standdata <- as.data.frame(vriClean)
View(VRI.no0@data)

histogramStem <- ggplot(VRI.no0, aes(x = Stand_StemBio))+ #Create new GGplot object with data attached and fire size mapped to X axis
  geom_histogram(bins = 30, color = "black", fill = "white") + #make histogram with 30 bins, black outline, white fill
  labs(title = "Histogram of Stemwood Biomass in Sooke Lake Watershed, 2020", x = "Stemwood Biomass [tonnes/HA]", y = "Frequency", caption = "Figure 2: Histogram of Summer Fire Sizes for 2020 Summer Season, British Columbia") + #label plot, x axis, y axis
  theme_classic() + #set the theme to classic (removes background and borders etc.)
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.caption = element_text(hjust = 0.5)) + #set title to center and bold
  scale_y_continuous(breaks = seq(0, 2000, by = 100)) # set y axis labels to 0 - 700 incrimenting by 100

print(histogramStem)

###########################################################################
#SAC of forest characteristic: stem biomass

#Queen's weight neighbourhood
vri.nb <- poly2nb(VRI.no0)
?nb2lines
vri.net <- nb2lines(vri.nb, coords=coordinates(VRI.no0))
crs(vri.net) <- crs(VRI.no0)

vri.lw <- nb2listw(vri.nb, zero.policy = TRUE, style = "W")
print.listw(vri.lw, zero.policy = TRUE)

#Global Moran's I
miStand_StemBio <- moran.test(VRI.no0$Stand_StemBio, vri.lw, zero.policy = TRUE)
miStand_StemBio
#3.95e-01, expectation -2.16e-04

mIstembio <- miStand_StemBio$estimate[[1]]
eIstembio <- miStand_StemBio$estimate[[2]]
varstembio <- miStand_StemBio$estimate[[3]]

zstembio <- (mIstembio -eIstembio) / sqrt(varstembio)
#39.64, so it is clustered

#Local Moran's I
lisa.test.stembio <- localmoran(VRI.no0$Stand_StemBio, vri.lw, zero.policy = TRUE)
lisa.test.stembio

VRI.no0$Ii <- lisa.test.stembio[,1]
VRI.no0$E.Ii<- lisa.test.stembio[,2]
VRI.no0$Var.Ii<- lisa.test.stembio[,3]
VRI.no0$Z.Ii<- lisa.test.stembio[,4]
VRI.no0$P<- lisa.test.stembio[,5]

VRI.no0 <- VRI.no0[!is.na(VRI.no0@data$Z.Ii), ]

map_LISASB <- tm_shape(VRI.no0) + 
  tm_polygons(col = "Z.Ii", 
              title = "Z Score",
              style = "fixed", breaks = c(-Inf, -1.96, 0, 1.96, Inf),
              palette = "-RdYlBu", contrast = c(0, 0.77),
              border.col = "lightgrey",
              lwd = 0.3) +
  tm_layout(panel.labels = "Z Score for Local Moran's I for\nStand Stem Biomass in the Sooke Lake Watershed",  panel.label.height = 3, panel.label.size = 2.2, inner.margins = 0.05) +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"))+
  tm_legend(legend.outside=FALSE, title.size = 2, legend.text.size = 1.5, legend.position = c("LEFT", "BOTTOM"))

map_LISASB
#The stem biomass variable appears to be overwhelmingly clustered.

###########################################################################
#Interpolation - the IDW method
#Create a grid called grd to use in your interpolation
# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(elev, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object
proj4string(grd) <- proj4string(elev)

# Interpolation using idp = 3
P.idw3 <- gstat::idw(grid_code~ 1, elev, newdata=grd, idp=3)
r3       <- raster(P.idw3)
r.m3     <- mask(r3, vriClean)

idwmap3 <- tm_shape(r.m3) + 
  tm_raster(n=10,palette = "Reds",
            title="Elevation [m]") + 
  tm_shape(elev) + tm_dots(size=0.1) +  tm_legend(legend.outside=FALSE, title.size = 1.1, legend.position = c("LEFT", "BOTTOM"))+
  tm_scale_bar(position = c("left", "bottom"))+
  tm_layout(panel.labels = "(A) Map of IDW Elevation, IDP = 3", panel.label.height = 3, inner.margins = 0.1) +
  tm_compass(position = c("right", "top"))

idwmap3

#Interpolation using idp = 2
P.idw2 <- gstat::idw(grid_code~ 1, elev, newdata=grd, idp=2)
r2       <- raster(P.idw2)
r.m2     <- mask(r2, vriClean)

idwmap2 <- tm_shape(r.m2) + 
  tm_raster(n=10,palette = "Reds",
            title="Elevation [m]") + 
  tm_shape(elev) + tm_dots(size=0.1) +
  tm_legend(legend.outside=FALSE, title.size = 1.1, legend.position = c("LEFT", "BOTTOM"))+
  tm_scale_bar(position = c("left", "bottom"))+
  tm_layout(panel.labels = "(A) Map of IDW Elevation, IDP = 2", panel.label.height = 3, inner.margins = 0.1) +
  tm_compass(position = c("right", "top"))

idwmap2

#Interpolation using idp = 4

P.idw4 <- gstat::idw(grid_code~ 1, elev, newdata=grd, idp=4)
r4       <- raster(P.idw4)
r.m4    <- mask(r4, vriClean)

idwmap4 <- tm_shape(r.m4) + 
  tm_raster(n=10,palette = "Reds",
            title="Elevation [m]") + 
  tm_shape(elev) + tm_dots(size=0.05) +
  tm_legend(legend.outside=FALSE, title.size = 1.1, legend.position = c("LEFT", "BOTTOM"))+
  tm_scale_bar(position = c("left", "bottom"))+
  tm_layout(panel.labels = "(A) Map of IDW Interpolation of Elevation, IDP = 4\nin the Sooke Lake Watershed", panel.label.height = 3, panel.label.size = 1.5, inner.margins = 0.1) +
  tm_compass(position = c("right", "top"))

idwmap4

#Interpolation using idp =5
P.idw5 <- gstat::idw(grid_code~ 1, elev, newdata=grd, idp=5)
r5       <- raster(P.idw5)
r.m5   <- mask(r5, vriClean)

idwmap5 <- tm_shape(r.m5) + 
  tm_raster(n=10,palette = "Reds",
            title="Elevation [m]") + 
  tm_shape(elev) + tm_dots(size=0.1) +
  tm_legend(legend.outside=FALSE, title.size = 1.1, legend.position = c("LEFT", "BOTTOM"))+
  tm_scale_bar(position = c("left", "bottom"))+
  tm_layout(panel.labels = "(A) Map of IDW Elevation, IDP = 5", panel.label.height = 3, inner.margins = 0.1) +
  tm_compass(position = c("right", "top"))

idwmap5

#Interpolation using idp =6
P.idw6 <- gstat::idw(grid_code~ 1, elev, newdata=grd, idp=6)
r6      <- raster(P.idw6)
r.m6   <- mask(r6, vriClean)

idwmap6 <- tm_shape(r.m6) + 
  tm_raster(n=10,palette = "Reds",
            title="Elevation [m]") + 
  tm_shape(elev) + tm_dots(size=0.1) +
  tm_legend(legend.outside=FALSE, title.size = 1.1, legend.position = c("LEFT", "BOTTOM"))+
  tm_scale_bar(position = c("left", "bottom"))+
  tm_layout(panel.labels = "(A) Map of IDW Elevation, IDP = 6", panel.label.height = 3, inner.margins = 0.1) +
  tm_compass(position = c("right", "top"))

idwmap6


#validation sequence - leave one out IDP 4
IDW.out4 <- vector(length = length(elev))
for (i in 1:length(elev)) {
  IDW.out4[i] <- idw(grid_code ~ 1, elev[-i,], elev[i,], idp=4)$var1.pred
}
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out4 ~elev$grid_code, asp = 1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5), xlim = c(0, 1000), ylim = c(0, 1000))
title("(C) Leave One Out Validation, IDP = 4", outer = TRUE)
abline(lm(IDW.out4 ~ elev$grid_code), col="red", lw=2,lty=2)
abline(0,1)

#the slope of the line for validation
lm(IDW.out4 ~ elev$grid_code)

#validation sequence - leave one out IDP 3
IDW.out3 <- vector(length = length(elev))
for (i in 1:length(elev)) {
  IDW.out3[i] <- idw(grid_code ~ 1, elev[-i,], elev[i,], idp=3)$var1.pred
}
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out3 ~elev$grid_code, asp = 1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5), xlim = c(0, 1000), ylim = c(0, 1000))
title("(C) Leave One Out Validation, IDP = 3", outer = TRUE)
abline(lm(IDW.out3 ~ elev$grid_code), col="red", lw=2,lty=2)
abline(0,1)

#validation sequence - leave one out IDP 2
IDW.out2 <- vector(length = length(elev))
for (i in 1:length(elev)) {
  IDW.out2[i] <- idw(grid_code ~ 1, elev[-i,], elev[i,], idp=2)$var1.pred
}
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out2 ~elev$grid_code, asp = 1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5), xlim = c(0, 1000), ylim = c(0, 1000))
title("(C) Leave One Out Validation, IDP = 2", outer = TRUE)
abline(lm(IDW.out2 ~ elev$grid_code), col="red", lw=2,lty=2)
abline(0,1)

#validation sequence - leave one out IDP 5
IDW.out5 <- vector(length = length(elev))
for (i in 1:length(elev)) {
  IDW.out5[i] <- idw(grid_code ~ 1, elev[-i,], elev[i,], idp=5)$var1.pred
}
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out5 ~elev$grid_code, asp = 1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5), xlim = c(0, 1000), ylim = c(0, 1000))
title("(C) Leave One Out Validation, IDP = 5", outer = TRUE)
abline(lm(IDW.out5 ~ elev$grid_code), col="red", lw=2,lty=2)
abline(0,1)

#validation sequence - leave one out IDP 6
IDW.out6 <- vector(length = length(elev))
for (i in 1:length(elev)) {
  IDW.out6[i] <- idw(grid_code ~ 1, elev[-i,], elev[i,], idp=6)$var1.pred
}
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out6 ~elev$grid_code, asp = 1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5), xlim = c(0, 1000), ylim = c(0, 1000))
title("(C) Leave One Out Validation, IDP = 6", outer = TRUE)
abline(lm(IDW.out5 ~ elev$grid_code), col="red", lw=2,lty=2)
abline(0,1)

#Root mean square error
idp2RMSE <- sqrt(sum((IDW.out2 - elev$grid_code)^2) / length(elev))
#76.056
idp3RMSE <- sqrt(sum((IDW.out3 - elev$grid_code)^2) / length(elev))
#53.66
idp4RMSE <- sqrt(sum((IDW.out4 - elev$grid_code)^2) / length(elev))
#50.55
idp5RMSE <- sqrt(sum((IDW.out5 - elev$grid_code)^2) / length(elev))
#51.21
idp6RMSE <- sqrt(sum((IDW.out6 - elev$grid_code)^2) / length(elev))
#52.31

##### Jackknife
img <- gstat::idw(grid_code~1, elev, newdata=grd, idp=4)
n   <- length(elev)
Zi  <- matrix(nrow = length(img$var1.pred), ncol = n)

# Remove a point then interpolate (do this n times for each point)
st <- stack()
for (i in 1:n){
  Z1 <- gstat::idw(grid_code~1, elev[-i,], newdata=grd, idp=4)
  st <- addLayer(st,raster(Z1,layer=1))
  # Calculated pseudo-value Z at j
  Zi[,i] <- n * img$var1.pred - (n-1) * Z1$var1.pred
}

# Jackknife estimator of parameter Z at location j
Zj <- as.matrix(apply(Zi, 1, sum, na.rm=T) / n )

# Compute (Zi* - Zj)^2
c1 <- apply(Zi,2,'-',Zj)            # Compute the difference
c1 <- apply(c1^2, 1, sum, na.rm=T ) # Sum the square of the difference

# Compute the confidence interval
CI <- sqrt( 1/(n*(n-1)) * c1)

# Create (CI / interpolated value) raster
img.sig   <- img
img.sig$v <- CI /img$var1.pred 

# Clip the confidence raster to Southern California
r <- raster(img.sig, layer="v")
r.m <- mask(r, vriClean)

#Interpolation using idp = 4

P.idw4 <- gstat::idw(grid_code~ 1, elev, newdata=grd, idp=4)
r4       <- raster(P.idw4)
r.m4    <- mask(r4, vriClean)

idwmap4 <- tm_shape(r.m4) + 
  tm_raster(n=4, palette = "Reds",
            title="Elevation [m]") + 
  tm_shape(elev) + tm_dots(size=0.05) +
  tm_legend(legend.outside=FALSE, title.size = 2, legend.text.size = 1.5, legend.position = c("LEFT", "BOTTOM"))+
  tm_scale_bar(position = c("left", "bottom"))+
  tm_layout(panel.labels = "(A) Map of IDW Interpolation of Elevation, IDP = 4\nin the Sooke Lake Watershed", panel.label.height = 3, panel.label.size = 2.2, inner.margins = 0.05) +
  tm_compass(position = c("right", "top"))

idwmap4

# Plot the map
mapJackknife <- tm_shape(r.m) + tm_raster(n=7,title="95% confidence\ninterval[m]") +
  tm_shape(elev) + tm_dots(size=0.05) + 
  tm_legend(legend.outside=FALSE, title.size = 2, legend.text.size = 1.5, legend.position = c("LEFT", "BOTTOM"))+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"))+
  tm_layout(panel.labels = "(B) Jack Knife Confidence Interval for IDP = 4", panel.label.size = 2.5, panel.label.height = 2.5, inner.margins = 0.05)

mapJackknife

tmap_arrange(idwmap4, mapJackknife, ncol =2)

#The best idw power is 4, lowest error and best fit
########################################################################
#Point Pattern Analysis of Elevation Data
#generate the coordinates for each point and subset
elev_subset <- elev[VRI, ]
kma <- elev_subset
kma$x <- coordinates(kma)[,1]
kma$y <- coordinates(kma)[,2]
coordinates(kma)

as.data.frame(elev_subset)
range(elev_subset$grid_code)
#check for and remove duplicated points
#first, finds zero distance among points to see if there are any duplicates
zd <- zerodist(kma)
zd

#if there are duplicates, remove them
kma <- remove.duplicates(kma)

#create an "extent" object which can be used to create the observation window for spatstat
library(raster)
kma.ext <- as.matrix(extent(kma))

#observation window
library(plyr)
library(spatstat)
window <- as.owin(list(xrange = kma.ext[1,], yrange = kma.ext[2,]))

#create ppp oject from spatstat
kma.ppp <- ppp(x = kma$x, y = kma$y, window = window)

###NEAREST NEIGHBOUR
nearestNeighbour <- nndist(kma.ppp)
library(rgeos)
gArea(vriClean)

##Convert the nearestNeighbor object into a dataframe.
nearestNeighbour=as.data.frame(as.numeric(nearestNeighbour))
##Change the column name to "Distance"
colnames(nearestNeighbour) = "Distance"

View(vriClean@data)
vriClean$area <- area(vriClean)
sum(vriClean$area)
#256404108

#Get VRI study area size in km2
studyArea<- 256404108
#256.4 km

##Calculate the nearest neighbor statistic to test for a random spatial distribution.
#mean nearest neighbour
N <- 380
nnd = (sum(nearestNeighbour))/N
nnd
# mean NND = 433.18 m
#mean nearest neighbour for random spatial distribution

library(raster)

pointDensity <-(N/studyArea)

r.nnd <- 1/(2*(sqrt(pointDensity)))

d.nnd = 1.07453/(sqrt(pointDensity))

c.nnd = 0

R = (nnd/r.nnd)

SE.NND <- (0.26136)/(sqrt(N*pointDensity))

z = ((nnd-r.nnd)/SE.NND)
#significantly dispersed in the study area
library(gtable)
library(raster)
library(gridExtra)
library(grid)
NND <- round(nnd, 1)
RandomNND<- round(r.nnd, 1)
DispersedNND<- round(d.nnd, 1)
R <- round(R, 3)
StandardErrorNND <- round(SE.NND, 1)
Zscore <- round(z, 1)

data.for.table1 = data.frame(NND, RandomNND, DispersedNND, R, StandardErrorNND, Zscore)
table1 <- tableGrob(data.for.table1) #make a table "Graphical Object" (GrOb) 
t1Caption <- textGrob("Table 2: Nearest Neighbour Analysis for\nElevation Sample, Sooke Lake Watershed", gp = gpar(fontsize = 13))

padding <- unit(5, "mm")

table1 <- gtable_add_rows(table1, 
                          heights = grobHeight(t1Caption) + padding, 
                          pos = 0)

table1 <- gtable_add_grob(table1,
                          t1Caption, t = 1, l = 2, r = ncol(data.for.table1) + 1)

grid.arrange(table1, newpage = TRUE)

#Quadrat analysis
quads <- 12

qcount <- quadratcount(kma.ppp, nx = quads, ny = quads)

plot(kma.ppp, pch = "+", cex = 0.5, main = "Quadrat Analysis for Elevation data\nin the Sooke Lake watershed")
plot(qcount, add = T, col = "red")

qcount.df <- as.data.frame(qcount)

##Second, count the number of quadrats with a distinct number of points.
qcount.df <- plyr::count(qcount.df,"Freq")

##Change the column names so that x=number of points and f=frequency of quadrats with x point.
colnames(qcount.df) <- c("x","f")


# make sure parts are going together as they should be 
sum.f.x2 <- sum((qcount.df$f)*(qcount.df$x)^2)
sum(qcount)
N <- 380
M <- 144

sum.fx.2 <- (sum((qcount.df$f)*(qcount.df$x)))^2


VAR <- ((sum.f.x2)*(sum.fx.2/M)^2)/(M-1)
VAR <- round(VAR, 0)

MEAN <- (N/M)
MEAN <- round(MEAN, 0)

VMR <- VAR/MEAN
VMR <- round(VMR, 0)


###### The K function
k.fun <- Kest(kma.ppp, correction = "Ripley")
plot(k.fun)

#use simulation to test the point pattern against CSR
k.fun.e <- envelope(kma.ppp, Kest, nsim = 99, correction = "Ripley")
plot(k.fun.e, main = "K-Function for Elevation Data\nin the Sooke Lake Watershed, 2020")

