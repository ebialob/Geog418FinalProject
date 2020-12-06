####Geographically Weighted Regression
#Let's say you are continuing with 
#your data from the regression analysis. 
#The first thing you need to do is to add the 
#polygon coordinates to the spatialpolygondataframe.
#You can obtain the coordinates using the 
#"coordinates" function from the sp library
VRI.no0.coords <- sp::coordinates(VRI.no0)
#Observe the result:
head(VRI.no0.coords)
#Now add the coordinates back to the spatialpolygondataframe
VRI.no0$X <- VRI.no0.coords[,1]
VRI.no0$Y <- VRI.no0.coords[,2]

View(VRI.no0.coords)
head(VRI.no0.coords)
length(VRI.no0.coords)
###Determine the bandwidth for GWR: this will take a while
# like searching for a sigma value, bandwidth tests the neighbourhoods, sizes of bandwidth, for regression
GWRbandwidth <- gwr.sel(VRI.no0$Stand_StemBio ~ VRI.no0$Elev, 
                        data=VRI.no0, coords=cbind(VRI.no0$X,VRI.no0$Y),adapt=T) 
#start at 1008, finish 1016
#result was 0.00073313

###Perform GWR on the two variables with the bandwidth determined above
gwr.model = gwr(VRI.no0$Stand_StemBio ~ VRI.no0$Elev, 
                data=VRI.no0, coords=cbind(VRI.no0$X,VRI.no0$Y), 
                adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 

#start at 1025 end at 1109
# warnings : In UseMethod("depth") :
#no applicable method for 'depth' applied to an object of class "NULL"

#Print the results of the model
gwr.model

#Look at the results in detail
results<-as.data.frame(gwr.model$SDF)
head(results)
names(results)
#Now for the magic. Let's add our local r-square values to the map
VRI.no0$localr <- results$localR2

View(VRI.no0@data)
#Create choropleth map of r-square values
map_r2 <- tm_shape(VRI.no0) +
  tm_polygons(col = "localr",
              title = "R2 values",
              style = "jenks",
              palette = "Reds", n = 6)+
  tm_legend(legend.position = c("LEFT", "BOTTOM"))+
  tm_layout(panel.labels = "Local R")+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"))

map_r2

View(results@data)
#Time for more magic. Let's map the coefficients
VRI.no0$coeff <- results$VRI.no0.Elev

#Create choropleth map of the coefficients
# vrino0 elev
#y = mx +b  
#something times x is the coeffient, each polygon get its own slope
palette_explorer()
map_coef <- tm_shape(VRI.no0) +
  tm_polygons(col = "coeff",
              title = "Coefficient",
              style = "fixed", breaks = c(-247, -5, 0, 5, 247), midpoint = NA,
              palette = "-RdBu", n = 4, contrast = c(0, 0.7), border.col = "darkgrey", lwd = 0.3)+
  tm_layout(panel.labels = "Coefficients of Stand Stem Biomass GWR\n in the Sooke Lake Watershed",
            panel.label.height = 3, panel.label.size = 2.2, inner.margins = 0.05)+
  tm_legend(legend.outside=FALSE, title.size = 2, legend.text.size = 1.5, legend.position = c("LEFT", "BOTTOM"))+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"))

map_coef

map_coef2 <- tm_shape(VRI.no0) +
  tm_polygons(col = "coeff",
              title = "Coefficient",
              style = "quantile", midpoint = NA,
              palette = "-RdBu", n = 4, contrast = c(0, 0.7), border.col = "darkgrey", lwd = 0.3)+
  tm_layout(panel.labels = "Coefficients of Stand Stem Biomass GWR\n in the Sooke Lake Watershed",
            panel.label.height = 3, panel.label.size = 2.2, inner.margins = 0.05)+
  tm_legend(legend.outside=FALSE, title.size = 2, legend.text.size = 1.5, legend.position = c("LEFT", "BOTTOM"))+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"))

map_coef2
#the breaks from the quantile map, -246.0 to -0.7, -0.7 to 0, 0 to 0.5, 0.5 to 50.5

map_coef3 <- tm_shape(VRI.no0) +
  tm_polygons(col = "coeff",
              title = "Coefficient",
              style = "fixed", breaks = c(-Inf, -0.7, 0, 0.7, Inf), midpoint = NA,
              palette = "-RdBu", n = 4, contrast = c(0, 0.7), border.col = "darkgrey", lwd = 0.3)+
  tm_layout(panel.labels = "Coefficients of Stand Stem Biomass GWR\n in the Sooke Lake Watershed",
            panel.label.height = 3, panel.label.size = 2.2, inner.margins = 0.05)+
  tm_legend(legend.outside=FALSE, title.size = 2, legend.text.size = 1.5, legend.position = c("LEFT", "BOTTOM"))+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"))

map_coef3

map_coef4 <- tm_shape(VRI.no0) +
  tm_polygons(col = "coeff",
              title = "Coefficient",
              style = "fixed", breaks = c(-Inf, -30, -0.7, 0, 0.7, 30, Inf), midpoint = NA,
              palette = "-RdBu", n = 6, contrast = c(0, 0.78), border.col = "darkgrey", lwd = 0.3)+
  tm_layout(panel.labels = "Coefficients of Stand Stem Biomass GWR\n in the Sooke Lake Watershed",
            panel.label.height = 3, panel.label.size = 2.2, inner.margins = 0.05)+
  tm_legend(legend.outside=FALSE, title.size = 2, legend.text.size = 1.5, legend.position = c("LEFT", "BOTTOM"))+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"))

map_coef4

tmap_mode("plot")

#histogram for the local r
gwrresultdata<-as.data.frame(VRI.no0)
histogramLocalRStem <- ggplot(gwrresultdata, aes(x = localr)) + #Create new GGplot object with data attached and fire size mapped to X axis
  geom_histogram(bins = 25, color = "black", fill = "white") + #make histogram with 30 bins, black outline, white fill
  labs(title = "Histogram of Stemwood Biomass GWR Local R Squared\nin Sooke Lake Watershed, 2020", x = "Local R Squared", y = "Frequency") + #label plot, x axis, y axis
  theme_classic() + #set the theme to classic (removes background and borders etc.)
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.caption = element_text(hjust = 0.5)) + #set title to center and bold
  scale_y_continuous(breaks = seq(0, 2500, by = 100))+
  scale_x_continuous(breaks = seq(-1, 1, by = 0.1)) # set y axis labels to 0 - 700 incrimenting by 100

print(histogramLocalRStem)

mean(VRI.no0$localr)
######## Spatial autocorrelation in the coefficients of the GWR
vri.nbresid <- poly2nb(VRI.no0)
?nb2lines
vri.netresid <- nb2lines(vri.nbresid, coords=coordinates(VRI.no0))
crs(vri.netresid) <- crs(VRI.no0)

vri.lwresid <- nb2listw(vri.nbresid, zero.policy = TRUE, style = "W")
print.listw(vri.lw, zero.policy = TRUE)

miStand_coeffResid <- moran.test(VRI.no0$residuals, vri.lwresid, zero.policy = TRUE)
miStand_coeffResid


mIresid <- miStand_coeffResid$estimate[[1]]
eIresid <- miStand_coeffResid$estimate[[2]]
varresid <- miStand_coeffResid$estimate[[3]]

zresid <- (mIcoeff -eIcoeff) / sqrt(varcoeff)
# GWR coefficients are clustered in the watershed
