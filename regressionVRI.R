######Linear Regression##########
#Let's say your dataset with both Elev and Height are stored in a dataset called VRI.
#Plot Height and Elev from the VRI dataset you created
plot(vriClean$Stand_StemBio ~ vriClean$Elev)
#dependent is the biological variable

#Notice that there are a lot of 0's in this dataset. If you decide to remove them, use the following line:
VRI.no0 <-  vriClean[which(vriClean$Stand_StemBio > 0), ]
VRI.no0 <-  VRI.no0[which(VRI.no0$Elev > 0), ]

VRI.no0
#Now plot the data again
plot(VRI.no0$Stand_StemBio ~ VRI.no0$Elev)

#Perform a linear regression on the two variables. You should decide which one is dependent.
lm.model <- lm(VRI.no0$Stand_StemBio ~ VRI.no0$Elev)

#Add the regression model to the plot you created
plot(VRI.no0$Stand_StemBio ~ VRI.no0$Elev, col = "grey15", ylab = "Stand Stem Biomass [tonnes/HA]",
     xlab = "Elevation [m]", main = "Linear Regression Model for Stand Stem Biomass\nand Elevation in the Sooke Lake Watershed")
abline(lm.model, col = "red", lwd = 3.5)
text(700, 700, "y = -0.26x + 260.2\nR^2: 0.13")

#Get the summary of the results
summary(lm.model)

#add the fitted values to your spatialpolygon dataframe
VRI.no0$predictlm <- lm.model$fitted.values

#You want to determine if the model residuals are spatially clustered. 
#add the residuals to your spatialpolygon dataframe
# look for independence in the regression residuals - Global Moran's I on the residuals out of the model
# are the residuals randomly distributed across the study area?
VRI.no0$residuals <- residuals.lm(lm.model)

#Observe the result to make sure it looks correct
head(VRI.no0@data)

#Now, create choropleth map of residuals
map_resid <- tm_shape(VRI.no0) +
  tm_polygons(col = "residuals",
              title = "Stand Stem Biomass Residuals",
              style = "jenks",
              palette = "Reds", n = 6)+
  tm_legend(legend.outside=FALSE, title.size = 1.1, legend.position = c("LEFT", "BOTTOM"))+
  tm_scale_bar(position = c("left", "bottom"))+
  tm_layout(panel.labels = "Residuals map of Stand Stem Biomass", panel.label.height = 3, inner.margins = 0.1) +
  tm_compass(position = c("right", "top"))

map_resid

# Doing a Global Moran's I of the residuals
vri.nbresid <- poly2nb(VRI.no0)
?nb2lines
vri.netresid <- nb2lines(vri.nbresid, coords=coordinates(VRI.no0))
crs(vri.netresid) <- crs(VRI.no0)

vri.lwresid <- nb2listw(vri.nbresid, zero.policy = TRUE, style = "W")
print.listw(vri.lw, zero.policy = TRUE)

miStand_StemResid <- moran.test(VRI.no0$residuals, vri.lwresid, zero.policy = TRUE)
miStand_StemResid
#2.9093e-01, expectation -2.16e-04

mIresid <- miStand_StemResid$estimate[[1]]
eIresid <- miStand_StemResid$estimate[[2]]
varresid <- miStand_StemResid$estimate[[3]]

zresid <- (mIresid -eIresid) / sqrt(varresid)
#z score is 29.22, so it is significantly clustered. must do the GWR
##################################################