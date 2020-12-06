#These steps will help you combine the outputs 
#from your spatial interpolation with your income data.
#Convert your interpolation into a raster and map it:

data.class(r.m4)
# it is already a raster
# Surface map is found on dataprep page for IDW = 4

#Extract average elev for each polygon
#takes a while to run
vriClean$Elev <- extract(r.m4, vriClean, fun = mean)[,1]

View(vriClean@data)

#Checking stand stem bio against other variables for limitations of the analysis

plot(vriClean$Stand_StemBio ~ vriClean$Stand_Age)
plot(vriClean$Stand_StemBio ~ vriClean$Stand_Dens)

#some polygons have height of 10m but a stand stem biomass of zero
#limitation for discussion
plot(vriClean$Stand_StemBio ~ vriClean$Stand_HT)
