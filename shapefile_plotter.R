rm(list = ls())
cat("\014")

# Required packages
libs <- c("rgdal", "maptools", "gridExtra", "gpclib", "PBSmapping", "rgeos", 
          "ggplot2", "ggmap")
lapply(libs, require, character.only = TRUE)
gpclibPermit()

# load geo data
file <- "RoutingKeys_ITM_region"
setwd("C:/Users/loughnge/Dropbox/Maps/Complete_Set_Of_SAPS_2011/Polygons/routingkeys_shape_itm_2016_09_29/")

# print info from shapefile
# EC_info <- ogrInfo(".", file)
# read in shapefile
EC_temp <- readOGR(".", file)
# projections
print(proj4string(EC_temp))
# convert
EC_temp <- spTransform(EC_temp, CRS("+proj=longlat +lat_0=53.5 +lon_0=-8"))

geo_eircodes <- as.vector(EC_temp@data$RoutingKey)

# fortify
EC_temp <- fortify(EC_temp)
# change ID to numeric in order to use it as a scale. won't need this after I get property prices
# EC_temp$id <- as.numeric(EC_temp$id)
EC_temp$MedPrices <- NA

# load previous property data frame
load("C:/Users/loughnge/Dropbox/Maps/Eircode Prices.Rda")

median_prices <- integer()
for (i in 1:length(geo_eircodes)) {
  median_prices[i] <- mean(property_prices[property_prices[, 1] == geo_eircodes[i], 9], na.rm = TRUE)
  EC_temp[EC_temp$id == i-1, 8] <- median_prices[i]
}

# plot(EC_temp, axes=TRUE, border="gray")

# dublin_center = as.numeric(geocode("Dublin Ireland", source = "google"))
# dublinMap = ggmap(get_googlemap(center=dublin_center, scale=2, zoom=10), extent="normal")

ireland_center = as.numeric(geocode("Ireland", source = "google"))
ireland_center[1] = ireland_center[1]-0.5
ireland_center[2] = ireland_center[2]+0.37
irelandMap = ggmap(get_googlemap(center=ireland_center, scale=2, zoom=7), extent="normal")

irelandMap +
  geom_polygon(data=EC_temp, mapping=aes(x=long, y=lat, fill=MedPrices, group=group), 
               size=.2, color='black', alpha=0.5) +
  scale_fill_gradient(name = "Price", low = "blue", high = "red")
