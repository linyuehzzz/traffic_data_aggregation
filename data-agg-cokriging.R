# $Id: cokriging.R,v 1.4 2006-02-10 19:05:02 edzer Exp $
library(sp)
library(gstat)
library(rgdal)
library(lattice)
library(raster)

# primary data
data_tomtom <- read.csv("data/d04_tomtom.csv", stringsAsFactors=F)
class(data_tomtom)
coordinates(data_tomtom) = ~x+y
proj4string(data_tomtom) <- CRS("+init=epsg:7131")
minx_tomtom <- min(data_tomtom$x)
miny_tomtom <- min(data_tomtom$y)

# secondary data
data_pems <- read.csv("data/d04_pems.csv", T)
class(data_pems)
data_pems <- data_pems[!is.na(data_pems$Avg_Speed),]
coordinates(data_pems) = ~x+y
proj4string(data_pems) <- CRS("+init=epsg:7131")
minx_pems <- min(data_pems$x)
miny_pems <- min(data_pems$y)

# Read grid
traffic.grid <- raster("data/d04_ras1.tif")
traffic.grid <- as(traffic.grid, "SpatialPixels")
projection(traffic.grid) <- CRS("+init=epsg:7131")
class(traffic.grid)

# cokriging of two variables
traffic.g <- gstat(id = "tomtom", formula=currentSpeed~1, data=data_tomtom, nmax=10, set=list(nocheck = 1))
traffic.g <- gstat(traffic.g, "pems", Avg_Speed~1, data_pems, nmax=10, set=list(nocheck = 1))
traffic.g <- gstat(traffic.g, model = vgm(1, "Sph", 900, 1), fill.all=T, set=list(nocheck = 1))
x <- variogram(traffic.g, cutoff=1000)
traffic.fit = fit.lmc(x, traffic.g, fit.method=2)
plot(x, model=traffic.fit)
z <- predict(traffic.fit, newdata=traffic.grid)

pl1 <- spplot(z["tomtom.pred"], main="Traffic speed prediction")
pl2 <- spplot(z["pems.pred"], main="pems predictions")
print(pl1)
print(pl2)
z$tomtom.se = sqrt(z$tomtom.var)
z$pems.se = sqrt(z$pems.var)
pl1 <- spplot(z["tomtom.se"], main="Traffic speed std.err.")
pl2 <- spplot(z["pems.se"], main="pems std.err.")
print(pl1)
print(pl2)
rm(traffic.g, x, traffic.fit, z)

