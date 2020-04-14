# $Id: cokriging.R,v 1.4 2006-02-10 19:05:02 edzer Exp $
library(sp)
library(gstat)
library(rgdal)
library(lattice)
library(raster)

# primary data
data_tomtom <- read.csv("data/d04_tomtom.csv", T)
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
st_grid <- raster("data/d04_ras1.tif")
st_grid <- as(st_grid, "SpatialPixels")
projection(st_grid) <- CRS("+init=epsg:7131")
class(st_grid)

# cokriging of two variables
traffic.g <- gstat(id = "tomtom", formula=log(currentSpeed)~1, data=data_tomtom, nmax=10, set=list(nocheck = 1))
traffic.g <- gstat(traffic.g, "pems", log(Avg_Speed)~1, data_pems, nmax = 10, set=list(nocheck = 1))
traffic.g <- gstat(traffic.g, model = vgm(1, "Sph", 900, 1), fill.all=T, set = list(nocheck = 1))
x <- variogram(traffic.g, cutoff=1000)
traffic.fit = fit.lmc(x, traffic.g, fit.method = 2)
plot(x, model = traffic.fit)
z <- predict(meuse.fit, newdata = meuse.grid)



data(meuse)
coordinates(meuse) = ~x+y
data(meuse.grid)
gridded(meuse.grid) = ~x+y
xyplot(y~x, meuse.grid, asp="iso", pch="+")

# cokriging of the four heavy metal variables
meuse.g <- gstat(id = "zn", formula=log(zinc)~1, data=meuse, nmax=10, set=list(nocheck = 1))
meuse.g <- gstat(meuse.g, "cu", log(copper)~1, meuse, nmax = 10, set=list(nocheck = 1))
meuse.g <- gstat(meuse.g, model = vgm(1, "Sph", 900, 1), fill.all=T, set = list(nocheck = 1))
x <- variogram(meuse.g, cutoff=1000)
meuse.fit = fit.lmc(x, meuse.g)
plot(x, model = meuse.fit)
z <- predict(meuse.fit, newdata = meuse.grid)

pl1 <- spplot(z["zn.pred"], main="log-zinc predictions")
pl2 <- spplot(z["cu.pred"], main="log-copper predictions")
print(pl1, split = c(1,1,2,2), more=TRUE)
print(pl2, split = c(1,2,2,2))
z$zn.se = sqrt(z$zn.var)
z$cu.se = sqrt(z$cu.var)
pl1 <- spplot(z["zn.se"], main="log-zinc std.err.")
pl2 <- spplot(z["cu.se"], main="log-copper std.err.")
print(pl1, split = c(1,1,2,2), more=TRUE)
print(pl2, split = c(1,2,2,2))
rm(meuse.g, x, meuse.fit, z)

# indicator cokriging for the 9 percentiles of zinc:
q <- quantile(meuse$zinc, seq(.1,.9,.1))
meuse.i <- gstat(id = "zn1", formula = I(zinc < q[1])~1, 
	data = meuse, nmax = 7, beta = .1, set = list(order = 4, zero = 1e-5, nocheck = 1))
meuse.i <- gstat(meuse.i, "zn2", I(zinc < q[2])~1, meuse, nmax = 7, beta=.2, set = list(nocheck = 1))
meuse.i <- gstat(meuse.i, "zn3", I(zinc < q[3])~1, meuse, nmax = 7, beta=.3, set = list(nocheck = 1))
meuse.i <- gstat(meuse.i, "zn4", I(zinc < q[4])~1, meuse, nmax = 7, beta=.4, set = list(nocheck = 1))
meuse.i <- gstat(meuse.i, "zn5", I(zinc < q[5])~1, meuse, nmax = 7, beta=.5, set = list(nocheck = 1))
meuse.i <- gstat(meuse.i, "zn6", I(zinc < q[6])~1, meuse, nmax = 7, beta=.6, set = list(nocheck = 1))
meuse.i <- gstat(meuse.i, "zn7", I(zinc < q[7])~1, meuse, nmax = 7, beta=.7, set = list(nocheck = 1))
meuse.i <- gstat(meuse.i, "zn8", I(zinc < q[8])~1, meuse, nmax = 7, beta=.8, set = list(nocheck = 1))
meuse.i <- gstat(meuse.i, "zn9", I(zinc < q[9])~1, meuse, nmax = 7, beta=.9, set = list(nocheck = 1))
meuse.i <- gstat(meuse.i, model=vgm(1, "Sph", 900, 1), fill.all=T, set = list(nocheck = 1))
x <- variogram(meuse.i, cutoff=1000)
meuse.fit = fit.lmc(x, meuse.i)
plot(x, model = meuse.fit)
z <- predict(meuse.fit, newdata = meuse.grid)
spplot(z, c(3,5,7,9,11,13,15,17,19), 
	names.attr = paste("est.Pr(Zn < ", q, ")", sep = ""))

