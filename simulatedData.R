# Synthetic Data for Testing Bikeshare Elevation Project
# Nathan Danneman
# Created 21August2016
# Last Edited 21August2016

require(lattice)

# N stations in R2:
N <- 25
lon <- runif(N, 0, 10) +1
lat <- runif(N, 0, 10) +1
# elevation increase that is linear in lat, quadratic in lon
elev <- lon*lon*.2 + lat + rnorm(N, 0, 2)

# Show station locations on this hill:
x <- seq(from=1, to=10, length.out=20)
y <- seq(from=1, to=10, length.out=20)
z <- matrix(0, nrow=length(x), ncol=length(y))
for(i in 1:length(x)){
  for(j in 1:length(y)){
    z[i,j] <- x[i]*x[i]*.2 + 1.5*y[j]
  }}
image(z,x=x,y=y, xlab="lon", ylab="lat", main="Stations on a Hill")
points(lon, lat, pch=19)

# Data Generating Process:
# People show up at a station randomly, and consider a trip to another station
# They consider distance and elevation change
# Then make a probabilistic decision

# assumes: people like to bike (positive intercept)
#   distance hurts
#   elevation hurts
prTrip <- function(dist, elevDelta){
  xb <- 2 -.2*dist - .1*elevDelta
  out <- exp(xb) / (1+exp(xb))
  out
}

start <- sample(1:N, 1)
goal <- sample((1:N)[-start], 1)
dist <- sqrt((lon[start]-lon[goal])^2 + (lat[start]-lat[goal])^2)
points(lon[start], lat[start], col="green")
points(lon[goal], lat[goal], col="magenta")
elevDelta <- elev[goal] - elev[start]







