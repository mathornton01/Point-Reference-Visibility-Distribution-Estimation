df <- read.csv("whale.csv"); 

# Plot a Histogram of the bearing for observations. 
hist(df$bearing,breaks=100,
     main = "Histogram of Vessel Bearing at Whale Sighting", 
     xlab = "Bearing of Vessel in Degrees", 
     ylab = "Frequency of Observing Whales"); 

library(MASS); 
# Plot a Histogram of the latitude and longitude for observations. 
png("contour-plot.png",height=500,width=500); 
filled.contour(kde2d(df$lon,df$lat),ylim=c(min(df$lat),max(df$lat)),xlim=c(min(df$lon),max(df$lon)),
	main = "Contour Plot Displaying Intensity of Whale Sightings in Antartic", 
	xlab = "Line of Longitude", ylab = "Line of Latitude");
dev.off(); 

library(INLA); 

## Set up a grid (discretize the observations, and count) 
res <- 0.5 # degree of lat/long
grid_bound <- list(x = seq(min(whale$lon),max(whale$lon),res),y=seq(min(whale$lat),max(whale$lat),res))

plot(whale$lon,whale$lat); 
lapply(grid_bound$x, function(i) {abline(v=i)});
lapply(grid_bound$y, function(i) {abline(h=i)});

rows <- length(grid_bound$y); 
cols <- length(grid_bound$x); 

counts_mat <- matrix(0,nrow=rows, ncol=cols);
bearings_mat <- matrix(0,nrow=rows, ncol=cols); 

respcov <- matrix(0,nrow=rows*cols, ncol=4); 

k <- 1; 

for (i in 1:(length(grid_bound$y)-1)) {
	for (j in 1:(length(grid_bound$x)-1)) {
		counts_mat[i,j] <- sum((whale$lat < grid_bound$y[i+1] & whale$lat > grid_bound$y[i]) & (whale$lon < grid_bound$x[j+1] & whale$lon > grid_bound$x[j]))
	bearings_mat[i,j] <- mean(whale[(whale$lat < grid_bound$y[i+1] & whale$lat > grid_bound$y[i]) & (whale$lon < grid_bound$x[j+1] & whale$lon > grid_bound$x[j]),]$bearing)
	respcov[k,1] <- counts_mat[i,j]; 
	respcov[k,2] <- grid_bound$y[i]; 
	respcov[k,3] <- grid_bound$x[j]; 
	respcov[k,4] <- bearings+mat[i,j]; 
	k <- k+1;
	}
}
bearings_mat <- apply(bearings_mat,2,rev) #flip 
counts_mat <- apply(counts_mat,2,rev); # flip

da <- list(whalecount = respcov[,1], lat = respcov[,2], lon = respcov[,3], ber = respcov[,4]) 

mod1 <- inla(whalecount~lat+lon, data=da,family=c("poisson"));

library(raster); 
plot(raster(counts_mat)); 

par(mfrow=c(2,3));
lapply(levels(whale$cruise_id), function(x) {contour(kde2d((whale[whale$cruise_id == x,]$lon),(whale[whale$cruise_id == x,]$lat)),xlim=c(min(whale$lon),max(whale$lon)),ylim=c(min(whale$lat),max(whale$lat)),main=x,xlab="lon",ylab="lat")});

