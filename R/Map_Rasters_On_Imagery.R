########################################################################################
#
# Plot LOOC-B generated rasters on earth imagery
#
########################################################################################

# load libraries
library(OpenStreetMap)
library(rgdal)
library(raster)

# point to the data
dat_dir <- "//fs1-cbr.nexus.csiro.au/{mmrg}/work/users/mok010/FarmAssessments/AWI/InitialAssessment/Data"
out_dir <- "//fs1-cbr.nexus.csiro.au/{mmrg}/work/users/mok010/FarmAssessments/AWI/InitialAssessment/Figures"
in_dir <- "//fs1-cbr.nexus.csiro.au/{mmrg}/work/users/mok010/FarmAssessments/AWI/Case study farms"
farm_number <- '3'
#boundary_fpath <- paste0(in_dir,"/Sheep farm regen ag 1 - Talaheni/Talaheni.json")
#boundary_fpath <- paste0(in_dir,"/Sheep farm regen ag 2 - Tolga/Tolga_guess.json")
boundary_fpath <- paste0(in_dir,"/Sheep farm regen ag 3 - Kia Ora/SheepRegenAgFarm3_KiaOra.json")
#boundary_fpath <- paste0(in_dir,"/Sheep farm regen ag 4 - Locmaria/FarmBoundary_GeoJSON_GDA94/Locmaria_all.json")

# Specify the width of the map image that ensures the legend fits in nicely (NOTE: AUTOMATE THIS)
fig_width <- 3200  #farm1=2600 #farm2=4300  #farm3=3200 #farm4=2900
# Specify the actual summed threatened species.ha (taken from L201 of LOOC-B_Analysis_API.R : response_data_summary$sum_changes$values$latest_estimate )
sum.act <- 1025.84  #farm1=1366.53  #farm2=1025.84  #farm3=148  #farm4=10.5 # NOTE: AUTOMATE THIS

## Plot condition ## CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
# read in the 2020 raster
r1<-raster::raster(paste0(dat_dir,"/condition_farm",farm_number,".tif"),
                   band = 17)
# convert to percent
r1 <-r1*100
# Get open street imagery
b <- 0.01 # buffer around raster
imagery.map <- openmap(c(r1@extent[3]-b,r1@extent[1]-b),c(r1@extent[4]+b,r1@extent[2]+b), type='bing')
imagery.map.prj <-openproj(imagery.map, projection = r1@crs)
# simple plot the imagery then the raster
#plot(imagery.map.prj)
#plot(r1, add=TRUE)

## Write a plot to file
png(paste0(out_dir,"/Condition_Map_2020_farm",farm_number,".png"), height=2000, width=fig_width)
par(mar=c(1, 1, 1, 70))
plot(imagery.map.prj)
plot(r1, 
     legend=FALSE, 
     #main=paste0('Lignum condition ',yr), 
     zlim=c(0,100), 
     col = colorRampPalette(c("lightyellow1", "green","darkblue"))(100),
     cex.axis=2.5, 
     cex.main=3.5,
     legend.mar=70,
     add=TRUE)
plot(r1, 
     legend.only=TRUE, 
     col=colorRampPalette(c("lightyellow1", "green","darkblue"))(100),
     zlim=c(0,100), 
     legend.width=2, 
     #legend.shrink=0.75,
     axis.args=list(at=seq(0, 100, 20),
                    labels=seq(0, 100, 20), 
                    cex.axis=3),
     legend.args=list(text='Habitat condition (%)', font=2, line=2.5, cex=4),
     add=TRUE)
dev.off() 

## plot the farm boundary on the imagery
farm_boundary <- readOGR(boundary_fpath)
farm_boundary <- spTransform(farm_boundary, CRSobj=r1@crs)
png(paste0(out_dir,"/Boundary_Map_2020_farm",farm_number,".png"), height=2000, width=fig_width)
par(mar=c(1, 1, 1, 70))
plot(imagery.map.prj)
plot(farm_boundary, 
     col = NA,
     border = 'cyan',
     lwd=5,
     add=TRUE)
dev.off() 

## Plot Threatened species ## TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
# read in the 2020 raster
r2<-raster::raster(paste0(dat_dir,"/threatenedspecies_farm",farm_number,".tif"),
                   band = 17)
# convert from percent of maximum to species hectares
sum.pct <- cellStats(r2, sum, na.rm=TRUE)
cell.frac <- sum.act / sum.pct
r2 <- r2*cell.frac

## Write a plot to file
cell.max <- cellStats(r2, max, na.rm=TRUE)
legend.interval <- ceiling(cell.max/5)
cell.max <- legend.interval * 5
png(paste0(out_dir,"/ThrtSpp_Map_2020_farm",farm_number,".png"), height=2000, width=fig_width)
par(mar=c(1, 1, 1, 70))
plot(imagery.map.prj)
plot(farm_boundary, 
     col = NA,
     border = 'cyan',
     lwd=5,
     add=TRUE)
plot(r2, 
     legend=FALSE, 
     #main=paste0('Lignum condition ',yr), 
     zlim=c(0,cell.max), 
     col = colorRampPalette(c("lightyellow1", "orange","darkred"))(100),
     cex.axis=2.5, 
     cex.main=3.5,
     legend.mar=70,
     add=TRUE)
plot(r2, 
     legend.only=TRUE, 
     col=colorRampPalette(c("lightyellow1", "orange","darkred"))(100),
     zlim=c(0,cell.max), 
     legend.width=2, 
     #legend.shrink=0.75,
     axis.args=list(at=seq(0, cell.max, legend.interval),
                    labels=seq(0, cell.max, legend.interval), 
                    cex.axis=3),
     legend.args=list(text='Threatened species\nhabitat\n(species.ha)', font=2, line=2.5, cex=4),
     add=TRUE)
dev.off() 

## Plot Persistence ## PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
# read in the 2020 raster
r3<-raster::raster(paste0(dat_dir,"/persistence_farm",farm_number,".tif"),
                   band = 17)
r3<-r3*100 #convert to percent

## Write a plot to file
cell.max <- ceiling(cellStats(r3, max, na.rm=TRUE))
cell.min <- floor(cellStats(r3, min, na.rm=TRUE))
legend.interval <- ceiling((cell.max-cell.min)/5)
cell.max <- cell.min + (legend.interval * 5)
png(paste0(out_dir,"/Persist_Map_2020_farm",farm_number,".png"), height=2000, width=fig_width)
par(mar=c(1, 1, 1, 70))
plot(imagery.map.prj)
plot(farm_boundary, 
     col = NA,
    border = 'cyan',
    lwd=5,
     add=TRUE)
plot(r3, 
     legend=FALSE, 
     #main=paste0('Lignum condition ',yr), 
     zlim=c(cell.min,cell.max), 
     col = colorRampPalette(c("azure", "darkblue"))(100),
     cex.axis=2.5, 
     cex.main=3.5,
     legend.mar=70,
     add=TRUE)
plot(r3, 
     legend.only=TRUE, 
     col=colorRampPalette(c("azure", "darkblue"))(100),
     zlim=c(cell.min,cell.max), 
     legend.width=2, 
     #legend.shrink=0.75,
     axis.args=list(at=seq(cell.min, cell.max, legend.interval),
                    labels=seq(cell.min, cell.max, legend.interval), 
                    cex.axis=3),
     legend.args=list(text='Plant species\npersistence\n(%)', font=2, line=2.5, cex=4),
     add=TRUE)
dev.off() 


