#############################################################################################
##
## Run LOOC-B analyses via API 
##
## Karel Mokany - 10 November 2022
##
##
#############################################################################################

# load libraries
library(httr)
library(jsonlite)
library(getPass)
library(base64enc)
library(RCurl)
library(png)
library(terra)
library(ggplot2)
#library(OpenStreetMap)
#library(rJava)
library(rgdal)
library(raster)

# Point to the LOOC-B url
# url = "https://api.looc-b.farm"  #production
url = "https://dev.api.looc-b.farm"  #development

token = getPass::getPass() # after running this line, paste into the dialogue box that appears your current session token from the looc-b UI 

# set the output data directory, figure directory and farm number
dat_dir <- "//fs1-cbr.nexus.csiro.au/{mmrg}/work/users/mok010/FarmAssessments/AWI/InitialAssessment/Data"
out_dir <- "//fs1-cbr.nexus.csiro.au/{mmrg}/work/users/mok010/FarmAssessments/AWI/InitialAssessment/Figures"
farm_number <- '3'

# Read the geojson file into a character
#fileName = "//fs1-cbr.nexus.csiro.au/{mmrg}/work/users/mok010/FarmAssessments/AWI/Case study farms/Sheep farm regen ag 1 - Talaheni/Talaheni.json"
#fileName = "//fs1-cbr.nexus.csiro.au/{mmrg}/work/users/mok010/FarmAssessments/AWI/Case study farms/Sheep farm regen ag 2 - Tolga/Tolga_guess.json"
fileName = "//fs1-cbr.nexus.csiro.au/{mmrg}/work/users/mok010/FarmAssessments/AWI/Case study farms/Sheep farm regen ag 3 - Kia Ora/SheepRegenAgFarm3_KiaOra.json"
#fileName = "//fs1-cbr.nexus.csiro.au/{mmrg}/work/users/mok010/FarmAssessments/AWI/Case study farms/Sheep farm regen ag 4 - Locmaria/FarmBoundary_GeoJSON_GDA94/Locmaria_all.json"
feature2=readChar(fileName, file.info(fileName)$size)

endpoint = 'monitoring'

params = list("polygon"= feature2, 
              "mode"= 'monitoring',
              "start_year"= 2004, 
              "end_year"= 2020,
              "action_year"= 2004,
              "dataset" = list("habitat-condition"),
              "download_geotiff" = 'true') # add 

auth = paste("Bearer", token)

response = POST(paste0(url, '/', endpoint, '?'),
               add_headers("Content-Type" = "application/json",
                            Accept="application/+json",
                            "Authorization" = auth),
                body = toJSON(params,auto_unbox = TRUE), 
                encode = 'json')
print(response$status_code)

# isolate the data from the returned info
response_data = content(response, 'parsed')
# see the structure of the returned data
#str(response_data$datasets)

# see the elements in the dataset list
#names(response_data$dataset[[1]])

# Get the summary
#response_data$dataset[[1]]['summary']

# Get the annual averages
ts.dat.vec <- unlist(response_data$dataset[[1]]["annual_averages"])
# revised condition data
ts.dat.hcon.df <- data.frame('year' = c(2004:2020),
                        'mean.condition' = ts.dat.vec[c(1:17)],
                        'perc.5th' = ts.dat.vec[c(18:34)],
                        'perc.95th' = ts.dat.vec[c(35:51)])
# convert to percents
ts.dat.hcon.df[,c(2:4)] <- ts.dat.hcon.df[,c(2:4)] * 100
# plot it
png(paste0(out_dir,"/Condition_Timeseries_Plot_Farm",farm_number,".png"), height=1000, width=1000)
ggplot(ts.dat.hcon.df, aes(x=year, y=mean.condition, group=1, ymin = perc.5th, ymax = perc.95th)) +
  geom_ribbon(alpha = 0.2) +
  geom_line(size=2) +
  xlab("Year")+
  ylab("Habitat condition (%)")+
  scale_x_continuous(limits = c(2004,2020), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,max(ts.dat.hcon.df$perc.95th)), expand = c(0, 0)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.2),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(1,1,1,1,'cm'),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(color = "black", size = 20),
        axis.title.y = element_text(color = "black", size = 30, vjust = +2.5, face="bold"),
        axis.title.x = element_text(color = "black", size = 30, vjust = -0.3, face="bold"),
        axis.text.x = element_text(color = "black", size = 20,vjust = -1),
        axis.ticks.x = element_line(size=1),
        legend.text = element_text(size = rel(2.2)),
        legend.title = element_text(size = rel(2.2)),
        legend.key.size = unit(1, 'cm'))
dev.off()

# Get the 'figure'  - NOTE _ I HAVEN"T SOLVED THIS ONE YET - What is this format? docs say "base64 encoded PNG" [it's html; write to html then convert to png]
#response_data$dataset[[1]]["figure"]
# write it to file 
#raw <- base64enc::base64decode(what = response_data$dataset[[1]]["figure"][[1]])
#png::writePNG(png::readPNG(raw), "//fs1-cbr.nexus.csiro.au/{mmrg}/work/users/mok010/FarmAssessments/AWI/InitialAssessment/Figures/TrendFig.png")

# Get the GeoTiff data figures
#zz <-response_data$dataset[[1]]["geotiff"]
# write to  file 
#raw <- base64enc::base64decode(what = zz$geotiff$images$`2012`)
#png::writePNG(png::readPNG(raw), "//fs1-cbr.nexus.csiro.au/{mmrg}/work/users/mok010/FarmAssessments/AWI/InitialAssessment/Figures/map2012b.png")

# Get the GeoTiff data
# download the raster data from the url (manually for now)
datalayers <-response_data$dataset[[1]]["geotiff"]
#datalayers$geotiff$url
download.file(datalayers$geotiff$url,
             destfile=paste0(dat_dir,"/condition_farm",farm_number,".tif"),
             method="wininet",  
             mode = "wb",
             overwrite=TRUE)  
# r1<-raster::raster("//fs1-cbr.nexus.csiro.au/{mmrg}/work/users/mok010/FarmAssessments/AWI/InitialAssessment/Data/test1.tif",
#                   band = 17)
# plot(r1)
# Get and query the data
#r1.df <- as.data.frame(r1[[17]])
# Get the number of cells with habitat condition > 0.5
#length(which(r1.df[,1]>0.5))
# Plot against earth imagery [ see:  https://www.r-bloggers.com/2012/03/plot-maps-like-a-boss/ ]


#### Run threatened species query #### TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
params = list("polygon"= feature2, 
              "mode"= 'monitoring',
              "start_year"= 2004, 
              "end_year"= 2020,
              "action_year"= 2004,
              "dataset" = list("threatened-species"),
              "download_geotiff" = 'true') # add 

auth = paste("Bearer", token)

response = POST(paste0(url, '/', endpoint, '?'),
                add_headers("Content-Type" = "application/json",
                            Accept="application/+json",
                            "Authorization" = auth),
                body = toJSON(params,auto_unbox = TRUE), 
                encode = 'json')
print(response$status_code)

# isolate the data from the returned info
response_data = content(response, 'parsed')

# Get the annual averages
ts.dat.vec <- unlist(response_data$dataset[[1]]["annual_averages"])
# revised data
ts.dat.thrt.df <- data.frame('year' = c(2004:2020),
                        'species.hectares' = ts.dat.vec[c(1:17)])
# plot it
png(paste0(out_dir,"/ThrtSpp_Timeseries_Plot_Farm",farm_number,".png"), height=1000, width=1000)
ggplot(ts.dat.thrt.df, aes(x=year, y=species.hectares)) +
  geom_line(size=2) +
  xlab("Year")+
  ylab("Threatened species habitat (species.ha)")+
  scale_x_continuous(limits = c(2004,2020), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,max(ts.dat.thrt.df$species.hectares)), expand = c(0, 0)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.2),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(1,1,1,1,'cm'),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(color = "black", size = 20),
        axis.title.y = element_text(color = "black", size = 30, vjust = +2.5, face="bold"),
        axis.title.x = element_text(color = "black", size = 30, vjust = -0.3, face="bold"),
        axis.text.x = element_text(color = "black", size = 20,vjust = -1),
        axis.ticks.x = element_line(size=1),
        legend.text = element_text(size = rel(2.2)),
        legend.title = element_text(size = rel(2.2)),
        legend.key.size = unit(1, 'cm'))
dev.off()

# download the raster data from the url (manually for now)
datalayers <-response_data$dataset[[1]]["geotiff"]
#datalayers$geotiff$url
download.file(datalayers$geotiff$url,
              destfile=paste0(dat_dir,"/threatenedspecies_farm",farm_number,".tif"),
              method="wininet",  
              mode = "wb",
              overwrite=TRUE)  

# get 2020 summed species.ha for grid conversion
response_data_summary <- response_data$dataset[[1]]['summary']
response_data_summary <- response_data_summary$summary
response_data_summary$sum_changes$values$latest_estimate 
# The above value gets used in the mapping code, to convert the threatened species habitat map from % of the maximum to 
#  the actual units (species hectares)


#### Run species persistence query #### TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
params = list("polygon"= feature2, 
              "mode"= 'monitoring',
              "start_year"= 2004, 
              "end_year"= 2020,
              "action_year"= 2004,
              "dataset" = list("biodiversity-persistence"),
              "download_geotiff" = 'true') # add 

auth = paste("Bearer", token)

response = POST(paste0(url, '/', endpoint, '?'),
                add_headers("Content-Type" = "application/json",
                            Accept="application/+json",
                            "Authorization" = auth),
                body = toJSON(params,auto_unbox = TRUE), 
                encode = 'json')
print(response$status_code)

# isolate the data from the returned info
response_data = content(response, 'parsed')

# Get the annual averages
pers.dat <- response_data$dataset[[1]]["annual_averages"]
pers.dat <- pers.dat$annual_averages
# revised data
ts.dat.pers.df <- data.frame('year' = c(2004:2020),
                             'persistence' = unlist(pers.dat$biodiversity_persistence$mean),
                             'pristine'= unlist(pers.dat$biodiversity_persistence_pristine$mean),
                             'degraded'=unlist(pers.dat$biodiversity_persistence_degraded$mean),
                             'noaction'=unlist(pers.dat$biodiversity_persistence_noaction$mean))
# calculate benefit of on-farm habitat for species persistence
ts.dat.pers.df$benefit <- ts.dat.pers.df$persistence - ts.dat.pers.df$degraded
# plot it
png(paste0(out_dir,"/Persist_Timeseries_Plot_Farm",farm_number,".png"), height=1000, width=1000)
ggplot(ts.dat.pers.df, aes(x=year, y=benefit)) +
  geom_line(size=2) +
  xlab("Year")+
  ylab("Benefit for plant species persistence (species)")+
  scale_x_continuous(limits = c(2004,2020), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,max(ts.dat.pers.df$benefit)),expand = c(0, 0)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.2),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(1,1,1,1,'cm'),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(color = "black", size = 20),
        axis.title.y = element_text(color = "black", size = 30, vjust = +2.5, face="bold"),
        axis.title.x = element_text(color = "black", size = 30, vjust = -0.3, face="bold"),
        axis.text.x = element_text(color = "black", size = 20,vjust = -1),
        axis.ticks.x = element_line(size=1),
        legend.text = element_text(size = rel(2.2)),
        legend.title = element_text(size = rel(2.2)),
        legend.key.size = unit(1, 'cm'))
dev.off()

# download the raster data from the url (manually for now) # NOTE THERE IS A BUG IN THE PERSISTENCE GEOTIFFS
datalayers <-response_data$dataset[[1]]["geotiff"]
#datalayers$geotiff$url
download.file(datalayers$geotiff$url,
              destfile=paste0(dat_dir,"/persistence_farm",farm_number,".tif"),
              method="wininet",  
              mode = "wb",
              overwrite=TRUE)  

## Combine the outputs for each aspect over time and write to csv ## OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
out.df <- data.frame('year'=ts.dat.hcon.df$year,
                     'mean.habitat.condition'=ts.dat.hcon.df$mean.condition,
                     'equivalent.habitat.area'=((ts.dat.hcon.df$mean.condition/100)*response_data$assessment_area),
                     'threatened.species.habitat'=ts.dat.thrt.df$species.hectares,
                     'benefit.for.species.persistence'=ts.dat.pers.df$benefit)
write.csv(out.df,paste0(dat_dir,"/Summary_Table_Farm",farm_number,".csv"),row.names = FALSE)





