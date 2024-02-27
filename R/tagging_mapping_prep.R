#' @title tagging_mapping_prep
#' @description prepares all tagging data and produces useful variables, including NS basemap, for mapping tagging data
#' @description Outputs a list of mapping variables
#' @author Geraint Element
#' @export
tagging_mapping_prep <- function(){

library(dplyr)
library(lubridate)
library(sf)
#library(raster)
#library(sp)
library(grid)
library(ggmap)
library(ggsflabel)
library(ggplot2)
library(basemaps)


#Releases
d1 <- read.csv("R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/Master_data/Tag_releases20212022_master_GE.csv")
d2 <- read.csv("R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/Master_data/Tag_releases2023_master_GE.csv")

######Prepare releases
select.names = c("Vessel", "Captain", "Port", "LFA", "Sampler", "Sampler.2", "Affiliation", "Day","Month", "Year", "Tag.Prefix",	"Tag.Color", "Tag.Num",	"Carapace.Length",	"Sex",	"Shell", "Claw", "V.Notch", "Lat.Degrees",	"Lat.Minutes",	"Lon.Degrees",	"Lon.Minutes")
d1select <- d1 %>% dplyr::select(all_of(select.names))
d2select <- d2 %>% dplyr::select(all_of(select.names))
rel <- rbind(d1select,d2select)
rel$Date = paste(rel$Year, rel$Month,rel$Day,  sep = "-")
rel <- rel %>% mutate(Date = as.Date(Date))
rel <- rel %>% mutate(Date = format(Date, "%d-%b-%Y"))
rel <- rel %>% mutate(lat = Lat.Degrees + Lat.Minutes/60, lon = -(Lon.Degrees+Lon.Minutes/60))
rel <- rel %>% mutate(trip = paste(lat,lon))
rel <- rel %>% rename(TAG = Tag.Num)
#### For tagging maps, if affiliation = FSRS, then captain = FSRS
rel <- rel %>% mutate(Captain = ifelse(Affiliation %in% "FSRS","FSRS", Captain))

#Recaptures
drv <- ROracle::Oracle()
con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.server)
rec <- ROracle::dbSendQuery(con, paste("select * from ", "LOBSTER.LBT_CAPTURE", sep=""))
rec <- ROracle::fetch(rec)
ROracle::dbDisconnect(con)

###### Prepare recaptures
rec <- rec %>% mutate(lat = LAT_DD_DDDD) %>% mutate(lon = LONG_DD_DDDD)
rec <- rec %>% mutate(trip = paste(lat,lon))
rec <- rec %>% mutate(CAPTURE_DATE = as.Date(CAPTURE_DATE))
rec <- rec %>% mutate(CAPTURE_DATE = format(CAPTURE_DATE, "%d-%b-%Y"))
rec <- rec %>% rename(Year = YEAR)


#### Other data cleaning/tweaking steps:
## clean NAs, whitespace
rel.clean <- rel %>% mutate(Captain = ifelse(Captain %in% ""|Captain %in% NA,
                                             ifelse(Vessel  %in% "" | Vessel %in% NA,
                                                    Affiliation, Vessel),
                                             Captain))
# remove problematic characters
rel.clean$Captain = gsub("/","_",rel.clean$Captain)

#### tag captains that are CBFH affiliated
rel.clean <- rel.clean %>% mutate(Captain = ifelse(Affiliation %in% "CBFH",paste0("CBFH - ",Captain), Captain))
######

#### stack all data vertically for path drawing (just include recaptured tags)
stackrec <- rec%>% mutate(Date = CAPTURE_DATE) %>% dplyr::select(TAG, lat, lon, Date) %>% mutate(type = "recapture")
stackrel <- rel.clean %>% dplyr::select(TAG, lat, lon, Date) %>% mutate(type ="release")
stack_dat <- rbind(stackrec, stackrel)
stack_dat <- stack_dat %>% filter(TAG %in% rec$TAG)

################ make sf objects
rel_sf <- st_as_sf(rel.clean, coords = c("lon","lat"))
st_crs(rel_sf) <- 4326

rec_sf <- st_as_sf(rec, coords = c("lon", "lat"))
st_crs(rec_sf) <- 4326

stack_sf <- st_as_sf(stack_dat, coords = c("lon","lat"), crs = 4326)


#####loop through tags to create simple line segments and paths
line_list <- list()
simple_line_list <- list()
for (i in unique(stack_dat$TAG)){
  tag.filt <- stack_dat %>% filter(TAG  %in% i)
  tag.filt <- arrange(tag.filt, Date)
  tag.filt$lat = as.numeric(tag.filt$lat)
  tag.filt$lon =as.numeric(tag.filt$lon)
  sublist <- list()
  for (j in 1:(nrow(tag.filt)-1)){
    line <- st_linestring(matrix(c(tag.filt$lon[j],tag.filt$lon[j+1],tag.filt$lat[j],tag.filt$lat[j+1]), ncol=2))
    sublist[[j]] <- line
    simple_line_list[[paste0("tag",i,"p",j)]] <- line ## list of single line segments for doing quick summaries. 
  }
  multiline <- st_multilinestring(sublist)
  line_list[[i]] <- multiline
}

line_sf <- st_sfc(line_list, crs = 4326)
simp_line_sf <- st_sfc(simple_line_list, crs = 4362)

path_sf <- st_sf(line_sf, TAG = names(line_sf))
st_crs(path_sf) <- 4326
simp_line_sf <- st_sf(simp_line_sf)
st_crs(simp_line_sf) <- 4326

#### measure simple lengths (just individual line segments, not full paths)
simp_line_sf <- simp_line_sf %>% mutate(distance_m = st_length(simp_line_sf))



#### load NS map here
ext.NS <- readRDS("C:/Users/ELEMENTG/Documents/Tagging/NS_extent")
set_defaults(ext.NS, map_service = "mapbox",map_type = "satellite",
             map_token = "pk.eyJ1IjoiZWxlbWVudGpha2UiLCJhIjoiY2xxZmh2bGFiMHhyNTJqcHJ0cDBqcW83ZiJ9.mDd49ObNcNdG6MzH3yc2dw")

ns <- basemap_raster(ext.NS) #### forces basemap crs to be in 3857

## change back to 4326 (raster package has some masking issues with sf, so just use ::)
ns <- raster::projectRaster(ns,  crs = 4326)

## save bounding limits for graphing
limits_ns <- st_bbox(ns)

#########

##### bring in LFAs
#### For just outer lines:
LFA.line <- read_sf("C:/bio.data/bio.lobster/data/maps/LFA polylines")

### combine outputs:(rename rel.clean and rec for user simplicity)

out.list <- list(releases = rel.clean,recaptures =rec,stack_dat = stack_dat,rel_sf=rel_sf,rec_sf=rec_sf,stack_sf=stack_sf,path_sf=path_sf,simp_line_sf=simp_line_sf,ns=ns,limits_ns=limits_ns,LFA.line=LFA.line)



return(out.list)

}
