#' @title tagging_mapping_prep
#' @description preps tagging data for mapping analysis (sf, raster, satellite basemaps, etc)
#' @description Outputs various sf objects and basemaps to begin mapping.
#' @param releases.data.path Get release data from specified path (gets recaptures from Oracle).
#' @author Geraint Element
#' @import dplyr ggplot2 sf lubridate basemaps raster
#' @export

tagging_mapping_prep = function(){

library(dplyr)
library(ggplot2)
library(sf)
library(lubridate)
##releases
d1 <- read.csv("R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/Master_data/Tag_releases20212022_master_GE.csv")
d2 <- read.csv("R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/Master_data/Tag_releases2023_master_GE.csv")

#pull recaptures directly from oracle
drv <- ROracle::Oracle()
con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.server)
rec <- ROracle::dbSendQuery(con, paste("select * from ", "LOBSTER.LBT_CAPTURE", sep=""))
rec <- ROracle::fetch(rec)
ROracle::dbDisconnect(con)
rec <- rec %>% mutate(lat = as.numeric(LAT_DD_DDDD)) %>% mutate(lon = as.numeric(LONG_DD_DDDD))
rec$CAPTURE_DATE = as.Date(rec$CAPTURE_DATE)
rec_og <- rec ### for later
#################
################

###prepare and merge datasets
######releases
select.names = c("Vessel", "Captain", "Port", "LFA", "Sampler", "Sampler.2", "Affiliation", "Day","Month", "Year", "Tag.Prefix",	"Tag.Color", "Tag.Num",	"Carapace.Length",	"Sex",	"Shell", "Claw", "V.Notch", "Lat.Degrees",	"Lat.Minutes",	"Lon.Degrees",	"Lon.Minutes")
d1select <- d1 %>% dplyr::select(all_of(select.names))
d2select <- d2 %>% dplyr::select(all_of(select.names))
releases <- rbind(d1select,d2select)
releases$Date = paste(releases$Year, releases$Month,releases$Day,  sep = "-")
releases <- releases %>% mutate(lat = Lat.Degrees + Lat.Minutes/60, lon = -(Lon.Degrees+Lon.Minutes/60))
releases <- releases %>% rename(TAG = Tag.Num)
releases$TAG = as.character(releases$TAG)
releases$Year = as.factor(releases$Year)

### recaptures
rec <- rec %>% rename(captured.by = PERSON, capture.carapace = CARAPACE_L, captured.by_B = PERSON_B, capture.captain = CAPTAIN, 
                      capture.vessel = VESSEL, capture.year = YEAR,
                      capture.sex = SEX, capture.egg = EGG, capture.tag.prefix = TAG_PREFIX, capture.lat = lat, capture.lon = lon)
rec <- rec %>% dplyr::select(-LAT_DDMM_MM,-LONG_DDMM_MM,-LAT_DD_DDDD,-LONG_DD_DDDD)


alldat <-  left_join(rec, releases)
alldat <- alldat %>% arrange(TAG)
alldat$Year = as.factor(alldat$Year)

#### stack all data vertically
stackrec <- rec_og %>% mutate(Date = CAPTURE_DATE) %>% dplyr::select(TAG, lat, lon, Date) %>% mutate(type = "recapture")
stackrel <- releases %>% dplyr::select(TAG, lat, lon, Date) %>% mutate(type ="release")
stack_dat <- rbind(stackrec, stackrel)
stack_dat <- stack_dat %>% filter(TAG %in% rec$TAG)
################ make sf objects
rel_sf <- st_as_sf(releases, coords = c("lon","lat"))
st_crs(rel_sf) <- 4326

rec_sf <- st_as_sf(rec, coords = c("capture.lon", "capture.lat"))
st_crs(rec_sf) <- 4326

stack_sf <- st_as_sf(stack_dat, coords = c("lon","lat"), crs = 4326)

#####loop through tags to create line segements
line_list <- list()
simple_line_list <- list()
for (i in unique(stack_dat$TAG)){
  tag.filt <- stack_dat %>% filter(TAG  %in% i)
  tag.filt <- arrange(tag.filt, Date)
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

#stack_sf$geometry <- line_sf

path_sf <- st_sf(line_sf, TAG = names(line_sf))
st_crs(path_sf) <- 4326
simp_path_sf <- st_sf(simp_line_sf)
st_crs(simp_path_sf) <- 4326

#### measure simple lengths (just individual line segments, not full paths)
simp_path_sf <- simp_path_sf %>% mutate(distance_m = st_length(simp_line_sf))


### rebuild metadata for path_sf
rel.date <- releases %>% dplyr::select(TAG, Date) %>% rename(rel.date = Date)
path_sf <- left_join(path_sf, rel.date)
last.catch <- rec %>% group_by(TAG) %>% summarise(last.catch = max(CAPTURE_DATE)) %>% dplyr::select(TAG, last.catch)
path_sf <- left_join(path_sf, last.catch)

path_sf <- path_sf %>% mutate(disp = as.numeric(st_length(line_sf)))
max(path_sf$disp)

path_sf <- path_sf %>% mutate(time.diff.d = difftime(as.Date(last.catch),as.Date(rel.date)))
path_sf <- path_sf %>% mutate(time.diff.yrs = as.numeric(difftime(as.Date(last.catch),as.Date(rel.date))/365))

path_sf <- path_sf %>% mutate(rate.p.yr = disp/time.diff.yrs)
path_sf <- path_sf %>% mutate(rate.p.d = disp/as.numeric(time.diff.d))




#### bring in base maps
# library(ggmap)
# library(ggspatial)
# library(rnaturalearth)
library(basemaps)

left = -66.898699
right = -58.401130
top = 47.5
bottom = 43.1
#base <- get_stamenmap(bbox=c(left = left , bottom = bottom, right = right, top = top), zoom=7, maptype = "terrain-background", source = 'stamen')
#save(base, file = "C:/Users/ELEMENTG/Documents/Tagging/stamenMapNS7.RData")
#load(file = "C:/Users/ELEMENTG/Documents/Tagging/stamenMapNS7.RData")
#Canada <- ne_states(returnclass = "sf", "canada")
#NS <- Canada %>% filter(name %in% "Nova Scotia")
##### bring in LFAs
LFA <- read.csv("C:/bio.data/bio.lobster/data/maps/LFAPolys.csv")
LFA <- LFA %>% mutate(PID = ifelse(PID  %in% 311, "31A",
                                   ifelse(PID %in% 312, "31B",PID)))

lfa_poly_list <- NULL
for (i in unique(LFA$PID)){
  lfa <- LFA %>% filter(PID %in% i)
  sub.box_list <- NULL
  for(j in 1:max(lfa$SID)){
    if(nrow(lfa %>% filter(SID %in% j)>0)){
      sub.box <- lfa %>% filter(SID %in% j)
      sub.box <- arrange(sub.box,POS) %>% dplyr::select(-PID,-SID,-POS)
      sub.box <- rbind(sub.box,sub.box[1,])
      sub.box_list[[as.character(j)]] <- st_polygon(list(as.matrix(sub.box)))
    }
  }
  multipoly <- st_multipolygon(sub.box_list)
  lfa_poly_list[[i]] <- multipoly
}


lfa_sf <- st_sfc(lfa_poly_list, crs = 4326)
lfa_sf <- st_sf(lfa_sf, LFA = names(lfa_sf))
st_crs(lfa_sf) <- 4326
lfa_sf <- lfa_sf %>% filter(!(LFA %in% 41))

######
# set defaults for satellite basemaps 
ext.NS <- readRDS("C:/Users/ELEMENTG/Documents/Tagging/NS_extent")
#ext.NS <- draw_ext()
#saveRDS(ext.NS, file = "C:/Users/ELEMENTG/Documents/Tagging/NS_extent")
ext <- ext.NS

set_defaults(ext, map_service = "mapbox",map_type = "satellite",
             map_token = "pk.eyJ1IjoiZWxlbWVudGpha2UiLCJhIjoiY2xxZmh2bGFiMHhyNTJqcHJ0cDBqcW83ZiJ9.mDd49ObNcNdG6MzH3yc2dw")

base <- basemap_raster(ext) #### forces basemap crs to be in 3857

## change back to 4326 (raster package has some masking issues with sf, so just use ::)
base <- raster::projectRaster(base,  crs = 4326)

limits <- st_bbox(base)

map.objects <- list(base,limits,lfa_sf,simp_path_sf,path_sf,rel_sf,rec_sf, alldat,stack_dat,stack_sf)
base <<- base
limits<<- limits
lfa_sf<<- lfa_sf
simp_path_sf<<-simp_path_sf
path_sf<<-path_sf
rel<<-rel
rec<<-rec
rel_sf<<-rel_sf
rec_sf<<-rec_sf
alldat<<-alldat
stack_dat<<-stack_dat
stack_sf<<-stack_sf

return(map.objects)
}