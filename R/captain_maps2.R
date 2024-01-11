#' @title captain_maps2
#' @description creates maps for individual captains who participated in tag releases showing their release locations and locations of all tags recaptured from those releases.
#' @description Outputs both kml and pdf maps.
#' @param output.directory Output to selected output directory
#' @author Geraint Element
#' @export

captain_maps <- function(output.directory = NULL){
  

  ##### captain maps
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

  #### load NS map here so it doesn't need to be reloaded each loop
    ext.NS <- readRDS("C:/Users/ELEMENTG/Documents/Tagging/NS_extent")
    set_defaults(ext.NS, map_service = "mapbox",map_type = "satellite",
               map_token = "pk.eyJ1IjoiZWxlbWVudGpha2UiLCJhIjoiY2xxZmh2bGFiMHhyNTJqcHJ0cDBqcW83ZiJ9.mDd49ObNcNdG6MzH3yc2dw")
  
    ns <- basemap_raster(ext.NS) #### forces basemap crs to be in 3857
  
    ## change back to 4326 (raster package has some masking issues with sf, so just use ::)
    ns <- raster::projectRaster(ns,  crs = 4326)
  
#####  mapping loop
    ## clean up
  rel.clean <- rel %>% mutate(Captain = ifelse(Captain %in% ""|Captain %in% NA,
                                               ifelse(Vessel  %in% "" | Vessel %in% NA,
                                                      Affiliation, Vessel),
                                               Captain))
    # remove problematic characters
  rel.clean$Captain = gsub("/","_",rel.clean$Captain)
  
  #### tag captains that are CBFH affiliated
  rel.clean <- rel.clean %>% mutate(Captain = ifelse(Affiliation %in% "CBFH",paste0("CBFH - ",Captain), Captain))
  
  for(i in unique(rel.clean$Captain)  ){
    capt <- rel.clean %>% filter(Captain %in% i)
    capt <- capt %>% filter(!(lat %in% NA)) %>% filter(!(lon %in% NA))
    #capt <- capt %>% filter(Year %in% rel.year)
    capt.tags <- capt$Tag.Num
    rel.yrs <- capt %>% dplyr::select(Year,Tag.Num) %>% rename(TAG = Tag.Num, "Release Year"= Year)
    rel.yrs$TAG = as.character(rel.yrs$TAG)
    capt$ord = row.names(capt)
    capt <- capt %>% group_by(trip) %>% summarise(Captain=first(Captain),lat=first(lat),lon=first(lon),date=first(Date),ord = first(ord), Year = first(Year)) %>%ungroup()
    capt <- arrange(capt,as.numeric(ord))
    capt <- capt %>% dplyr::select(-trip,-ord)
    capt <- capt %>% mutate(name = paste0(Captain,": ",date))
    capt <- capt %>% mutate("Release Year"=Year)
    
    tag <- rec %>% dplyr::select(TAG,CAPTURE_DATE,lat,lon,Year)
    tag <- tag %>% filter(TAG  %in% capt.tags)
    tag <- left_join(tag,rel.yrs)
    tag <- tag %>% mutate(TAG = paste0("Tag: ",TAG,", ",CAPTURE_DATE))
    
    
    # #assign colour schemes ---- doesn't work!
    # num.colour.levels <- length(unique(capt$Year))
    # color_palette <-RColorBrewer::brewer.pal(n = num.colour.levels, name = "Set3")
    # capt <- capt %>% mutate(Color = color_palette[match(Year, unique(Year))])
    # num.colour.levels <- length(unique(tag$YEAR))
    # color_palette <-RColorBrewer::brewer.pal(n = num.colour.levels, name = "Set3")
    # tag <- tag %>% mutate(Color = color_palette[match(YEAR, unique(YEAR))])
    # 
    
    ####
    capt.name <- capt$Captain[1]
    capt <- capt %>% dplyr::select(-Captain,-date)
    capt$Point = "Release Trip"
    tag <- tag %>% dplyr::select(-CAPTURE_DATE)
    colnames(tag)[1] = "name"
    if(nrow(tag)>0){
      tag$Point = "Fisher Recapture"
      #tag$Icon = "C:/bio/LobTag/inst/extdata/lobster-export.png"
      tag$Icon = "https://images2.imgbox.com/e4/fa/Yn5jhJKh_o.png"   ##find image at https://imgbox.com/Yn5jhJKh
      plot.title =""}else{plot.title = "No Tag Recaptures Yet"}
    capt$Icon = "http://maps.google.com/mapfiles/kml/shapes/sailing.png"
    
    dat <- rbind(capt,tag)
    dat$lat = as.numeric(dat$lat)
    dat$lon = as.numeric(dat$lon)
    
    
    ##create sf/sp objects
    
    ### Release trip lines
    lns_all <- list()
    ref_tab_all <- NULL
    for(j in unique(capt$name)){
      coords = capt %>% filter(name %in% j) %>% dplyr::select(lon,lat)
      # ref_tab = capt %>% filter(name %in% j)
      # ref_tab_all = rbind(ref_tab_all,ref_tab)
      ln1 <- sp::Line(coords)
      lns <- sp::Lines(list(ln1), ID = j)
      lns_all <- append(lns_all,lns)
    }
    # ref_tab_all <- ref_tab_all %>% group_by(name) %>% summarise(Icon = first(Icon))
    # row.names(ref_tab_all) = ref_tab_all$name
    sp_lns <- sp::SpatialLines(lns_all, proj4string = sp::CRS("+proj=longlat +datum=WGS84") )
    
    ## points
    capt$ord = row.names(capt)
    day.points <- capt %>% group_by(name) %>% summarise(lat=first(lat),lon=first(lon),Point=first(Point),Icon=first(Icon), ord = first(ord), Year = first(Year)) %>%ungroup()
    day.points <- arrange(day.points,as.numeric(ord))
    capt <- capt %>% dplyr::select(-ord)
    day.points <- day.points %>% dplyr::select(-ord)
    
    sf_capt <- sf::st_as_sf(day.points, coords = c("lon","lat"))
    sf::st_crs(sf_capt) <- sf::st_crs(4326)  # EPSG code for WGS84
    
    sf_capt.all <- sf::st_as_sf((capt %>% filter(!(name %in% day.points$name & lat %in% day.points$lat & lon %in% day.points$lon))), coords = c("lon","lat"))
    sf::st_crs(sf_capt.all) <- sf::st_crs(4326)  # EPSG code for WGS84
    
    tag.points <- dat %>% filter(Point  %in% "Fisher Recapture")
    sf_tag <- sf::st_as_sf(tag.points, coords = c("lon","lat"))
    sf::st_crs(sf_tag) <- sf::st_crs(4326)  # EPSG code for WGS84
    
    
    sf_capt$Year = factor(sf_capt$Year)
    sf_capt.all$Year=factor(sf_capt.all$Year)


    #run.year = as.character(paste(year(Sys.Date()),month(Sys.Date())))
    run.year ="2023 12"
    ## create file directory
    if(dir.exists(file.path(output.directory,paste0(capt.name,"_",run.year)))== FALSE){
      dir.create(file.path(output.directory, paste0(capt.name,"_",run.year)))
    }
    ##create kml
    kml_file <- file.path(output.directory,paste0(capt.name,"_",run.year),paste0(capt.name,"_",run.year,".kml"))
    #sf::st_write(sf_capt, kml_file, driver = "KML", append = F)
    
    library(plotKML)
    #shape = ""
    #shape = "C:/Users/ELEMENTG/Pictures/lobster-export.png"
    kml_open(kml_file)
    kml_layer.SpatialLines(sp_lns, width = 10, colour = "blue")
    kml_layer(sf_capt.all, size = 1, LabelScale = 0, shape ="http://maps.google.com/mapfiles/dir_117.png", alpha = 8)
    kml_layer(sf_capt,labels = name,size = 1.5, shape = sf_capt$Icon, alpha = 5)
    kml_layer(sf_tag,labels = name,size = 1.5, shape = sf_tag$Icon, alpha = 5)
    kml_close(kml_file)
    
#### hard maps
    
    ##### bring in LFAs
        #### For just outer lines:
    LFA.line <- read_sf("C:/bio.data/bio.lobster/data/maps/LFA polylines")
    
        #### For full polygons:
    # LFA <- read.csv("C:/bio.data/bio.lobster/data/maps/LFAPolys.csv")
    # LFA <- LFA %>% mutate(PID = ifelse(PID  %in% 311, "31A",
    #                                    ifelse(PID %in% 312, "31B",PID)))
    # 
    # lfa_poly_list <- NULL
    # for (k in unique(LFA$PID)){
    #   lfa <- LFA %>% filter(PID %in% k)
    #   sub.box_list <- NULL
    #   for(l in 1:max(lfa$SID)){
    #     if(nrow(lfa %>% filter(SID %in% l)>0)){
    #       sub.box <- lfa %>% filter(SID %in% l)
    #       sub.box <- arrange(sub.box,POS) %>% dplyr::select(-PID,-SID,-POS)
    #       sub.box <- rbind(sub.box,sub.box[1,])
    #       sub.box_list[[as.character(l)]] <- st_polygon(list(as.matrix(sub.box)))
    #     }
    #   }
    #   multipoly <- st_multipolygon(sub.box_list)
    #   lfa_poly_list[[k]] <- multipoly
    # }
    # 
    # 
    # lfa_sf <- st_sfc(lfa_poly_list, crs = 4326)
    # lfa_sf <- st_sf(lfa_sf, LFA = names(lfa_sf))
    # st_crs(lfa_sf) <- 4326
    # lfa_sf <- lfa_sf %>% filter(!(LFA %in% 41))
    # 
    ######
    
    ##some tweaks to variable names for legend
    sf_capt <- sf_capt %>% mutate("Year of Event" =Year) %>% rename(Event=Point)
    sf_capt.all <- sf_capt.all %>% mutate("Year of Event" =Year) %>% rename(Event=Point)
    sf_tag <- sf_tag %>% mutate("Year of Event" =Year) %>% rename(Event=Point)
    sf_tag <- sf_tag %>% mutate(Event = ifelse(Event %in% "Fisher Recapture","Lobster Recaptured", Event))
    
    
    ### get separate basemaps for each year of releases
    for(k in unique(sf_capt$Year)){
    dat.y <- dat %>% filter(`Release Year` %in% k)  
    
    ### set plotting region
    #Make plotting region visually more square
    maxx =max(dat.y$lon)
    maxy = max(dat.y$lat)
    minx = min(dat.y$lon)
    miny = min(dat.y$lat)
    
    xlen = maxx - minx
    ylen = maxy - miny
    stretch.y=F
    stretch.x=F
    if(ylen-xlen>0.2){stretch.y=T
                      stretch.y.factor = ylen-xlen}
    if(xlen-ylen>0.2){stretch.x=T}
    
    
    while(xlen < ylen){
      maxx = maxx+.01
      minx = minx-.01
      xlen =  maxx - minx
    }
    while(ylen < xlen){
      maxy = maxy+.01
      miny = miny-.01
      ylen =  maxy - miny
    }

    while(xlen < ylen){
      maxx = maxx+.001
      minx = minx-.001
      xlen =  maxx - minx
    }
    
    while(ylen < xlen){
      maxy = maxy+.001
      miny = miny-.001
      ylen =  maxy - miny
    }
    
    # #visually scale plotting area a bit wider
    scale = (maxy-miny)/120
    maxx = maxx+scale
    minx = minx-scale
    maxy = maxy+scale
    miny = miny-scale

    
    
    #Expand region (stretch a bit on one axis if that range is very large)
    minx = minx - ylen/3  
    maxx = maxx + ylen/3
    
    if(stretch.y){
      miny = miny - ylen/6  
      maxy = maxy + ylen/6
    }
    if(stretch.x){
      minx = minx - ylen/6  
      maxx = maxx + ylen/6  
    }
    
     
    ylen = maxy-miny
    xlen = maxx-minx
    
    left <- minx
    right <- maxx
    top <- maxy
    bottom <- miny
    
    ### If just one point, set box size
    if(maxx == minx & maxy == miny){
      left = minx - 0.1
      right = maxx + 0.1
      top = maxy + 0.07
      bottom = miny - 0.07
      
      ylen = top-bottom
      xlen = right-left
      
      ext <- sf::st_polygon(list(matrix(c(left,bottom,right,bottom,right,top,left,top,left,bottom),ncol = 2, byrow = T)))
    }else{ ext <- sf::st_polygon(list(matrix(c(minx,miny,maxx,miny,maxx,maxy,minx,maxy,minx,miny),ncol = 2, byrow = T))) }
    
   
    ### get basemap
    ext_sf <- sf::st_sfc(ext, crs = 4326)
    ext_sf <- sf::st_sf(ext_sf)
    sf::st_crs(ext_sf) <- 4326
    
    set_defaults(ext_sf, map_service = "mapbox",map_type = "satellite",
                 map_token = "pk.eyJ1IjoiZWxlbWVudGpha2UiLCJhIjoiY2xxZmh2bGFiMHhyNTJqcHJ0cDBqcW83ZiJ9.mDd49ObNcNdG6MzH3yc2dw")
  
    base <- basemap_raster(ext_sf) #### forces basemap crs to be in 3857
    
    ## change back to 4326 (raster package has some masking issues with sf, so just use ::)
    base <- raster::projectRaster(base,  crs = 4326)
    limits <- st_bbox(base)
    
    
    #### make inset with NS map
    b1 <- gg_raster(ns)+
      geom_sf(data = ext, colour = "red", alpha = 0.1)+
      coord_sf(expand = F)+
      theme(axis.title = element_blank(), 
            axis.text  = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = unit(c(0.0,0.0,0.001,0.001),'lines'))
    
    
    
    b <- ggplotGrob(b1)
    
    ### if there are multiple release years with assosciated captures, make separate maps
    if(length(unique(sf_capt$Year))>1){
      sf.rel.yr <- sf_capt %>% filter(Year  %in% k)
      sf.rel.yr.all <- sf_capt.all %>% filter(Year %in% k)
      sf.tag.yr <- sf_tag %>% filter(`Release Year` %in% k)
        plot.title.yr = ""
        if(length(sf.tag.yr$name)==0){
          plot.title.yr = "No Tag Recaptures For This Release Year Yet"
          sf.rel.yr <- sf.rel.yr %>% mutate("Release Year"=Year)
          sf.rel.yr.all$`Release Year` = as.character(sf.rel.yr.all$`Release Year`)
          
          ###legend doesn't like no sf.rel.yr.all rows, so don't run that table if this case
          if(length(sf.rel.yr.all$name)>0){
            a <-  gg_raster(base)+
              ggtitle(plot.title.yr)+
              geom_sf(data=LFA.line, colour = "red")+
              ggspatial::annotation_scale(data = sf.rel.yr, bar_cols = c("grey", "white"), text_col = "white")+
              geom_sf(data=sf.rel.yr, aes(colour = `Release Year`), shape = 17, size = 5, alpha=0.6)+
              geom_sf(data= sf.rel.yr.all, aes(colour = `Release Year`), shape = 17, size = 1.2)+
              geom_sf_label_repel(data = sf.rel.yr, aes(label=name, colour=Year),show.legend=F,nudge_y=-(ylen/10), alpha=0.8,max.overlaps = 20, size=2.1)+
              coord_sf(ylim=as.numeric(c(limits$ymin,limits$ymax)), xlim = as.numeric(c(limits$xmin,limits$xmax)), expand = F)
          }else{
            a <-  gg_raster(base)+
              ggtitle(plot.title.yr)+
              geom_sf(data=LFA.line, colour = "red")+
              ggspatial::annotation_scale(data = sf.rel.yr, bar_cols = c("grey", "white"), text_col = "white")+
              geom_sf(data=sf.rel.yr, aes(colour = `Release Year`), shape = 17, size = 5, alpha=0.6)+
              geom_sf_label_repel(data = sf.rel.yr, aes(label=name, colour=Year),show.legend=F,nudge_y=-(ylen/10), alpha=0.8,max.overlaps = 20, size=2.1)+
              coord_sf(ylim=as.numeric(c(limits$ymin,limits$ymax)), xlim = as.numeric(c(limits$xmin,limits$xmax)), expand = F)
          }
          
        }else{
          a <-  gg_raster(base)+
            ggtitle(plot.title.yr)+
            geom_sf(data=LFA.line, colour = "red")+
            ggspatial::annotation_scale(data = sf_capt, bar_cols = c("grey", "white"), text_col = "white")+
            geom_sf(data = sf.tag.yr, aes(colour = `Year of Event`, shape = Event), size = 3)+
            geom_sf(data=sf.rel.yr, aes(colour = `Year of Event`, shape = Event), size = 5, alpha=0.6)+
            geom_sf(data= sf.rel.yr.all, aes(colour = `Year of Event`, shape = Event), size = 1.2)+
            geom_sf_label_repel(data = sf.rel.yr, aes(label=name, colour=Year),show.legend=F,nudge_y=-(ylen/10), alpha=0.8,max.overlaps = 20, size=2.1)+
            coord_sf(ylim=as.numeric(c(limits$ymin,limits$ymax)), xlim = as.numeric(c(limits$xmin,limits$xmax)), expand = F)
          }
        
        if(stretch.y){
          c <- a +
            annotation_custom(grob=b, xmin = right-ylen/4, xmax = right+ylen/3, ymax = top+ylen/1.7, ymin = top-ylen/2.3-ylen/15)
          ggsave(file.path(output.directory,paste0(capt.name,"_",run.year),paste0(capt.name,"_",run.year,"_",k,"releases",".pdf")), width = 8.5, height = 8.5+stretch.y.factor*3)
        }else{
          c <- a +
            annotation_custom(grob=b, xmin = right-ylen/2, xmax = right+ylen/8, ymax = top+ylen/1.7, ymin = top-ylen/2.3)                                                                                                                           
          ggsave(file.path(output.directory,paste0(capt.name,"_",run.year),paste0(capt.name,"_",run.year,"_",k,"releases",".pdf")), width = 8.5, height = 8.5)
        }
        
      
    }else{
      
      if(length(sf_tag$name)==0){
        sf_capt <- sf_capt %>% mutate("Release Year"=Year)
        sf_capt.all$`Release Year` = as.character(sf_capt.all$`Release Year`)
        a <-  gg_raster(base)+
          ggtitle(plot.title)+
          geom_sf(data=LFA.line, colour = "red")+
          ggspatial::annotation_scale(data = sf_capt, bar_cols = c("grey", "white"), text_col = "white")+
          geom_sf(data=sf_capt, aes(colour = `Release Year`),shape =17, size = 5, alpha=0.6)+
          geom_sf(data= sf_capt.all, aes(colour = `Release Year`),shape = 17, size = 1.2)+
          geom_sf_label_repel(data = sf_capt, aes(label=name, colour=Year),show.legend=F,nudge_y=-0.03, alpha=0.8,max.overlaps = 20, size=2)+
          coord_sf(ylim=as.numeric(c(limits$ymin,limits$ymax)), xlim = as.numeric(c(limits$xmin,limits$xmax)), expand = F)
        
      }else{
        a <-  gg_raster(base)+
          ggtitle(plot.title)+
          geom_sf(data=LFA.line, colour = "red")+
          ggspatial::annotation_scale(data = sf_capt, bar_cols = c("grey", "white"), text_col = "white")+
          geom_sf(data = sf_tag, aes(colour = `Year of Event`, shape = Event), size = 3)+
          geom_sf(data=sf_capt, aes(colour = `Year of Event`, shape = Event), size = 5, alpha=0.6)+
          geom_sf(data= sf_capt.all, aes(colour = `Year of Event`,shape = Event), size = 1.2)+
          geom_sf_label_repel(data = sf_capt, aes(label=name, colour=Year),show.legend=F,nudge_y=-0.03, alpha=0.8,max.overlaps = 20, size=2.1)+
          coord_sf(ylim=as.numeric(c(limits$ymin,limits$ymax)), xlim = as.numeric(c(limits$xmin,limits$xmax)), expand = F)
      }
   
        
      
      if(stretch.y){
        c <- a +
          annotation_custom(grob=b, xmin = right-ylen/4, xmax = right+ylen/3, ymax = top+ylen/1.7, ymin = top-ylen/2.3-ylen/15)
        ggsave(file.path(output.directory,paste0(capt.name,"_",run.year),paste0(capt.name,"_",run.year,".pdf")), width = 8.5, height = 8.5+stretch.y.factor*3)
      }else{
        c <- a +
          annotation_custom(grob=b, xmin = right-ylen/2, xmax = right+ylen/8, ymax = top+ylen/1.7, ymin = top-ylen/2.3)
        ggsave(file.path(output.directory,paste0(capt.name,"_",run.year),paste0(capt.name,"_",run.year,".pdf")), width = 8.5, height = 8.5)
      }
      
    }
      
      
### release year loop end
   }
    
### Captain loop end    
  }

  
  ### function end
}
  
