#' @title captain_maps
#' @description creates maps for individual captains who participated in tag releases showing their release locations and locations of all tags recaptured from those relesaes.
#' @description Outputs both kml and pdf maps.
#' @param releases.data.path Get release data from specified path (gets recaptures from Oracle).
#' @param output.directory Output to selected output directory
#' @author Geraint Element
#' @export

captain_maps <- function(releases.data.path = NULL, output.directory = NULL){
  
  ##### captain maps
  library(dplyr)
  library(lubridate)
  # library(sf)
  #library(raster)
  #library(sp)
  library(grid)
  library(ggmap)
  library(ggrepel)
  library(ggplot2)
  
  #setwd("C:/Users/ELEMENTG/Documents/Tagging")
  rel <-  read.csv(releases.data.path)
  #read.csv("Tag_releases_master_GE.csv")
  
  # test <- rel %>% group_by(Captain) %>% summarise(Vessel = first(Vessel),Affiliation=first(Affiliation)) %>% ungroup()
  # test1 <- rel %>% filter(Captain %in% "")
  
  rel <- rel %>% mutate(lat = Lat.Degrees+Lat.Minutes/60) %>% mutate(lon = -(Lon.Degrees+Lon.Minutes/60))
  rel <- rel %>% mutate(trip = paste(lat,lon))
  rel <- rel %>% mutate(Date = as.Date(Date, "%d-%b-%y"))
  rel <- rel %>% mutate(Date = format(Date, "%d-%b-%Y"))
  
  
  #pull recaptures directly from oracle for vetted tag numbers etc.
  drv <- ROracle::Oracle()
  con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.server)
  rec <- ROracle::dbSendQuery(con, paste("select * from ", "LOBSTER.LBT_CAPTURE", sep=""))
  rec <- ROracle::fetch(rec)
  ROracle::dbDisconnect(con)
  
  rec <- rec %>% mutate(lat = LAT_DD_DDDD) %>% mutate(lon = LONG_DD_DDDD)
  rec <- rec %>% mutate(trip = paste(lat,lon))
  rec <- rec %>% mutate(CAPTURE_DATE = as.Date(CAPTURE_DATE))
  rec <- rec %>% mutate(CAPTURE_DATE = format(CAPTURE_DATE, "%d-%b-%Y"))
  
  #### load NS map here so it doesn't need to be reloaded each loop
  ns <- get_stamenmap(bbox=c(left =-66.77  , bottom = 42.8, right = -58.87, top = 47.2), zoom=9, maptype = "terrain", source = 'stamen')
  
  
  
  ##### loop
  rel.clean <- rel %>% mutate(Captain = ifelse(Captain %in% ""|Captain %in% NA,
                                               ifelse(Vessel  %in% "" | Vessel %in% NA,
                                                      Affiliation, Vessel),
                                               Captain))
  
  
  
  for(i in unique(rel.clean$Captain)){
    capt <- rel.clean %>% filter(Captain %in% i)
    capt <- capt %>% filter(!(lat %in% NA)) %>% filter(!(lon %in% NA))
    #capt <- capt %>% filter(Year %in% rel.year)
    capt.tags <- capt$Tag.Num
    capt$ord = row.names(capt)
    capt <- capt %>% group_by(trip) %>% summarise(Captain=first(Captain),lat=first(lat),lon=first(lon),date=first(Date),ord = first(ord)) %>%ungroup()
    capt <- arrange(capt,as.numeric(ord))
    capt <- capt %>% select(-trip,-ord)
    capt <- capt %>% mutate(name = paste0(Captain,": ",date))
    
    tag <- rec %>% select(TAG,CAPTURE_DATE,lat,lon)
    tag <- tag %>% filter(TAG  %in% capt.tags)
    tag <- tag %>% mutate(TAG = paste0("Tag: ",TAG,", ",CAPTURE_DATE))
    
    ####
    capt.name <- capt$Captain[1]
    capt <- capt %>% select(-Captain,-date)
    capt$Point = "Release Trip"
    tag <- tag %>% select(-CAPTURE_DATE)
    colnames(tag)[1] = "name"
    if(nrow(tag)>0){
      tag$Point = "Fisher Recapture"
      tag$Icon = "C:/Users/ELEMENTG/Pictures/lobster-export.png"
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
      coords = capt %>% filter(name %in% j) %>% select(lon,lat)
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
    day.points <- capt %>% group_by(name) %>% summarise(lat=first(lat),lon=first(lon),Point=first(Point),Icon=first(Icon), ord = first(ord)) %>%ungroup()
    day.points <- arrange(day.points,as.numeric(ord))
    capt <- capt %>% select(-ord)
    day.points <- day.points %>% select(-ord)
    
    sf_capt <- sf::st_as_sf(day.points, coords = c("lon","lat"))
    sf::st_crs(sf_capt) <- sf::st_crs(4326)  # EPSG code for WGS84
    
    sf_capt.all <- sf::st_as_sf((capt %>% filter(!(name %in% day.points$name & lat %in% day.points$lat & lon %in% day.points$lon))), coords = c("lon","lat"))
    sf::st_crs(sf_capt.all) <- sf::st_crs(4326)  # EPSG code for WGS84
    
    tag.points <- dat %>% filter(Point  %in% "Fisher Recapture")
    sf_tag <- sf::st_as_sf(tag.points, coords = c("lon","lat"))
    sf::st_crs(sf_tag) <- sf::st_crs(4326)  # EPSG code for WGS84
    
    #sp_df <- sp::SpatialLinesDataFrame(sp_lns,ref_tab_all)
    
    # sf_lines = sf::st_linestring(as.matrix(coords))
    # sfc_lines = sf::st_sfc(sf_lines)
    # dfr <- ref_tab %>% select(-lon,-lat)
    # sf_df <- sf::st_sf(dfr,sfc_lines)
    
    run.year = as.character(year(Sys.Date()))
    ## create file directory
    if(dir.exists(paste0("C:/Users/ELEMENTG/Documents/Tagging/capt_maps/",capt.name,"_",run.year))== FALSE){
      dir.create(paste0("C:/Users/ELEMENTG/Documents/Tagging/capt_maps/",capt.name,"_",run.year))
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
    
    #library(ggthemes)
    #library(RStoolbox)
    #library(cowplot)
    
    ### set plotting region
    #Make plotting region visually more square
    maxx =max(dat$lon)
    maxy = max(dat$lat)
    minx = min(dat$lon)
    miny = min(dat$lat)
    
    xlen = maxx - minx
    ylen = maxy - miny
    
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
    
    #visually scale plotting area a bit wider
    scale = (maxy-miny)/120
    maxx = maxx+scale
    minx = minx-scale
    maxy = maxy+scale
    miny = miny-scale
    
    
    
    #Expand region
    minx = minx - ylen/3  
    maxx = maxx + ylen/3
    # miny = miny - ylen/2  
    # maxy = maxy + ylen/2
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
    }
    
    ### get basemap
    base <- get_stamenmap(bbox=c(left = left , bottom = bottom, right = right, top = top), zoom=11, maptype = "terrain-background", source = 'stamen')
    
    
    ##for scalebar and northing symbol
    dist = round(geosphere::distHaversine(c(left, bottom), c(right, bottom), r=6378137)/1000/8,1)
    hei = ylen/100
    nx = left+ylen/17
    nx.end = nx
    ny = top - ylen/25
    ny.end = ny+ylen/50
    
    # ### show only start of days for release trips
    # dat <- dat %>% mutate(trip = paste(lat,lon))
    # day.points <- day.points %>% mutate(trip = paste(lat,lon))
    # dat <- dat %>% mutate(start = ifelse(trip  %in% day.points$trip, "Start",NA))
    # dat <- dat %>% mutate(name = ifelse(Point  %in% "Fisher Recapture" | start  %in% "Start",name,NA))
    # dat <- dat %>% mutate(line.col = ifelse(Point  %in% "Fisher Recapture",NA,"Release Trip"))
    # dat <- dat %>% mutate(Point = ifelse(Point  %in% "Fisher Recapture" | start  %in% "Start",as.character(Point),NA))
    
    dat <- rbind(day.points,tag.points)
    dat$Point = factor(dat$Point, levels = c("Release Trip","Fisher Recapture"))
    capt$Point= factor(capt$Point, levels = c("Release Trip","Fisher Recapture"))
    
    ### plot map
    a <- ggmap(base)+
      geom_path(data = capt, aes(x = lon, y = lat, group = name), colour = "#2E9FDF", linewidth = 1)+
      geom_point(data = capt, aes(x = lon, y = lat, colour = Point),size=0.5)+
      geom_point(data = dat, aes(x = lon, y = lat, colour = Point),size=2.5)+
      ggtitle(plot.title)+
      scale_color_manual(values = c("#2E7FDF","#FC4E07"), drop = FALSE)+
      ggsn::scalebar(x.min = left, x.max = right, y.min = bottom ,y.max = top, dist_unit= "km", location = "bottomleft", st.bottom = F,anchor = c(x = left+2*hei, y=bottom+2*hei), transform = T, dist = dist, st.size=4,  model = 'WGS84')+
      geom_segment(arrow=arrow(length=unit(6,"mm"),type="closed", angle=40),linewidth =1, 
                   aes(x=nx,xend=nx.end,y=ny,yend=ny.end), colour="black") +
      geom_label(aes(x=nx, y=ny-ylen/100, label="N"),
                 size=4, label.padding=unit(1,"mm"), label.r=unit(0.4,"lines"))+
      geom_label_repel(data=dat, aes(label=name, colour=Point), alpha=0.7, max.overlaps = 80, size =2.9)
    
    ### alternate plot for tailoring specific maps
    # a <- ggmap(base)+
    #   geom_path(data = capt, aes(x = lon, y = lat, group = name), colour = "#2E9FDF", linewidth = 1)+
    #   geom_point(data = capt, aes(x = lon, y = lat, colour = Point),size=0.5)+
    #   geom_point(data = dat, aes(x = lon, y = lat, colour = Point),size=1.9)+
    #   ggtitle(plot.title)+
    #   scale_color_manual(values = c("#2E7FDF","#FC4E07"), drop = FALSE)+
    #   ggsn::scalebar(x.min = left, x.max = right, y.min = bottom ,y.max = top, dist_unit= "km", location = "bottomleft", st.bottom = F,anchor = c(x = left+2*hei, y=bottom+2*hei), transform = T, dist = dist, st.size=4,  model = 'WGS84')+
    #   geom_segment(arrow=arrow(length=unit(6,"mm"),type="closed", angle=40),linewidth =1,
    #                aes(x=nx,xend=nx.end,y=ny,yend=ny.end), colour="black") +
    #   geom_label(aes(x=nx, y=ny-ylen/100, label="N"),
    #              size=4, label.padding=unit(1,"mm"), label.r=unit(0.4,"lines"))+
    #   geom_label_repel(data=dat, aes(label=name, colour=Point), alpha=0.7, max.overlaps = 30, size =2)
    
    #### make inset with NS map
    
    b1 <- ggmap(ns)+
      theme(axis.title = element_blank(), 
            axis.text  = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = unit(c(0.0,0.0,0.001,0.001),'lines'))+
      geom_rect(aes(xmin = left, xmax = right, ymin = bottom, ymax = top),colour="red", alpha = 0.1, linewidth = 0.1)
    
    b <- ggplotGrob(b1)
    
    c <- a +
      inset(grob = b, xmin = right-ylen/50, xmax = right+ylen/2.2, ymax = top+ylen/3.9, ymin = top-ylen/2)  
    
    # pdf(file = file.path("C:/Users/ELEMENTG/Documents/Tagging/capt_maps/",paste0(capt.name,"_",run.year),paste0(capt.name,"_",run.year,".pdf")), width = 8.5, height = 8.5)
    # ggsn::north2(a,scale = 0.06, symbol = 10, x = 0.8, y = 0.85)
    # dev.off()
    ggsave(file.path(output.directory,paste0(capt.name,"_",run.year),paste0(capt.name,"_",run.year,".pdf")), width = 8.5, height = 8.5)
    
  }
  
}
