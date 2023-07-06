#' @title  Shortestpaths.SC
#' @description  Creates and writes the shortest paths to database
#' @param neighborhood The number of adjacent cell over which to calculate, defaults to 16.
#' @param type The type of calculation, either 'random.walk' or 'least.cost'. Defaults to 'least.cost'
#' @param redo Set redo = TRUE if you want to rewrite all data, FALSE to only update new entries
#' @param region Either 'ScotianShelf' or 'Gulf'
#' @import PBSmapping raster gdistance ROracle RMySQL rJava DBI
#' @return dataframe
#' @export
shortestpaths.SC = function(neighborhood = 16, type = "least.cost", redo = F, region = "ScotianShelf"){
  raster.path <- "C:/bio.data/bio.lobster/data/tagging/depthraster2.tif"
  drv <- DBI::dbDriver("Oracle")
  gstring = ""
  x = get.capturedata(region)
  x$PID = as.character(x$PID)
  
  trans = NULL
  r = raster(raster.path)
  mr = as.matrix(r)
  mr[which(mr > -5000 & mr < 0)] = -1                       #using least cost (the lowest point is in the -4000s so we can go from -5000 to 0 ie. sea level)
  mr = apply(mr, 2, function(x) dnorm(x,mean=-1,sd=1))
  r = setValues(r, mr)
  
  tr <- transition(r, mean, neighborhood)
  if(type  == "random.walk"){
    trans = geoCorrection(tr, type = "r", scl=FALSE)
  }
  if(type  == "least.cost"){
    trans = geoCorrection(tr, type = "c", scl=FALSE)
  }

  dftowrite = NULL
  df2towrite = NULL
  dxtowrite = NULL
  
  if(!redo){
    
    con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
    respat <- ROracle::dbSendQuery(con, "SELECT * FROM LOBSTER.LBT_PATH")

    da <-  ROracle::fetch(respat)
    ROracle::dbDisconnect(con)
    
    
    goodind = which(paste(as.character(x$PID), format(x$capdate, "%d/%m/%Y")) %in% paste(as.character(da$TID), as.character(da$CDATE)))
    if(length(goodind) > 0) x = x[-goodind,]
    zeroind = which(as.numeric(x$caplat) == 0 | x$caplat == 'unknown')
    if(length(zeroind) > 0) x = x[-zeroind,]
    
    count = 1
    previd = ""
    if(nrow(x) == 0)message("No new paths to create!")
    else{
      #i = 3
      for(i in 1:nrow(x)){
        if(x$PID[i] == previd){
          count = count + 1 
          #print(paste0("row 57: ", count))
        }
        else{
          previd = x$PID[i]
          count = 1 + sum(da$TID == x$PID[i])
          #print(paste0("row 62: ", count))
        }
        start <- c(as.numeric(x$rellon[i]), as.numeric(x$rellat[i]))
        end <- c(as.numeric(x$caplon[i]), as.numeric(x$caplat[i]))
        
        if(abs(start[1] - end[1]) < res(trans)[1] && abs(start[2] - end[2]) < res(trans)[1] || is.na(cellFromXY(r, start)) || is.na(cellFromXY(r, end))){
          AtoB = rbind(start, end)
        }
        else{
          AtoB = shortestPath(trans, start, end, output="SpatialLines")
        }
        cor = data.frame(coordinates(AtoB))
        names(cor) = c("x", "y")
        xrep = cor$x[1]
        yrep = cor$y[1]
        #k = 1
        for(k in 1:(nrow(cor)-1)){
          if(cor$x[k] == xrep){ cor$x[k] = start[1] }
          else{ xrep = 1000000 }
          
          if(cor$y[k] == yrep){ cor$y[k] =  start[2] }
          else{ yrep = 1000000 }
        }
        xrep = cor$x[nrow(cor)]
        yrep = cor$y[nrow(cor)]
        #k = 2
        for(k in nrow(cor):2){
          if(cor$x[k] == xrep) cor$x[k] =  end[1]
          else xrep = 1000000
          
          if(cor$y[k] == yrep) cor$y[k] =  end[2]
          else yrep = 1000000
        }
        
        names(cor) = c("X", "Y")
        cor$PID = 1
        cor$POS = 1:nrow(cor)
        tpoly = as.PolySet(cor, projection = "LL")
        leng = calcLength (tpoly, rollup = 3, close = FALSE) #km    
      
        dxp = cbind(rep(x$PID[i], nrow(cor)),rep(count, nrow(cor)), 1:nrow(cor), cor$X, cor$Y)
        dxtowrite = rbind(dxtowrite, dxp)
        df2towrite = rbind(df2towrite, cbind(x$PID[i], count, as.character(x$capdat[i]), leng$length))
        
        dftowrite = rbind(dftowrite, cbind(x$PID[i],paste(cor[,1], collapse = ","), paste(cor[,2], collapse = ","), as.character(x$capdat[i]), leng$length))
      }
      }
    }
  else{
    count  = 1
    previd = ""
    for(i in 1:nrow(x)){
      if(x$PID[i] == previd){
        count = count+1
      }
      else{
        previd = x$PID[i]
        count = 1
      }
      start <- c(as.numeric(x$rellon[i]), as.numeric(x$rellat[i]))
      end <- c(as.numeric(x$caplon[i]), as.numeric(x$caplat[i]))
      
      if(abs(start[1] - end[1]) < res(trans)[1] && abs(start[2] - end[2]) < res(trans)[1] || is.na(cellFromXY(r, start)) || is.na(cellFromXY(r, end))){
        AtoB = rbind(start, end)
      }
      else{
        AtoB = shortestPath(trans, start, end, output="SpatialLines")
      }
      cor = data.frame(coordinates(AtoB))
      names(cor) = c("x", "y")
      xrep = cor$x[1]
      yrep = cor$y[1]
      for(k in 1:(nrow(cor)-1)){
        if(cor$x[k] == xrep){ cor$x[k] = start[1] }
        else{ xrep = 1000000 }
        if(cor$y[k] == yrep){ cor$y[k] =  start[2] }
        else{ yrep = 1000000 }
      }
      xrep = cor$x[nrow(cor)]
      yrep = cor$y[nrow(cor)]
      for(k in nrow(cor):2){
        if(cor$x[k] == xrep){ cor$x[k] =  end[1] }
        else{ xrep = 1000000 }
        if(cor$y[k] == yrep){ cor$y[k] =  end[2] }
        else{ yrep = 1000000}
      }
      names(cor) = c("X", "Y")
      cor$PID = 1
      cor$POS = 1:nrow(cor)
      tpoly = as.PolySet(cor, projection = "LL")
      leng = calcLength (tpoly, rollup = 3, close = FALSE) #km    
      
      dxp = cbind(rep(x$PID[i], nrow(cor)),rep(count, nrow(cor)), 1:nrow(cor), cor$X, cor$Y)
      dxtowrite = rbind(dxtowrite, dxp)
      df2towrite = rbind(df2towrite, cbind(x$PID[i], count, as.character(x$capdat[i]), leng$length))
      
      dftowrite = rbind(dftowrite, cbind(x$PID[i],paste(cor[,1], collapse = ","), paste(cor[,2], collapse = ","), as.character(x$capdat[i]), leng$length))
    }
  }
  
  if(!is.null(dftowrite)){
    dftowrite = data.frame(dftowrite)
    df2towrite = data.frame(df2towrite)
    
    #tax prefix should follow through, this needs to be updated before different tagging programs are added.
    #add tag column
    dftowrite$tag = "XY"
    df2towrite$tag = "XY"
    
    names(dftowrite) = c("ID", "LON", "LAT", "CDATE", "DIST", "TAG_PREFIX")
    names(df2towrite) = c("TID", "CID", "CDATE", "DIST", "TAG_PREFIX")
    dxtowrite = data.frame(dxtowrite)
    dxtowrite$tag = "XY"
    names(dxtowrite) = c("TID", "CID", "POS", "LON", "LAT", "TAG_PREFIX")
    drv <- DBI::dbDriver("Oracle")
    
    Sys.setenv(TZ = "America/Halifax")
    Sys.setenv(ORA_SDTZ = "America/Halifax")
    str(dftowrite$CDATE)
    dftowrite$CDATE = as.POSIXct(dftowrite$CDATE, origin = '1970-01-01')
    df2towrite$CDATE = as.POSIXct(df2towrite$CDATE, origin = '1970-01-01')
    
    xx= NULL
    xy=NULL
    xx = as.character(as.Date(dftowrite$CDATE))
    xx[which(is.na(xx))] = "1111-11-11"
    xy = matrix(unlist(strsplit(xx, "-")), ncol = 3,  byrow = T)
    
    dftowrite$CDATE = paste(xy[,3], xy[,2], xy[,1], sep = "/")
    dftowrite$CDATE[which(dftowrite$CDATE == "11/11/1111")] = NA
    
    xx= NULL
    xy=NULL
    xx = as.character(as.Date(df2towrite$CDATE))
    xx[which(is.na(xx))] = "1111-11-11"
    xy = matrix(unlist(strsplit(xx, "-")), ncol = 3,  byrow = T)
    
    df2towrite$CDATE = paste(xy[,3], xy[,2], xy[,1], sep = "/")
    df2towrite$CDATE[which(df2towrite$CDATE == "11/11/1111")] = NA
    
    con <- dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
    #replace this with sql loop...
    #dbWriteTable bugs out sometimes, deleting and entering path/paths fresh is solution for now
    if(redo){
      dbWriteTable(con,"LOBSTER.LBT_PATHS", dxtowrite, overwrite = T)
      dbWriteTable(con,"LOBSTER.LBT_PATH", df2towrite, overwrite = T)     
    }
    else{
      #add data to oracle with one long SQL query, this is to circumnavigate a permission bug with oracle that sometimes
      #denies db access when uploading dataframes.
      
      pathdb = paste("LOBSTER",".","LBT_PATH", sep = "")
      pathsdb = paste("LOBSTER",".","LBT_PATHS", sep = "")
      
      path_call = create_sql_query(pathdb, df2towrite)
      paths_call = create_sql_query(pathsdb, dxtowrite)
      
      result <- ROracle::dbSendQuery(con, path_call)
      result <- ROracle::dbSendQuery(con, paths_call)
      
      ROracle::dbCommit(con)
    }
    dbDisconnect(con)
    print("New paths calculated and written to paths table.")
  }
  else{
    print("No new paths created.")  
  }
  return(dftowrite)
}

#' @title  get.capturedata
#' @description  Return capture data
#' @import ROracle DBI
#' @return dataframe
#' @export
get.capturedata = function(region = "ScotianShelf"){
  #this didn't have the connection to oracle here, so it could never connnect. Looks like it was probably deleted by mistake.
  tryCatch({
    drv <- DBI::dbDriver("Oracle")
    conn <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  }, warning = function(w) {
  }, error = function(e) {
    return(toJSON("Connection failed"))
  }, finally = {
  })
  
  #everything defaults to 'ScotianShelf' because 'gulf' is irrelevant. Remove all 'gulf' flow control first, check to ensure functionality
  #isn't changed, then remove 'ScotianShelf'
  
  gstring = ""
  da = NULL
  
  captdb = paste("LOBSTER",".","LBT_CAPTURE", sep = "")
  peopdb = paste("LOBSTER",".","LBT_PEOPLE", sep = "")
  lbiodb = paste("LOBSTER",".","LBT_BIO", sep = "")
  tripdb = paste("LOBSTER",".","LBT_TRIP", sep = "")
  sampdb = paste("LOBSTER",".","LBT_SAMPLE", sep = "")
  
  #new lobster code
  query = paste("SELECT ", lbiodb ,".TAG_ID, ",
                lbiodb, ".SAMPLE_NUM, ",
                captdb, ".CAPTURE_DATE, ",
                captdb, ".STATSAREA, ",
                captdb, ".LAT_DD_DDDD, ",
                captdb, ".LONG_DD_DDDD, ",
                captdb, ".YEAR, ",
                captdb, ".RELCODE, ",
                tripdb, ".TRIP_ID, ",
                tripdb, ".STATSAREA AS STATSAREA1, ",
                tripdb, ".YEAR AS YEAR1, ",
                tripdb, ".CAPTAIN, ",
                tripdb, ".REPORTED, ",
                tripdb, ".RELEASE_DATE, ",
                sampdb, ".LAT_DD_DDDD AS LAT_DD_DDDD1, ",
                sampdb, ".LONG_DD_DDDD AS LONG_DD_DDDD1, ",
                sampdb, ".SAMPLE_ID FROM ",
                captdb, " INNER JOIN ", lbiodb, " ON ",
                lbiodb, ".TAG_ID = ", captdb, ".TAG INNER JOIN ",
                sampdb, " ON ", lbiodb, ".SAMPLE_NUM = ", sampdb, ".SAMPLE_ID INNER JOIN ",
                tripdb, " ON ", sampdb, ".TRIP = ", tripdb, ".TRIP_ID ORDER BY ", tripdb, ".CAPTAIN, ",
                tripdb, ".TRIP_ID, ",
                lbiodb, ".TAG_ID, ",
                captdb, ".CAPTURE_DATE", sep = "")
  
  
  # original crab code below!
  # query = paste("SELECT SCT_BIO", gstring,".TAG_ID,
  #               SCT_BIO", gstring,".SAMPLE_NUM,
  #               SCT_CAPTURE", gstring,".CAPTURE_DATE,
  #               SCT_CAPTURE", gstring,".STATSAREA,
  #               SCT_CAPTURE", gstring,".LAT_DD_DDDD,
  #               SCT_CAPTURE", gstring,".LONG_DD_DDDD,
  #               SCT_CAPTURE", gstring,".YEAR,
  #               SCT_CAPTURE", gstring,".RELCODE,
  #               SCT_TRIP", gstring,".TRIP_ID,
  #               SCT_TRIP", gstring,".STATSAREA AS STATSAREA1,
  #               SCT_TRIP", gstring,".YEAR      AS YEAR1,
  #               SCT_TRIP", gstring,".CAPTAIN,
  #               SCT_TRIP", gstring,".REPORTED,
  #               SCT_TRIP", gstring,".RELEASE_DATE,
  #               SCT_SAMPLE", gstring,".LAT_DD_DDDD              AS LAT_DD_DDDD1,
  #               SCT_SAMPLE", gstring,".LONG_DD_DDDD             AS LONG_DD_DDDD1,
  #               SCT_SAMPLE", gstring,".SAMPLE_ID
  #               FROM SCT_CAPTURE", gstring,"
  #               INNER JOIN SCT_BIO", gstring,"
  #               ON SCT_BIO", gstring,".TAG_ID = SCT_CAPTURE", gstring,".TAG
  #               INNER JOIN SCT_SAMPLE", gstring,"
  #               ON SCT_BIO", gstring,".SAMPLE_NUM = SCT_SAMPLE", gstring,".SAMPLE_ID
  #               INNER JOIN SCT_TRIP", gstring,"
  #               ON SCT_SAMPLE", gstring,".TRIP = SCT_TRIP", gstring,".TRIP_ID
  #               ORDER BY SCT_TRIP", gstring,".CAPTAIN,
  #               SCT_TRIP", gstring,".TRIP_ID,
  #               SCT_BIO", gstring,".TAG_ID,
  #               SCT_CAPTURE", gstring,".CAPTURE_DATE", sep = "")
  
  resbio <- ROracle::dbSendQuery(conn, query)
  da <-  ROracle::fetch(resbio)
  
  ROracle::dbDisconnect(conn)
  
  da$CAPTURE_DATE = as.Date(da$CAPTURE_DATE)
  da$RELEASE_DATE = as.Date(da$RELEASE_DATE)
  da = unique(da)
  da$SAMPLE_NUM = NULL
  da$TRIP_ID = NULL
  da$CAPTAIN = NULL
  da$REPORTED = NULL
  da$SAMPLE_ID = NULL
  
  names(da) = c("PID", "capdate", "caparea","caplat", "caplon", "year","relcode", "area", "sampyear", "sampdat", "samplat", "samplon")
  #names(da) = c("PID", "capdate", "caparea","caplat", "caplon", "year", "area","relcode", "sampyear", "sampdat", "rellat", "rellon")
  previd = ""
  # da = da[order(da$PID),]
  for(i in 1:nrow(da)){
    if(da$PID[i] == previd){
      da$samplat[i] = da$caplat[i-1]
      da$samplon[i] = da$caplon[i-1]
      da$sampdat[i] = da$capdat[i-1]
    }
    previd = da$PID[i] 
  }
  
  names(da) = c("PID", "capdate", "caparea","caplat", "caplon", "year", "area","relcode", "sampyear", "sampdat", "rellat", "rellon")
  
  return(da)
}

#' @title readcsvnew2
#' @import readxl dplyr
#' @export
readcsvnew2 <- function(file, ...){
  
  #make sure file is a csv file
  if (!grepl(".csv$", file)) {
    stop("Uploaded file must be a .csv file!")
  }
  
### G.Element edit: commentted most of this out to avoid temporary file creation/saving, so now all this function does is check that file is a .csv then run upload_from_file3
  
  #saving the data as an .rda file was the quickest format to move in and out of
  #had some formatting issues with saving as .csv first
  # my_data <- read.csv(file)
  # tempdata <- paste0(tempdir(),"\\data.Rda")
  # 
  # save(my_data, file=tempdata)
  # load(file=tempdata)
  # 
  # #save location to a temporary location and pass the location to the upload_from_file3 function
  # #this can be streamlined by just passing data around instead of reading/writing files but
  # #is usefull for debugging
  # file_location = tempfile(pattern = "uploaded_file_data", tmpdir = tempdir(), fileext = ".csv")
  # 
  # #file_location = "C:/Users/mckinnonsea/Desktop/New_Lobtag/LobTag/file_uploads/data.csv"
  # write.csv(my_data, file_location, row.names = F)
  # 
  # #remove temporary file is super important because you can't really dig these out
  # #remove the csv we saved locally as well.
  #file.remove(tempdata)
  # 
  #pass file to function that handles upload line by line
  my_return = upload_from_file3(file)
  
  #delete temporary file
  #file.remove(file_location)
  
  return(my_return)
}

#this creates the sql query that we use to get around the permission glitch to upload data to oracle
#used for lbt_path/lbt_paths tables
create_sql_query = function(lbt_table, df){
  
  all_start = "INSERT ALL "
  
  helper = ""
  for (k in 1:length(names(df))){
    if (k==1){
      helper = names(df)[k]
    }
    else {helper = paste(helper, names(df)[k], sep = ", ")}
  }
  
  row_start = paste("INTO ", lbt_table, " (", helper ,") VALUES ('", sep = "")
  footer = " SELECT * FROM DUAL"
  
  #values
  values = ""
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if (j==1){
        values = paste(values, row_start, df[j][i,], sep = "")
      } else {
        values = paste(values,"' , '" ,df[j][i,], sep = "")
      }
    }
    if (i==nrow(df)){
      values = paste(values, "' )", sep = "")
    } else {
      values = paste(values, "' ) ", sep = "")
    }
  }
  
  sql_call = paste(all_start, values, footer, sep = "")
  return(sql_call)
}

is.in = function(are2, lon, lat){
  lon = as.numeric(as.character(lon))
  lat = as.numeric(as.character(lat))
  fn = system.file("extdata", "areas", "areaborders.csv", package = "SCtagging")
  borders= read.csv(file=fn, head=T, sep=",")
  
  b=borders[which(borders$area==are2),]
  
  yli=c(b$slat,b$nlat)
  xli=c(-(b$wlon),-(b$elon))
  
  len = length(lon)
  ew = c(1:len)
  sn = c(1:len)
  fin = c(1:len)
  ew[1:len] = FALSE
  sn[1:len] = FALSE
  fin[1:len] = FALSE
  
  ew[(lon>xli[1]) & (lon<xli[2])] = TRUE
  
  sn[(lat>yli[1]) & (lat<yli[2])] = TRUE
  
  fin[(ew == TRUE) & (sn == TRUE)] = TRUE
  
  
  return(fin)
}

#' @title  absolutely.in.area
#' @description  Function that determines if a position is inside defined polygons 
#' @param area The area name exa. 'cfa23' 'cfa24' 'nens' 'sens' 'gulf' 'cfa4x'
#' @param lon The longitude of position in question
#' @param lat The latitude of position in question
#' @import sp rgeos
#' @return TRUE or FALSE
#' @export
absolutely.in.area = function(area, abslon, abslat){
  # this function should just take in a lat/long and then return an area...
  # or in lobster's case... and LFA
  
  #pa1
  p1 = c(-60.41568418984078,47.03853523297152)
  p2 = c(-60.00014897319883,47.83408958793515 )
  p3 = c(-60, 50.3)
  p4 = c(-68, 50)
  p5 = c(-65.3, 44.25)
  p6 = c(-61, 46)
  p7 = c(-60.41568418984078,47.03853523297152)
  are = rbind(p1, p2, p3, p4, p5, p6, p7)
  
  lat1 = are[,1]
  lon1 = are[,2]

  pa1 = Polygon(are)
  paa = Polygons(list(pa1), "s1")
  gulf = SpatialPolygons(list(paa), 1:1)
  
  #pa2
  p1 = c(-60.41568418984078,47.03853523297152)
  p2 = c(-60.00014897319883,47.83408958793515 )
  p3 = c(-57.5, 46)
  p4 = c(-57.78560987011954,46.00106076110973) 
  p5 = c(-59.85247480316924,46.00291960992756) 
  p6 = c(-61, 46)
  p7 = c(-60.41568418984078,47.03853523297152)
  are = rbind(p1, p2, p3, p4, p5, p6, p7)
  pa2 = Polygon(are)
  paa = Polygons(list(pa2), "s1")
  nens = SpatialPolygons(list(paa), 1:1)
  
  lat2 = are[,1]
  lon2 = are[,2]
  
  #pa3
  p1 = c(-57.78560987011954,46.00106076110973) 
  p2 = c(-59.85247480316924,46.00291960992756) 
  p3 = c(-61, 46)
  p4 = c(-60.66040620990439,45.58083805201212)
  p5 = c(-59.11881785180857,43.67610276909335)
  p6 = c(-56.5, 44)
  p7 = c(-57.78560987011954,46.00106076110973) 
  
  are = rbind(p1, p2, p3, p4, p5, p6, p7)
  pa3 = Polygon(are)
  paa = Polygons(list(pa3), "s1")
  twothree = SpatialPolygons(list(paa), 1:1)
  
  lat3 = are[,1]
  lon3 = are[,2]
  
  #pa4
  p1 = c(-61, 46)
  p2 = c(-60.66040620990439,45.58083805201212)
  p3 = c(-59.11881785180857,43.67610276909335)
  p4 = c(-63.33161397633095,42.50186912534272)
  p5 = c(-63.33296001561555,44.33343763428088)
  p6 = c(-63.52502801857509,44.5005704612574)
  p7 = c(-64, 45)
  p8 = c(-61, 46)
  are = rbind(p1, p2, p3, p4, p5, p6, p7, p8)
  pa4 = Polygon(are)
  paa = Polygons(list(pa4), "s1")
  twofour = SpatialPolygons(list(paa), 1:1)
  
  lat4 = are[,1]
  lon4 = are[,2]
  
  #pa5
  p1 = c(-63.52502801857509,44.5005704612574)
  p2 = c(-63.33296001561555,44.33343763428088)
  p3 = c(-63.33161397633095,42.50186912534272)
  p4 = c(-66.5, 42)
  p5 = c(-65, 45.5)
  p6 = c(-63.52502801857509,44.5005704612574)
  are = rbind(p1, p2, p3, p4, p5, p6)
  pa5 = Polygon(are)
  paa = Polygons(list(pa5), "s1")
  xxxx = SpatialPolygons(list(paa), 1:1)
  
  lat5 = are[,1]
  lon5 = are[,2]
  
  #pa6
  p1 = c(-61.4,44)
  p2 = c(-61.4,45.5)
  p3 = c(-58,45.5)
  p4 = c(-58,44)
  p5 = c(-61.4,44)
  are = rbind(p1, p2, p3, p4, p5)
  pa6 = Polygon(are)
  paa = Polygons(list(pa6), "s1")
  holes = SpatialPolygons(list(paa), 1:1)
  
  lat6 = are[,1]
  lon6 = are[,2]
  
  #pa7
  p1 = c(-62.5,43.8)
  p2 = c(-62.5,45.7)
  p3 = c(-59.2,45.7)
  p4 = c(-59.2,43.8)
  p5 = c(-62.5,43.8)
  are = rbind(p1, p2, p3, p4, p5)
  pa7 = Polygon(are)
  paa = Polygons(list(pa7), "s1")
  twofourz = SpatialPolygons(list(paa), 1:1)
  
  lat7 = are[,1]
  lon7 = are[,2]
  
  #pa8
  p1 = c(-60.6,44.1)
  p2 = c(-60.6,46.1)
  p3 = c(-57.75,46.1)
  p4 = c(-57.75,44.1)
  p5 = c(-60.6,44.1)
  are = rbind(p1, p2, p3, p4, p5)
  pa8 = Polygon(are)
  paa = Polygons(list(pa8), "s1")
  twothreez = SpatialPolygons(list(paa), 1:1)
  
  lat8 = are[,1]
  lon8 = are[,2]
  
  #pa9
  p1 = c(-60.41568418984078,47.03853523297152)
  p2 = c(-60.00014897319883,47.83408958793515 )
  p3 = c(-60, 50.3)
  p4 = c(-68, 50)
  p5 = c(-65.3, 44.25)
  p6 = c(-61, 46)
  p7 = c(-60.41568418984078,47.03853523297152)
  are = rbind(p1, p2, p3, p4, p5, p6, p7)
  pa9 = Polygon(are)
  paa = Polygons(list(pa9), "s1")
  gulf = SpatialPolygons(list(paa), 1:1)
  
  lat9 = are[,1]
  lon9 = are[,2]
  
  #pa10
  p1 = c(-60.41568418984078,47.03853523297152)
  p2 = c(-60.00014897319883,47.83408958793515 )
  p3 = c(-57.5, 46)
  p4 = c(-57.78560987011954,46.00106076110973) 
  p5 = c(-59.85247480316924,46.00291960992756) 
  p6 = c(-61, 46)
  p7 = c(-60.41568418984078,47.03853523297152)
  are = rbind(p1, p2, p3, p4, p5, p6, p7)
  pa10 = Polygon(are)
  paa = Polygons(list(pa10), "s1")
  nens = SpatialPolygons(list(paa), 1:1)
  
  lat10 = are[,1]
  lon10 = are[,2]
  
  #pa11
  p1 = c(-57.78560987011954,46.00106076110973) 
  p2 = c(-59.85247480316924,46.00291960992756) 
  p3 = c(-61, 46)
  p4 = c(-60.66040620990439,45.58083805201212)
  p5 = c(-59.11881785180857,43.67610276909335)
  p6 = c(-56.5, 44)
  p7 = c(-57.78560987011954,46.00106076110973) 
  are = rbind(p1, p2, p3, p4, p5, p6, p7)
  pa11 = Polygon(are)
  paa = Polygons(list(pa11), "s1")
  twothree = SpatialPolygons(list(paa), 1:1)
  
  lat11 = are[,1]
  lon11 = are[,2]
  
  #pa12
  p1 = c(-61, 46)
  p2 = c(-60.66040620990439,45.58083805201212)
  p3 = c(-59.11881785180857,43.67610276909335)
  p4 = c(-63.33161397633095,42.50186912534272)
  p5 = c(-63.33296001561555,44.33343763428088)
  p6 = c(-63.52502801857509,44.5005704612574)
  p7 = c(-64, 45)
  p8 = c(-61, 46)
  are = rbind(p1, p2, p3, p4, p5, p6, p7, p8)
  pa12 = Polygon(are)
  paa = Polygons(list(pa12), "s1")
  twofour = SpatialPolygons(list(paa), 1:1)
  
  lat12 = are[,1]
  lon12 = are[,2]
  
  #pa13
  p1 = c(-63.52502801857509,44.5005704612574)
  p2 = c(-63.33296001561555,44.33343763428088)
  p3 = c(-63.33161397633095,42.50186912534272)
  p4 = c(-66.5, 42)
  p5 = c(-65, 45.5)
  p6 = c(-63.52502801857509,44.5005704612574)
  are = rbind(p1, p2, p3, p4, p5, p6)
  pa13 = Polygon(are)
  paa = Polygons(list(pa13), "s1")
  xxxx = SpatialPolygons(list(paa), 1:1)
  
  lat13 = are[,1]
  lon13 = are[,2]
  
  #pa14
  p1 = c(-61.4,44)
  p2 = c(-61.4,45.5)
  p3 = c(-58,45.5)
  p4 = c(-58,44)
  p5 = c(-61.4,44)
  are = rbind(p1, p2, p3, p4, p5)
  pa14 = Polygon(are)
  paa = Polygons(list(pa14), "s1")
  holes = SpatialPolygons(list(paa), 1:1)
  
  lat14 = are[,1]
  lon14 = are[,2]
  
  #pa15
  p1 = c(-62.5,43.8)
  p2 = c(-62.5,45.7)
  p3 = c(-59.2,45.7)
  p4 = c(-59.2,43.8)
  p5 = c(-62.5,43.8)
  are = rbind(p1, p2, p3, p4, p5)
  pa15 = Polygon(are)
  paa = Polygons(list(pa15), "s1")
  twofourz = SpatialPolygons(list(paa), 1:1)
  
  lat15 = are[,1]
  lon15 = are[,2]
  
  #pa16
  p1 = c(-60.6,44.1)
  p2 = c(-60.6,46.1)
  p3 = c(-57.75,46.1)
  p4 = c(-57.75,44.1)
  p5 = c(-60.6,44.1)
  are = rbind(p1, p2, p3, p4, p5)
  pa16 = Polygon(are)
  paa = Polygons(list(pa16), "s1")
  twothreez = SpatialPolygons(list(paa), 1:1)
  
  lat16 = are[,1]
  lon16 = are[,2]
  
  
ggplot() + 
  geom_polygon(data = pa1, aes(x = lon1, y = lat1), colour='black', fill='white') +
  geom_polygon(data = pa2, aes(x = lon2, y = lat2), colour='black', fill='white') +
  geom_polygon(data = pa3, aes(x = lon3, y = lat3), colour='black', fill='white') +
  geom_polygon(data = pa4, aes(x = lon4, y = lat4), colour='black', fill='white') +
  geom_polygon(data = pa5, aes(x = lon5, y = lat5), colour='black', fill='white') +
  geom_polygon(data = pa6, aes(x = lon6, y = lat6), colour='black', fill='white') +
  geom_polygon(data = pa7, aes(x = lon7, y = lat7), colour='black', fill='white') +
  geom_polygon(data = pa8, aes(x = lon8, y = lat8), colour='black', fill='white') +
  geom_polygon(data = pa9, aes(x = lon9, y = lat9), colour='black', fill='white') +
  geom_polygon(data = pa10, aes(x = lon10, y = lat10), colour='black', fill='white') +
  geom_polygon(data = pa11, aes(x = lon11, y = lat11), colour='black', fill='white') +
  geom_polygon(data = pa12, aes(x = lon12, y = lat12), colour='black', fill='white') +
  geom_polygon(data = pa13, aes(x = lon13, y = lat13), colour='black', fill='white') +
  geom_polygon(data = pa14, aes(x = lon14, y = lat14), colour='black', fill='white') +
  geom_polygon(data = pa15, aes(x = lon15, y = lat15), colour='black', fill='white') +
  geom_polygon(data = pa16, aes(x = lon16, y = lat16), colour='black', fill='white')
  
  
  pb = SpatialPoints(rbind(c(as.numeric(abslon), as.numeric(abslat))))
  
  
  
  # all.holes,44,45.5,61.4,58
  bo = FALSE
  if(area == "cfa23"){
    if(gContains(twothree, pb)) bo = TRUE
  }
  
  if(area == "cfa23zoom"){
    if(gContains(twothreez, pb)) bo = TRUE
  }
  if(area == "cfa24"){
    if(gContains(twofour, pb)) bo = TRUE
  }
  if(area == "cfa24zoom"){
    if(gContains(twofourz, pb)) bo = TRUE
  }
  if(area == "cfa4x"){
    if(gContains(xxxx, pb)) bo = TRUE
  }
  if(area == "nens"){
    if(gContains(nens, pb)) bo = TRUE
  }  
  if(area == "nens_gulf"){
    if(gContains(nens, pb) | gContains(gulf, pb)) bo = TRUE
  }  
  
  
  if(area == "sens"){
    if(gContains(twofour, pb) | gContains(twothree, pb)) bo = TRUE
  } 
  if(area == "gulf"){
    if(gContains(gulf, pb)) bo = TRUE
  } 
  if(area == "all" | area == "ens"){
    if(gContains(twofour, pb) | gContains(twothree, pb) | gContains(xxxx, pb) | gContains(nens, pb)) bo = TRUE
  }
  if(area == "allandgulf"){
    if(gContains(twofour, pb) | gContains(twothree, pb) | gContains(xxxx, pb) | gContains(nens, pb) | gContains(gulf, pb)) bo = TRUE
  }
  if(area == "all.holes"){
    if(gContains(holes, pb)) bo = TRUE
  }  
  
  return(bo)
  
}

#' @title degminsec2decdeg
#' @description  Function that converts degree minutes decimal seconds to decimal degrees 
#' @param ddmmss.ss The coordinate to convert
#' @return numeric converted coordinate
#' @import stringr
#' @export
degmin2decdeg = function(ddmmss.ss){
  dms = as.character(ddmmss.ss)
  neg = grepl("-", dms)
  dms = str_replace(dms, "-", "")
  decsec = unlist(strsplit(dms, "\\."))[2]
  if(is.na(decsec)) decsec = "0" 
  decsec = paste(".", decsec, sep = "") 
  degminsec = unlist(strsplit(dms, "\\."))[1]
  sec = substr(degminsec, nchar(degminsec)-1, nchar(degminsec))
  min = substr(degminsec, nchar(degminsec)-3, nchar(degminsec)-2)
  deg = substr(degminsec, 1, nchar(degminsec)-4)
  sec = as.numeric(decsec) + as.numeric(sec)
  decmin = as.numeric(min) + sec/60
  decdeg = as.numeric(deg) + decmin/60
  if(neg) decdeg = decdeg*-1
  return(decdeg)
}

#' @title  get.pathdata.tid
#' @description  Return calculated paths by supplied TID
#' @import ROracle
#' @return dataframe
#' @export
get.pathdata.tid = function(region = "ScotianShelf", tid = ""){
  gstring = ""
  
  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  
  da = NULL
  
  pathsdb = paste("LOBSTER",".","LBT_PATHS", sep ="")
  
  #new lobster code
  query = paste("SELECT * FROM ", pathsdb , " where ", pathsdb, ".TID = '", tid, "'", sep = "")
  
  resbio <- ROracle::dbSendQuery(con, query) 
  da <- ROracle::fetch(resbio)
  
  da = da[order(da$CID, da$POS),]
  ROracle::dbDisconnect(con)
  
  return(da)
}
