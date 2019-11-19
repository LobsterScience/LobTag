

#' @title  plottags
#' @description  Function that creates movement charts
#' @import stringr PBSmapping
plottags = function(are, years){
    svg(filename = paste("output/tagplots/outchart_", are, "_", years[1], "-", years[length(years)], ".svg", sep = ""), width = 9, height = 9)
    makemap(x, area = are, title=paste("SNOWCRAB TAG DATA    Area:", are, "     Year(s):", years, sep=" "))
da = get.capturedata()
    #Set up variables
    X=NULL
    Y=NULL
    PID=NULL
    POS=NULL
    colour=NULL
    
    #Remove entries that have useless data
    ind = which(as.character(da$caplat) == "0.0")
    if(length(ind)>0) da = da[-ind,]
    ind = which(as.character(da$caplat) == "0")
    if(length(ind)>0) da = da[-ind,]
    
    #Remove positions that have been sampled or captured outside the query area
    ii = is.in(are, da$samplon, da$samplat)
    jj = is.in(are, da$caplon, da$caplat)
    ind = which(!(ii | jj))
    if(length(ind)>0) 
      da = da[-ind,]
    
    #REMOVE GULF ENTRIES
    #ind = which( as.character(da$caparea) == "GULF" & as.character(da$area) == "GULF"  )
    #if(length(ind)>0) 
    #da = da[-ind,]

    syear = da$sampyear
    #Remove data sampled outside the query year, years
    if(years != "all"){
      years = strsplit(years, ",")
      years = unlist(years)
      syear = match(syear, years)
      indic = which(is.na(syear) == TRUE)
      if(length(indic)>0) 
        da = da[-indic,]
    }
    # Remove unknow year data	
    mattxt = "unknown"
    ind = which(as.character(da$year) == mattxt)
    if(length(ind)>0) 
      da = da[-ind,]
    
    dup = NULL
    #Build the plot dataframe, needs to check time since tag event to give a color 
    for (i in 1:nrow(da)) {
      #Need to do differnt opperations for first entry
      #Not first entry
      if(i > 1){
        if(da$PID[i] != da$PID[i-1]){
          dup = 1
          
          X = c(X, da$samplon[i])
          Y = c(Y, da$samplat[i])
          POS = c(POS, dup[1])
          PID = c(PID, da$PID[i])
          colour = c(colour, "white")
        }
        sampchron = chron(da$sampdat[i], format = "y-m-d")
        capchron = chron(da$capdat[i], format = "y-m-d")
        #If capture date is unknow a mid season date is choosen
        if(is.na(capchron)){
          if(da$caparea[i] == "NENS") capchron = chron(paste(da$year[i],"-07-15", sep=""), format = "y-m-d")
          if(da$caparea[i] == "SENS") capchron = chron(paste(da$year[i],"-08-15", sep=""), format = "y-m-d")
          if(da$caparea[i] == "GULF") capchron = chron(paste(da$year[i],"-08-15", sep=""), format = "y-m-d")
          if(da$caparea[i] == "4X") capchron = chron(paste(da$year[i],"-03-15", sep=""), format = "y-m-d")
          
        }
        dif = capchron - sampchron
        dif = as.numeric(dif)
        
        # Color match but days since tag event
        cc = ""
        if(is.na(dif)){ cc = "black" } 
        else if(dif < 180){ cc = "red" }
        else if(dif < 545){ cc = "green" }
        else if(dif < 1090){ cc = "blue" }
        else if(dif < 1455){ cc = "yellow" }
        else{ cc = "purple"}
        dup = dup+1
        
        #Data frame creation
        X = c(X, da$caplon[i])
        Y = c(Y, da$caplat[i])
        POS = c(POS, dup[1])
        PID = c(PID, da$PID[i])
        colour = c(colour, cc)
      }
      #first entry
      else{
        X = c(X,da$samplon[i])
        Y = c(Y,da$samplat[i])
        POS = c(POS, 1)
        PID = c(PID,da$PID[i])
        X = c(X, da$caplon[i])
        Y = c(Y, da$caplat[i])
        POS = c(POS, 2)
        PID = c(PID, da$PID[i])
        colour = c(colour, "white")
        
        sampchron = chron(da$sampdat[i], format = "y-m-d")
        capchron = chron(da$capdat[i], format = "y-m-d")
        
        if(is.na(capchron)){
          if(da$caparea[i] == "NENS") capchron = chron(paste(da$year[i],"-07-15", sep=""), format = "y-m-d")
          if(da$caparea[i] == "SENS") capchron = chron(paste(da$year[i],"-08-15", sep=""), format = "y-m-d")
          if(da$caparea[i] == "GULF") capchron = chron(paste(da$year[i],"-08-15", sep=""), format = "y-m-d")
          if(da$caparea[i] == "4X") capchron = chron(paste(da$year[i],"-03-15", sep=""), format = "y-m-d")
        }
        
        dif = capchron - sampchron
        dif = as.numeric(dif)
        
        cc = ""
        if(is.na(dif)){ cc = "black" } 
        else if(dif < 180){ cc = "red" }
        else if(dif < 545){ cc = "green" }
        else if(dif < 1090){ cc = "blue" }
        else if(dif < 1455){ cc = "yellow" }
        else{ cc = "purple"}
        colour = c(colour, cc)
        dup = 2
      }
    }
    #Set up the legend
    co = c("black", "red", "green", "blue", "yellow", "purple")
    na = c("unknown" ,"same season", "1 season", "2 seasons", "3 seasons", "4+ seasons")
    cocode = cbind(co, na) 
    cocode = data.frame(cocode)
 
    
    x = cbind(PID, POS, X, Y, colour)
    x = data.frame(x)
    x$PID = as.character(x$PID)
    x$POS = trunc(as.numeric(as.character(x$POS)))
    x$X = as.numeric(as.character(x$X))
    x$Y = as.numeric(as.character(x$Y))
    
    #Only keep color that have been encountered
    icoco = match(as.character(cocode$co), as.character(unique(x$colour)))
    icoco = which(is.na(icoco) == TRUE)
    cocode = cocode[-icoco,]
    #Split dataframe by tag ID so that each set of tags can be plotted
    x = split(x, x$PID)
    #Loop through each tag set and plot
    for (i in 1:length(x)) {
      chunk = data.frame(x[i])
      
      names(chunk) = c("PID", "POS", "X", "Y", "colour")
      dd = as.character(chunk$colour)
      arrows(x0 = chunk$X[1:length(chunk$X)-1], y0 =  chunk$Y[1:length(chunk$Y)-1], x1 = chunk$X[2:length(chunk$X)] , y1 = chunk$Y[2:length(chunk$Y)], col = dd[2:length(dd)], angle= 20, code=2, length = 0.06)
      
    }
    
    legend("bottomright", as.character(cocode$na), lty=c(1,1), lwd= 3, col= as.character(cocode$co), cex = .7, bty = "n", title = "Seasons Since Tag Applied")
    dev.off()
    
    
    

    
}

plotsamples = function(are, years, xlim, ylim){
  svg(filename = paste("output/tagplots/outsamples_", are, "_", years[1], "-", years[length(years)], ".svg", sep = ""), width = 9, height = 9)
  makemap(x, area = are, title=paste("SNOWCRAB SAMPLE DATA  area:", are, "  year(s):", years, sep=" "))
  
  con = odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
  
  da = sqlQuery(con, paste("SELECT SCT_TRIP.TRIP_ID, SCT_TRIP.STATSAREA, SCT_TRIP.YEAR, SCT_TRIP.DATE, SCT_SAMPLE.LAT_DD_DDDD, SCT_SAMPLE.LONG_DD_DDDD, SCT_SAMPLE.SAMPLE_ID
                    from SCT_TRIP join SCT_SAMPLE where SCT_SAMPLE.TRIP = SCT_TRIP.TRIP_ID", sep = "") )
  odbcClose(con)
  
  da$sample_id = NULL 
  da$trip_id = NULL
  
  names(da) = c("area", "sampyear","sampdat", "samplat", "samplon")
  
  #Set up needed variables
  res = NULL
  X=NULL
  Y=NULL
  si=NULL
  
  #Remove unrequested data
  ii = is.in(are, da$samplon, da$samplat)
  ind = which(ii == "FALSE")
  if(length(ind)>0) 
    da = da[-ind,]
  
  #REMOVE GULF ENTRIES
  #ind = which(as.character(da$area) == "GULF")
  #if(length(ind)>0) 
  #  da = da[-ind,]
  
  
  library(stringr)
  syear = da$sampyear
  #Remove years that have not been requested
  if(years != "all"){
    years = strsplit(years, ",")
    years = unlist(years)
    syear = match(syear, years)
    indic = which(is.na(syear) == TRUE)
    if(length(indic)>0) 
      da = da[-indic,]
    
  }
  
  #Build the dataframe for proper plotting
  for (i in 1:nrow(da)) {
    X = da$samplon[i]
    Y = da$samplat[i]
    PID = i
    res = rbind(res, cbind(PID, as.numeric(as.character(X)), as.numeric(as.character(Y))))
  }
  
  res = data.frame(res)
  names(res) = c("PID", "X", "Y")
  
  #Need numeric tag id for PBSMapping so gulf tags get changed to fully numeric   
  res$PID = as.numeric(gsub("G", "111111", res$PID))
  res = as.PolyData(res) #To polyset data
  
  addPoints(res, col = "red", lwd=1.5) 
  co = c("red")
  na = c("Sample Locations")
  cocode = cbind(co, na) 
  cocode = data.frame(cocode)
  legend("bottomright", as.character(cocode$na), col= as.character(cocode$co), bty = "n", title = "", pch = 'o')
  dev.off()
}