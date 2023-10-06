#' @title  getBetween
#' @description  Return from a text between search pattern. Helps facilitate letter format changes. 
#' @param text The text field to search
#' @param pattern The pattern to match
#' @export
getBetween = function(text = "", pattern = ""){
  text = substr(text, gregexpr(pattern, text)[[1]][1], gregexpr(pattern, text)[[1]][2]-1)
  text = substr(text, regexpr(">", text)[1]+1, regexpr("</", text)[1]-1)
  return(text)
}

#' @title  tag.all.rewarded
#' @description  Update the database to reflect that awards were sent. 
#' @import ROracle DBI
#' @export
tag.all.rewarded = function(update = F){
  if(update){
    drv <- DBI::dbDriver("Oracle")
    con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
    
    captdb = paste("LOBSTER",".","LBT_CAPTURE", sep = "")
    
    toda = paste("Update ", captdb, " set REWARDED = 'Y' where REWARDED = 'N'", sep = "")
    #toda = "Update SCT_CAPTURE set REWARDED = 'N' where REWARDED = 'M'"
    ROracle::dbSendQuery(con, toda)
    ROracle::dbDisconnect(con)
  }
}

#' @title  rewards.knit.document
#' @description  The main function call to create reward pdf from R. 
#' @import knitr rmarkdown
#' @export
rewards.knit.document = function(region = "ScotianShelf", other, version){
  #if(region == "ScotianShelf"){
    #render('knit_rewards.Rmd', 'all')
    render(input = 'knit_rewards.Rmd', output_format = 'pdf_document', output_file = "final_knit_document")
    message("PDF of reward letters created. Review carefully and if happy print double sided. Once rewards are sent out by mail call tag.all.rewarded(update = T) to update the database to reflect that the tags included in this report have been rewarded")
  #}
  ##Put other regions letter generation Rmd file to render here:
  return("Done")
}

#' @title process_returns_for_web
#' @export
process_returns_for_web = function(){
  #easier to group functions here than in the javascript
  shortestpaths.SC()
  rewards.loop.function()
  out = paste("All good")
  return(out)
}

#' @title  rewards.loop.function
#' @description  function that will generate all the rewards emails and put them in a folder
#' @import rmarkdown tinytex
#' @export
rewards.loop.function = function(tag.markdown.location = "C:/bio/LobTag/knit_rewards.Rmd", 
                                 working.emails.location = "R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/LobTag_outputs/Email Attachments/"){
  perlist = generate.reward.data()

  markdownfilepath = tag.markdown.location
  #markdownfilepath = "C:/bio/LobTag/knit_rewards2.Rmd"
  #k = 1
  for(k in 1:length(perlist)){
    #anon_path = "R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/Taggging Files/Maps/Tags Only/"
    #email_path = "C:/Users/mckinnonsea/Desktop/New_Lobtag/LobTag/inst/extdata/emails_attachments/"
    email_path = working.emails.location
    
    #if we put the fishername with a ".pdf" on the end of it before passing it
    #to the markdown function we can create filenames with spaces in them which will match
    #the formatting of the fishername in the oracle table so we don't have to do any extra
    #steps when looking up fisher data from the file names later on.
    pdf_file_name = paste(email_path, perlist[[k]]$name, ".pdf", sep = "")
    
    #render function needs TinyTex installed to run properly. Check if installed and install if not
    if(tinytex::is_tinytex() %in% FALSE){tinytex::install_tinytex()}
    
    rmarkdown::render(input = markdownfilepath,
                      output_format = 'pdf_document',
                      output_file = pdf_file_name,
                      params = list(i = k, perlist = perlist))
    
    #temporary for now, better to use an anon argument in the rewards chart function.
    # render(input = markdownfilepath,
    #                   output_format = 'pdf_document',
    #                   output_file = paste(anon_path,
    #                                       perlist[[k]]$matcheddata$TAG_ID,
    #                                       sep = ""),
    #                   params = list(i = k, perlist = perlist))
    
    # perlist[[k]]$matcheddata$TAG_ID
    #rewards.knit.document(other = perlist[1], version = 1)
    print("done")
    #return(perlist[i])
  }
  return(TRUE)
}

#' @title  generate.reward.data
#' @description  Gather Reward data in preparation for document creation with rewards.knit.document() . 
#' @import ROracle DBI stringi lubridate
#' @export
generate.reward.data = function(region = "ScotianShelf"){
  # Letter text and formatting, be careful when changing text to be aware of return line formatting
  lettertxt = rewards.letter.fill()
  
  #its never gulf
  gstring = ""
  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  
  da = NULL
  
  #initialize lobster databases
  lbiodb = paste("LOBSTER",".","LBT_BIO", sep = "")
  captdb = paste("LOBSTER",".","LBT_CAPTURE", sep = "")
  tripdb = paste("LOBSTER",".","LBT_TRIP", sep = "")
  peopdb = paste("LOBSTER",".","LBT_PEOPLE", sep = "")
  pathdb = paste("LOBSTER",".","LBT_PATH", sep = "")
  
  # new lobster code
  query = paste("SELECT LOBSTER.LBT_BIO.TAG_ID,
                LOBSTER.LBT_BIO",".SAMPLE_NUM,
                LOBSTER.LBT_CAPTURE",".CAPTURE_DATE,
                LOBSTER.LBT_CAPTURE",".STATSAREA,
                LOBSTER.LBT_CAPTURE",".LAT_DD_DDDD AS CAPLAT,
                LOBSTER.LBT_CAPTURE",".LONG_DD_DDDD AS CAPLON,
                LOBSTER.LBT_CAPTURE",".YEAR,
                LOBSTER.LBT_CAPTURE",".RELCODE,
                LOBSTER.LBT_CAPTURE",".REWARDED,
                LOBSTER.LBT_TRIP",".TRIP_ID,
                LOBSTER.LBT_TRIP",".STATSAREA AS STATSAREA1,
                LOBSTER.LBT_TRIP",".YEAR AS YEAR1,
                LOBSTER.LBT_TRIP",".CAPTAIN,
                LOBSTER.LBT_TRIP",".REPORTED,
                LOBSTER.LBT_TRIP",".RELEASE_DATE,
                LOBSTER.LBT_SAMPLE",".LAT_DD_DDDD AS RELLAT,
                LOBSTER.LBT_SAMPLE",".LONG_DD_DDDD AS RELLON,
                LOBSTER.LBT_SAMPLE",".SAMPLE_ID,
                LOBSTER.LBT_PEOPLE",".NAME,
                LOBSTER.LBT_PEOPLE",".EMAIL,
                LOBSTER.LBT_PEOPLE",".CIVIC,
                LOBSTER.LBT_PEOPLE",".TOWN,
                LOBSTER.LBT_PEOPLE",".PROV,
                LOBSTER.LBT_PEOPLE",".POST,
                LOBSTER.LBT_PEOPLE",".LFA
                FROM LOBSTER.LBT_CAPTURE","
                INNER JOIN LOBSTER.LBT_BIO","
                ON LOBSTER.LBT_BIO",".TAG_ID = LOBSTER.LBT_CAPTURE",".TAG
                INNER JOIN LOBSTER.LBT_SAMPLE","
                ON LOBSTER.LBT_BIO",".SAMPLE_NUM = LOBSTER.LBT_SAMPLE",".SAMPLE_ID
                INNER JOIN LOBSTER.LBT_TRIP","
                ON LOBSTER.LBT_SAMPLE",".TRIP = LOBSTER.LBT_TRIP",".TRIP_ID
                INNER JOIN LOBSTER.LBT_PEOPLE","
                ON LOBSTER.LBT_PEOPLE",".NAME = LOBSTER.LBT_CAPTURE",".PERSON
                ORDER BY LOBSTER.LBT_TRIP",".CAPTAIN,
                LOBSTER.LBT_TRIP",".TRIP_ID,
                LOBSTER.LBT_BIO",".TAG_ID,
                LOBSTER.LBT_CAPTURE",".CAPTURE_DATE", sep = "")
  
  resbio <- ROracle::dbSendQuery(con, query)
  da <- ROracle::fetch(resbio) #Get all capture data
  tid = da$TAG_ID[which(da$REWARDED == 'N')] #Get tag ids of unrewarded returns
  da = da[which(da$TAG_ID %in% tid),] #Get all required data for plotting tag histories of unrewarded 
  da$CAPTURE_DATE = as.character(as.Date(da$CAPTURE_DATE))
  da$RELEASE_DATE = as.character(as.Date(da$RELEASE_DATE))
  dx = unique(da)
  previd = ""
  
  da = dx
  
  da$CAPTURE_DATE = lubridate::ymd(da$CAPTURE_DATE)
  da$RELEASE_DATE = lubridate::ymd(da$RELEASE_DATE)
  
  da = da[order(da$CAPTURE_DATE),] #Proper history order for positional reformatting
  da = da[order(da$TAG_ID),] #Proper tag order for positional reformatting
  #loop through and set position and dates of previous release location to 
  #the proper release not the sample release
  #i=1
  
  ##### only go through tags that have paths
  #TAG_ID from paths
  
  #############
  
  for(i in 1:nrow(da)){
    if(da$TAG_ID[i] == previd){
      da$RELLAT[i] = da$CAPLAT[i-1]
      da$RELLON[i] = da$CAPLON[i-1]
      da$RELEASE_DATE[i] = da$CAPTURE_DATE[i-1]
    }
    previd = da$TAG_ID[i] 
  }
  perlist = list() #set up list to hold relevant data
  persplit = split(da, da$NAME)
  
#for troubleshooint a specific person this will print out all the i's and the corresponding name
# for(i in 1:length(persplit)){
#   per = list()
#   per$data = persplit[[i]]
#   per$name = per$data$NAME[1]
#   per$email = per$data$EMAIL
#   print(paste(i, per$name, sep = " " ))
# }
  #i = 4
  #Loop thru each person who needs to be rewarded.
  for(i in 1:length(persplit)){
    if(i < 3000){ #Test toggle so full report isnt generated while testing ex change 3000 to 3
      create_letter = TRUE #default letter creation to true, don't update fisher if their tags have been caught
      per = list()
      per$data = persplit[[i]]
      per$name = per$data$NAME[1]
      per$email = per$data$EMAIL
      #Add all relevant tag data to persons frame
      per$matcheddata = da[which(da$TAG_ID %in% per$data$TAG_ID),]
      #if this name has been completely rewarded, don't go into the loop
      #if(per$name != 'unknown' & per$data$REWARDED != 'Y'){
      if(per$name != 'unknown'){
        #Following id for code folding to exclude reading letter creation step
        if(TRUE){  
          per$addresslabel = getBetween(lettertxt, "PARAGRAPH addresslabel" )
          per$paraA = getBetween(lettertxt, "PARAGRAPH A")
          per$paraB = getBetween(lettertxt, "PARAGRAPH B")
          per$paraB = sub("<name>", per$name, per$paraB)
          #new paragraph for hat pickup locations
          per$pickup = getBetween(lettertxt, "PARAGRAPH pickup")
          if(length(unique(per$data$TAG_ID))>1){
            per$paraB = gsub("<tag/tags>", "tags", per$paraB)
          }else{
            per$paraB = gsub("<tag/tags>", "tag", per$paraB)
          }
          if(all(per$data$REWARDED == "Y")){
            per$mytagcapturedbutihavenoreturns = getBetween(lettertxt, "PARAGRAPH mytagcapturedbutihavenoreturns" )
            create_letter = FALSE #don't create letter if this fisher hasn't returned anything
          }else{
            per$mytagcapturedbutihavenoreturns = ""
          }
          per$info = getBetween(lettertxt, "PARAGRAPH info" )
          if(length(unique(per$data$TAG_ID))>1){
            if(length(unique(per$data$YEAR1))>1){
              per$info = gsub("<was/were/wereall>", "were", per$info)
              per$info = gsub("<yeartagged/yearstagged/season/seasons>", paste(stri_reverse(sub(",", "dna ", stri_reverse(paste0(unique(per$data$YEAR1), collapse = ", ")))), " seasons", sep = ""), per$info)
            }else{
              per$info = gsub("<was/were/wereall>", "were all", per$info)
              per$info = gsub("<yeartagged/yearstagged/season/seasons>", paste(unique(per$data$YEAR1), " season", sep = ""), per$info)
            }
          }else{
            per$info = gsub("<was/were/wereall>", "was", per$info)
            per$info = gsub("<yeartagged/yearstagged/season/seasons>", paste(unique(per$data$YEAR1), " season", sep = ""), per$info)
          }
          
          capbefore = F
          capbeforegto = F
          capafter = F
          capaftergto = F
          cb = 0
          ca = 0
          utid = unique(per$data$TAG_ID)
          #k = 1
          for(k in 1:length(utid)){
            cb = max(cb, length(which(per$matcheddata$CAPTURE_DATE[which(per$matcheddata$TAG_ID == utid[k])] <  per$data$CAPTURE_DATE[which(per$data$TAG_ID == utid[k])])))
            if(cb > 0) capbefore = T
            ca = max(ca, length(which(per$matcheddata$CAPTURE_DATE[which(per$matcheddata$TAG_ID == utid[k])] >  per$data$CAPTURE_DATE[which(per$data$TAG_ID == utid[k])])))
            if(ca > 0) capafter = T
          }
          if(capbefore & !capafter){
            per$capbefore = getBetween(lettertxt, "PARAGRAPH capturedbefore" )
            if(cb == 1){
              per$capbefore = gsub("<onebefore/somebefore>", "One", per$capbefore)
              per$capbefore = gsub("<wasb/wereb>", "was", per$capbefore)
            }
            if(cb > 1){
              per$capbefore = gsub("<onebefore/somebefore>", "Some", per$capbefore)
              per$capbefore = gsub("<wasb/wereb>", "were", per$capbefore)
            }
          }else{
            per$capbefore = ""
          }
          if(!capbefore & capafter){
            per$capafter = getBetween(lettertxt, "PARAGRAPH capturedafter" )
            if(ca == 1){
              per$capafter = gsub("<oneafter/someafter>", "One", per$capafter)
              per$capafter = gsub("<wasa/werea>", "was", per$capafter)
            }
            if(ca > 1){
              per$capafter = gsub("<oneafter/someafter>", "Some", per$capafter)
              per$capafter = gsub("<wasa/werea>", "were", per$capafter)
            }
          }else{
            per$capafter = ""
          }  
          if(capbefore & capafter){
            per$capturedbeforeandafter = getBetween(lettertxt, "PARAGRAPH capturedbeforeandafter" )
            if(ca == 1){
              per$capturedbeforeandafter = gsub("<oneafter/someafter>", "one", per$capturedbeforeandafter)
              per$capturedbeforeandafter = gsub("<wasa/werea>", "was", per$capturedbeforeandafter)
            }
            if(ca > 1){
              per$capturedbeforeandafter = gsub("<oneafter/someafter>", "some", per$capturedbeforeandafter)
              per$capturedbeforeandafter = gsub("<wasa/werea>", "were", per$capturedbeforeandafter)
            }
            if(cb == 1){
              per$capturedbeforeandafter = gsub("<onebefore/somebefore>", "One", per$capturedbeforeandafter)
              per$capturedbeforeandafter = gsub("<wasb/wereb>", "was", per$capturedbeforeandafter)
            }
            if(cb > 1){
              per$capturedbeforeandafter = gsub("<onebefore/somebefore>", "Some", per$capturedbeforeandafter)
              per$capturedbeforeandaftere = gsub("<wasb/wereb>", "were", per$capturedbeforeandafter)
            }
          }else{
            per$capturedbeforeandafter = ""
          }  
          if(all(per$data$RELCODE == "1")){
            per$released = getBetween(lettertxt, "PARAGRAPH released" )
          }else{
            per$released = ""
          }
          if(all(per$data$RELCODE == "2")){
            per$notreleased = getBetween(lettertxt, "PARAGRAPH notreleased" )
          }else{
            per$notreleased = ""
          }
          if(all(per$data$RELCODE == "3")){
            per$unknownrel = getBetween(lettertxt, "PARAGRAPH unknownrel" )
          }else{
            #per$notreleased = ""
            per$unknownrel = ""
          }
          #if( per$released == "" & per$notreleased == "" & per$notreleased == ""){
          if(per$released == "" & per$notreleased == "" & per$unknownrel == ""){
            per$mixedrelret = getBetween(lettertxt, "PARAGRAPH mixedrelret" )
          }else{
            per$mixedrelret = ""
          }
          
          #if(is.null(per$data$LFA)){}
          if(all(per$data$LFA == "27")){
            per$pickup = gsub("<community>", "As a token of our appreciation for your participation in the lobster tagging program, we have a Lobster Science hat for you. It can be picked up at the office for the Cape Breton Fish Harvesters at your convenience.", per$pickup)
          } else if(all(per$data$LFA %in% c("28", "29", "30"))){
            per$pickup = gsub("<community>", "As a token of our appreciation for your participation in the lobster tagging program, we have a Lobster Science hat for you. It can be picked up at the office for the Richmond County Inshore Fishermen's Association at your convenience.", per$pickup)
          } else if(all(per$data$LFA == "31A")){
            per$pickup = gsub("<community>", "As a token of our appreciation for your participation in the lobster tagging program, we have a Lobster Science hat for you. It can be picked up at the office for the Guysborough County Inshore Fishermen's Association at your convenience. ", per$pickup)
          } else if(all(per$data$LFA == "31B")){
            per$pickup = gsub("<community>", "As a token of our appreciation for your participation in the lobster tagging program, we have a Lobster Science hat for you. It can be picked up at the office for your respective association (Eastern Shore Fisherman's Protective Association or Guysborough County Inshore Fishermen's Association) at your convenience.", per$pickup)
          } else if(all(per$data$LFA == "32")){
            per$pickup = gsub("<community>", "As a token of our appreciation for your participation in the lobster tagging program, we have a Lobster Science hat for you. It can be picked up at the office for the Eastern Shore Fisherman's Protective Association at your convenience.", per$pickup)
          }
        }
        
        per$consider = getBetween(lettertxt, "PARAGRAPH consider")
        per$final = getBetween(lettertxt, "PARAGRAPH final" )
        per$end = getBetween(lettertxt, "PARAGRAPH end" )
        per$mapdisclaimer = getBetween(lettertxt, "PARAGRAPH mapdisclaimer")
        
        if(create_letter){
          #Generates charts for this person and returns a list of file locations to them
          per$charts = rewards.chart(name = per$name, data = per$matcheddata)
          
          perlist[[length(perlist)+1]] = per
        }
      }
    } #End person loop 
  } #End test toggle
  return(perlist)
}

#' @title  rewards.chart
#' @description  Create Reward charts. 
#' @param name The name of the person
#' @param data The data to chart
#' @import rgeos sp spatstat shadowtext ggplot2 ggmap ggthemes ggrepel geosphere RStoolbox raster
#' @export
rewards.chart = function(name = "", data = NULL, 
                         output.location = "R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/LobTag_outputs",
                         working.maps.location = "R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/LobTag_outputs/Temp Files/Working Maps"){
  
  # The lable colors that are later randomized, opted for the following colors that are color blind friendly
  collist <- c("#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  # collist = c('green', 'orange', 'blue', 'red', 'yellow', 'purple', 'pink')
  
  outlist = c()
  clist = list()
  tx = split(data, data$TAG_ID)
  #loop thru each tag series to define boundaries and create geometeries 
  
  #i = 1 #for testing...
  #length(tx) is number of tags, therefor number of maps
  #each map will be saved to 3 different locations: 1 working, 1 anon, 1 to save
  for(i in 1:length(tx)){
    collist = collist[sample.int(7, 7)]
    curt = tx[[i]]
    path = get.pathdata.tid(tid = curt$TAG_ID[1])
    path = path[order(as.numeric(path$CID), as.numeric(path$POS)),]
    path$LON = as.numeric(path$LON)
    path$LAT = as.numeric(path$LAT)
    globalextent = raster::extent(min(path$LON), max(path$LON), min(path$LAT), max(path$LAT))
    #globalextent = extent(-60.2216, -60.2048, 45.718, 45.719) #extent is part of the raster package...
    
    linlats = cbind(curt$RELLAT)
    linlats = rbind(linlats, curt$CAPLAT[nrow(curt)])
    linlons = cbind(curt$RELLON)
    linlons = rbind(linlons, curt$CAPLON[nrow(curt)])
    
    linestring = SpatialLines(list(Lines(Line(cbind(as.numeric(linlons),as.numeric(linlats))), ID=name)))
    bb = data.frame(linestring@bbox)
    
    coords1 <- as.matrix(data.frame(cbind(c(bb[1,1], bb[1,1], bb[1,2], bb[1,2], bb[1,1]), c(bb[2,1], bb[2,2], bb[2,2], bb[2,1], bb[2,1]))))
    bbox.polyc = Polygon(coords = coords1, hole = F)
    bbox.polyc = Polygons(list(bbox.polyc),1)
    bbox.polyc = SpatialPolygons(list(bbox.polyc))
    
    s = (bb[1,2] - bb[1,1])/2
    coords2 <- as.matrix(data.frame(cbind(c(bb[1,1]-s, bb[1,1]-s, bb[1,2]+s, bb[1,2]+s, bb[1,1]-s), c(bb[2,1]-s, bb[2,2]+s, bb[2,2]+s, bb[2,1]-s, bb[2,1]-s))))
    bbox.polyc2 = Polygon(coords = coords2, hole = F)
    bbox.polyc2 = Polygons(list(bbox.polyc2),1)
    bbox.polyc2 = SpatialPolygons(list(bbox.polyc2))
    
    clisind = length(clist)+1
    
    if(length(clist) > 0){
      #loop through each persons data to test if tags can be combined with the map of the current tag 
      for(j in 1:length(clist)){
        # Test if bounding boxes intersect")
        if(gIntersects(clist[[j]]$bbox.poly, bbox.polyc2)){
          # Test if the bounding boxes are reasonably proportional to one annother
          if(min(gArea(clist[[j]]$bbox.poly), gArea(bbox.polyc))/max(gArea(bbox.polyc),gArea(clist[[j]]$bbox.poly)) > .083333 ){
            intersects = F 
            #loop through current tag line lists to test overlaps
            for(k in 1:length(clist[[j]]$linelist)){
              if(gIntersects(linestring, clist[[j]]$linelist[[k]])){
                intersects = T
              }
            }
            #commented out to keep all tags on their own chart
            #if(!intersects) clisind = j #if we get here tag can be combined at this location
          }
        }
      }
    }
    # If we adding to a current map we need to do the following
    if(clisind <= length(clist)){
      temp = clist[[clisind]]
      
      temp$linelist[[length(temp$linelist)+1]] = linestring
      temp$pathlist[[length(temp$pathlist)+1]] = path
      temp$datalist[[length(temp$datalist)+1]] = curt
      temp$globextent = extent(min(temp$globextent@xmin, globalextent@xmin), max(temp$globextent@xmax, globalextent@xmax), min(temp$globextent@ymin, globalextent@ymin), max(temp$globextent@ymax, globalextent@ymax))
      
      clist[[clisind]] = temp
      # If we did not find an good map to add to we need to add a new entry  
    } else{
      #clist[[clisind]] = list(bbox.poly = bbox.polyc, linelist = list(linestring), pathlist = "", datalist = list(curt), globextent = globalextent)
      clist[[clisind]] = list(bbox.poly = bbox.polyc, linelist = list(linestring), pathlist = list(path), datalist = list(curt), globextent = globalextent)
    }
  }
  # Now that combinations are made where desired, set up geographies and 
  # plotting constraints for each map in persons clist
  #i = 1
  
  for(i in 1:length(clist)){
    mdata = clist[[i]]
    
    xmin = xmin(mdata$globextent)
    xmax = xmax(mdata$globextent)
    ymin = ymin(mdata$globextent)
    ymax = ymax(mdata$globextent)
    
    xlen = xmax - xmin
    ylen = ymax - ymin
    
    #Make plotting region visually more square
    while(xlen < ylen){
      xmax = xmax+.1
      xmin = xmin-.1
      xlen =  xmax - xmin
    }
    while(ylen < xlen){
      ymax = ymax+.1
      ymin = ymin-.1
      ylen =  ymax - ymin
    }
    
    while(xlen < ylen){
      xmax = xmax+.001
      xmin = xmin-.001
      xlen =  xmax - xmin
    }
    while(ylen < xlen){
      ymax = ymax+.001
      ymin = ymin-.001
      ylen =  ymax - ymin
    }
    #visually scale plotting area a bit wider
    scale = (ymax-ymin)/100
    xmax = xmax+scale
    xmin = xmin-scale
    
    xlim = c(xmin, xmax)
    ylim = c(ymin, ymax)
    
    #rasters for plot backgrounds
    #Git hub dosnt allow large files user will need to have these stored charts folder
    #C:\bio.data\bio.lobster\data\tagging
    #l1 = brick(file.path('c:', 'bio', '801_LL_WGS84.tif'))
    #l2 = brick(file.path('c:', 'bio', 'atl_merged.tif'))
    
    l1 = brick(file.path('c:', 'bio.data', 'bio.lobster', 'data', 'tagging', '801_LL_WGS84.tif'))
    l2 = brick(file.path('c:', 'bio.data', 'bio.lobster', 'data', 'tagging', 'atl_merged.tif'))
    
    #Expand region
    xmin = xmin - ylen/4  
    xmax = xmax + ylen/4
    ymin = ymin - ylen/4  
    ymax = ymax + ylen/4
    ylen = ymax-ymin
    xlen = xmax-xmin
    
    e = extent(xmin, xmax, ymin, ymax)
    e2 = e #e2 will be representing inset plot boundary, needs to be larger geographic area than main plot
    ear = xlen*ylen
    e2ar = ear
    while(e2ar/20 < ear){
      e2@xmin = e2@xmin - .1
      e2@xmax = e2@xmax + .1
      e2@ymin = e2@ymin - .1
      e2@ymax = e2@ymax + .1
      e2ar = (e2@xmax-e2@xmin) * (e2@ymax-e2@ymin)  
    }
    # Also needs to be significantly large enough to get an idea on where in the world we are 
    while(e2ar < 20){
      e2@xmin = e2@xmin - .1
      e2@xmax = e2@xmax + .1
      e2@ymin = e2@ymin - .1
      e2@ymax = e2@ymax + .1
      e2ar = (e2@xmax-e2@xmin) * (e2@ymax-e2@ymin)  
    }
    
    rectan = data.frame(x = c(e@xmin, e@xmin, e@xmax, e@xmax), ex = c(e@xmin, e@xmax, e@xmax, e@xmin), y = c(e@ymin, e@ymax, e@ymax, e@ymin), ey = c(e@ymax, e@ymax, e@ymin, e@ymin))
    #Get distance for plotting scalebar
    dist = round(distHaversine(c(xmin, ymin), c(xmax, ymin), r=6378137)/1000/8, -1)
    #Round differently for smaller areas
    if(dist < 10){
      dist = ceiling(distHaversine(c(xmin, ymin), c(xmax, ymin), r=6378137)/1000/8)
    }
    hei = ylen/100  
    
    # ggplot()+
    #   geom_raster(data = raster.path, aes(x = x, y = y)) + 
    #   coord_quickmap()
    
    # raster.path = system.file("extdata", "depthraster2.tif", package = "Lobtag")
    # raster.path = system.file("extdata", "depthraster2.tif", package = "Lobtag")
    # my_test <- raster(raster.path)
    
    #Main plot 
    # ggplot() + 
    #   ggRGB(l1, r=1,g=2,b=3) + 
    #   ggtitle("title")
    
    #it doesn't make sense to have if-anon here, as every map will have to have an anon
    #version. Best to loop through twice
    # if(keep_anon == TRUE){
    #   tempname = name
    #   name = paste("Tag: ",reldata$TAG_ID,sep= "")
    # }
    
    # map_counter = 1 working directory
    # map_counter = 2 saved to r drive
    # map_counter = 3 anonymous r drive
    
    # for(map_counter in 1:3){
    #   if(map_counter == 1){
    #     map_path = working.maps.location
    #     savename = paste(gsub(" ", "", name),"-", i, ".pdf", sep = "")
    #     #outlist = c(outlist, paste(working.maps.location, paste(gsub(" ", "", name),"-", i, ".pdf", sep = ""), sep = '/'))
    #   } else if(map_counter == 2) {
    #     map_path = save.maps.location
    #     savename = paste(gsub(" ", "", name),"-", i, ".pdf", sep = "")
    #   } else if(map_counter == 3) {
    #     name = paste("Tag: ",reldata$TAG_ID,sep= "")
    #     map_path = anon.maps.location
    #     savename = paste(gsub(" ", "", reldata$TAG_ID)," ",reldata$CAPTURE_DATE,"-", i, ".pdf", sep = "")
    #   }
    #}
    anon_or_not <- c(FALSE, TRUE)
    for(keep_anon in anon_or_not){
      #keep_anon = TRUE
      print(keep_anon)
    
    if(keep_anon == TRUE){
      anon_temp_name = name
      name = reldata$TAG_ID
      you_caught_lobster = "Lobster Captured: "
      other_caught_lobster = "Lobster Captured: "
    } else {
      you_caught_lobster = "You Caputured: "
      other_caught_lobster = "Other Fisher Captured: "
    }
    mp = ggRGB(l1, r=1, g=2, b=3, maxpixels = 800000, ext = e, ggObj = T)+
      ggtitle(paste(name, i, sep = "-")) + xlab("Longitude") + ylab("Latitude")+
      ggsn::scalebar(x.min = xmin, x.max = xmax, y.min = ymin, y.max = ymax, dist_unit= "km", location = "bottomleft", anchor = c(x = xmin+2*hei, y=ymin+5*hei), transform = T, dist = dist, st.size=4, height=hei, model = 'WGS84')+
      ggsn::north(x.min = xmin, x.max = xmax, y.min = ymin, y.max = ymax, location = "topleft", scale = 0.06, symbol = 10, anchor = c(x = xmin+hei, y=ymax-hei))+
      scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
      geom_hline(yintercept=ymax, color = "red", size = 2)+
      geom_vline(xintercept=xmax, color = "red", size = 2)+
      theme(plot.margin=unit(c(1,.5,.5,-.2),"cm"), 
            title = element_text(size=8),
            axis.title = element_text( size=15, face=2),
            axis.line = element_line(size = 1, colour = "red", linetype=1),
            axis.text.x = element_text( angle = 0, size=15),
            axis.text.y = element_text( angle = 0, size=15))
    
    #Add movement paths for each tag in current plot
    #j=2
    for(j in 1:length(mdata$pathlist)){
      pl = mdata$pathlist[[j]]
      #loop through each pach segment
      #l = 1
      for(l in unique(pl$CID)){
        seg = pl[which(pl$CID == l),]
        #if start and end points are so close together, don't show an arrow...
        distancedf = seg[seg$POS == 1 | seg$POS == max(seg$POS),]
        path_distance = distm(c(distancedf$LON[1], distancedf$LAT[1]), c(distancedf$LON[2], distancedf$LAT[2]), fun = distHaversine)
        if(path_distance < 100){
          #print(paste("short path: ", name, sep = ""))
          alpha = 0
        } else {
          alpha = 0.85
        }
        
        mp = mp+geom_path(data=seg, aes(x = as.numeric(LON), y = as.numeric(LAT)),
                          arrow = arrow(length = unit(0.5, "cm")), colour = 'red', alpha = alpha, size = 1.0)
      }
    }
    # Randomized the label colors, make each plot look a bit more unique
    collist = collist[sample.int(length(collist), length(collist))]
    
    labelframe = NULL
    dat = NULL
    coords = NULL
    #Create a dataframe of all labels for this plot. This is needed for 
    #so that the label repel works on all labels for this plot
    #j = 2
    for(j in 1:length(mdata$datalist)){
      reldata = mdata$datalist[[j]][1,]
      reldata$RELLON = as.numeric(reldata$RELLON)
      reldata$RELLAT = as.numeric(reldata$RELLAT)
      labelframe = rbind(labelframe, cbind(reldata$RELLON, reldata$RELLAT, collist[j],  paste("Tag: ", reldata$TAG_ID," ",as.character(reldata$RELEASE_DATE), sep="")))
      mdata$datalist[[j]]$label = as.character(mdata$datalist[[j]]$CAPTURE_DATE)
      mdata$datalist[[j]]$label[which(mdata$datalist[[j]]$NAME == name)] = paste(you_caught_lobster, mdata$datalist[[j]]$label[which(mdata$datalist[[j]]$NAME == name)], sep = "")
      mdata$datalist[[j]]$label[which(mdata$datalist[[j]]$NAME != name)] = paste(other_caught_lobster, mdata$datalist[[j]]$label[which(mdata$datalist[[j]]$NAME != name)], sep = "")
      labelframe = rbind(labelframe, cbind(as.numeric(mdata$datalist[[j]]$CAPLON), as.numeric(mdata$datalist[[j]]$CAPLAT), collist[j], as.character(mdata$datalist[[j]]$label)))
    }
    

    
    
    
    labelframe = as.data.frame(labelframe)
    names(labelframe) = c("x", "y","colID", "lab")
    labelframe$ID = 1:nrow(labelframe) 
    labelframe$x = as.numeric(as.character(labelframe$x))
    labelframe$y = as.numeric(as.character(labelframe$y))
    labelframe$colID = as.character(labelframe$colID)
    labelframe$lab = as.character(labelframe$lab)
    
    #add points to plot from other captures.... (use label plot df)
    mp = mp + geom_point(data = labelframe, aes(x = x, y = y), color = "black")
    
    #if only no name tags are of interest to make points for make new df from labelframe (you'll have to match the label column with 'other fisher captured')
    
    
    #Add labels to main plot
    mp = mp+geom_label_repel(data=labelframe, aes(x = x, y = y, label = lab),
                             fill = alpha(labelframe$colID,0.5), fontface = "bold",                        
                             color="black", segment.color = 'black', segment.size = .7,
                             family = "Helvetica",
                             box.padding = unit(0.35, "lines"),
                             point.padding = unit(0.5, "lines"))

    #Inset plot
    ip = ggplotGrob(
      ggRGB(l2, r=1, g=2, b=3, maxpixels = 100000, ext = e2, ggObj = T)+
        theme_map()+
        theme(panel.border = element_rect(colour = "black", fill = NA),
              plot.margin=unit(c(0,0,0,0),"cm"))+
        scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
        geom_segment(data=rectan, aes(x = x, y = y, xend = ex, yend = ey),colour=c("red"))
    )
    #Output plot, a combination of the main plot with inset plot positioned correctly
    g3 <- mp +
      annotation_custom(grob = ip, xmin = e@xmax-((e@xmax-e@xmin)/4), xmax = e@xmax+((e@xmax-e@xmin)/20),
                        ymin = e@ymax-((e@ymax-e@ymin)/4), ymax = e@ymax +((e@ymax-e@ymin)/20))
    #Save to file
    #write function to check if filename exists and check filename here
    #add date the tag was caught, and if it's still replicated then 
    
    #need to save 3 versions of the same map: lookup copy, anonymous copy, and then working file copy.
    #fn = system.file("extdata", "rewards", "maps", package = "SCtagging")
    #fn = system.file("extdata", "rewards", "maps", package = "LobTag")
    
    #tempath is the working copy for now
    #norm_path will have tag data and name
    #anon_path will have tag data only
    
    #temppath = "C:/bio/LobTag/temp_maps"
    #norm_path = "R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/Taggging Files/Maps/Tags and Names"
    #anon_path = "R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/Taggging Files/Maps/Tags Only"
    # ggsave(filename = paste(gsub(" ", "", name),"-", i, ".pdf", sep = ""), path = temppath, plot = g3, width = 11, height = 10)
    # ggsave(filename = paste(gsub(" ", "", name),"-", i, ".pdf", sep = ""), path = norm_path, plot = g3, width = 11, height = 10)
    # ggsave(filename = paste(gsub(" ", "", reldata$TAG_ID)," ",reldata$CAPTURE_DATE,"-", i, ".pdf", sep = ""), path = anon_path, plot = g3, width = 11, height = 10)
    #ggsave(filename = paste(gsub(" ", "", name),"-", i, ".pdf", sep = ""), path = fn, plot = mp, width = 11, height = 10)
    if(!keep_anon){
      
      map_path = working.maps.location
      savename = paste(gsub(" ", "", name),"-", i, ".pdf", sep = "")
      ggsave(filename = savename, path = map_path, plot = g3, width = 11, height = 10)
    
      #save to r directory with name, tag number and date.
      savename = paste(gsub(" ", "", name),"-", reldata$TAG_ID, " ", as.character(Sys.Date()),".pdf", sep = "")
      map_path = file.path(output.location,"Maps","Tags and Names")
      ggsave(filename = savename, path = map_path, plot = g3, width = 11, height = 10)
      outlist = c(outlist, paste(working.maps.location, paste(gsub(" ", "", name),"-", i, ".pdf", sep = ""), sep = '/'))
      }
    
    if(keep_anon){
      #save anonymously to r drive
      savename = paste(name, " ", as.character(Sys.Date()),".pdf", sep = "")
      map_path = file.path(output.location,"Maps/Tags Only")
      ggsave(filename = savename, path = map_path, plot = g3, width = 11, height = 10)
      name = anon_temp_name
    }
    
    #outlist is for working copy of map
    #outlist = c(outlist, paste(working.maps.location, paste(gsub(" ", "", name),"-", i, ".pdf", sep = ""), sep = '/'))
    
    
    }
    
    #outlist = c(outlist, paste(working.maps.location, paste(gsub(" ", "", name),"-", i, ".pdf", sep = ""), sep = '/'))
  }
  return(outlist)
}

#' @title rewards.letter.fill
#' @description Letter text for rewards markdown
#' @export
rewards.letter.fill = function(){
  
  lettertxt = "<PARAGRAPH addresslabel>  
  <PARAGRAPH A>Lobster Ecology & Assessment Team
  DFO Science  
  Dartmouth, NS</PARAGRAPH A>
  <PARAGRAPH mytagcapturedbutihavenoreturns>  <name>,  
  \\newline  
  Thank you for participating in the lobster tagging program. I thought you would be interested to know that a lobster you had previously released was captured again and information sent in. This information is shown in a chart provided. Recaptures such as these will help us better track the movement of lobsters across the Scotian shelf.</PARAGRAPH mytagcapturedbutihavenoreturns>
  <PARAGRAPH B><name>,  
  \\newline  
  Thanks for reporting the lobster <tag/tags> caught last season. A chart showing the release and all recapture positions for the <tag/tags> is included. The information provided by tagging recaptures is helpful in determining the movement of lobster throughout the Scotian Shelf. </PARAGRAPH B>
  <PARAGRAPH info>  
  \\newline  
  The tagged lobster you caught <was/were/wereall> tagged in the <yeartagged/yearstagged/season/seasons>.</PARAGRAPH info>
  <PARAGRAPH capturedbefore>  
  \\newline
  <onebefore/somebefore> of the tagged lobster you caught <wasb/wereb> captured before and released.</PARAGRAPH capturedbefore>
  <PARAGRAPH capturedafter>
  \\newline
  \\newline
  <oneafter/someafter> of the tagged lobster you caught and released in the past <wasa/werea> captured this season.</PARAGRAPH capturedafter>
  <PARAGRAPH capturedbeforeandafter>
  \\newline
  <onebefore/somebefore> of the tagged lobster you caught <wasb/wereb> captured before and released. As well, <oneafter/someafter> of the tagged lobster you caught and released in the past <wasa/werea> captured this season.</PARAGRAPH capturedbeforeandafter>
  <PARAGRAPH consider>
  \\newline
  \\newline
  For future tagged lobster recaptures, please let us know if they are released or retained. Additional knowledge will be gained by tracking subsequent recaptures of individual lobsters over their lifespan.</PARAGRAPH consider>
  <PARAGRAPH pickup>
  \\newline
  \\newline
  <community>
  </PARAGRAPH pickup>
  <PARAGRAPH notreleased>If you are interested in participating in tagging lobsters at the close of the commercial season in your area, please send an email to lobtags@gmail.com. </PARAGRAPH notreleased>
  <PARAGRAPH released>If you are interested in participating in tagging lobsters at the close of the commercial season in your area, please send an email to lobtags@gmail.com. </PARAGRAPH released>
  <PARAGRAPH mixedrelret>If you are interested in participating in tagging lobsters at the close of the commercial season in your area, please send an email to lobtags@gmail.com. </PARAGRAPH mixedrelret>
  <PARAGRAPH unknownrel>If you are interested in participating in tagging lobsters at the close of the commercial season in your area, please send an email to lobtags@gmail.com. </PARAGRAPH unknownrel>
  <PARAGRAPH final>  
  \\newline  
  I have included a one page information sheet on our tagging program. On the reverse side of this sheet is a form which can easily be used to record all required information on any tagged lobster you may catch in the future. This entire form can be sent to DFO Science at the end of the lobster season. </PARAGRAPH final>
  <PARAGRAPH end>
  \\newline
  \\newline
  Thanks for your help.
  \\newline
  \\newline
  Ben Zisserson
  \\newline
  (902) 222-5211
  \\newline
  Ben.Zisserson@dfo-mpo.gc.ca</PARAGRAPH end>
  <PARAGRAPH mapdisclaimer>
  * Lobster positions very near the coast may appear to be on land due to map resolution.</PARAGRAPH mapdisclaimer>"
  
  return(lettertxt)
}
