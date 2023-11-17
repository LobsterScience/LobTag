#' @title upload_from_file3
#' @import dplyr readxl
#' @return note to use will be read in web interface
#' @description parses data, error check and sends to sample_ent function
#' @export
upload_from_file3 <- function(myfile){
  #this function is called by readcsvnew2 in 'LobTagging.r'
  
  #go through excel sheet and upload one chain at a time...
  #has to do one batch per lat/lon send the data to 'sample_ent'
  
  #initilize user message
  out = ""
  
  #data read from CSV, multiple steps for debugging
  my_data = read.csv(myfile)
  my_new_Data <- my_data
  test_data  <- my_data
  
  #format really shouldn't change, but this will help you drop unused columns in case
  #you want to batch upload data from a spreadsheet that isn't the template
  
  #drop_columns <- c("Comments", "lat.dd.dd", "lon.dd.dd", "date2", "Lat", "Lon", "Sampler.3", "Claw.lost.in.trap")
  #after_dropped_columns = test_data[,!(names(test_data) %in% drop_columns)]
  #test_data = after_dropped_columns
  
  #data file doesn't hold any calculations.
  #decimal degrees and degrees minutes formatting done here
  test_data$latddmm.mm = test_data$Lat.Degrees * 100 + test_data$Lat.Minutes
  test_data$londdmm.mm = test_data$Lon.Degrees * 100 + test_data$Lon.Minutes
  
  test_data$latdd.dd = test_data$Lat.Degrees + test_data$Lat.Minutes / 60
  test_data$londd.dd = test_data$Lon.Degrees + test_data$Lon.Minutes / 60
  
  #date column isn't 100% necessary but it's a good indication if things are going wrong
  test_data$Date = paste(test_data$Day, test_data$Month, test_data$Year, sep = "/")
  
  my_new_Data <- test_data

  #removed lat and lon columns (names has to be the same width as data)
  #I've re-arranged the lat/lon calculations above so the order is only
  #correct for the FSRS list here.
  
  ### G.Element edit: existing code takes columns by position, can make this more flexible to just select for desired columns if upload file contains additional columns/different column order:
  select.names = c("Vessel", "Captain", "Port", "LFA", "Sampler", "Sampler.2", "Affiliation", "Day",	"Month", "Year", "Tag.Prefix",	"Tag.Color", "Tag.Num",	"Carapace.Length",	"Sex",	"Shell", "Claw", "V.Notch", "Lat.Degrees",	"Lat.Minutes",	"Lon.Degrees",	"Lon.Minutes", "latddmm.mm", "londdmm.mm", "latdd.dd", "londd.dd", "Date")
  my_new_Data <- dplyr::select(my_new_Data,(all_of(select.names)))
  
  
  
  #all_names_new = c("Vessel", "Port", "Captain",	"LFA",	"Affiliation",	"Sampler",	"Sampler 2",	"Sampler 3", "Tag Prefix",	"Tag Num",	"Tag Color",	"Carapace Length",	"Sex",	"Shell",	"Claw",	"V-Notch",	"Claw lost in trap",	"Day",	"Month", "Year",	"Date",	"Lat Degrees",	"Lat Minutes",	"Lon Degrees",	"Lon Minutes",	"Comments",	"latdd.dd",	"londd.dd", "date2")
  #all_names_new2 = c("Vessel", "Captain", "Port", "LFA", "Sampler", "Sampler 2", "Affiliation", "Day",	"Month", "Year", "Tag Prefix",	"Tag Color", "Tag Num",	"Carapace Length",	"Sex",	"Shell", "Claw", "Lat Degrees",	"Lat Minutes",	"Lon Degrees",	"Lon Minutes")
  all_names_new_test = c("Vessel", "Captain", "Port", "LFA", "Sampler", "Sampler 2", "Affiliation", "Day",	"Month", "Year", "Tag Prefix",	"Tag Color", "Tag Num",	"Carapace Length",	"Sex",	"Shell", "Claw", "V-Notch", "Lat Degrees",	"Lat Minutes",	"Lon Degrees",	"Lon Minutes", "latddmm.mm", "londdmm.mm", "latdd.dd", "londd.dd", "Date")
  #all_names_FSRS = c("Vessel", "Port", "Captain", "LFA", "Affiliation", "Sampler", "Sampler 2", "Tag Prefix","Tag Num",	"Tag Color", 	"Carapace Length",	"Sex",	"Shell", "Claw","V-Notch", "Day",	"Month", "Year","Date", "Lat Degrees",	"Lat Minutes",	"Lon Degrees",	"Lon Minutes", "latddmm.mm", "londdmm.mm", "latdd.dd", "londd.dd")
  
  #old df named columns
  #names(my_new_Data) = all_names_new
  #names(my_new_Data) = all_names_new2
  
  #new template data
  names(my_new_Data) = all_names_new_test
  
  #all of the data should be in each upload
  #bdata and sdata can be mined if format is correct.
  bdata_keep = c("Tag Num","Tag Color","Carapace Length","Sex","Shell","Claw","V-Notch")
  
  #other error checks we could do?
  
  #these lists are to catch errors. Sex must be 1-3 (not optional so no NA): until can figure out why LobTag won't take sex = NA, use 0 for sex = NA 
  #shell is optional but if entered must be 1-7, same with claw and v-notch except 1-3 and yes/no respectively
  my_new_Data <- my_new_Data %>% mutate(Sex = ifelse(Sex  %in% NA, 0,Sex))
  sex_values <- c(0,1,2,3)
  shell_values <- c(NA, 1:7)
  claw_values <- c(NA,1,2,3)
  vnotch_values <- c(NA,"YES","NO")
  carapace_values <- c(NA, 40:150) #this one will alert the user but continue with upload.
  carapace_values_fsrs <- c(NA, 40:170) # fsrs samples some really big females as part of the v-notch program, so increase the threshold in this case. 
  
  
  #create subset to check data integrity, also check for duplicate tags being entered on same sheet.
  repeat_tags = my_new_Data$`Tag Num`[duplicated(my_new_Data$`Tag Num`)==TRUE]
  sex_problems <- subset(my_new_Data, !(Sex %in% sex_values))$'Tag Num'
  shell_problems <- subset(my_new_Data, !(Shell %in% shell_values))$'Tag Num'
  claw_problems <- subset(my_new_Data, !(Claw %in% claw_values))$'Tag Num'
  vnotch_problems <- subset(my_new_Data, !(`V-Notch` %in% vnotch_values))$'Tag Num'
  carapace_problems <- subset(my_new_Data, !(`Carapace Length` %in% carapace_values) & !(Affiliation  %in% "FSRS"))$'Tag Num'
  carapace_problems_fsrs <- subset(my_new_Data, !(`Carapace Length` %in% carapace_values_fsrs) & (Affiliation  %in% "FSRS"))$'Tag Num'
  return_error = FALSE
  
  #error checks for each paramater
  if(length(repeat_tags) > 0){
    for(i in 1:length(repeat_tags)){
      out = paste(out, "\nPossible duplicate tag number: ", repeat_tags[i], sep = "")
    }
    return_error = TRUE
  }
  
  if(length(sex_problems) > 0){
    for(i in 1:length(sex_problems)){
      out = paste(out, "\nCheck Sex of tag: ", sex_problems[i], sep = "")
    }
    return_error = TRUE
  }
  
  if(length(shell_problems) > 0){
    for(i in 1:length(shell_problems)){
      out = paste(out, "\nCheck Shell of tag: ", shell_problems[i], sep = "")
    }
    return_error = TRUE
  }
  
  if(length(claw_problems) > 0){
    for(i in 1:length(claw_problems)){
      out = paste(out, "\nCheck Claw of tag: ", claw_problems[i], sep = "")
    }
    return_error = TRUE
  }
  
  if(length(carapace_problems) > 0){
    for(i in 1:length(carapace_problems)){
      out = paste(out, "\nCarapace of tag is unusual (will still be uploaded): ", carapace_problems[i], sep = "")
    }
  }
  
  if(length(carapace_problems_fsrs) > 0){
    for(i in 1:length(carapace_problems_fsrs)){
      out = paste(out, "\nCarapace of tag is unusual, even for FSRS tagging (will still be uploaded): ", carapace_problems_fsrs[i], sep = "")
    }
  }
  
  if(return_error){
    return(out)
  }
  
  #do data manipulation after error checking
  #fill in all missing tag colours/prefixes based on affiliation: DFO/CBFH is Blue (XY), SWLSS is RED (SWLSS)
  #doesn't overwrite existing tag colours or prefixes
  #check out functions tag_color_filler and tag_prefix_filler to add new affiliations and colours
  my_new_Data[(is.na(my_new_Data$'Tag Color')),]$'Tag Color' <- sapply(my_new_Data[(is.na(my_new_Data$'Tag Color')),]$Affiliation,tag_color_filler)
  my_new_Data[(is.na(my_new_Data$'Tag Prefix')),]$'Tag Prefix' <- sapply(my_new_Data[(is.na(my_new_Data$'Tag Prefix')),]$Affiliation,tag_prefix_filler)

  
  
  #test_data$latdd.dd <- with(my_new_Data, 'Lat Degrees' * 100)
  #test_data = my_new_Data
  #as.numeric(test_data['Lat Degrees'])

  
  #create pipe that groups excel
  #since we are no longer importing latdd.dd and londd.dd these have to be calculated and added to the dataframe
  new_excel_df <- my_new_Data %>%
    group_by(Date, latdd.dd, londd.dd) %>%
    mutate(id = cur_group_id())
  
  #########################################################################################################################
  # the code below isn't used at the moment but can be used to develop an output csv to make sure data upload succesful.  #
  #########################################################################################################################
  #save temporary csvs to computer to load individually
  # for(x in 1:max(new_excel_df$id)){
  #   print(x)
  #   partial_load_df <- new_excel_df  %>% filter(id == x)
  #   write_csv_directory = "C:/Users/mckinnonsea/Desktop/New_Lobtag/LobTag/file_uploads/tempdataframes/"
  #   write_file_name = paste(write_csv_directory, "temp", x, ".csv", sep = "")
  #   write.csv(partial_load_df, write_file_name, row.names = F)
  # }
  
  #load in each csv and upload 1 at a time
  # for(x in list.files(write_csv_directory)){
  #   #print(x)
  #   current_csv = paste(write_csv_directory, x, sep = "")
  #   tempcsvdf <- read.csv(current_csv)
  #   names(tempcsvdf) <- all_names_new
  #   
  #   tempcsvdf2 <- as.data.frame(tempcsvdf)
  #   tempcsvdf2_bdata_df = tempcsvdf2[bdata_keep]
  #   
  #   sdata_list = c(tempcsvdf2["Vessel"][1,1],
  #                  tempcsvdf2["Port"][1,1],
  #                  tempcsvdf2["Captain"][1,1],
  #                  tempcsvdf2["LFA"][1,1],
  #                  tempcsvdf2["Affiliation"][1,1],
  #                  tempcsvdf2["Sampler"][1,1],
  #                  tempcsvdf2["Date"][1,1],
  #                  tempcsvdf2["Lat Degrees"][1,1] * 100 + tempcsvdf2["Lat Minutes"][1,1],
  #                  tempcsvdf2["Lon Degrees"][1,1] * 100 + tempcsvdf2["Lon Minutes"][1,1],
  #                  tempcsvdf2["Comments"][1,1],
  #                  tempcsvdf2["Day"][1,1],
  #                  tempcsvdf2["Month"][1,1],
  #                  tempcsvdf2["Year"][1,1],
  #                  tempcsvdf2["latdd.dd"][1,1],
  #                  tempcsvdf2["londd.dd"][1,1]
  #   )
  #   print(paste(x,max(tempcsvdf[30]),sep = " & "))
  #   sample_ent(bdata = tempcsvdf2_bdata_df, sdata = sdata_list, from_file = TRUE)
  #   
  #   file.remove(current_csv)
  # }
  
  ####################################################################################
  #x = 1
  for(x in 1:max(new_excel_df$id)){
    #look at specific group, this will be looped
    newdf2 <- new_excel_df  %>% filter(id == x)

    #bdata is dataframe, sdata is list
    new_df3 <- as.data.frame(newdf2)
    bdata_df = new_df3[bdata_keep]

    #sdata_list is ultimately what is passed through to sample_ent
    #because it's passed as a list and sample ent removed things based on position in the list
    #if you add or removed any elements here you MUST change the position in sample ent as well
    #positions
    keener_variables = c("Vessel", "Port","Captain", "LFA",
                         "Affiliation", "Sampler", "Date",
                         "Lat Degrees", "Lon Degrees", "Day",
                         "Month", "Year", "latdd.dd", "londd.dd")
    keener_positions = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    
    #if the spreadsheet changes make sure these field names are the same as previous
    #if new fields and the data needs to be passed to Oracle add them to the END of this list
    #and then specify the list number in sample_ent to ensure the data gets all the way through
    #note: if you add variable to the beginning of this list it will change all the variables in
    #sample_ent
    sdata_list = c(new_df3["Vessel"][1,1],
                   new_df3["Port"][1,1],
                   new_df3["Captain"][1,1],
                   new_df3["LFA"][1,1],
                   new_df3["Affiliation"][1,1],
                   new_df3["Sampler"][1,1],
                   new_df3["Date"][1,1],
                   new_df3["Lat Degrees"][1,1] * 100 + new_df3["Lat Minutes"][1,1], #conpos function does the same thing but it's quicker here
                   new_df3["Lon Degrees"][1,1] * 100 + new_df3["Lon Minutes"][1,1],
                   #new_df3["Comments"][1,1], #no comments in new upload template
                   new_df3["Day"][1,1],
                   new_df3["Month"][1,1],
                   new_df3["Year"][1,1],
                   new_df3["latdd.dd"][1,1],
                   new_df3["londd.dd"][1,1],
                   new_df3["Sampler 2"][1,1],
                   new_df3["Tag Prefix"][1,1]
    )
    #the first print is for debug, but sample_ent loads all data into oracle
    print(paste(x,max(new_excel_df$id),sep = " of "))
    out = paste(out, sample_ent(bdata = bdata_df, sdata = sdata_list, from_file = TRUE), sep = " ")
  }
  return(out)
}

#' @title tag_color_filler
#' @description matches affiliation to tag colour
#' @import dplyr readxl
#' @return tag colour 
#' @export
tag_color_filler <- function(x){ 
  if(is.na(x) | is.null(x)) return('NA')
  if(x == "DFO" | x == "CBFH") y <- "Blue"
  if(x == "SWLSS") y <- "Red"
  return(y)
}

#' @title tag_prefix_filler
#' @description matches affiliation to tag prefix
#' @import dplyr readxl
#' @return tag prefix
#' @export
tag_prefix_filler <- function(x){ 
  if(is.na(x) | is.null(x)) return('NA')
  if(x == "DFO" | x == "CBFH") y <- "XY"
  if(x == "SWLSS") y <- "SWLSS"
  return(y)
}

#' @title  sample_ent
#' @description  Function that enters release data entered in the html app 
#' @import jsonlite stringr opencpu ROracle DBI
#' @return message to webpage 
#' @export
sample_ent <- function(bdata, sdata, from_file = FALSE){
  tryCatch({
    drv <- DBI::dbDriver("Oracle")
    con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  }, warning = function(w) {
  }, error = function(e) {
    return(toJSON("Connection failed"))
  }, finally = {
  })
  
  #the data behaves differently when saved to file, this script was run and debugged with an intermediate file
  #may need additional testing if temp file to be removed.
  if(from_file == FALSE){
    #tempfile path (this will different every time script is run)
    jsonFilePath = tempfile(pattern = "bdata", tmpdir = tempdir(), fileext = ".json")
    write(bdata, jsonFilePath)
  }
  
  if(from_file == FALSE){
    sdata_file = tempfile(pattern = "sdata", tmpdir = tempdir(), fileext = ".txt")
    write.table(sdata, file = sdata_file, sep = "")
    file_str <- paste(readLines(sdata_file), collapse="\n")
    
    #sdata file has been read so we can delete the file
    unlink(sdata_file)
    
    start_string = unlist(gregexpr('date', file_str))
    end_string = unlist(gregexpr('&lon=.{12}W', file_str)) + 19 # the '+19' is static because that's the length of the delimeter to the end of the string
    
    sdata = substring(file_str, start_string, end_string - 1)
    
    samp = myUrlEncode(sdata)
    samp = unlist(str_split(samp, "&"))
  
    dat = ""
    sam = ""
    sam2 = ""
    ves = ""
    LFA = ""
    capt = ""
    dep = ""
    com = ""
    lat = ""
    lon = ""
    affl = "DFO"    #affl is not in the webpage yet so added here for now
    
    for(i in 1:length(samp)){
      if(samp[i] != ""){
        
        sa = unlist(str_split(samp[i], "="))
        
        if(sa[1] == "radio-choice-1")
          rc1 = sa[2]
        if(sa[1] == "date")
          dat = sa[2]
        if(sa[1] == "samp")
          sam = sa[2]
        if(sa[1] == "samp2")
          sam2 = sa[2]
        if(sa[1] == "ves")
          ves = sa[2]
        if(sa[1] == "lfa")
          LFA = sa[2]
        if(sa[1] == "capt")
          capt = sa[2]
        if(sa[1] == "dep")
          dep = sa[2]
        if(sa[1] == "com")
          com = sa[2]
        if(sa[1] == "lat")
          lat = sa[2]
        if(sa[1] == "lon")
          lon = sa[2]
      }
    }
    
    lat = str_replace(lat, "N","")
    lon = str_replace(lon, "W","")
    
    rlat = as.character(conpos(lat))
    rlon = as.character((conpos(lon)*-1))
    
    df = unlist(str_split(dat, "/"))
    
    year = df[3]
    mon = df[1]
    day = df[2]
    
    dat = paste(day, mon, year, sep = "/")
    
  } else{
    dat = sdata[7]     #date, leading indicator of good data (written over later)
    sam = sdata[6]     #technician
    sam2 = sdata[15]    #2nd technician
    ves = sdata[1]     #vessel
    LFA = sdata[4]     #LFA
    capt = sdata[3]    #captain
    dep = ""
    #com = sdata[10]    #comments have been removed in new upload template
    lat = sdata[8]     #lat
    lon = sdata[9]     #lon
    affl = sdata[5]    #affiliation
    #day = sdata[11]    #day
    day = sdata[10]    #new template day
    #mon = sdata[12]    #month
    mon = sdata[11]    #new template month
    #year = sdata[13]   #year
    year = sdata[12]   #new template year
    #rlat = sdata[14]
    rlat = sdata[13]
    #rlon = as.numeric(sdata[15]) * -1
    rlon = as.numeric(sdata[14]) * -1
    tag_prefix = sdata[16]
    #tag prefix will be important when there are multiple tagging programs
    #in this database. All oracle tables with a tag (TAGID) should also
    #have tag prefix to help differentiate programs down the line
    
    #dat called twice, keeping sdata dat as a leading indicator in error checking
    dat = paste(day, mon, year, sep = "/")
  }
  
  
  #database variables
  sta = ""
  res = ""
  samp = ""
  sampsql = ""
  out = ""
  wrisamp = FALSE
  writrip = FALSE

  #database names
  database_name = paste("LOBSTER",".","LBT_TRIP", sep="")
  sampdb = paste("LOBSTER",".","LBT_SAMPLE", sep="")
  tripdb = paste("LOBSTER",".","LBT_TRIP", sep="")
  

  #does the trip already exist?
  sql = paste("SELECT TRIP_ID from ", tripdb, " where RELEASE_DATE = to_date('", dat,"', 'dd/mm/yyyy') AND CAPTAIN = '",capt,"' AND TECHNICIAN = '",sam,"'", sep = "")

  result <- ROracle::dbSendQuery(con, sql) 
  result <- ROracle::fetch(result)
  exis = nrow(result)
  
  rowid = ""
  
  if (exis > 0){
    res = result[,1]
  }
  
  if (exis == 0) {            
    sql = paste('SELECT TRIP_ID FROM ', tripdb, sep = "")
    result <- ROracle::dbSendQuery(con, sql) 
    result <- ROracle::fetch(result)
    res = nrow(result) + 300 
    
    #LFAs to fill both stat and sub areas for now.
    sta = LFA
    suba = LFA
    
    reldat = lubridate::dmy(dat)
    
    #newly added second technician for release data
    tripsql = paste("INSERT INTO ",tripdb," (TRIP_ID, TECHNICIAN, TECHNICIAN_B, AFFILIATION, VESSEL, LFA, YEAR, STATSAREA, REPORTED, CAPTAIN, SUBAREA, RELEASE_DATE) VALUES( '",res,"' , '",sam,"' , '", sam2,"' , '",affl,"' , '",SQLsafty(ves),"' , '",LFA,"' , '",year,"' , '",sta ,"' , 0 , '",SQLsafty(capt) ,"' , '",suba,"' , to_date('", dat,"', 'dd/mm/yyyy'))", sep = "")
    
    writrip = T
  }

#does this sample section exist yet?
#check trip id with LAT/LON
sql = paste("SELECT SAMPLE_ID FROM ",sampdb, " WHERE TRIP = '",res,"' AND LAT_DD_DDDD = '",rlat,"' AND LONG_DD_DDDD = '",rlon,"'", sep = "")

result <- ROracle::dbSendQuery(con, sql)
result <- ROracle::fetch(result)
res2 = nrow(result) 

if (res2 > 0){
  samp = result[,1]
}

if (res2 == 0) {   
  sql = paste("SELECT SAMPLE_ID FROM ", sampdb, sep = "")
  result <- ROracle::dbSendQuery(con, sql) 
  result <- ROracle::fetch(result)
  samp = as.character(nrow(result) + 3500) 
  
  #no comments in the new template, defaulting to say NA
  #it's the last field in sampdb, so if it ever changes just change 'NA' to the variable
  #this is where the dep (depth) variable in fathoms goes, this is currently stored as a string with zero length
  #sampsql = paste("INSERT INTO ", sampdb, " VALUES( '",samp,"' , '",res,"' , '",lat,"' , '",lon,"'  ,  '",rlat,"' , '",rlon,"' , '",dep,"' , '",SQLsafty(com),"')", sep = "")
  sampsql = paste("INSERT INTO ", sampdb, " VALUES( '",samp,"' , '",res,"' , '",lat,"' , '",lon,"'  ,  '",rlat,"' , '",rlon,"' , '",dep,"' , '",'NA',"')", sep = "")
  
  wrisamp = TRUE
}

if(from_file == TRUE){
  dd = bdata
} else {
  #create a dataframe, the headers are included and the data is passed as a json
  dd = as.data.frame(jsonlite::fromJSON(jsonFilePath)[2:nrow(jsonlite::fromJSON(jsonFilePath)),])
  names(dd) = jsonlite::fromJSON(jsonFilePath)[1,]
  
  #delete tempfile
  unlink(jsonFilePath)
}

#this is legacy code but still necessary in case anyone uses the webui to input a few tags at a time instead of batch uploading
#duplicate tags from bath upload will be flagged in upload_from_file3
#this for loop will check if any of the Tag Numbers that you are inputting have already been entered. If they have it will reject all the entries and report a list of repeat tags.
writedata = TRUE
#i = 1
for(i in 1:nrow(dd)){
  if(i > 0){
    if(!is.na(dd$`Tag Num`[i])){
      biodb = paste("LOBSTER",".","LBT_BIO", sep="")
      sql = paste("SELECT TAG_ID FROM ", biodb, " where TAG_ID = '", dd$`Tag Num`[i],"'", sep = "")
      
      result <- ROracle::dbSendQuery(con, sql) 
      result <- ROracle::fetch(result)
      ntn = nrow(result) 
      if(ntn > 0) {
        out = paste(out, "\nLobster with tag " , dd$`Tag Num`[i], " has already been added!! ", sep = "")
        writedata = FALSE;
      } 
    }
  }
}		

#if you get into this if statement in means the tag numbers are novel.
if(writedata){
  #i = 1
  for(i in 1:nrow(dd)){
    if(i > 0){
      if(!is.na(dd$`Tag Num`[i])){
        if(is.null(dd$`Claw`[i])) dd$`Claw`[i] = NA
        if(is.na(dd$`Tag Color`[i])) dd$`Tag Color`[i] = 'Blue'  #should default = blue be on front end? (use isna to default to blue)
        #okay, this one is a bit tricky. Tag Num = Tag Num, Carapace = Carapace, Claw = Claw, Shell Cond
        
        #if tag colour is blue, make the tag xy...
        #if(dd$`Tag Color`[i] = 'Blue'{
        #tag_prefix = 'XY'
        #}
        
        # *********** this inserts 'XY' into the BIO table regardless of affiliation
        #sql = paste("INSERT INTO ", biodb, " vALUES ('",samp,"', '",dd$`Tag Num`[i],"', '",dd$`Carapace`[i],"', '",dd$Shell[i],"','",dd$Claw[i],"','",dd$`Tag Color`[i],"','",dd$Sex[i],"','",dd$`V-Notch`[i],"','",'XY',"')", sep = "")

        #updated to include tag prefix
        sql = paste("INSERT INTO ", biodb, " VALUES ('",samp,"', '",dd$`Tag Num`[i],"', '",dd$`Carapace`[i],"', '",dd$Shell[i],"','",dd$Claw[i],"','",dd$`Tag Color`[i],"','",dd$Sex[i],"','",dd$`V-Notch`[i],"','",dd$`Tag Prefix`[i],"')", sep = "")
        
        result <- ROracle::dbSendQuery(con, sql) 

        if(dbGetInfo(result, what = "rowsAffected") > 0){
          out =  paste(out, "\nLobster with  tag " , dd$`Tag Num`[i], " successfully added", sep = "")
        }
        else{
          out =  paste(out,"\nError: ",  result)
          return(out)
          die()
        }
        ROracle::dbClearResult(result)
      }
    }
  }
  
  if(wrisamp){
    
    #sampsql is an action query, we are looking to update lbt_samp
    rs = ROracle::dbSendQuery(con, sampsql) 
    
    #update successful
    if(dbGetInfo(rs, what = "rowsAffected") == 1){
      out = paste(out,"\nSample from trip ",res, " with pos ",lat, " " ,lon, " successfully added", sep = "")
    }
    else{
      #update unsuccessful, the rs now holds the error
      out =  paste(out, "\nError:  " ,sampsql , "\n" , rs, "\n", sep = "")
      return(out)
      die()
    }
  }
  if(writrip){
    #send update to lbt_trip
    result2 <- ROracle::dbSendQuery(con, tripsql) 
    
    if(dbGetInfo(result2, what = "rowsAffected") == 1){
      out =  paste(out, "\nNew Trip ", res, " Successfully Added.")
    }
    else{
      out =  paste(out,"\nError: ",  result2)
      return(out)
      die()
    }
  }
  ROracle::dbCommit(con)
}

out = paste(out,"\n\n", sep = "")

ROracle::dbDisconnect(con)
return(out)
}

#' @title  ret_ent
#' @description  Function that enters return data entered in the html app 
#' @import ROracle DBI jsonlite stringr sp rgeos
#' @return message to webpage
#' @export
ret_ent <- function(ddata){
  tryCatch({
    drv <- DBI::dbDriver("Oracle")
    conn <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  }, warning = function(w) {
  }, error = function(e) {
    return(toJSON("Connection failed"))
  }, finally = {
  })
  
  out = ""
  
  ent = myUrlEncode(ddata)
  ent = unlist(str_split(ent, "&"))
  
  #radio-choice-2 
  reg = ""
  rc2 = ""
  per = ""
  tid = ""
  lat = ""
  lon = ""
  date = ""
  depth = ""
  ves = ""
  
  add = ""
  str = ""
  rou = ""
  loc = ""
  pro = ""
  cou = ""
  poc = ""
  ema = ""
  phoa = ""
  phob = ""
  com = ""
  carapace_l = ""
  #shellcond = ""
  sex = ""
  egg = ""
  tag_prefix = ""
  
  for(i in 1:length(ent)){
    if(ent[i] != ""){
      
      sa = unlist(str_split(ent[i], "="))
      
      if(sa[1] == "ssorg")
        reg = sa[2]
      if(sa[1] == "radio-choice-2")
        rc2 = sa[2]
      if(sa[1] == "per")
        per = sa[2]
      if(sa[1] == "tid")
        tid = sa[2]
      if(sa[1] == "lat")
        lat = sa[2]
      if(sa[1] == "lon")
        lon = sa[2]
      if(sa[1] == "date")
        date = sa[2]
      if(sa[1] == "depth")
        depth = sa[2]
      if(sa[1] == "ves")
        ves = sa[2]
    
      if(sa[1] == "add")
        add = sa[2]
      if(sa[1] == "str")
        str = sa[2]
      if(sa[1] == "rou")
        rou = sa[2]
      if(sa[1] == "loc")
        loc = sa[2]
      if(sa[1] == "pro")
        pro = sa[2]
      if(sa[1] == "cou")
        cou = sa[2]
      if(sa[1] == "poc")
        poc = sa[2]
      if(sa[1] == "ema")
        ema = sa[2]
      if(sa[1] == "phoa")
        phoa = sa[2]
      if(sa[1] == "phob")
        phob = sa[2]
      if(sa[1] == "lfa")
        lfa = sa[2]
      if(sa[1] == "afi")
        afi = sa[2]
      if(sa[1] == "comments")
        com = sa[2]
      if(sa[1] == "carapace_l")
        carapace_l = sa[2]
      if(sa[1] == "sex")
        sex = sa[2]
      if(sa[1] == "egg")
        egg = sa[2]
      if(sa[1] == "tagprefix")
        tag_prefix = sa[2]
    }
  }
  
  lat = str_replace(lat, "N","")
  lon = str_replace(lon, "W","")
  
  rlat = as.character(conpos(lat))
  rlon = as.character((conpos(lon)*-1))
  
  # redo this function to accept rlon and rlat and return an area that way
  # should just return and LFA, keeping crab code below as example.
  
  # if(absolutely.in.area("cfa23", rlon, rlat)){ 
  #   statsarea = "sens"
  #   subarea = "(cfa23)(all)(ens)(sens)(allandgulf)(cfa23zoom)(all.holes)"
  # }
  # if(absolutely.in.area("cfa24", rlon, rlat)){ 
  #   statsarea = "sens"
  #   subarea = "(cfa24)(all)(ens)(sens)(allandgulf)(cfa24zoom)(all.holes)"
  # }
  # if(absolutely.in.area("gulf", rlon, rlat)){ 
  #   statsarea = "gulf"
  #   subarea = "(gulf)(nens_gulf)(allandgulf)"
  # }
  # if(absolutely.in.area("nens", rlon, rlat)){ 
  #   statsarea = "nens"
  #   subarea = "(all)(ens)(nens)(nens_gulf)(allandgulf)"
  # }
  # if(absolutely.in.area("cfa4x", rlon, rlat)){ 
  #   statsarea = "cfa4x"
  #   subarea = "(all)(ens)(cfa4x)(allandgulf)"
  # }
  
  #keep these variables blank or null till absolutely.in.area gets reworked.
  statsarea =""
  subarea = ""
  
  df = unlist(str_split(date, "/"))

  year = df[3]
  mon = df[1]
  day = df[2]
  dat = paste(day, mon, year, sep = "/")

  #ret = ""
  if(rc2 == "choice-1")ret = 1
  if(rc2 == "choice-2")ret = 2
  if(rc2 == "choice-3")ret = 3
  #radio button for tag return status. Was it returned with tag vs not returned at all vs other?
  #remember these will be entered in to database as RELCODE
  #if(rc2 == "choice-4")ret = 4
  #if(rc2 == "choice-5")ret = 5

  #if a berried female isn't returned to water exit program and show mistake.
  if(ret == 2 || ret == 3){
    if(sex == 3){
      out = paste("ERROR: berried lobster not returned to water, data not recorded.", "\n", sep = "")
      return(out)
    }
  }
  
  #egg data can be derived from sex input
  #two names for same variable
  if(sex == 1){
    egg = "NA"
  }
  if(sex == 2){
    egg = "no"
  }
  if(sex == 3){
    egg = "yes"
  }
  
  add = unlist(str_split(add, ","))[1]
  
  #route if return address doesn't have a town.
  if(add == ""){
    if(rou != "")
      add = paste(str, rou, sep = ", ")
    else add = str
  }
  
  #leftovers
  gstring = ""
  
  captdb = paste("LOBSTER",".","LBT_CAPTURE", sep = "")
  peopdb = paste("LOBSTER",".","LBT_PEOPLE", sep = "")
  
  #keep two tables up to date here
  #the variables coming from the web ui are the ids, it plays nice with the data flow.
  #
  toda1 = paste("INSERT INTO ",captdb, " ", gstring, " (TAG_PREFIX, TAG, CAPTURE_DATE, PERSON, PERSON_B, LAT_DDMM_MM, LONG_DDMM_MM, LAT_DD_DDDD, LONG_DD_DDDD, FATHOMS, 
                RELCODE, COMMENTS, CAPTAIN, VESSEL, YEAR, STATSAREA, CARAPACE_L, REWARDED, SUBAREA, SEX, EGG) 
                values ('",tag_prefix, "','", tid,"', to_date('", dat,"', 'dd/mm/yyyy'),'", SQLsafty(per),"','NA','", lat,"','", lon,"','", rlat,"','", rlon,"','", depth,
                "','", ret,"','", SQLsafty(com),"','", SQLsafty(per),"','", SQLsafty(ves),"','", year,"','", statsarea,"','", carapace_l,
                "','N','", subarea,"','", sex,"','", SQLsafty(egg), "')", sep = "")
  
  que1 = paste("SELECT count(NAME) num from ", peopdb," ", gstring, "  where NAME = '", per,"'", sep = "")
  toda2 = paste("INSERT into ", peopdb, " ", gstring, "  (NAME, CIVIC, TOWN, PROV, POST, EMAIL, PHO1, PHO2, COUNTRY, AFFILIATION, LFA) values ('", SQLsafty(per),"','", SQLsafty(add),"','", SQLsafty(loc),"','", SQLsafty(pro),"','", poc,"','", SQLsafty(ema),"','", SQLsafty(phoa),"','", SQLsafty(phob), "','", SQLsafty(cou), "','",afi, "','",lfa, "')", sep = "")
  toda4 = paste("update ", peopdb, " ", gstring, "  set NAME = '", SQLsafty(per),"', CIVIC = '", SQLsafty(add),"', TOWN = '", SQLsafty(loc),"', PROV = '", SQLsafty(pro),"', POST = '", poc,"', EMAIL = '", SQLsafty(ema),"', PHO1 = '", SQLsafty(phoa),
                "', PHO2 = '", SQLsafty(phob),"', COUNTRY = '", SQLsafty(cou),"', AFFILIATION = '", SQLsafty(afi),"', LFA = '", SQLsafty(lfa),"' where NAME = '", per, "'", sep = "")  
  
  result <- ROracle::dbSendQuery(conn, que1)
  result <- ROracle::fetch(result)
  
  toda = toda2
  out = paste("New person added to ", peopdb, " ", gstring, "  table: ", per, sep = "")
  if(result == 1){
    toda = toda4
    out = paste("Updated data in ", peopdb, " ", gstring, "  table for person: ", per, sep = "")
  }
  
  out = paste(out, "\n", sep = "")
  
  rs = ROracle::dbSendQuery(conn, toda) 
  
  if(dbGetInfo(rs, what = "rowsAffected") == 1){
    
  }
  else{
    out =  paste(out, "\nError: " ,toda , "\n" , rs, "\n", sep = "")
    return(out)
    die()
  }
  
  result <- ROracle::dbSendQuery(conn, toda1) 
  
  if(dbGetInfo(result, what = "rowsAffected") == 1){
    out = paste(out, "New entry added to ", captdb, "  with TAG_ID: ", tid, sep = "")
  }
  else{
    out =  paste(out, "\nError: " ,toda1 , "\n" , result, "\n", sep = "")
    return(out)
    die()
  }
  
  out = paste(out, "\n", sep = "")
  out = paste(out, "\n", sep = "")
  
  ROracle::dbCommit(conn)
  
  ### save a backup of updated LBT_CAPTURE on shared drive
  rec <- ROracle::dbSendQuery(conn, paste("select * from ", "LOBSTER.LBT_CAPTURE", sep=""))
  rec <- ROracle::fetch(rec)
  write.csv(rec, file = "R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/Master_data/LBT_CAPTURE.csv", row.names = F)
  
  ROracle::dbDisconnect(conn)
  return(out)
  
}

#' @title THE MAIN GUI FUNCTION!
#' @description Opens web page of options for data entry
#' @import opencpu
#' @export
enter_data_app <- function(){
  ocpu_start_app("LobTag", no_cache = TRUE)
}

#' @title  SQLsafty
#' @description  Replace ' with '' to preserve entries
#' @param var The variables to modify
#' @export
SQLsafty <- function(var) {
  return(gsub("'", "''", var))
}

#' @title  myUrlEncode
#' @description  Decode json url
#' @param string The url to decode
#' @export
myUrlEncode <- function(string) {
  entities = c("%21", "%2A", "%27", "%28", "%29", "%3B", "%3A", "%40", "%26", "%3D", "%2B", "%24", "%2C", "%2F", "%3F", "%25", "%23", "%5B", "%5D", "%C2%B0", "\\+", "%0D%0A")
  replacements = c("!", "*", "'", "(", ")", ";", ":", "@", "%26", "%3D", "+", "$", ",", "/", "?", "%", "#", "[", "]", "", " ", "  ")
  
  for(i in 1:length(entities)){
    #print(entities[i])
    string = gsub(entities[i], replacements[i], string)
  }
  return(string)
}

#' @title  conpos
#' @description  Decode positional data
#' @param string The pos to decode
#' @return the decoded position
#' @export
conpos = function(string) {
  
  if(substr(string, 4, 4) == '.'){
    string = paste(substr(string, 1, 2),'0',substr(string, 3, nchar(string)), sep = "")
  }
  de = as.integer(substr(string, 1, 2)) 
  min = as.integer(substr(string, 3, 4))
  dmin = as.integer(substr(string, 6, 7))/100
  dm = (min + dmin)/60
  nu = de + dm
  return(nu)
}

#' @title  auto_availableP
#' @description Function that help autopopulate people in the html form
#' @import ROracle DBI jsonlite
#' @export
autoavailableP = function(options = "", region = ""){
  tryCatch({
    drv <- DBI::dbDriver("Oracle")
    con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  }, warning = function(w) {
  }, error = function(e) {
    return(toJSON("Connection failed"))
  }, finally = {
  })
  
  captdb = paste("LOBSTER",".","LBT_CAPTURE", sep = "")
  peopdb = paste("LOBSTER",".","LBT_PEOPLE", sep = "")
  
  res = ""
  region = "ss"
  if(region == "ss"){
    my_qry <- paste("SELECT NAME FROM ", peopdb, sep = "")
    result <- ROracle::dbSendQuery(con, my_qry) 
  }
  
  result <- ROracle::fetch(result)
  ROracle::dbDisconnect(con)
  
  return(toJSON(result))
}

#' @title  auto_availableT
#' @description Function that help autopopulate Tag id in the html form
#' @import ROracle jsonlite
#' @export
autoavailableT = function(region = ""){
  tryCatch({
    drv <- DBI::dbDriver("Oracle")
    con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  }, warning = function(w) {
  }, error = function(e) {
    return(toJSON("Connection failed"))
  }, finally = {
  })
  
  # lobster database shortcuts
  # captdb = paste("LOBSTER",".","LBT_CAPTURE", sep = "")
  # peopdb = paste("LOBSTER",".","LBT_PEOPLE", sep = "")
  lbiodb = paste("LOBSTER",".","LBT_BIO", sep = "")
  
  result = ""
  my_qry <- paste("SELECT TAG_ID from ", lbiodb, sep = "")
  result <- ROracle::dbSendQuery(con, my_qry)
  
  result <- ROracle::fetch(result)
  ROracle::dbDisconnect(con)

  return(toJSON(result))
}

#' @title  auto_availableCT
#' @description same as auto_availableT, except it looks only at captured tags
#' @import ROracle jsonlite
#' @export
autoavailableCT = function(region = ""){
  tryCatch({
    drv <- DBI::dbDriver("Oracle")
    con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  }, warning = function(w) {
  }, error = function(e) {
    return(toJSON("Connection failed"))
  }, finally = {
  })
  
  # lobster database shortcuts
  captdb = paste("LOBSTER",".","LBT_CAPTURE", sep = "")
  # peopdb = paste("LOBSTER",".","LBT_PEOPLE", sep = "")
  #lbiodb = paste("LOBSTER",".","LBT_BIO", sep = "")
  
  result = ""
  #my_qry <- paste("SELECT TAG_ID from ", lbiodb, sep = "")
  my_qry <- paste("SELECT TAG from ", captdb, sep = "")
  result <- ROracle::dbSendQuery(con, my_qry)
  
  result <- ROracle::fetch(result)
  ROracle::dbDisconnect(con)
  
  return(toJSON(result))
}

#' @title  autoaddData
#' @description Function that help autopopulate people in the html form
#' @param name The persons name from which to obtain current info
#' @import ROracle DBI jsonlite
#' @export
autoaddData = function(name = "", region = ""){
  tryCatch({
    drv <- DBI::dbDriver("Oracle")
    con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  }, warning = function(w) {
  }, error = function(e) {
    return(toJSON("Connection failed"))
  }, finally = {
  })
  
  peopdb = paste("LOBSTER",".","LBT_PEOPLE", sep = "")
  
  #gstring = ""
  
  #result <- ROracle::dbSendQuery(con, paste("select * from ", peopdb, " ", gstring,"  where NAME = '", name, "'", sep = "" )) 
  result <- ROracle::dbSendQuery(con, paste("SELECT * FROM ", peopdb, " ","  where NAME = '", name, "'", sep = "" )) 
  
  result <- ROracle::fetch(result)
  ROracle::dbDisconnect(con)
  
  return(toJSON(result))
  
}

#' @title  find_capture_tag
#' @description find captured tag data to delete
#' @import stringr jsonlite
find_capture_tag = function(deldata = ""){
  tryCatch({
    drv <- DBI::dbDriver("Oracle")
    con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  }, warning = function(w) {
  }, error = function(e) {
    return(toJSON("Connection failed"))
  }, finally = {
  })
  
  #deldata = "ssorg=ss&tid=901&date=05%2F14%2F2022"
  
  del = myUrlEncode(deldata)
  del = unlist(str_split(del, "&"))
  
  tagid = ""
  date = ""
  
  for(i in 1:length(del)){
    if(del[i] != ""){
      
      sa = unlist(str_split(del[i], "="))
      
      if(sa[1] == "ssorg")
        reg = sa[2]
      if(sa[1] == "tid")
        tagid = sa[2]
      if(sa[1] == "date")
        date = sa[2]
    }
  }
  
  #convert day to correct format
  df = unlist(str_split(date, "/"))
  
  year = df[3]
  mon = df[1]
  day = df[2]
  
  dat = paste(day, mon, year, sep = "/")
  
  captdb = paste("LOBSTER",".","LBT_CAPTURE", sep = "")
  
  #find data in capture table and show in message console
  sql = paste("SELECT * FROM ", captdb, " WHERE TAG = '", tagid, "' AND CAPTURE_DATE = to_date('", dat,"', 'dd/mm/yyyy')", sep = "")
  #sql = paste("SELECT * FROM ", captdb, " WHERE TAG = '", tagid, "'", sep = "")
  
  result <- ROracle::dbSendQuery(con, sql)
  result <- ROracle::fetch(result)
  ROracle::dbDisconnect(con)
  
  out = ""
  
  if(nrow(result) > 1){
    out <- paste("warning, same tag caught multiple times that day")
  }
  
  da <- result
  
  #my_vector <- names(da)
  #out <- paste(da[1,1], da[1,2], da[1,3], da[1,4], da[1,5], da[1,6], sep = " ")
  out <- paste(out, jsonlite::toJSON(da), sep = "\n")
  #out <- da

  #out = paste(tagid, date, sep = " ")
  return(out)
}

#' @title  delete_capture_tag2
#' @description find captured tag data to delete
#' @import stringr jsonlite
delete_capture_tag2 = function(deldata = ""){
  tryCatch({
    drv <- DBI::dbDriver("Oracle")
    con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  }, warning = function(w) {
  }, error = function(e) {
    return(toJSON("Connection failed"))
  }, finally = {
  })
  
  del = myUrlEncode(deldata)
  del = unlist(str_split(del, "&"))
  
  tagid = ""
  date = ""
  
  for(i in 1:length(del)){
    if(del[i] != ""){
      
      sa = unlist(str_split(del[i], "="))
      
      if(sa[1] == "ssorg")
        reg = sa[2]
      if(sa[1] == "tid")
        tagid = sa[2]
      if(sa[1] == "date")
        date = sa[2]
    }
  }
  
  #convert day to correct format
  df = unlist(str_split(date, "/"))
  
  year = df[3]
  mon = df[1]
  day = df[2]
  
  dat = paste(day, mon, year, sep = "/")
  
  captdb = paste("LOBSTER", ".", "LBT_CAPTURE", sep = "")
  pathdb = paste("LOBSTER", ".", "LBT_PATH", sep = "")
  pathsdb = paste("LOBSTER", ".", "LBT_PATHS", sep = "")
  
  #user will have okay'd the query, so we'll run it here and delete
  #delete from capture table
  capture_delete_query = paste("DELETE FROM ", captdb, " WHERE TAG = '", tagid, "' AND CAPTURE_DATE = to_date('", dat,"', 'dd/mm/yyyy')", sep = "")
  
  #delete all instances of tag in PATH table
  path_delete_query = paste("DELETE FROM ", pathdb, " WHERE TID = '", tagid, "'", sep = "")
  
  #delete all instances in PATHS table
  paths_delete_query = paste("DELETE FROM ", pathsdb, " WHERE TID = '", tagid, "'", sep = "")
  
  #send all queries
  send_capture_query = ROracle::dbSendQuery(con, capture_delete_query)
  send_path_query = ROracle::dbSendQuery(con, path_delete_query)
  send_paths_query = ROracle::dbSendQuery(con, paths_delete_query)
  
  ROracle::dbCommit(con)
  ROracle::dbDisconnect(con)

  out <- paste("\nCAPTURE data for tag: ", tagid, " on ", dat, " has been deleted.\nAll PATH and PATHS data for tag: ", tagid, " has been deleted.", sep = "")

  return(out)
}

#' @title  find_release_tag
#' @description find captured tag data to delete
#' @import stringr jsonlite
find_release_tag = function(deldata = ""){
  tryCatch({
    drv <- DBI::dbDriver("Oracle")
    con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  }, warning = function(w) {
  }, error = function(e) {
    return(toJSON("Connection failed"))
  }, finally = {
  })
  
  #deldata = "ssorg=ss&tid=300&date=05%2F14%2F2022"
  
  #each tag only has one release, so we can search everything but tagid only
  
  del = myUrlEncode(deldata)
  del = unlist(str_split(del, "&"))
  
  tagid = ""
  date = ""
  
  for(i in 1:length(del)){
    if(del[i] != ""){
      
      sa = unlist(str_split(del[i], "="))
      
      if(sa[1] == "ssorg")
        reg = sa[2]
      if(sa[1] == "tid")
        tagid = sa[2]
      if(sa[1] == "date")
        date = sa[2]
    }
  }
  
  #convert day to correct format
  df = unlist(str_split(date, "/"))
  
  year = df[3]
  mon = df[1]
  day = df[2]
  
  dat = paste(day, mon, year, sep = "/")
  
  captdb = paste("LOBSTER",".","LBT_CAPTURE", sep = "")
  biodb = paste("LOBSTER",".","LBT_BIO", sep = "")
  sampdb = paste("LOBSTER",".","LBT_SAMPLE", sep = "")
  tripdb = paste("LOBSTER",".","LBT_TRIP", sep = "")
  
  #find data in bio table and show in message console
  tag_sql = paste("SELECT * FROM ", biodb, " WHERE TAG_ID = '", tagid, "'", sep = "")
  
  result <- ROracle::dbSendQuery(con, tag_sql)
  result <- ROracle::fetch(result)
  
  samp = result[['SAMPLE_NUM']]
  samp_sql = paste("SELECT * FROM ", sampdb, " WHERE SAMPLE_ID = '", samp, "'", sep = "")
  
  samp_result <- ROracle::dbSendQuery(con, samp_sql)
  samp_result <- ROracle::fetch(samp_result)
  
  #get all tags that are in the same sample as the problem tag.
  all_tags_in_sample_sql = paste("SELECT TAG_ID FROM ", biodb, " WHERE SAMPLE_NUM = '", samp, "'", sep = "")
  
  all_tags_result <- ROracle::dbSendQuery(con, all_tags_in_sample_sql)
  all_tags_result <- ROracle::fetch(all_tags_result)
  
  #trip = samp_result[['TRIP']]
  #trip_sql = paste("SELECT * FROM ", tripdb, " WHERE SAMPLE_ID = '", samp, "'", sep = "")
  
  #samp_result <- ROracle::dbSendQuery(con, samp_sql)
  #samp_result <- ROracle::fetch(samp_result)
  
  ROracle::dbDisconnect(con)
  
  out = ""
  
  da <- result
  out <- paste(out, jsonlite::toJSON(da), sep = "\n")
  
  out = paste(out, "\n\nThese are all the tags in the same sample group:", sep = "")
  out = paste(out, all_tags_result, sep = "")

  return(out)
}

#' @title  delete_one_release_tag
#' @description only deletes the one tag
#' @import stringr jsonlite
delete_one_release_tag = function(deldata = ""){
  tryCatch({
    drv <- DBI::dbDriver("Oracle")
    con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  }, warning = function(w) {
  }, error = function(e) {
    return(toJSON("Connection failed"))
  }, finally = {
  })
  
  del = myUrlEncode(deldata)
  del = unlist(str_split(del, "&"))
  
  tagid = ""
  date = ""
  
  for(i in 1:length(del)){
    if(del[i] != ""){
      
      sa = unlist(str_split(del[i], "="))
      
      if(sa[1] == "ssorg")
        reg = sa[2]
      if(sa[1] == "tid")
        tagid = sa[2]
      if(sa[1] == "date")
        date = sa[2]
    }
  }
  
  #convert day to correct format
  df = unlist(str_split(date, "/"))
  
  year = df[3]
  mon = df[1]
  day = df[2]
  
  dat = paste(day, mon, year, sep = "/")
  
  captdb = paste("LOBSTER", ".", "LBT_CAPTURE", sep = "")
  pathdb = paste("LOBSTER", ".", "LBT_PATH", sep = "")
  pathsdb = paste("LOBSTER", ".", "LBT_PATHS", sep = "")
  biodb = paste("LOBSTER",".","LBT_BIO", sep="")
  
  release_delete_query = paste("DELETE FROM ", biodb, " WHERE TAG_ID = '", tagid, "'", sep = "")

  #send query
  send_capture_query = ROracle::dbSendQuery(con, release_delete_query)
  
  ROracle::dbCommit(con)
  ROracle::dbDisconnect(con)
  
  out <- paste("\nRelease data for tag: ", tagid, " has been deleted from BIO Table.", sep = "")
  
  return(out)
}

#' @title  delete_all_release_tag
#' @description delete entire sample of tags
#' @import stringr jsonlite
delete_all_release_tag = function(deldata = ""){
  tryCatch({
    drv <- DBI::dbDriver("Oracle")
    con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  }, warning = function(w) {
  }, error = function(e) {
    return(toJSON("Connection failed"))
  }, finally = {
  })
  
  del = myUrlEncode(deldata)
  del = unlist(str_split(del, "&"))
  
  tagid = ""
  date = ""
  
  for(i in 1:length(del)){
    if(del[i] != ""){
      
      sa = unlist(str_split(del[i], "="))
      
      if(sa[1] == "ssorg")
        reg = sa[2]
      if(sa[1] == "tid")
        tagid = sa[2]
      if(sa[1] == "date")
        date = sa[2]
    }
  }
  
  #convert day to correct format
  df = unlist(str_split(date, "/"))
  
  year = df[3]
  mon = df[1]
  day = df[2]
  
  dat = paste(day, mon, year, sep = "/")
  
  captdb = paste("LOBSTER", ".", "LBT_CAPTURE", sep = "")
  pathdb = paste("LOBSTER", ".", "LBT_PATH", sep = "")
  pathsdb = paste("LOBSTER", ".", "LBT_PATHS", sep = "")
  biodb = paste("LOBSTER",".","LBT_BIO", sep="")
  
  #find problem tag number and delete all tags from same sample
  tag_sql = paste("SELECT * FROM ", biodb, " WHERE TAG_ID = '", tagid, "'", sep = "")
  
  result <- ROracle::dbSendQuery(con, tag_sql)
  result <- ROracle::fetch(result)
  
  samp = result[['SAMPLE_NUM']]
  samp_sql = paste("SELECT * FROM ", sampdb, " WHERE SAMPLE_ID = '", samp, "'", sep = "")
  
  samp_result <- ROracle::dbSendQuery(con, samp_sql)
  samp_result <- ROracle::fetch(samp_result)
  
  #this is just for reporting to console, to let user know exactly which tags have
  #been deleted.
  all_tags_in_sample_sql = paste("SELECT TAG_ID FROM ", biodb, " WHERE SAMPLE_NUM = '", samp, "'", sep = "")
  
  all_tags_result <- ROracle::dbSendQuery(con, all_tags_in_sample_sql)
  all_tags_result <- ROracle::fetch(all_tags_result)
  
  #delete bio table tags and single sample number row
  tags_delete_query = paste("DELETE FROM ", biodb, " WHERE SAMPLE_NUM = '", samp, "'", sep = "")
  sample_delete_query = paste("DELETE FROM ", sampdb, " WHERE SAMPLE_ID = '", samp, "'", sep = "")
  
  #send query
  send_bio_query = ROracle::dbSendQuery(con, tags_delete_query)
  send_sample_query = ROracle::dbSendQuery(con, sample_delete_query)
  
  ROracle::dbCommit(con)
  ROracle::dbDisconnect(con)
  
  out <- paste("\nAll releases in sample: ", samp, " have been deleted from BIO Table: ", all_tags_result, "\nSample ", samp, " deleted from LBT_SAMPLE table",  sep = "")
  
  return(out)
}