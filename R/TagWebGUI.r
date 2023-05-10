#' @title upload_from_file3
#' @import dplyr readxl
#' @return note to use will be read in web interface
#' @export
upload_from_file3 <- function(myfile){
  #this function is called by readcsvnew2 in 'LobTagging.r'
  
  #go through excel sheet and upload one chain at a time...
  #has to do one batch per lat/lon send the data to 'sample_ent'
  
  #initilize user message
  out = ""
  
  #this was left over from a debug, just gonna leave it even though it's a bit redundant
  my_data = read.csv(myfile)
  my_new_Data <- my_data
  
  #removed lat and lon columns (names has to be the same width as data)
  all_names_new = c("Vessel", "Port", "Captain",	"LFA",	"Affiliation",	"Sampler",	"Sampler 2",	"Sampler 3", "Tag Prefix",	"Tag Num",	"Tag Color",	"Carapace Length",	"Sex",	"Shell",	"Claw",	"V-Notch",	"Claw lost in trap",	"Day",	"Month", "Year",	"Date",	"Lat Degrees",	"Lat Minutes",	"Lon Degrees",	"Lon Minutes",	"Comments",	"latdd.dd",	"londd.dd", "date2")
  
  names(my_new_Data) = all_names_new
  bdata_keep = c("Tag Num","Tag Color","Carapace Length","Sex","Shell","Claw","V-Notch")
  
  #do an error check in here, data should be in a certain format.
  #other error checks we could do?
  #lat/lon outside of NS LFAs
  #could check that LFA matches the LAT/LON too with the ISIN function...
  
  #these lists are to catch errors. Sex must be 1-3 (not optional so no NA)
  #shell is optional but if entered must be 1-6, same with claw and v-noth except 1-3 and yes/no respectively
  sex_values <- c(1,2,3)
  shell_values <- c(NA, 1:6)
  claw_values <- c(NA,1,2,3)
  vnotch_values <- c(NA,"YES","NO")
  carapace_values <- c(NA, 40:160) #this one will alert the user but continue with upload. 
  
  #create subset to check data integrity, also check for duplicate tags being entered on same sheet.
  repeat_tags = my_new_Data$`Tag Num`[duplicated(my_new_Data$`Tag Num`)==TRUE]
  sex_problems <- subset(my_new_Data, !(Sex %in% sex_values))$'Tag Num'
  shell_problems <- subset(my_new_Data, !(Shell %in% shell_values))$'Tag Num'
  claw_problems <- subset(my_new_Data, !(Claw %in% claw_values))$'Tag Num'
  vnotch_problems <- subset(my_new_Data, !(`V-Notch` %in% vnotch_values))$'Tag Num'
  carapace_problems <- subset(my_new_Data, !(`Carapace Length` %in% carapace_values))$'Tag Num'
  
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
  
  if(return_error){
    return(out)
  }
  
  #do data manipulation after error checking
  #fill in all missing tag colours/prefixes based on affiliation: DFO/CBFH is Blue (XY), SWLSS is RED (SWLSS)
  #doesn't overwrite existing tag colours or prefixes
  my_new_Data[(is.na(my_new_Data$'Tag Color')),]$'Tag Color' <- sapply(my_new_Data[(is.na(my_new_Data$'Tag Color')),]$Affiliation,tag_color_filler)
  my_new_Data[(is.na(my_new_Data$'Tag Prefix')),]$'Tag Prefix' <- sapply(my_new_Data[(is.na(my_new_Data$'Tag Prefix')),]$Affiliation,tag_prefix_filler)
  
  #create pipe that groups excel
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
  for(x in 1:max(new_excel_df$id)){
    #look at specific group, this will be looped
    newdf2 <- new_excel_df  %>% filter(id == x)

    #bdata is dataframe, sdata is list
    new_df3 <- as.data.frame(newdf2)
    bdata_df = new_df3[bdata_keep]

    sdata_list = c(new_df3["Vessel"][1,1],
                   new_df3["Port"][1,1],
                   new_df3["Captain"][1,1],
                   new_df3["LFA"][1,1],
                   new_df3["Affiliation"][1,1],
                   new_df3["Sampler"][1,1],
                   new_df3["Date"][1,1],
                   new_df3["Lat Degrees"][1,1] * 100 + new_df3["Lat Minutes"][1,1], #conpos function does the same thing but it's quicker here
                   new_df3["Lon Degrees"][1,1] * 100 + new_df3["Lon Minutes"][1,1],
                   new_df3["Comments"][1,1],
                   new_df3["Day"][1,1],
                   new_df3["Month"][1,1],
                   new_df3["Year"][1,1],
                   new_df3["latdd.dd"][1,1],
                   new_df3["londd.dd"][1,1]
    )
    #the print is for debug, but sample_ent loads all data into oracle
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
    end_string = unlist(gregexpr('&lon=.{13}W', file_str)) + 19 # the '+19' is static because that's the length of the delimeter to the end of the string
    
    sdata = substring(file_str, start_string, end_string - 1)
    
    samp = myUrlEncode(sdata)
    samp = unlist(str_split(samp, "&"))
  
    dat = ""
    sam = ""
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
    dat = sdata[7]
    sam = sdata[6]
    ves = sdata[1]
    LFA = sdata[4]
    capt = sdata[3]
    dep = ""
    com = sdata[10]
    lat = sdata[8]
    lon = sdata[9]
    affl = sdata[5]
    day = sdata[11]
    mon = sdata[12]
    year = sdata[13]
    rlat = sdata[14]
    rlon = as.numeric(sdata[15]) * -1
    
    dat = paste(day, mon, year, sep = "/")
  }
  
  sta = ""
  res = ""
  samp = ""
  sampsql = ""
  out = ""
  wrisamp = FALSE
  writrip = FALSE

  #database names...
  database_name = paste("LOBSTER",".","LBT_TRIP", sep="")
  sampdb = paste("LOBSTER",".","LBT_SAMPLE", sep="")
  tripdb = paste("LOBSTER",".","LBT_TRIP", sep="")
  
  sql = paste("SELECT TRIP_ID from ", tripdb, " where RELEASE_DATE = to_date('", dat,"', 'dd/mm/yyyy') AND TECHNICIAN = '",sam,"'", sep = "")

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
  
  tripsql = paste("INSERT INTO ",tripdb," (TRIP_ID, TECHNICIAN, AFFILIATION, VESSEL, LFA, YEAR, STATSAREA, REPORTED, CAPTAIN, SUBAREA, RELEASE_DATE) VALUES( '",res,"' , '",sam,"' , '",affl,"' , '",SQLsafty(ves),"' , '",LFA,"' , '",year,"' , '",sta ,"' , 0 , '",SQLsafty(capt) ,"' , '",suba,"' , to_date('", dat,"', 'dd/mm/yyyy'))", sep = "")
  
  writrip = T
  }

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
  
  sampsql = paste("INSERT INTO ", sampdb, " VALUES( '",samp,"' , '",res,"' , '",lat,"' , '",lon,"'  ,  '",rlat,"' , '",rlon,"' , '",dep,"' , '",SQLsafty(com),"')", sep = "")
  wrisamp = TRUE
}

if(from_file == TRUE){
  dd = bdata
} else {
  dd = as.data.frame(jsonlite::fromJSON(jsonFilePath)[2:nrow(jsonlite::fromJSON(jsonFilePath)),])
  names(dd) = jsonlite::fromJSON(jsonFilePath)[1,]
  
  #delete tempfile
  unlink(jsonFilePath)
}


#this for loop will check if any of the Tag Numbers that you are inputting have already been entered. If they have it will reject all the entries and report a list of repeat tags.
writedata = TRUE
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
        sql = paste("INSERT INTO ", biodb, " vALUES ('",samp,"', '",dd$`Tag Num`[i],"', '",dd$`Carapace`[i],"', '",dd$Shell[i],"','",dd$Claw[i],"','",dd$`Tag Color`[i],"','",dd$Sex[i],"','",dd$`V-Notch`[i],"','",'XY',"')", sep = "")
        
        #the commented code below has headers that belong to crab... we've since updated.
        result <- ROracle::dbSendQuery(con, sql) 

        if(dbGetInfo(result, what = "rowsAffected") > 0){
          out =  paste(out, "\nLobster with  tag " , dd$`Tag Num`[i], " successfully added", sep = "")
        }
        else{
          out =  paste(out,"\nError: ",  result)
          return(out)
          die()
        }
      }
    }
  }
  
  if(wrisamp){
    
    rs = ROracle::dbSendQuery(con, sampsql) 
    
    if(dbGetInfo(rs, what = "rowsAffected") == 1){
      out = paste(out,"\nSample from trip ",res, " with pos ",lat, " " ,lon, " successfully added", sep = "")
    }
    else{
      out =  paste(out, "\nError: row 264: " ,sampsql , "\n" , rs, "\n", sep = "")
      return(out)
      die()
    }
  }
  if(writrip){
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
  
  # region = "ss"
  
  # lobster database shortcuts
  # captdb = paste("LOBSTER",".","LBT_CAPTURE", sep = "")
  # peopdb = paste("LOBSTER",".","LBT_PEOPLE", sep = "")
  lbiodb = paste("LOBSTER",".","LBT_BIO", sep = "")
  
  # con = odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
  result = ""
  #if(region == "ss"){
    #res = sqlQuery(con, "select TAG_ID from SCT_BIO" )
  my_qry <- paste("SELECT TAG_ID from ", lbiodb, sep = "")
  result <- ROracle::dbSendQuery(con, my_qry)
    #old crab code
    #result <- ROracle::dbSendQuery(con, "select TAG_ID from SCT_BIO") 
    
  #}
  # if(region == "g"){
  #   #res = sqlQuery(con, "select NAME from SCT_PEOPLE_GULF" )
  #   result <- ROracle::dbSendQuery(con, "select TAG_ID from SCT_BIO_GULF") 
  # }
  
  result <- ROracle::fetch(result)
  ROracle::dbDisconnect(con)
  
  #odbcClose(con)
  
  return(toJSON(result))
  # 
  # con = odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
  # 
  # res = ""
  # if(region == "ss"){
  #   res = sqlQuery(con, "select TAG_ID from SCT_BIO" )
  # }
  # if(region == "g"){
  #   res = sqlQuery(con, "select TAG_ID from SCT_BIO_GULF" )
  # }
  # odbcClose(con)
  # 
  # return(toJSON(res))
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

#' @title  auto_availableDate
#' @description Function that autocompletes dates from captured tags
#' @import ROracle jsonlite
#' @export
autoavailableDate = function(region = "", tagid = ""){
  tryCatch({
    drv <- DBI::dbDriver("Oracle")
    con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  }, warning = function(w) {
  }, error = function(e) {
    return(toJSON("Connection failed"))
  }, finally = {
  })

  captdb = paste("LOBSTER",".","LBT_CAPTURE", sep = "")

  tagid = 2587 #testout a random tag
  my_qry <- paste("SELECT CAPTURE_DATE FROM ", captdb, " WHERE TAG = '", tagid, "'", sep = "")
  result <- ROracle::dbSendQuery(con, my_qry)

  result <- ROracle::fetch(result)
  ROracle::dbDisconnect(con)
  
  return(toJSON(result))
}

#' @title  delete_capture_tag
#' @description delete lobster tag after bad data
delete_capture_tag = function(deldata = ""){
  tryCatch({
    drv <- DBI::dbDriver("Oracle")
    con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  }, warning = function(w) {
  }, error = function(e) {
    return(toJSON("Connection failed"))
  }, finally = {
  })
  
  tagid = ""
  date = ""
  
  biodb  = paste("LOBSTER",".","LBT_BIO", sep = "")
  tripdb  = paste("LOBSTER",".","LBT_TRIP", sep = "")
  captdb = paste("LOBSTER",".","LBT_CAPTURE", sep = "")
  
  my_query = paste('DELETE FROM "LOBSTER"."LBT_CAPTURE" WHERE TAGID = ' ,sep = "")
  #tagid = 2587
  #date = result[[1]][1]
  
  sql = paste("SELECT * FROM ", captdb, " WHERE TAG = '", tagid, "' AND CAPTURE_DATE = '", '24-MAY-22', "'", sep = "")
  
  my_query = paste("DELETE FROM ", captdb, " WHERE TAG_ID = '", tagid, "' AND CAPTURE_DATE = '", date, "'", sep = "")
  my_query = paste("DELETE FROM ", captdb, " WHERE TAG_ID = '", tagid, "' AND CAPTURE_DATE = '", date, "'", sep = "")
  sql = paste("SELECT * FROM ", captdb, " WHERE TAG = '", tagid, "' AND CAPTURE_DATE = '", date, "'", sep = "")
  sql = paste("SELECT * FROM ", captdb, " WHERE TAG = '", tagid, "'", sep = "")
  
  #tagid = 'testvessel'
  sql = paste("DELETE FROM ", biodb, " where TAG_ID = '", tagid,"'", sep = "")
  sql = paste("DELETE FROM ", tripdb, " where VESSEL = '", tagid,"'", sep = "")
  
  #do this except delete entire row instead of selecting.
  
  send_query = ROracle::dbSendQuery(con, sql)
  
  ROracle::dbCommit(con)
  ROracle::dbDisconnect(con)
  
  result <- ROracle::dbSendQuery(con, sql)
  result <- ROracle::fetch(result)
  ROracle::dbDisconnect(con)
  
  
  return(TRUE)
}

#' @title  delete_lobster_peripherals
#' @description delete lobster tag after bad data
delete_lobster_peripherals = function(tagid = ""){
  tryCatch({
    drv <- DBI::dbDriver("Oracle")
    con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  }, warning = function(w) {
  }, error = function(e) {
    return(toJSON("Connection failed"))
  }, finally = {
  })
  
  #delete trip if empty
  biodb  = paste("Lobster",".","LBT_BIO", sep = "")
  captdb = paste("LOBSTER",".","LBT_CAPTURE", sep = "")
  peopdb = paste("LOBSTER",".","LBT_PEOPLE", sep = "")
  
  send_query = ROracle::dbSendQuery(con, sampsql)
  
  ROracle::dbCommit(con)
  ROracle::dbDisconnect(con)
  
  #delete sample
  return(TRUE)
}

#' @title  delete_lobster_capture
#' @description delete lobster capture data
delete_lobster_capture = function(tagid = ""){
  tryCatch({
    drv <- DBI::dbDriver("Oracle")
    con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  }, warning = function(w) {
  }, error = function(e) {
    return(toJSON("Connection failed"))
  }, finally = {
  })
  
  my_query = paste('DELETE FROM "LOBSTER"."LBT_CAPTURE" WHERE ',sep = "")
  return(TRUE)
}
