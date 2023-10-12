#' @title  setup_and_send
#' @description  dual for setup  and send
#' @export
setup_and_send <- function(){
  setup_gmailr()
  sent_status = send_single_email()
  return(sent_status)
}

#' @title  setup_gmailr
#' @description  only needs to be run when setting up email client
#' @import gmailr
#' @export
setup_gmailr <- function(){
  jsondir2 <- "C:/bio.data/bio.lobster/data/tagging/lobtags_credentials.json"
  # suppress/automate interactive caching options for gargle functions (can't access these when running through html):
  # options(gargle_verbosity = "silent") ## other options are info and debug 
  # options(gargle_oauth_cache = TRUE)
  # do setup
  gm_auth_configure(path = jsondir2)
  gm_auth(email = "lobtags@gmail.com") # needs to run to initialize and validate local app
  
  return(TRUE)
}

#' @title  latest_pdf_file
#' @description  returns next pdf email file
#' @export
latest_pdf_file <- function(r_drive_email_dir ="R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/LobTag_outputs/Temp Files/Email Attachments/"){
  my_pdfs <- paste(r_drive_email_dir, sep = "")
  file_list <- list.files(path=my_pdfs, pattern = "*.pdf")
  if(length(file_list) == 0){
    out = FALSE
  } else {
    out <- paste0(my_pdfs,file_list[1])
  }
  
  return(out)
}

#' @title  send_single_email
#' @description  uses latest pdf, looks up email address associated with fisher and send attachment to them.
#' @export
send_single_email <- function(working_email_dir = "R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/LobTag_outputs/Temp Files/Email Attachments/",
                              r_drive_sent_email_dir = "R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/LobTag_outputs/Sent Documents/"){
#if the pdf looks good, we will send 1 single email to the corresponding address
#if it's not good, then we move the file to a different directory, but do not delete.
#this function works well, perhaps have fishername as argument in case user selects which pdf?
  
  out = ""
  
  next_fisher_attachment <- latest_pdf_file()
  p2 <- tools::file_path_sans_ext(next_fisher_attachment)
  
  x <- nchar(working_email_dir)
  
  #this is the name we will search for
  fisher_name = substr(p2, start = x+1, stop = nchar(p2))
  
  peopdb = paste("LOBSTER",".","LBT_PEOPLE", sep = "")
  captdb = paste("LOBSTER",".","LBT_CAPTURE", sep = "")
  
  my_sql_qry = paste("SELECT EMAIL FROM ", peopdb," WHERE NAME = '", fisher_name, "'", sep = "" )
  
  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  
  result <- ROracle::dbSendQuery(con, my_sql_qry)
  result <- ROracle::fetch(result)
  ROracle::dbDisconnect(con)
  
  fisher_email <- result[[1]]
  
  #this will send the email with the correct attachment
  #will need a body and a subject.
  
  subject = paste("Lobster Tag Report for ", fisher_name, sep = "")
  body_header = paste(fisher_name, ",\n\n", sep = "")
  #body_main = "Thanks for your participation in the lobster tagging program.\nPlease see attached report for movement information about the tagged lobster(s) you reported."
  body_main = "Thanks for your participation in the lobster tagging program. Please see attached tag recapture report for information on the tag(s) that you reported to us.
  
Our apologies that these tag reports have taken so long to be distributed. We have been developing a system for data storage and mapping. Future tag reports should be done in a more timely manner at the close of your fishing season.
  
Much appreciated.
  
DFO Lobster Science Team
Bedford Institute of Oceanography,
1 Challenger Dr.
Dartmouth, NS B2Y 4A2"
  
  body = paste0(body_header, body_main)
  
  #email_rewards2 returns a sent status. should use that return to optimize flow.
  email_statement = email_rewards2(to = fisher_email, subject = subject, body = body, attachments = next_fisher_attachment)
  out = paste(out, email_statement)
  
  #sent location
  sent_emails <- paste(working_email_dir, "sent/", sep = "")
  copied_location <- paste(sent_emails, fisher_name, " ", as.character(Sys.Date()), ".pdf", sep = "")
  sent_email_to_r_drive <- paste(r_drive_sent_email_dir, fisher_name, " ", as.character(Sys.Date()), ".pdf", sep = "")
  
  #copy file to sent directory on success (copy to r drive for archive and lookup later on)
  file.copy(next_fisher_attachment, copied_location)
  file.copy(next_fisher_attachment, sent_email_to_r_drive)
  out = paste(out, "\nemail attachment moved to sent folders")
  
  #delete file
  file.remove(next_fisher_attachment)
  
  #this functions are called twice in this function, create new function and put a try catch like the other database functions.
  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  
  #it probably makes more sense to move the rewarded section to its own function
  #change rewarded status from N to Y after email is sent
  update_rewarded = paste("UPDATE ", captdb, " SET REWARDED = 'Y' WHERE PERSON = '", fisher_name, "'", sep = "")
  ROracle::dbSendQuery(con, update_rewarded)
  ROracle::dbCommit(con)
  ROracle::dbDisconnect(con)
  
  return(out)
}

#' @title  do_not_send_email
#' @description moves pdf for further review, indicates human action required
#' @export
do_not_send_email <- function(working_email_dir = "R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/LobTag_outputs/Temp Files/Email Attachments/",
                              r_drive_not_sent_email_dir = "R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/LobTag_outputs/Not Sent Documents/"){
  
  not_sent_emails <- paste(working_email_dir, "not_sent/", sep = "")
  if(!(dir.exists(not_sent_emails))){
    dir.create(not_sent_emails, recursive = T)
  }
  
  next_fisher_attachment <- latest_pdf_file()
  x <- nchar(working_email_dir)
  
  #this is the name we will search for
  p2 <- tools::file_path_sans_ext(next_fisher_attachment)
  fisher_name = substr(p2, start = x+1, stop = nchar(p2))
  
  copied_location <- paste(not_sent_emails, fisher_name, ".pdf", sep = "")
  sent_email_to_r_drive <- paste(r_drive_not_sent_email_dir, fisher_name, " ", as.character(Sys.Date()), ".pdf", sep = "")
  
  file.copy(next_fisher_attachment, copied_location)
  file.copy(next_fisher_attachment, sent_email_to_r_drive)
  
  file.remove(next_fisher_attachment)
  
  out = paste("File moved to ", not_sent_emails, " for further review", sep = "")
  out = paste(out, "\nfile also copied to R drive not sent folder")
  return(out)
}

#' @title  open_pdf_file
#' @description  opens pdf file returned by latest_pdf_file
#' @export
open_pdf_file <- function(){
  system(paste0('open "', latest_pdf_file(), '"'))
  # out = paste("file opened succesfully")
  # return(out)
}

#' @title  prep_email_send
#' @description  preps and returns info about the email that's about to be sent.
#' @export
prep_email_send <- function(r_drive_email_dir = "R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/LobTag_outputs/Temp Files/Email Attachments/"){
  #we will collect all the info here and then we'll pass it along to the send single email function to keep things streamlined
  #falls apart if there are no files there.
  next_fisher_attachment <- latest_pdf_file()
  
  if (next_fisher_attachment == FALSE){
    email_info = "no attachments in directory, no emails to send"
    return(email_info)
  }
  
  p2 <- tools::file_path_sans_ext(next_fisher_attachment)
  x <- nchar(r_drive_email_dir)
  
  #this is the name we will search for
  fisher_name = substr(p2, start = x+1, stop = nchar(p2))
  
  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  
  peopdb = paste("LOBSTER",".","LBT_PEOPLE", sep = "")
  my_sql_qry = paste("SELECT EMAIL FROM ", peopdb," WHERE NAME = '", fisher_name, "'", sep = "" )
  
  result <- ROracle::dbSendQuery(con, my_sql_qry)
  result <- ROracle::fetch(result)
  ROracle::dbDisconnect(con)
  
  fisher_email <- result[[1]]
  
  #send email to 'fisher name' @ 'the fisher email'?
  email_info <- paste("\nsend email to ", fisher_name, " at email: ", fisher_email)
  
  return(email_info)
}

#' @title  email_rewards2
#' @description  Sends an email
#' @param subject Text for subject
#' @param body Text for body
#' @param to email address as text
#' @param attachments path to file to attach to email
#' @import gmailr
#' @export
email_rewards2 = function(to = "lobtags@gmail.com", subject = "default subject", body = "default body", attachments = ""){
  
  msg_subject = subject
  msg_body = body
  email_to = to
  total_attach = attachments
  
  #lets try configuring sooner...
  #gm_auth_configure(path = jsondir2)
  #gm_auth() # I believe this only needs be run once at setup
  
  my_email_message <- gm_mime() %>%
    gm_to(email_to) %>%
    gm_from("lobtags@gmail.com") %>%
    gm_subject(msg_subject) %>%
    gm_text_body(msg_body) %>%
    gm_attach_file(total_attach)
  
  gm_send_message(my_email_message)
  
  #there is a new sent status created with a string when this function is called
  #it would be better to use this one
  sent_status = paste("\nemail with attachment sent to ", email_to, sep = "")
  
  return(sent_status)
}
