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
  
  gm_auth_configure(path = jsondir2)
  gm_auth(email = "lobtags@gmail.com") # needs to run to initialize and validate local app
  
  #lets try configuring sooner...
  #email as an arguement or no email?
  #gm_auth_configure(path = jsondir2)
  #gm_auth() # I believe this only needs be run once at setup
  
  return(TRUE)
}

#' @title  latest_pdf_file
#' @description  returns next pdf email file
#' @export
latest_pdf_file <- function(){
  #my_pdfs <- "C:/Users/mckinnonsea/Desktop/New_Lobtag/LobTag/inst/extdata/emails_attachments"
  #r_drive_email_dir <- "R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/Taggging Files/email attachments"
  #r_drive_email_dir <- "C:/bio/LobTag/temp_files/final_attachments"
  r_drive_email_dir <- "C:/bio/LobTag/temp_files/emails_attachments"
  my_pdfs <- paste(r_drive_email_dir, sep = "")
  file_list <- list.files(path=my_pdfs, pattern = "*.pdf")
  out <- paste0(my_pdfs,"/",file_list[1])
  return(out)
}

#' @title  delete_latest_file
#' @description  deletes last pdf email file
#' @export
delete_latest_file <- function(){
  r_drive_email_dir <- "R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/Taggging Files/email attachments"
  my_pdfs <- paste(r_drive_email_dir, sep = "")
  #my_pdfs <- "C:/Users/mckinnonsea/Desktop/New_Lobtag/LobTag/inst/extdata/emails_attachments"
  file_list <- list.files(path=my_pdfs)
  unlink(paste0(my_pdfs,"/",file_list[1]))
  return(TRUE)
}

#' @title  collect_emails
#' @description  returns list of emails associated with names of letters in email_attachment directory
#' @export
collect_emails <- function(){
  #looking for emails
  #sql query...
  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  
  peopdb = paste("LOBSTER",".","LBT_PEOPLE", sep = "")
  
  #collect all emails
  #start list
  
  emails <- list()
  
  #emails for the people that 
  for(i in file_list){
    p <- tools::file_path_sans_ext(i)
    # print(p)
    
    my_sql_qry = paste("SELECT EMAIL FROM ", peopdb," WHERE NAME = '", p, "'", sep = "" )
    result <- ROracle::dbSendQuery(con, my_sql_qry)
    result <- ROracle::fetch(result)
    emails <- append(emails,result)
  }
  #result <- ROracle::fetch(result)
  ROracle::dbDisconnect(con)
  return(emails)
}

#' @title  send_single_email
#' @description  uses latest pdf, looks up email address associated with fisher and send attachment to them.
#' @export
send_single_email <- function(){
#if the pdf looks good, we will send 1 single email to the corresponding address
#if it's not good, then we move the file to a different directory, but do not delete.
#this function works well, perhaps have fishername as arguement in case user selects which pdf?
  next_fisher_attachment <- latest_pdf_file()
  
  p2 <- tools::file_path_sans_ext(next_fisher_attachment)
  
  #r_drive_email_dir <- "R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/Taggging Files/email attachments/"
  #x <- nchar(r_drive_email_dir)
  #x <- nchar("C:/Users/mckinnonsea/Desktop/New_Lobtag/LobTag/inst/extdata/emails_attachments/")
  #x is 79... but I'll leave this here just in case the directory changes
  
  r_drive_email_dir <- "C:/bio/LobTag/temp_files/emails_attachments/"
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
  email_rewards2(to = fisher_email, subject = subject, body = body, attachments = next_fisher_attachment)

  #this is the status that will be returned to the message console in the user interface
  #it should be returned from email_rewards2 so we can see errors.
  status_statement = paste("\nemail sent to ", fisher_name, " at ", fisher_email, "\nLocal file moved to sent", sep = "")
  
  #sent location
  #sent_emails <- "C:/Users/mckinnonsea/Desktop/New_Lobtag/LobTag/inst/extdata/emails_attachments/sent/"
  sent_emails <- paste(r_drive_email_dir, "sent/", sep = "")
  copied_location <- paste(sent_emails, fisher_name, ".pdf", sep = "")
  
  #copy file to sent directory on success
  file.copy(next_fisher_attachment, copied_location)
  
  #delete file ... move it to a sent bin.
  file.remove(next_fisher_attachment)
  
  #this functions are called twice in this function, create new function and put a try catch like the othere database functions.
  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  
  captdb = paste("LOBSTER",".","LBT_CAPTURE", sep = "")
  
  #it probably makes more sense to move the rewarded section to its own function
  #change rewarded status from N to Y after email is sent
  update_rewarded = paste("UPDATE ", captdb, " SET REWARDED = 'Y' WHERE PERSON = '", fisher_name, "'", sep = "")
  ROracle::dbSendQuery(con, update_rewarded)
  ROracle::dbCommit(con)
  ROracle::dbDisconnect(con)
  
  return(status_statement)
}

#' @title  do_not_send_email
#' @description moves pdf for further review, indicates human action required
#' @export
do_not_send_email <- function(){
  #r_drive_email_dir <- "R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/Taggging Files/email attachments/"
  r_drive_email_dir <- "C:/bio/LobTag/temp_files/emails_attachments/"
  #not_sent_emails <- paste(r_drive_email_dir, "not_sent/", sep = "")
  not_sent_emails <- paste(r_drive_email_dir, "not_sent/", sep = "")
  #not_sent_emails <- "C:/Users/mckinnonsea/Desktop/New_Lobtag/LobTag/inst/extdata/emails_attachments/not_sent/"
  
  next_fisher_attachment <- latest_pdf_file()
  x <- nchar(r_drive_email_dir)
  
  #this is the name we will search for
  p2 <- tools::file_path_sans_ext(next_fisher_attachment)
  fisher_name = substr(p2, start = x+1, stop = nchar(p2))
  
  copied_location <- paste(not_sent_emails, fisher_name, ".pdf", sep = "")
  
  file.copy(next_fisher_attachment, copied_location)
  file.remove(next_fisher_attachment)
  
  out = paste("File moved to ", not_sent_emails, " for further review", sep = "")
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
prep_email_send <- function(){
  #we will collect all the info here and then we'll pass it along to the send single email function to keep things streamlined
  #falls apart if there are no files there.
  next_fisher_attachment <- latest_pdf_file()
  p2 <- tools::file_path_sans_ext(next_fisher_attachment)
  #r_drive_email_dir <- "R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/Taggging Files/email attachments/"
  #r_drive_email_dir <- "C:/bio/LobTag/temp_files/final_attachments/"
  r_drive_email_dir <- "C:/bio/LobTag/temp_files/emails_attachments/"
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

#' @title  email_rewards
#' @description  Sends an email
#' @param subject Text for subject
#' @param body Text for body
#' @param to email address as text
#' @param attachments path to file to attach to email
#' @import gmailr
#' @export
email_rewards = function(to = "lobtags@gmail.com", subject = "default subject", body = "default body", attachments = ""){
  
  jsondir2 <- "C:/Users/mckinnonsea/Desktop/New_Lobtag/LobTag/data/client_secret_868198986667-jo5m08ijphktjq04hp5plhfd26c5nb4s.apps.googleusercontent.com.json"
  gm_auth_configure(path = jsondir2)
  gm_auth(email = "lobtags@gmail.com") # needs to run to initialize and validate local app
  
  # if(attachments == ""){
  #   attachdir <- "C:/Users/mckinnonsea/Downloads/"
  #   attachfile <- "20220617_074627.jpg"
  #   total_attach = paste(attachdir, attachfile, sep="")
  # } else {
  #   total_attach = attachments
  # }
  
  msg_subject = subject
  msg_body = body
  email_to = fisher_email
  total_attach = next_fisher_attachment
  
  #lets try configuring sooner...
  gm_auth_configure(path = jsondir2)
  gm_auth() # I believe this only needs be run once at setup
  
  my_email_message <- gm_mime() %>%
    gm_to(email_to) %>%
    gm_from("lobtags@gmail.com") %>%
    gm_subject(msg_subject) %>%
    gm_text_body(msg_body) %>%
    gm_attach_file(total_attach)
  
  gm_send_message(my_email_message)
  
  sent_status = paste("email with attachment sent to ", email_to, sep = "")
  
  return(sent_status)
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
