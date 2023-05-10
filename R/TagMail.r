#' @title  setup_gmailr
#' @description  only needs to be run when setting up email client
#' @import gmailr
#' @export
setup_gmailr <- function(){
  #jsondir2 <- "C:/Users/mckinnonsea/Desktop/New_Lobtag/LobTag/data/client_secret_868198986667-jo5m08ijphktjq04hp5plhfd26c5nb4s.apps.googleusercontent.com.json"
  jsondir2 <- "C:/bio/lobtags_credentials.json"
  
  gm_auth_configure(path = jsondir2)
  gm_auth(email = "lobtags@gmail.com") # needs to run to initialize and validate local app
  return(TRUE)
}

#' @title  latest_pdf_file
#' @description  returns next pdf email file
#' @export
latest_pdf_file <- function(){
  #my_pdfs <- "C:/Users/mckinnonsea/Desktop/New_Lobtag/LobTag/inst/extdata/emails_attachments"
  r_drive_email_dir <- "R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/Taggging Files/email attachments"
  r_drive_email_dir <- "C:/bio/LobTag/temp_files/emails_attachments"
  my_pdfs <- paste(r_drive_email_dir, sep = "")
  file_list <- list.files(path=my_pdfs)
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
  
  r_drive_email_dir <- "R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/Taggging Files/email attachments/"
  x <- nchar(r_drive_email_dir)
  #x <- nchar("C:/Users/mckinnonsea/Desktop/New_Lobtag/LobTag/inst/extdata/emails_attachments/")
  #x is 79... but I'll leave this here just in case the directory changes
  
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
  body = "Please see attached report for movement information about the tagged lobster(s) you reported"
  
  email_rewards(to = fisher_email, subject = subject, body = body, attachments = next_fisher_attachment)
  
  #delete file, we are going to ove
  #unlink(next_fisher_attachment)
  
  status_statement = paste("email sent to ", fisher_name, " at ", fisher_email, "\nLocal file moved to sent", sep = "")
  
  #sent location
  #sent_emails <- "C:/Users/mckinnonsea/Desktop/New_Lobtag/LobTag/inst/extdata/emails_attachments/sent/"
  sent_emails <- paste(r_drive_email_dir, "sent/", sep = "")
  
  #copy file to sent directory on success
  file.copy(next_fisher_attachment, sent_emails)
  
  #delete file ... move it to a sent bin.
  file.remove(next_fisher_attachment)
  
  #this functions are called twice in this function, create new function and put a try catch like the othere database functions.
  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, username = oracle.lobster.user, password = oracle.lobster.password, dbname = oracle.lobster.server)
  
  captdb = paste("LOBSTER",".","LBT_CAPTURE", sep = "")
  
  #change rewarded status from N to Y after email is sent
  update_rewarded = paste("update ", captdb, " set REWARDED = 'Y' where PERSON = '", fisher_name, "'", sep = "")
  ROracle::dbSendQuery(con, update_rewarded)
  ROracle::dbCommit(con)
  ROracle::dbDisconnect(con)
  
  return(status_statement)
}

#' @title  do_not_send_email
#' @description moves pdf for further review, indicates human action required
#' @export
do_not_send_email <- function(){
  r_drive_email_dir <- "R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Projects and Programs/Tagging/Taggging Files/email attachments/"
  not_sent_emails <- paste(r_drive_email_dir, "sent/", sep = "")
  #not_sent_emails <- "C:/Users/mckinnonsea/Desktop/New_Lobtag/LobTag/inst/extdata/emails_attachments/not_sent/"
  
  next_fisher_attachment <- latest_pdf_file()
  
  file.copy(next_fisher_attachment, not_sent_emails)
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
  
  email_info <- paste("send email to ", fisher_name, " at email: ", fisher_email)
  
  return(email_info)
}