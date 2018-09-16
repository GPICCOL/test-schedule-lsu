#Import libraries
library(tidyverse)
library(rvest)

scrp_fun <- function(flg) {
  # Target forming
  
  source("config.R")
  
  section <- get(paste0("section", flg))
  test <- get(paste0("test", flg))
  id <- get(paste0("id", flg))
  pwd <- get(paste0("pwd", flg))
  
  target <- paste0("https://www.cae.lsu.edu/instr_getApptlist.asp?selCourse=", section, "&testid=", test)
  
  # Session and login
  session <- html_session(login)
  form <- html_form(session)[[1]]
  form <- set_values(form, pawsid = id, pwd = pwd)
  submit_form(session = session, form = form)
  
  # Data extraction
  tb <- jump_to(x = session, url = target) %>% 
    read_html() %>% 
    html_table(trim = TRUE)
  df <- tb[[2]]
  colnames(df) <- df[1, ]
  df <- df[-1, ]
  
  # Results
  write.csv(file = paste0("tests/", "s", flg, test_name, tm, ".csv"), df, row.names = FALSE) 
}

sapply(3:6, scrp_fun)


