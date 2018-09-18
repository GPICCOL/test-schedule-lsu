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
  
  target <- paste0("https://www.cae.lsu.edu/instr_getresults2.asp?selCourse", section, "&testid=", test)
  target2 <- paste0("https://www.cae.lsu.edu/temp/ISDS110200", flg, ".txt")
  
  # Session and login
  session <- html_session(login)
  form <- html_form(session)[[1]]
  form <- set_values(form, pawsid = id, pwd = pwd)
  submit_form(session = session, form = form)
  
  # Data extraction
  jump_to(x = session, url = target)
  df <- read.delim(target2,header = FALSE)
  colnames(df) <- c("id", "name", "section", "date", "time", "q_asked", "q_correct", "percentage")
  df$section <- flg
    # read_html() %>% 
    # html_node("form") %>%
    # html_node("a")
    # html_table(trim = TRUE)
    # 
    # 
    # 
    # html_table(trim = TRUE)
    # 
    # tb <- readHTMLTable("temp.html", which = 2)
  
  # Results
  return(df)
#  write.csv(file = paste0("tests/", "res_s", flg, test_name, tm, ".csv"), df, row.names = FALSE) 
}

df <- do.call(rbind, lapply(c(1:6), scrp_fun))
df$time <- paste0(df$date, " ", df$time)
df$date <- as.Date(df$date, format = "%m/%d/%Y")
df$timestamp <- strptime(df$time, format = "%m/%d/%Y %I:%M:%S %p", tz = "America/Chicago")
str(df)
df$t <- with(df, ifelse(section >= 3 & section <= 6, "yes", "no"))

df %>% 
  select(-timestamp) %>% 
  group_by(section) %>% 
  summarize(average = mean(percentage), n = n())

df %>% 
  select(-timestamp) %>% 
  group_by(t) %>% 
  summarize(avetrage = mean(percentage))

fit <- lm(formula = percentage ~ t + section, data = df)
summary(fit)

