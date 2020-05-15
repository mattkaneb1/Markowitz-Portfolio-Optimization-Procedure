library(tidyverse) 
library(rvest)
library(robotstxt)
library(janitor)
library(RSelenium)
library(dplyr)
library(lubridate)

ticker <- c("AMZN","GOOG","MSFT","FB")
rn <- as.integer(as.POSIXct( Sys.time() ))

#start RSelenium
rD <- rsDriver(browser="chrome",chromever = "81.0.4044.138")
remDr <- rD[["client"]]
remDr$open()

#navigate to your page
remDr$navigate(paste("https://finance.yahoo.com/quote/",
                     ticker[1],
                     "/history?period1=",
                     as.character(rn - 315400000),
                     "&period2=",
                     as.character(rn),
                     "&interval=1d&filter=history&frequency=1d",sep=""))

#scroll down 20 times, waiting for the page to load at each time
for(i in 1:27){      
  remDr$executeScript(paste("scroll(0,",i*10000,");"))
  Sys.sleep(.25)    
}


#Import into R
page_source<-remDr$getPageSource()
tables <- read_html(page_source[[1]]) %>% html_nodes("table")
hist_data <- html_table(tables[[1]]) %>%
  clean_names() %>%
  select(date,adj_close)%>%
  filter(!grepl("Dividend",adj_close,ignore.case=TRUE)) %>%
  rename(!!ticker[1] := adj_close)


ticker2 <- ticker[2:length(ticker)]
# Repeat for the rest of the tickers
#==========================================================================
for(t in ticker2){
  remDr$navigate(paste("https://finance.yahoo.com/quote/",
                       t,
                       "/history?period1=",
                       as.character(rn - 315400000),
                       "&period2=",
                       as.character(rn),
                       "&interval=1d&filter=history&frequency=1d",sep=""))
  
  #scroll down 27 times, waiting for the page to load at each time
  for(i in 1:27){      
    remDr$executeScript(paste("scroll(0,",i*10000,");"))
    Sys.sleep(1)    
  }
  
  #Import into R
  page_source<-remDr$getPageSource()
  tables <- read_html(page_source[[1]]) %>% html_nodes("table")
  hist_data2 <- html_table(tables[[1]]) %>%
    clean_names() %>%
    select(date,adj_close) %>%
    filter(!grepl("Dividend",adj_close,ignore.case=TRUE)) %>%
    rename(!!t := adj_close)
  hist_data <- hist_data %>% inner_join(hist_data2)
}
#==========================================================================

#Close Server
remDr$close()
rD[["server"]]$stop()
gc(rD)

pct <- function(x) { (x[1]-x[2])/x[2] * 100}
parse<- function(x) {as.numeric(gsub(",","",x))}

wr_hist_data <- hist_data %>% 
  mutate(date = mdy(date), year = year(date),month=month(date),day = day(date)) %>%
  group_by(year,month) %>%
  filter(day == max(day) | day == min(day)) %>%
  mutate_at(ticker,parse) %>%
  summarize_at(ticker,pct)

annualize_ror <- function(x) {mean(x)*12}
annualize_sd <- function(x) {sd(x)*sqrt(12)}

annualized_monthly_ror <- wr_hist_data %>%
  ungroup() %>%
  group_by(year) %>%
  summarize_at(ticker,annualize_ror) 

annualized_monthly_sd <- wr_hist_data %>%
  ungroup() %>%
  group_by(year) %>%
  summarize_at(ticker,annualize_sd)

covar <- wr_hist_data %>% 
  ungroup() %>%
  select(-c(year,month)) %>%
  cov()
  


