library(tidyverse) 
library(rvest)
library(robotstxt)
library(janitor)
library(RSelenium)

ticker <- "AMZN"
rn <- as.integer(as.POSIXct( Sys.time() ))

#start RSelenium
rD <- rsDriver(browser="chrome",chromever = "81.0.4044.138")
remDr <- rD[["client"]]
remDr$open()

#navigate to your page
remDr$navigate(paste("https://finance.yahoo.com/quote/",
                     ticker,
                     "/history?period1=",
                     as.character(rn - 315400000),
                     "&period2=",
                     as.character(rn),
                     "&interval=1d&filter=history&frequency=1d",sep=""))

#scroll down 20 times, waiting for the page to load at each time
for(i in 1:25){      
  remDr$executeScript(paste("scroll(0,",i*10000,");"))
  Sys.sleep(1)    
}

#Import into R
page_source<-remDr$getPageSource()
amzn_tables <- read_html(page_source[[1]]) %>% html_nodes("table")
amzn10y <- html_table(amzn_tables[[1]])

#Close Server
remDr$close()
rD[["server"]]$stop()
gc(rD)


