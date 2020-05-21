#IMPORTS
#===================
library(tidyverse) 
library(rvest)
library(robotstxt)
library(janitor)
library(RSelenium)
library(dplyr)
library(lubridate)
library(lpSolve)
#===================

#List of Tickers (Manually Inputted For Now)
ticker <- c("AMZN","MSFT","FB")

#Grab TimeStamp
rn <- as.integer(as.POSIXct( Sys.time() ))

#start RSelenium
rD <- rsDriver(browser="chrome",chromever = "81.0.4044.138")
remDr <- rD[["client"]]
remDr$open()

#navigate to the yahoo finance page of first ticker
remDr$navigate(paste("https://finance.yahoo.com/quote/",
                     ticker[1],
                     "/history?period1=",
                     as.character(rn - 315400000),
                     "&period2=",
                     as.character(rn),
                     "&interval=1d&filter=history&frequency=1d",sep=""))

#scroll down 27 times, waiting for the page to load at each time
for(i in 1:27){      
  remDr$executeScript(paste("scroll(0,",i*10000,");"))
  Sys.sleep(.25)    
}


#Import data into R
page_source<-remDr$getPageSource()
tables <- read_html(page_source[[1]]) %>% html_nodes("table")
hist_data <- html_table(tables[[1]]) %>%
  clean_names() %>%
  select(date,adj_close)%>%
  filter(!grepl("Dividend",adj_close,ignore.case=TRUE)) %>%
  rename(!!ticker[1] := adj_close)

#Create List of Remaining Tickers
ticker2 <- ticker[2:length(ticker)]

# Repeat for remaining tickers
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

#Close Selenium Server
remDr$close()
rD[["server"]]$stop()
gc(rD)

# Percent Change Function
pct <- function(x) { (x[1]-x[2])/x[2]}

# Parse Imported Data to Numeric Format
parse<- function(x) {as.numeric(gsub(",","",x))}

# Seperate date out into day,month & year and then calculate monthly percent change
wr_hist_data <- hist_data %>% 
  mutate(date = mdy(date), year = year(date),month=month(date),day = day(date)) %>%
  group_by(year,month) %>%
  filter(day == max(day) | day == min(day)) %>%
  mutate_at(ticker,parse) %>%
  summarize_at(ticker,pct)

#Calculate Average Monthly ROR
avg_monthly_ror <- wr_hist_data %>%
  ungroup() %>%
  summarize_at(ticker,mean) 
avg_monthly_ror <- as.numeric(avg_monthly_ror[1,])

# Create Covariance Matrix
covar <- wr_hist_data %>% 
  ungroup() %>%
  select(-c(year,month)) %>%
  cov()

# Create a random portfolio breakdown 
rand_wts <- runif(n = length(ticker))
rand_wts <- rand_wts/sum(rand_wts)

# Function for calculating the Sharpe Ratio of a portfolio given weights 
fr <- function(x) {  
  #Annualized Returns given Weights
  returns <- (sum(x * avg_monthly_ror) + 1)^12 - 1 
  #Portfolio Variance given Weights
  risk <- sqrt(t(x) %*% (covar %*% x))
  #Sharpe Ratio
  returns/risk
}

#Create Constraint Matrix(Each position must be less than 1 and all of them must sum to 1)
con <- rep(c(-1),length(ticker))
con<- con %>% rbind(diag(length(ticker))*-1)

# Get rid of this  to enable short selling
con<- con %>% rbind(diag(length(ticker))* 1)

# Optimize Sharpe Ratio Function Subject to Constraints
solution <- constrOptim(theta=rand_wts,
                        f=fr,
                        grad=NULL,
                        ui=con,
                        #Get rid of third part of this to enable short selling
                        ci=c(-1,rep(c(-1),length(ticker)),rep(c(0),length(ticker)))-1e-6,
                        control=list(fnscale = -1))

#Save the resulting weights
optim_wts <- solution$par

#Calculate Annualized Returns with Optimized Weights
returns <- (sum(optim_wts * avg_monthly_ror) + 1)^12 - 1

#Calculate Portfolio Variance with Optimized Weights
risk <- sqrt(t(optim_wts) %*% (covar %*% optim_wts))

#Calculate Sharpe Ratio with Optimized Weights
sharpe<- returns/risk

#Create dataframe by combining ticker and optimal weights vectors
optim_wts<-data.frame(ticker,optim_wts)

# Plot the optimal portfolio
ggplot(data = optim_wts, mapping = aes(x = reorder(ticker,optim_wts), y = optim_wts,fill=ticker)) +
  geom_col(color="black",show.legend = FALSE) +
  geom_text(aes(label=round(optim_wts,3))) +
  xlab("Ticker") +
  ylab("Weight") +
  ggtitle(paste("Optimized Portfolio(Annualized Return ",round(returns*100,2),"%)",sep=""))