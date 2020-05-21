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
library(shinythemes)
#===================

# Given the list of tickers, Number of Years of Historical Data, and whether
# or not to allow for short selling, this function launches a Selenium Server
# to scrape Yahoo Finance for the requested data. 
#============================================================================
scrape <- function(input){
  #List of Tickers 
  ticker <- as.character(strsplit(input$tickers, ",")[[1]])
  
  #Grab TimeStamp
  rn <- as.integer(as.POSIXct( Sys.time() ))
  
  #start RSelenium
  rD <- rsDriver(browser="chrome",chromever = input$chrome)
  remDr <- rD[["client"]]
  remDr$open()
  
  #navigate to the yahoo finance page of first ticker
  remDr$navigate(paste("https://finance.yahoo.com/quote/",
                       ticker[1],
                       "/history?period1=",
                       as.character(rn - (input$years * 31536600) ),
                       "&period2=",
                       as.character(rn),
                       "&interval=1d&filter=history&frequency=1d",sep=""))
  
  #scroll down 27 times, waiting for the page to load at each time
  for(i in 1:(3*input$years)){      
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
                         as.character(rn - (input$years * 31536600)),
                         "&period2=",
                         as.character(rn),
                         "&interval=1d&filter=history&frequency=1d",sep=""))
    
    #scroll down 27 times, waiting for the page to load at each time
    for(i in 1:(3*input$years)){      
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
  
  return(wr_hist_data)
}
#============================================================================


# Given the historical data, the list of tickers, and whether or not to allow
# for short selling, this function solves for the portfolio weights with the 
# highest Sharpe Ratio. 
#============================================================================
optimize <- function(wr_hist_data,short,ticker) {
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
  con <- con %>% rbind(diag(length(ticker))*-1)
  
  if (short == FALSE){
    con <- con %>% rbind(diag(length(ticker))* 1)
    solution <- constrOptim(theta=rand_wts,
                            f=fr,
                            grad=NULL,
                            ui=con,
                            ci=c(-1,rep(c(-1),length(ticker)),rep(c(0),length(ticker)))-1e-6,
                            control=list(fnscale = -1))
  }else{
    solution <- constrOptim(theta=rand_wts,
                            f=fr,
                            grad=NULL,
                            ui=con,
                            ci=c(-1,rep(c(-1),length(ticker)))-1e-6,
                            control=list(fnscale = -1))
  }
  
  #Create dataframe by combining ticker and optimal weights vectors
  optim_wts<-data.frame(ticker,solution$par)
    
  return(optim_wts)
}
#============================================================================


# Given the historical data, the list of tickers, and whether or not to allow
# for short selling, this function determines the Annualized Returns, Portfolio
# Variance, and Sharpe Ratio of the optimal portfolio
#============================================================================
portfolio_stats <- function(wr_hist_data,short,ticker){
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
  
  if (short == FALSE){
    con<- con %>% rbind(diag(length(ticker))* 1)
    solution <- constrOptim(theta=rand_wts,
                            f=fr,
                            grad=NULL,
                            ui=con,
                            ci=c(-1,rep(c(-1),length(ticker)),rep(c(0),length(ticker)))-1e-6,
                            control=list(fnscale = -1))
  }else{
    solution <- constrOptim(theta=rand_wts,
                            f=fr,
                            grad=NULL,
                            ui=con,
                            ci=c(-1,rep(c(-1),length(ticker)))-1e-6,
                            control=list(fnscale = -1))
  }
  
  optim_wts <- solution$par
  #Calculate Annualized Returns with Optimized Weights
  returns <- (sum(optim_wts * avg_monthly_ror) + 1)^12 - 1
  
  #Calculate Portfolio Variance with Optimized Weights
  risk <- sqrt(t(optim_wts) %*% (covar %*% optim_wts))
  
  #Calculate Sharpe Ratio with Optimized Weights
  sharpe<- returns/risk
  
  table <- data.frame("Annualized Returns" = c(paste(round(returns*100,2),"%",sep="")),
                      "Portfolio Variance" = round(risk,2),
                      "Sharpe Ratio"       = round(sharpe,2))
  return(table)
}
#============================================================================



# ui 
ui <- fluidPage(
  theme = shinytheme("united"),
  
  h1("Markowitz Portfolio Optimization"),
  
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "chrome",
                label = "Chrome/Chrome Driver Version",
                value = "81.0.4044.138")
      ,
      textInput(inputId = "tickers",
            label = "Tickers",
            value = "AMZN,MSFT,FB" )
      ,
      sliderInput(inputId = "years",
                  label = "Years of Historical Data",
                  min = 5,
                  max = 12,
                  value = 10)
      ,
      checkboxInput(inputId = "short",
            label = "Enable Short Selling",
            value = FALSE)
      ,
      actionButton(inputId = "go",
            label = "Go" )
      ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs"
                  , tabPanel("Optimized Portfolio Weights", value = 1, plotOutput(outputId = "bar1"))
                  , tabPanel("Portfolio Statistics", tableOutput("table1"))
      )
    )
  )
)


# server
server <- function(input,output){
  
  hist_data <- eventReactive(input$go, {
    data <- scrape(input)
  })
  
  opt_wts<- reactive({
    data<- optimize(hist_data(),input$short,as.character(strsplit(input$tickers, ",")[[1]])) %>%
      rename(optim_wts = `solution.par`)
  })
  
  output$bar1 <- renderPlot({
    ggplot(data = opt_wts(), mapping = aes(x = reorder(ticker,optim_wts), y = optim_wts,fill=ticker)) +
      geom_col(color="black",show.legend = FALSE) +
      geom_text(aes(label=round(optim_wts,3))) +
      xlab("Ticker") +
      ylab("Weight") +
      ggtitle("Optimized Portfolio")
  })
  
  output$table1 <- renderTable({
    portfolio_stats(hist_data(),input$short,as.character(strsplit(input$tickers, ",")[[1]]))
  })
  
}

# call to shinyApp
shinyApp(ui = ui, server = server)
