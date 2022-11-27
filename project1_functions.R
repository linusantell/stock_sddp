setwd("C:/Users/Antell/Desktop/projects/stocks")
library(magrittr)
library(data.table)
library(ggplot2)
library(lubridate)
library(quantmod)
library(rvest)
library(rstan)
library(stringi)
library(gridExtra)
library(matrixcalc)
library(rpart)
library(rpart.plot)

options("datatable.print.nrows" = 5)

grid_figs <- function(fig_list, ncols=NULL,nrows=NULL, WIDTH = NULL){
  do.call("grid.arrange",
          c(fig_list, ncol = ncols,nrow = nrows, widths = WIDTH))
}

returns <- function(X, lag = 0){
  if(lag>0){
    100*(X-shift(X, n=lag))/shift(X, n=lag)
  }else{
    100*(X-shift(X))/shift(X)
  }
}

log_returns <- function(X, lag = 0){
  if(lag == 0){
    log(X/(shift(X)))
  }else{
    log(X/(shift(X, n = lag)))
  }
}

update_log <- function(downloads){
  if(downloads[download == today()] %>% length == 0){
    ans <- rbind(
      downloads , 
      data.table(
        download = today() %>% as.character() 
      )
    ) 
    ans %>% 
      write.csv("download_log.csv", row.names = F)
    ans
  }else{
    downloads
  }
}

qsave <- function(xts){
  var_name. <- as.character(substitute(xts))
  getSymbols(var_name., src = "yahoo", env = .GlobalEnv)
  
  ans <- xts %>% as.data.table 
  
  c_names <- ans %>% colnames %>% .[2:7]
  ans %>%
    setnames(old=c_names, new = c_names %>%
               stri_replace_all(replacement="", 
                                fixed = paste(
                                  var_name.,
                                  ".",
                                  sep = ""
                                )
               ))
  
  
  ans %>%
    .[, date.dt := index] %>%
    .[, index := .I] %>%
    .[, ticker := var_name.]
  
  
  file_name. <- paste(today() %>% as.character(), "/",var_name.,".csv",sep="")
  ans %>% write.csv(file_name., row.names = F)
  
  assign(var_name., ans, envir = .GlobalEnv)
}

qread <- function(ticker){
  var_name. <- as.character(substitute(ticker))
  latest_download <- get("latest_download", envir = .GlobalEnv)
  file_name. <- paste(latest_download,"/",var_name.,".csv",sep="")
  if(file_name. %>% file.exists()){
    ans <- read.csv(file_name.) %>%
      as.data.table  
    assign(var_name., ans, envir = .GlobalEnv)
  }else{
    print(file_name. %>% paste("does not exist!"))
  }
}


open_plot <- function(input.dt, lag = F, leq = T,
                      start_date = "2017-01-01",
                      end_date = "2018-01-01",
                      BY=0.5){
  
  dt <- input.dt %>% copy %>%
    .[date.dt <= end_date & date.dt >= start_date] %>%
    .[, Open.r := Open %>% returns] %>%
    .[, Close.r.l := Close %>% returns %>% shift] %>%
    .[, Adj.r.l := returns(Adjusted, lag=7) %>% shift ]
  
  p1 <- dt %>%
    ggplot(aes(Close.r.l, Open.r))+geom_point()+
    ggtitle(dt[1, ticker]%>% paste("All")) +
    scale_x_continuous(breaks = seq(-100,100,by=BY))
  
  q1 <- dt %>% ggplot(aes(Open.r)) + geom_histogram() +
    scale_x_continuous(breaks = seq(-100,100,by=BY))
  
  p2 <- dt %>%
    .[Adj.r.l < 0] %>%
    ggplot(aes(Close.r.l, Open.r))+geom_point()+
    ggtitle("High < 0") +
    scale_x_continuous(breaks = seq(-100,100,by=BY))
  
  q2 <- dt %>%
    .[Adj.r.l < 0] %>%
    ggplot(aes(Open.r)) + geom_histogram() +
    scale_x_continuous(breaks = seq(-100,100,by=BY))
  
  p3 <- dt %>%
    .[Adj.r.l > 0] %>%
    ggplot(aes(Close.r.l, Open.r))+geom_point()+
    ggtitle("High > 0") +
    scale_x_continuous(breaks = seq(-100,100,by=BY))
  
  q3 <- dt %>%
    .[Adj.r.l > 0] %>%
    ggplot(aes(Open.r)) + geom_histogram() +
    scale_x_continuous(breaks = seq(-100,100,by=BY))
  
  
  grid_figs(list(p1,q1,p2,q2,p3,q3),ncols=2)
}


series_plot <- function(input.dt){
  
  input.dt %>%
    ggplot(aes(date(date.dt), Adjusted))+geom_point()+geom_line()
  
}








