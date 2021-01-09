# 整理赛事列表
library(tidyverse)
library(httr)
library(rvest)
library(RCurl)

make_event_url <- function(type = 0, year = 2021) {
  # para type 0:全部 1:正在进行 2:即将开始 3: 已经结束
  # year 从 2014 开始
  paste0("https://www.wanplus.com/event?t=", type, "&year=", year)
}

get_event_list <- function(event_url) {
  
  web <- httr::GET(event_url) %>%
    httr::content(as = "parsed")
  
  event_name <- rvest::html_nodes(web, ".event-title") %>%
    rvest::html_text(trim = TRUE)
  
  info <- rvest::html_nodes(web, "#info p") %>%
    rvest::html_text(trim = TRUE) %>% 
    matrix(ncol = 2, byrow = TRUE) %>% 
    `colnames<-`(c("status", "duration"))
  
  link <- rvest::html_nodes(web, ".event-list ul li a") %>% 
    rvest::html_attr("href")

  res <- data.frame(event_name = event_name, link = link) %>% cbind(info)
  
  res
  
}

# test = make_event_url(year = 2014) %>% 
#   get_event_list()

get_all_event <- function(event_url) {
  
  web <- httr::GET(event_url) %>%
    httr::content(as = "parsed")
  
  max_page <- rvest::html_nodes(web, ".pages a") %>%
    rvest::html_text(trim = TRUE) %>% 
    as.integer() %>% 
    max()
  
  out <- list()
  
  for (i in seq_len(max_page)) {
    
    event_url <- ifelse(i == 1, event_url, paste0(event_url, "&page=", i))
    
    res <- get_event_list(event_url)
    
    out[[i]] <- res
    
    Sys.sleep(1)
    
  }
  
  purrr::reduce(out, rbind)
  
}
