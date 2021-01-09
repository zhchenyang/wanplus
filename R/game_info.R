source("R/init.R")

token <- rstudioapi::askForSecret("token")

make_header <- function() {
  
  cookies = rstudioapi::askForSecret()
  
  myheader = c(
    "User-Agent" = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:64.0) Gecko/20100101 Firefox/64.0",
    "Authority" = "www.wanplus.com",
    "X-Requested-With" = "XMLHttpRequest",
    'origin' = 'https://www.wanplus.com',
    'cookie' = cookies
  )
}


tidy_schedule = function(web) {
  
  web = web %>% content(as = 'parsed') 
  
  result = web %>% 
    {
      list(
        teamname = html_nodes(.,'.teamname, .team-name') %>%  ## '-' ??
          html_text(trim = TRUE) %>% 
          grep('[^Winner Of]',. ,value = TRUE),
        teamcode = html_nodes(.,'.teamcode, .team-code') %>% 
          html_text(trim = TRUE)
      )
    } %>% 
    map(~matrix(.x, ncol = 2, byrow = TRUE)) %>%
    map(function(x) x[!(x[,1] == '0' & x[,2] == '0'),]) %>% 
    reduce(cbind)
  
  result = result[which(result[,1] != ''), ]
  
  dates = web %>% 
    html_nodes('.time') %>% 
    html_text(trim = TRUE) %>% 
    matrix(ncol = 2, byrow = TRUE)
  
  index = web %>% 
    html_nodes(".war .teams a") %>% 
    html_attr("href") %>%
    paste0("https://www.wanplus.com",.) %>% 
    .[1:nrow(result)]
  
  cbind(result, dates, index)
  
}
