source("R/init.R")

token <- rstudioapi::askForSecret("token")
cookies = rstudioapi::askForSecret("cookies")

make_header <- function() {
  
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
        teamname = html_nodes(.,'.teamname, .team-name') %>%  
          html_text(trim = TRUE) %>% 
          grep('[^Winner Of]',. ,value = TRUE),
        teamcode = html_nodes(.,'.teamcode, .team-code') %>% 
          html_text(trim = TRUE)
      )
    } %>% 
    map(~matrix(.x, ncol = 2, byrow = TRUE)) %>%
    map(function(x) x[!(x[,1] == '0' & x[,2] == '0'),]) # 无比分
    
 
  result = result$teamname[seq_len(nrow(result$teamcode)),] %>% 
    cbind(result$teamcode)
   
  # result = result[which(result[,1] != ''), ]
  
  dates = web %>% 
    html_nodes('.time') %>% 
    html_text(trim = TRUE) %>% 
    matrix(ncol = 2, byrow = TRUE) %>% 
    .[1:nrow(result), ]
  
  index = web %>% 
    html_nodes(".war .teams a") %>% #季军赛缺失，在 add team 中
    html_attr("href") %>%
    paste0("https://www.wanplus.com",.) %>% 
    .[1:nrow(result)]
  
  cbind(result, dates, index)
  
}

get_schedule = function(event_id = NULL) {
  
  url =  glue::glue('https://www.wanplus.com/event/{event_id}.html')
  ref = c('referer' = 'https://www.wanplus.com/')
  myheader = append(make_header(), ref)
  
  web = GET(url, add_headers(.headers = myheader))
  
  out = content(web, as = 'parsed') %>% 
    html_nodes('#event_stage li') %>% 
    {
      list(
        stageid = html_attr(.,'data-stageid'),
        name    = html_nodes(., 'a') %>% html_text()
      )
    }
  
  glue::glue("有 {length(out$name)} 个赛程：{out$name}")
  
  url = 'https://www.wanplus.com/ajax/event/shedule/detail'
  
  # length(out$stageid) == 1 ???
  
  res = list()
  
  for (i in seq_along(out$stageid)) {
    
    web = POST(
      url,
      add_headers(.headers = myheader),
      body = list(
        '_gtk' = token,
        'eId' = event_id,
        'stageId' = out$stageid[i],
        'gameType' = '2'
      )
    )
    
    res[[i]] = cbind(out$name[i],tidy_schedule(web = web))
    
    Sys.sleep(1)
    
  }
  
  reduce(res, rbind)
}

index = rio::import("data/schedules.csv")$index

# ========================

res = list()

indexs = 501:600

urls = index[indexs]

for (i in seq_along(urls)) {
  r = GET(urls[i])
  
  matchid = r %>% content() %>% html_nodes(".game a") %>% html_attr("data-matchid")
  
  url1 = glue::glue("https://www.wanplus.com/ajax/matchdetail/{matchid}?_gtk={token}")
  
  d = debugGatherer()
  h = basicHeaderGatherer()
  
  myhttpheader = c(
    "User-Agent" = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:64.0) Gecko/20100101 Firefox/64.0",
    "Referer" = urls[i],
    "Authority" = "www.wanplus.com",
    "X-CSRF-Token" = token,
    "X-Requested-With" = "XMLHttpRequest",
    "Cookie" = cookies
  )
  
  x = list()
  
  for (j in seq_along(url1)) {
    curl = getCurlHandle(httpheader = myhttpheader)
    
    td = getURL(
      url1[j],
      curl = curl,
      debugfunction = d$update,
      headerfunction = h$update
    )
    
    td = jsonlite::fromJSON(td)
    
    x[[j]] = td
    
    Sys.sleep(0.7)
    
    print(glue::glue("game{i}bo{j}--{td[['msg']]}"))
  }
  
  res[[i]] = x
  
  if ((i %% 10) == 0) {
    Sys.sleep(5)
  } else {
    Sys.sleep(1)
  }
  
}
ff = paste0(range(indexs), collapse = "_")
save.image(glue::glue("data/{ff}.RData"))
