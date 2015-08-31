
library(httr)


########## open API connection

credentials <- readLines("user_rav.txt")
names(credentials) <- c("user","access_key","secret_key")

open_connection <- function(credentials){
  # open connection to Ravelry API and return token
  reqURL <- "https://www.ravelry.com/oauth/request_token"
  accessURL <- "https://www.ravelry.com/oauth/access_token"
  authURL <- "https://www.ravelry.com/oauth/authorize"
  
  ravelry.app <- oauth_app("ravelry", key=credentials["access_key"], 
                           secret=credentials["secret_key"])
  ravelry.urls <- oauth_endpoint(reqURL, authURL, accessURL)
  
  return(oauth1.0_token(ravelry.urls, ravelry.app))
}

test_connection <- function(ravelry.token){
  #quick test of API connection by getting connected user info
  test <- GET("https://api.ravelry.com/current_user.json", 
              config=config("token"=ravelry.token)) 
  print(content(test)$user$username)
}

ravelry.token <- open_connection(credentials)
test_connection(ravelry.token)
