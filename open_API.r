# Functions to get Ravelry API token and test connection.

library(httr)

# user_rav.txt contains API username and password 
credentials <- readLines("user_rav.txt")
names(credentials) <- c("user","access_key","secret_key")

OpenConnection <- function(credentials){
  # Args: login info for the Ravelry API
  # Returns oauth token
  # Open connection to Ravelry API and return token
  reqURL <- "https://www.ravelry.com/oauth/request_token"
  accessURL <- "https://www.ravelry.com/oauth/access_token"
  authURL <- "https://www.ravelry.com/oauth/authorize"
  
  ravelry.app <- oauth_app("ravelry", key=credentials["access_key"], 
                           secret=credentials["secret_key"])
  ravelry.urls <- oauth_endpoint(reqURL, authURL, accessURL)
  
  return(oauth1.0_token(ravelry.urls, ravelry.app))
}

# Quick test of API connection by getting connected user info
TestConnection <- function(ravelry.token) {
  # Arg: API token
  # Returns name of the user connected with this token
  test <- GET("https://api.ravelry.com/current_user.json", 
              config=config("token"=ravelry.token)) 
  print(content(test)$user$username)
}

ravelry.token <- OpenConnection(credentials)
TestConnection(ravelry.token)

