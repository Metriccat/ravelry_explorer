# For testing Cron scheduler

sayHello <- function(){
  print('hello')
}

sayHello()

write("hello", "/home/saskia/R/ravelry_explorer/crontest.txt", append=T)
