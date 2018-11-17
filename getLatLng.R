library(httr)   #install.packages("httr")
library(rjson)  #install.packages("rjson")
getLatLng <- function(address){
  
  urlData <- GET(paste0("https://maps.googleapis.com/maps/api/geocode/json?key=YOUR_KEY&language=zh-TW&address=",  #YOUR_KEY = google API憑證
                        address))
  
  jsonResult <- rjson::fromJSON(rawToChar(urlData$content))
  #   Sys.sleep(1) #延遲一秒避免被GOOGLE擋
  if (jsonResult$status != "OK"){
    print("Google geocode API Error !")
    return("error")
  }
  print("LateLng Got")
  lat <<- jsonResult$results[[1]]$geometry$location$lat
  lng <<- jsonResult$results[[1]]$geometry$location$lng
  
  return(paste(lat, lng, sep=","))
}