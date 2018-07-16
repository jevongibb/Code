library(devtools)
library(rjson)
library(RCurl)
library(blsAPI)






blsAPI <- function(data=NA){ 
  require(rjson) 
  require(RCurl) 
  h = basicTextGatherer() 
  h$reset() 
  if(is.na(data)){ 
    message('blsAPI: No parameters specified.') 
  } 
  else{ 
  ## Parameters specified so make the request 
    if(is.list(data)){ 
    ## Multiple Series or One or More Series, Specifying Years request 
    curlPerform(url='https://api.bls.gov/publicAPI/v1/timeseries/data/', 
      httpheader=c('Content-Type' = "application/json;"), 
      postfields=toJSON(data), 
      verbose = FALSE,  
      writefunction = h$update 
      ) 
  }else{ 
    ## Single Series request 
    curlPerform(url=paste0('https://api.bls.gov/publicAPI/v1/timeseries/data/',data), 
      verbose = FALSE,  
      writefunction = h$update 
      ) 
    }
  } 
  h$value()
}

response <- blsAPI('ENS13121105') 
json <- fromJSON(response)









payload <- list('seriesid'=c('LAUCN040010000000005','LAUCN040010000000006'), 'startyear'='2010', 'endyear'='2012') 
response <- blsAPI(payload) 
json <- fromJSON(response)