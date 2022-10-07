
## PACOTES UTILIZADOS

library(RSelenium)

## Especifica a função

windowSwitch <- function (remDr, windowId){
  
  qpath <- sprintf("%s/session/%s/window", 
                   remDr$serverURL, 
                   remDr$sessionInfo[["id"]])
  
  remDr$queryRD(qpath, "POST", 
                qdata = list(handle = windowId))

  }