library(rvest)
library(RSelenium)
library(dplyr)

#selenium-stuff
startlink <- 'https://www.bilbasen.dk/brugt/bil?IncludeEngrosCVR=true&PriceFrom=0&includeLeasing=false'
rD <- rsDriver(port = 4591L, browser = c('firefox'))
rclient <- rD[['client']]
rclient$navigate(startlink)

#rvest
maincartag=".bb-listing-clickable"
#carhtml <- read_html(pagesource[[1]])
#carlist <- carhtml %>% html_nodes(maincartag)

resdf = as.data.frame(matrix(nrow = 0, ncol = 7))
names=c("mpg","milage","maketype","id","region","year","price")
colnames(resdf) <- names
maxlimit=3

for (counter in (1:maxlimit)) {
  pagesource <- rclient$getPageSource()
  carhtml <- read_html(pagesource[[1]])
  carlist <- carhtml %>% html_nodes(maincartag)

  for (car in carlist) {
    tmpdf = as.data.frame(matrix(nrow = 1, ncol = 7))
    colnames(tmpdf) <- names
    
    #get idtag
    carid <- getcarid(car)
    tmpdf$id=carid
    
    #get price
    price <- getcarprice(car)
    tmpdf$price=price
    
    # get mpg, milage, year
    infolist <- getinfolist(car)
    tmpdf$year=infolist[4]
    tmpdf$milage=infolist[3]
    tmpdf$mpg=infolist[2]
    
    # get maketype-info
    maketypetag = ".darkLink"
    mklist <-  car %>% html_nodes(maketypetag) %>% html_text()
    tmpdf$maketype=mklist
    
    # get region
    regtag = ".listing-region"
    region <-  car %>% html_nodes(regtag) %>% html_text()
    tmpdf$region=region
    resdf <- rbind(resdf,tmpdfd)
  }
  button=rclient$findElement(using = "class name","next")
  button$clickElement()
  Sys.sleep(5)
}


getcarid <- function(car) {
  idtag="data-track-content-id"
  carid <- car %>% html_attr(idtag)
  return(carid)
}

getinfolist <- function(car) {
  infotag <-  ".listing-data"
  infolist <- car %>% html_nodes(infotag) %>% html_text()
  infolist <- gsub("\n","",infolist)
  infolist <- gsub(" ","",infolist)
  infolist <- gsub("\\.","",infolist)
  return(infolist)
}

getcarprice <- function(car) {
  pricetag=".listing-price"
  price <- car %>% html_nodes(pricetag) %>% html_text() 
  return(price)
}

