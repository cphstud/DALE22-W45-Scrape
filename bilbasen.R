library(rvest)
library(RSelenium)
library(dplyr)

#selenium-stuff
startlink <- 'https://www.bilbasen.dk/brugt/bil?IncludeEngrosCVR=true&PriceFrom=0&includeLeasing=false'
#rD <- rsDriver(port = 4589L, browser = c('chrome'))
rD <- rsDriver(port = 4590L, browser = c('firefox'))
rclient <- rD[['client']]
rclient$navigate(startlink)
pagesource <- rclient$getPageSource()

#rvest
maincartag=".bb-listing-clickable"
carhtml <- read_html(pagesource[[1]])
carlist <- carhtml %>% html_nodes(maincartag)
testcar = carlist[[3]]

resdf = as.data.frame(matrix(nrow = 0, ncol = 7))
names=c("mpg","milage","maketype","id","region","year","price")
colnames(resdf) <- names

for (car in carlist) {
  tmpdf = as.data.frame(matrix(nrow = 1, ncol = 7))
  colnames(tmpdf) <- names
  idtag="data-track-content-id"
  carid <- car %>% html_attr(idtag)
  tmpdf$id=carid
  
  #get price
  prictag=".listing-price"
  price <- car %>% html_nodes(prictag) %>% html_text() 
  tmpdf$price=price
  
  # get mpg, milage, year
  infotag = ".listing-data"
  infolist <-  car %>% html_nodes(infotag) %>% html_text()
  tmpdf$year=infolist[4]
  tmpdf$milage=infolist[3]
  tmpdf$mpg=gsub("[\n ]","",infolist[2])
  
  # get maketype-info
  maketypetag = ".darkLink"
  mklist <-  car %>% html_nodes(maketypetag) %>% html_text()
  tmpdf$maketype=mklist
  
  # get region
  regtag = ".listing-region"
  region <-  car %>% html_nodes(regtag) %>% html_text()
  tmpdf$region=region
  resdf <- rbind(resdf,tmpdf)
}


