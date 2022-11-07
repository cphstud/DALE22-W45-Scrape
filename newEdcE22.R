library(rvest)
library(RSelenium)
library(dplyr)
library(RMariaDB)
library(DBI)

# create database connection

condb <- dbConnect(MariaDB(),
                   db="mybilbasen",
                   host="localhost",
                   port=3306,
                   user="root",
                   password="root123")




#selenium-stuff
startlink <- 'https://www.bilbasen.dk/brugt/bil?IncludeEngrosCVR=true&PriceFrom=0&includeLeasing=false'
startlink <- 'https://www.edc.dk/sog/?ejd-typer=1'
rD <- rsDriver(port = 4593L, browser = c('firefox'))
rclient <- rD[['client']]
rclient$navigate(startlink)

#rvest
maincartag=".propertyitem--list .media__link__wrapper"
mainhousetag=".propertyitem--list"
pagesource <- rclient$getPageSource()
househtml <- read_html(pagesource[[1]])
houselist <- househtml %>% html_nodes(mainhousetag)

testhouse = houselist[[4]]

#get id
idtag = ".propertyitem__openhouselink"
houseid <- testhouse %>% html_elements("a") %>% html_attr("href")
#houseid <- testhouse %>% html_elements(idtag)
#houseid <- testhouse %>% html_nodes("div")

#addr
addrtag =".propertyitem__address--listview"
addr <- testhouse %>% html_nodes(addrtag) %>% html_text()

#pris
pristag =".propertyitem__price"
houseprice <- testhouse %>% html_nodes(pristag) %>% html_text()

# stamdata i tabel
hdata <- testhouse %>% html_element("table") %>% html_table()


houseresdf = as.data.frame(matrix(nrow = 0, ncol = 7))
names=c("sqm","milage","maketype","id","region","year","price")
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

