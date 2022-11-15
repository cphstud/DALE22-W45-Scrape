library(rvest)
library(RSelenium)
library(dplyr)
library(stringr)
library(logr)
library(colorfindr)

# logfile
tmp <- file.path("./logs","bb.log")
lf <- log_open(tmp)

#selenium-stuff
rD <- rsDriver(port = 4598L, browser = c('firefox'))
rclient <- rD$client
rclient$navigate(startlink)


#cookies - not working. Must click manually
#rclient$findElement(using = "xpath",'//button[cotains(text(),"Tillad alle"')
#allc <- rclient$getAllCookies()
#saveRDS(allc,"mycookies.rds")

maincartag=".bb-listing-clickable"
nresdf = data.frame(matrix(ncol=13, nrow=0))
nresnames=c("id","dealer","mpg","milage","year","price","nn","make","type","maketype","region","link","scrapedate")
colnames(nresdf) = nresnames

# how many times (x32)
maxlimit=10
stableTime=3

#base-link for paging
baselooplink='https://www.bilbasen.dk/brugt/bil/ford?make=toyota&make=volvo&Fuel=1&PriceFrom=0&ZipCode=0000&IncludeEngrosCVR=True&Seller=1&IncludeSellForCustomer=True&IncludeWithoutVehicleRegistrationTax=True&IncludeLeasing=False&IncludeCallForPrice=False&HpFrom=&HpTo=&page='

#navigate to page. Remember manually accept cookies
rclient$navigate(baselooplink)

for (counter in (1:maxlimit)) {
  log_print(c("counter: ",counter))
  tmplink <- paste0(baselooplink,counter)
  carhtml <- read_html(tmplink)
  Sys.sleep(stableTime)
  carlist <- carhtml %>% html_nodes(maincartag)
  
  for (car in carlist) {
    # tmp-datframe for collecting car-data
    tmpdf = as.data.frame(matrix(nrow = 1, ncol = 7))
    colnames(tmpdf) <- names
    
    # get link
    tlink <- gettlink(car)
    tmpdf$link=tlink
    
    #get idtag
    carid <- getcarid(car)
    tmpdf$id=carid
    
    #get price
    price <- getcarprice(car)
    tmpdf$price=as.numeric(price)
    
    # get mpg, milage, year
    infolist <- getinfolist(car)
    #infolist <- gsub("[^0-9,]","",infolist)
    log_print(infolist)
    tmpdf$year=as.numeric(infolist[4])
    tmpdf$milage=as.numeric(infolist[3])
    tmpg=gsub("[^0-9,]","",infolist[2])
    tmpdf$mpg=gsub(",",".",tmpg)
    
    # get maketype-info
    maketypetag = ".darkLink"
    mklist <-  car %>% html_nodes(maketypetag) %>% html_text()
    tmpdf$maketype=mklist
    carv=str_split(mklist," ")
    make=carv[[1]][1]
    type=carv[[1]][2]
    tmpdf$make <- make
    tmpdf$type <- type
    
    # get region
    regtag = ".listing-region"
    region <-  car %>% html_nodes(regtag) %>% html_text()
    tmpdf$region=region
    
    # get dealer
    dealer=savelogo(car)
    tmpdf$dealer=dealer
    
    nresdf <- rbind(nresdf,tmpdf)
  }
  counter=counter+1
}

# save dataframe to file - RDS-format
saveRDS(nresdf,"firsbb.rds")

log_close()

getcarid <- function(car) {
  idtag="data-track-content-id"
  carid <- car %>% html_attr(idtag)
  return(carid)
}

getinfolist <- function(car) {
  infotag <-  ".listing-data"
  infolist <- car %>% html_nodes(infotag) %>% html_text()
  log_print(c("RAW: ",infolist))
  infolist <- gsub("\n","",infolist)
  infolist <- gsub(" ","",infolist)
  infolist <- gsub("\\.","",infolist)
  return(infolist)
}

getcarprice <- function(car) {
  pricetag=".listing-price"
  price <- car %>% html_nodes(pricetag) %>% html_text() 
  price <- gsub("[^0-9]","",price)
  return(price)
}
savelogo <- function(s) {
  #src = s %>% html_nodes(".listing-dealer-logo-sm") %>% html_attr("src")
  retval="NA"
  src = s %>% html_nodes(".listing-dealer-logo-sm") %>% html_attr("data-echo")
  log_print(c("S:",src))
  #src="https://billeder.bilbasen.dk/bilinfo/70da5fa5-6af4-45c0-b207-d85ca4de0caf.jpg?class=S400X400"
  tryCatch(
    {
      dealerm=str_match(src,"\\/([a-z0-9-]+)\\.jpg")
      log_print(c("DEAL: ",dealerm))
      retval=dealerm[1,2]
      dfile=paste0("./img/",str_match(src,"[0-9a-z-]+\\.jpeg"))
      #download.file(src, dfile, mode = "wb")
    },
    error=function(e) {
      log_print(e)
    }
  )
  return(retval)
}

gettlink <- function(car) {
 tlink <- car %>% 
      html_nodes(".darkLink") %>% 
      html_attr("href") %>% 
      paste0("https://bilbasen.dk",.)
 return(tlink)
}


# for manual test
testbaselooplink='https://www.bilbasen.dk/brugt/bil/Ford?make=Toyota&make=Volvo&IncludeEngrosCVR=true&PriceFrom=0&includeLeasing=false&Fuel=1&IncludeCallForPrice=false&page=1'
rclient$navigate(testbaselooplink)
pagesource <- rclient$getPageSource()
testcarhtml <- read_html(pagesource[[1]])
testcarlist <- testcarhtml %>% html_nodes(maincartag)
testcar = testcarlist[[4]]
