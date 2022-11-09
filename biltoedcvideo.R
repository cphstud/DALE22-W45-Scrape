library(shadowr)
library(rvest)
library(RSelenium)
library(dplyr)
library(stringr)
library(RMariaDB)
library(DBI)
library(colorfindr)
library(logr)

# create database connection

condb <- dbConnect(MariaDB(),
                   db="mybilbasen",
                   host="localhost",
                   port=3306,
                   user="root",
                   password="root123")

# logfile
tmp <- file.path(tempdir(),"bb.log")
lf <- log_open(tmp)

#selenium-stuff
startlink <- 'https://www.edc.dk/sog/?ejd-typer=1'
rD <- rsDriver(port = 4539L, browser = c('firefox'))
rclient <- rD[['client']]
rclient$navigate(startlink)
houspsource <- rclient$getPageSource()


#rvest
basepaginglink <- 'https://www.edc.dk/sog/?ejd-typer=1&side='
endpageinglink <- '#lstsort'
mainhousetag=".propertyitem__wrapper"
totaldf = data.frame(matrix(ncol=13, nrow=0))
housedfnames=c("m2","Grund","Rum","byggeaar","Liggetid", "plusminus","prism2","Ejerudgprmd","Sagsnr","Pris", "Solgt","Addr","Link")
colnames(totaldf) = housedfnames

maxlimit=3

#testhouse = houselist[[4]]

#rclient$navigate(startlink)

for (counter in (1:maxlimit)) {
  tmplink <- paste0(basepaginglink,counter,endpageinglink)
  log_print(c("link: ",tmplink))
  househtml <- read_html(tmplink)
  houselist <- househtml %>% html_nodes(mainhousetag)
  #pagesource <- rclient$getPageSource()
  #carhtml <- read_html(pagesource[[1]])
  #print(tmplink)
  Sys.sleep(3)
  
  for (house in houselist) {
    #tmpdf = as.data.frame(matrix(nrow = 1, ncol = 13))
    #colnames(tmpdf) <- housenames
    #house=testhouse
    housedf <- gethouselist(house)
    # get link
    linktag =".propertyitem__openhouselink"
    tlink <- house %>% 
      html_nodes(linktag) %>% 
      html_attr("href") %>% 
      paste0("https://edc.dk",.)
    
    housedf$link=tlink
    
    #get sagsnr
    sagsnr <- getsagsnr(house)
    housedf$id=sagsnr
    
    #get price
    houseprice <- gethouseprice(house)
    housedf$price=as.numeric(houseprice)
    
    # get region
    regtag = ".propertyitem__address--listview"
    address <-  house %>% html_nodes(regtag) %>% html_text()
    housedf$address=address
    
    # get dealer
    tryCatch({  totaldf <- rbind(totaldf,housedf) },
             error = function(e) {print(e)} )
  }
  #button=rclient$findElement(using = "class name","next")
  #button$clickElement()
  counter=counter+1
}

# save dataframe to file - RDS-format
saveRDS(nresdf,"firsbb.rds")


# save dataframe to mysql - first time only


dbWriteTable(condb,"mycars2",nresdfbu,overwrite=T)
#dbWriteTable(con, "mtcars", datasets::mtcars, overwrite = TRUE)

# get cars from db
#fetchcars=dbSendQuery(condb,"Select * from cars")
resdb=dbGetQuery(condb,"Select * from cars")



getsagsnr <- function(house) {
  hpattern='=([0-9]+)&'
  resm=str_match(tlink,hpattern)
  log_print(c("Sagsnr: ",resm))
  sagsnr = resm[1,2]
  return(sagsnr)
}

gethouselist <- function(house) {
    infolist = as.data.frame(matrix(nrow = 1, ncol = 8))
    colnames(infolist) <- nnd
  tryCatch({
      infolist <- house %>% html_nodes("table") %>% html_table() %>% as.data.frame()
      colnames(infolist) <- nnd
      log_print(c("RAW: ",infolist))
      infolist <- as.data.frame(lapply(infolist, function(x) gsub("[^0-9]","",x)))
    },
    error = function(e) { log_print(e) }
  )
  return(infolist)
}

gethouseprice <- function(house) {
  pricetag=".propertyitem__price"
  price <- house %>% html_nodes(pricetag) %>% html_text() 
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
