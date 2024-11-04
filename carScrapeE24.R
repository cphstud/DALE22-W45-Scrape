  library(logr)
  library(rvest)
  library(RSelenium)
  library(dplyr)
  library(stringr)
  library(httr)
  library(DBI)
  library(RMariaDB)
  library(RMySQL)
  
  
  # Establish a connection to your MySQL database
  # Connect to the local shop database
  conms <- dbConnect(MySQL(),
                   db = "bilbasen", 
                   host = "localhost", 
                   port = 3306,
                   user = "root",
                   password = "OttoRehagel123",
                   local_infile = TRUE)
  
  # logfile
  f=getwd()
  
  # get cookie
  # Define headers
  headers <- c(
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36",
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
    "Accept-Language" = "en-US,en;q=0.5",
    "Accept-Encoding" = "gzip, deflate, br",
    "Connection" = "keep-alive",
    "Referer" = "https://google.com"
  )
  startlink="https://www.bilbasen.dk/brugt/bil/skoda?includeengroscvr=true&includeleasing=false&page=1"
  startlink="https://www.bilbasen.dk/brugt/bil/skoda?includeengroscvr=true&includeleasing=false"
  initial_response <- GET(startlink, add_headers(.headers = headers))
  initial_response$status_code
  cookies <- cookies(initial_response)
  
  
  page=content(initial_response,as="text")
  pagehtml=read_html(page)
  carlist=pagehtml %>% html_elements("div")
  #tag
  tag="Listing_listing__XwaYe"
  carlist=pagehtml %>% html_elements("article")
  colldf=as.data.frame(data=NA,nrow=30,ncol=10)
  
  # next page
  nlink="https://www.bilbasen.dk/brugt/bil/skoda?includeengroscvr=true&includeleasing=false&page=2"
  cookie_name <- "bbtracker"
  cookie_value <- "id=4665f5ad-3a6c-4977-a105-faf8b0a81ebd"
  response <- GET(nlink, set_cookies(.cookies = c(bbtracker = cookie_value)))
  response$status_code
  page=content(response,as="text")
  pagehtml=read_html(page)
  carlist2=pagehtml %>% html_elements("article")
  totcarlist=c(carlist,carlist2)
  
  for (car in totcarlist) {
    
  }
  
  
  
  
  
  
  # log stuff
  
  logname=paste0("log_",as.numeric(as.POSIXct(Sys.Date())),".txt")
  tmp <- file.path(f, logname)
  lf <- log_open(tmp)
  log_print(date())
  
  #selenium-stuff
  rD$server$stop()
  rD <- rsDriver(port = 4598L, browser = 'firefox')
  rclient <- rD$client
  
  #rclient$closeall()
  #rclient$closeServer()
  
  # carlist
  
  listofurls=list(
    "urlvwbz" = "https://www.bilbasen.dk/brugt/bil/vw?fuel=1&includeengroscvr=true&includeleasing=false",
    "urlvwel"="https://www.bilbasen.dk/brugt/bil/vw?fuel=3&includeengroscvr=true&includeleasing=false",
    "urlmercbc"="https://www.bilbasen.dk/brugt/bil/mercedes?fuel=1&includeengroscvr=true&includeleasing=false"
    
  )
  
  #"urlaudi"="https://www.bilbasen.dk/brugt/bil/audi?includeengroscvr=true&includeleasing=false&page=",
  #"urlford"="https://www.bilbasen.dk/brugt/bil/ford?includeengroscvr=true&includeleasing=false&page=",
  #"urlskoda"="https://www.bilbasen.dk/brugt/bil/skoda?includeengroscvr=true&includeleasing=false&page="
  #"urlford"="https://www.bilbasen.dk/brugt/bil/ford?includeengroscvr=true&includeleasing=false&page=",
  #"urlbmw"="https://www.bilbasen.dk/brugt/bil/bmw?includeengroscvr=true&includeleasing=false&page=",
  #"urlkia"="https://www.bilbasen.dk/brugt/bil/kia?includeengroscvr=true&includeleasing=false&page="
  
  for (url in names(listofurls)) {
  #startlink="urlskoda"="https://www.bilbasen.dk/brugt/bil/skoda?includeengroscvr=true&includeleasing=false&page="
  #searchmake="skoda"
    searchmake=gsub("url","",url)
    startlink=listofurls[[url]]
    
  #startlink='https://www.bilbasen.dk/brugt/bil/ford?make=toyota&make=volvo&Fuel=1&PriceFrom=0&ZipCode=0000&IncludeEngrosCVR=True&Seller=1&IncludeSellForCustomer=True&IncludeWithoutVehicleRegistrationTax=True&IncludeLeasing=False&IncludeCallForPrice=False&HpFrom=&HpTo=&page=1'
  #startlink='https://www.bilbasen.dk/brugt/bil?includeengroscvr=true&includeleasing=false&page=1'
  firstlink=paste0(startlink,"1")
  log_print(firstlink)

# first run
  #firsvisit='https://www.bilbasen.dk/brugt/bil?includeengroscvr=true&includeleasing=false&page=1'
  #rclient$navigate(firsvisit)
  rclient$navigate(startlink)
  log_print("Navigate to start")
  Sys.sleep(runif(1, 1, 5))
# get source
  page=rclient$getPageSource()
  Sys.sleep(runif(1, 1, 5))
  pagecode=read_html(page[[1]])
  
  # get the number of cars
  tmpnum <- pagecode %>% html_nodes("[class*='CarAbundance_availableCars']") %>% html_text()
  numofc <- gsub("[^0-9]","",tmpnum) %>% as.numeric()
  log_print(paste("Num of cars: ",numofc))
  
  # get the number of pages
  numofp <- pagecode %>% html_node('span[data-e2e="pagination-total"]') %>% html_text() %>% as.numeric()
  log_print(paste("Num of pages: ",numofp))
  
  # get first page stuff
  prices <- pagecode %>% html_nodes("[class*='Listing_price']") %>% html_text() 
  makemodel <- pagecode %>% html_nodes("[class*='Listing_makeModel']") %>% html_text()
  details <- pagecode %>% html_nodes("[class*='Listing_details']") %>% html_text()
  props <- pagecode %>% html_nodes("[class*='Listing_properties']") %>% html_text()
  desc <- pagecode %>% html_nodes("[class*='Listing_description']") %>% html_text()
  loc <- pagecode %>% html_nodes("[class*='Listing_location']") %>% html_text()
  refs <- pagecode %>% html_elements("article") %>% html_node("a:first-child") %>% html_attr("href")
  carid=unlist(lapply(refs, function(x) str_extract(x,"[0-9]{7}")))
  Sys.Date()
  
  allcardata = data.frame(
    price=gsub("[^0-9]","", prices) %>% as.numeric(),
    make_model=makemodel,
    details=details,
    properties=props,
    location=loc,
    refs=refs,
    searchstring=searchmake,
    car_id=as.numeric(carid),
    scrape_date=Sys.Date(),
    url=nextlink
  )
  allTotalcardata=allcardata

# find articles
#carlist=pagecode %>% html_elements("article")
  for (i in (2:(numofp-1))) {
    nextlink=paste0(startlink,i)
    log_print(paste("In loop: ",i," with link: ",nextlink ))
    rclient$navigate(nextlink)
    log_print(paste("In loop: ",i," and navigate"))
    Sys.sleep(runif(1, 2, 5))
    page=rclient$getPageSource()
    Sys.sleep(runif(1, 1, 3))
    pagecode=read_html(page[[1]])
    log_print(paste("In loop: ",i," read page"))
  # get source
  
  prices <- pagecode %>% html_nodes("[class*='Listing_price']") %>% html_text()
  make_model <- pagecode %>% html_nodes("[class*='Listing_makeModel']") %>% html_text()
  details <- pagecode %>% html_nodes("[class*='Listing_details']") %>% html_text()
  props <- pagecode %>% html_nodes("[class*='Listing_properties']") %>% html_text()
  desc <- pagecode %>% html_nodes("[class*='Listing_description']") %>% html_text()
  loc <- pagecode %>% html_nodes("[class*='Listing_location']") %>% html_text()
  refs <- pagecode %>% html_elements("article") %>% html_node("a:first-child") %>% html_attr("href")
  id=unlist(lapply(refs, function(x) str_extract(x,"[0-9]{7}")))
  price=lapply(prices,function(x) gsub("[^0-9]","", x))
  
  allcardata = data.frame(
    price=price %>% as.numeric(),
    make_model=make_model,
    details=details,
    properties=props,
    location=loc,
    refs=refs,
    searchstring=searchmake,
    car_id=as.numeric(carid),
    scrape_date=Sys.Date(),
    url=nextlink
  )
  allTotalcardata=rbind(allTotalcardata,allcardata)
  log_print(paste("In loop: ",i," finish with rbind"))
  }
  log_print(paste("Now finish ", searchmake))
# Separate car_data and price_history dataframes
price_history <- allTotalcardata %>% 
  select(car_id, price, scrape_date) %>% 
 rename(date = scrape_date)

car_data_clean <- allTotalcardata %>% 
  select(car_id, make_model, details, properties, location, refs, scrape_date, url, searchstring) %>% 
  rename(id = car_id)
  #       properties = Properties, location = Location, refs = Refs, 
  #       scrape_date = Scrapedate, url = url)

# Get the old data from sql
#searchmake
#getOldq <- paste0('select id from car_data where url Like "%',unlist(searchmake),'%"')
#getOldq <- paste0('select * from price_history where car_id in  (select id from car_data where url Like "%',unlist(searchmake),'%")')
#getOldq
#oldC <- dbGetQuery(conms,getOldq)
#str(oldC)

#res=anti_join(oldC,price_history)
#str(price_history)
#str(oldC)
dbWriteTable(conms, "car_data", car_data_clean, append = TRUE, row.names = FALSE)
dbWriteTable(conms, "price_history", price_history, append = TRUE, row.names = FALSE)
log_print("Writing pricehistory and car_data")
  
  #selenium-stuff
  rD$server$stop()
  rD <- rsDriver(port = 4598L, browser = 'firefox')
  Sys.sleep(10)
  rclient <- rD$client
  
}
