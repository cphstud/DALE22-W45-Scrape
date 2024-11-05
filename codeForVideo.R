library(httr)
library(rvest)
library(dplyr)

# first page 
#https://www.bilbasen.dk/brugt/bil/bmw?fuel=1&includeengroscvr=true&includeleasing=false&pagesize=30#

startlink <- "https://www.bilbasen.dk/brugt/bil/volvo?fuel=1&includeengroscvr=true&includeleasing=false&pagesize=100"
rawres <- GET(url=startlink)
rawres$status_code
rawcontent <- httr::content(rawres,as="text")

# tranform text to html-nodes
page <- read_html(rawcontent)

# extract car-elements from startpage
carlist <- page %>% html_elements("article")

# tag-liste
ptag=".Listing_price__6B3kE"
proptag=".Listing_properties___ptWv"
mmtag="[class^='Listing_makeModel']"
dettag="[class^='Listing_details']"
dettagitem="[class^='ListingDetails_listItem']"
desctag="[class^='Listing_description']"
loctag="[class^='Listing_location']"

# dataframe til opsamling
cn=c("price","details","makemodel","props","desc","location","link","carid","scrapedate")
colldf=as.data.frame(matrix(data=NA,nrow=0,ncol = 9))
colnames(colldf)=cn

for (car in carlist) {
  tryCatch({
  price <- car %>% html_element(ptag) %>% html_text()
  props <- car %>% html_element(ptag) %>% html_text()
  makemodel <- car %>% html_element(mmtag) %>% html_text()
  #details1 <- car %>% html_element(dettag) %>% html_text()
  details <- car %>% html_elements(dettagitem) %>% html_text() %>% paste0(collapse = "_")
  description <- car %>% html_elements(desctag) %>% html_text()
  location <- car %>% html_elements(loctag) %>% html_text()
  link <- car %>% html_element("a") %>% html_attr("href") 
  carid <- link %>% str_extract("[0-9]{7}")
  tmpdf <- data.frame(price,details,makemodel,props,description,location,link,carid,Sys.time())
  colldf <- rbind(colldf,tmpdf)
  },
  error = function(cond) {
    print(makemodel)
  }
  )
}





nextlink <- "https://www.bilbasen.dk/brugt/bil/volvo?fuel=1&includeengroscvr=true&includeleasing=false&page=2"
nextlink <- "https://www.bilbasen.dk/brugt/bil/volvo?fuel=1&includeengroscvr=true&includeleasing=false&page=1"