library(rvest)
library(RSelenium)
library(dplyr)

#selenium-stuff
startlink <- 'https://www.bilbasen.dk/brugt/bil?IncludeEngrosCVR=true&PriceFrom=0&includeLeasing=false'
rD <- rsDriver(port = 4589L, browser = c('chrome'))
rclient <- rD[['client']]
rclient$navigate(startlink)
pagesource <- rclient$getPageSource()

#rvest
html <- read_html(pagesource[[1]])
