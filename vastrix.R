#   ____________________________________________________________________________
#   Nombre del proyecto                                                     ####


##  ............................................................................
##  Configure options and load functions                                    ####

library(dplyr)
library(tibble)


##  ............................................................................
##  Step 1: Load Dataset                                                    ####

url_vastrix <- 'http://talent.com.gt/IntroR/Trx_Vastrix.rda'
download.file(url = url_vastrix, destfile = 'vastrix.rda')
load('vastrix.rda')

vastrix <- as_tibble(Trx_Vastrix)
rm(Trx_Vastrix)

