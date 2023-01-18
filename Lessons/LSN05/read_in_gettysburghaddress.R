library(tidyverse)

gw <- scan('http://www.isi-stats.com/isi/data/chap3/GettysburgAddress.txt', character(), quote ="")

gw <- gw %>% gsub(",", "", x=. ) %>% gsub("\\.", "", x=.)
GW <- data.frame(words = gw) %>% 
  mutate(wordlen = str_length(words), 
         cont.e = grepl("e", words))


