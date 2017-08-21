library(stringr)
library(parsedate)

system.time(mydf$date <- as.Date(parsedate::parse_date(str_sub(mydf[, 15], -50, -5))))