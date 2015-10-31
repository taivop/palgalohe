library(dplyr)

loo_nimetabel = function() {
  
  # Loeme naisenimed listi
  naised <- list()
  lines <- readLines("nimed/naised.txt")
  for(line in lines) {
    uued_nimed <- strsplit(line, "\t")
    naised <- unlist(c(naised, uued_nimed))
  }
  
  # Loeme mehenimed listi
  mehed <- list()
  lines <- readLines("nimed/mehed.txt")
  for(line in lines) {
    uued_nimed <- strsplit(line, "\t")
    mehed <- unlist(c(mehed, uued_nimed))
  }
  
  # Ühendame kaks listi
  dfn <- as.data.frame(naised) %>% rename(Nimi=naised)
  dfm <- as.data.frame(mehed) %>% rename(Nimi=mehed)
  dfm <- dfm %>% mutate(Mees=TRUE)
  dfn <- dfn %>% mutate(Naine=TRUE)
  
  # Paneme nimetabeli kokku
  nimetabel <- rbind(dfm %>% select(Nimi), dfn %>% select(Nimi)) %>%
    group_by(Nimi) %>%
    filter(row_number(Nimi) == 1) %>%  # Eemaldame duplikaadid
    left_join(dfm, by="Nimi") %>%
    left_join(dfn, by="Nimi") %>%
    mutate(Mees=ifelse(is.na(Mees), FALSE, TRUE)) %>%
    mutate(Naine=ifelse(is.na(Naine), FALSE, TRUE))
  
  return(nimetabel)
}