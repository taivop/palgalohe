library(dplyr)
library(rvest)
library(readxl)
library(reshape2)
library(ggplot2)

source("loe_nimed.R")

# Loe failid sisse
palgad_kov <- read_excel("data/palgad_kov.xlsx",
                  col_types=c(rep("text", 6), "numeric", "numeric")) %>%
  mutate(Tüüp="Kohalik omavalitsus")
palgad_riik <- read_excel("data/palgad_riik.xlsx",
                         col_types=c(rep("text", 6), "numeric", "numeric")) %>%
  mutate(Tüüp="Riigiasutus")

# Mudi andmed paremale kujule
palgad <- rbind(palgad_kov, palgad_riik) %>%
  mutate(Täiskoormusega=Põhipalk/Koormus) %>%
  mutate(Põhipalk=round(Põhipalk), Täiskoormusega=round(Täiskoormusega)) %>%
  mutate(Ametikoht=tolower(Ametikoht)) %>%
  # Eemaldame eesnimetud
  filter(!is.na(Eesnimi)) %>%
  # Eemaldame eesnime lõpust ja algusest tühikud
  mutate(Eesnimi=sub("\\s+$", "", Eesnimi)) %>%
  left_join(nimetabel, by=c("Eesnimi"="Nimi")) %>%
  select(Tüüp, Asutus, Struktuuriüksus, Ametikoht, Koormus, Põhipalk,
         Täiskoormusega, Eesnimi, Mees, Naine) %>%
  # Lisame tunnuse Sugu
  mutate(Sugu=as.factor(ifelse(Mees, "Mees", "Naine")))

palgad <- palgad %>%
  # Eemaldame inimesed, kelle nimi on nii meeste kui naiste nimekirjas
  filter(!(Mees && Naine)) %>%
  # Eemaldame inimesed, kelle sugu me ei tea
  filter(!is.na(Mees))
  

# Vaatame inimesi, kellele ei leidunud nimede tabelis vastet
vastetud <- palgad %>%
  filter(is.na(Mees)) %>% # Kui nime ei leidu, siis on nii Mees kui Naine NA-d
  group_by(Eesnimi) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

ggplot(palgad) +
  geom_bar(aes(x=Täiskoormusega, fill=Sugu)) +
  facet_wrap(~Tüüp, nrow=2, ncol=1, scales="free_y")

g <- palgad %>%
  group_by(Asutus, Struktuuriüksus, Ametikoht) %>%
  summarise(count=n()) %>%
  filter(count > 1)