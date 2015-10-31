library(dplyr)
library(rvest)
library(readxl)
library(reshape2)
library(ggplot2)

source("loe_nimed.R")
nimetabel = loo_nimetabel()

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


# ---- Graafikud ja analüüs ----
ggplot(palgad) +
  geom_bar(aes(x=Täiskoormusega, fill=Sugu)) +
  facet_wrap(~Tüüp, nrow=2, ncol=1, scales="free_y")


# Asutuse kaupa keskmiste palkade vahe
lohe <- palgad %>%
  group_by(Tüüp, Asutus, Ametikoht) %>%
  summarise(KeskminePalkN=mean(ifelse(Naine, Täiskoormusega, 0)), # TODO kas mu arvutused üldse meigivad senssi?
            KeskminePalkM=mean(ifelse(Mees , Täiskoormusega, 0)),
            countN=sum(ifelse(Naine, 1, 0)),
            countM=sum(ifelse(Mees, 1, 0))) %>%
  summarise(SummaPalkN=sum(KeskminePalkN*countN),
            SummaPalkM=sum(KeskminePalkM*countM),
            totalcountN=sum(countN),
            totalcountM=sum(countM)) %>%
  mutate(KeskmineN=SummaPalkN/totalcountN,
         KeskmineM=SummaPalkM/totalcountM) %>%
  ungroup() %>%
  filter(totalcountN >= 3) %>%
  filter(totalcountM >= 3)

ggplot(lohe) +
  geom_point(aes(x=KeskmineM, y=KeskmineN, color=Tüüp,
                 size=2000*sqrt(totalcountN+totalcountM))) +
  geom_abline(alpha=1, colour="#555555") +
  geom_abline(alpha=0.5, colour="#aaaaaa", slope=0.6) +
  geom_abline(alpha=0.5, colour="#aaaaaa", slope=0.8) +
  geom_abline(alpha=0.5, colour="#aaaaaa", slope=1.2) +
  geom_abline(alpha=0.5, colour="#aaaaaa", slope=1.4) +
  xlim(0, 2500) +
  ylim(0, 2500) +
  theme_bw() +
  theme(panel.grid.minor=element_blank())





