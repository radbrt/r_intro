library(tidyverse)
library(janitor)

# Leser datasettet - spesifiser at det er semikolon som feltskilletegn
# Argumentet "locale" er for å angi at vi har komma som desimalskilletegn
ytelser <- read_delim('data/ytelser.csv', <...>, locale=locale(decimal_mark = ","))

# Rydder i navnene - helt automatisk
ytelser <- clean_names(ytelser)

# Sjekker de nye kolonnenavnene
<...>

# Vi har lyst på en egen kolonne med kommunenummer, og en kolonne med fylkesnummer
ytelser <- ytelser %>% 
  mutate(kommnr = substr(kommune_nr_navn, 1, 4),
         fylkenr = substr(kommune_nr_navn, 1, 2))

# Summere ytelser pr år
ytelser %>% 
  group_by(<...>) %>% 
  summarize(belop_mrd = <...>) 
  
  
# Hmmm... klarer vi å lage et plot av dette? Konverter til milliarder i samme slengen.
ytelser %>% 
  group_by(ar) %>% 
  summarize(<...>) %>% 
  ggplot(aes(ar, belop_mrd)) + 
  geom_line() +
  ylim(0, 500) +
  <...>("Beløp (mrd kroner)") +
  <...>("År")

ytelser_pr_kom <- ytelser %>% 
  group_by(ar, kommune_nr_navn) %>% 
  summarize(belop_alle_ytelser = sum(utbetalt_mill_kr))

ytelser_andel_komm <- ytelser %>% 
  inner_join(ytelser_pr_kom, by=c("ar", "kommune_nr_navn")) %>% 
  mutate(ytelsesandel = utbetalt_mill_kr/belop_alle_ytelser)


# Største ytelsen pr kommune
ytelser_andel_komm %>% 
  group_by(kommune_nr_navn) %>% 
  arrange(desc(ytelsesandel)) %>% 
  mutate(rownum = row_number()) %>% 
  filter(rownum==1) %>% 
  ungroup() %>% 
  arrange(kommune_nr_navn)


unique(ytelser$stonadsomrade_gruppe)

# Hovedgruppe ytelser, summere og pivotere
ytelser %>% 
  select(ar, fylke_nr_navn, stonadsomrade_gruppe, utbetalt_mill_kr) %>% 
  group_by_at(vars(-utbetalt_mill_kr)) %>% 
  summarize(bel = sum(utbetalt_mill_kr)) %>% 
  pivot_wider(names_from = stonadsomrade_gruppe, values_from = bel) %>% 
  clean_names() -> ytelser_wide

