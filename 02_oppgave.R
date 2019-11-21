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

# Oppsummere ytelser pr år
ytelser %>% 
  group_by(<...>) %>% 
  summarize(belop_mrd = <...>) 
  
  
# Hmmm... klarer vi å lage et plot av dette?
ytelser %>% 
  group_by(ar) %>% 
  summarize(belop_mrd = sum(utbetalt_mill_kr)/1000) %>% 
  ggplot(aes(ar, belop_mrd)) + 
  geom_line() +
  ylim(0, 500) +
  ylab("Beløp (mrd kroner)") +
  xlab("År")

ytelser_pr_kom <- ytelser %>% 
  group_by(ar, kommune_nr_navn) %>% 
  summarize(belop_alle_ytelser = sum(utbetalt_mill_kr))

ytelser_andel <- ytelser %>% 
  inner_join(ytelser_pr_kom, by=c("ar", "kommune_nr_navn")) %>% 
  mutate(ytelsesandel = utbetalt_mill_kr/belop_alle_ytelser)