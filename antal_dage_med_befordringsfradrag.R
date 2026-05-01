# Forberedelse ####

# export calender som CSV

library(dplyr)
library(qlcal)
library(lubridate)


# Dataforberedelse ####

kal <- read.csv("~/Documents/SKTST/personlige_dokumenter/kalender_til_befordringsfradrag.CSV")

kal_simple <- kal %>% transmute(Emne = Emne, 
                                Startdato = as.Date(Startdato, tryFormats = c("%d-%m-%Y")), 
                                Slutdato = as.Date(Slutdato, tryFormats = c("%d-%m-%Y")), 
                                Slutdato = if_else(Hele.dagen=="Sand", Slutdato - days(1), Slutdato),
                                Hele_dagen = if_else(Hele.dagen=="Sand", TRUE, FALSE),
                                Vis_tidspunkt_som = Vis.tidspunkt.som, 
                                tidsinterval = interval(start = Startdato, end = Slutdato))

kal_simple_2025 <- kal_simple %>% filter(year(kal_simple$Startdato) == 2025, year(kal_simple$Slutdato) == 2025)


# Filtrering af HAP-dage ####

ingen_bef <- "hap|bsy|syg|fri|ferie|^$"
hap_2025 <-  kal_simple_2025 %>% filter(str_like(string = .$Emne, pattern = ingen_bef) & Hele_dagen)

# Tjek: heldags aftaler der ikke matcher: 
ikke_hap_2025 <- kal_simple_2025 %>% filter(!(str_like(string = .$Emne, pattern = ingen_bef)) & Hele_dagen)
# OK (helligdage er taget med i kalenderen)

# 0 — Free
# 1 — Tentative
# 2 — Busy
# 3 — Out of Office (or OOF)
# 4 — Working Elsewhere (or Working Elsewhere/Working Accompanied)

# Kalenderforberedelse ####

setCalendar("Denmark")


# Beregning af månedlige dage med befordring ####

befordring = data.frame(brutto = getBusinessDays(as.Date("2025-01-01"), as.Date("2025-12-15")))
befordring <- befordring %>% rowwise() %>% mutate(kontor = !any(brutto %within% hap_2025$tidsinterval))

befordring %>% group_by(month(brutto)) %>% summarise(sum(kontor))
