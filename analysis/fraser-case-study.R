################################################################################
# FRASER ESTUARY CASE STUDY
# 16 Feb 2022
# Amie MacDonald
#
################################################################################

# load packages
library(tidyverse)
library(lubridate)
library(viridis)

# set timezone
Sys.setenv(TZ = "GMT")

theme_set(theme_bw())

# load cleaned detection data for WESA and DUNL
wesa.det <- readRDS("./processed-data/wesa-tag-detections-clean.rds")
dunl.det <- readRDS("./processed-data/dunl-tag-detections-clean.rds")

fields <- names(wesa.det)

dunl.det <- dunl.det %>% 
  select(all_of(fields))

# pull out tags of representative individuals
wesa.tags <- c(44264, 48903, 48906, 48907, 48919)
dunl.tags <- c(49146, 49152, 49156, 49166, 49168)

wesa.sub <- wesa.det %>% 
  filter(motusTagID %in% wesa.tags)

dunl.sub <- dunl.det %>% 
  filter(motusTagID %in% dunl.tags)

det.sub <- bind_rows(wesa.sub, dunl.sub)

# pull out detections in fraser estuary
fraser.recvs <- c("Brunswick Point farm", "Blackie Spit", "Alaksen",
                  "Iona Shore Station", "Boundary Bay Field", "Surrey",
                  "Tsawwassen First Nation", "Abbotsford")

det.sub <- det.sub %>% 
  filter(recvName %in% fraser.recvs)

# summarize detections by day
det.summary <- det.sub %>%
  arrange(motusTagID, ts) %>%
  mutate(ts = as_datetime(ts),
         date = as_date(ts),
         doy = yday(ts),
         year = year(ts),
         md = format(as.Date(date), "%b-%d")) %>% 
  #filter(ts >= "2020-07-01") %>% 
  select(motusTagID, speciesSci, date, year, md, doy) %>%
  distinct() %>% 
  mutate(motusTagID = as.factor(motusTagID))

# plot summarized detections
labels <- c(`Calidris alpina` = "Dunlin", `Calidris mauri` = "Western Sandpiper")

png(filename = "./figures/fraser-dunl-wesa-stays.png",
    width=8, height=4, units="in", res=600)

print(ggplot(data = det.summary, aes(x = date, y = motusTagID,
                               colour = motusTagID)) +
  geom_line(lwd = 4, alpha = 0.3) +
  geom_point(size = 1) +
  scale_color_viridis_d() +
  labs(x = "Date", y = "Motus tag ID") +
  facet_wrap(speciesSci ~ ., nrow = 2, scales = "free_y", labeller = labeller(speciesSci = labels)) +
  theme(legend.position = "none"))

dev.off()
