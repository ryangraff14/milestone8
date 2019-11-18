#Update this library list from pset 8

library(lintr)
library(gt)
library(readr)
library(dplyr)
library(skimr)
library(glue)
library(broom)
library(knitr)
library(styler)
library(tidyr)
library(readxl)
library(purrr)
library(tibble)
library(scales)
library(stringr)
library(janitor)
library(ggplot2)
library(googlesheets4)
library(infer)
library(rstanarm)
library(broom)
library(cowplot)
library(ggridges)
library(sf)
library(fs)
library(tidyverse)

# Creating my directories

dir.create("raw-draft-data")

dir.create("clean-draft-data")

draft1 <- read_xlsx("raw-draft-data/draft_data.xlsx", skip = 1) %>%
  clean_names()

draft2 <- draft1 %>% 
  select(rk, pk, year, rd, tm, player, fg_percent, x3p_percent, ft_percent, mp_17, pts_18, trb_19, ast_20, ws_48, bpm, vorp) %>% 
  # BPM is historical usage stat since VORP includes playing time
  mutate_all(~replace(., is.na(.), 0))
  
write_rds(draft2, "clean-draft-data/cleaned.rds", compress = "none")
