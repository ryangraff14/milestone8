library(ballr)

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

# Command to install ballr, necessary package to gather current player stats
# install.packages("devtools")
# library(devtools)
# install_github("rtelmore/ballr")

lebron_basic <- NBAPlayerPerGameStats("/players/j/jamesle01.html") %>% 
  filter(season == "Career") %>% 
  select(fgpercent, x3ppercent, ftpercent, mp, pts, trb, ast)

lebron_adv <- NBAPerGameAdvStatistics(season = 2009) %>% 
  filter(rk == "208") %>% 
  select(ws_48, bpm, vorp)

lebron <- merge(lebron_basic, lebron_adv)

mj_basic <- NBAPlayerPerGameStats("/players/j/jordami01.html") %>% 
  filter(season == "Career") %>% 
  select(fgpercent, x3ppercent, ftpercent, mp, pts, trb, ast)

mj_adv <- NBAPerGameAdvStatistics(season = 1991) %>% 
  filter(rk == "175") %>% 
  select(ws_48, bpm, vorp)

mj <- merge(mj_basic, mj_adv)

steph_basic <- NBAPlayerPerGameStats("/players/c/curryst01.html") %>% 
  filter(season == "Career") %>% 
  select(fgpercent, x3ppercent, ftpercent, mp, pts, trb, ast)

steph_adv <- NBAPerGameAdvStatistics(season = 2016) %>% 
  filter(rk == "105") %>% 
  select(ws_48, bpm, vorp)

steph <- merge(steph_basic, steph_adv)

manu_basic <- NBAPlayerPerGameStats("/players/g/ginobma01.html") %>% 
  filter(season == "Career") %>% 
  select(fgpercent, x3ppercent, ftpercent, mp, pts, trb, ast)

manu_adv <- NBAPerGameAdvStatistics(season = 2008) %>% 
  filter(rk == "152") %>% 
  select(ws_48, bpm, vorp)

manu <- merge(manu_basic, manu_adv)

