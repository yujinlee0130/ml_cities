# Header -------------------------------------------

# This script loads and prepares data on rent-stabilized
# unit counts for the years 2019 - 2021. Pluto data
# from those respective years are used to obtain total
# unit counts for each bbl per year. 

# rs unit counts taken from nycdb


# Set Up -------------------------------------------

library(tidyverse)
library(janitor)
library(sf)


# Load Data ----------------------------------------

# rs unit counts by bbl, from 2019 - 2021
rs_19_21 <- read_csv("https://s3.amazonaws.com/justfix-data/rentstab_counts_from_doffer_2021.csv") %>%
  transmute(bbl = as.character(ucbbl),
            rs_units_19 = uc2019,
            rs_units_20 = uc2020,
            rs_units_21 = uc2021) %>%
  # for rs unit counts, replace NA values with 0 
  mutate_all(~replace(., is.na(.), 0))
  
# pluto data 2019
pluto_19 <- read_sf("~/Desktop/Machine Learning for Cities/Final Project/nyc_mappluto_19v2_arc_shp/MapPLUTO.shp") %>%
  # drop buildings with condos 
  filter(!grepl('^R', BldgClass)) %>%
  transmute(bbl = as.character(BBL),
            total_units_19 = UnitsRes) %>%
  st_drop_geometry()

# pluto data 2020
pluto_20 <- read_sf("~/Desktop/Machine Learning for Cities/Final Project/nyc_mappluto_20v8_arc_shp/MapPLUTO.shp") %>%
  filter(!grepl('^R', BldgClass)) %>%
  transmute(bbl = as.character(BBL),
            total_units_20 = UnitsRes) %>%
  st_drop_geometry()

# pluto data 2021
pluto_21 <- read_sf("~/Desktop/Machine Learning for Cities/Final Project/nyc_mappluto_21v3_arc_shp/MapPLUTO.shp") %>%
  filter(!grepl('^R', BldgClass)) %>%
  transmute(bbl = as.character(BBL),
            total_units_21 = UnitsRes,
            boro = as.character(BoroCode),
            cd = as.character(CD),
            # only 2010 census tracts are available through pluto 
            ct_10 = Tract2010,
            year_built = YearBuilt,
            bldg_class = BldgClass) %>%
  st_drop_geometry()


# Create Tables ------------------------------------

# rs shares 2019 - 2021
rs_shares_19_21 <- rs_19_21 %>%
  # filter out buildings where there are no rs units for any of the years 
  filter(!(rs_units_19 == 0 & rs_units_20 == 0 & rs_units_21 == 0)) %>%
  inner_join(pluto_19, by = 'bbl') %>%
  inner_join(pluto_20, by = 'bbl') %>%
  inner_join(pluto_21, by = 'bbl') %>%
  filter(total_units_19 > 0,
         total_units_20 > 0,
         total_units_21 > 0,
         year_built != 0) %>%
  mutate(rs_share_19 = rs_units_19 / total_units_19,
         rs_share_20 = rs_units_20 / total_units_20,
         rs_share_21 = rs_units_21 / total_units_21) 

test_table <- rs_shares_19_21 %>%
  filter(rs_units_19 > total_units_19 |
           rs_units_20 > total_units_19 |
           rs_units_21 > total_units_19) 

test_table_2 <- rs_shares_19_21 %>%
  filter(rs_units_19 <= total_units_19,
         rs_units_20 <= total_units_20,
         rs_units_21 <= total_units_21)

# export rs_share_19_21 as csv
write_csv(rs_shares_19_21, "~/Desktop/rs_shares_19_21.csv")


# Exploratory --------------------------------------

# n buildings by building class
test_table <- rs_shares_19_21 %>%
  group_by(bldg_class) %>%
  summarize(n_buildings = n()) %>%
  ungroup() %>%
  arrange(desc(n_buildings))


