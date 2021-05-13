
# packages
library(tidyverse)
library(readxl)
library(janitor)

# ---------------
# LPI and Redlist data

# importing LPI data
lpi_data <- read_csv(here::here('dataset', 'LPR2020data_public.csv'),
                     na = 'NULL',
                     col_types = paste('fccccccccccccccddcccccccccccc',
                                       paste(rep('d', 69), collapse = ''),
                                       sep = '')) %>% 
  clean_names() %>% 
  select(-c('reference', 'citation')) 

# converting LPI data to time series format
lpi_ts <- lpi_data %>% 
  pivot_longer(cols = x1950:x2018,
               names_to = 'year',
               values_to = 'count') %>% 
  mutate(year = as.integer(gsub('x', '', year)))

# importing IUCN Redlist data
redlist_data <- read_csv(here::here('dataset', 'redlistspecies.csv'),
                         col_types = 'ffccccccccccccfccff') %>% 
  clean_names() %>% 
  select(-c('internal_taxon_id',
            'infra_type',
            'infra_name',
            'infra_authority',
            'redlist_criteria')) %>% 
  # creating a binomial feature to match that in lpi_ts
  mutate(binomial = gsub(' ', '_', scientific_name))

# joining lpi with redlist
lpi_red_data <- left_join(lpi_data,
                         redlist_data,
                         by = 'binomial')

# converting merged data to time series format
lpi_red_ts <- lpi_red_data %>%
  pivot_longer(cols = x1950:x2018,
               names_to = 'year',
               values_to = 'count') %>% 
  mutate(year = as.integer(gsub('x', '', year))) %>% 
  filter(!is.na(count))

# ---------------
# expenditure data

# functions to help read brackets as in expenditure data files

# function to read outermost bracket
# example: 'welcome (home) (my(lovely) gator)' %>% last_br()
# returns: 'my(lovely) gator'
last_br <- function(string) {
  
  # initializing counters
  open_br <- 0
  start_pos <- 0
  end_pos <- 0
  
  # list of string characters
  string_list <- unlist(strsplit(string, split = ""))
  
  # finds final outermost opening and closing brackets
  for (i in 1:nchar(string)) {
    if (string_list[i] == '(') {
      if (open_br == 0) {
        start_pos = i
      }
      open_br <- open_br + 1
    } else if (string_list[i] == ')') {
      if (open_br == 1) {
        end_pos = i
      }
      open_br <- open_br - 1
    }
  }
  
  # finding and returning string in last bracket
  final <- substr(string, start_pos + 1, end_pos - 1)
  return(final)
}

# function to remove final brackets and content within
# example: 'my(lovely) gator' %>% rm_br()
# returns 'my gator'
rm_br <- function (string) {
  # ignore empty strings
  if (nchar(string) == 0) {return(string)}
  
  # initializing counters
  open_br <- 0
  start_pos <- 0
  end_pos <- 0
  
  # list of string characters
  string_list <- unlist(strsplit(string, split = ""))
  
  # finds start and end brackets
  for (i in 1:nchar(string)) {
    if (string_list[i] == '(') {
      if (open_br == 0) {
        start_pos = i
      }
      open_br <- open_br + 1
    } else if (string_list[i] == ')') {
      if (open_br == 1) {
        end_pos = i
      }
      open_br <- open_br - 1
    }
  }
  
  # finding and returning string except last bracket
  final <- paste(substr(string, 1, start_pos - 1),
                 substr(string, end_pos + 1, nchar(string)),
                 sep = '')
  return(final)
}

# combining functions to read last bracket without inner
# example: 'welcome (home) (my(lovely) gator)' %>% read_br()
# returns: 'my gator'
# ignores if bracket begins with '=', is too short, begins with lower
# returns NA if blank
read_br <- function (string) {
  
  if (!all(grepl('\\(', string),
           str_count(string, '\\)') == str_count(string, '\\('))) {
    return(string)
  }
  
  bracket <- rm_br(last_br(string))
  
  # avoiding too-small or false brackets
  if (any(nchar(bracket) <= 2,
          str_detect(substr(bracket, 1, 1), '[[:lower:]]'),
          substr(bracket, 1, 1) == '=')) {
    bracket <- read_br(gsub('  ', ' ', rm_br(string)))
  }
  
  final <- ifelse(bracket == '',
                  NA,
                  bracket)
  
  return(final)
}


# -----
# reading expenditure data per year
# each year is first read, then cleaned
# `raw_exp_<X>` is the raw data for year X
# `exp_<X>` is clean data for year X

# 2000
raw_exp_2000 <- read_excel(here::here('dataset',
                                      'expen/expenditure_2000.xlsx'),
                           sheet = 3)

exp_2000 <- raw_exp_2000 %>% 
  clean_names() %>% 
  rename(common = species_name_50_cfr_part_17,
         scientific = scientific_name,
         class = rank,
         species_total = species_total_000) %>% 
  filter(!is.na(common),
         !is.na(class),
         !is.na(scientific)) %>% 
  
  # convert expenditure units to dollars
  mutate(across(fws_total:species_total, ~ .x * 1000)) %>% 
  
  # changing scientific to Genus_species, subspecies, genus format
  rowwise() %>% 
  mutate(binomial = paste(unlist(strsplit(scientific,
                                          split = ' '))[1:2],
                          collapse = '_'),
         subspecies = ifelse(str_count(scientific, ' ') == 2,
                             unlist(strsplit(scientific,
                                             split = ' '))[3],
                             NA),
         genus = paste(unlist(strsplit(scientific,
                                       split = ' '))[1],
                       collapse = '_')) %>% 
  ungroup() %>% 
  mutate(year = 2000)


# 2001
raw_exp_2001 <- read_excel(here::here('dataset',
                                      'expen/expenditure_2001.xlsx'),
                           sheet = 5)

exp_2001 <- raw_exp_2001 %>% 
  clean_names() %>% 
  rename(common = species_50_cfr_part_17) %>% 
  filter(!is.na(common),
         !is.na(class)) %>% 
  
  # changing scientific to Genus_species, subspecies, genus format
  rowwise() %>% 
  mutate(scientific = read_br(gsub('\r?\n|\r', ' ', common))) %>%
  mutate(scientific = gsub('  ', ' ', scientific)) %>% 
  mutate(binomial = paste(unlist(strsplit(scientific,
                                          split = ' '))[1:2],
                          collapse = '_'),
         subspecies = ifelse(str_count(scientific, ' ') == 2,
                             unlist(strsplit(scientific,
                                             split = ' '))[3],
                             NA),
         genus = paste(unlist(strsplit(scientific,
                                       split = ' '))[1],
                       collapse = '_')) %>% 
  ungroup() %>% 
  mutate(year = 2001)

# 2002
raw_exp_2002 <- read_excel(here::here('dataset',
                                      'expen/expenditure_2002.xlsx'),
                           sheet = 5)

exp_2002 <- raw_exp_2002 %>% 
  clean_names() %>% 
  rename(common = species_50_cfr_part_17) %>% 
  filter(!is.na(common),
         !is.na(class)) %>% 
  
  # changing scientific to Genus_species, subspecies, genus format
  rowwise() %>% 
  mutate(scientific = read_br(gsub('\r?\n|\r', ' ', common))) %>%
  mutate(scientific = gsub('  ', ' ', scientific)) %>% 
  mutate(binomial = paste(unlist(strsplit(scientific,
                                          split = ' '))[1:2],
                          collapse = '_'),
         subspecies = ifelse(str_count(scientific, ' ') == 2,
                             unlist(strsplit(scientific,
                                             split = ' '))[3],
                             NA),
         genus = paste(unlist(strsplit(scientific,
                                       split = ' '))[1],
                       collapse = '_')) %>% 
  ungroup() %>% 
  mutate(year = 2002)

# 2003
raw_exp_2003 <- read_excel(here::here('dataset',
                                      'expen/expenditure_2003.xlsx'),
                           sheet = 5)

exp_2003 <- raw_exp_2003 %>% 
  clean_names() %>% 
  rename(common = species_50_cfr_part_17,
         status = stat_us) %>% 
  filter(!is.na(common),
         !is.na(class)) %>% 
  
  # changing scientific to Genus_species, subspecies, genus format
  rowwise() %>% 
  mutate(scientific = read_br(gsub('\r?\n|\r', ' ', common))) %>%
  mutate(scientific = gsub('  ', ' ', scientific)) %>% 
  mutate(binomial = paste(unlist(strsplit(scientific,
                                          split = ' '))[1:2],
                          collapse = '_'),
         subspecies = ifelse(str_count(scientific, ' ') == 2,
                             unlist(strsplit(scientific,
                                             split = ' '))[3],
                             NA),
         genus = paste(unlist(strsplit(scientific,
                                       split = ' '))[1],
                       collapse = '_')) %>% 
  ungroup() %>% 
  mutate(year = 2003)


# 2004
raw_exp_2004 <- read_excel(here::here('dataset',
                                      'expen/expenditure_2004.xlsx'),
                           sheet = 5)

exp_2004 <- raw_exp_2004 %>% 
  clean_names() %>% 
  rename(common = species_50_cfr_part_17) %>% 
  filter(!is.na(common),
         !is.na(class)) %>% 
  
  # changing scientific to Genus_species, subspecies, genus format
  rowwise() %>% 
  mutate(scientific = read_br(gsub('\r?\n|\r', ' ', common))) %>%
  mutate(scientific = gsub('  ', ' ', scientific)) %>% 
  mutate(binomial = paste(unlist(strsplit(scientific,
                                          split = ' '))[1:2],
                          collapse = '_'),
         subspecies = ifelse(str_count(scientific, ' ') == 2,
                             unlist(strsplit(scientific,
                                             split = ' '))[3],
                             NA),
         genus = paste(unlist(strsplit(scientific,
                                       split = ' '))[1],
                       collapse = '_')) %>% 
  ungroup() %>% 
  mutate(year = 2004)

# 2005
raw_exp_2005 <- read_excel(here::here('dataset',
                                      'expen/expenditure_2005_06.xlsx'),
                           sheet = 1)

exp_2005 <- raw_exp_2005 %>% 
  clean_names() %>% 
  rename(common = species_50_cfr_part_17,
         class = rank) %>% 
  filter(!is.na(common),
         !is.na(class)) %>% 
  
  # changing scientific to Genus_species, subspecies, genus format
  rowwise() %>% 
  mutate(scientific = read_br(gsub('\r?\n|\r', ' ', common))) %>%
  mutate(scientific = gsub('  ', ' ', scientific)) %>% 
  mutate(binomial = paste(unlist(strsplit(scientific,
                                          split = ' '))[1:2],
                          collapse = '_'),
         subspecies = ifelse(str_count(scientific, ' ') == 2,
                             unlist(strsplit(scientific,
                                             split = ' '))[3],
                             NA),
         genus = paste(unlist(strsplit(scientific,
                                       split = ' '))[1],
                       collapse = '_')) %>% 
  ungroup() %>% 
  mutate(year = 2005)

# 2006
raw_exp_2006 <- read_excel(here::here('dataset',
                                      'expen/expenditure_2005_06.xlsx'),
                           sheet = 2)

exp_2006 <- raw_exp_2006 %>% 
  clean_names() %>% 
  rename(common = species_50_cfr_part_17) %>% 
  filter(!is.na(common),
         !is.na(class)) %>% 
  
  # changing scientific to Genus_species, subspecies, genus format
  rowwise() %>% 
  mutate(scientific = read_br(gsub('\r?\n|\r', ' ', common))) %>%
  mutate(scientific = gsub('  ', ' ', scientific)) %>% 
  mutate(binomial = paste(unlist(strsplit(scientific,
                                          split = ' '))[1:2],
                          collapse = '_'),
         subspecies = ifelse(str_count(scientific, ' ') == 2,
                             unlist(strsplit(scientific,
                                             split = ' '))[3],
                             NA),
         genus = paste(unlist(strsplit(scientific,
                                       split = ' '))[1],
                       collapse = '_')) %>% 
  ungroup() %>% 
  mutate(year = 2006)

# 2007
raw_exp_2007 <- read_excel(here::here('dataset',
                                      'expen/expenditure_2007.xlsx'),
                           sheet = 1)

exp_2007 <- raw_exp_2007 %>% 
  clean_names() %>% 
  rename(common = species_50_cfr_part_17) %>% 
  filter(!is.na(common),
         !is.na(class)) %>% 
  
  # changing scientific to Genus_species, subspecies, genus format
  rowwise() %>% 
  mutate(scientific = read_br(gsub('\r?\n|\r', ' ', common))) %>%
  mutate(scientific = gsub('  ', ' ', scientific)) %>% 
  mutate(binomial = paste(unlist(strsplit(scientific,
                                          split = ' '))[1:2],
                          collapse = '_'),
         subspecies = ifelse(str_count(scientific, ' ') == 2,
                             unlist(strsplit(scientific,
                                             split = ' '))[3],
                             NA),
         genus = paste(unlist(strsplit(scientific,
                                       split = ' '))[1],
                       collapse = '_')) %>% 
  ungroup() %>% 
  mutate(year = 2007)

# 2008
raw_exp_2008 <- read_excel(here::here('dataset',
                                      'expen/expenditure_2008.xlsx'),
                           sheet = 2) %>% 
  select(-starts_with('...'))

exp_2008 <- raw_exp_2008 %>% 
  clean_names() %>% 
  rename(common = species_50_cfr_part_17,
         class = group_name) %>% 
  filter(!is.na(common),
         !is.na(class)) %>% 
  
  # changing scientific to Genus_species, subspecies, genus format
  rowwise() %>% 
  mutate(scientific = read_br(gsub('\r?\n|\r', ' ', common))) %>%
  mutate(scientific = gsub('  ', ' ', scientific)) %>% 
  mutate(binomial = paste(unlist(strsplit(scientific,
                                          split = ' '))[1:2],
                          collapse = '_'),
         subspecies = ifelse(str_count(scientific, ' ') == 2,
                             unlist(strsplit(scientific,
                                             split = ' '))[3],
                             NA),
         genus = paste(unlist(strsplit(scientific,
                                       split = ' '))[1],
                       collapse = '_')) %>% 
  ungroup() %>% 
  mutate(year = 2008)

# 2009
raw_exp_2009 <- read_excel(here::here('dataset',
                                      'expen/expenditure_2009.xlsx'),
                           sheet = 1) %>% 
  select(-starts_with('...'))

exp_2009 <- raw_exp_2009 %>% 
  clean_names() %>% 
  rename(common = species_50_cfr_part_17,
         class = group) %>% 
  filter(!is.na(common),
         !is.na(class)) %>% 
  
  # changing scientific to Genus_species, subspecies, genus format
  rowwise() %>% 
  mutate(scientific = read_br(gsub('\r?\n|\r', ' ', common))) %>%
  mutate(scientific = gsub('  ', ' ', scientific)) %>% 
  mutate(binomial = paste(unlist(strsplit(scientific,
                                          split = ' '))[1:2],
                          collapse = '_'),
         subspecies = ifelse(str_count(scientific, ' ') == 2,
                             unlist(strsplit(scientific,
                                             split = ' '))[3],
                             NA),
         genus = paste(unlist(strsplit(scientific,
                                       split = ' '))[1],
                       collapse = '_')) %>% 
  ungroup() %>% 
  mutate(year = 2009)

# 2010
raw_exp_2010 <- read_excel(here::here('dataset',
                                      'expen/expenditure_2010.xlsx'),
                           sheet = 3) %>% 
  select(-starts_with('...'))

exp_2010 <- raw_exp_2010 %>% 
  clean_names() %>% 
  rename(common = species_sci) %>% 
  filter(!is.na(common),
         !is.na(class)) %>% 
  
  # changing scientific to Genus_species, subspecies, genus format
  rowwise() %>% 
  mutate(scientific = read_br(gsub('\r?\n|\r', ' ', common))) %>%
  mutate(scientific = gsub('  ', ' ', scientific)) %>% 
  mutate(binomial = paste(unlist(strsplit(scientific,
                                          split = ' '))[1:2],
                          collapse = '_'),
         subspecies = ifelse(str_count(scientific, ' ') == 2,
                             unlist(strsplit(scientific,
                                             split = ' '))[3],
                             NA),
         genus = paste(unlist(strsplit(scientific,
                                       split = ' '))[1],
                       collapse = '_')) %>% 
  ungroup() %>% 
  mutate(year = 2010)

# 2011
# multiple sheets must be concatenated
path_2011 <- here::here('dataset', 'expen/expenditure_2011.xlsx')
raw_exp_2011 <- excel_sheets(path_2011)[1:66] %>%
  map_df(., ~ read_excel(path_2011, sheet = .x)) %>% 
  clean_names() %>% 
  # messed up cell divisions require concatenation over rows
  rowwise() %>% 
  mutate(species = ifelse(all(!is.na(species_50_cfr_part_17),
                              is.na(species_50_cfr)),
                          species_50_cfr_part_17,
                          species_50_cfr)) %>% 
  ungroup() %>% 
  mutate(groups = cumsum(!is.na(status))) %>% 
  group_by(groups) %>% 
  mutate(common = gsub('\\-\\s', '', str_c(species, collapse = ' '))) %>% 
  ungroup() %>% 
  mutate(groups = cumsum(!is.na(group_name))) %>% 
  group_by(groups) %>% 
  mutate(class = first(group_name)) %>% 
  ungroup() %>% 
  select(-c('group_name',
            'species_50_cfr',
            'species_50_cfr_part_17',
            'species',
            'groups')) %>% 
  filter(!is.na(status))

exp_2011 <- raw_exp_2011 %>% 
  filter(!is.na(common),
         !is.na(class)) %>% 
  
  # changing scientific to Genus_species, subspecies, genus format
  rowwise %>% 
  mutate(scientific = read_br(gsub('\r?\n|\r', ' ', common))) %>%
  mutate(scientific = gsub('  ', ' ', scientific)) %>% 
  mutate(binomial = paste(unlist(strsplit(scientific,
                                          split = ' '))[1:2],
                          collapse = '_'),
         subspecies = ifelse(str_count(scientific, ' ') == 2,
                             unlist(strsplit(scientific,
                                             split = ' '))[3],
                             NA),
         genus = paste(unlist(strsplit(scientific,
                                       split = ' '))[1],
                       collapse = '_')) %>% 
  ungroup() %>% 
  mutate(year = 2011)

# 2012
raw_exp_2012 <- read_excel(here::here('dataset',
                                      'expen/expenditure_2012.xlsx')) %>% 
  select(-starts_with('...'))

exp_2012 <- raw_exp_2012 %>% 
  clean_names() %>% 
  rename(common = species_sci) %>% 
  filter(!is.na(common),
         !is.na(class)) %>% 
  
  # changing scientific to Genus_species, subspecies, genus format
  rowwise() %>% 
  mutate(scientific = read_br(gsub('\r?\n|\r', ' ', common))) %>%
  mutate(scientific = gsub('  ', ' ', scientific)) %>% 
  mutate(binomial = paste(unlist(strsplit(scientific,
                                          split = ' '))[1:2],
                          collapse = '_'),
         subspecies = ifelse(str_count(scientific, ' ') == 2,
                             unlist(strsplit(scientific,
                                             split = ' '))[3],
                             NA),
         genus = paste(unlist(strsplit(scientific,
                                       split = ' '))[1],
                       collapse = '_')) %>% 
  ungroup() %>% 
  mutate(year = 2012)

# 2013
raw_exp_2013 <- read_excel(here::here('dataset',
                                      'expen/expenditure_2013.xlsx')) %>% 
  select(-starts_with('...'))

exp_2013 <- raw_exp_2013 %>% 
  clean_names() %>% 
  rename(common = species_sci) %>% 
  filter(!is.na(common),
         !is.na(class)) %>% 
  
  # changing scientific to Genus_species, subspecies, genus format
  rowwise() %>% 
  mutate(scientific = read_br(gsub('\r?\n|\r', ' ', common))) %>%
  mutate(scientific = gsub('  ', ' ', scientific)) %>% 
  mutate(binomial = paste(unlist(strsplit(scientific,
                                          split = ' '))[1:2],
                          collapse = '_'),
         subspecies = ifelse(str_count(scientific, ' ') == 2,
                             unlist(strsplit(scientific,
                                             split = ' '))[3],
                             NA),
         genus = paste(unlist(strsplit(scientific,
                                       split = ' '))[1],
                       collapse = '_')) %>% 
  ungroup() %>% 
  mutate(year = 2013)

# 2014
raw_exp_2014 <- read_excel(here::here('dataset',
                                      'expen/expenditure_2014.xlsx')) %>% 
  select(-starts_with('...'))

exp_2014 <- raw_exp_2014 %>% 
  clean_names() %>% 
  rename(common = species_sci) %>% 
  filter(!is.na(common),
         !is.na(class)) %>% 
  
  # changing scientific to Genus_species, subspecies, genus format
  rowwise() %>% 
  mutate(scientific = read_br(gsub('\r?\n|\r', ' ', common))) %>%
  mutate(scientific = gsub('  ', ' ', scientific)) %>% 
  mutate(binomial = paste(unlist(strsplit(scientific,
                                          split = ' '))[1:2],
                          collapse = '_'),
         subspecies = ifelse(str_count(scientific, ' ') == 2,
                             unlist(strsplit(scientific,
                                             split = ' '))[3],
                             NA),
         genus = paste(unlist(strsplit(scientific,
                                       split = ' '))[1],
                       collapse = '_')) %>% 
  ungroup() %>% 
  mutate(year = 2014)

# 2015
raw_exp_2015 <- read_excel(here::here('dataset',
                                      'expen/expenditure_2015.xlsx')) %>% 
  select(-starts_with('...'))

exp_2015 <- raw_exp_2015 %>% 
  clean_names() %>% 
  rename(common = species_sci) %>% 
  filter(!is.na(common),
         !is.na(class)) %>% 
  
  # changing scientific to Genus_species, subspecies, genus format
  rowwise() %>% 
  mutate(scientific = read_br(gsub('\r?\n|\r', ' ', common))) %>%
  mutate(scientific = gsub('  ', ' ', scientific)) %>% 
  mutate(binomial = paste(unlist(strsplit(scientific,
                                          split = ' '))[1:2],
                          collapse = '_'),
         subspecies = ifelse(str_count(scientific, ' ') == 2,
                             unlist(strsplit(scientific,
                                             split = ' '))[3],
                             NA),
         genus = paste(unlist(strsplit(scientific,
                                       split = ' '))[1],
                       collapse = '_')) %>% 
  ungroup() %>% 
  mutate(year = 2015)

# 2016
raw_exp_2016 <- read_excel(here::here('dataset',
                                      'expen/expenditure_2016.xlsx')) %>% 
  select(-starts_with('...'))

exp_2016 <- raw_exp_2016 %>% 
  clean_names() %>% 
  rename(common = species_sci) %>% 
  filter(!is.na(common),
         !is.na(class)) %>% 
  
  # changing scientific to Genus_species, subspecies, genus format
  rowwise() %>% 
  mutate(scientific = read_br(gsub('\r?\n|\r', ' ', common))) %>%
  mutate(scientific = gsub('  ', ' ', scientific)) %>% 
  mutate(binomial = paste(unlist(strsplit(scientific,
                                          split = ' '))[1:2],
                          collapse = '_'),
         subspecies = ifelse(str_count(scientific, ' ') == 2,
                             unlist(strsplit(scientific,
                                             split = ' '))[3],
                             NA),
         genus = paste(unlist(strsplit(scientific,
                                       split = ' '))[1],
                       collapse = '_')) %>% 
  ungroup() %>% 
  mutate(year = 2016)

# 2017
raw_exp_2017 <- read_excel(here::here('dataset',
                                      'expen/expenditure_2017.xlsx')) %>% 
  select(-starts_with('...'))

exp_2017 <- raw_exp_2017 %>% 
  clean_names() %>% 
  rename(common = species_sci) %>% 
  filter(!is.na(common),
         !is.na(class)) %>% 
  
  # changing scientific to Genus_species, subspecies, genus format
  rowwise() %>% 
  mutate(scientific = read_br(gsub('\r?\n|\r', ' ', common))) %>%
  mutate(scientific = gsub('  ', ' ', scientific)) %>% 
  mutate(binomial = paste(unlist(strsplit(scientific,
                                          split = ' '))[1:2],
                          collapse = '_'),
         subspecies = ifelse(str_count(scientific, ' ') == 2,
                             unlist(strsplit(scientific,
                                             split = ' '))[3],
                             NA),
         genus = paste(unlist(strsplit(scientific,
                                       split = ' '))[1],
                       collapse = '_')) %>% 
  ungroup() %>% 
  mutate(year = 2017)

# -----
# expenditures as time series

# function to select specific variables
exp_select <- function (data) {
  
  # selecting
  sorted <- data %>% 
    select(class, binomial, subspecies, fws_total:species_total, year)
  
  # forcing column types
  final <- type_convert(sorted, col_types = 'cccnnnnni')
  return(final)
}

# all clean expenditures in a list
exp_list <- list(exp_2000,
                 exp_2001,
                 exp_2002,
                 exp_2003,
                 exp_2004,
                 exp_2005,
                 exp_2006,
                 exp_2007,
                 exp_2008,
                 exp_2009,
                 exp_2010,
                 exp_2011,
                 exp_2012,
                 exp_2013,
                 exp_2014,
                 exp_2015,
                 exp_2016,
                 exp_2017) %>%
  lapply(exp_select)

# vertical join to convert to time series
exp_ts <- map_df(exp_list, rbind)

# ---------------
# LPI and expenditure time series
# all time series data merged

merge_ts <- lpi_red_ts %>% 
  left_join(., exp_ts, by = c('year', 'binomial', 'class')) %>% 
  
  # adding normalized counts
  group_by(id) %>% 
  mutate(norm_count = (count-mean(count))/sd(count)) %>% 
  ungroup()

# converting class to factor
merge_ts$class <- as.factor(merge_ts$class)

# ---------------
# FWS recovery data merged with the rest

# basic data
fws_data <- read_csv(here::here('dataset', 'FWS_full.csv'),
                     col_types = 'cccccccccc') %>%
  clean_names() %>% 
  rowwise() %>%
  mutate(scientific = read_br(scientific_name)) %>% 
  mutate(binomial = paste(unlist(strsplit(scientific,
                                          split = ' '))[1:2],
                          collapse = '_'),
         subspecies = ifelse(str_count(scientific, ' ') == 2,
                             unlist(strsplit(scientific,
                                             split = ' '))[3],
                             NA)) %>% 
  ungroup()

# recovery plan data
fws_recovery <- read_csv(here::here('dataset', 'FWS_recovery.csv'),
                         col_types = 'cccccccccccc') %>%
  clean_names() %>% 
  rowwise() %>%
  mutate(scientific = read_br(species_scientific_name)) %>% 
  mutate(binomial = paste(unlist(strsplit(scientific,
                                          split = ' '))[1:2],
                          collapse = '_'),
         subspecies = ifelse(str_count(scientific, ' ') == 2,
                             unlist(strsplit(scientific,
                                             split = ' '))[3],
                             NA)) %>% 
  ungroup() %>% 
  select(scientific:subspecies,
         recovery_document_title,
         recovery_document_date:region_name)

# all datasets merged together
full_ts <- left_join(exp_ts,
                     fws_data,
                     by = 'binomial') %>%
  distinct(.keep_all = TRUE) %>%
  filter(!is.na(scientific_name)) %>%
  left_join(fws_recovery, by = 'binomial')

# small fix to maintain consistency
full_ts$class <- gsub('Amphibias', 'amphibia', full_ts$class)

# ---------------
# saving clean data

write_csv(lpi_ts, here::here('dataset','clean/lpi_ts.csv'))
write_csv(lpi_red_ts, here::here('dataset','clean/lpi_red_ts.csv'))
write_csv(exp_ts, here::here('dataset','clean/exp_ts.csv'))
write_csv(full_ts, here::here('dataset','clean/full_ts.csv'))
