################################################################################
############################  LIBRARIES  #######################################
################################################################################
library(tidyverse)
library(cmdstanr)
library(posterior)
library(data.table)
library(lubridate)
library(ggnewscale)
library(ggpubr)
################################################################################
############################  READ DATA  #######################################
################################################################################
fit <- read_cmdstan_csv(files = c("data/epi_1.csv","data/epi_2.csv"))
draws <-fit[['post_warmup_draws']]
sim_df <- as_draws_df(draws) %>% 
  as_tibble()

data <- read_rds('data/processed_data.rds')

df_region_codes <- read_csv('data/mexico-population.csv')


################################################################################
###################  PREPARING DATES DATAFRAME  ################################
################################################################################

fc <- 10

dates <- data[['dates']]

transf <- function(x, l){
  y <- as_tibble(l[[x]]) 
  z <- y %>% 
    mutate(country = x)
  return(z)
}

tmp <- lapply(names(dates), transf, dates)
# tmp_df <- do.call(rbind, tmp)
dates_df <- tmp %>% 
  rbindlist %>% 
  rename(date = value) %>% 
  mutate(date = as_date(date),
         country = as_factor(country))

dates_fc_df = dates_df %>%
  mutate(forecast = fc) %>% 
  group_by(country) %>% 
  filter(date == max(date)) %>% 
  group_by(date, country) %>% 
  complete(forecast = sequence(forecast)) %>% 
  mutate(date = date + forecast) %>% 
  select(-forecast) %>% 
  rbind(dates_df) %>% 
  group_by(country) %>% 
  arrange(country, date) %>% 
  mutate(day = row_number())

################################################################################
#######################  PREPARING DEATH DATA  #################################
################################################################################

deaths <- data[['deaths_by_country']]
deaths_sus <- data[['deaths_pos_sosp_country']]

tmp1 <- lapply(names(deaths), transf, deaths)
tmp2 <- lapply(names(deaths_sus), transf, deaths_sus)

deaths_df <- tmp1 %>% 
  rbindlist %>% 
  rename(Confirmed = value) %>% 
  cbind(tmp2 %>% 
          rbindlist %>% 
          rename(Suspected = value) %>% 
          select(-country)) %>% 
  mutate(country = as_factor(country)) %>% 
  group_by(country) %>% 
  mutate(
    day = row_number()) %>% 
  # select(-tmp) %>% 
  right_join(dates_fc_df,
             by = c('country', 'day')) %>% 
  pivot_longer(cols = c('Confirmed', 'Suspected'),
               names_to = 'status',
               values_to = 'y')

################################################################################
#######################  PREPARING PREDICTION DATA  ############################
################################################################################

hosp <- data[['hospi_by_country']]
hosp_sus <- data[['hospi_pos_sosp_country']]

tmp1 <- lapply(names(hosp), transf, hosp)
tmp2 <- lapply(names(hosp_sus), transf, hosp_sus)

hosp_df <- tmp1 %>% 
  rbindlist %>% 
  rename(Confirmed = value) %>% 
  cbind(tmp2 %>% 
          rbindlist %>% 
          rename(Suspected = value) %>% 
          select(-country)) %>% 
  mutate(country = as_factor(country)) %>% 
  group_by(country) %>% 
  mutate(
    day = row_number()) %>% 
  # select(-tmp) %>% 
  right_join(dates_fc_df,
             by = c('country', 'day')) %>% 
  pivot_longer(cols = c('Confirmed', 'Suspected'),
               names_to = 'status',
               values_to = 'y')


################################################################################
#######################  PREPARING PREDICTION DATA  ############################
################################################################################
countries <- c("AS","BC","BS","CC","CS","CH","DF","CL","CM","DG","GT","GR","HG",
               "JC","MC","MN","MS","NT","NL","OC","PL","QT","QR","SP","SL","SR",
               "TC","TL","TS","VZ","YN","ZS")

pred_df <- sim_df %>%
  select(starts_with(c('E_deaths', 'E_hosp', 'Rt_', 'prediction'))) %>% 
  as.matrix() %>% 
  matrixStats::colQuantiles(probs = c(0.5, 0.025, 0.975, 0.25, 0.75)) %>% 
  data.frame(variable = row.names(.), .) %>% 
  as_tibble() %>% 
  mutate(variable = trimws(
    str_replace_all(variable,
                    pattern = '[\\[\\],]',
                    replacement = ' '),
    which = 'right'
  )) %>% 
  separate(variable,
           into = c('var', 'day', 'n_country'),
           sep = ' ',
           convert = T,
           remove = T) %>% 
  mutate(country = as_factor(recode(n_country, !!!countries))) %>% 
  inner_join(dates_fc_df,
             by = c('country', 'day')) %>% 
  select(var, country, date, everything(), -day) %>% 
  ungroup %>% 
  rename(y = X50.,
         y_li = X2.5.,
         y_ui = X97.5.,
         y_li2 = X25.,
         y_ui2 = X75.)

pred_df_long <- pred_df %>% 
  select(var, country, date, y, y_li = y_li2, y_ui = y_ui2) %>% 
  mutate(credibility = '50%') %>% 
  rbind(
    pred_df %>% 
      select(var, country, date, y, y_li, y_ui) %>% 
      mutate(credibility = '95%')
  )


################################################################################
####################################  SAVING  ##################################
################################################################################

save(countries, pred_df_long, deaths_df, hosp_df, df_region_codes, fc,
     file="data/index_data.RData")




