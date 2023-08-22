library(tidyverse)

titanic = read_csv('Data/RE2 titanic3.csv')
names(titanic)

# Cleaning
titanic = titanic %>%
  select(pclass, survived, sex, age, body) %>%
  mutate(body = ifelse(is.na(body), 0 , body),
         age = ceiling(age)) %>%
  na.omit()


# CReating treatment variable
titanic = titanic %>% 
  mutate(d = (pclass == 1)*1,
         child = (age < 13)*1)

titanic %>%
  group_by(sex, child, d) %>%
  summarise(survival_prob = mean(survived),
            n_group = n()) %>%
  pivot_wider(names_from = d, values_from = c(survival_prob, n_group)) %>%
  mutate(diff_prob = survival_prob_1 - survival_prob_0,
         prob_s = (n_group_0 + n_group_1)/nrow(titanic)) %>%
  data.frame() 

titanic %>%
  select(survived, d, sex, child) %>% # unecessary, just for console output demonstration
  mutate(wt_denom = n()) %>%
  group_by(sex, child) %>%
  mutate(wt_num = n(),
         wt = wt_num/wt_denom) %>%
  group_by(d, sex, child) %>%
  summarise(y_s = mean(survived),
            wt = unique(wt),
            .groups = 'drop') %>%
  pivot_wider(id_cols = wt,
              names_from = d,
              values_from = y_s,
              names_prefix = 'y_') %>%
  mutate(diff = y_1 - y_0) %>%
  summarise(w_ate = weighted.mean(diff, wt)) %>%
  pull(w_ate)


titanic %>%
  group_by(sex, age, d) %>%
  summarise(survival_prob = mean(survived),
            n_group = n()) %>%
  pivot_wider(names_from = d, values_from = c(survival_prob, n_group)) %>% # don't show yet
  mutate(diff = survival_prob_1 - survival_prob_0) %>% #show
  data.frame() %>% head
