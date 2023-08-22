library(data.table)
library(tidyverse)

acs = fread('data/Reg2 ACS 2021.csv')
names(acs) = acs %>% names %>% tolower

acs2 =
  acs %>%
  filter(gq %in% 1:2 &
           ownershp != 0 &
           age >= 16 &
           empstat > 0 & 
           ftotinc < 9999999 & 
           migrate1 > 0) %>%
  mutate(in_lf = (empstat != 3)*1,
         renter = (ownershp == 2)*1,
         has_children = (nchild > 0)*1,
         female = (sex == 2)*1,
         married = (marst <= 3)*1,
         black = (race == 2)*1,
         asian = (race %in% 4:5)*1,
         hipsanic = (hispan > 1)*1,
         degree = case_when(educd == 116 ~ 'Ph.D.',
                            educd == 115 ~ 'Professional',
                            educd == 114 ~ "Master's",
                            educd == 101 ~ "Bachelor's",
                            educd == 81 ~ "Associate's",
                            educd >= 63 ~ 'High School',
                            educd >= 2 ~ 'None'),
         yos = case_when(educd %in% 2:12  ~ 0,
                         educd == 14 ~ 1,
                         educd == 15 ~ 2,
                         educd == 16 ~ 3,
                         educd == 17 ~ 4,
                         educd == 22 ~ 5,
                         educd == 23 ~ 6,
                         educd == 25 ~ 7,
                         educd == 26 ~ 8,
                         educd == 30 ~ 9,
                         educd == 40 ~ 10,
                         educd == 50 ~ 11,
                         educd %in% c(61:65)  ~ 12,
                         educd == 71 ~ 13,
                         educd == 81 ~ 14,
                         educd == 101 ~ 16,
                         educd == 114 ~ 18,
                         educd == 115 ~ 20,
                         educd == 116 ~ 21),
         moved_ly = (migrate1 > 1)*1)
print(object.size(acs2), units = 'auto')

ggplot(acs2, aes(x = age, y = in_lf)) +
  geom_bin_2d(bins = 8) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20)) +
  scale_y_continuous(breaks = 0:1) +
  labs(x = 'Age', y = 'Labor Force Participant',
       title = 'Labor Force Participant vs Age')

ggsave('Lecture/07 Reg2 Logistic Regression/Labor Force Participant-Age.png',
       width = 16/2, height = 9/2)

## ------------------ ##
#### Logistic Shape ####
## ------------------ ##
x = seq(-5, 5, length = 1001)
y = plogis(x)
df_logit = data.frame(x = x,
                      y = y)

ggplot(df_logit, aes(x = x, y = y)) +
  geom_line(size = 1.1, color = '#E84A27') +
  geom_hline(yintercept = 1, linetype = 'dashed', size = 1.1) +
  geom_hline(yintercept = 0, linetype = 'dashed', size = 1.1) +
  theme_minimal() +
  scale_x_continuous(labels = NULL,
                     breaks = NULL) +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20))

ggsave('Lecture/07 Reg2 Logistic Regression/Logit shape.png',
       width = 16/2, height = 9/2)

## ---------------- ##
#### Logistic fit ####
## ---------------- ##
fit1 = glm(in_lf ~ age, data = acs2,
           family = binomial)
df_fit = data.frame(yhat = fit1$fitted.values,
       x = fit1$model$age) %>%
  distinct() %>% arrange(x)

ggplot(acs2, aes(x = age, y = in_lf)) +
  geom_bin_2d(bins = 10) +
  geom_line(data = df_fit, aes(x = x, y = yhat), size = 1.1, color = '#E84A27') +
  scale_x_continuous(breaks = c(25, 50, 75)) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20)) +
  labs(x = 'Age', y = 'Labor Force Participant',
       title = 'Labor Force Participant vs Age') 

ggsave('Lecture/07 Reg2 Logistic Regression/LF age logit.png',
       width = 16/2, height = 9/2)

## ------------------------ ##
#### Typical cutoff: 0.50 ####
## ------------------------ ##
cutoff = 0.50
age = df_fit %>%
  filter(yhat < cutoff) %>% 
  head(1) %>%
  pull(x)

ggplot(acs2, aes(x = age, y = in_lf)) +
  geom_bin_2d(bins = 10) +
  geom_line(data = df_fit, aes(x = x, y = yhat), size = 1.1, color = '#E84A27') +
  geom_hline(data = tibble(alpha = 0.50),
             aes(yintercept = alpha, linetype = "Cutoff: 0.50"),
             size = 1.1) +
  geom_vline(xintercept = age, size = 1.1) +
  scale_linetype_manual(values = c('Cutoff: 0.50' = 'dashed')) +
  scale_fill_continuous(guide = 'none') +
  scale_x_continuous(breaks = c(25, 50, 75)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20)) +
  labs(x = 'Age', y = 'Labor Force Participant',
       title = 'Labor Force Participant vs Age',
       linetype = '') 

ggsave('Lecture/07 Reg2 Logistic Regression/LF age logit cutoff 0.50.png',
       width = 16/2, height = 9/2)

## ------------------------ ##
#### Typical cutoff: 0.85 ####
## ------------------------ ##
cutoff = 0.85
age = df_fit %>%
  filter(yhat < cutoff) %>% 
  head(1) %>%
  pull(x)

ggplot(acs2, aes(x = age, y = in_lf)) +
  geom_bin_2d(bins = 10) +
  geom_line(data = df_fit, aes(x = x, y = yhat), size = 1.1, color = '#E84A27') +
  geom_hline(data = tibble(alpha = 0.85),
             aes(yintercept = alpha, linetype = "Cutoff: 0.85"),
             size = 1.1) +
  geom_vline(xintercept = age, size = 1.1) +
  scale_linetype_manual(values = c('Cutoff: 0.85' = 'dashed')) +
  scale_fill_continuous(guide = 'none') +
  scale_x_continuous(breaks = c(25, 50, 75)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20)) +
  labs(x = 'Age', y = 'Labor Force Participant',
       title = 'Labor Force Participant vs Age',
       linetype = '') 
ggsave('Lecture/07 Reg2 Logistic Regression/LF age logit cutoff 0.85.png',
       width = 16/2, height = 9/2)

## ------------------------ ##
#### Typical cutoff: 0.33 ####
## ------------------------ ##
cutoff = 0.33
age = df_fit %>%
  filter(yhat < cutoff) %>% 
  head(1) %>%
  pull(x)

ggplot(acs2, aes(x = age, y = in_lf)) +
  geom_bin_2d(bins = 10) +
  geom_line(data = df_fit, aes(x = x, y = yhat), size = 1.1, color = '#E84A27') +
  geom_hline(data = tibble(alpha = 0.33),
             aes(yintercept = alpha, linetype = "Cutoff: 0.33"),
             size = 1.1) +
  geom_vline(xintercept = age, size = 1.1) +
  scale_linetype_manual(values = c('Cutoff: 0.33' = 'dashed')) +
  scale_fill_continuous(guide = 'none') +
  scale_x_continuous(breaks = c(25, 50, 75)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20)) +
  labs(x = 'Age', y = 'Labor Force Participant',
       title = 'Labor Force Participant vs Age',
       linetype = '') 

ggsave('Lecture/07 Reg2 Logistic Regression/LF age logit cutoff 0.33.png',
       width = 16/2, height = 9/2)

##============##
## ---------- ##
#### Output ####
## ---------- ##
##============##

## --------- ##
#### Logit ####
## --------- ##
summary(fit1)
