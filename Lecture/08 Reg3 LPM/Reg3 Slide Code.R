library(data.table)
library(tidyverse)
library(latex2exp)

x = seq(-2*pi, 2*pi, length = 1000)
y_sin = sin(x)

plot(x, y_sin)
abline(a = 0, b = 1)

x2 = seq(-1, 1, length = 1000)
y = x2

df_plot = bind_rows(tibble(x = x,
                         y = y_sin,
                         group = 'y = sin(x)'),
                  tibble(x = x2,
                         y = y,
                         group = 'y = x'))

ggplot(df_plot, aes(x = x, y = y, color = group)) +
  geom_rect(xmin = -1.25, xmax = 1.25, ymin = -1, ymax = 1,
            color = 'lightgrey', mapping = aes(fill = 'data')) +
  geom_line(size = 1.5) + 
  scale_fill_manual(values = c('data' = 'lightgrey')) +
  scale_x_continuous(breaks = seq(-2*pi, 2*pi, by = pi),
                     labels = TeX(c('-2\\pi', '-\\pi', '0', '\\pi', '2\\pi'))) +
  theme_minimal() +
  theme(text = element_text(size = 20)) +
  labs(color = '', fill = '') 





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

## -------------------------- ##
#### Logit vs OLS: unbiased ####
## -------------------------- ##
fit1 = glm(in_lf ~ age, data = acs2,
           family = binomial)
df_fit = data.frame(yhat = fit1$fitted.values,
                    x = fit1$model$age) %>%
  distinct() %>% arrange(x)

ggplot(acs2, aes(x = age, y = in_lf)) +
  geom_bin_2d(bins = 10) +
  geom_line(data = df_fit, aes(x = x, y = yhat, color = 'Logit'), size = 1.1) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.1, linetype = 'dashed',
              aes(color = 'OLS')) +
  scale_color_manual(values = c('Logit' = '#E84A27', 'OLS' = 'blue')) +
  scale_fill_continuous(guide = 'none') +
  scale_x_continuous(breaks = c(25, 50, 75)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20)) +
  labs(x = 'Age', y = 'Labor Force Participant',
       title = 'Labor Force Participant vs Age',
       color = '') 

ggsave('Lecture/08 Reg3 LPM/LF age logit vs OLS.png',
       width = 16/2, height = 9/2)





