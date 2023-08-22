library(haven)
library(tidyverse)
library(fixest)

df = read_dta('C:/Users/johnj/Dropbox/Julian/Academia/UIUC/Semester 11 Fall 2022/ECON474/Final Project/Reimers 2019/amazon_data.dta')
df = df %>%
  filter(!is.na(plr))
# write_dta(df, 'C:/Users/johnj/Dropbox/Julian/Academia/UIUC/Semester 11 Fall 2022/ECON474/Final Project/Reimers 2019/amazon_data.dta')

fit_tab2_1 = feols(ntitle ~ post1923 + plr + prize_author + canon_title + canon_author + poly(year,3),
      data = df, 
      cluster = ~ year,
      subset = ~ year >= -8 &
        year <= 7 &
        format == "Hardcover")

deltas = coef(fit_tab2_1)[2]
pre_y0 = df %>%
  filter(year %in% -8:-1 & format == "Hardcover") %>%
  pull(ntitle) %>%
  mean

fit_tab2_2_4 = list()
formats = c('Hardcover', 'Paperback', 'E-book')
for(i in 1:length(formats)){
  f = formats[i]
  fit_tab2_2_4[[i]] = feols(ntitleformat ~ post1923 + plr + prize_author + canon_title + canon_author + poly(year,3),
                         data = df, 
                         cluster = ~ year,
                         subset = df$year %in% -8:7 & df$format == formats[i])
  
  pre_y0 = c(pre_y0, 
             (df %>%
                filter(year %in% -8:-1 & format == formats[i]) %>%
                pull(ntitleformat) %>%
                mean))
  
  deltas = c(deltas, 
             coef(fit_tab2_2_4[[i]])[2])
}

effect_size = deltas/pre_y0


etable(fit_tab2_1, fit_tab2_2_4,
       order = "!Intercept",
       fitstat = ~ n + ar2,
       extralines = list("_^Effect Size" = effect_size))



df_plot = df %>%
  group_by(year) %>%
  summarise(ntitle = mean(ntitle),
            year = unique(year + 1923)) %>%
  mutate(group = year >= 1923)

ggplot(df_plot, aes(x = year, y = ntitle, group = group)) +
  geom_smooth(method = 'lm',
              formula = y ~ poly(x, 3)) +
  geom_vline(xintercept = 1922.5, linetype = 'dashed')






