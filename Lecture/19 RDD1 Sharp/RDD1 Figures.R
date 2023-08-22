df = read_csv('C:/Users/JJPWade/Dropbox/Julian/Academia/UIUC/Semester 12 Spring 2023/ECON474sp23/Data/RDD1 ACS5 2019 median income house value.csv')

df = df %>%
  rename(med_house_value = SE_A10036_001,
         med_hh_income = SE_A14006_001)

fig1 = ggplot(df, aes(x = log(med_house_value), y = log(med_hh_income))) +
  geom_point() +
  theme(text = element_text(size = 20)) +
  labs(x = 'log Median House Value ($)',
       y = 'log Median Household Income ($)',
       title = 'Tract-level Median Household Income versus Median Household Value',
       subtitle = 'Source: ACS5 2019')
fig1



ggplot(df, aes(x = log(med_house_value), y = log(med_hh_income))) +
  stat_summary_bin(fun = mean, bins = 20,
                   size = 1.5) +
  theme(text = element_text(size = 20)) +
  labs(x = 'log Median House Value ($)',
       y = 'log Median Household Income ($)',
       title = 'Tract-level Median Household Income versus Median Household Value',
       subtitle = 'Source: ACS5 2019') +
  ylim(layer_scales(fig1)$y$range$range)


ggplot(df, aes(x = log(med_house_value), y = log(med_hh_income))) +
  geom_bin_2d() +
  stat_summary_bin(fun = mean, bins = 20,
                   color = 'orange',
                   size = 1.5) +
  scale_fill_continuous(type = "viridis") +
  theme(text = element_text(size = 20)) +
  labs(x = 'log Median House Value ($)',
       y = 'log Median Household Income ($)',
       title = 'Tract-level Median Household Income versus Median Household Value',
       subtitle = 'Source: ACS5 2019')



{
set.seed(42)
x = 1:24 - 13
y = 2 + x*0.1 + c(rep(0, 12), rep(3, 12)) + rnorm(24, sd = 0.25)

df_plot = data.frame(x = x + 0.5, 
                     y = y,
                     d = (x >= 0)*1)
}

library(cowplot)
p1 = ggplot(df_plot, aes(x = x, y = y, group = d)) +
  geom_point() +
  geom_smooth(method = 'lm',
              formula = y ~ poly(x, 1),
              se = FALSE)
p11 = ggplot(df_plot, aes(x = x, y = y, group = d)) +
  geom_point() +
  geom_smooth(method = 'lm',
              formula = y ~ poly(x, 11),
              se = FALSE)

plot_grid(p1, p11, nrow = 2)


library(fixest)

df_2 = df_plot %>%
  mutate(x_plus_30 = x + 30)

feols(y ~ sw(x*d, x_plus_30*d), data = df_2) %>%
  etable()




