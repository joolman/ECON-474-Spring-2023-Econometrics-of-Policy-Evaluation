library(haven)
library(tidyverse)
library(fixest)



df = read_dta('C:/Users/johnj/Dropbox/Julian/Academia/UIUC/Semester 11 Fall 2022/ECON474/Data/RDD1 Lindo et al 2010.dta')
df$dist_from_cut
summary(df$dist_from_cut)


df = df %>%
  mutate(d_year1 = as.factor(dist_from_cut < 0))



df_plot = df %>%
  filter(dist_from_cut < -0.05 | dist_from_cut > 0.05) %>%
  mutate(dist_from_cut_bin = round(dist_from_cut, 1)) %>%
  group_by(dist_from_cut_bin, d_year1) %>%
  summarise(freq = n(), # density check
            probation_year1 = mean(probation_year1), # first stage
            # outcomes
            left_school = mean(left_school),
            nextGPA = mean(nextGPA, na.rm = TRUE), # those who left school do not have nextGPA
            gradin4 = mean(gradin4, na.rm = TRUE),
            gradin5 = mean(gradin5, na.rm = TRUE),
            gradin6 = mean(gradin6, na.rm = TRUE)) %>%
  filter(dist_from_cut_bin >= -1.2 & dist_from_cut_bin <= 1.2)

ggplot(df_plot, aes(x = dist_from_cut_bin, y = freq, color = factor(d_year1))) +
  geom_point() +
  geom_smooth(method = 'lm',
              formula = y ~ poly(x, 2) + (x > 0),
              se = FALSE) # they are not the SEs from regression



feols(freq ~ poly(dist_from_cut_bin, 2) + d_year1, data = df_plot)



# Table 2


# Covariate balance
fit_tab2 = feols(c(hsgrade_pct, totcredits_year1, male, age_at_entry, bpl_north_america,
                   english, loc_campus1, loc_campus2) ~ 
                   gpalscutoff + dist_from_cut, 
                 data = df, cluster = 'clustervar',
                 subset = df$dist_from_cut >= -0.6 & df$dist_from_cut <= 0.6) 

etable(fit_tab2,
       se.below = TRUE,
       keep = c('gpalscutoff', 'Intercept'),
       fitstat = c('n', 'r2'))



ggplot(df_plot, aes(x = dist_from_cut_bin, y = probation_year1, color = d_year1)) +
  geom_point() +
  geom_smooth(method = 'lm',
              formula = y ~ poly(x, 2) + (x > 0),
              se = FALSE)


# Table 3 heterogeneity first-stage effects
subsets = 
  list(subset_all = df$dist_from_cut >= -0.6 & df$dist_from_cut <= 0.6,
       subset_lowhs = df$dist_from_cut >= -0.6 & df$dist_from_cut <= 0.6 & df$lowHS,
       subset_highhs = df$dist_from_cut >= -0.6 & df$dist_from_cut <= 0.6 & df$highHS,
       subset_male = df$dist_from_cut >= -0.6 & df$dist_from_cut <= 0.6 & df$male,
       subset_female = df$dist_from_cut >= -0.6 & df$dist_from_cut <= 0.6 & df$female,
       subset_english = df$dist_from_cut >= -0.6 & df$dist_from_cut <= 0.6 & df$english,
       subset_noenglish = df$dist_from_cut >= -0.6 & df$dist_from_cut <= 0.6 & df$noenglish)

fit_tab3a = list()
for(s in 1:length(subsets)){
  fit_tab3a[[s]] = feols(probation_year1 ~ gpalscutoff + dist_from_cut, data = df, cluster = 'clustervar', subset = subsets[[s]])
}

columns = c('All', 'HS grades < med', 'HS grades > med', 'Male', 'Female', 'Native English', 'Nonnative English')

etable(fit_tab3a[[1]], fit_tab3a[[2]], fit_tab3a[[3]], fit_tab3a[[4]], fit_tab3a[[5]], fit_tab3a[[6]], fit_tab3a[[7]],
       se.below = TRUE,
       keep = c('gpalscutoff', 'Intercept'),
       fitstat = c('n', 'r2'),
       headers = columns)

 

# Left school
ggplot(df_plot, aes(x = dist_from_cut_bin, y = left_school, color = d_year1)) +
  geom_point() +
  geom_smooth(method = 'lm',
              formula = y ~ poly(x, 2) + (x > 0),
              se = FALSE)


# Tab4 Left school
fit_tab4 = list()
for(s in 1:length(subsets)){
  fit_tab4[[s]] = feols(left_school ~ gpalscutoff + dist_from_cut, data = df, cluster = 'clustervar', subset = subsets[[s]])
}

columns = c('All', 'HS grades < med', 'HS grades > med', 'Male', 'Female', 'Native English', 'Nonnative English')

etable(fit_tab4[[1]], fit_tab4[[2]], fit_tab4[[3]], fit_tab4[[4]], fit_tab4[[5]], fit_tab4[[6]], fit_tab4[[7]],
       se.below = TRUE,
       keep = c('gpalscutoff', 'Intercept'),
       fitstat = c('n', 'r2'),
       headers = columns)


# The sample changed. The remaining results are conditional on not dropping out

ggplot(df_plot, aes(x = dist_from_cut_bin, y = nextGPA, color = d_year1)) +
  geom_point() +
  geom_smooth(method = 'lm',
              formula = y ~ poly(x, 2) + (x > 0),
              se = FALSE)


# tab5 
fit_tab5 = list()
for(s in 1:length(subsets)){
  fit_tab5[[s]] = feols(nextGPA ~ gpalscutoff + dist_from_cut, data = df, cluster = 'clustervar', subset = subsets[[s]])
}


etable(fit_tab5[[1]], fit_tab5[[2]], fit_tab5[[3]], fit_tab5[[4]], fit_tab5[[5]], fit_tab5[[6]], fit_tab5[[7]],
       se.below = TRUE,
       keep = c('gpalscutoff', 'Intercept'),
       fitstat = c('n', 'r2'),
       headers = columns)


# impacts on graduation

# tab6a graduating faster
fit_tab6a = list()
for(s in 1:length(subsets)){
  fit_tab6a[[s]] = feols(gradin4 ~ gpalscutoff + dist_from_cut, data = df, cluster = 'clustervar', subset = subsets[[s]])
}


etable(fit_tab6a[[1]], fit_tab6a[[2]], fit_tab6a[[3]], fit_tab6a[[4]], fit_tab6a[[5]], fit_tab6a[[6]], fit_tab6a[[7]],
       se.below = TRUE,
       keep = c('gpalscutoff', 'Intercept'),
       fitstat = c('n', 'r2'),
       headers = columns)


# tab6b graduating faster
fit_tab6b = list()
for(s in 1:length(subsets)){
  fit_tab6b[[s]] = feols(gradin5 ~ gpalscutoff + dist_from_cut, data = df, cluster = 'clustervar', subset = subsets[[s]])
}


etable(fit_tab6b[[1]], fit_tab6b[[2]], fit_tab6b[[3]], fit_tab6b[[4]], fit_tab6b[[5]], fit_tab6b[[6]], fit_tab6b[[7]],
       se.below = TRUE,
       keep = c('gpalscutoff', 'Intercept'),
       fitstat = c('n', 'r2'),
       headers = columns)



# tab6c graduating faster
fit_tab6c = list()
for(s in 1:length(subsets)){
  fit_tab6c[[s]] = feols(gradin6 ~ gpalscutoff + dist_from_cut, data = df, cluster = 'clustervar', subset = subsets[[s]])
}


etable(fit_tab6c[[1]], fit_tab6c[[2]], fit_tab6c[[3]], fit_tab6c[[4]], fit_tab6c[[5]], fit_tab6c[[6]], fit_tab6c[[7]],
       se.below = TRUE,
       keep = c('gpalscutoff', 'Intercept'),
       fitstat = c('n', 'r2'),
       headers = columns)

