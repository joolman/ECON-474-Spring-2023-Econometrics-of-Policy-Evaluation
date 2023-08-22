library(tidyverse)
library(stargazer)
library(xtable)

setwd('C:/Users/johnj/Dropbox/Julian/Academia/UIUC/Semester 11 Fall 2022/ECON474/Lecture Materials/07 Directed Acyclic Graphs/')

tb <- tibble(
  female = ifelse(runif(10000)>=0.5,1,0),
  ability = rnorm(10000),
  discrimination = female,
  occupation = 1 + 2*ability + 0*female - 2*discrimination + rnorm(10000),
  wage = 1 - 1*discrimination + 1*occupation + 2*ability + rnorm(10000) 
)

lm_1 <- lm(wage ~ female, tb)
lm_2 <- lm(wage ~ female + occupation, tb)
lm_3 <- lm(wage ~ female + occupation + ability, tb)

stargazer(lm_1,lm_2,lm_3, type = "latex", 
          column.labels = c("Biased Unconditional", 
                            "Biased",
                            "Unbiased Conditional"),
          dep.var.labels = 'Wage',
          title = 'Simulated Discrimination',
          omit.stat = c('adj.rsq', 'f', 'ser'),
          out = 'sim gender gap.tex')



set.seed(3444)

library(cowplot)

star_is_born <- tibble(
  beauty = rnorm(2500),
  talent = rnorm(2500),
  score = beauty + talent,
  c85 = quantile(score, .85),
  star = ifelse(score>=c85,1,0)
)

g1 = star_is_born %>% 
  lm(beauty ~ talent, .) %>% 
  ggplot(aes(x = talent, y = beauty)) +
  geom_point(size = 0.5, shape=23) + xlim(-4, 4) + ylim(-4, 4) +
  labs(title = 'Aspiring Actors and Actresses')

g2 = star_is_born %>% 
  filter(star == 1) %>% 
  lm(beauty ~ talent, .) %>% 
  ggplot(aes(x = talent, y = beauty)) +
  geom_point(size = 0.5, shape=23) + xlim(-4, 4) + ylim(-4, 4) + 
  labs(title = 'Stars')

g3 = star_is_born %>% 
  filter(star == 0) %>% 
  lm(beauty ~ talent, .) %>% 
  ggplot(aes(x = talent, y = beauty)) +
  geom_point(size = 0.5, shape=23) + xlim(-4, 4) + ylim(-4, 4) +
  labs(title = 'Gas Giants')

plot_grid(g1, g2, g3, nrow = 1)
ggsave('A Star is Born.png', width = 10, height = 4)
