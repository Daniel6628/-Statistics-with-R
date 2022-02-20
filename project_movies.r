library(statsr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(GGally)

data(evals)
str(evals)
hist(evals$score)

boxplot(evals$score)

evals %>% 
  group_by(score>='4.6') %>% 
  summarise(count = n(), ratio = count/463)

evals %>% 
  group_by(score<'3') %>% 
  summarise(count = n(), ratio = count/463)

summary(evals$score)
boxplot(evals$age, evals$bty_avg)

ggplot(evals, aes(x = age, y = bty_avg))+
  geom_point()

m_bty <- lm(score ~ bty_avg, data = evals)
summary(m_bty)


ggplot(m_bty, aes(x = .fitted, y = .resid))+
  geom_point()+
  geom_hline(yintercept = 0, linetype = 'dashed')+
  xlab('Fitted Values')+
  ylab('Residuals')

ggpairs(evals, columns = 13:19)

m_bty_gen <- lm(score ~ bty_avg + gender, data = evals)

ggplot(m_bty_gen, aes(x = .fitted, y = .resid))+
  geom_point()+
  geom_hline(yintercept = 0, linetype = 'dashed')+
  xlab('Fitted Values')+
  ylab('Residuals')

ggplot(m_bty_gen, aes(x = .resid)) +
  geom_histogram(bins = 25)+
  xlab('Residuals')

hist(m_bty_gen$residuals)

qqnorm(m_bty_gen$residuals)
qqline(m_bty_gen$residuals, col = 2)

summary(m_bty_gen)


m_full <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval 
             + cls_students + cls_level + cls_profs + cls_credits + bty_avg 
             + pic_outfit + pic_color, data = evals)


step(m_full, direction = 'backward', trace = FALSE)
