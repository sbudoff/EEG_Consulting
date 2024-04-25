library(tidyverse)

canna_df <- read.csv('/home/sam/Downloads/anonymized data - Sheet1.csv')


summ <- canna_df %>%
  filter(P != 1, Q != ".") %>%
  select(X, Q) %>%
  group_by(X,Q) %>%
  summarise(n())

sum(summ$`n()`)
