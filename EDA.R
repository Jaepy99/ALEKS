##### Exploratory Data Analysis #####
load("25-spring.Rdata")

grade.lms <- dt.prep[[1]]
attendance <- dt.prep[[2]]
aleks <- dt.prep[[3]]

library(ggplot2)
### grade
# A tibble: 6 × 2
#   성적      n
#   <chr> <int>
# 1 A+       16
# 2 A0        9
# 3 B+       13
# 4 B0        9
# 5 C+        7

grade.lms %>% 
  ggplot(aes(x = 성적, y = aleks)) +
  geom_boxplot()

grade.lms %>%
  ggplot(aes(x = 성적, y = 숙제합계)) +
  geom_boxplot()

grade.lms %>% 
  filter(week == '중간1' | week == '중간2' | week == '중간3' | week == '기말') %>% 
  ggplot(aes(x = week, y = score)) +
  geom_boxplot()

grade.lms %>%
  filter(week == '중간1' | week == '중간2' | week == '중간3' | week == '기말') %>% 
  ggplot(aes(x = week, y = score)) +
  geom_boxplot() +
  facet_wrap(~성적)
