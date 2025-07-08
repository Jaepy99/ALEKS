##### Exploratory Data Analysis #####
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

grade.prep %>% 
  ggplot(aes(x = 성적, y = aleks)) +
  geom_boxplot()

grade.prep %>% 
  ggplot(aes(x = 성적, y = 조별문제)) +
  geom_boxplot()

grade.prep %>% 
  ggplot(aes(x = test, y = score)) +
  geom_boxplot()

grade.prep %>% 
  ggplot(aes(x = test, y = score)) +
  geom_boxplot() +
  facet_wrap(~성적)

