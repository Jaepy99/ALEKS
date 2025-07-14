##### Exploratory Data Analysis #####
load("25-spring.Rdata")

grade.lms <- dt.prep[[1]]
attendance <- dt.prep[[2]]
aleks.prep <- dt.prep[[3]]

library(ggplot2)
library(dplyr)
library(tidyr)
### grade
# A tibble: 6 × 2
#   성적      n
#   <chr> <int>
# 1 A+       16
# 2 A0        9
# 3 B+       13
# 4 B0        9
# 5 C+        7

# 성적과 aleks
grade.lms %>%
  ggplot(aes(x = 성적, y = aleks)) +
  geom_boxplot()

# 성적과 과제
grade.lms %>%
  ggplot(aes(x = 성적, y = 숙제합계)) +
  geom_boxplot()

# 시험 성적 분포
grade.lms %>%
  filter(week == '중간1' | week == '중간2' | week == '중간3' | week == '기말') %>%
  ggplot(aes(x = week, y = score)) +
  geom_boxplot()

# 등급별 시험 성적 분포
grade.lms %>%
  filter(week == '중간1' | week == '중간2' | week == '중간3' | week == '기말') %>%
  ggplot(aes(x = week, y = score)) +
  geom_boxplot() +
  facet_wrap(~성적)

# 과제 점수 분포
grade.lms %>%
  filter(week != '중간1' & week != '중간2' & week != '중간3' & week != '기말') %>%
  ggplot(aes(x = week, y = score)) +
  geom_boxplot() +
  facet_wrap(~성적)

# aleks 모듈별 학습정도 비교
aleks.prep %>%
  filter(module != "Total_Grade") %>%
  ggplot(aes(x = module, y = master)) +
  geom_boxplot()

# aleks 학습정도와 성적의 관계 시각화
grade %>%
  select(id, total) %>%
  left_join(aleks.prep) %>%
  filter(module == "Total_Grade") %>%
  ggplot(aes(x = master, y = total)) +
  geom_point()

##### clustering #####
library(cluster)
### Euclidean
grade.eu <- grade.lms %>%
  pivot_wider(names_from = week, values_from = score, values_fill = NA) %>%
  select(-id & -성적 & -평점 & -숙제합계 & -total) %>%
  dist(method = "euclidean")

# 계층적 군집분석
grade.eu.hc <- hclust(grade.eu, method = "ward.D2")

# 덴드로그램 시각화
plot(grade.eu.hc, labels = F, main = "Dendrogram by Euclidean")

### k = 3
eu.cl <- grade.lms %>%
  pivot_wider(names_from = week, values_from = score, values_fill = NA)
eu.cl$cluster <- cutree(grade.eu.hc, k = 3)

eu.sil <- silhouette(eu.cl$cluster, grade.eu)

mean(eu.sil[,3])
# result = 0.2791413
# 실루엣 계수의 평균값이 낮으므로 군집화가 잘 되었다고 할 수 없다.


### Correlation
library(proxy)

grade.cor <- grade.lms %>%
  pivot_wider(names_from = week, values_from = score, values_fill = NA) %>%
  select(-id & -성적 & -평점 & -숙제합계 & -total) %>%
  dist(method = "correlation")

# 계층적 군집분석
grade.cor.hc <- hclust(grade.cor, method = "ward.D2")

# 덴드로그램 시각화
plot(grade.cor.hc, labels = F, main = "Dendrogram by Correlation")

### k = 3
cor.cl <- grade.lms %>%
  pivot_wider(names_from = week, values_from = score, values_fill = NA)
cor.cl$cluster <- cutree(grade.cor.hc, k = 3)

cor.sil <- silhouette(cor.cl$cluster, grade.cor)

mean(cor.sil[,3])
# result = 0.3269147
# 실루엣 계수의 평균값이 낮으므로 군집화가 잘 되었다고 할 수 없다.


