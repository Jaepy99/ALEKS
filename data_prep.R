# 25-1 통계학 수업의 성적 및 ALEKS 사용간 상관관계에 대해서

##### Data 설명 #####
# 성적 반영비율 
# 출석:ALEKS:조별과제:중간1:중간2:중간3:기말
#   5:    10:       5:   20:   20:   20:  20

# 출석
# A:출석(Attendance), T:지각(Tardy), B:결석(Absence), E:조퇴(Early leave), U:미확인(Unidentified)
# 공휴일: 2025-03-03삼일절 대체 휴일, 2025-05-05어린이날, 석가탄신일, 
#         2025-05-06대체 휴일,	2025-06-03제21대 대통령선거, 2025-06-06현충일

# ALEKS 모듈
# Module 1 : introduction - Mar 12, 2025 - Closed
# Module 2 : descrubing data - Mar 19, 2025 - Closed
# Module 3 : introduction to probability - Mar 29, 2025 - Closed
# Module 4 : probability distribution of discrete random variables - Apr 9, 2025 - Closed
# Module 5 : normal distribution - Apr 16, 2025 - Closed
# Module 6 : sampling distributions - Apr 26, 2025 - Closed
# Module 7 : estimation - single population - May 7, 2025 - Closed
# Module 8 : hypothesis tests - single populations - May 14, 2025 - Closed
# Module 9 : hypothesis tests - single populations - May 24, 2025 - Closed
# Module 10 : inference making - two populations - May 24, 2025 - Closed
# Module 11 : hypothesis tests using the chi-square distribution - Jun 4, 2025 - Closed
# Module 12 : one-way analysis of variance - Jun 11, 2025 - Closed
# Module 13 : simple linear regression analysis - Jun 19, 2025 - Open

library(readxl)

sheet.names <- excel_sheets("25-spring.xlsx")
dt <- list()

for(i in 1:length(sheet.names)){
  dt[[i]] <- read_excel("25-spring.xlsx", sheet = sheet.names[i])
}

library(dplyr)
library(tidyr)

grade <- dt[[1]]
lms <- dt[[2]]
attendance <- dt[[3]]
aleks <- dt[[4]]

##### data prep #####
grade.lms <- grade %>% 
  select(-중간1 & -중간2 & -중간3 & -기말 & -조별문제) %>% 
  left_join(lms, by = 'id') %>% 
  filter(id != 99) %>% 
  select(-week_12) %>% 
  pivot_longer(cols = -c(id, 숙제합계, 성적, 평점, 출석, aleks, total),
               names_to = "week", values_to = "score") %>% 
  mutate(week = factor(week, levels = c("week_2", "week_3", "week_5","중간1", 
                                        "week_6", "week_7", "중간2", "week_9", 
                                        "week_10", "week_11", "중간3", "week_13", 
                                        "week_14", "week_15", "기말")))


dt.prep <- list(grade.lms, attendance, aleks)

save(dt.prep, file = "25-spring.Rdata")
