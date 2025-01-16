
library(ggplot2)
library(tidyr)
library(dplyr)


crime <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/경찰청_연도별 사이버 범죄 통계 현황_08_31_2020.csv", 
                  fileEncoding = "CP949")


#Task1-1: 2020년 범죄 유형별 통계를 아래와 같이 나타내어라.

crime%>%
  filter(연도 == '2020')%>%
  gather(key = "범죄_유형",value = "건수", -연도, -구분)%>%
  ggplot( aes(x = 범죄_유형, y = as.numeric(건수))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ 구분, ) +
  labs(title = "2020년 사이버 범죄 유형별 발생건수 및 검거건수", x = "", y = "건수") +
  coord_flip()




crime%>%
  filter(연도 == '2020')%>%
  gather(key = "범죄_유형",value = "건수", -연도, -구분)



#Task1-2: 2014~20 년 합산 범죄 유형별 발생 건수 및 검거 건수를 아래와 같이 나타내어라.


crime %>%
  filter(연도 >= 2014 & 연도 <= 2020) %>%
  gather(key = "범죄_유형", value = "건수", -연도, -구분) %>%
  mutate(건수 = as.numeric(건수)) %>%
  filter(!is.na(건수)) %>%
  group_by(범죄_유형, 구분) %>%
  summarise(총건수 = sum(건수)) %>%  # 발생건수와 검거건수를 요약
  ggplot( aes(x = 범죄_유형, y = 총건수)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ 구분, ) +
  scale_y_continuous(labels = scales::scientific, breaks = c(0, 2e+05, 4e+05, 6e+05)) +  # Y축을 지수 표기법으로 설정 및 구간 정의
  labs(title = "2014~2020년 사이버 범죄 유형별 발생건수 및 검거건수", x = "", y = "건수") +
  coord_flip()







#Task1-3: 발생 건수별로 정렬된 범죄 유형별 그래프를 아래와 같이 그리시오.
crime %>%
  filter(연도 >= 2014 & 연도 <= 2020) %>%
  gather(key = "범죄_유형", value = "건수", -연도, -구분) %>%
  mutate(건수 = as.numeric(건수)) %>%
  filter(!is.na(건수)) %>%
  group_by(범죄_유형, 구분) %>%
  summarise(총건수 = sum(건수)) %>%  # 발생건수와 검거건수를 요약
  ggplot(aes(x = reorder(범죄_유형, 총건수), y = 총건수)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ 구분, ) +  # 구분(발생건수와 검거건수)을 나눠서 시각화
  scale_y_continuous(labels = scales::scientific, breaks = c(0, 2e+05, 4e+05, 6e+05)) +  # Y축을 지수 표기법으로 설정 및 구간 정의
  labs(title = "2014~2020년 합산 사이버 범죄 유형별 발생건수 및 검거건수", 
       x = "범죄 유형", y = "건수") +
  coord_flip()

#Task2-1: 사이버 범죄 유형의 수가 많아서 좀더 단순화된 통계를 나타내려고 한다. 아래와 같이 나타내보자
library(scales)
library(stringr) 
crime1 <- crime %>%
  filter(연도 >= 2014 & 연도 <= 2020) %>%
  gather(key = "범죄_유형", value = "건수", -연도, -구분) %>%
  mutate(범죄_유형 = as.character(범죄_유형)) %>%
  mutate(범죄_그룹 = case_when(
    str_detect(범죄_유형, "사이버사기.") ~ "사이버 사기",
    str_detect(범죄_유형, "명예훼손") ~ "사이버 명예훼손.모욕",
    str_detect(범죄_유형, "사이버.도박") ~ "사이버도박",
    str_detect(범죄_유형, "사이버금융범죄") ~ "사이버금융범죄",
    str_detect(범죄_유형, "사이버저작권침해") ~ "사이버저작권침해",
    str_detect(범죄_유형, "사이버.음란물") ~ "사이버음란물",
    str_detect(범죄_유형, "기타.정보통신망.이용형") ~ "기타.정보통신망.이용형.범죄",
    str_detect(범죄_유형, "해킹.") ~ "해킹",
    str_detect(범죄_유형, "개인위치정보") ~ "개인위치정보.침해",
    str_detect(범죄_유형, "기타.정보통신망.침해형") ~ "기타.정보통신망.침해형.범죄",
    str_detect(범죄_유형, "기타 불법 컨텐츠") ~ "기타.불법.컨텐츠",
    str_detect(범죄_유형, "사이버.스토킹") ~ "사이버.스토킹",
    str_detect(범죄_유형, "악성프로그램.") ~ "악성프로그램",
    str_detect(범죄_유형, "서비스거부공격") ~ "서비스거부공격",
    TRUE ~ "기타 범죄"
  ))
crime %>%
  filter(연도 >= 2014 & 연도 <= 2020) %>%
  gather(key = "범죄_유형", value = "건수", -연도, -구분)%>%
  distinct(범죄_유형)

unique(crime1$범죄_그룹)
head(crime1$건수)

FC<-crime1%>%
    group_by(범죄_그룹, 구분) %>%
    mutate(건수 = as.numeric(건수))%>%
    filter(!is.na(건수)) %>%
    summarise(총건수 = sum(건수), .groups = 'drop')
  
crime1 %>%
  group_by(범죄_그룹, 구분) %>%
  mutate(건수 = as.numeric(건수))%>%
  filter(!is.na(건수)) %>%
  summarise(총건수 = sum(건수), .groups = 'drop') %>%  # .groups 인수를 사용하여 그룹핑을 명시적으로 설정
  ggplot(aes(x = reorder(범죄_그룹, 총건수), y = 총건수)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ 구분) +  # 구분(발생건수와 검거건수)을 나눠서 시각화
  scale_y_continuous(labels = scales::scientific, breaks = c(0, 2e+05, 4e+05, 6e+05)) +  # Y축을 지수 표기법으로 설정 및 구간 정의
  labs(title = "2014~2020년 합산 사이버 범죄 유형별 발생건수 및 검거건수", 
       x = "범죄 그룹", y = "건수") +
  coord_flip()


#-----------------------------


#Task2-2:해킹, 악성프로그램, 사이버사기, 사이버금융범죄, 사이버금융범죄, 사이버음란물, 사이버도박을 제외한
#유형은 모두 “기타범죄”로 분류한 후 다시 그래프를 그려보자.
crime2 <- crime %>%
  filter(연도 >= 2014 & 연도 <= 2020) %>%
  gather(key = "범죄_유형", value = "건수", -연도, -구분) %>%
  mutate(범죄_유형 = as.character(범죄_유형)) %>%
  mutate(범죄_그룹 = case_when(
    str_detect(범죄_유형, "사이버사기.") ~ "사이버 사기",
    str_detect(범죄_유형, "사이버.도박") ~ "사이버도박",
    str_detect(범죄_유형, "사이버금융범죄") ~ "사이버금융범죄",
    str_detect(범죄_유형, "사이버.음란물") ~ "사이버음란물",
    str_detect(범죄_유형, "해킹.") ~ "해킹",
    str_detect(범죄_유형, "악성프로그램.") ~ "악성프로그램",
    TRUE ~ "기타 범죄"
  ))

crime2 %>%
  group_by(범죄_그룹, 구분) %>%
  mutate(건수 = as.numeric(건수))%>%
  filter(!is.na(건수)) %>%
  summarise(총건수 = sum(건수), .groups = 'drop') %>%  # .groups 인수를 사용하여 그룹핑을 명시적으로 설정
  ggplot(aes(x = reorder(범죄_그룹, 총건수), y = 총건수)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ 구분) +  # 구분(발생건수와 검거건수)을 나눠서 시각화
  scale_y_continuous(labels = scales::scientific, breaks = c(0, 2e+05, 4e+05, 6e+05)) +  # Y축을 지수 표기법으로 설정 및 구간 정의
  labs(title = "2014~2020년 합산 사이버 범죄 유형별 발생건수 및 검거건수", 
       x = "범죄 그룹", y = "건수") +
  coord_flip()

#Task2-3:위 예시그래프는 검거건수를 기준으로 정렬되어 있는데 발생 건수를 기준으로 정렬하여보자.
crime3 <- crime2 %>%
  group_by(범죄_그룹, 구분) %>%
  mutate(건수 = as.numeric(건수)) %>%
  filter(!is.na(건수)) %>%
  summarise(총건수 = sum(건수), .groups = 'drop') %>%
  group_by(구분) %>% 
  mutate(범죄_그룹_정렬 = case_when(
    구분 == "검거건수" ~ reorder(범죄_그룹, 총건수),
    TRUE ~ factor(범죄_그룹)  # 발생건수도 factor로 변환
  )) 

crime3 %>%
  ggplot(aes(x = 범죄_그룹_정렬, y = 총건수)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ 구분,) +  # 각 구분에 맞는 범죄 그룹 정렬 유지
  scale_y_continuous(labels = scales::scientific, breaks = c(0, 2e+05, 4e+05, 6e+05)) +
  labs(title = "2014~2020년 합산 사이버 범죄 유형별 발생건수 및 검거건수", 
       x = "범죄 그룹", y = "건수") +
  coord_flip()
#Task3-1: 연도별 검거 건수/발생 건수를 계산하면 검거율을 계산할 수 있다.
#범죄유형별 검거율을 계산하여보고 검거율이 높은 범죄부터 낮은 범죄까지 정렬하여 막대그래프로 나타내자.

crime2 %>%
  group_by(범죄_그룹, 구분) %>%
  mutate(건수 = as.numeric(건수))%>%
  filter(!is.na(건수)) %>%
  summarise(총건수 = sum(건수), .groups = 'drop')%>%
  spread(key = 구분, value = 총건수) %>%  # 발생건수와 검거건수를 열로 분리
  mutate(검거율 = 검거건수 / 발생건수) %>%
  ggplot(aes(x = reorder(범죄_그룹, -검거율), y = 검거율)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "2014~2020년 합산 사이버 범죄 유형별 발생건수 및 검거건수", 
       x = "범죄 그룹", y = "건수") 
#구분을 사용하여야 하기 때문에 spread를 통해 총 건수를 다시 구분별로 정리하고, 검거율을 계산 이후 시각화를 진행한다.


#Task3-2:검거율을 나타낸 막대그래프의 검거율을 정확하게 읽을 수 있도록 label을 달아보자.
crime2 %>%
  group_by(범죄_그룹, 구분) %>%
  mutate(건수 = as.numeric(건수))%>%
  filter(!is.na(건수)) %>%
  summarise(총건수 = sum(건수), .groups = 'drop')%>%
  spread(key = 구분, value = 총건수) %>%  # 발생건수와 검거건수를 열로 분리
  mutate(검거율 = 검거건수 / 발생건수) %>%
  ggplot(aes(x = reorder(범죄_그룹, -검거율), y = 검거율)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "2014~2020년 합산 사이버 범죄 유형별 발생건수 및 검거건수", 
       x = "범죄 그룹", y = "건수") 
#구분을 사용하여야 하기 때문에 spread를 통해 총 건수를 다시 구분별로 정리하고, 검거율을 계산 이후 시각화를 진행한다.
#Task3-3:범죄들의 검거율을 연도별로 비교하여보자.
crime2 %>%
  group_by(범죄_그룹, 구분) %>%
  mutate(건수 = as.numeric(건수))%>%
  filter(!is.na(건수))


crime2 %>%
  group_by(범죄_그룹, 연도, 구분) %>%
  mutate(건수 = as.numeric(건수)) %>%
  filter(!is.na(건수)) %>%
  summarise(총건수 = sum(건수), .groups = 'drop') %>%
  pivot_wider(names_from = 구분, values_from = 총건수) %>%
  mutate(검거율 = 검거건수 / 발생건수) %>%
  ggplot(aes(x = 범죄_그룹, y = 검거율, fill = 범죄_그룹)) +  # fill로 범죄_그룹 별 색상 지정
  geom_bar(stat = "identity", width = 0.7) +  # 막대 그래프
  geom_text(aes(label = scales::percent(검거율, accuracy = 0.1)), 
            vjust = -0.5, size = 2) +  # 검거율을 퍼센트로 표시
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +  # Y축 퍼센트로 표시
  facet_wrap(~ 연도, ncol = 1, strip.position = "right") +  # 연도를 각 패싯 오른쪽에 표시
  labs(title = "연도별 사이버 범죄 유형별 검거율", 
       x = "범죄 그룹", y = NULL) +  # X축 제목 제거
  theme_minimal(base_size = 4) +  # 미니멀 테마
  theme(
    strip.text.y = element_text(size = 4, angle = 0),  # 각 연도 라벨을 오른쪽에 표시
    panel.grid.major = element_blank(),  # 주요 그리드 라인 제거
    panel.grid.minor = element_blank(),  # 작은 그리드 라인 제거
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # 패널 테두리 추가
    legend.position = "none"  # 범례 제거
  )
#Task 4:2014~2020년도까지의 사이버 범죄 현황을 검토하여 발견한 insight를 하나의 시각화로 정리하여 explanatory
#visualization을 하나 그리고, 그 내용을 설명하시오.

#2014~2020년도까지의 자료밖에 없음
#사이버 사기에 조금 더 집중해보자
#2014년도 사기건수, 15년도 사기건수 등등... 
crime2 %>%
  group_by(범죄_그룹, 구분) %>%
  mutate(건수 = as.numeric(건수))%>%
  filter(!is.na(건수))%>%
  filter(범죄_그룹 == "사이버 사기")%>%
  filter(구분 == "발생건수")%>%
  # .groups 인수를 사용하여 그룹핑을 명시적으로 설정
  ggplot(aes(x = 연도, y = 건수)) +
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "년도별 사이버 사기 빈도수", x = "년도", y = "건수")

crime2 %>%
  group_by(범죄_그룹, 구분) %>%
  mutate(건수 = as.numeric(건수))%>%
  filter(!is.na(건수))%>%
  filter(범죄_그룹 == "사이버금융범죄")%>%
  filter(구분 == "발생건수")%>%
  # .groups 인수를 사용하여 그룹핑을 명시적으로 설정
  ggplot(aes(x = 연도, y = 건수)) +
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "년도별 사이버 금융범죄 빈도수", x = "년도", y = "건수")

crime2 %>%
  group_by(범죄_그룹, 구분) %>%
  mutate(건수 = as.numeric(건수))%>%
  filter(!is.na(건수))%>%
  filter(범죄_그룹 == "사이버도박")%>%
  filter(구분 == "발생건수")%>%
  # .groups 인수를 사용하여 그룹핑을 명시적으로 설정
  ggplot(aes(x = 연도, y = 건수)) +
  geom_bar(stat = "identity", position = "dodge") 

crime2 %>%
  group_by(범죄_그룹, 구분) %>%
  mutate(건수 = as.numeric(건수))%>%
  filter(!is.na(건수))%>%
  filter(범죄_그룹 == "사이버음란물")%>%
  filter(구분 == "발생건수")%>%
  # .groups 인수를 사용하여 그룹핑을 명시적으로 설정
  ggplot(aes(x = 연도, y = 건수)) +
  geom_bar(stat = "identity", position = "dodge") 

crime2 %>%
  group_by(범죄_그룹, 구분) %>%
  mutate(건수 = as.numeric(건수))%>%
  filter(!is.na(건수))%>%
  filter(구분 == "발생건수")%>%
  # .groups 인수를 사용하여 그룹핑을 명시적으로 설정
  ggplot(aes(x = 연도, y = 건수)) +
  geom_bar(stat = "identity", position = "dodge") 

crime2%>%
  group_by(범죄_유형,구분)%>%
  mutate(검수 = as.numeric(건수))%>%
  filter(!is.na(건수))%>%
  filter(구분 == "발생건수")%>%
  filter(범죄_그룹 == "사이버 사기")
  ggplot(aes(x = 연도, y = 건수)) +
  geom_bar(stat = "identity", position = "dodge")
  
  
  crime2 %>%
    group_by(범죄_유형, 구분) %>%
    mutate(건수 = as.numeric(건수)) %>%
    filter(!is.na(건수)) %>%
    filter(구분 == "발생건수") %>%
    filter(범죄_그룹 == "사이버 사기") %>%
    ggplot(aes(x = 연도, y = 건수, fill = 범죄_유형)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "년도별 사이버 사기 유형별 발생 건수",
         x = "연도",
         y = "발생 건수") +
    scale_y_continuous(labels = scales::comma) +  # Y축을 1000 단위로 콤마 추가
    theme_minimal()
  
  
  crime2 %>%
    group_by(범죄_유형, 구분) %>%
    mutate(건수 = as.numeric(건수)) %>%
    filter(!is.na(건수)) %>%
    filter(구분 == "발생건수") %>%
    filter(범죄_그룹 == "사이버금융범죄") %>%
    ggplot(aes(x = 연도, y = 건수, fill = 범죄_유형)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "년도별 사이버금융범죄 유형별 발생 건수",
         x = "연도",
         y = "발생 건수") +
    scale_y_continuous(labels = scales::comma) +  # Y축을 1000 단위로 콤마 추가
    theme_minimal()

  
  
  crime2 %>%
  group_by(범죄_그룹, 구분) %>%
  mutate(건수 = as.numeric(건수))%>%
  filter(!is.na(건수)) %>%
  spread(key = 구분, value = 건수) %>%  # 발생건수와 검거건수를 열로 분리
  mutate(검거율 = 검거건수 / 발생건수) %>%
    filter(범죄_그룹 == "사이버금융범죄") %>%
  ggplot(aes(x = 연도, y = 검거율)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "사이버금융범죄 검거율", 
       x = "년도", y = "건수") 
#구분을 사용하여야 하기 때문에 spread를 통해 총 건수를 다시 구분별로 정리하고, 검거율을 계산 이후 시각화를 진행한다.
  
  crime2 %>%
    group_by(범죄_그룹, 구분) %>%
    mutate(건수 = as.numeric(건수))%>%
    filter(!is.na(건수)) %>%
    spread(key = 구분, value = 건수) %>%  # 발생건수와 검거건수를 열로 분리
    mutate(검거율 = 검거건수 / 발생건수) %>%
    filter(범죄_그룹 == "사이버 사기") %>%
    ggplot(aes(x = 연도, y = 검거율)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "사이버사기 검거율", 
         x = "년도", y = "건수") 

  crime2 %>%
    group_by(범죄_그룹, 구분) %>%
    mutate(건수 = as.numeric(건수))%>%
    filter(!is.na(건수)) %>%
    spread(key = 구분, value = 건수) %>%  # 발생건수와 검거건수를 열로 분리
    mutate(검거율 = 검거건수 / 발생건수) %>%
    filter(범죄_그룹 == "사이버 사기") %>%
    ggplot(aes(x = 연도, y = 검거율, fill = 범죄_유형)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "사이버사기 검거율", 
         x = "년도", y = "건수") 
  
  
  crime2 %>%
    group_by(범죄_그룹, 구분) %>%
    mutate(건수 = as.numeric(건수))%>%
    filter(!is.na(건수)) %>%
    spread(key = 구분, value = 건수) %>%  # 발생건수와 검거건수를 열로 분리
    mutate(검거율 = 검거건수 / 발생건수) %>%
    filter(범죄_그룹 == "사이버금융범죄") %>%
    ggplot(aes(x = 연도, y = 검거율, fill = 범죄_유형)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "사이버금융범죄 검거율", 
         x = "년도", y = "건수") 
  