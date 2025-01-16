library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

kospi1 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_Kospi2022.csv", fileEncoding = "CP949")
kospi2 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_Kospi2023.csv",fileEncoding = "CP949")
kospi3 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_Kospi2024.csv",fileEncoding = "CP949")

kosdaq1 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_Kosdaq2022.csv",fileEncoding = "CP949")
kosdaq2 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_Kosdaq2023.csv",fileEncoding = "CP949")
kosdaq3 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_Kosdaq2024.csv",fileEncoding = "CP949")

sam1 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_sam_2022.csv",fileEncoding = "CP949")
sam2 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_sam_2023.csv",fileEncoding = "CP949")
sam3 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_sam_2024.csv",fileEncoding = "CP949")



#Task1-1: 대한민국 주식시장의 대표적인 지표인 KOSPI와 KOSDAQ 주가지수의 일별 데이터를 수집한 후 line plot으로 나타내보자
#일단 각 데이터프레임을 합쳐보자
head(kospi1)
head(kospi2)

kospi <- bind_rows(kospi3, kospi2, kospi1)

kospi<- kospi%>%
  distinct()

kosdaq <- bind_rows(kosdaq3, kosdaq2, kosdaq1)%>%
  distinct()

kospi%>%
  mutate(일자 = as.Date(일자, format = "%Y/%m/%d")) %>% 
  ggplot(aes(x = 일자, y = 종가))+
  geom_line()+
  labs(title = "2021-09~2024-09 Kospi 날자별 종가",x = "날자", y = "종가")


kosdaq%>%
  mutate(일자 = as.Date(일자, format = "%Y/%m/%d")) %>% 
  ggplot(aes(x = 일자, y = 종가))+
  geom_line()+
  labs(title = "2021-09~2024-09 Kosdaq 날자별 종가",x = "날자", y = "종가")


#이번에는 kospi와 kosdaq의 종가를 각각 나타내보자
ko<-kospi%>%
  select(일자,종가)
ko2 <- kosdaq%>%
  select(일자,종가)

kos <- ko%>%
  left_join(ko2,by = "일자")





kos_long <- kos %>%
  pivot_longer(cols = c(종가.x, 종가.y), 
               names_to = "지수", 
               values_to = "종가") %>%
  mutate(지수 = recode(지수, "종가.x" = "KOSPI", "종가.y" = "KOSDAQ"))

kos_long <- kos_long %>%
  mutate(일자 = as.Date(일자, format = "%Y/%m/%d"))

ggplot(kos_long, aes(x = 일자, y = 종가, color = 지수, group = 지수)) +
  geom_line(size = 1) +
  labs(title = "KOSPI와 KOSDAQ 종가 비교",
       x = "날짜", y = "종가") +
  theme_minimal() +
  scale_color_manual(values = c("KOSPI" = "blue", "KOSDAQ" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#삼성전자의 시가총액은 전체 KOSPI 에서 약 20~ 2 5% 가량을 차지하는 것으로 알려져 있다
삼성전자 주가의 일별 가격 종가기준 ) 변동을 1 1 그래프와 함께 그래프에 나타내보고 , 주
가지표의 움직임과 어떻게 연동되어 움직이는지 그렇지 않은지에 대해서 설명해보자


sam <- bind_rows(sam3, sam2, sam1)
sam_s <- sam%>%
  select(일자,종가)
kos_s <- kos%>%
  left_join(sam_s,by = "일자")

kos_long1 <- kos_s %>%
  pivot_longer(cols = c(종가.x, 종가.y,종가), 
               names_to = "지수", 
               values_to = "종가") %>%
  mutate(지수 = recode(지수, "종가.x" = "KOSPI", "종가.y" = "KOSDAQ", "종가" = "삼성전자"))

kos_long1 %>%
  mutate(일자 = as.Date(일자, format = "%Y/%m/%d")) %>%  # 날짜 형식으로 변환
  ggplot(aes(x = 일자, y = 종가, color = 지수, group = 지수)) +
  geom_line(size = 1) +
  labs(title = "KOSPI, KOSDAQ, 그리고 삼성전자 종가 비교",
       x = "날짜", y = "종가") +
  theme_minimal() +
  scale_color_manual(values = c("KOSPI" = "blue", "KOSDAQ" = "red", "삼성전자" = "green")) +  # 색상 지정
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # X축 날짜 각도 조정

kos_long1 <- kos_long1 %>%
  mutate(일자 = as.Date(일자, format = "%Y/%m/%d"))  # 날짜 형식 변환



sam_s <- sam %>%
  select(일자, 종가) %>%
  reframe(일자 = 일자, 종가 = 종가 / 10)  # reframe()으로 여러 값을 반환


kos_s1 <- kos%>%
  left_join(sam_s,by = "일자")

kos_long2 <- kos_s1 %>%
  pivot_longer(cols = c(종가.x, 종가.y,종가), 
               names_to = "지수", 
               values_to = "종가") %>%
  mutate(지수 = recode(지수, "종가.x" = "KOSPI", "종가.y" = "KOSDAQ", "종가" = "삼성전자"))

kos_long2 %>%
  mutate(일자 = as.Date(일자, format = "%Y/%m/%d")) %>%  # 날짜 형식으로 변환
  ggplot(aes(x = 일자, y = 종가, color = 지수, group = 지수)) +
  geom_line(size = 1) +
  labs(title = "KOSPI, KOSDAQ, 그리고 삼성전자 종가 비교",
       x = "날짜", y = "종가") +
  theme_minimal() +
  scale_color_manual(values = c("KOSPI" = "blue", "KOSDAQ" = "red", "삼성전자" = "green")) +  # 색상 지정
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # X축 날짜 각도 조정
#이번에는 -4000을 해보자
sam_s <- sam_s %>%
  select(일자, 종가) %>%
  reframe(일자 = 일자, 종가 = 종가 - 4000)  # reframe()으로 여러 값을 반환


kos_s2 <- kos%>%
  left_join(sam_s,by = "일자")

kos_long3 <- kos_s2 %>%
  pivot_longer(cols = c(종가.x, 종가.y,종가), 
               names_to = "지수", 
               values_to = "종가") %>%
  mutate(지수 = recode(지수, "종가.x" = "KOSPI", "종가.y" = "KOSDAQ", "종가" = "삼성전자"))

kos_long3 %>%
  mutate(일자 = as.Date(일자, format = "%Y/%m/%d")) %>%  # 날짜 형식으로 변환
  ggplot(aes(x = 일자, y = 종가, color = 지수, group = 지수)) +
  geom_line(size = 1) +
  labs(title = "KOSPI, KOSDAQ, 그리고 삼성전자 종가 비교",
       x = "날짜", y = "종가") +
  theme_minimal() +
  scale_color_manual(values = c("KOSPI" = "blue", "KOSDAQ" = "red", "삼성전자" = "green")) +  # 색상 지정
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # X축 날짜 각도 조정

#미국 다우존스 S&P 500, 나스닥 ), 일본 닛케이 등 세계 주요국 최소 5 개국 이상의 주가지수를 수집한 후 대한민국의 주가 지수와 동시에 l ine plo t 을 그리고 상관관계를 살펴보자



#3-1 task 3 에서는 코스피 지수를 다른 경제 지표 유가 금가격 등 들과 비교하려고 한다 경제지표
들은 주로 월별로 발표하는 경우가 많기 때문에 코스피 지수를 월별 평균값 을 계산한 월별 이동
평균 값과 월별 종가로 계산한 월별 그래프 2 가지를 동시에 line plot 으로 표현하여라
library(zoo)
install.packages("zoo")
kospi_m <- kospi %>% select(일자, 종가)

# 일자를 Date 형식으로 변환
kospi_m <- kospi_m %>%
  mutate(일자 = as.Date(일자, format = "%Y/%m/%d"))

# 월별 종가 (월 마지막 거래일의 종가)
kospi_monthly <- kospi_m %>%
  group_by(month = format(일자, "%Y-%m")) %>%
  summarise(월종가 = last(종가))

# 월별 이동 평균 (여기서는 3개월 이동 평균 사용)
kospi_monthly <- kospi_monthly %>%
  mutate(이동평균 = rollmean(월종가, k = 3, fill = NA, align = "right"))

# 선 그래프로 월간 종가와 이동 평균을 시각화
ggplot(kospi_monthly, aes(x = as.Date(paste0(month, "-01")))) +
  geom_line(aes(y = 월종가, color = "월간 종가"), size = 1) +
  geom_line(aes(y = 이동평균, color = "월간 이동평균"), size = 1) +
  labs(title = "KOSPI 월간 종가 및 이동평균 비교",
       x = "날짜", y = "지수",
       color = "범례") +
  theme_minimal()

#3-2 3-1의 지표와 함께 생활관 관련된 다른 지표들의 월별 지수를 라인으로 시각화해봐라
#보험 지표
bo1 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_bo_2022.csv",fileEncoding = "CP949")
bo2 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_bo_2023.csv",fileEncoding = "CP949")
bo3 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_bo_2024.csv",fileEncoding = "CP949")
bo <- bind_rows(bo3, bo2, bo1)

bo<- bo%>%
  distinct()

#식료품
food1 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_food2022.csv",fileEncoding = "CP949")
food2 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_food2023.csv",fileEncoding = "CP949")
food3 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_food2024.csv",fileEncoding = "CP949")
food <- bind_rows(food3, food2, food1)

food<- food%>%
  distinct()

#의약품
med1 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_med2022.csv",fileEncoding = "CP949")
med2 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_med2023.csv",fileEncoding = "CP949")
med3 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_med2024.csv",fileEncoding = "CP949")
med <- bind_rows(med3, med2, med1)

med<- med%>%
  distinct()
#국제 금시세
gold1 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_gold2022.csv",fileEncoding = "CP949")
gold2 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_gold2023.csv",fileEncoding = "CP949")
gold3 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_gold2024.csv",fileEncoding = "CP949")
gold <- bind_rows(gold3, gold2, gold1)

gold<- gold%>%
  distinct()

#국내 유가(경유)
oil1 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_oil2022.csv",fileEncoding = "CP949")
oil2 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_oil2023.csv",fileEncoding = "CP949")
oil3 <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data_oil2024.csv",fileEncoding = "CP949")
oil <- bind_rows(oil3, oil2, oil1)

oil<- oil%>%
  distinct()

bo_m <- bo %>% select(일자, 종가)
food_m <- food%>% select(일자, 종가)
med_m <- med%>% select(일자, 종가)
gold_m <- gold%>% select(일자, 원.g_종가)
oil_m <- oil%>% select(일자, 전체)

# 일자를 Date 형식으로 변환
bo_m <- bo_m %>%
  mutate(일자 = as.Date(일자, format = "%Y/%m/%d"))
food_m <- food_m %>%
  mutate(일자 = as.Date(일자, format = "%Y/%m/%d"))
med_m <- med_m %>%
  mutate(일자 = as.Date(일자, format = "%Y/%m/%d"))
gold_m <- gold_m %>%
  mutate(일자 = as.Date(일자, format = "%Y/%m/%d"))
oil_m <- oil_m %>%
  mutate(일자 = as.Date(일자, format = "%Y/%m/%d"))
# 월별 종가 (월 마지막 거래일의 종가)
bo_monthly <- bo_m %>%
  group_by(month = format(일자, "%Y-%m")) %>%
  summarise(월종가 = last(종가))
food_monthly <- food_m %>%
  group_by(month = format(일자, "%Y-%m")) %>%
  summarise(월종가 = last(종가))
med_monthly <- med_m %>%
  group_by(month = format(일자, "%Y-%m")) %>%
  summarise(월종가 = last(종가))
gold_monthly <- gold_m %>%
  group_by(month = format(일자, "%Y-%m")) %>%
  summarise(월종가 = last(원.g_종가))
oil_monthly <- oil_m %>%
  group_by(month = format(일자, "%Y-%m")) %>%
  summarise(월종가 = last(전체))

bo_monthly <- bo_monthly %>%
  mutate(이동평균 = rollmean(월종가, k = 3, fill = NA, align = "right"))
food_monthly <- food_monthly %>%
  mutate(이동평균 = rollmean(월종가, k = 3, fill = NA, align = "right"))
med_monthly <- med_monthly %>%
  mutate(이동평균 = rollmean(월종가, k = 3, fill = NA, align = "right"))
gold_monthly <- gold_monthly %>%
  mutate(이동평균 = rollmean(월종가, k = 3, fill = NA, align = "right"))
oil_monthly <- oil_monthly %>%
  mutate(이동평균 = rollmean(월종가, k = 3, fill = NA, align = "right"))

#이제 kospi와 각 월종가, 이동평균을 하나의 데이터프레임으로 합친 후 시각화를 진행한다
mon_end <- kospi_monthly %>%
  left_join(bo_monthly, by = "month", suffix = c("_KOSPI", "_BO")) %>%
  left_join(food_monthly, by = "month", suffix = c("_BO", "_FOOD")) %>%
  left_join(med_monthly, by = "month", suffix = c("_FOOD", "_MED")) %>%
  left_join(gold_monthly, by = "month", suffix = c("_MED", "_GOLD")) %>%
  left_join(oil_monthly, by = "month", suffix = c("_GOLD", "_OIL"))
  
head(mon_end)
# 선 그래프로 월간 종가와 이동 평균을 시각화

# 1번 그래프: 월종가에 대한 시각화


# 1번 그래프: 월종가에 대한 시각화
ggplot(mon_end, aes(x = as.Date(paste0(month, "-01")))) +
  geom_line(aes(y = 월종가_KOSPI, color = "KOSPI"), size = 1) +
  geom_line(aes(y = 월종가_BO, color = "BO"), size = 1) +
  geom_line(aes(y = 월종가_FOOD, color = "FOOD"), size = 1) +
  geom_line(aes(y = 월종가_MED, color = "MED"), size = 1) +
  geom_line(aes(y = 월종가_GOLD, color = "GOLD"), size = 1) +
  geom_line(aes(y = 월종가_OIL, color = "OIL"), size = 1) +
  labs(title = "KOSPI와 다른 경제 지표들의 월종가 비교",
       x = "날짜", y = "월종가", color = "지수") +
  scale_color_manual(values = c("KOSPI" = "blue", "BO" = "red", "FOOD" = "green", 
                                "MED" = "purple", "GOLD" = "gold", "OIL" = "brown")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# 2번 그래프: 이동평균에 대한 시각화
# 2번 그래프: 이동평균에 대한 시각화
ggplot(mon_end, aes(x = as.Date(paste0(month, "-01")))) +
  geom_line(aes(y = 이동평균_KOSPI, color = "KOSPI"), size = 1) +
  geom_line(aes(y = 이동평균_BO, color = "BO"), size = 1) +
  geom_line(aes(y = 이동평균_FOOD, color = "FOOD"), size = 1) +
  geom_line(aes(y = 이동평균_MED, color = "MED"), size = 1) +
  geom_line(aes(y = 이동평균_GOLD, color = "GOLD"), size = 1) +
  geom_line(aes(y = 이동평균_OIL, color = "OIL"), size = 1) +
  labs(title = "KOSPI와 다른 경제 지표들의 이동평균 비교",
       x = "날짜", y = "이동평균", color = "지수") +
  scale_color_manual(values = c("KOSPI" = "blue", "BO" = "red", "FOOD" = "green", 
                                "MED" = "purple", "GOLD" = "gold", "OIL" = "brown")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


mon_end1<-mon_end%>%
  mutate(월종가_GOLD = 월종가_GOLD/10 )%>%
  mutate(이동평균_GOLD = 이동평균_GOLD/10)%>%
  mutate(월종가_MED = 월종가_MED/10)%>%
  mutate(이동평균_MED = 이동평균_MED/10)%>%
  mutate(월종가_BO = 월종가_BO/10)%>%
  mutate(이동평균_BO = 이동평균_BO/10)%>%
  mutate(월종가_GOLD = 월종가_GOLD-5000 )%>%
  mutate(이동평균_GOLD = 이동평균_GOLD-5000)%>%
  mutate(이동평균_FOOD = 이동평균_FOOD-2000)%>%
  mutate(월종가_FOOD = 월종가_FOOD-2000)

ggplot(mon_end1, aes(x = as.Date(paste0(month, "-01")))) +
  geom_line(aes(y = 월종가_KOSPI, color = "KOSPI"), size = 1) +
  geom_line(aes(y = 월종가_BO, color = "BO"), size = 1) +
  geom_line(aes(y = 월종가_FOOD, color = "FOOD"), size = 1) +
  geom_line(aes(y = 월종가_MED, color = "MED"), size = 1) +
  geom_line(aes(y = 월종가_GOLD, color = "GOLD"), size = 1) +
  geom_line(aes(y = 월종가_OIL, color = "OIL"), size = 1) +
  labs(title = "KOSPI와 다른 경제 지표들의 월종가 비교",
       x = "날짜", y = "월종가", color = "지수") +
  scale_color_manual(values = c("KOSPI" = "blue", "BO" = "red", "FOOD" = "green", 
                                "MED" = "purple", "GOLD" = "gold", "OIL" = "brown")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(mon_end1, aes(x = as.Date(paste0(month, "-01")))) +
  geom_line(aes(y = 이동평균_KOSPI, color = "KOSPI"), size = 1) +
  geom_line(aes(y = 이동평균_BO, color = "BO"), size = 1) +
  geom_line(aes(y = 이동평균_FOOD, color = "FOOD"), size = 1) +
  geom_line(aes(y = 이동평균_MED, color = "MED"), size = 1) +
  geom_line(aes(y = 이동평균_GOLD, color = "GOLD"), size = 1) +
  geom_line(aes(y = 이동평균_OIL, color = "OIL"), size = 1) +
  labs(title = "KOSPI와 다른 경제 지표들의 이동평균 비교",
       x = "날짜", y = "이동평균", color = "지수") +
  scale_color_manual(values = c("KOSPI" = "blue", "BO" = "red", "FOOD" = "green", 
                                "MED" = "purple", "GOLD" = "gold", "OIL" = "brown")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/data.csv")
names(data)
per <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/경제활동인구_24162428.csv")

head(per)
#코스피 월 종가와
unemployment_data <- per %>% 
  filter(계정항목 == "  실업률") %>% 
  select(starts_with("X"))  
# 열 이름을 날짜로 변환
colnames(unemployment_data) <- gsub("X", "", colnames(unemployment_data))

# 데이터 구조 변환 (wide -> long)
unemployment_long <- gather(unemployment_data, key = "월", value = "실업률")

# 데이터 확인
head(unemployment_long)
head(kospi_monthly)

unemployment_long <- unemployment_data %>%
  gather(key = "month", value = "실업률")

# 월 데이터를 제대로 정리하고 공백 및 포맷 일치
unemployment_long$month <- gsub("X", "", unemployment_long$month)  # "X" 제거
unemployment_long$month <- as.yearmon(unemployment_long$month, "%Y.%m")  # 연도-월 형식으로 변환
kospi_monthly$month <- as.yearmon(kospi_monthly$month, "%Y-%m")  # kospi_monthly도 연도-월 형식으로 변환
# 데이터 결합 (left_join을 사용하여 월별로 결합)
combined_data <- left_join(kospi_monthly, unemployment_long, by = "month")

# 결합된 데이터 확인
head(combined_data)
combined_data$실업률 <- as.numeric(combined_data$실업률)

# 이중 축 그래프 그리기
ggplot(combined_data, aes(x = month)) +
  geom_line(aes(y = 월종가, color = "KOSPI 월종가"), size = 1) +  # KOSPI 월종가 선
  geom_line(aes(y = 실업률 * 1000, color = "실업률"), size = 1) +  # 실업률 (스케일 조정)
  scale_y_continuous(
    name = "KOSPI 월종가",
    sec.axis = sec_axis(~./1000, name = "실업률 (%)")  # 두 번째 축을 실업률로 설정
  ) +
  labs(title = "KOSPI 월종가와 실업률 추이", x = "월") +
  scale_color_manual(values = c("KOSPI 월종가" = "blue", "실업률" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))













install.packages("maps")
library(maps)
house <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/유형별 주택매매가격지수_24162747.csv")


asia_data_clean <- data %>%
  filter(!is.na(GDP.per.capita..current.US..)) %>%
  mutate(Country.Name = tolower(trimws(Country.Name)),
         Country.Name = case_when(
           Country.Name == "korea, rep." ~ "south korea",  # 한국 이름 변경
           Country.Name == "viet nam" ~ "vietnam",         # 베트남 이름 변경
           TRUE ~ Country.Name  # 나머지는 그대로
         ))

# 아시아 국가 목록 (소문자로 맞추기)
asia_countries <- c("china", "india", "japan", "south korea", "indonesia", 
                    "thailand", "vietnam", "malaysia", "philippines", "pakistan")

# 아시아 지도 그리기
map("world", regions = asia_countries, fill = TRUE, col = "lightgray", bg = "lightblue")

# 국가별 데이터에 색상 적용 (GDP 기준으로 색상 조정)
for (i in 1:nrow(asia_data_clean)) {
  # 해당 국가가 아시아 국가에 포함되는지 확인
  if (asia_data_clean$Country.Name[i] %in% asia_countries) {
    # 알파 값이 유효하도록 NA 처리 및 max 값이 올바르게 설정되었는지 확인
    gdp_alpha <- asia_data_clean$GDP.per.capita..current.US..[i] / max(asia_data_clean$GDP.per.capita..current.US.., na.rm = TRUE)
    
    # 알파 값을 0~1로 제한
    gdp_alpha <- ifelse(is.na(gdp_alpha) | gdp_alpha < 0, 0, ifelse(gdp_alpha > 1, 1, gdp_alpha))
    
    # 지도에 해당 국가의 GDP 색상 반영
    map("world", regions = asia_data_clean$Country.Name[i], fill = TRUE, 
        col = rgb(1, 0, 0, alpha = gdp_alpha), add = TRUE)
  }
}
unique(data$Country.Name)
# Country.Name 열에 있는 국가명 확인
unique(data$Country.Name)









theme_set(theme_gray(base_family="NanumGothic"))

ggplot(korpop1,aes(map_id=code,fill=총인구_명))+
  geom_map(map=kormap1,colour="black",size=0.1)+
  expand_limits(x=kormap1$long,y=kormap1$lat)+
  scale_fill_gradientn(colours=c('white','orange','red'))+
  ggtitle("2014년도 시도별 인구분포도")+
  coord_map()






install.packages("devtools")
devtools::install_github("cardiomoon/kormaps2014")
library(kormaps2014)
library(moonBook2)
library(ggplot2)
theme_set(theme_gray(base_family="NanumGothic"))


