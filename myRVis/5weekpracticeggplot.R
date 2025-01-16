library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
install.packages("openxlsx")
library(openxlsx)
reg <-read.xlsx("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/지역총생산.xlsx")

reg_rate <-read.xlsx("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/100801_20240927164400722_excel.xlsx")

house <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/주택도시보증공사_전국 신규 민간아파트 분양가격 동향_20240731.csv", 
                  fileEncoding = "CP949")
Consumption <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/1인당_민간소비지출액_시도__20240927170852.csv",
                        fileEncoding = "CP949")

Personal_income <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/1인당_개인소득_시도__20240927170417.csv",
                            fileEncoding = "CP949")

Gross_regional_income<- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/1인당_지역총소득_시도__20240927170650.csv",
                                 fileEncoding = "CP949")

apart<-reg%>%
  select(X1,"2022")#지역총생산산

Consumption%>%
  select(시도별)

Consumption <- Consumption %>%
  mutate(시도별 = gsub("전국", "전국", 시도별)) %>%  # 서울의 경우에는 변환이 불필요할 수 있습니다.
  mutate(시도별 = gsub("서울특별시", "서울", 시도별)) %>%  # '강원도' -> '강원' 으로 변경
  mutate(시도별 = gsub("부산광역시", "부산", 시도별)) %>%
  mutate(시도별 = gsub("대구광역시", "대구", 시도별)) %>%
  mutate(시도별 = gsub("인천광역시", "인천", 시도별)) %>%
  mutate(시도별 = gsub("광주광역시", "광주", 시도별)) %>%
  mutate(시도별 = gsub("대전광역시", "대전", 시도별)) %>%
  mutate(시도별 = gsub("울산광역시", "울산", 시도별)) %>%
  mutate(시도별 = gsub("세종특별자치시", "세종", 시도별)) %>%
  mutate(시도별 = gsub("경기도", "경기", 시도별)) %>%
  mutate(시도별 = gsub("강원특별자치도", "강원", 시도별)) %>%
  mutate(시도별 = gsub("충청북도", "충북", 시도별)) %>%
  mutate(시도별 = gsub("충청남도", "충남", 시도별)) %>%
  mutate(시도별 = gsub("전북특별자치도", "전북", 시도별)) %>%
  mutate(시도별 = gsub("전라남도", "전남", 시도별)) %>%
  mutate(시도별 = gsub("경상북도", "경북", 시도별)) %>%
  mutate(시도별 = gsub("경상남도", "경남", 시도별)) %>%
  mutate(시도별 = gsub("제주특별자치도", "제주", 시도별))  # 제주특별자치도 -> 제주

Personal_income <- Personal_income %>%
  mutate(시도별 = gsub("전국", "전국", 시도별)) %>%  # 서울의 경우에는 변환이 불필요할 수 있습니다.
  mutate(시도별 = gsub("서울특별시", "서울", 시도별)) %>%  # '강원도' -> '강원' 으로 변경
  mutate(시도별 = gsub("부산광역시", "부산", 시도별)) %>%
  mutate(시도별 = gsub("대구광역시", "대구", 시도별)) %>%
  mutate(시도별 = gsub("인천광역시", "인천", 시도별)) %>%
  mutate(시도별 = gsub("광주광역시", "광주", 시도별)) %>%
  mutate(시도별 = gsub("대전광역시", "대전", 시도별)) %>%
  mutate(시도별 = gsub("울산광역시", "울산", 시도별)) %>%
  mutate(시도별 = gsub("세종특별자치시", "세종", 시도별)) %>%
  mutate(시도별 = gsub("경기도", "경기", 시도별)) %>%
  mutate(시도별 = gsub("강원특별자치도", "강원", 시도별)) %>%
  mutate(시도별 = gsub("충청북도", "충북", 시도별)) %>%
  mutate(시도별 = gsub("충청남도", "충남", 시도별)) %>%
  mutate(시도별 = gsub("전북특별자치도", "전북", 시도별)) %>%
  mutate(시도별 = gsub("전라남도", "전남", 시도별)) %>%
  mutate(시도별 = gsub("경상북도", "경북", 시도별)) %>%
  mutate(시도별 = gsub("경상남도", "경남", 시도별)) %>%
  mutate(시도별 = gsub("제주특별자치도", "제주", 시도별))  # 제주특별자치도 -> 제주

Gross_regional_income <- Gross_regional_income %>%
  mutate(시도별 = gsub("전국", "전국", 시도별)) %>%  # 서울의 경우에는 변환이 불필요할 수 있습니다.
  mutate(시도별 = gsub("서울특별시", "서울", 시도별)) %>%  # '강원도' -> '강원' 으로 변경
  mutate(시도별 = gsub("부산광역시", "부산", 시도별)) %>%
  mutate(시도별 = gsub("대구광역시", "대구", 시도별)) %>%
  mutate(시도별 = gsub("인천광역시", "인천", 시도별)) %>%
  mutate(시도별 = gsub("광주광역시", "광주", 시도별)) %>%
  mutate(시도별 = gsub("대전광역시", "대전", 시도별)) %>%
  mutate(시도별 = gsub("울산광역시", "울산", 시도별)) %>%
  mutate(시도별 = gsub("세종특별자치시", "세종", 시도별)) %>%
  mutate(시도별 = gsub("경기도", "경기", 시도별)) %>%
  mutate(시도별 = gsub("강원특별자치도", "강원", 시도별)) %>%
  mutate(시도별 = gsub("충청북도", "충북", 시도별)) %>%
  mutate(시도별 = gsub("충청남도", "충남", 시도별)) %>%
  mutate(시도별 = gsub("전북특별자치도", "전북", 시도별)) %>%
  mutate(시도별 = gsub("전라남도", "전남", 시도별)) %>%
  mutate(시도별 = gsub("경상북도", "경북", 시도별)) %>%
  mutate(시도별 = gsub("경상남도", "경남", 시도별)) %>%
  mutate(시도별 = gsub("제주특별자치도", "제주", 시도별))  # 제주특별자치도 -> 제주

Consumption_2022<-Consumption%>%
  select(시도별,"X2022")
Personal_income_2022 <- Personal_income%>%
  select(시도별,X2022.p.)
Gross_regional_income_2022 <- Gross_regional_income%>%
  select(시도별,X2022)





names(house)

house_2022<-house%>%
  filter(규모구분 == "모든면적")%>%
  filter(연도 == 2022)%>%
  filter(월 == 1)%>%
  select("지역명","분양가격.제곱미터.")
  
apart_mer <- apart %>%
  left_join(Consumption_2022, by = c("X1" = "시도별"))%>%#1인당 소비지출액
  left_join(Personal_income_2022, by = c("X1" = "시도별"))%>%#1인당 개인소득
  left_join(Gross_regional_income_2022,by = c("X1" = "시도별"))%>%#1인당 지역총소득
  left_join(house_2022,by = c("X1" = "지역명"))#분양가격
head(apart_mer)

ggplot(apart_mer, aes(x = X2022.x, y = 분양가격.제곱미터., label = X1)) +
  geom_point(color = "blue", size = 1) +            # 산점도 점 추가
  geom_text(vjust = -1, hjust = 1, size = 3) +      # 각 점에 라벨 추가
  labs(title = "2022년 소비지출액과 분양가격 간의 관계",
       x = "2022년 1인당 소비지출액",
       y = "분양가격 (제곱미터당)")
names(apart_mer)
apart_mer <- apart_mer %>%
  mutate(지역총생산 = as.numeric(gsub("[^0-9]", "", `2022`))) %>%  # '2022' 변수를 숫자로 변환하고 새 이름으로 지정
  select(-`2022`)  # 기존의 '2022' 변수를 삭제

ggplot(apart_mer, aes(x = X2022.p., y = 분양가격.제곱미터., label = X1)) +
  geom_point(color = "blue", size = 1) +            # 산점도 점 추가
  geom_text(vjust = -1, hjust = 1, size = 3) +# 각 점에 라벨 추가
  scale_x_log10() +
  labs(title = "2022년 소득과 분양가격 간의 관계",
       x = "2022년 1인당 개인소득",
       y = "분양가격 (제곱미터당)")
# '2022' 변수의 데이터 타입 확인

ggplot(apart_mer, aes(x = 지역총생산, y = 분양가격.제곱미터., label = X1)) +
  geom_point(color = "blue", size = 1) +            # 산점도 점 추가
  geom_text(vjust = -1, hjust = 1, size = 3) +# 각 점에 라벨 추가
  scale_x_log10() +
  labs(title = "2022년 지역총생산과 분양가격 간의 관계",
       x = "2022년 지역총생산",
       y = "분양가격 (제곱미터당)")

ggplot(apart_mer, aes(x = X2022.y, y = 분양가격.제곱미터., label = X1)) +
  geom_point(color = "blue", size = 1) +            # 산점도 점 추가
  geom_text(vjust = -1, hjust = 1, size = 3) +# 각 점에 라벨 추가
  scale_x_log10() +
  labs(title = "2022년 지역총소득과 분양가격 간의 관계",
       x = "2022년 지역총소득",
       y = "분양가격 (제곱미터당)")

ggplot(apart_mer, aes(x = X2022.x, y = 분양가격.제곱미터., label = X1)) +
  geom_point(color = "blue", size = 1) +            # 산점도 점 추가
  geom_text(vjust = -1, hjust = 1, size = 3) +      # 각 점에 라벨 추가
  labs(title = "2022년 소비지출액과 분양가격 간의 관계",
       x = "2022년 1인당 소비지출액",
       y = "분양가격 (제곱미터당)")


apart_long <- apart_mer %>%
  pivot_longer(cols = c(X2022.p., 지역총생산, X2022.y, X2022.x), 
               names_to = "variable", values_to = "value")
# facet_wrap을 사용하여 여러 관계를 한 번에 시각화
ggplot(apart_long, aes(x = value, y = 분양가격.제곱미터., label = X1)) +
  geom_point(color = "blue", size = 1) +          # 산점도 점 추가
  geom_text(vjust = -1, hjust = 1, size = 3) +    # 각 점에 라벨 추가
  scale_x_log10() +                               # X축 로그 스케일 사용
  labs(title = "2022년 소득과 분양가격 간의 관계",
       x = "소득 (단위 천원)",
       y = "분양가격 (제곱미터당)") +
  facet_wrap(~ variable, scales = "free_x")       # 변수별로 그래프 분리


#2022년 평균 신규아파트 분양가
house_2022 <- house_2022 %>%
  mutate(권역 = case_when(
    지역명 %in% c("서울", "인천", "경기") ~ "수도권",
    지역명 %in% c("부산", "대구", "울산", "경북", "경남") ~ "영남권",
    지역명 %in% c("광주", "전북", "전남") ~ "호남권",
    지역명 %in% c("대전", "충북", "충남", "세종") ~ "충청권",
    지역명 %in% c("강원") ~ "강원권",
    지역명 %in% c("제주") ~ "제주권"
  ))

ggplot(house_2022, aes(x = reorder(지역명, 분양가격.제곱미터.), y = 분양가격.제곱미터., fill = 권역)) +
  geom_col() +  # 막대 그래프 그리기
  geom_text(aes(label = 분양가격.제곱미터.), vjust = -0.5) +  # 막대 위에 분양가 값 표시
  facet_wrap(~ 권역, scales = "free") +  # 권역별로 나누기
  coord_flip() +  # 막대를 가로로 변경
  labs(title = "2022년 평균 신규아파트 분양가 (권역별)",
       x = NULL,
       y = "분양가격 (제곱미터당)")

house_2022%>%
  group_by(권역,분양가격.제곱미터.)%>%
  ggplot( aes(x = reorder(지역명, 분양가격.제곱미터.), y = 분양가격.제곱미터., fill = 권역)) +
  geom_col() +
  geom_text(aes(label = 분양가격.제곱미터.), hjust = -0.1) +  # 텍스트 추가
  scale_fill_brewer(palette = "Set3") +  # 권역별 색상 설정
  labs(title = "2022년 평균 신규아파트 분양가 (시/도별)",
       x = NULL, 
       y = "분양가격 (제곱미터당)") +
  coord_flip()+  # 간결한 테마 적용
  facet_wrap(~ 권역, ncol = 1, scales = "free_y",strip.position = "right")+
  theme(
    strip.text.y = element_text(size = 5, angle = 0),
    panel.grid.major = element_blank(),  # 주요 그리드 라인 제거
    panel.grid.minor = element_blank(),  # 작은 그리드 라인 제거
    legend.position = "none"  # 범례 제거
  )

p <-ggplot(house_2022, aes(x = 지역명, fill = 권역))
p + geom_bar()
p+stat_count()

ggplot(house_2022, aes(x = reorder(지역명, 분양가격.제곱미터.), y = 분양가격.제곱미터., fill = 권역)) +
  geom_col() + # 막대 그래프
  geom_text(aes(label = 분양가격.제곱미터.), hjust = -0.1) +  # 텍스트 추가
  scale_fill_brewer(palette = "Set3") +  # 권역별 색상 설정
  labs(title = "2022년 평균 신규아파트 분양가 (시/도별)",
       x = NULL, 
       y = "분양가격 (제곱미터당)") +
  coord_flip() +  # 가로 막대 그래프
  facet_wrap(~ 권역, scales = "free", ncol = 1) +  # 권역별로 나눔
  theme_minimal() 
house_2224<-house%>%
  filter(규모구분 == "모든면적")%>%
  filter(연도 >= 2020)%>%
  select("지역명","분양가격.제곱미터.",연도,월)%>%
  mutate(권역 = case_when(
    지역명 %in% c("서울", "인천", "경기") ~ "수도권",
    지역명 %in% c("부산", "대구", "울산", "경북", "경남") ~ "영남권",
    지역명 %in% c("광주", "전북", "전남") ~ "호남권",
    지역명 %in% c("대전", "충북", "충남", "세종") ~ "충청권",
    지역명 %in% c("강원") ~ "강원권",
    지역명 %in% c("제주") ~ "제주권"
  ))%>%
  mutate(날짜 = as.Date(paste(연도, 월, "01", sep = "-"), format = "%Y-%m-%d"))
house_labels <- house_2224 %>%
  group_by(지역명) %>%
  slice(1) 

ggplot(house_2224,aes(x = 날짜, y = 분양가격.제곱미터., color = 지역명, group = 지역명)) +
  geom_line(linewidth = 1) +  # 선 그래프의 두께 설정
  facet_wrap(~ 권역, ncol = 1, scales = "free_y",strip.position = "right")+
  geom_text(data = house_labels,aes(label = 지역명), hjust = -0.1)
  geom_point(size = 1) + # 각 포인트 표시
  labs(title = "시간에 따른 지역별 분양가격 추이",
       x = "날짜",
       y = "분양가격 (제곱미터당)") +
  theme_minimal() +
  theme(legend.position = "none")  
  
install.packages("sf")
library(sf)
korea <- read_sf("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/maps/ctp_rvn.shp")

print(korea)
korea <- korea%>%
  st_set_crs(5179)
korea <- korea%>%
  mutate(CTP_KOR_NM = iconv(CTP_KOR_NM, from = "EUC-KR", to = 'UTF-8'))
Personal_income <- Personal_income %>%
  mutate(시도별 = gsub("강원특별자치도", "강원도", 시도별)) %>%  # 서울의 경우에는 변환이 불필요할 수 있습니다.
  mutate(시도별 = gsub("전북특별자치도", "전라북도", 시도별)) # '강원도' -> '강원' 으로 변경
 

prin <- Personal_income%>%
  select(시도별,X2022.p.)
prin<-korea%>%
  left_join(prin,by =c("CTP_KOR_NM" = "시도별"))


head(prin)
ggplot(prin) +
  geom_sf()
ggplot(prin) +
  geom_sf(aes(fill = X2022.p.)) +  # 개인소득(X2022.p.) 변수를 색상에 매핑
  scale_fill_viridis_c(option = "plasma", direction = -1) +  # 색상 팔레트 설정
  labs(title = "대한민국 지역별 개인소득", 
       fill = "개인소득 (X2022.p.)", 
       x = "경도", 
       y = "위도")
korea$CTP_KOR_NM
house_2022 <- house_2022 %>%
  mutate(지역명 = recode(지역명,
                      "서울" = "서울특별시",
                      "부산" = "부산광역시",
                      "대구" = "대구광역시",
                      "인천" = "인천광역시",
                      "광주" = "광주광역시",
                      "대전" = "대전광역시",
                      "울산" = "울산광역시",
                      "세종" = "세종특별자치시",
                      "경기" = "경기도",
                      "강원" = "강원도",
                      "충북" = "충청북도",
                      "충남" = "충청남도",
                      "전북" = "전라북도",
                      "전남" = "전라남도",
                      "경북" = "경상북도",
                      "경남" = "경상남도",
                      "제주" = "제주특별자치도"
  ))
prin<-korea%>%
  left_join(house_2022,by =c("CTP_KOR_NM" = "지역명"))
ggplot(prin) +
  geom_sf(aes(fill = 분양가격.제곱미터.)) +  # 개인소득(X2022.p.) 변수를 색상에 매핑
  scale_fill_viridis_c(option = "plasma", direction = -1) +  # 색상 팔레트 설정
  labs(title = "대한민국 지역별 아파트 가격", 
       fill = "아파트 가격(제곱미터)", 
       x = "경도", 
       y = "위도")
prin <- Personal_income%>%
  select(시도별,X2022.p.)
prin<-korea%>%
  left_join(prin,by =c("CTP_KOR_NM" = "시도별"))%>%
  left_join(house_2022,by =c("CTP_KOR_NM" = "지역명"))
prin%>%
  mutate(아파트지수 = 분양가격.제곱미터./X2022.p.)%>%
  ggplot() +
  geom_sf(aes(fill = 아파트지수)) +  # 개인소득(X2022.p.) 변수를 색상에 매핑
  scale_fill_viridis_c(option = "plasma", direction = -1) +  # 색상 팔레트 설정
  labs(title = "대한민국 지역별 아파트 지수", 
       fill = "아파트 지수", 
       x = "경도", 
       y = "위도")
