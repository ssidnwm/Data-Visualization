
library(ggplot2)
library(tidyr)
library(dplyr)

countries <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/All Countries.csv")

#Task1: 인구가 많은 국가가 탄소 배출량도 많은가? 이를 확인하기 위한 scatterplot을 그려보자
#필요한 것은 인구수, 탄소 배출량
names(countries)
co2_emissions

countries%>%
  ggplot(aes(x = population, y =co2_emissions))+
  geom_point()
head(countries$population)

countries%>%
  ggplot(aes(x = population, y =co2_emissions,color = country))+
  geom_point()+
  ggtitle("Correlation b/w log(Pop) and log (co2 emissions)")+
  scale_y_log10("Co2 emissions",labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_x_log10("POP",labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  stat_smooth(method = 'lm', se = F, col = "Blue")


#Task1-2: scale 함수나 labs , ggtitle 등의 함수를 활용하여그래프를 더 잘 읽을수 있도록 보완하자


#Task2-1: 산업이 발달하여 인당 GDP 가 높은 국가는 도시 인구 비율이 더 높은지 나타내는scatterplot 을 아래와 같이 그려보자
#+도시 인구의 비율은 도시 인구를 전체 인구로 나누어 계산할 수 있다
#+인당GDP 는 GDP 를 인구수로 나누어 계산할 수 있다


countries%>%
  mutate("urban_ratio"=urban_population/population)%>%
  mutate("pop_GDP"=gdp/population)%>%
  ggplot(aes(x = pop_GDP,y = urban_ratio))+
  geom_point()+
  scale_y_continuous("Urban Population %")+
  scale_x_log10("personal GDP(USD)",labels = scales::trans_format("log10", scales::math_format(10^.x)))
  
low_pop<-countries%>%
  mutate("urban_ratio"=urban_population/population)%>%
  mutate("pop_GDP"=gdp/population)%>%
  filter(urban_ratio < 0.25, pop_GDP > 100000)

hi_pop<-countries%>%
  mutate("urban_ratio"=urban_population/population)%>%
  mutate("pop_GDP"=gdp/population)%>%
  filter(urban_ratio >= 1, pop_GDP > 100000)%>%
  select(country)

lolo<-countries%>%
  mutate("urban_ratio"=urban_population/population)%>%
  mutate("pop_GDP"=gdp/population)%>%
  filter(urban_ratio < 0.25, pop_GDP < 500)%>%
  select(country)
  


countries%>%
  mutate("urban_ratio"=urban_population/population)%>%
  mutate("pop_GDP"=gdp/population)%>%
  mutate(country_type = case_when(
    country %in% low_pop$country ~ "Liechtenstein",
    country %in% hi_pop$country ~ "Monaco",
    country %in% lolo$country ~ "Burundi",
    TRUE ~ "normal"
  )) %>%
  ggplot(aes(x = pop_GDP,y = urban_ratio, colour = country_type))+
  geom_point()+
  scale_color_manual(values = c("Liechtenstein" = "red","Burundi" = "blue","Monaco" = 'yellow', "normal" = "black"))+
  scale_y_continuous("Urban Population %")+
  scale_x_log10("personal GDP(USD)",labels = scales::trans_format("log10", scales::math_format(10^.x)))
  


#Task2-2: 인당 GDP 와 합계 출산율 (fertility rate)의 관계를 나타내는 scatterplot 을그려보자
countries%>%
  mutate("pop_GDP"=gdp/population)%>%
  ggplot(aes(x = pop_GDP, y = fertility_rate, color = country))+
  geom_point()+
  scale_colour_manual(values = c("Korea" = "red"))+
  scale_x_log10("personal GDP(USD)",labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  labs(y = "Fertility Rate")+
  ggtitle("Relation Personal GDP and Fertility Rate")

  

#Task2-3: 2-1, 2-2 의 그래프를 동일한 x 축 (GPD per Capita) 상에 나타내어라.
countries%>%
  mutate("pop_GDP"=gdp/population)%>%
  mutate("urban_ratio"=urban_population/population)%>%
  ggplot(aes(x = pop_GDP))+
  geom_point()+
  facet_grid(pop_GDP~., )+
  scale_x_log10("personal GDP(USD)",labels = scales::trans_format("log10", scales::math_format(10^.x)))


countries <- countries %>%
  mutate(pop_GDP = gdp / population)  # 1인당 GDP

# long format으로 데이터 변환
df_long <- countries %>%
  mutate("pop_GDP"=gdp/population)%>%
  mutate("urban_ratio"=urban_population/population)%>%
  select("pop_GDP", "fertility_rate", "urban_ratio") %>%
  pivot_longer(cols = c(fertility_rate, urban_ratio), 
               names_to = "variable", 
               values_to = "value")

ggplot(df_long, aes(x = pop_GDP, y = value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = "free_y") +  # 서로 다른 y축을 사용
  scale_x_log10("GDP per Capita(USD)", labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(y = NULL)+   # y축 라벨 제거 (개별적으로 출력)
  ggtitle("Vis for fertility, urban ratio to pop GDP")



#Task 3-1: 정치형태별(democracy_type) 인당 gdp 를 표현하는 그래프를 아래와 같이 그려보자

countries%>%
  mutate("pop_GDP"=gdp/population)%>%
  ggplot(aes(x = democracy_type, y = pop_GDP, color = democracy_type))+
  geom_jitter()+
  scale_y_log10("GDP per Capita(USD)", labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  theme(legend.position = "none")+
  labs(x="Democracy Type")+
  ggtitle("GDP per capita by political form")
  
  



#Task 3-2: 정치 형태에 따른 평균 인당 gdp 를 계산하여 아래 그림과 같이 layer 를 추가해보자
countries%>%
  mutate("pop_GDP"=gdp/population)%>%
  group_by(democracy_type)%>%
  mutate("decGDP" = mean(pop_GDP, na.rm = TRUE))%>%
  ggplot(aes(x = democracy_type, y = pop_GDP, color = democracy_type))+
  geom_jitter()+
  geom_point(aes(x = democracy_type, y = decGDP), color = "black",size = 4, shape = 3)+
  scale_y_log10("GDP per Capita(USD)", labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  theme(legend.position = "none")+
  labs(x = "Democracy Type")


countries%>%
  mutate("pop_GDP"=gdp/population)%>%
  group_by(democracy_type)%>%
  mutate("decGDP" = mean(pop_GDP, na.rm = TRUE))%>%
  ggplot(aes(x = democracy_type, y = pop_GDP, color =  country))+
  geom_jitter()+
  scale_colour_manual(values = c("Korea" = "red"))+
  geom_point(aes(x = democracy_type, y = decGDP), color = "black",size = 4, shape = 3)+
  scale_y_log10("GDP per Capita(USD)", labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  theme(legend.position = "none")+
  labs(x = "Democracy Type")


countries%>%
  mutate("pop_GDP"=gdp/population)%>%
  group_by(democracy_type)%>%
  summarise(decGDP = mean(pop_GDP, na.rm = TRUE))%>%
  arrange(desc(decGDP))



#Task4: 각 국가별 지표들을 사용해서 흥미로운 insight 를 도출 할 수 있는 scatterplot 을그려보자
#위에서 진행한 내용들에 대해 우리나라의 경우를 추가해보자
  
#인구수당 탄소배출량 한국의 경우
countries%>%
  ggplot(aes(x = population, y =co2_emissions,color = country))+
  geom_point()+
  scale_colour_manual(values = c("Korea" = "red"))+
  ggtitle("Correlation b/w log(Pop) and log (co2 emissions)")+
  scale_y_log10("Co2 emissions",labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_x_log10("POP",labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  stat_smooth(method = 'lm', se = F, col = "Blue")

#1인당 gdp가 23,000이상인 국가들에 대한 co2배출량
countries%>%
  mutate("pop_GDP"=gdp/population)%>%
  filter(pop_GDP >= 23000)


countries %>%
  mutate(pop_GDP = gdp / population) %>%
  ggplot(aes(x = pop_GDP, y = co2_emissions)) +  # 그룹에 따라 색상 변경
  geom_jitter() +
  scale_y_log10("CO2 Emissions", labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10("GDP per Capita (USD)", labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  stat_smooth(method = 'lm', se = FALSE, col = "blue") 




countries %>%
  mutate(pop_GDP = gdp / population) %>%
  mutate(is_high_GDP = ifelse(pop_GDP >= 23000, "High GDP", "Other")) %>%  # GDP 기준으로 그룹화
  ggplot(aes(x = pop_GDP, y = co2_emissions, color = is_high_GDP)) +  # 그룹에 따라 색상 변경
  geom_jitter() +
  scale_y_log10("CO2 Emissions", labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10("GDP per Capita (USD)", labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  stat_smooth(method = 'lm', se = FALSE, col = "blue") +
  scale_color_manual(values = c("High GDP" = "red", "Other" = "black")) +  # 색상 설정
  theme_minimal() +
  theme(legend.position = "none") 


countries%>%
  mutate("urban_ratio"=urban_population/population)%>%
  mutate("pop_GDP"=gdp/population)%>%
  ggplot(aes(x = pop_GDP,y = urban_ratio, color = country))+
  scale_colour_manual(values = c("Korea" = "red"))+
  geom_point()+
  scale_y_continuous("Urban Population %")+
  scale_x_log10("personal GDP(USD)",labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  stat_smooth(method = 'lm', se = FALSE, col = "blue") 


#정치형태 우리나라의 정치형태태

countries %>%
  mutate(pop_GDP = gdp / population) %>%
  filter(democracy_type == "Flawed democracy") %>%
  ggplot(aes(x = pop_GDP,color = country)) +
  geom_jitter(aes(y = democracy_type)) +
  scale_x_log10("GDP per Capita (USD)", labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(x = "GDP per Capita (USD)", y = "Democracy Type")+
  scale_colour_manual(values = c("Korea" = "red"))+
  ggtitle("Flawed democracy and GDP per capita")


korea_gdp <- countries %>%
  filter(country == "Korea") %>%
  summarise(korea_gdp = gdp / population) %>%
  pull(korea_gdp)

# 2. 한국보다 인당 GDP가 높은 나라들을 필터링
countries %>%
  mutate(pop_GDP = gdp / population) %>%
  filter(democracy_type == "Flawed democracy") %>%
  filter(pop_GDP > korea_gdp)%>%
  select(country,pop_GDP)%>%
  arrange(desc(pop_GDP))
