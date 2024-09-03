getwd()
setwd("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis")
CIC <- read.csv("Cost_of_Living_Index_by_Country_2024.csv")
library(ggplot2)
library(tidyr)
library(dplyr)

#Task 1.For the variables “Cost of Living Index”, “Rent Index”, “Groceries Index”, “Restaurants Index”, 
#and “Local Purchasing Power”. Draw scatterplots for all combinations of these
#variables which will be 10 plots in total.
#Highlight (with difference point color or text annotation) the relative position of “South
#Korea” in the plots, and state what you have observed from those plots especially for living
#expense in “South Korea”. 

#우선 이 변수들을 사용한 ggplot을 나타내보자
summary(CIC)
CIC %>% #1번째 플롯, 생활비와 임대료에 관한 관계계 
  ggplot(aes(x =Cost.of.Living.Index, y = Rent.Index,color = Country ))+
  geom_jitter(alpha = 0.6)+
  scale_colour_manual(values = c("South Korea" = "red"))+
  stat_smooth(method = 'lm', se = F, col = "Blue")
#생활비와 임대료 사이의 관계에 있어서 한국은 어느정도 국제적인 추세선에 대해 조금 아래쪽에 떨어진 모습으로,
#LA에 비해 생활비는 60%정도의 수준이며 임대료는 20%에 조금 미치지 못한다. 그러므로 생활비에 비해 임대료가 상대적으로
#더 낮은 편에 속하게 된다. 


CIC %>%# 2번째 플롯, 생활비와 식료품에 관한 관계계
  ggplot(aes(x =Cost.of.Living.Index, y = Groceries.Index, color = Country))+
  geom_jitter(alpha = 0.6)+
  scale_colour_manual(values = c("South Korea" = "red"))+
  stat_smooth(method = 'lm', se = F, col = "Blue")
#이번에는 생활비와 식료품 ㅅ ㅏ이의 관계인데, 생활비는 동일한 상태에서 국제적인 비율을 보았을떄, 식료품은 LA에 비해
#85%를 넘는 상태로, 국가적인 경향이나, LA와의 비교를 생각했을때 식료품의 가격이 상대적으로 더 비싸다고 할 수 있다.

CIC %>%# 3번째 플롯, 생활비와 식당지수에 관한 관계 
  ggplot(aes(x =Cost.of.Living.Index, y = Restaurant.Price.Index ,color = Country))+
  geom_jitter(alpha = 0.6)+
  scale_colour_manual(values = c("South Korea" = "red"))+
  stat_smooth(method = 'lm', se = F, col = "Blue")
#생활비와 식당지수 사이의 관계인데, 이 통계에서는 한국의 외식 식당지수는 국제적인 경향에 비해 상대적으로
#낮아 외식물가는 그리 비싸지 않다고 볼 수 있다.

CIC %>%#4번째 플롯, 생활비와 지역 구매력에 관한 관계계
  ggplot(aes(x =Cost.of.Living.Index, y = Local.Purchasing.Power.Index,color = Country ))+
  geom_jitter(alpha = 0.6)+
  scale_colour_manual(values = c("South Korea" = "red"))+
  stat_smooth(method = 'lm', se = F, col = "Blue")
#생활비와 구매력에 관한 관계에서는 한국의 구매력이 100이상으로 상당히 높아 그만큼 벌이가 좋다는 것을 의미한다.


CIC %>%#5번째 플롯, 임대료와 식료품에 관한 관계 
  ggplot(aes(x =Rent.Index, y = Groceries.Index ,color = Country))+
  geom_jitter(alpha = 0.6)+
  scale_colour_manual(values = c("South Korea" = "red"))+
  stat_smooth(method = 'lm', se = F, col = "Blue")
#번외로 임대료에 비해 식료품의 가격이 매우 비싸다

CIC %>%#6번째 플롯, 임대료와 식당지수에 관한 관계계
  ggplot(aes(x =Rent.Index, y = Restaurant.Price.Index,color = Country ))+
  geom_jitter(alpha = 0.6)+
  scale_colour_manual(values = c("South Korea" = "red"))+
  stat_smooth(method = 'lm', se = F, col = "Blue")
#국제 추세선과 매우 비슷한 그래프

CIC %>%#7번째 플롯, 임대료와 지역 구매력에 관한 관계 
  ggplot(aes(x =Rent.Index, y = Local.Purchasing.Power.Index,color = Country ))+
  geom_jitter(alpha = 0.6)+
  scale_colour_manual(values = c("South Korea" = "red"))+
  stat_smooth(method = 'lm', se = F, col = "Blue")

CIC %>%#8번째 플롯 식료품과 식당지수에 관한 관계 
  ggplot(aes(x =Groceries.Index, y = Restaurant.Price.Index ,color = Country))+
  geom_jitter(alpha = 0.6)+
  scale_colour_manual(values = c("South Korea" = "red"))
CIC %>%#9번째 플롯, 식료품과 지역 구매력에 관한 관계계
  ggplot(aes(x =Groceries.Index, y = Local.Purchasing.Power.Index ,color = Country))+
  geom_jitter(alpha = 0.6)+
  scale_colour_manual(values = c("South Korea" = "red"))
CIC %>%#10번째 플롯, 식당지수와 지역 구매력에 관한 관계계
  ggplot(aes(x =Restaurant.Price.Index, y = Local.Purchasing.Power.Index,color = Country ))+
  geom_jitter(alpha = 0.6)+
  scale_colour_manual(values = c("South Korea" = "red"))
#종합하자면, 한국은 식료품이 비싼 대신 외식비가 상대적으로 저렴하고, 임대료가 싸며 구매 수준이 높다고 할 수 있다.


#TASK2 : Choose one or more graphs from task 1 to support your statement and refine the graphs 
#to highlight your message. You need to put proper graph label, x and y axis label,
#and tick label for clear explanation. This is the task to practice explanatory data visualization 
#for effective data-story telling.You might use some other techniques to improve your graphs and effective 
#message delivery
 
#임대료와 구매력에 대한 내용을 가지고, 한국은 국제적으로 살기가 좋다라는 주장을 뒷받침하려고 한다.
CIC %>%#7번째 플롯, 임대료와 지역 구매력에 관한 관계 
  ggplot(aes(x =Rent.Index, y = Local.Purchasing.Power.Index,color = Country ))+
  geom_jitter(alpha = 0.6)+
  scale_colour_manual(values = c("South Korea" = "red"))+
  stat_smooth(method = 'lm', se = F, col = "Blue")+
  geom_text(data = CIC[CIC$Country == "South Korea",], 
            aes(label = paste("South Korea\n(", round(Rent.Index, 1), ", ", round(Local.Purchasing.Power.Index, 1), ")", sep = "")), 
            vjust = -1, hjust = 1, color = "red", size = 3)

#이번에는 한국보다 위쪽에 있는 국가들을 확인해보고자 함
south_korea_values <- CIC[CIC$Country == "South Korea", c("Rent.Index", "Local.Purchasing.Power.Index")]
#한국의 값을 기본적으로 저장한 후, 한국보다 값이 높은 국가들을 filter한다
filtered_countries <- CIC %>%
  filter(Rent.Index < south_korea_values$Rent.Index & 
           Local.Purchasing.Power.Index > south_korea_values$Local.Purchasing.Power.Index)

CIC %>%#7번째 플롯, 임대료와 지역 구매력에 관한 관계 
  ggplot(aes(x =Rent.Index, y = Local.Purchasing.Power.Index,color = Country ))+
  geom_jitter(alpha = 0.6)+
  scale_colour_manual(values = c("South Korea" = "red","Japan"= "blue","Oman"= "blue","Saudi Arabia" = "blue"))+
  stat_smooth(method = 'lm', se = F, col = "Blue")+
  geom_text(data = CIC[CIC$Country == "South Korea",], 
            aes(label = paste("South Korea\n(", round(Rent.Index, 1), ", ", round(Local.Purchasing.Power.Index, 1), ")", sep = "")), 
            vjust = -1, hjust = 1, color = "red", size = 3)+
  geom_text(data = filtered_countries, 
            aes(label = Country), 
            vjust = -1, hjust = 1, color = "blue", size = 2)
#기준이 되는 뉴욕에 비해, 임대료는 16%, 구매력은 109%로 더욱 살기 좋음.

