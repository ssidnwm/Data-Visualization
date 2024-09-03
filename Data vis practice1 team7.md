# Data vis practice1

생성자: 벼리 문
생성 일시: 2024년 9월 3일 오후 4:01

- 22000245 문벼리, 
    
    ```jsx
    setwd("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis")
    CIC <- read.csv("Cost_of_Living_Index_by_Country_2024.csv")
    library(ggplot2)
    library(tidyr)
    library(dplyr)
    ```
    
    사용한 라이브러리 ggplot와 tidyr, dplyr  
    
    데이터 로드 후 CIC라는 이름으로 저장
    
    ```jsx
    Task 1.For the variables “Cost of Living Index”, “Rent Index”, “Groceries Index”, “Restaurants Index”, 
    #and “Local Purchasing Power”. Draw scatterplots for all combinations of these
    #variables which will be 10 plots in total.
    #Highlight (with difference point color or text annotation) the relative position of “South
    #Korea” in the plots, and state what you have observed from those plots especially for living
    #expense in “South Korea”. 
    ```
    
    ```jsx
    CIC %>% #1번째 플롯, 생활비와 임대료에 관한 관계
      ggplot(aes(x =Cost.of.Living.Index, y = Rent.Index,color = Country ))+
      geom_jitter(alpha = 0.6)+
      scale_colour_manual(values = c("South Korea" = "red"))+
      stat_smooth(method = 'lm', se = F, col = "Blue")
    ```
    
    ![image.png](Data%20vis%20practice1%2032c4f1d58e2540f191c36c7602668a55/image.png)
    
    1번째 플롯의 경우에는 생활비와 임대료의 관계를 보여주며, 한국의 경우 뉴욕과 비교해서 생활비는 60퍼센트 정도의  수치이지만 렌트비가 20퍼센트보다 더 아래임을 보이며 국제적인 추세에서 한국의 임대료가 생활비에 비해 높지 않음을 확인할 수 있었다.
    
    ---
    
    ---
    
    ```jsx
    
    CIC %>%# 2번째 플롯, 생활비와 식료품에 관한 관계
      ggplot(aes(x =Cost.of.Living.Index, y = Groceries.Index, color = Country))+
      geom_jitter(alpha = 0.6)+
      scale_colour_manual(values = c("South Korea" = "red"))+
      stat_smooth(method = 'lm', se = F, col = "Blue")
    ```
    
    ![image.png](Data%20vis%20practice1%2032c4f1d58e2540f191c36c7602668a55/image%201.png)
    
    2번째 그래프에서는 생활비와 식료품의 관계를 확인할 수 있었는데, 한국의 경우 식료품비가 국제적인 추세로 보았을때 생활비에 비해 높게 책정되어있음을 확인할 수 있었으며, 이는 뉴욕과 비교해도 큰 차이가 나지 않는것으로 보아 한국의 식료품비가 비싼 편임을 확인할 수 있다.
    
    ```jsx
    CIC %>%# 3번째 플롯, 생활비와 식당지수에 관한 관계 
      ggplot(aes(x =Cost.of.Living.Index, y = Restaurant.Price.Index ,color = Country))+
      geom_jitter(alpha = 0.6)+
      scale_colour_manual(values = c("South Korea" = "red"))+
      stat_smooth(method = 'lm', se = F, col = "Blue")ㅇ
    ```
    
    ![image.png](Data%20vis%20practice1%2032c4f1d58e2540f191c36c7602668a55/image%202.png)
    
    다음으로 생활비와 외식비에 대한 비교인데, 놀랍게도 한국의 외식비는 뉴욕의 30퍼센트 수준이며, 이는 국제적인 추세에 있어서도 상당히 저렴한 편임을 보여준다. 기준이 뉴욕과의 비교이다 보니 한국은 생활비에 비해서 외식비가 상당히 저렴함을 확인할 수 있다. 다만 우리가 체감하는 외식비와는 많이 다른 것 같다.
    
    ```jsx
    CIC %>%#4번째 플롯, 생활비와 지역 구매력에 관한 관계계
      ggplot(aes(x =Cost.of.Living.Index, y = Local.Purchasing.Power.Index,color = Country ))+
      geom_jitter(alpha = 0.6)+
      scale_colour_manual(values = c("South Korea" = "red"))+
      stat_smooth(method = 'lm', se = F, col = "Blue")
    ```
    
    ![image.png](Data%20vis%20practice1%2032c4f1d58e2540f191c36c7602668a55/image%203.png)
    
    다음으로는 생활비와 구매력 간의 데이터인데, 한국의 경우 생활비와 비교해서 구매력은 국제적인 추세에서 살짝 위쪽에 있지만 한국 이상으로 구매력이 높은 나라가 상당히 많이 관찰되었다.
    
    ```jsx
    CIC %>%#5번째 플롯, 임대료와 식료품에 관한 관계 
      ggplot(aes(x =Rent.Index, y = Groceries.Index ,color = Country))+
      geom_jitter(alpha = 0.6)+
      scale_colour_manual(values = c("South Korea" = "red"))+
      stat_smooth(method = 'lm', se = F, col = "Blue")
    ```
    
    ![image.png](Data%20vis%20practice1%2032c4f1d58e2540f191c36c7602668a55/image%204.png)
    
    다음으로 5번째 플롯은 렌트비와 식료품에 대한 관계이다. 한국은 렌트비와 비교해서도 저렴한 임대료에 비해 식료품이 많이 비싸다는 것을 확인할 수 있다. 그러나 실제 우리가 느끼기에는 오히려 임대료가 매우 비싸고 식료품은 그에 비해서는 조금 덜 비싸다고 인식하는 것으로 보아 한국의 상황보다는 뉴욕과의 비교이기에 생기는 괴리라고 생각한다. 
    
    뉴욕의 경우에는 매우 높은 임대료를 가지고 식료품비가 그보다는 조금 낮은 편이라고 생각할 수 있을 것 같다
    
    ```jsx
    CIC %>%#6번째 플롯, 임대료와 식당지수에 관한 관계계
      ggplot(aes(x =Rent.Index, y = Restaurant.Price.Index,color = Country ))+
      geom_jitter(alpha = 0.6)+
      scale_colour_manual(values = c("South Korea" = "red"))+
      stat_smooth(method = 'lm', se = F, col = "Blue")
    ```
    
    ![image.png](Data%20vis%20practice1%2032c4f1d58e2540f191c36c7602668a55/image%205.png)
    
    ```jsx
    CIC %>%#7번째 플롯, 임대료와 지역 구매력에 관한 관계 
      ggplot(aes(x =Rent.Index, y = Local.Purchasing.Power.Index,color = Country ))+
      geom_jitter(alpha = 0.6)+
      scale_colour_manual(values = c("South Korea" = "red"))+
      stat_smooth(method = 'lm', se = F, col = "Blue")
    ```
    
    ![image.png](Data%20vis%20practice1%2032c4f1d58e2540f191c36c7602668a55/image%206.png)
    
    ```jsx
    CIC %>%#8번째 플롯 식료품과 식당지수에 관한 관계 
      ggplot(aes(x =Groceries.Index, y = Restaurant.Price.Index ,color = Country))+
      geom_jitter(alpha = 0.6)+
      scale_colour_manual(values = c("South Korea" = "red"))
    ```
    
    ![image.png](Data%20vis%20practice1%2032c4f1d58e2540f191c36c7602668a55/image%207.png)
    
    ```jsx
    CIC %>%#9번째 플롯, 식료품과 지역 구매력에 관한 관계계
      ggplot(aes(x =Groceries.Index, y = Local.Purchasing.Power.Index ,color = Country))+
      geom_jitter(alpha = 0.6)+
      scale_colour_manual(values = c("South Korea" = "red"))
    ```
    
    ![image.png](Data%20vis%20practice1%2032c4f1d58e2540f191c36c7602668a55/image%208.png)
    
    ```jsx
    CIC %>%#10번째 플롯, 식당지수와 지역 구매력에 관한 관계계
      ggplot(aes(x =Restaurant.Price.Index, y = Local.Purchasing.Power.Index,color = Country ))+
      geom_jitter(alpha = 0.6)+
      scale_colour_manual(values = c("South Korea" = "red"))
    ```
    
    ![image.png](Data%20vis%20practice1%2032c4f1d58e2540f191c36c7602668a55/image%209.png)
    
    Task 2:
    
    하나의 그래프를 선택하여, 추가적인 처리를 한 뒤 메세지를 강조하라
    
    ```jsx
    CIC %>%#7번째 플롯, 임대료와 지역 구매력에 관한 관계 
      ggplot(aes(x =Rent.Index, y = Local.Purchasing.Power.Index,color = Country ))+
      geom_jitter(alpha = 0.6)+
      scale_colour_manual(values = c("South Korea" = "red"))+
      stat_smooth(method = 'lm', se = F, col = "Blue")+
      geom_text(data = CIC[CIC$Country == "South Korea",], 
                aes(label = paste("South Korea\n(", round(Rent.Index, 1), ", ", round(Local.Purchasing.Power.Index, 1), ")", sep = "")), 
                vjust = -1, hjust = 1, color = "red", size = 3)
    ```
    
    우리는 특히나 한국의 20대 청년이 가장 큰 관심을 가지는 임대료와, 또 수입과 비슷한 관계라고 생각하는 구매력 사이의 관계를 더욱 깊게 살펴보고자 하였다. 그리고 우리는 이 그래프를 확인했을때, 우리나라가 국제적인 수준에서 조금 더 살기 좋은 나라라고 생각했다.
    
    ![image.png](Data%20vis%20practice1%2032c4f1d58e2540f191c36c7602668a55/image%2010.png)
    
    국제적 추세와 한국의 포인트를 확인했을때, 특히나 뉴욕의 살인적인 임대료를 비교하였을때 우리나라의 임대료는 많이 비싼편이 아니라고 생각한다. 다만, 포인트가 찍힌 점을 생각해 보았을때, 대부분의 나라가 뉴욕보다 저렴한 임대료를 가지고 있고, 그만큼 구매력이 낮기 때문에 조금 더 자세한 비교가 될만한 국가를 찾아 보았다.
    
    ```jsx
    
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
    ```
    
    한국과 비슷하거나 조금 더 잘 사는 나라가 어디있을까? 한국보다 임대료가 더욱 저렴한(뉴욕에 비해) 국가들 중 우리나라보다 더욱 구매력이 높은 국가를 필터링해 그래프에 표시해 보았다.
    
    ![image.png](Data%20vis%20practice1%2032c4f1d58e2540f191c36c7602668a55/image%2011.png)
    
    한국보다 저렴한 렌트비용에, 구매력이 좋은 국가들을 찾아보니, oman, saudi, japan이 확인이 되었다.
    

타이틀 적을 것(그래프)