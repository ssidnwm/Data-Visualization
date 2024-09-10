# Data vis practice2

생성자: 벼리 문
생성 일시: 2024년 9월 7일 오후 4:15

타이틀 적을 것(그래프)

- 문벼리
    
    ```jsx
    
    countries <- read.csv("https://raw.githubusercontent.com/ssidnwm/Data-Visualization/main/myRVis/All%20Countries.csv")
    
    ```
    
    데이터 다운로드
    
    ### Task1: 인구가 많은 국가가 탄소 배출량도 많은가? 이를 확인하기 위한 scatterplot을 그려보자
    
    ```jsx
    
    countries%>%
      ggplot(aes(x = population, y =co2_emissions))+
      geom_point()
    ```
    
    ![image.png](https://github.com/ssidnwm/Data-Visualization/blob/main/Data%20vis%20practice2/image.png?raw=true)
    
    ### Task1-2: scale 함수나 labs , ggtitle 등의 함수를 활용하여 그래프를 더 잘 읽을수 있도록 보완하자
    
    ```jsx
    countries%>%
      ggplot(aes(x = population, y =co2_emissions))+
      geom_point()+
      ggtitle("Correlation b/w log(Pop) and log (co2 emissions)")+
      scale_y_log10("Co2 emissions",labels = scales::trans_format("log10", scales::math_format(10^.x)))+
      scale_x_log10("POP",labels = scales::trans_format("log10", scales::math_format(10^.x)))
    ```
    
    ![image.png](https://github.com/ssidnwm/Data-Visualization/blob/main/Data%20vis%20practice2/image%201.png?raw=true)
    
    지수로 표현되는 값들에 대해 보다 그 값이 정규화되도록 log를 사용해 값들을 흩뿌려 시각화함
    
    여기서 사용된 scale_y_log10은 y의 값에 log10을 적용하는 함수이며, labels들은 포멧을 바꾸는 역할이며 trans_format에서 “log10”은 데이터가 log10으로 변환되어있다는 사실을 알리기 위해 사용되며 이후 scales:: math_format(10^.x)부분은 실제 시각화 자료에서 각 축에 값들이 10의 ^x제곱으로 표시됨을 의미함
    
    ### Task2-1: 산업이 발달하여 인당 GDP 가 높은 국가는 도시 인구 비율이 더 높은지 나타내는scatterplot 을 아래와 같이 그려보자
    
    - 도시 인구의 비율은 도시 인구를 전체 인구로 나누어 계산할 수 있다
    - 인당GDP 는 GDP 를 인구수로 나누어 계산할 수 있다
    
    ```jsx
    #도시 인구의 비율을 구하는 식
    countries%>%
      mutate("urban_ratio"=urban_population/population)
    #인당 GDP를 구하는 식
    countries%>%
      mutate("urban_ratio"=urban_population/population)%>%
      mutate("pop_GDP"=gdp/population)
    #이후 pop_GDP와 urban_ratio를 사용하여 scatterplot을 그려보자
    countries%>%
      mutate("urban_ratio"=urban_population/population)%>%
      mutate("pop_GDP"=gdp/population)%>%
      ggplot(aes(x = pop_GDP,y = urban_ratio))+
      geom_point()+
      scale_y_continuous("Urban Population %")+
      scale_x_log10("personal GDP(USD)",labels = scales::trans_format("log10", scales::math_format(10^.x)))
    ```
    
    ![image.png](https://github.com/ssidnwm/Data-Visualization/blob/main/Data%20vis%20practice2/image%202.png?raw=true)
    
    도시 인구비율과 인당GDP에 대해 시각화하였다, y축의 경우 %값으로 0~1사이의 분포되어 있으며 personal GDP는 달러값으로 지수함수로 표현이 되었다. 여기서 추가로 몇가지 신기한 이상치들만 체크해보려고 한다.
    
    ```jsx
    #가장 먼저 우 하단에 있는 국가 도시인구 비중이 낮고, 인당 gpd가 높다.
    low_pop<-countries%>%
      mutate("urban_ratio"=urban_population/population)%>%
      mutate("pop_GDP"=gdp/population)%>%
      filter(urban_ratio < 0.25, pop_GDP > 100000)
    #도시비율을 0.25아래로, gdp를 10^5로 필터를 하니 Liechtenstein이라는 국가가 나왔다.
    
    #우 상단의 국가, 도시인구 비중이 높고 인당 gdp가 높다
    hi_pop<-countries%>%
      mutate("urban_ratio"=urban_population/population)%>%
      mutate("pop_GDP"=gdp/population)%>%
      filter(urban_ratio >= 1, pop_GDP > 100000)%>%
      select(country)
      #도시비율을 100%로, gdp를 10^5로 필터를 하니 Monaco가 나왔다.
      
    #마지막 좌하단의 국가, 도시인구 비중이 낮고, 인당 gdp가 낮다
    lolo<-countries%>%
      mutate("urban_ratio"=urban_population/population)%>%
      mutate("pop_GDP"=gdp/population)%>%
      filter(urban_ratio < 0.25, pop_GDP < 500)%>%
      select(country)
      #도시비중을 0.25미만으로, gdp를 500미만으로 필터하니 Burundi가 나왔다.
      
    #이제 이 국가들을 포함한 plot을 다시 그리자
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
    ```
    
    ![image.png](https://github.com/ssidnwm/Data-Visualization/blob/main/Data%20vis%20practice2/image%203.png?raw=true)
    
    ### Task2-2: 인당 GDP 와 합계 출산율 (fertility rate)의 관계를 나타내는 scatterplot 을 그려보자
    
    ```jsx
    countries%>%
      mutate("pop_GDP"=gdp/population)%>%
      ggplot(aes(x = pop_GDP, y = fertility_rate))+
      geom_point()+
      scale_x_log10("personal GDP(USD)",labels = scales::trans_format("log10", scales::math_format(10^.x)))+
      labs(y = "Fertility Rate")+
      ggtitle("Relation Personal GDP and Fertility Rate")
    ```
    
    ![image.png](https://github.com/ssidnwm/Data-Visualization/blob/main/Data%20vis%20practice2/image%204.png?raw=true)
    
    인당 gdp가 높은 국가들의 경우 합계출산률이 전반적으로 내려가는 경향을 보이고 있다.
    
    이중에서 우리나라의 경우를 찾아보자
    
    ```jsx
    countries%>%
      mutate("pop_GDP"=gdp/population)%>%
      ggplot(aes(x = pop_GDP, y = fertility_rate, color = country))+
      geom_point()+
      scale_colour_manual(values = c("Korea" = "red"))+
      scale_x_log10("personal GDP(USD)",labels = scales::trans_format("log10", scales::math_format(10^.x)))+
      labs(y = "Fertility Rate")+
      ggtitle("Relation Personal GDP and Fertility Rate")
    ```
    
    ![image.png](https://github.com/ssidnwm/Data-Visualization/blob/main/Data%20vis%20practice2/image%205.png?raw=true)
    
    ### Task2-3: 2-1, 2-2 의 그래프를 동일한 x 축 (GPD per Capita) 상에 나타내어라.
    
    ```jsx
    df_long <- countries %>%
      mutate("pop_GDP"=gdp/population)%>%
      mutate("urban_ratio"=urban_population/population)%>%
      select("pop_GDP", "fertility_rate", "urban_ratio") %>%
      pivot_longer(cols = c(fertility_rate, urban_ratio), 
                   names_to = "variable", 
                   values_to = "value")
                   
      # 두 개의 데이터프레임을 하나의 plot로 표현하기 위해 pivot_longer를 사용함
      
      ggplot(df_long, aes(x = pop_GDP, y = value)) +
      geom_point() +
      facet_grid(variable ~ ., scales = "free_y") +  # 서로 다른 y축을 사용
      scale_x_log10("GDP per Capita(USD)", labels = scales::trans_format("log10", scales::math_format(10^.x))) +
      labs(y = NULL)+   # y축 라벨 제거 (개별적으로 출력)
      ggtitle("Vis for fertility, urban ratio to pop GDP")
    ```
    
    ![image.png](https://github.com/ssidnwm/Data-Visualization/blob/main/Data%20vis%20practice2/image%206.png?raw=true)
    
    ### 정치형태별(democracy_type) 인당 gdp 를 표현하는 그래프를 아래와 같이 그려보자
    
    ```jsx
    
    countries%>%
      mutate("pop_GDP"=gdp/population)%>%
      ggplot(aes(x = democracy_type, y = pop_GDP, color = democracy_type))+
      geom_jitter()+
      scale_y_log10("GDP per Capita(USD)", labels = scales::trans_format("log10", scales::math_format(10^.x)))+
      theme(legend.position = "none")+
      labs(x="Democracy Type")+
      ggtitle("GDP per capita by political form")
    ```
    
    ![image.png](https://github.com/ssidnwm/Data-Visualization/blob/main/Data%20vis%20practice2/image%207.png?raw=true)
    
    ### Task 3-2:  정치 형태에 따른 평균 인당 gdp 를 계산하여 아래 그림과 같이 layer 를 추가해보자
    
    ```jsx
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
    ```
    
    ![image.png](https://github.com/ssidnwm/Data-Visualization/blob/main/Data%20vis%20practice2/image%208.png?raw=true)
    

정치형태별 인당GDP의 평균을 정확한 수치로 확인해보자

```jsx
countries%>%
  mutate("pop_GDP"=gdp/population)%>%
  group_by(democracy_type)%>%
  summarise(decGDP = mean(pop_GDP, na.rm = TRUE))%>%
  arrange(desc(decGDP))
```

![image.png](https://github.com/ssidnwm/Data-Visualization/blob/main/Data%20vis%20practice2/image%209.png?raw=true)

full democracy가 57631$,  unknown이 26745$, flawed democracy가 17741$이다.

### Task4: 각 국가별 지표들을 사용해서 흥미로운 insight 를 도출 할 수 있는 scatterplot 을그려보자

위에서 구한 인당GDP에 대한 시각화 자료들에서 한국의 경우를 더 찾아보자

인구 수 당 탄소배출량

```jsx
countries%>%
  ggplot(aes(x = population, y =co2_emissions,color = country))+
  geom_point()+
  scale_colour_manual(values = c("Korea" = "red"))+
  ggtitle("Correlation b/w log(Pop) and log (co2 emissions)")+
  scale_y_log10("Co2 emissions",labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_x_log10("POP",labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  stat_smooth(method = 'lm', se = F, col = "Blue")
```

![image.png](https://github.com/ssidnwm/Data-Visualization/blob/main/Data%20vis%20practice2/image%2010.png?raw=true)

한국의 탄소배출량은 조금 높은 편이다. 그렇다면 인당 GDP에 따른 탄소배출량은 어떨까?

1인당 GDP가 23,000달러 이상일 경우 추가적인 처리를 통해 경향을 보고자 한다

```jsx
countries %>%
  mutate(pop_GDP = gdp / population) %>%
  ggplot(aes(x = pop_GDP, y = co2_emissions)) +  # 그룹에 따라 색상 변경
  geom_jitter() +
  scale_y_log10("CO2 Emissions", labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10("GDP per Capita (USD)", labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  stat_smooth(method = 'lm', se = FALSE, col = "blue") 
```

단순히 인당GDP와 탄소배출량의 관계를 살펴보았다. 

![image.png](https://github.com/ssidnwm/Data-Visualization/blob/main/Data%20vis%20practice2/image%2011.png?raw=true)

해당 시각화 자료에서는 인당 gdp에 대해서 전반적으로 고른 분포가 나온것으로 보아 선진국들도 탄소배출 억제를 위해 노력하고 있음을 알 수 있다.

다음은 도시 인구 비중에 따른 인당GDP인데, 이 역시도 한국이 어디에 위치하는지 찾아보고자 한다

```jsx
#도시 인구의 비율을 구하는 식
countries%>%
  mutate("urban_ratio"=urban_population/population)
#인당 GDP를 구하는 식
countries%>%
  mutate("urban_ratio"=urban_population/population)%>%
  mutate("pop_GDP"=gdp/population)
#이후 pop_GDP와 urban_ratio를 사용하여 scatterplot을 그려보자
countries%>%
  mutate("urban_ratio"=urban_population/population)%>%
  mutate("pop_GDP"=gdp/population)%>%
  ggplot(aes(x = pop_GDP,y = urban_ratio))+
  scale_colour_manual(values = c("Korea" = "red"))+
  geom_point()+
  scale_y_continuous("Urban Population %")+
  scale_x_log10("personal GDP(USD)",labels = scales::trans_format("log10", scales::math_format(10^.x)))
```

![image.png](https://github.com/ssidnwm/Data-Visualization/blob/main/Data%20vis%20practice2/image%2012.png?raw=true)

![image.png](https://github.com/ssidnwm/Data-Visualization/blob/main/Data%20vis%20practice2/image%2013.png?raw=true)

한국의 경우 도시중심 비중이 높고, 인당 gdp역시 높은것을 볼 수 있다. 그러나 전 세계적인 추세로 본다면 한국의 수도권 과밀화가 다른 나라들만큼 크지 않다고 볼수도 있다.

다음으로 우리나라가 속한 정치형태인 flawed democracy와 인당 GDP사이의 관계를 살펴본 후, 우리나라의 위치를 찾아보자

```jsx
countries %>%
  mutate(pop_GDP = gdp / population) %>%
  filter(democracy_type == "Flawed democracy") %>%
  ggplot(aes(x = pop_GDP,color = country)) +
  geom_jitter(aes(y = democracy_type)) +
  scale_x_log10("GDP per Capita (USD)", labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(x = "GDP per Capita (USD)", y = "Democracy Type")+
  scale_colour_manual(values = c("Korea" = "red"))+
  ggtitle("Flawed democracy and GDP per capita")
```

![image.png](https://github.com/ssidnwm/Data-Visualization/blob/main/Data%20vis%20practice2/image%2014.png?raw=true)

flawed democracy안에서 우리나라는의 위치를 시각화해 보았다. 그렇다면 어느 나라들이 우리나라 이상의 인당GDP를 가지고 있는지 확인해보자

```jsx
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

```

![image.png](https://github.com/ssidnwm/Data-Visualization/blob/main/Data%20vis%20practice2/image%2015.png?raw=true)

데이터프레임으로 확인하였을때, 다음과 같은 국가들이 나온다.