library(ggplot2)
str(mtcars)
ggplot(mtcars, aes(x = cyl, y = mpg)) + 
  geom_point()

#cyl은 categori에 대한 변수이나, ggplot에서는 그 값에 따라 정렬하였기에, 이를 factor로 변환한다.
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) + 
  geom_point()


ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))+
  geom_jitter(alpha = 0.6)+
  facet_grid(. ~Species)+
  stat_smooth(method = 'lm',se = F, col = "Blue")

levels(iris$Species) <- c("Setosa","Versicolor","Virginica")

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))+
  geom_jitter(alpha = 0.6)+
  facet_grid(Species~ .)+
  stat_smooth(method = 'lm', se = F, col = "Blue")+
  scale_y_continuous("Sepal width (Cm)",
                     limits = c(2,5),
                     expand = c(0,0))+
  scale_x_continuous("Sepal Length (cm)",
                     limits = c(4,8),
                     expand = c(0,0))+
  coord_equal()+
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text = element_blank(),
        panel.spacing = unit(1,"lines")
        )

load(url('https://github.com/hbchoi/SampleData/raw/master/weather.RData'))
library(tidyr)
library(dplyr)
#exercise weather2 테이블로부터월별min, max, mean.temperature의평균을계산하시오. (tidyr, dplyr을pipe로연동)
weather2%>%
  gather(days,temp,-year,-month,-measure)%>%
  spread(measure,temp)%>%
  group_by(year,month)%>%
  mutate(Mean.TemperatureF = as.numeric(gsub("[^0-9.]", "", Mean.TemperatureF)))%>%
  summarise(maxt = max(Max.TemperatureF,na.rm = T),mint = min(Min.TemperatureF, na.rm = T),
            meant = mean(Mean.TemperatureF,na.rm = T))%>%
  arrange(month)


#iris exercise해볼것
iris%>%
  gather(part,value,-Species)%>%
  separate(part, into = c("Part", "Measurement"), sep = "\\.")%>%
  mutate(value = as.numeric(value)) %>%
  ggplot(aes(x = Measurement,y = value,color = Part))+
  geom_jitter(alpha = 0.6)+
  facet_grid(. ~Species)+
  stat_smooth(method = 'lm',se = F, col = "Blue")# 이때 추세선은 그려지지 않는데 그 이유는 x,y의 값들 중 숫자형이 아닌 
#범주형 데이터가 있기 때문

iris%>%
  gather(part,value,-Species)%>%
  separate(part, into = c("Part", "Measurement"), sep = "\\.")%>%
  mutate(value = as.numeric(value)) %>%
  ggplot(aes(x = Species,y = value,color = Part))+
  geom_jitter(alpha = 0.6)+
  facet_grid(. ~Measurement)



