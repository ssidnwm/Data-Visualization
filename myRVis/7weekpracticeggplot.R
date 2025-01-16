library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(openxlsx)

spo <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/Spotify_Youtube.csv")

spt_Artist<-spo %>% 
  group_by(Artist)%>%
  select(Artist)%>%
  distinct()
print(spt_Artist)
head(spt_Artist)
# Artist 열을 벡터로 추출
artist_vector <- spt_Artist %>% pull(Artist)

# 모든 값을 쉼표로 묶어서 하나의 문자열로 변환
artist_string <- paste(artist_vector, collapse = ", ")

# 결과 출력
artist_string

# 한국인 솔로 가수 및 그룹 리스트
korean_artists <- c("BTS", "EXO", "BLACKPINK", "TWICE", "Stray Kids", "Red Velvet", "NCT", 
                    "SEVENTEEN", "BIGBANG", "SHINee", "(G)I-DLE", "ATEEZ", "TOMORROW X TOGETHER", 
                    "ENHYPEN", "IVE", "LE SSERAFIM", "NewJeans", "TAEYANG", "RM", "Jimin", 
                    "j-hope", "IU", "Girls' Generation", "JAY PARK", "ITZY")

ko_sty<-spo %>%
  filter(Artist %in% korean_artists)
library(scales)
ko_sty %>%
  group_by(Artist) %>%
  summarise(average_stream = mean(Stream, na.rm = TRUE)) %>%
  ggplot(aes(x = Artist, y = average_stream)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # x축 텍스트를 45도 기울임

ko_sty %>%
  group_by(Artist) %>%
  summarise(average_stream = mean(Stream, na.rm = TRUE)) %>%
  summarise(average_views = mean(Views,na.rm = TRUE))%>%
  ggplot(ae(x = Artist, y = ))

ko_sty_summary <- ko_sty %>%
  group_by(Artist) %>%
  summarise(average_stream = mean(Stream, na.rm = TRUE),
            average_views = mean(Views, na.rm = TRUE)) %>%
  gather(key = "Metric", value = "Value", average_stream, average_views)

# 시각화
ggplot(ko_sty_summary, aes(x = Artist, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "stack") +  # 겹치는 방식으로 설정
  labs(y = "Value (Stream/Views)", title = "Comparison of Average Stream and Views") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # x축 레이블을 45도 기울임
ko_sty %>%
  filter(Artist == "IU")%>%
  select(Title, Stream, Views) 

ko_sty_IU <- ko_sty %>%
  filter(Artist == "IU") %>%
  select(Title, Stream, Views) %>%
  # Title을 짧게 지정할 수 있도록 새 열 생성
  mutate(Short_Title = case_when(
    Title == "[MV] IU(아이유)_LILAC(라일락)" ~ "라일락",
    Title == "[MV] IU(아이유) _ eight(에잇) (Prod.&Feat. SUGA of BTS)" ~ "에잇",
    Title == "[MV] IU(아이유) _ BBIBBI(삐삐)" ~ "삐삐",
    Title == "[MV] IU(아이유) _ Blueming(블루밍)" ~ "블루밍",
    Title == "IU & Oh Hyuk - Can't Love You Anymore | 아이유 & 오혁 - 사랑이 잘 [Yu Huiyeol's Sketchbook / 2017.07.26]" ~ "사랑이 잘",
    Title == "[MV] IU(아이유) _ Celebrity" ~ "Celebrity",
    Title == "[MV] IU(아이유) _ strawberry moon" ~ "strawberry moon",
    Title == "[MV] IU(아이유) _ Palette(팔레트) (Feat. G-DRAGON)" ~ "Palette",
    Title == "[MV] IU(아이유) _ Through the Night(밤편지)" ~ "밤편지",
    Title == "IU (아이유) _ Good Day (좋은 날) _ MV" ~ "좋은 날",
    Title == "IU (아이유) _ Good Day (좋은 날) _ MV" ~ "좋은 날",
    TRUE ~ Title  # 나머지 곡은 원래 제목 사용
  )) %>%
  gather(key = "Metric", value = "Value", Stream, Views)
# 시각화
ggplot(ko_sty_IU, aes(x = Short_Title, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +  # 막대 그래프 나란히 배치
  labs(title = "Comparison of Stream and Views for Each Song by IU", 
       y = "Value (Stream/Views)", x = "Song Title") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # x축 텍스트 기울임

install.packages("tidyverse")
install.packages("tm")
install.packages("wordcloud")
library(tidyverse)
library(tm)
library(wordcloud)


music_data<-  read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/Spotify_Youtube.csv")


# Filter videos with high views
high_view_videos <- music_data %>% filter(Views > quantile(Views, 0.75, na.rm = TRUE))
low_view_videos <- music_data %>% filter(Views > quantile(Views, 0.25, na.rm = TRUE))

# Create corpus of titles
corpus <- Corpus(VectorSource(high_view_videos$Title))
corpus <- Corpus(VectorSource(high_view_videos$Title))

# Clean and tokenize titles
corpus_clean <- tm_map(corpus, content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace)

# Create word cloud
wordcloud(corpus_clean, max.words = 100, random.order = FALSE)


#################################
#################################
#################################
#################################
# Install and load required library
library(reshape2)
library(ggplot2)

# Select only numeric columns from the dataset
numeric_data <- music_data %>% select_if(is.numeric)

# Correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Melt the correlation matrix for use in ggplot
melted_cor_matrix <- melt(cor_matrix)

# Plot heatmap using ggplot2
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed()

#################################
#################################
#################################
#################################

# 새로운 열 추가: YouTube 수익 (Revenue from YouTube Views)
music_data <- music_data %>%
  mutate(
    YouTube_Revenue = ifelse(Views <= 10000, Views * 0.06, Views * 0.12)
  )

# 새로운 열 추가: Spotify 수익 (Revenue from Spotify Streams)
music_data <- music_data %>%
  mutate(
    Spotify_Revenue = Stream * (0.04 / 10) # 10스트리밍 당 $0.04 계산
  )

# YouTube와 Spotify의 총 수익 계산
music_data <- music_data %>%
  mutate(
    Total_Revenue = YouTube_Revenue + Spotify_Revenue
  )

# 결과 확인
head(music_data %>% select(Track, Artist, Views, Stream, YouTube_Revenue, Spotify_Revenue, Total_Revenue))


#################################
#################################
#################################
#################################


# Install and load required library for sorting
library(dplyr)

# 가수별로 수익 합산 및 Top 10 추출
top_artists <- music_data %>%
  group_by(Artist) %>%
  summarise(Total_Revenue = sum(Total_Revenue, na.rm = TRUE)) %>%
  arrange(desc(Total_Revenue)) %>%
  top_n(10, Total_Revenue)

# 결과 확인
print(top_artists)







# 상위 10명의 가수 리스트 추출
top_artists_list <- top_artists$Artist

# Top 10 가수의 노래만 포함된 데이터프레임 생성
top_singer_data <- music_data %>%
  filter(Artist %in% top_artists_list) %>%
  arrange(Artist, desc(Total_Revenue))  # 각 가수별로 Total_Revenue에 따라 정렬

# 결과 확인
head(top_singer_data)






# Load ggplot2
library(ggplot2)

# Create a combined dataset for both Danceability and Energy
combined_data <- top_singer_data %>%
  select(Artist, Total_Revenue, Danceability, Energy) %>%
  pivot_longer(cols = c(Danceability, Energy), names_to = "Attribute", values_to = "Value")

# Plot for Danceability
ggplot(combined_data %>% filter(Attribute == "Danceability"), aes(x = Total_Revenue, y = Value, color = Artist, group = Artist)) +
  geom_line(size = 1) +  # Line plot
  geom_point(size = 2) +  # Points on the line
  labs(title = "Danceability vs Total Revenue", x = "Total Revenue", y = "Danceability") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Plot for Energy
ggplot(combined_data %>% filter(Attribute == "Energy"), aes(x = Total_Revenue, y = Value, color = Artist, group = Artist)) +
  geom_line(size = 1) +  # Line plot
  geom_point(size = 2) +  # Points on the line
  labs(title = "Energy vs Total Revenue", x = "Total Revenue", y = "Energy") +
  theme_minimal() +
  theme(legend.title = element_blank())








# Load required libraries
library(ggplot2)
library(tidyr)

# Create a combined dataset for all relevant attributes
combined_data <- top_singer_data %>%
  select(Artist, Total_Revenue, Key, Loudness, Speechiness, Acousticness, Instrumentalness, Liveness, Valence, Tempo, Duration_ms, Views, Likes, Comments, Stream) %>%
  pivot_longer(cols = c(Key, Loudness, Speechiness, Acousticness, Instrumentalness, Liveness, Valence, Tempo, Duration_ms, Views, Likes, Comments, Stream), 
               names_to = "Attribute", values_to = "Value")

# Create a list of attributes to plot
attributes_to_plot <- c("Key", "Loudness", "Speechiness", "Acousticness", 
                        "Instrumentalness", "Liveness", "Valence", 
                        "Tempo", "Duration_ms", "Views", 
                        "Likes", "Comments", "Stream")

# Loop through each attribute and create plots
for (attribute in attributes_to_plot) {
  p <- ggplot(combined_data %>% filter(Attribute == attribute), 
              aes(x = Total_Revenue, y = Value, color = Artist, group = Artist)) +
    geom_line(size = 1) +  # Line plot
    geom_point(size = 2) +  # Points on the line
    labs(title = paste(attribute, "vs Total Revenue"), x = "Total Revenue", y = attribute) +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  print(p)  # Print each plot
}







# 가수별로 수익 합산 및 Top 10 추출
top_artists <- music_data %>%
  group_by(Artist) %>%
  summarise(Total_Revenue = sum(Total_Revenue, na.rm = TRUE)) %>%
  arrange(desc(Total_Revenue)) %>%
  top_n(20, Total_Revenue)

# 결과 확인
print(top_artists)







# 상위 10명의 가수 리스트 추출
top_artists_list <- top_artists$Artist

# Top 10 가수의 노래만 포함된 데이터프레임 생성
top_singer_data <- music_data %>%
  filter(Artist %in% top_artists_list) %>%
  arrange(Artist, desc(Total_Revenue))  # 각 가수별로 Total_Revenue에 따라 정렬


# Load ggplot2
library(ggplot2)

# Create a combined dataset for both Danceability and Energy
combined_data <- top_singer_data %>%
  select(Artist, Total_Revenue, Danceability, Energy) %>%
  pivot_longer(cols = c(Danceability, Energy), names_to = "Attribute", values_to = "Value")

# Plot for Danceability
ggplot(combined_data %>% filter(Attribute == "Danceability"), aes(x = Total_Revenue, y = Value, color = Artist, group = Artist)) +
  geom_line(size = 1) +  # Line plot
  geom_point(size = 2) +  # Points on the line
  labs(title = "Danceability vs Total Revenue", x = "Total Revenue", y = "Danceability") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Plot for Energy
ggplot(combined_data %>% filter(Attribute == "Energy"), aes(x = Total_Revenue, y = Value, color = Artist, group = Artist)) +
  geom_line(size = 1) +  # Line plot
  geom_point(size = 2) +  # Points on the line
  labs(title = "Energy vs Total Revenue", x = "Total Revenue", y = "Energy") +
  theme_minimal() +
  theme(legend.title = element_blank())




korean_artists <- c("BTS", "EXO", "BLACKPINK", "TWICE", "Stray Kids", "Red Velvet", "NCT", 
                    "SEVENTEEN", "BIGBANG", "SHINee", "(G)I-DLE", "ATEEZ", "TOMORROW X TOGETHER", 
                    "ENHYPEN", "IVE", "LE SSERAFIM", "NewJeans", "TAEYANG", "RM", "Jimin", 
                    "j-hope", "IU", "Girls' Generation", "JAY PARK", "ITZY")


korean_singer_data <- music_data %>%
  filter(Artist %in% korean_artists) %>%
  arrange(Artist, desc(Total_Revenue))  # 

#한국 가수들에 대한 내용용

# Create a combined dataset for all relevant attributes
combined_data <- korean_singer_data %>%
  select(Danceability, Energy, Artist, Total_Revenue, Key, Loudness, Speechiness, Acousticness, Instrumentalness, Liveness, Valence, Tempo, Duration_ms, Views, Likes, Comments, Stream) %>%
  pivot_longer(cols = c(Danceability,Energy, Key, Loudness, Speechiness, Acousticness, Instrumentalness, Liveness, Valence, Tempo, Duration_ms, Views, Likes, Comments, Stream), 
               names_to = "Attribute", values_to = "Value")

# Create a list of attributes to plot
attributes_to_plot <- c("Danceability","Energy", "Key", "Loudness", "Speechiness", "Acousticness", 
                        "Instrumentalness", "Liveness", "Valence", 
                        "Tempo", "Duration_ms", "Views", 
                        "Likes", "Comments", "Stream")

# Loop through each attribute and create plots
for (attribute in attributes_to_plot) {
  p <- ggplot(combined_data %>% filter(Attribute == attribute), 
              aes(x = Total_Revenue, y = Value, color = Artist, group = Artist)) +
    geom_line(size = 1) +  # Line plot
    geom_point(size = 2) +  # Points on the line
    labs(title = paste(attribute, "vs Total Revenue"), x = "Total Revenue", y = attribute) +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  print(p)  # Print each plot
}



korean_artists2 <- c("EXO", "TWICE", "Stray Kids", "Red Velvet", "NCT", 
                    "SEVENTEEN", "BIGBANG", "SHINee", "(G)I-DLE", "ATEEZ", "TOMORROW X TOGETHER", 
                    "ENHYPEN", "IVE", "LE SSERAFIM", "NewJeans", "TAEYANG", "RM", "Jimin", 
                    "j-hope", "IU", "Girls' Generation", "JAY PARK", "ITZY")


korean_singer_data <- music_data %>%
  filter(Artist %in% korean_artists2) %>%
  arrange(Artist, desc(Total_Revenue))%>%  # 
  top_n(30, Total_Revenue)
#한국 가수들에 대한 내용용

# Create a combined dataset for all relevant attributes
combined_data <- korean_singer_data %>%
  select(Danceability, Energy, Artist, Total_Revenue, Key, Loudness, Speechiness, Acousticness, Instrumentalness, Liveness, Valence, Tempo, Duration_ms, Views, Likes, Comments, Stream) %>%
  pivot_longer(cols = c(Danceability,Energy, Key, Loudness, Speechiness, Acousticness, Instrumentalness, Liveness, Valence, Tempo, Duration_ms, Views, Likes, Comments, Stream), 
               names_to = "Attribute", values_to = "Value")

# Create a list of attributes to plot
attributes_to_plot <- c("Danceability","Energy", "Key", "Loudness", "Speechiness", "Acousticness", 
                        "Instrumentalness", "Liveness", "Valence", 
                        "Tempo", "Duration_ms", "Views", 
                        "Likes", "Comments", "Stream")

# Loop through each attribute and create plots
for (attribute in attributes_to_plot) {
  p <- ggplot(combined_data %>% filter(Attribute == attribute), 
              aes(x = Total_Revenue, y = Value, color = Artist, group = Artist)) +
    geom_line(size = 1) +  # Line plot
    geom_point(size = 2) +  # Points on the line
    labs(title = paste(attribute, "vs Total Revenue"), x = "Total Revenue", y = attribute) +
    geom_smooth(method = "lm", se = FALSE, color = "black") + 
    theme_minimal() +
    theme(legend.title = element_blank())
  
  print(p)  # Print each plot
}
