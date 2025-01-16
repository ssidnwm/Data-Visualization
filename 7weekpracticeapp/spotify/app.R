
library(shiny)
library(ggplot2)
library(dplyr)
library(httr)
library(jsonlite)


music_data <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/Spotify_Youtube.csv")
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



korean_artists <- c("BTS", "EXO", "BLACKPINK", "TWICE", "Stray Kids", "Red Velvet", "NCT", 
                    "SEVENTEEN", "BIGBANG", "SHINee", "(G)I-DLE", "ATEEZ", "TOMORROW X TOGETHER", 
                    "ENHYPEN", "IVE", "LE SSERAFIM", "NewJeans", "TAEYANG", "RM", "Jimin", 
                    "j-hope", "IU", "Girls' Generation", "JAY PARK", "ITZY")
K_data <- music_data%>%
  filter(Artist %in% korean_artists)
avg_total_revenue <- mean(K_data$Total_Revenue, na.rm = TRUE)


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Comparison of Artists with Higher Total Revenue"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("yvar", "Select Variable for Y-axis:",
                  choices = names(music_data)[sapply(music_data, is.numeric)],
                  selected = "Views")
    ),
    
    mainPanel(
      plotOutput("revenuePlot")
    )
  )
)


server <- function(input, output) {
  
  # 그래프 그리기
  output$revenuePlot <- renderPlot({
    
    # 500,000,000 이하의 상위 30명의 아티스트(한국 아티스트 제외)
    non_korean_top30 <- music_data %>%
      group_by(Artist)%>%
      filter(Total_Revenue < 500000000 & !(Artist %in% korean_artists)) %>%
      arrange(desc(Total_Revenue)) %>%
      slice_head(n = 30)
    
    # 한국 아티스트 데이터
    korean_data <- music_data %>%
      filter(Artist %in% korean_artists)
    
    # 두 데이터를 결합
    combined_data <- bind_rows(non_korean_top30, korean_data) %>%
      mutate(color_group = ifelse(Artist %in% korean_artists, "Korean Artist", "Other Artist"))
    
    if(nrow(combined_data) == 0){
      return(NULL)  # 데이터가 없을 경우 그래프를 그리지 않음
    }
    
    # Total_Revenue와 선택된 변수(yvar) 비교 그래프 생성
    ggplot(combined_data, aes(y = Total_Revenue, x = .data[[input$yvar]], color = color_group)) +
      geom_point() +
      scale_color_manual(values = c("Korean Artist" = "blue", "Other Artist" = "gray")) +  # 색상 설정
      
      labs(title = paste("Comparison of Total Revenue vs", input$yvar, "for Top 30 Non-Korean Artists and Korean Artists"),
           y = "Total Revenue", x = input$yvar) +
      theme_minimal() +
      theme(legend.title = element_blank())  # 범례 제목 제거
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
