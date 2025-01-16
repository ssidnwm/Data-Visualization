# 필요한 라이브러리 로드
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(httr)
library(jsonlite)

# OpenAI API 키 설정 (실제 키를 사용하기 위해 사용자 입력으로 변경하는 것이 좋음)
#api_key <- "your api key" # 실제 환경에서는 환경변수 또는 사용자 입력 방식으로 처리하는 것을 권장합니다.

# GPT API 호출 함수
call_gpt <- function(prompt,api_key) {
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type("application/json"),
    body = toJSON(list(
      model = "gpt-4o-mini-2024-07-18",  # "gpt-4"로도 변경 가능
      messages = list(
        list(role = "user", content = prompt)
      )
    ), auto_unbox = TRUE)
  )
  
  # API 응답 상태와 내용 확인
  print(response$status_code)  # 상태 코드 출력
  print(content(response, as = "text"))  # 응답 내용 출력
  
  # API 응답 확인
  if (response$status_code == 200) {
    result <- content(response, as = "parsed")
    return(result$choices[[1]]$message$content)
  } else {
    return(paste("Error:", response$status_code))
  }
}

# 음악 데이터를 로드 (Spotify와 YouTube 데이터를 포함한 CSV 파일)
music_data <- read.csv("C:/Users/silkj/Desktop/한동대학교/5학기/데이터 시각화/Data-Visualization/myRVis/Spotify_Youtube.csv")

# YouTube와 Spotify 수익 계산
music_data <- music_data %>%
  mutate(
    YouTube_Revenue = ifelse(Views <= 10000, Views * 0.06, Views * 0.12),
    Spotify_Revenue = Stream * (0.04 / 10),
    Total_Revenue = YouTube_Revenue + Spotify_Revenue
  )

# 한국 아티스트 목록 정의
korean_artists <- c("BTS", "EXO", "BLACKPINK", "TWICE", "Stray Kids", 
                    "Red Velvet", "NCT", "SEVENTEEN", "BIGBANG", 
                    "SHINee", "(G)I-DLE", "ATEEZ", "TOMORROW X TOGETHER", 
                    "ENHYPEN", "IVE", "LE SSERAFIM", "NewJeans", 
                    "TAEYANG", "RM", "Jimin", "j-hope", "IU", 
                    "Girls' Generation", "JAY PARK", "ITZY")

# UI 정의
ui <- fluidPage(
  titlePanel("Artist Revenue Distribution"),
  sidebarLayout(
    sidebarPanel(
      textInput("api_key", "Enter your OpenAI API Key:", value = "", placeholder = "Your OpenAI API Key"),
      selectInput("xaxis", 
                  "Select Feature for X-axis:",
                  choices = names(music_data)[!(names(music_data) %in% c("Artist", "Total_Revenue"))],
                  selected = "Avg_Energy"),
      sliderInput("globalCount", 
                  "This number represents the top percentage of global artists:",
                  min = 1.5, 
                  max = 100, 
                  value = 1.5),
      actionButton("analyze", "Analyze with GPT")
    ),
    mainPanel(
      plotlyOutput("distributionPlot"),
      textOutput("gpt_analysis")  # GPT 분석 결과 출력
    )
  )
)

# 서버 로직 정의
server <- function(input, output) {
  
  # x축 고정 범위 계산
  overall_x_limits <- reactive({
    x_values <- music_data[[input$xaxis]]
    c(min(x_values, na.rm = TRUE) * 1.1, max(x_values, na.rm = TRUE) * 1.1)
  })
  
  # 분포도 생성 및 selected_data 반응형 객체 정의
  selected_data <- reactive({
    global_artist_count <- floor((input$globalCount / 100) * 2000)
    
    global_data <- music_data %>% 
      filter(!(Artist %in% korean_artists)) %>% 
      arrange(desc(Total_Revenue)) %>% 
      slice(1:global_artist_count)
    
    korean_data <- music_data %>% filter(Artist %in% korean_artists)
    
    bind_rows(global_data, korean_data) %>%
      mutate(Artist_Type = ifelse(Artist %in% korean_artists, "Korean Artist", "Global Artist"))
  })
  
  output$distributionPlot <- renderPlotly({
    data <- selected_data()
    
    # 한국 및 글로벌 아티스트의 평균 값 계산
    korean_mean <- data %>% 
      filter(Artist_Type == "Korean Artist") %>% 
      summarise(mean_value = mean(.data[[input$xaxis]], na.rm = TRUE)) %>% 
      pull(mean_value)
    
    global_mean <- data %>% 
      filter(Artist_Type == "Global Artist") %>% 
      summarise(mean_value = mean(.data[[input$xaxis]], na.rm = TRUE)) %>% 
      pull(mean_value)
    
    # 분포도 그리기
    p <- ggplot() +
      geom_point(data = data %>% filter(Artist_Type == "Global Artist"), 
                 aes(x = .data[[input$xaxis]], y = Total_Revenue, 
                     color = Artist_Type, 
                     text = paste('Artist:', Artist, '<br>Total Revenue:', Total_Revenue, '<br>', input$xaxis, ':', .data[[input$xaxis]])), 
                 show.legend = TRUE) +
      geom_point(data = data %>% filter(Artist_Type == "Korean Artist"), 
                 aes(x = .data[[input$xaxis]], y = Total_Revenue, 
                     color = Artist_Type, 
                     text = paste('Artist:', Artist, '<br>Total Revenue:', Total_Revenue, '<br>', input$xaxis, ':', .data[[input$xaxis]])), 
                 show.legend = TRUE) +
      scale_color_manual(values = c("Korean Artist" = "red", "Global Artist" = "grey")) +
      geom_vline(xintercept = korean_mean, color = "orange", linetype = "dashed", size = 1) +
      geom_vline(xintercept = global_mean, color = "black", linetype = "dashed", size = 1) +
      labs(title = "Distribution of Total Revenue by Selected Feature",
           x = input$xaxis,
           y = "Total Revenue") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # GPT 분석 요청
  observeEvent(input$analyze, {
    req(input$api_key) 
    data <- selected_data()
    
    if(nrow(data) > 0) {
      avg_total_revenue <- round(mean(data$Total_Revenue, na.rm = TRUE), 2)
      avg_selected_feature <- round(mean(data[[input$xaxis]], na.rm = TRUE), 2)
      
      summary_text <- paste(
        "Overall Data Summary:\n",
        "Average Total Revenue:", avg_total_revenue, "\n",
        "Average", input$xaxis, ":", avg_selected_feature, "\n",
        "This data includes both Korean and Global artists."
      )
      
      prompt <- paste("Provide insights based on the following data summary:Answers must be provided in Korean. Answers should be provided in the following format
1. explain what the data you provided is.
2. provide insights you can find from the data.
3. provide additional comments based on your analysis of the data.\n", summary_text)
      gpt_answer <- call_gpt(prompt,input$api_key)
      
      output$gpt_analysis <- renderText({
        # GPT 응답과 상태 코드를 같이 출력
        paste("Response:", gpt_answer)
      })
    } else {
      output$gpt_analysis <- renderText({
        "No data available for analysis."
      })
    }
  })
}

# Shiny 앱 실행
shinyApp(ui = ui, server = server)
