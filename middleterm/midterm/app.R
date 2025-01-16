library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(shiny)
library(sf)
library(readxl)
library(rsconnect)
data<- read.csv("시도별_의료보장_적용인구_현황_20241018111611.csv", fileEncoding = "CP949")
doc<-read.csv("시도별_의료인력_현황_20241018111515.csv", fileEncoding = "CP949")
skin <- read_excel("기관수현황_지역별의원표시과목별_기관수_2022년4분기.xlsx")
pla <- read_excel("기관수현황_지역별의원표시과목별_기관수_2022년4분기 (1).xlsx")
colnames(skin) <- skin[2, ]
skin <- skin[-c(1, 2), ]
head(skin)
colnames(pla) <- pla[2, ]
pla <- pla[-c(1, 2), ]
head(pla)

pla <- pla%>%
  filter(시군구 == '계')
skin <- skin%>%
  filter(시군구 == '계')
combined_data <- merge(pla, skin, by = c("시도", "기준분기"))

# 필요한 열만 선택하여 새로운 데이터프레임 생성
result <- combined_data %>%
  select(시도, 피부과, 성형외과)

# '피부과'와 '성형외과' 데이터를 숫자로 변환
result$피부과 <- as.numeric(result$피부과)
result$성형외과 <- as.numeric(result$성형외과)

# 데이터 확인
head(result)
result_clean <- result %>%
  mutate(시도 = ifelse(is.na(시도), "강원도", 시도))

head(doc)
# 'doc' 데이터 처리
new_colnames <- paste(doc[1, ], doc[2, ], sep = "-")  # 두 번째, 세 번째 행 결합
new_colnames[1] <- "시도별"  # 첫 번째 열 이름 설정

# 첫 번째와 두 번째 행 제거 후 열 이름 설정
doc <- doc[-c(1, 2), ]
colnames(doc) <- new_colnames





# 데이터 확인
head(doc)
head(data)
# 'data' 데이터 처리
new_colnames_data <- as.character(data[1, ])  # 두 번째 행을 열 이름으로 사용
colnames(data) <- new_colnames_data           # 열 이름 설정

# 첫 번째와 세 번째 행 제거
data <- data[-1, ]  # 첫 번째 행 제거
data <- data[-2, ]  # 세 번째 행 제거
colnames(data)[1] <- "시도별"
# 데이터 확인
head(data)
print(colnames(doc))

profession_mapping <- list(
  "의사" = "의사-소계",
  "치과의사" = "치과의사-소계",
  "한의사" = "한의사-소계",
  "간호사" = "간호사-소계",
  "약사" = "약사-소계",
  "물리치료사" = "물리치료사-소계",
  "작업치료사" = "작업치료사-소계",
  "사회복지사" = "사회복지사-소계"
)
data <- data %>%
  select(시도별, `의료보장 적용인구 (명)`)

# '의료보장 적용인구'를 숫자로 변환
data$`의료보장 적용인구 (명)` <- as.numeric(gsub(",", "", data$`의료보장 적용인구 (명)`))
# 의료 인력과 의료보장 적용인구 데이터 병합
merged_data <- merge(doc, data, by = "시도별")

# 비율 계산: 의사 인원을 의료보장 적용인구로 나눈 값 계산
merged_data$의사_비율 <- ifelse(is.na(merged_data$`의사-소계`) | merged_data$`의사-소계` == 0 | 
                              is.na(merged_data$`의료보장 적용인구 (명)`) | merged_data$`의료보장 적용인구 (명)` == 0, 
                            0, 
                            as.numeric(merged_data$`의사-소계`) / merged_data$`의료보장 적용인구 (명)`)


# 상급종합병원 수 데이터프레임 생성
hospital_data <- data.frame(
  시도별 = c("서울특별시", "부산광역시", "대구광역시", "인천광역시", "광주광역시", "대전광역시", 
          "울산광역시", "경기도", "강원특별자치도", "충청북도", "충청남도", "전라북도", 
          "전라남도", "경상북도", "경상남도", "제주특별자치도"),
  상급종합병원수 = c(14, 4, 5, 4, 2, 1, 1, 5, 2, 1, 2, 2, 1, 0, 3, 0)
)
# 대한민국 Shapefile 데이터 불러오기 및 병합
korea <- read_sf("maps/ctp_rvn.shp")
korea <- korea %>% st_set_crs(5179) %>%
  mutate(CTP_KOR_NM = iconv(CTP_KOR_NM, from = "EUC-KR", to = 'UTF-8'))

# 병원 수 데이터와 지도 데이터 병합
merged_hospital_data <- korea %>% left_join(hospital_data, by = c("CTP_KOR_NM" = "시도별"))
merged_hospital_data$상급종합병원수[is.na(merged_hospital_data$상급종합병원수)] <- 0


merged_beauty_data <- korea %>% left_join(result_clean, by = c("CTP_KOR_NM" = "시도"))

merged_beauty_data <- merged_beauty_data %>% 
  mutate(centroid = st_centroid(geometry)) %>% 
  mutate(lon = st_coordinates(centroid)[,1], lat = st_coordinates(centroid)[,2])
# Shiny UI 정의
ui <- fluidPage(
  titlePanel("서울의 의료 수준 및 미용 지표 시각화"),
  tabsetPanel(
    # 첫 번째 탭: 서울의 의료 수준
    tabPanel("서울의 의료 수준", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("data_type", "데이터 선택:", choices = c("의료 인력", "의료보장 적용인구", "의사/의료보장 비율")),
                 selectInput("profession", "직종 선택:", choices = names(profession_mapping))  # 사용자에게 보여줄 직종 선택
               ),
               mainPanel(
                 plotOutput("barPlot"),
                 textOutput("graphComment")
               )
             )
    ),
    
    # 두 번째 탭: 상급 종합병원 수 비교
    tabPanel("상급 종합병원 수 비교",
             sidebarLayout(
               sidebarPanel(
                 h3("상급 종합병원 수 비교"),
                 tags$p("더 많은 정보를 원하시면 ", 
                        tags$a(href = "https://www.mohw.go.kr/board.es?mid=a10503000000&bid=0027&list_no=1479568&act=view", 
                               "보건복지부 상급 종합병원 정보"), 
                        "를 참조하세요.")  # 링크 추가
               ),
               mainPanel(
                 plotOutput("hospitalPlot"),
                 tableOutput("hospitalTable")
               )
             )
    ),
    
    # 세 번째 탭: 미용 관련 지표
    tabPanel("서울의 미용 관련 지표", 
             sidebarLayout(
               sidebarPanel(
                 h3("서울의 미용 관련 지표"),
                 selectInput("metric", "지표 선택:", choices = c("피부과", "성형외과"))  # 사용자에게 보여줄 지표 선택
               ),
               mainPanel(
                 plotOutput("beautyMap"),  # 지도를 표시할 공간
                 tableOutput("beautyTable")  # 테이블 표시
               )
             )
    )
  )
)
ordered_regions <- c("서울특별시", "경기도", "강원특별자치도", "부산광역시", "대구광역시", "인천광역시", "광주광역시", 
                     "대전광역시", "울산광역시", "세종특별자치시", "경상북도", "경상남도", "전라북도", "전라남도", 
                     "제주특별자치도", "충청북도", "충청남도")
# 서버 정의
server <- function(input, output) {
  # 첫 번째 탭의 그래프
  output$barPlot <- renderPlot({
    if (input$data_type == "의료 인력") {
      selected_profession <- profession_mapping[[input$profession]]
      filtered_doc <- doc %>% filter(시도별 != "계")
      filtered_doc$시도별 <- factor(filtered_doc$시도별, levels = ordered_regions)
      filtered_doc[[selected_profession]] <- as.numeric(filtered_doc[[selected_profession]])
      ggplot(filtered_doc, aes(x = 시도별, y = !!sym(selected_profession))) +
        geom_bar(stat = "identity", fill = "blue", na.rm = TRUE) +
        labs(title = paste(input$profession, "수"), y = "인원 수", x = "지역") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$data_type == "의료보장 적용인구") {
      filtered_data <- data %>% filter(시도별 != "계")
      filtered_data$시도별 <- factor(filtered_data$시도별, levels = ordered_regions)
      ggplot(filtered_data, aes(x = 시도별, y = `의료보장 적용인구 (명)`)) +
        geom_col(fill = "blue") +
        labs(title = "의료보장 적용인구", y = "인원 수", x = "지역") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$data_type == "의사/의료보장 비율" || input$data_type == "직종/의료보장 비율") {
      selected_profession <- profession_mapping[[input$profession]]
      
      # 직종별 비율 계산
      merged_data <- merged_data %>%
        mutate(직종_비율 = ifelse(
          is.na(merged_data[[selected_profession]]) | merged_data[[selected_profession]] == 0 | 
            is.na(merged_data$`의료보장 적용인구 (명)`) | merged_data$`의료보장 적용인구 (명)` == 0, 
          0, 
          as.numeric(merged_data[[selected_profession]]) / merged_data$`의료보장 적용인구 (명)`))
      
      # 직종/의료보장 적용인구 비율 그래프
      filtered_merged <- merged_data %>% filter(시도별 != "계")
      filtered_merged$시도별 <- factor(filtered_merged$시도별, levels = ordered_regions)
      ggplot(filtered_merged, aes(x = 시도별, y = 직종_비율)) +
        geom_col(fill = "blue") +
        labs(title = paste(input$profession, "/의료보장 적용인구 비율"), y = "비율", x = "지역") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # 첫 번째 탭의 설명
  output$graphComment <- renderText({
    if (input$data_type == "의료 인력") {
      paste("이 그래프는 선택한 직종에 대해 각 지역별로 의료 인력이 얼마나 분포되어 있는지를 보여줍니다. ",
            "의료 인력의 절대적인 숫자가 클수록 해당 지역에서 활동하는 의료 종사자가 많이 분포해 있으며 이는 곧 사람들이 받을 수 있는 의료 혜택의 가짓수나 퀄리티가 높고 접근성이 뛰어남을 의미합니다.")
    } else if (input$data_type == "의료보장 적용인구") {
      paste("이 그래프는 각 지역별로 의료보장 적용 인구수를 보여줍니다. ",
            "이 값은 해당 지역에서 의료보장을 받고 있는 인구수로, 지역별 인구 규모를 파악할 수 있는 중요한 지표입니다. 단순한 인구 분포보다 더욱 의료 데이터에 정확한 값을 확인할 수 있습니다.")
    } else if (input$data_type == "의사/의료보장 비율") {
      paste("이 그래프는 각 지역별로 의사 인력이 전체 의료보장 적용 인구 대비 어느 정도 비율을 차지하고 있는지를 보여줍니다. ",
            "의사 비율이 높을수록 해당 지역의 의료서비스 접근성이 상대적으로 높으며, 한 명의 의사가 더 많은 서비스를 제공할 수 있습니다.")
    }
  })
  
  # 상급 종합병원 수 시각화
  output$hospitalPlot <- renderPlot({
    ggplot(merged_hospital_data) +
      geom_sf(aes(fill = 상급종합병원수), color = "black") +  # 상급종합병원 수를 색상에 매핑
      scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey90", name = "상급 종합병원 수") +
      labs(title = "한국 지역별 상급 종합병원 수", 
           subtitle = "출처: 보건복지부 데이터") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 12),
        legend.position = "bottom"
      )
  })
  # 병원 수 테이블 출력
  output$hospitalTable <- renderTable({
    hospital_data
  })
  # 미용 관련 지표 시각화
  output$beautyMap <- renderPlot({
    selected_metric <- input$metric  # 선택된 지표 (피부과 or 성형외과)
    
    ggplot(merged_beauty_data) +
      geom_sf(aes(fill = !!sym(selected_metric)), color = "black") +  # 선택된 지표를 매핑
      scale_fill_gradient(low = "lightpink", high = "red", na.value = "grey90", name = selected_metric) +  # 색상 설정
      geom_text(aes(x = lon, y = lat, label = !!sym(selected_metric)), size = 3, color = "black") +  # 각 지역의 중심 좌표에 숫자 추가
      labs(title = paste("한국 지역별", selected_metric, "수"), subtitle = "출처: 데이터") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 12),
        legend.position = "bottom"
      )
  })
  
  # 미용 관련 지표 테이블 출력
  output$beautyTable <- renderTable({
    result_clean
  })
  # 세 번째 탭의 설명
  output$beautyComment <- renderText({
    paste("이 그래프는 서울의 미용 관련 지표를 선택한 값에 따라 시각화한 결과입니다. 피부과와 성형외과 등 미용과 관련된 의료시설의 수를 판단하여 지역별 분포를 확인할 수 있습니다.")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
