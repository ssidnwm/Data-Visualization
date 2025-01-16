library(readr)
NBA_PerPosition_Stats_PG <- read_csv("NBA_PerPosition Stats - PG.csv")
# Regular expression to match the player's name and team abbreviation
pattern <- "(.+?)([A-Z]{2,3})$"

# Extract the player's name
NBA_PerPosition_Stats_PG$Player <- sub(pattern, "\\1", NBA_PerPosition_Stats_PG$Name)

# Extract the team abbreviation
NBA_PerPosition_Stats_PG$Team <- sub(pattern, "\\2", NBA_PerPosition_Stats_PG$Name)

# Remove any trailing whitespace from the player's name
NBA_PerPosition_Stats_PG$Player <- trimws(NBA_PerPosition_Stats_PG$Player)

# Display the updated data frame
print(NBA_PerPosition_Stats_PG)

# Replace 'Name' with 'Player' and rearrange columns
NBA_PerPosition_Stats_PG <- NBA_PerPosition_Stats_PG[, c("RK", "Player", "Team", "POS", "GP", "MIN", 
                                                         "PTS", "FGM", "FGA", "FG%", "3PM", "3PA", "3P%", 
                                                         "FTM", "FTA", "FT%", "REB", "AST", "STL", "BLK", 
                                                         "TO", "DD2", "TD3")]

rsconnect::setAccountInfo(name='calebhan',
                          token='6046023895F7AC0DD5AAFD8D4D3B8760',
                          secret='XBvS7Zo809I2gY84c+x/N4suhGPVe+Mb1bxqyd/m')

# Display the updated data frame
colnames(NBA_PerPosition_Stats_PG)


nba_teams_colors <- data.frame(
  Team = c(
    "Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Hornets",
    "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks", "Denver Nuggets",
    "Detroit Pistons", "Golden State Warriors", "Houston Rockets", "Indiana Pacers",
    "Los Angeles Clippers", "Los Angeles Lakers", "Memphis Grizzlies", "Miami Heat",
    "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Pelicans", "New York Knicks",
    "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns",
    "Portland Trail Blazers", "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors",
    "Utah Jazz", "Washington Wizards"
  ),
  PrimaryColor = c(
    "#E03A3E", "#007A33", "#000000", "#1D1160",
    "#CE1141", "#6F263D", "#00538C", "#0E2240",
    "#C8102E", "#1D428A", "#CE1141", "#002D62",
    "#C8102E", "#552583", "#5D76A9", "#98002E",
    "#00471B", "#0C2340", "#0C2340", "#006BB6",
    "#007AC1", "#0077C0", "#006BB6", "#1D1160",
    "#E03A3E", "#5A2D81", "#000000", "#CE1141",
    "#002B5C", "#002B5C"
  ),
  stringsAsFactors = FALSE
)

team_abbreviations <- data.frame(
  Abbreviation = c("ATL", "BOS", "BKN", "CHA", "CHI", "CLE", "DAL", "DEN", "DET",
                   "GS", "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN",
                   "NO", "NY", "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SA",
                   "TOR", "UTA", "WSH"),
  FullName = c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Hornets",
               "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks", "Denver Nuggets",
               "Detroit Pistons", "Golden State Warriors", "Houston Rockets", "Indiana Pacers",
               "Los Angeles Clippers", "Los Angeles Lakers", "Memphis Grizzlies", "Miami Heat",
               "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Pelicans", "New York Knicks",
               "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns",
               "Portland Trail Blazers", "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors",
               "Utah Jazz", "Washington Wizards"),
  stringsAsFactors = FALSE
)

# Merge the team abbreviations with team colors
team_colors <- merge(team_abbreviations, nba_teams_colors, by.x = "FullName", by.y = "Team")


# Merge player stats with team colors
NBA_PerPosition_Stats_PG <- merge(NBA_PerPosition_Stats_PG, team_colors, by.x = "Team", by.y = "Abbreviation", all.x = TRUE)

###
library(shiny)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("NBA Player Comparison by Position"),
  sidebarLayout(
    sidebarPanel(
      selectInput("position", "Select Position:", choices = unique(NBA_PerPosition_Stats_PG$POS)),
      selectInput("statistic", "Select Statistic:", choices = c(
        "Points" = "PTS",
        "Minutes Played" = "MIN",
        "Assists" = "AST",
        "Rebounds" = "REB",
        "Field Goal Percentage" = "FG%",
        "3-Pointers Made" = "3PM",
        "3-Point Percentage" = "3P%",
        "Free Throw Percentage" = "FT%",
        "Free Throw Attempts" = "FTA",
        "Steals" = "STL",
        "Blocks" = "BLK",
        "Turnovers" = "TO"
      )),
      numericInput("top_n", "Number of Top Players to Display:", value = 5, min = 1)
    ),
    mainPanel(
      plotOutput("playerPlot"),
      tableOutput("playerTable")
    )
  )
)

# Define Server
server <- function(input, output) {
  filteredData <- reactive({
    NBA_PerPosition_Stats_PG %>%
      filter(POS == input$position) %>%
      arrange(desc(.data[[input$statistic]])) %>%
      head(input$top_n)
  })
  
  output$playerPlot <- renderPlot({
    data <- filteredData()
    ggplot(data, aes(
      x = reorder(Player, .data[[input$statistic]]),
      y = .data[[input$statistic]],
      fill = PrimaryColor  # Use the team primary color
    )) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_identity() +  # Use actual colors from the PrimaryColor column
      geom_text(
        aes(
          label = paste(Team),  # Add team abbreviation as label
          color = PrimaryColor
        ),
        hjust = -0.1,           # Adjust horizontal alignment for label visibility
        size = 4                # Adjust font size
      ) +
      labs(
        x = "Player",
        y = names(which(c(
          "Points" = "PTS",
          "Minutes Played" = "MIN",
          "Assists" = "AST",
          "Rebounds" = "REB",
          "Field Goal Percentage" = "FG%",
          "3-Pointers Made" = "3PM",
          "3-Point Percentage" = "3P%",
          "Free Throw Percentage" = "FT%",
          "Free Throw Attempts" = "FTA",
          "Steals" = "STL",
          "Blocks" = "BLK",
          "Turnovers" = "TO"
        ) == input$statistic))[1],
        title = paste("Top", input$top_n, input$position, "Players by", names(which(c(
          "Points" = "PTS",
          "Minutes Played" = "MIN",
          "Assists" = "AST",
          "Rebounds" = "REB",
          "Field Goal Percentage" = "FG%",
          "3-Pointers Made" = "3PM",
          "3-Point Percentage" = "3P%",
          "Free Throw Percentage" = "FT%",
          "Free Throw Attempts" = "FTA",
          "Steals" = "STL",
          "Blocks" = "BLK",
          "Turnovers" = "TO"
        ) == input$statistic))[1])
      ) +
      theme_minimal() +
      theme(
        legend.position = "none"  # Hide legend since team colors are self-explanatory
      )
  })
  
  output$playerTable <- renderTable({
    data <- filteredData()
    data %>%
      select(Player, Team, POS, GP, MIN, PTS, AST, REB, `FG%`, `3PM`, `3P%`, `FT%`, STL, BLK, TO)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
