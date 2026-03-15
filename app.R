library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(readr)

# Read data
features <- read_csv("data/raw/ai_productivity_features.csv", show_col_types = FALSE)
targets <- read_csv("data/raw/ai_productivity_targets.csv", show_col_types = FALSE)

df <- features %>%
  left_join(targets, by = "Employee_ID") %>%
  mutate(
    ai_band = case_when(
      ai_tool_usage_hours_per_week <= quantile(ai_tool_usage_hours_per_week, 1/3, na.rm = TRUE) ~ "Low",
      ai_tool_usage_hours_per_week <= quantile(ai_tool_usage_hours_per_week, 2/3, na.rm = TRUE) ~ "Moderate",
      TRUE ~ "High"
    )
  )

job_role_choices <- sort(unique(df$job_role))

ui <- page_fluid(
  titlePanel("AI Usage & Burnout Checkup (R Version)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "job_role",
        "Job Role",
        choices = c("All", job_role_choices),
        selected = "All"
      )
    ),
    
    mainPanel(
      layout_columns(
        card(
          card_header("Average Burnout Risk Score"),
          textOutput("avg_burnout")
        ),
        col_widths = c(12)
      ),
      br(),
      card(
        card_header("Burnout Risk by Job Role"),
        plotOutput("burnout_plot")
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_df <- reactive({
    d <- df
    
    if (input$job_role != "All") {
      d <- d %>% filter(job_role == input$job_role)
    }
    
    d
  })
  
  output$avg_burnout <- renderText({
    d <- filtered_df()
    
    if (nrow(d) == 0) {
      return("No data")
    }
    
    round(mean(d$burnout_risk_score, na.rm = TRUE), 2)
  })
  
  output$burnout_plot <- renderPlot({
    d <- filtered_df()
    
    req(nrow(d) > 0)
    
    d %>%
      group_by(job_role) %>%
      summarise(avg_burnout = mean(burnout_risk_score, na.rm = TRUE)) %>%
      ggplot(aes(x = reorder(job_role, avg_burnout), y = avg_burnout)) +
      geom_col() +
      coord_flip() +
      labs(
        x = "Job Role",
        y = "Average Burnout Risk Score"
      ) +
      theme_minimal()
  })
}

shinyApp(ui, server)