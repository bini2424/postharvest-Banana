library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(tidyr)
library(readr)

# Create the dataframe from your provided data
data <- data.frame(
  Day = c(4,4,4,4,4,4,4,4,4,4,6,6,6,6,6,6,6,6,6,6,8,8,8,8,8,8,8,8,8,8,10,10,10,10,10,10,10,10,10,10),
  Type = c("Trad","Trad","Trad","Trad","Trad","Cont","Cont","Cont","Cont","Cont",
           "Trad","Trad","Trad","Trad","Trad","Cont","Cont","Cont","Cont","Cont",
           "Trad","Trad","Trad","Trad","Trad","Cont","Cont","Cont","Cont","Cont",
           "Trad","Trad","Trad","Trad","Trad","Cont","Cont","Cont","Cont","Cont"),
  Treatment = c("Control(T)","Gunny bag","Tomato","Straw","Ethephon",
                "Control(Co)","LDPE 24","LDPE 48","HDPE 24","HDPE 48",
                "Control(T)","Gunny bag","Tomato","Straw","Ethephon",
                "Control(Co)","LDPE 24","LDPE 48","HDPE 24","HDPE 48",
                "Control(T)","Gunny bag","Tomato","Straw","Ethephon",
                "Control(Co)","LDPE 24","LDPE 48","HDPE 24","HDPE 48",
                "Control(T)","Gunny bag","Tomato","Straw","Ethephon",
                "Control(Co)","LDPE 24","LDPE 48","HDPE 24","HDPE 48"),
  PLW = c(3.48,2.86,7.48,5.44,7.84,6.17,1.00,2.46,2.95,5.45,
          5.26,3.18,8.57,6.33,9.24,5.62,1.85,1.81,3.87,3.03,
          5.62,4.43,9.68,7.06,10.06,6.61,1.78,2.88,4.21,4.78,
          6.43,5.42,9.91,7.77,11.50,6.83,2.17,3.09,4.84,4.44),
  PP = c(1.83,2.07,1.34,2.05,1.42,1.52,1.36,1.31,1.25,1.25,
         2.36,2.32,1.24,2.08,1.78,1.74,1.28,1.47,1.43,1.42,
         1.96,1.92,1.78,2.02,2.24,2.04,1.46,1.50,1.63,1.58,
         1.65,2.03,2.52,1.64,2.62,2.51,1.52,2.15,2.30,2.27),
  Firmness = c(7.00,6.60,4.85,8.03,1.01,1.67,1.73,1.70,1.77,1.74,
               6.70,7.08,1.70,8.73,0.81,1.58,1.63,1.55,1.57,1.59,
               6.73,6.75,2.73,4.60,0.60,1.38,1.34,1.33,1.41,1.40,
               2.25,1.46,0.66,1.32,0.82,0.80,0.70,0.74,0.89,0.87),
  TSS = c(5.62,5.25,13.13,6.37,22.10,22.50,22.00,22.13,22.50,22.50,
          6.38,5.25,19.87,5.25,22.87,24.00,23.63,23.63,25.12,25.13,
          6.00,4.87,22.50,5.62,22.87,23.63,22.50,22.50,23.25,23.25,
          13.95,15.45,22.95,10.95,22.95,22.65,21.50,21.45,21.67,21.67),
  TA = c(1.60,0.70,1.00,0.70,0.70,0.45,0.40,0.45,0.40,0.45,
         0.80,0.65,0.70,0.50,0.60,0.40,0.35,0.35,0.40,0.35,
         0.60,0.50,0.50,0.40,0.40,0.40,0.35,0.30,0.35,0.35,
         0.65,0.65,0.47,0.25,0.47,0.30,0.30,0.30,0.30,0.30),
  TSS_TA = c(4.78,7.58,13.65,9.16,32.16,51.56,64.68,50.93,65.62,51.56,
             9.16,8.08,42.88,14.92,44.74,69.06,75.00,73.12,71.25,77.81,
             11.43,10.25,48.94,16.47,56.86,67.74,69.91,83.90,81.10,71.78,
             21.46,23.86,49.05,45.90,49.05,84.75,81.25,80.62,80.81,81.00),
  PH = c(5.30,5.19,4.36,5.18,4.18,4.23,4.34,4.31,4.31,4.25,
         5.11,4.97,4.24,5.04,4.40,4.49,4.57,4.58,4.60,4.64,
         4.93,4.64,4.44,4.75,4.64,5.00,5.05,5.15,5.19,5.17,
         4.75,4.23,4.58,4.25,4.95,5.72,6.17,5.88,6.07,5.85)
)
data <- data %>%
  mutate(Type_Trt = paste(Type, Treatment, sep = " - "))
header <- dashboardHeader(title = "Postharvest Banana Ripening Study")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Temporal Dynamics", tabName = "temporal", icon = icon("chart-line")),
    menuItem("Treatment Comparison", tabName = "comparison", icon = icon("balance-scale")),
    menuItem("Quality Trade-offs", tabName = "tradeoffs", icon = icon("exchange-alt"))
  ),
  selectizeInput("traits", "Select Quality Traits:",
                 choices = c("PLW", "PP", "Firmness", "TSS", "TA", "TSS_TA", "PH"),
                 selected = c("PLW", "Firmness", "TSS"),
                 multiple = TRUE),
  sliderInput("days", "Select Days:",
              min = 4, max = 10, value = c(4, 10), step = 2),
  selectInput("treatments", "Select Treatments:",
              choices = unique(data$Type_Trt),
              selected = c("Trad - Control(T)", "Trad - Ethephon", "Cont - LDPE 24", "Cont - HDPE 48"),
              multiple = TRUE)
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "temporal",
            fluidRow(
              box(title = "Temporal Evolution of Quality Traits", 
                  width = 12, plotlyOutput("temporal_plot"))
            )),
    tabItem(tabName = "comparison",
            fluidRow(
              box(title = "Radar Chart Comparison", width = 6, plotlyOutput("radar_plot")),
              box(title = "Trade-off Analysis", width = 6, plotlyOutput("tradeoff_plot"))
            )),
    tabItem(tabName = "tradeoffs",
            fluidRow(
              box(title = "Trait Relationships", width = 12, plotlyOutput("corr_plot"))
            ))
  )
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)
# Temporal Dynamics Tab
tabItemtabName = "temporal"
fluidRow(
  box(title = "Temporal Evolution of Quality Traits", 
      width = 12, plotlyOutput("temporal_plot"))
)
tabItemtabName = "comparison"
fluidRow(
  box(title = "Radar Chart Comparison", 
      width = 6, plotlyOutput("radar_plot")),
  box(title = "Trade-off Analysis", 
      width = 6, plotlyOutput("tradeoff_plot"))
)
tabItemtabName = "tradeoffs"
fluidRow
box(title = "Trait Relationships", 
    width = 12, plotlyOutput("corr_plot"))

server <- function(input, output) {
  filtered_data <- reactive({
    data %>%
      filter(Day >= input$days[1], Day <= input$days[2],
             Type_Trt %in% input$treatments)
  })
  output$temporal_plot <- renderPlotly({
    df <- filtered_data() %>%
      pivot_longer(cols = c(PLW, PP, Firmness, TSS, TA, TSS_TA, PH),
                   names_to = "Trait", values_to = "Value")
    df <- df %>% filter(Trait %in% input$traits)
    p <- ggplot(df, aes(x = Day, y = Value, color = Type_Trt, group = interaction(Type_Trt, Trait))) +
      geom_line() +
      geom_point(aes(text = paste("Treatment:", Type_Trt,
                                  "<br>Day:", Day,
                                  "<br>Trait:", Trait,
                                  "<br>Value:", round(Value, 2)))) +
      facet_wrap(~Trait, scales = "free_y", ncol = 2) +
      labs(x = "Days After Harvest", y = "Trait Value", color = "Treatment") +
      theme_bw() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })    
  output$radar_plot <- renderPlotly({
    df <- filtered_data() %>%
      group_by(Type_Trt) %>%
      summarize(across(c(PLW, PP, Firmness, TSS, TA, TSS_TA, PH), mean, na.rm = TRUE)) %>%
      pivot_longer(-Type_Trt, names_to = "Trait", values_to = "Mean")             
    df <- df %>% filter(Trait %in% input$traits)
    
    plot_ly(df, type = 'scatterpolar', mode = 'lines+markers',
            r = ~Mean, theta = ~Trait, color = ~Type_Trt,
            fill = 'toself', opacity = 0.7,
            hoverinfo = 'text',
            text = ~paste("Treatment:", Type_Trt,
                          "<br>Trait:", Trait,
                          "<br>Mean:", round(Mean, 2))) %>%
      layout(polar = list(radialaxis = list(visible = TRUE)),
             showlegend = TRUE)
  })    
  output$corr_plot <- renderPlotly({
    req(length(input$traits) >= 2)  # Need at least 2 traits
    
    df <- filtered_data()
    
    plot_ly(df, x = ~get(input$traits[1]), y = ~get(input$traits[2]),
            color = ~Type_Trt, size = ~Day,
            type = 'scatter', mode = 'markers',
            text = ~paste("Treatment:", Type_Trt,
                          "<br>Day:", Day,
                          "<br>", input$traits[1], ":", round(get(input$traits[1]), 2),
                          "<br>", input$traits[2], ":", round(get(input$traits[2]), 2)),
            hoverinfo = 'text') %>%
      layout(xaxis = list(title = input$traits[1]),
             yaxis = list(title = input$traits[2]))
  })
  output$tradeoff_plot <- renderPlotly({
    df <- filtered_data() %>%
      group_by(Type, Treatment) %>%
      summarize(PLW = mean(PLW),
                Firmness = mean(Firmness),
                TSS = mean(TSS),
                TA = mean(TA),
                TSS_TA = mean(TSS_TA)) %>%
      ungroup()
    
    plot_ly(df, x = ~Firmness, y = ~TSS_TA,
            color = ~Type, size = ~PLW,
            text = ~paste("Treatment:", Treatment,
                          "<br>Type:", Type,
                          "<br>Firmness:", round(Firmness, 2),
                          "<br>TSS/TA:", round(TSS_TA, 2),
                          "<br>PLW:", round(PLW, 2)),
            hoverinfo = 'text', type = 'scatter', mode = 'markers') %>%
      layout(
        title = "Structural vs Biochemical Quality Trade-off",
        xaxis = list(title = "Firmness (Structural Integrity)"),
        yaxis = list(title = "TSS/TA Ratio (Biochemical Quality)"),
        annotations = list(
          x = 0.5, y = 1.05,
          text = "Size = Physiological Weight Loss (PLW)",
          xref = "paper", yref = "paper",
          showarrow = FALSE
        )
      )
  })
}
shinyApp(ui, server)
