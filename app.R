library(shiny)
library(dplyr)
library(ggplot2)

df <- read.csv('UAH-lower-troposphere-long.csv.bz2', sep = '\t')
df$date <- ISOdate(year = df$year,
                   month = df$month,
                   day = 1)
df$decade <- df$year - df$year %% 10

regions <- unique(df$region)
color_palettes <- c("Standard" = "Set1",
                    "Set 2" = "Set2")
time_levels <- c("month" = "month",
                 "year" = "year",
                 "decade" = "decade")

# Define UI for random distribution app ----
ui <- fluidPage(# Show a tabset that includes a plot, summary, and
  # table view of the generated distribution
  mainPanel(tabsetPanel(
    tabPanel("About",
             fluidRow(
               HTML('This app uses satellite temperature data from <b>UAH</b>'),
               br(),
               br(),
               p(
                 'Temperature temp is measured as deviation (deg C) from 1991-2020 baseline'
               ),
               HTML(
                 'The dataset contains <strong>14310</strong> observations and 5 variables<br>'
               ),
               HTML('Here is a small (random) <em>sample</em> of data:<br>'),
               br(),
               column(4, dataTableOutput('sample'))
             )),
    tabPanel("Plots",
             sidebarLayout(
               sidebarPanel(
                 p(
                   'You can analyze the global temperature for different regions. Select the regions you are interested in. You see a monthly scatterplot and the corresponding trend lines.'
                 ),
                 
                 checkboxInput("trend", "Display trend(s)", FALSE),
                 
                 radioButtons("palette", "Palette:",
                              color_palettes,
                              selected = color_palettes[1]),
                 
                 checkboxGroupInput(
                   "regions",
                   "Select the region(s) to display",
                   choiceNames = regions,
                   choiceValues = regions,
                   selected = regions[1]
                 )
               ),
               mainPanel(plotOutput("plot"))
             )),
    
    tabPanel("Tables",
             sidebarLayout(
               sidebarPanel(
                 p(
                   'This panel displays average temperature over different time periods: months , years and decades'
                 ),
                 radioButtons("time_level", "Average over:",
                              time_levels,
                              selected = time_levels[1])
               ),
               mainPanel(textOutput('range'),
                         br(),
                         dataTableOutput('table'))
             ))
  )))

# Define server logic for random distribution app ----
server <- function(input, output) {
  output$sample <- renderDataTable(sample_n(df, 5))
  
  df_region <- reactive({
    test <- df %>% filter(region %in% input$regions & !is.na(temp))
    print(nrow(test))
    test
  })
  
  palette <- reactive({
    input$palette
  })
  
  output$plot <- renderPlot({
    df_region <- df_region()
    count <- nrow(df_region)
    title <-
      paste("Time period 1978 - 2023 . In total",
            count,
            "non-missing observations")
    p <-
      ggplot(df_region, aes(x = date, y = temp, color = region)) +
      geom_point(na.rm = TRUE) +
      ggtitle(title) +
      xlab("Year") +
      ylab("Temperature: deviation from 1991-2020 baseline, deg C") +
      scale_color_brewer(palette = palette())
    if (input$trend) {
      p + geom_smooth(method = lm) #add linear trend line
    } else {
      p
    }
  })
  
  output$range <- renderText({
    if (input$time_level == 'month') {
      t <-
        df %>% filter(!is.na(temp)) %>% group_by(year, month) %>% summarise(temp =
                                                                              mean(temp))
    } else if (input$time_level == 'year') {
      t <- df %>% group_by(year) %>% summarise(temp = mean(temp))
    } else if (input$time_level == 'decade') {
      t <- df %>% group_by(decade) %>% summarise(temp = mean(temp))
    }
    min <- sprintf("%.2f", min(t$temp))
    max <- sprintf("%.2f", max(t$temp))
    return(paste('Temperature range', min, max))
  })
  
  output$table <- renderDataTable({
    if (input$time_level == 'month') {
      t <- df %>% group_by(year, month) 
    } else if (input$time_level == 'year') {
      t <- df %>% group_by(year)
    } else if (input$time_level == 'decade') {
      t <- df %>% group_by(decade)
    }
    return(t %>% summarise(temp = round(mean(temp),2)))
  })
}

# Create Shiny app ----
shinyApp(ui, server)