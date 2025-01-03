library(shiny)
library(shinyjs)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(fresh)
library(cluster)
library(gargle)

# Google OAuth Configuration
options(
  gargle_oauth_email = TRUE,
  gargle_oauth_client = gargle::gargle_oauth_client(
    id = "YOUR_CLIENT_ID",        # Replace with your client ID
    secret = "YOUR_CLIENT_SECRET" # Replace with your client secret
  )
)

# Custom theme for the dashboard
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#1f77b4",
    red = "#d62728",
    green = "#2ca02c",
    yellow = "#ff7f0e"
  ),
  adminlte_sidebar(
    width = "240px",
    dark_bg = "#2c3e50",
    dark_hover_bg = "#34495e",
    dark_color = "#ecf0f1"
  ),
  adminlte_global(
    content_bg = "#f8f9fa",
    box_bg = "#ffffff",
    info_box_bg = "#ffffff"
  )
)

# Load and prepare data
df <- dataset

medals_data <- df %>%
  filter(!is.na(Medal)) %>%
  group_by(Year, NOC) %>%
  summarise(
    Gold = sum(Medal == "Gold"),
    Silver = sum(Medal == "Silver"),
    Bronze = sum(Medal == "Bronze"),
    Total = Gold + Silver + Bronze
  ) %>%
  ungroup()

# Dashboard UI
dashboard_ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = span(
      tags$img(src = "olympic_rings.png", height = "25px"),
      "Olympic Analytics"
    ),
    titleWidth = 300
  ),
  dashboardSidebar(
    width = 240,
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("info-circle")), 
      menuItem("Overview", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Country Analysis", tabName = "country", icon = icon("flag")),
      menuItem("Trends", tabName = "trends", icon = icon("chart-area")),
      menuItem("Advanced Analytics", tabName = "advanced", icon = icon("brain")),
      menuItem("Visualization", tabName = "visualization", icon = icon("chart-bar")),
      menuItem("Country Success", tabName = "country_success", icon = icon("trophy")),
      menuItem("About Us", tabName = "about_us", icon = icon("info-circle")),
      actionButton("logout_btn","Logout",style="background-color:red;width:100px;"),
      tags$hr(),
      selectInput(
        "year", "Olympic Year",
        choices = unique(medals_data$Year),
        selected = max(medals_data$Year)
      ),
      selectInput(
        "noc", "Select Country",
        choices = c("All", sort(unique(medals_data$NOC))),
        selected = "All"
      ),
      selectInput(
        "medal_type", "Medal Category",
        choices = c("Total", "Gold", "Silver", "Bronze"),
        selected = "Total"
      )
    )
  ),
  dashboardBody(
    use_theme(mytheme),
    tags$head(
      tags$style(HTML("
        .box {border-radius: 10px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);}
        .content-wrapper {background-color: #f8f9fa;}
        .small-box {border-radius: 10px;}
      "))
    ),
    tabItems(
      # Welcome Tab
      tabItem(
        tabName = "welcome",
        fluidRow(
          box(
            title = "Welcome to the Dashboard!",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = 400,
            div(
              tags$img(src = "olympic_rings.png", height = "100px"),
              h2("Hello, and Welcome!"),
              p("We are thrilled to have you here."),
              p("This dashboard is designed to offer a deep dive into the fascinating world of Olympic analytics."),
              p("Use the navigation menu to explore various insights, trends, and advanced analytics."),
              p("Start your journey by selecting a tab from the sidebar menu. Enjoy your exploration!")
            )
          )
        )
      ),
      
      # Visualization Tab
      tabItem(
        tabName = "visualization",
        fluidRow(
          box(
            title = "Visualization Filters",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            selectInput("visual_year", "Select Year", choices = unique(medals_data$Year), selected = max(medals_data$Year)),
            selectInput("visual_noc", "Select Country", choices = c("All", sort(unique(medals_data$NOC))), selected = "All"),
            selectInput("visual_medal_type", "Select Medal Type", choices = c("Total", "Gold", "Silver", "Bronze"), selected = "Total")
          )
        ),
        fluidRow(
          box(
            title = "Bar Chart of Medal Counts",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("visual_bar_chart"),
            width = 6
          ),
          box(
            title = "Scatter Plot of Medal Counts",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("visual_scatter_plot"),
            width = 6
          )
        ),
        fluidRow(
          box(
            title = "Area Chart of Medal Trends",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("visual_area_chart"),
            width = 6
          ),
          box(
            title = "Pie Chart of Medal Distribution",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("visual_pie_chart"),
            width = 6
          )
        )
      ),
      # tabs (Overview, Country Analysis, Trends, Advanced Analytics)
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("totalMedals", width = 3),
          valueBoxOutput("goldMedals", width = 3),
          valueBoxOutput("silverMedals", width = 3),
          valueBoxOutput("bronzeMedals", width = 3)
        ),
        fluidRow(
          box(
            title = "Medal Distribution by Country",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("barChart"),
            width = 8,
            height = 400
          ),
          box(
            title = "Medal Type Distribution",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("pieChart"),
            width = 4,
            height = 400
          )
        ),
        fluidRow(
          box(
            title = "Historical Trends",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("lineChart"),
            width = 12,
            height = 400
          )
        )
      ),
      tabItem(
        tabName = "country",
        fluidRow(
          box(
            title = "Country Performance Analysis",
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("scatterPlot"),
            width = 6
          ),
          box(
            title = "Medal Distribution",
            status = "danger",
            solidHeader = TRUE,
            plotlyOutput("histogram"),
            width = 6
          )
        )
      ),
      tabItem(
        tabName = "trends",
        fluidRow(
          box(
            title = "Medal Trends Over Time",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("trendChart"),
            width = 12,
            height = 400
          )
        )
      ),
      tabItem(
        tabName = "advanced",
        fluidRow(
          box(
            title = "Correlation Analysis",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("correlationHeatmap"),
            width = 6,
            height = 400
          ),
          box(
            title = "Country Clustering",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("clusterPlot"),
            width = 6,
            height = 400
          )
        ),
        fluidRow(
          box(
            title = "Medal Prediction",
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("regressionPlot"),
            width = 12,
            height = 400
          )
        )
      ),
      tabItem(
        tabName = "country_success",
        fluidRow(
          box(
            title = "Country Success Rate Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            column(4,
                   selectInput("success_country", "Select Country", 
                               choices = c("All", sort(unique(medals_data$NOC))), 
                               selected = "All"),
                   selectInput("success_medal_type", "Medal Type", 
                               choices = c("Total", "Gold", "Silver", "Bronze"), 
                               selected = "Total"),
                   sliderInput("success_year_range", "Year Range", 
                               min = min(medals_data$Year), 
                               max = max(medals_data$Year), 
                               value = c(min(medals_data$Year), max(medals_data$Year)))
            ),
            column(8,
                   plotlyOutput("country_success_rate_plot")
            )
          )
        ),
        fluidRow(
          box(
            title = "Medal Prediction Model",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            column(4,
                   selectInput("predict_country", "Select Country", 
                               choices = c("All", sort(unique(medals_data$NOC))), 
                               selected = "All"),
                   selectInput("predict_medal_type", "Predict Medal Type", 
                               choices = c("Total", "Gold", "Silver", "Bronze"), 
                               selected = "Total"),
                   numericInput("predict_future_year", "Predict for Year", 
                                value = max(medals_data$Year) + 4, 
                                min = max(medals_data$Year) + 1)
            ),
            column(8,
                   plotlyOutput("medal_prediction_plot"),
                   verbatimTextOutput("prediction_details")
            )
          )
        )
      ),
      tabItem(
        tabName = "about_us",
        fluidRow(
          box(
            title = "About Olympic Analytics",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            div(
              style = "text-align: center;",
              tags$img(src = "olympic_rings.png", height = "150px", style = "margin-bottom: 20px;"),
              h1("Olympic Analytics: Unraveling Olympic Excellence", style = "color: #2c3e50;"),
              p("Our mission is to transform Olympic data into meaningful insights", 
                style = "font-size: 18px; color: #7f8c8d;")
            )
          )
        ),
        fluidRow(
          box(
            title = "Our Story",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            div(
              h3("The Genesis of Olympic Analytics"),
              p("Founded by a team of data enthusiasts and sports lovers, Olympic Analytics was born from a passion to understand the complex world of Olympic achievements."),
              p("We believe that behind every medal lies a story of dedication, strategy, and human potential."),
              p("Our platform aims to provide unprecedented insights into Olympic performance, helping athletes, coaches, and sports enthusiasts understand the deeper narratives of Olympic success.")
            )
          ),
          box(
            title = "Our Vision",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            div(
              h3("Empowering Insights Through Data"),
              p("We envision a world where sports analytics bridges the gap between raw performance and strategic understanding."),
              tags$ul(
                tags$li("Democratize access to comprehensive Olympic data"),
                tags$li("Provide intuitive and powerful analytical tools"),
                tags$li("Support athletes and sports organizations in their pursuit of excellence")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Key Features",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            div(
              h3("What Makes Us Unique"),
              fluidRow(
                column(4,
                       div(
                         icon("chart-line", class = "fa-3x", style = "color: #3498db; margin-bottom: 10px;"),
                         h4("Comprehensive Analytics"),
                         p("In-depth analysis of Olympic performances across years and countries.")
                       )
                ),
                column(4,
                       div(
                         icon("brain", class = "fa-3x", style = "color: #2ecc71; margin-bottom: 10px;"),
                         h4("Predictive Modeling"),
                         p("Advanced algorithms to predict future Olympic performances.")
                       )
                ),
                column(4,
                       div(
                         icon("globe", class = "fa-3x", style = "color: #e74c3c; margin-bottom: 10px;"),
                         h4("Global Perspective"),
                         p("Insights that transcend national boundaries and sporting disciplines.")
                       )
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Data and Methodology",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            div(
              h3("Our Commitment to Accuracy"),
              p("Olympic Analytics is powered by comprehensive Olympic datasets covering events from 1896 to 2016."),
              p("Our methodologies include:"),
              tags$ul(
                tags$li("Rigorous data cleaning and preprocessing"),
                tags$li("Advanced statistical techniques"),
                tags$li("Machine learning algorithms for predictive analysis"),
                tags$li("Continuous validation and refinement of analytical models")
              ),
              p("We are committed to transparency, accuracy, and continuous improvement in our analytical approach.")
            )
          )
        ),
        fluidRow(
          box(
            title = "Contact Us",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            div(
              style = "text-align: center;",
              h3("Get in Touch"),
              p("Interested in our work? Have questions or suggestions?"),
              tags$a(
                href = "mailto:sutharyogesh843@gmail.com", 
                class = "btn btn-primary",
                style = "background-color: #3498db; color: white; padding: 10px 20px; text-decoration: none; border-radius: 5px;",
                "Contact Us"
              )
            )
          )
        )
      )
    )
  )
)

# Define Main UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
        font-family: 'Arial', sans-serif;
      }
      #welcome-page, #login-page {
        background: rgba(255, 255, 255, 0.9);
        border-radius: 20px;
        box-shadow: 0 10px 25px rgba(0,0,0,0.1);
        padding: 40px;
        max-width: 600px;
        margin: 50px auto;
        text-align: center;
      }
      .btn-get-started {
        background: linear-gradient(to right, #1e90ff, #4169e1);
        color: white !important;
        border: none;
        padding: 12px 30px;
        font-size: 18px;
        border-radius: 30px;
        transition: transform 0.3s ease;
      }
      .btn-get-started:hover {
        transform: scale(1.05);
        background: linear-gradient(to right, #4169e1, #1e90ff);
      }
      .login-title {
        color: #2c3e50;
        font-weight: bold;
        margin-bottom: 20px;
      }
      .login-subtitle {
        color: #7f8c8d;
        margin-bottom: 30px;
      }
    "))
  ),
  useShinyjs(),
  
  # Welcome Page
  div(
    id = "welcome-page",
    div(
      tags$img(src = "olympic_rings.png", height = "200px", style = "margin-bottom: 30px;"),
      h1("Olympic Analytics", class = "login-title", style = "font-size: 2.5em;"),
      p("Explore the World of Olympic Excellence", class = "login-subtitle"),
      actionButton(
        "get_started_btn", 
        "Get Started", 
        class = "btn btn-primary btn-lg btn-get-started", 
        icon = icon("arrow-right")
      )
    )
  ),
  
  # Login Page (hidden by default)
  hidden(
    div(
      id = "login-page",
      h2("Welcome Back!", class = "login-title"),
      p("Sign in to access your Olympic Analytics Dashboard", class = "login-subtitle"),
      actionButton("login_btn", "Log in with Google", 
                   class = "btn btn-primary btn-lg", 
                   style = "background-color: #db4437; color: white; border: none; border-radius: 30px; padding: 12px 30px;"),
      tags$div(
        style = "margin-top: 20px;",
        actionButton("back_to_welcome", "Back", 
                     class = "btn btn-secondary", 
                     style = "border-radius: 30px; padding: 10px 25px;")
      )
    )
  ),
  
  # Dashboard Page (hidden by default)
  hidden(
    div(
      id = "dashboard-page",
      dashboard_ui
    )
  )
)

# Define Server
server <- function(input, output, session) {
  user_data <- reactiveVal(NULL)
  
  # Handle Get Started button
  observeEvent(input$get_started_btn, {
    hide("welcome-page", anim = TRUE, animType = "fade")
    show("login-page", anim = TRUE, animType = "slide")
  })
  
  # Handle Back button from login page
  observeEvent(input$back_to_welcome, {
    hide("login-page", anim = TRUE, animType = "slide")
    show("welcome-page", anim = TRUE, animType = "fade")
  })
  
  # Handle Google Login
  observeEvent(input$login_btn, {
    token <- gargle::token_fetch(scopes = "https://www.googleapis.com/auth/userinfo.profile")
    user_info <- gargle::token_userinfo(token)
    
    # Save user data
    user_data(user_info)
    
    # Toggle views
    hide("login-page", anim = TRUE, animType = "fade")
    show("dashboard-page", anim = TRUE, animType = "slide")
    
    # Automatically navigate to Welcome tab
    updateTabItems(session, "sidebarMenu", selected = "welcome")
  })
  
  # Dashboard server logic (filtered dataset, value boxes, and plots)
  # Filtered dataset for Visualization tab
  visual_data <- reactive({
    data <- medals_data %>%
      {if (input$visual_noc != "All") filter(., NOC == input$visual_noc) else .} %>%
      filter(Year == input$visual_year)
    return(data)
  })
  
  # Bar Chart
  output$visual_bar_chart <- renderPlotly({
    plot_ly(visual_data(), x = ~NOC, y = ~get(input$visual_medal_type), type = "bar") %>%
      layout(title = paste(input$visual_medal_type, "Medals by Country"))
  })
  
  # Scatter Plot
  output$visual_scatter_plot <- renderPlotly({
    plot_ly(visual_data(), x = ~Gold, y = ~Silver, type = "scatter", mode = "markers",
            text = ~paste("Country:", NOC, "<br>Gold:", Gold, "<br>Silver:", Silver)) %>%
      layout(title = "Gold vs Silver Medals")
  })
  
  # Area Chart
  output$visual_area_chart <- renderPlotly({
    medals_data %>%
      filter(NOC == input$visual_noc | input$visual_noc == "All") %>%
      group_by(Year) %>%
      summarise(Medals = sum(get(input$visual_medal_type))) %>%
      plot_ly(x = ~Year, y = ~Medals, type = "scatter", fill = "tozeroy", mode = "lines") %>%
      layout(title = paste(input$visual_medal_type, "Medal Trends Over Time"))
  })
  
  # Pie Chart
  output$visual_pie_chart <- renderPlotly({
    plot_ly(labels = c("Gold", "Silver", "Bronze"),
            values = c(sum(visual_data()$Gold), sum(visual_data()$Silver), sum(visual_data()$Bronze)),
            type = "pie") %>%
      layout(title = "Medal Type Distribution")
  })
  
  # Filtered dataset
  filtered_data <- reactive({
    data <- medals_data %>%
      {if (input$noc != "All") filter(., NOC == input$noc) else .} %>%
      filter(Year == input$year)
    return(data)
  })
  
  get_selected_medals <- reactive({
    switch(input$medal_type,
           "Total" = filtered_data()$Total,
           "Gold" = filtered_data()$Gold,
           "Silver" = filtered_data()$Silver,
           "Bronze" = filtered_data()$Bronze)
  })
  
  # Value Boxes
  output$totalMedals <- renderValueBox({
    valueBox(sum(filtered_data()$Total), "Total Medals", icon = icon("medal"), color = "blue")
  })
  output$goldMedals <- renderValueBox({
    valueBox(sum(filtered_data()$Gold), "Gold Medals", icon = icon("trophy"), color = "yellow")
  })
  output$silverMedals <- renderValueBox({
    valueBox(sum(filtered_data()$Silver), "Silver Medals", icon = icon("award"), color = "aqua")
  })
  output$bronzeMedals <- renderValueBox({
    valueBox(sum(filtered_data()$Bronze), "Bronze Medals", icon = icon("medal"), color = "orange")
  })
  
  
  # Overview Charts
  output$barChart <- renderPlotly({
    plot_ly(filtered_data(), x = ~NOC, y = ~get(input$medal_type), type = "bar") %>%
      layout(title = paste(input$medal_type, "Medals by Country"))
  })
  output$pieChart <- renderPlotly({
    plot_ly(labels = c("Gold", "Silver", "Bronze"),
            values = c(sum(filtered_data()$Gold), sum(filtered_data()$Silver), sum(filtered_data()$Bronze)),
            type = "pie")
  })
  output$lineChart <- renderPlotly({
    medals_data %>%
      group_by(Year) %>%
      summarise(Value = sum(get(input$medal_type))) %>%
      plot_ly(x = ~Year, y = ~Value, type = "scatter", mode = "lines+markers") %>%
      layout(title = paste(input$medal_type, "Medals Over Years"))
  })
  
  # Country Analysis Charts
  output$scatterPlot <- renderPlotly({
    plot_ly(filtered_data(), x = ~Gold, y = ~Silver, text = ~NOC, mode = "markers")
  })
  output$histogram <- renderPlotly({
    plot_ly(x = get_selected_medals(), type = "histogram")
  })
  
  # Trends Chart
  output$trendChart <- renderPlotly({
    medals_data %>%
      group_by(Year) %>%
      summarise(Gold = sum(Gold), Silver = sum(Silver), Bronze = sum(Bronze)) %>%
      plot_ly(x = ~Year) %>%
      add_trace(y = ~Gold, name = "Gold", type = "scatter", mode = "lines+markers") %>%
      add_trace(y = ~Silver, name = "Silver", type = "scatter", mode = "lines+markers") %>%
      add_trace(y = ~Bronze, name = "Bronze", type = "scatter", mode = "lines+markers") %>%
      layout(title = "Medal Trends Over Time")
  })
  
  # Advanced Analytics Charts
  output$correlationHeatmap <- renderPlotly({
    corr_data <- medals_data %>%
      select(Gold, Silver, Bronze, Total) %>%
      cor()
    plot_ly(x = colnames(corr_data), y = rownames(corr_data), z = corr_data, type = "heatmap")
  })
  output$clusterPlot <- renderPlotly({
    data <- medals_data %>% select(Gold, Silver, Bronze) %>% scale()
    clust <- kmeans(data, centers = 3)
    medals_data$Cluster <- as.factor(clust$cluster)
    plot_ly(medals_data, x = ~Gold, y = ~Silver, z = ~Bronze, color = ~Cluster, type = 'scatter3d')
  })
  output$regressionPlot <- renderPlotly({
    medals_trend <- medals_data %>% group_by(Year) %>% summarise(TotalMedals = sum(Total))
    fit <- lm(TotalMedals ~ Year, data = medals_trend)
    medals_trend$Prediction <- predict(fit, newdata = medals_trend)
    plot_ly(medals_trend, x = ~Year, y = ~TotalMedals, name = "Actual") %>%
      add_trace(y = ~Prediction, name = "Predicted")
  })
  # Country Success Rate Analysis
  country_success_data <- reactive({
    data <- medals_data %>%
      filter(Year >= input$success_year_range[1] & Year <= input$success_year_range[2]) %>%
      {if (input$success_country != "All") filter(., NOC == input$success_country) else .}
    
    data %>%
      group_by(NOC) %>%
      summarise(
        MedalCount = sum(get(input$success_medal_type)),
        YearCount = n_distinct(Year)
      ) %>%
      mutate(SuccessRate = MedalCount / YearCount)
  })
  
  output$country_success_rate_plot <- renderPlotly({
    success_data <- country_success_data()
    
    plot_ly(success_data, x = ~NOC, y = ~SuccessRate, type = "bar") %>%
      layout(
        title = paste("Medal Success Rate for", 
                      input$success_medal_type, "Medals"),
        xaxis = list(title = "Country"),
        yaxis = list(title = "Average Medals per Olympic Year")
      )
  })
  
  # Medal Prediction Model
  predict_model <- reactive({
    # Prepare data for prediction
    prediction_data <- medals_data %>%
      {if (input$predict_country != "All") filter(., NOC == input$predict_country) else .} %>%
      group_by(Year) %>%
      summarise(MedalValue = sum(get(input$predict_medal_type)))
    
    # Simple linear regression model
    model <- lm(MedalValue ~ Year, data = prediction_data)
    
    # Predict future medals
    future_prediction <- predict(model, newdata = data.frame(Year = input$predict_future_year))
    
    list(
      model = model,
      prediction_data = prediction_data,
      future_prediction = future_prediction
    )
  })
  
  output$medal_prediction_plot <- renderPlotly({
    pred_result <- predict_model()
    
    plot_ly(pred_result$prediction_data, x = ~Year, y = ~MedalValue, 
            type = "scatter", mode = "lines+markers", name = "Historical") %>%
      add_trace(x = input$predict_future_year, 
                y = pred_result$future_prediction, 
                type = "scatter", mode = "markers", 
                marker = list(color = "red", size = 10), 
                name = "Prediction")
  })
  
  output$prediction_details <- renderPrint({
    pred_result <- predict_model()
    
    cat("Prediction Details:\n")
    cat("Predicted", input$predict_medal_type, "Medals for", input$predict_future_year, ":\n")
    cat(round(pred_result$future_prediction, 2), "\n\n")
    
    cat("Model Summary:\n")
    summary(pred_result$model)
  })
  
  # Handle Logout
  observeEvent(input$logout_btn, {
    gargle::token_remove()  # Clear token
    user_data(NULL)         # Clear user data
    
    # Toggle views
    hide("dashboard-page", anim = TRUE, animType = "slide")
    show("login-page", anim = TRUE, animType = "fade")
  })
}

# Run the App
shinyApp(ui, server)