#Load packages 
library(dplyr)
library(tidyverse)
library(purrr)
library(ggplot2)
library(readr)
library(reshape2)
library(gganimate)
library(shinydashboard)
library(bslib)
library(shinythemes)

#Define functions

#this function is used twice, so I am defining it up here 
calculate_4thdown_conv <- function(data, selected_teams) {
  data %>%
    filter(posteam %in% selected_teams & down == 4 & yrdline100 <=2) %>%
    group_by(Season, posteam) %>%
    summarise(TotalAttempts = n(),
              Successful = sum(Touchdown == 1, na.rm = TRUE),
              ConversionRate = Successful / TotalAttempts,
              Type = "Fourth Down Conversion") %>% 
    mutate(Team = posteam) %>% select(-posteam)
}

#this function is also used twice 
calculate_twopt_conv <- function(data, selected_teams) {
  data %>% 
    mutate(TwoPointConv = ifelse(TwoPointConv == "Success", 1, 0)) %>% 
    filter(posteam %in% selected_teams & !is.na(TwoPointConv)) %>% 
    group_by(Season,posteam) %>% 
    summarise(TotalAttempts = n(),
              Successful = sum(TwoPointConv, na.rm = TRUE),
              ConversionRate = Successful/TotalAttempts,
              Type = "Two-Point Conversion") %>%
    mutate(Team=posteam) %>% select(-posteam)
}


#Load data 
read_csv("NFL_Play_by_Play_2009-2017(v4).csv") 

#Define UI 

ui <- fluidPage(
  theme=shinytheme("darkly"),
  titlePanel("NFL 2-point and 4th and Short Conversion Analysis"),
  
  sidebarLayout(
    sidebarPanel(
    
    #Select team (categorical variable)
    selectInput(
      inputId = "team_selection",
      label = "Select Team(s):",
      choices = unique(NFL_data$posteam),
      selected = "PIT",
      multiple = TRUE #Enables multiple selection
    ),
    
    #Select Season (categorical variable)
    sliderInput(
      inputId = "season_range",
      label = "Select Season Range for Plots:",
      min = 2013,
      max= 2017,
      value = c(2013,2017),
      sep = ""
    ),
    
    #select season for contingency tables 
    #i made a seperate input for the cables for if someone wanted to compare say the first year of the range, 2013, to the most recent year 
    checkboxGroupInput(
      inputId = "contingency_seasons",
      label = "Select Season(s) for Contingency Table:",
      choices = unique(NFL_data$Season),
      selected = unique(NFL_data$Season)[1]
    ),
    
    uiOutput("numeric_vars_ui"), #dynamic ui for num var selection
    actionButton("update","Update")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel( #ABOUT TAB 
          "About",
          h4("About This App"),
          p("This Shiny App provides insights into NFL teams' performance on two-point conversions and fourth-down plays in the 2013-2017 Seasons. Many fans have speculated that the success rates of these plays have been increasing, and thus teams should chance them more. Here, we can explore the statistics of these plays by team or between teams for these 5 seasons."),
          h5("Data Description"),
          p("The data used in this app includes NFL play-by-play data from 2013 to 2017, featuring variables such as team name, season, down, distance from the end zone, and play outcomes. This data can be downloaded in the Data Download tab. For more information, visit the data source", a("here on Kaggle.",href = "https://www.kaggle.com/datasets/maxhorowitz/nflplaybyplay2009to2016/data")),
          
          h5("Tab Descriptions"),
          p(strong("Statistics:"), " Summary statistics for selected numeric variables categorized by team, as well as conversion rates of both 4th and short downs and 2-point plays across the entire League."),
          p(strong("2-Point Conversion Rates:"), " Visualizations and contingency tables for two-point conversion attempts across selected seasons and teams."),
          p(strong("4th Down and Short Conversion Rates:"), " Visualizations and contingency tables of fourth-down conversion rates on short-yardage plays. Here, we define 'short' as less than or equal to two yards from the goal line."),
          p(strong("Team Comparisons:"), " Comparison plots of conversion rates across selected teams over multiple seasons."),
          p(strong("Compare Conversions Per Team:"), " Interactive plot allowing side-by-side comparison of both types of conversion rates by team."),
          
          #NFL logo 
          img(src = "nfl_logo.svg", height = "300px", width = "400px")
  
        ),
        tabPanel( #NUMERICAL SUMMARIES AND STATISTICS TAB 
          "Statistics",
          tagList(
            h4("Summary Table: Select Numeric Variables and Teams"),
          tableOutput("summary_table")
          ),
          tagList(
            h4("Multi-year Conversion Rates Across the League"),
          tableOutput("multiyear_table")
          )
        ),
        tabPanel( #TWO POINT CONVERSION RATES TAB 
          "2-Point Conversion Rates",
          tableOutput("two_pt_contingency_table"),
          plotOutput("twopoint_plot"),
          tableOutput("twopt_team_table"),
          plotOutput("twopt_team_plot")
        ),
        tabPanel( #4TH DOWN AND SHORT YARDAGE CONVERSION RATES TAB 
          "4th Down and Short Conversion Rates",
          tableOutput("fourth_down_contingency_table"),
          plotOutput("fourthdown_plot"),
          tableOutput("fourth_team_table"),
          plotOutput("fourth_team_plot")
        ),
        tabPanel( #CONVERSION COMPARISONS BETWEEN TEAMS TAB- LINE PLOT 
          "Team Comparisons",
          plotOutput("fourth_lineplot"),
          plotOutput("twopt_lineplot")
        ),
        tabPanel("Compare Conversions Per Team", #INTERACTIVE PLOT TO VIEW BOTH RATES TAB 
                 textOutput("dynamic_message"),
                 plotOutput("interactive_plot")
        ),
        tabPanel("Data Download", #DATA DOWNLOAD TAB 
          fluidPage(
            h3("Download Filtered Data"),
            h4("Please use the season range slider to select years. Make sure to hit the 'update' button for data!"),
      
            #data table output for viewing
            DT::dataTableOutput("filtered_data_table"),
                   
            #download button
            downloadButton("download_data", "Download Filtered Data")
                 )
        )
    )
    
  )))


#Define server
server <- function(input, output, session) {
  
  #Identify numeric columns in NFL_data for dynamic UI generation
  numeric_vars <- reactive({
    NFL_data %>%
      select(where(is.numeric)) %>%
      colnames()
  })
  
  #render UI for numeric variable selection
  output$numeric_vars_ui <- renderUI({
    checkboxGroupInput(
      inputId = "numeric_vars",
      label = "Select Numeric Variables to Summarize:",
      choices = numeric_vars(),
      selected = numeric_vars()[1]
    )
  })
  
  #reactive expression for filtered data based on user input 
  filtered_data <- reactive({
    NFL_data %>% 
      filter(
        posteam %in% input$team_selection,
        Season >= input$season_range[1],
        Season <= input$season_range[2]
      )
  })
  
  output$dynamic_message <- renderText({
    selected_team <- input$team_selection
    paste("You selected team(s):", selected_team)
  })
  
  #STATISTICS TAB -------------------------------------------
  #summary statistics table 
  output$summary_table <- renderTable({
    req(input$numeric_vars)
    
    filtered_data() %>% 
      group_by(posteam) %>%
      summarise(across(
        all_of(input$numeric_vars),
        list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE), median = ~median(.x, na.rm= TRUE))
      ))
  })
  
  output$multiyear_table <- renderTable({
    # Two-point conversion counts
    temp_2013_2pt <- c(Failure = 38, Success = 33)
    temp_2014_2pt <- c(Failure = 31, Success = 28)
    temp_2015_2pt <- c(Failure = 46, Success = 43)
    temp_2016_2pt <- c(Failure = 52, Success = 47)
    temp_2017_2pt <- c(Failure = 44, Success = 33)
    multiyr_2ptdata <- rbind(temp_2013_2pt, temp_2014_2pt, temp_2015_2pt, temp_2016_2pt, temp_2017_2pt)
    
    # 4th down conversion counts
    temp_2013_4th <- c(Failure = 20, Success = 25)
    temp_2014_4th <- c(Failure = 18, Success = 22)
    temp_2015_4th <- c(Failure = 21, Success = 26)
    temp_2016_4th <- c(Failure = 23, Success = 29)
    temp_2017_4th <- c(Failure = 19, Success = 24)
    multiyr_4thdata <- rbind(temp_2013_4th, temp_2014_4th, temp_2015_4th, temp_2016_4th, temp_2017_4th)
    
    # Combine both tables
    multiyear_table <- cbind("Year" = 2013:2017, "2PT_Failure" = multiyr_2ptdata[, "Failure"], 
                             "2PT_Success" = multiyr_2ptdata[, "Success"],
                             "4th_Failure" = multiyr_4thdata[, "Failure"], 
                             "4th_Success" = multiyr_4thdata[, "Success"])
    
    multiyear_table
  })
  
  #2PT CONV TAB ------------------------------------------

  #two point cont. table 
  output$two_pt_contingency_table <- renderTable({
    twopt_contingency_data <- NFL_data %>% 
      filter(Season %in% input$contingency_seasons) %>% 
      group_by(Season, TwoPointConv) %>% 
      summarise(count = n(), .groups = 'drop') %>% 
      pivot_wider(names_from = TwoPointConv, values_from = count, values_fill = 0)
    
    twopt_contingency_data
  })
  
  #multiyear plot
  output$twopoint_plot <- renderPlot({
    temp_2013_2pt <- c(Failure = 38, Success = 33)
    temp_2014_2pt <- c(Failure = 31, Success = 28)
    temp_2015_2pt <- c(Failure = 46, Success = 43)
    temp_2016_2pt <- c(Failure = 52, Success = 47)
    temp_2017_2pt <- c(Failure = 44, Success = 33)
    multiyr_2ptdata <- rbind(temp_2013_2pt, temp_2014_2pt, temp_2015_2pt, temp_2016_2pt, temp_2017_2pt)
    
    # Assign row and column names for better interpretation
    rownames(multiyr_2ptdata) <- c("2013", "2014", "2015", "2016", "2017")
    colnames(multiyr_2ptdata) <- c("Failure", "Success")
    
    #barplot of data to view any trends
    multi_2ptdata<- as.data.frame(multiyr_2ptdata)
    #Adding column for the year 
    multi_2ptdata$year <- rownames(multiyr_2ptdata)
    #reshaping for ggplot, i like this function from reshape2
    multi_2pt_long<- melt(multi_2ptdata, id.vars = "year")
    #plotting
    ggplot(multi_2pt_long, aes(x = year, y = value, fill = variable)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Two-Point Conversion Successes and Failures by Year",
           x = "Year",
           y = "Count") +
      scale_fill_manual(values = c("Failure" = "red", "Success" = "green")) +
      theme_minimal()

  })
  
  #contingency table by TEAM 
  output$twopt_team_table <- renderTable({
    two_point_plays <- NFL_data %>%
      filter(TwoPointConv %in% c("Success", "Failure") & posteam %in% input$team_selection & Season %in% input$contingency_seasons)
    
    #create a contingency table based on team, year, and two-point conversion success
    contingency_2pt_by_team_year <- table(two_point_plays$posteam, 
                                          two_point_plays$Season, 
                                          two_point_plays$TwoPointConv)
    
    contingency2pt_df <- as.data.frame(contingency_2pt_by_team_year)
    colnames(contingency2pt_df) <- c("Team", "Year", "TwoPointConv", "Count")
    
    #View the contingency table
    contingency2pt_df
  })
  
  
  #barplot of data by TEAM 
  output$twopt_team_plot <- renderPlot({
    #filter the two-point conversion plays for the selected team
    two_point_plays_team <- NFL_data %>%
      filter(TwoPointConv %in% c("Success", "Failure") & posteam %in% input$team_selection & Season %in% input$season_range)
    
    contingency_2pt_by_year <- table(two_point_plays_team$Season, 
                                     two_point_plays_team$TwoPointConv)
    
    #Convert the contingency table to a data frame
    selected_team_2pt_df <- as.data.frame(contingency_2pt_by_year)
    colnames(selected_team_2pt_df) <- c("Year", "TwoPointConv", "Count")
    
    #Plotting
    ggplot(selected_team_2pt_df, aes(x = factor(Year), y = Count, fill = factor(TwoPointConv))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = paste("Two-Point Conversion Success for", input$team_selection),
           x = "Year",
           y = "Count of Two-Point Conversions",
           fill = "Two-Point Conversion") +
      scale_fill_manual(values = c("Failure" = "red", "Success" = "green"),
                        labels = c("Failure" = "Failure", "Success" = "Success")) +
      theme_minimal()
  })
  

  #4th DOWN CONV TAB --------------------------------------------------------
  
  #Contingency table 
  output$fourth_down_contingency_table <- renderTable({
    fourth_down_data <- NFL_data %>%
      filter(Season %in% input$contingency_seasons, down == 4, yrdline100 <= 2) %>%
      group_by(Season, Touchdown) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(Touchdown = ifelse(Touchdown == 1, "Success", "Failure")) %>%
      pivot_wider(names_from = Touchdown, values_from = count, values_fill = 0)
    
    fourth_down_data
  })
  
  #Multiyear barplot for 4th and short conv data  - fourthdown_plot 
  output$fourthdown_plot <- renderPlot({
    four_temp_2013 <- c(Failure = 67, Success = 23)
    four_temp_2014 <- c(Failure = 52, Success = 13)
    four_temp_2015 <- c(Failure = 48, Success = 16)
    four_temp_2016 <- c(Failure = 46, Success = 19)
    four_temp_2017 <- c(Failure = 47, Success = 16)
    
    # Combine data
    multiyr_fourthdata <- rbind(four_temp_2013, four_temp_2014, four_temp_2015, four_temp_2016, four_temp_2017)
    
    # Assign row and column names for better interpretation
    rownames(multiyr_fourthdata) <- c("2013", "2014", "2015", "2016", "2017")
    colnames(multiyr_fourthdata) <- c("Failure", "Success")
    
    multiyr_fourthdata
    
    #barplot of data to view any trends
    multi_fourthdata<- as.data.frame(multiyr_fourthdata)
    #Adding column for the year 
    multi_fourthdata$year <- rownames(multiyr_fourthdata)
    #reshaping for ggplot, i like this function from reshape2
    multi_fourth_long<- melt(multi_fourthdata, id.vars = "year")
    #plotting
    ggplot(multi_fourth_long, aes(x = year, y = value, fill = variable)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Fourth and Short Conversion Successes and Failures by Year",
           x = "Year",
           y = "Count") +
      scale_fill_manual(values = c("Failure" = "red", "Success" = "green")) +
      theme_minimal()
  })
  
  #contingency table by TEAM 
  output$fourth_team_table <- renderTable({
    # Filter the dataset for fourth down plays
    fourth_down_plays <- NFL_data %>%
      filter(down == 4 & yrdline100 <= 2 & posteam %in% input$team_selection & Season %in% input$contingency_seasons) 
    
    # Create a contingency table based on team, year, and touchdown success
    contingency_4th_by_team_year <- table(fourth_down_plays$posteam, 
                                          fourth_down_plays$Season, 
                                          fourth_down_plays$Touchdown)
    
    contingency4th_df <- as.data.frame(contingency_4th_by_team_year)
    colnames(contingency4th_df) <- c("Team", "Year", "Touchdown", "Count")
    
    #view table 
    contingency4th_df
  })
  
  
  #4th and short plot by TEAM 
  output$fourth_team_plot <- renderPlot({
    fourth_down_plays_team <- NFL_data %>%
      filter(down == 4 & yrdline100 <= 2 & posteam %in% input$team_selection & Season %in% input$season_range) 
    
    #Create a contingency table based on touchdown success for the team
    contingency4th_table_by_team <- table(fourth_down_plays_team$Season, fourth_down_plays_team$Touchdown)
    
    contingency4th_table_by_team
    
    #Convert contingency table to a data frame for easier manipulation
    selected_team_4th_df <- as.data.frame(contingency4th_table_by_team)
    colnames(selected_team_4th_df) <- c("Season", "Touchdown", "Count")
    
    #plotting
    ggplot(selected_team_4th_df, aes(x = factor(Season), y = Count, fill = factor(Touchdown))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = paste("Fourth Down Touchdown Success for", input$team_selection),
           x = "Season",
           y = "Count of Fourth Down Attempts",
           fill = "Touchdown Outcome") +
      scale_fill_manual(values = c("0" = "red", "1" = "green"),
                        labels = c("0" = "Failure", "1" = "Success")) +
      theme_minimal()
    
  })
  
#TEAM COMPARISONS TAB --------------------------------------------------------
  
  output$fourth_lineplot <- renderPlot({
    
  #fourth down conversion rates needed for plot 
  calculate_4thdown_conv(NFL_data, selected_teams = input$team_selection) -> temp
  
  #Create the line plot
  fourth_line_plot <- ggplot(temp, aes(x = Season, y = ConversionRate, color = Team, group = Team)) +
    geom_line(size = 1) +  # Line for each team
    geom_point(size = 3) +  # Points at each season
    labs(title = "Fourth Down Conversion Rates by Team",
         x = "Season",
         y = "Conversion Rate",
         color = "Team") +
    theme_minimal()
  
  # Display the plot
  print(fourth_line_plot)
  
  })
  
  output$twopt_lineplot <- renderPlot({
    
    calculate_twopt_conv(NFL_data, selected_teams = input$team_selection) ->temp
    
    #Create the line plot
    line_plot_2pt <- ggplot(temp, aes(x = Season, y = ConversionRate, color = Team, group = Team)) +
      geom_line(size = 1) +  # Line for each team
      geom_point(size = 3) +  # Points at each season
      labs(title = "Two-Point Conversion Rates by Team",
           x = "Season",
           y = "Conversion Rate",
           color = "Team") +
      theme_minimal()
    
    # Display the plot
    print(line_plot_2pt)
    
  })
  
  #COMPARE CONVERSIONS PER TEAM TAB ----------------------------------------
  output$interactive_plot <- renderPlot({
    # Get conversion rates for two-point and fourth down for multiple teams
    temp_two_point_data <- calculate_twopt_conv(NFL_data, selected_teams = input$team_selection )
    temp_fourth_down_data <- calculate_4thdown_conv(NFL_data, selected_teams = input$team_selection)
    
    #Combining datasets 
    temp_all <- bind_rows(temp_fourth_down_data,temp_two_point_data)
    
    #Plotting
    ggplot(temp_all, aes(x=Season, y=ConversionRate, color= Team, linetype = Type, group = interaction(Team, Type))) +
      geom_smooth(se=FALSE, size = 1.2) + #smoothed lines
      geom_text(data = temp_all %>% group_by(Team,Type) %>% slice_max(Season),
                aes(label=Team), hjust=1.2,size = 4) + #add team labels at the end of each line 
      labs(title= "Two-point and Fourth Down Conversion Rates",
           x = "Season",
           y = "Conversion Rate",
           color = "Team",
           linetype = "Conversion Type") + 
      scale_x_continuous(breaks = 2013:2017) + #setting axis for the 2013-2017 seasons 
      theme_minimal()
    
    
  })
  
  #DATA DOWNLOAD TAB -----------------------------------------------------------
  # Reactive to subset data when the update button is clicked
  filtered_data <- eventReactive(input$update, {
  # Filter data based on the user inputs for team and season
  NFL_data %>%
    filter(posteam %in% input$team_selection,
           Season >= input$season_range[1],
           Season <= input$season_range[2])
})

# Render the filtered data table using DT
output$filtered_data_table <- DT::renderDataTable({
  DT::datatable(filtered_data())
})

# Download handler for saving the filtered data
output$download_data <- downloadHandler(
  filename = function() { paste("Filtered_NFL_Data", Sys.Date(), ".csv", sep = "") },
  content = function(file) {
    write.csv(filtered_data(), file, row.names = FALSE)
  }
)
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
