##### WorkAround #####
## By: Ning Soong, Elena Tsai, and Jihane Mellouk
## Pearl Hacks 2023

#Hi, and welcome to our R-based custom workout app! Before you get started, here's some setup:
  #Step 1: Install required packages
  #Step 2: Change your working directory
  #Step 3: Get some water, some tunes pumping, a timer, and start your at-home custom workout!

#STEP 1: TO INSTALL REQUIRED PACKAGES:
  #Run this code in your R console:
  # (1) install.packages("shiny")
  # (2) install.packages("DT")

#STEP 2: TO CHANGE YOUR WORKING DIRECTORY:
  # (1) Ctrl - Shift - H
  # OR
  # (1) Session > Set Working Directory > Choose Directory
  # (2) Navigate to the folder you saved app.R in
  # (3) Hit "Open"
  # (4) Your console should have printed out a setwd() command. Copy & paste it below, replacing "setwd("~/App-1")

#Change working directory here:
setwd("~/App-1")

#STEP #: GET YOUR WORKOUT ON!
  # (1) Press "Run App" in the top right-hand corner of your script window.
  # (2) Select your options and follow the displayed workout!

library(shiny)
library(DT)

#Read in our custom-made workout databases
armworkouts <- read.csv("https://raw.githubusercontent.com/ningsoong/pearlhacks2023/main/data/arm_workout.csv")
legworkouts <- read.csv("https://raw.githubusercontent.com/ningsoong/pearlhacks2023/main/data/leg_workout.csv")
coreworkouts <- read.csv("https://raw.githubusercontent.com/ningsoong/pearlhacks2023/main/data/core_workout.csv")

#Read in our custom-made workout sequences
tenmin_one <- read.csv("https://raw.githubusercontent.com/ningsoong/pearlhacks2023/main/data/10min1.csv")
tenmin_two <- read.csv("https://raw.githubusercontent.com/ningsoong/pearlhacks2023/main/data/10min2.csv")
tenmin_three <- read.csv("https://raw.githubusercontent.com/ningsoong/pearlhacks2023/main/data/10min3.csv")
tenmin_four <- read.csv("https://raw.githubusercontent.com/ningsoong/pearlhacks2023/main/data/10min4.csv")
tenmin_five <- read.csv("https://raw.githubusercontent.com/ningsoong/pearlhacks2023/main/data/10min5.csv")

twentymin_one <- read.csv("https://raw.githubusercontent.com/ningsoong/pearlhacks2023/main/data/20min1.csv")
twentymin_two <- read.csv("https://raw.githubusercontent.com/ningsoong/pearlhacks2023/main/data/20min2.csv")
twentymin_three <- read.csv("https://raw.githubusercontent.com/ningsoong/pearlhacks2023/main/data/20min3.csv")
twentymin_four <- read.csv("https://raw.githubusercontent.com/ningsoong/pearlhacks2023/main/data/20min4.csv")
twentymin_five <- read.csv("https://raw.githubusercontent.com/ningsoong/pearlhacks2023/main/data/20min5.csv")

thirtymin_one <- read.csv("https://raw.githubusercontent.com/ningsoong/pearlhacks2023/main/data/30min1.csv")
thirtymin_two <- read.csv("https://raw.githubusercontent.com/ningsoong/pearlhacks2023/main/data/30min2.csv")
thirtymin_three <- read.csv("https://raw.githubusercontent.com/ningsoong/pearlhacks2023/main/data/30min3.csv")
thirtymin_four <- read.csv("https://raw.githubusercontent.com/ningsoong/pearlhacks2023/main/data/30min4.csv")
thirtymin_five <- read.csv("https://raw.githubusercontent.com/ningsoong/pearlhacks2023/main/data/30min5.csv")

#Creates time categories
ten_min_workouts <- list(tenmin_one, tenmin_two, tenmin_three, tenmin_four, tenmin_five)
twenty_min_workouts <- list(twentymin_one, twentymin_two, twentymin_three, twentymin_four, twentymin_five)
thirty_min_workouts <- list(thirtymin_one, thirtymin_two, thirtymin_three, thirtymin_four, thirtymin_five)

#Creates blank dataframe used for output
df <- data.frame(exercises  = NA,
                 time = NA
                 
)

# Define UI ----
ui <- fluidPage(
  titlePanel("WorkAround"),
  
  sidebarLayout(
    
    sidebarPanel( #The areas for user input
      
      numericInput("time", label="Time", value = 10, min = 10, max = 30, step = 10),
      selectInput("targetarea", label="Target Area", choices = c("Arms", "Legs", "Core"))
      
    ),
    
    mainPanel( #The area for workout output
      p("Workout"),
      textOutput("time_output"),
      textOutput("targetarea_output"),
      DT::dataTableOutput("workout_table_output")
    ),
    
  ),
)


# Define server logic ----
server <- function(input, output) {
  
  output$time_output <- renderText({ 
    paste("You have selected a ", input$time, " minute workout.")
  })
  
  output$targetarea_output <- renderText({
    paste("You are focusing on ", tolower(input$targetarea), "today.")
  })
  
  output$workout_table_output <- DT::renderDataTable({
    #Getting the blank workout sequence
    if (as.numeric(input$time) == 10) {
      random_seq_num <- sample(1:length(ten_min_workouts), 1, replace = TRUE)
      workout_seq <- data.frame(ten_min_workouts[random_seq_num])
    } else if (as.numeric(input$time) == 20) {
      random_seq_num <- sample(1:length(twenty_min_workouts), 1, replace = TRUE)
      workout_seq <- data.frame(twenty_min_workouts[random_seq_num])
    } else if (as.numeric(input$time) == 30) {
      random_seq_num <- sample(1:length(thirty_min_workouts), 1, replace = TRUE)
      workout_seq <- data.frame(thirty_min_workouts[random_seq_num])
    }
    
    #Getting the workouts
    if (input$targetarea == "Arms") {
      n <- as.numeric(sum(is.na(workout_seq$activity)))
      random_workout_list <- sample(1:nrow(armworkouts), n, replace = TRUE)
      workout_list <- armworkouts$Arms[random_workout_list]
    } else if (input$targetarea == "Legs") {
      n <- as.numeric(sum(is.na(workout_seq$activity)))
      random_workout_list <- sample(1:nrow(legworkouts), n, replace = TRUE)
      workout_list <- legworkouts$Legs[random_workout_list]
    } else if (input$targetarea == "Core") {
      n <- as.numeric(sum(is.na(workout_seq$activity)))
      random_workout_list <- sample(1:nrow(coreworkouts), n, replace = TRUE)
      workout_list <- coreworkouts$Core[random_workout_list]
    }

    #Combine times & workouts
    num_of_breaks <- 0
    for (i in 1:nrow(workout_seq)) {
      df$time[i] <- workout_seq[i, "time"]
      if(is.na(workout_seq$activity[i]) == TRUE) { # Adds a workout into only blank slots
        df$exercises[i] <- workout_list[i - num_of_breaks]
      } else {
        df$exercises[i] <- "BREAK"
        num_of_breaks <- num_of_breaks + 1
      }
      df[nrow(df)+1,] <- NA
      if (i == nrow(workout_seq)) {
        df <- head(df, -1)
      }
    }
    
    #Display the dataframe
    DT::datatable(df, options = list(pageLength = 1000, replace = TRUE), filter = c("none"), selection = "none")
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)