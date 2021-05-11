library(httpuv)
library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
#library(quantreg)
#library(shinythemes)
#library(colourpicker)
#library(basicTrendline)
library(shinydashboard)
#library("flexdashboard")
library(png)
library(jpeg)
#library(directlabels)
library(scales)
library(shinycssloaders)
library(shinyWidgets)
library(gridExtra)
library(grid)
library(gridGraphics)
library(devtools)
library(lubridate)


library(data.table)
library(RJSONIO)
library(ndjson)
library(jsonlite)




list.of.packages <- c("httpuv", "shiny", "ggplot2", "tidyverse", "dplyr", "shinydashboard", "png", "jpeg", "scales", "shinycssloaders", "shinyWidgets", "gridExtra", "grid", "gridGraphics", "devtools", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

Sys.setenv(LANG = "en")
#Sys.setlocale("LC_ALL","English")

#tiFile <- read.csv("p02ti.csv", stringsAsFactors = FALSE)
#scoresFile <- read.csv("p02scores.csv", stringsAsFactors = FALSE, sep="\t")
#adhFile <- read.csv("p02adh.csv", stringsAsFactors = FALSE, sep="\t")
#pdFile <- read.csv("p02pd.csv", stringsAsFactors = FALSE, sep="\t")

scores_directory="data/ball_game_acc"
ti_directory="data/TIP"
adh_directory="data/medication_adherence"
pd_directory="data/health"
drawings1_directory="data/Drawing_data_for_R.csv"
merged_directoy="data/screenshots/JPG/merged"
spiral_pd_directoy="data/screenshots/JPG/spiral/pd"
square_pd_directoy="data/screenshots/JPG/square/pd"
photos_directoy="data/photos"
baseUrl="https://stop1-8af28.firebaseio.com"

#if (interactive()) {

ui <- dashboardPage(
  #skin = "red",
  dashboardHeader(title = "Track Myself"),
  #dashboardHeader(title = "STOP", titleWidth = 350),
  
  dashboardSidebar(
    #width = 350,
    
    sidebarMenu(id = "sidebar",
                #sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                #                           label = "Type patient number"),
                #textInput("search"),
                #textOutput("text2"),
                #selectInput("list_of_patients", "Patient number:",
                #           c("Select", 1:13)),
                textInput("textInputDeviceId", "Device ID", value = "", width = NULL,
                          placeholder = NULL),
                #selectInput("list_of_patients", "Patient number:",
                #           c("Select", 1:13), size = 13, selectize=FALSE),
                
                #sidebarMenuOutput("sidebar_search_content"),
                #sidebarUserPanel(name="a", subtitle = NULL, image = NULL),
                menuItem("Dashboard", tabName = "dashboard",startExpanded=TRUE, icon = icon("dashboard")),
                menuItem("Medication", icon = icon("th"), startExpanded=TRUE, tabName = "medication"),
                menuItem("Symptoms", icon = icon("th"), startExpanded=TRUE, tabName = "symptoms"),
                #menuItem("Drawings Error", icon = icon("th"), tabName = "drawings1"),
                #menuItem("Drawings", icon = icon("th"), tabName = "drawings2"),
                #menuItem("Tremor Intensity Parameter", icon = icon("th"), tabName = "games_tip"),
                menuItem("Game scores", icon = icon("th"), startExpanded=TRUE, tabName = "game_scores")
    )
  ),
  dashboardBody(
    #titlePanel("Game date and TIP"),
    fluidRow(
      #column(1, textOutput("text99")),
      column(3, textOutput("textPlaceHolder")),
      column(5, textOutput("text990")),
      #column(2, textOutput("text2")),
      column(4, dateRangeInput(
        inputId = "daterange1",
        label = NULL,
        start = Sys.Date()-6, # The initial start date. Either a Date object, or a string in yyyy-mm-dd format
        end = Sys.Date(), # The initial end date. Either a Date object, or a string in yyyy-mm-dd format 
        min = as.Date('2018-5-5'), # The minimum allowed date. Either a Date object, or a string in yyyy-mm-dd format.
        max = Sys.Date(), # The maximum allowed date. Either a Date object, or a string in yyyy-mm-dd format.
        format = "dd/mm/yyyy",  # The format of the date to display in the browser. try "mm/dd/yy"  
        separator = "to" # String to display between the start and end input boxes. try "to"
      ))
      #column(4, checkboxInput("input1Id", label = "Order by highest TIP")),
      #column(4, colourInput("col9", "Select colour", "red"))
    ),
    #if (input$sidebar == "dashboard") {
    
    conditionalPanel(
      condition = "input.sidebar == 'dashboard' && (input.textInputDeviceId == '')",
      fluidRow(
        box(width=12, title = "Medication", status = "primary", solidHeader = TRUE, height = 308,
            collapsible = TRUE, withSpinner(htmlOutput("text1")))
        #column(6, box(title = "Histogram", status = "primary", solidHeader = TRUE,
        #             collapsible = TRUE, plotOutput("plot3"))),
        #column(6, box(title = "Histogram", status = "primary", solidHeader = TRUE,
        #             collapsible = TRUE, plotOutput("plot4")))
      )),
    conditionalPanel(
      condition = "input.sidebar == 'dashboard' && (input.textInputDeviceId == '')",
      fluidRow(
        box(width=12, title = "Symptoms", status = "primary", solidHeader = TRUE, height = 308,
            collapsible = TRUE, withSpinner(htmlOutput("text2")))
        #column(6, box(title = "Histogram", status = "primary", solidHeader = TRUE,
        #             collapsible = TRUE, plotOutput("plot3"))),
        #column(6, box(title = "Histogram", status = "primary", solidHeader = TRUE,
        #             collapsible = TRUE, plotOutput("plot4")))
      )),
    conditionalPanel(
      condition = "input.sidebar == 'dashboard' && (input.textInputDeviceId == '')",
      fluidRow(
        box(width=12, title = "Game scores", status = "primary", solidHeader = TRUE, height = 308,
            collapsible = TRUE, withSpinner(htmlOutput("text6")))
        #column(6, box(title = "Histogram", status = "primary", solidHeader = TRUE,
        #             collapsible = TRUE, plotOutput("plot3"))),
        #column(6, box(title = "Histogram", status = "primary", solidHeader = TRUE,
        #             collapsible = TRUE, plotOutput("plot4")))
      )),
    
    #),
    
    conditionalPanel(
      condition = "input.sidebar == 'dashboard' && input.textInputDeviceId != ''",
      fluidRow(
        box(width=12, title = "Medication", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, withSpinner(plotOutput("plot1", height = 245))),
        
        #column(6, box(title = "Histogram", status = "primary", solidHeader = TRUE,
        #             collapsible = TRUE, plotOutput("plot3"))),
        #column(6, box(title = "Histogram", status = "primary", solidHeader = TRUE,
        #             collapsible = TRUE, plotOutput("plot4")))
      ),
      fluidRow(
        box(width=12, title = "Symptoms", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, withSpinner(plotOutput("plot2", height = 245)))
        #box(title = "Drawings 2", status = "primary", solidHeader = TRUE,
        #   collapsible = TRUE, plotOutput("plot4", height = 245))
      ),
      fluidRow(
        #column(6, box(plotOutput("plot1"))),
        #column(6, box(plotOutput("plot2")))
        box(width=12, title = "Game scores", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, withSpinner(plotOutput("plot6", height = 245)))
      )
    ),
    
    
    conditionalPanel(
      condition = "input.sidebar == 'medication' && (input.textInputDeviceId == '')",
      fluidRow(
        column(12, box(width=12, title = "Medication", status = "primary", solidHeader = TRUE, height = 603,
                       collapsible = TRUE, withSpinner(htmlOutput("text10")))
        ))),
    conditionalPanel(
      condition = "input.sidebar == 'medication' && input.textInputDeviceId != ''",
      fluidRow(
        column(12, box(width=12, title = "Medication", status = "primary", solidHeader = TRUE,
                       collapsible = TRUE, withSpinner(plotOutput("plot10", height = 540)))
        ))),
    
    conditionalPanel(
      condition = "input.sidebar == 'symptoms' && (input.textInputDeviceId == '')",
      fluidRow(
        column(12, box(width=12, title = "Symptoms", status = "primary", solidHeader = TRUE, height = 603,
                       collapsible = TRUE, withSpinner(htmlOutput("text20")))
        ))),
    conditionalPanel(
      condition = "input.sidebar == 'symptoms' && input.textInputDeviceId != ''",
      fluidRow(
        column(12, box(width=12, title = "Symptoms", status = "primary", solidHeader = TRUE,
                       collapsible = TRUE, withSpinner(plotOutput("plot20", height = 540)))
        ))),
    
    
    
    conditionalPanel(
      condition = "input.sidebar == 'game_scores' && (input.textInputDeviceId == '')",
      fluidRow(
        column(12, box(width=12, title = "Game scores", status = "primary", solidHeader = TRUE, height = 603,
                       collapsible = TRUE, withSpinner(htmlOutput("text60")))
        ))),
    conditionalPanel(
      condition = "input.sidebar == 'game_scores' && input.textInputDeviceId != ''",
      fluidRow(
        column(12, box(width=12, title = "Game scores", status = "primary", solidHeader = TRUE,
                       collapsible = TRUE, withSpinner(plotOutput("plot60", height = 540)))
        )))
    
  )
)


server <- function(input, output, session) {
  
  
  
  
  # 1. Trick file date creation update
  onStop(function() {
    # 1. File name
    p <- paste0(getwd(), "app.R")
    # 2. Update file 'date creation'
    Sys.setFileTime(p, Sys.time())
  }) # onStop
  
  #output$text2 <- renderText({
  #  a = input$list_of_patients
  # a
  #})
  
  
  output$text <- renderText({
    b=input$textInputDeviceId
    if (b=="Select"||b==0) {
      b=""
    }
    a=paste("Device ID: ", b, sep=" ")
    #a=input$sidebar
    a
  })
  
  output$text990 <- renderText({
    goodGsonFlag=TRUE
    b=input$textInputDeviceId
    if (b=="") {
      active_time_for_patient="Active period for patient:"
    } else {
      days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
      
      
      deviceId=b
      
      a=str_interp("${baseUrl}/${deviceId}/games.json")
      #theJsonSymptoms <- fromJSON(a)
      
      tryCatch(
        {
          theJsonGames <- fromJSON(a)
        },
        error= function(error_condition) {
          active_time_for_patient="Active period for patient:"
          goodGsonFlag=FALSE
          #stop(safeError("Wrong patient id, please check patient id again."))
        }
      )
      if (is.null(theJsonGames)) {
        active_time_for_patient="Active period for patient:"
        goodGsonFlag=FALSE
        #stop(safeError("Wrong patient id, please check patient id again."))
      }
      
      ids <- NULL
      dateTimes <- NULL
      scores <- NULL
      levels <- NULL
      
      counter=1
      for (i in theJsonGames) {
        ids[counter]=theJsonGames[[counter]]$id
        dateTimes[counter]=theJsonGames[[counter]]$dateTime
        scores[counter]=theJsonGames[[counter]]$score
        levels[counter]=theJsonGames[[counter]]$level
        counter=counter+1
      }
      mydf2=data.frame(id=ids, Date=dateTimes, Scores=scores, Levels=levels)
      mydf2$Scores<-(as.double(mydf2$Scores))
      
      
      if (goodGsonFlag) {
        #scoresFile <- read.csv(a, stringsAsFactors = FALSE, sep="\t")
        first_date=as.Date(first(mydf2$Date))
        last_date=as.Date(last(mydf2$Date))
        active_time=paste(first_date, last_date, sep = " to ")
        active_time_for_patient=paste("Active period for patient:", active_time, sep=" ")
      } else {
        active_time_for_patient="Active period for patient:"
      }
    }
    
    active_time_for_patient
    
  })
  
  #output$text1 <- renderImage({
  # width  <- session$clientData$output_text1_width
  #height <- session$clientData$output_text1_height
  #filename <- normalizePath(file.path('./data/photos',
  #                                   paste('med_adh', '.png', sep='')))
  #list(src = filename, width = width, height = height)
  #png()
  #}, deleteFile = FALSE)
  
  
  output$text1 <- renderText({
    
    paste("<h4> <b> The deviation of the medication intake time from the regimen 
          (before of after) in minutes. 15 minutes limit is shown as a red line 
          in the plot. The trend is shown as a blue line in the plot. </b>",
          "<br> <br>", "For extended review, press the 'Medication' side tab.",
          "<br>", "Please enter the Device ID.", "</h4>")
  })
  output$text2 <- renderText({
    paste("<h5> <b> The user evaluates the symptom level daily: </b>", "<br>",
          "- No symptoms, excellent day.", "<br>",
          "- Light symptoms with no concrete interface with my daily activities.", "<br>",
          "- Mild symptoms that slightly interfered with my daily activities.", "<br>",
          "- Strong symptoms but they did not stop me from carrying out my daily activities.", "<br>",
          "- Severe symptoms that prevented me from carrying out one or more of my daily activities.", "<br>",
          "The trend is shown as a blue line in the plot.",
          "<br> <br>", "For extended review, press the 'Symptoms' side tab.",
          "<br>", "Please enter the Device ID.", "</h5>")
  })
  output$text3 <- renderText({
    paste("<h3> <b> The average error of the spiral drawings in pixels. The trend 
          is shown as a blue line in the plot. </b>",
          "<br> <br>", "For extended review, press the Drawings Error' side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  output$text33 <- renderText({
    paste("<h3> <b>The average error of the square drawings in pixels. 
          The trend is shown as a blue line in the plot. </b>",
          "<br> <br>", "For extended review, press the 'Drawings Error' side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  output$text4 <- renderText({
    paste("<h3> <b> The spiral drawings. </b>",
          "<br> <br>", "For extended review, press the 'Drawings' side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  output$text44 <- renderText({
    paste("<h3> <b> The square drawings. </b>",
          "<br> <br>", "For extended review, press the 'Drawings' side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  output$text5 <- renderText({
    paste("<h3> <b>Tremor Intensity Parameter of the each ball game session. 
          Tremor intensity parameter describes the tremor level during a game session. 
          The trend is shown as a blue line in the plot. </b>",
          "<br> <br>", "For extended review, press the 'Tremor Intensity Parameter' side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  output$text6 <- renderText({
    paste("<h3> <b>Ball game scores. The trend is shown as a blue line in the plot.</b>",
          "<br> <br>", "For extended review, press the 'Game scores' side tab.",
          "<br>", "Please enter the Device ID.", "</h3>")
  })
  
  
  output$text10 <- renderText({
    paste("<h3> <b> The deviation of the medication intake time from the regimen 
          (before of after) in minutes. 15 minutes limit is shown as a red line 
          in the plot. The trend is shown as a blue line in the plot. </b>",
          "<br> <br>", "For extended review, press the 'Medication' side tab.",
          "<br>", "Please enter the Device ID.", "</h3>")
  })
  output$text20 <- renderText({
    paste("<h4> <b> The user evaluates the symptom level daily: </b>", "<br>",
          "- No symptoms, excellent day.", "<br>",
          "- Minimal symptoms with no concrete interface with my daily activities.", "<br>",
          "- Mild symptoms that slightly interfered with my daily activities.", "<br>",
          "- Moderate symptoms but they did not stop me from carrying out my daily activities.", "<br>",
          "- Severe symptoms that prevented me from carrying out one or more of my daily activities.", "<br>",
          "The trend is shown as a blue line in the plot.",
          "<br> <br>", "For extended review, press the 'Symptoms' side tab.",
          "<br>", "Please enter the Device ID.", "</h4>")
  })
  output$text30 <- renderText({
    paste("<h3> <b> The average error of the spiral drawings in pixels. The trend 
          is shown as a blue line in the plot. </b>",
          "<br> <br>", "For extended review, press the 'Drawings Error' side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  output$text330 <- renderText({
    paste("<h3> <b>The average error of the square drawings in pixels. 
          The trend is shown as a blue line in the plot. </b>",
          "<br> <br>", "For extended review, press the 'Drawings Error' side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  output$text40 <- renderText({
    paste("<h3> <b> The spiral drawings. </b>",
          "<br> <br>", "For extended review, press the 'Drawings' side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  output$text440 <- renderText({
    paste("<h3> <b> The square drawings. </b>",
          "<br> <br>", "For extended review, press the 'Drawings' side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  output$text50 <- renderText({
    paste("<h3> <b>Tremor Intensity Parameter of the each ball game session. 
          Tremor intensity parameter describes the tremor level during a game session. 
          The trend is shown as a blue line in the plot. </b>",
          "<br> <br>", "For extended review, press the 'Tremor Intensity Parameter' side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  output$text60 <- renderText({
    paste("<h3> <b>Ball game scores. The trend is shown as a blue line in the plot.</b>",
          "<br> <br>", "For extended review, press the 'Game scores' side tab.",
          "<br>", "Please enter the Device ID.", "</h3>")
  })
  
  #output$sidebar_search_content <- renderPlot({
  # ggplot()
  #})
  
  
  
  output$plot1 <- renderPlot({
    
    days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
    if (days_count<1) {
      stop(safeError("End date should be after start date."))
      
    }
    
    
    #b=input$list_of_patients
    #if (b=="Select"||b==0) {
    #photo=paste(photos_directoy, "med_adh.png", sep="/")
    #pic = readPNG(photo)
    #plot.new()
    #return(grid::grid.raster(pic))
    #}
    #if (b!="Select"&&b!=0)
    #{
    #c=paste("p0", b, sep="")
    #if (as.numeric(b)>9) {
    # c=paste("p", b, sep="")
    #}
    #d=paste(c, "csv", sep = ".")
    #a=paste(adh_directory, d, sep = "/")
    
    #if (file.exists(a)) {
    # adhFile <- read.csv(a, stringsAsFactors = FALSE, sep="\t")
    #}
    #else
    # stop(safeError("Missing data file for the patient."))
    
    
    #filtered <-
    # adhFile %>%
    #filter(intake_time >= input$daterange1[1],
    #      intake_time <= input$daterange1[2])
    #filtered <- filtered[ ,c("diff_min", "intake_time")]
    #filtered <- unique(filtered)
    
    #a <- filtered[ ,c("intake_time")]
    #b <- filtered[ ,c("diff_min")]
    #mydf = data.frame(Date=a, Diff=b)
    
    deviceId=input$textInputDeviceId
    
    if (deviceId!="") {
      
      #deviceId= "f2cdccbde923a7bb"
      a=str_interp("${baseUrl}/${deviceId}/normalmedicationsgroups.json")
      #theJsonSymptoms <- fromJSON(a)
      
      tryCatch(
        {
          theJsonMedicationGroups <- fromJSON(a)
        },
        error= function(error_condition) {
          stop(safeError("Wrong patient id, please check patient id again."))
          
        }
      )
      if (is.null(theJsonMedicationGroups)) {
        stop(safeError("Wrong patient id, please check patient id again."))
      }
      #print(theJsonMedicationGroups)
      
      
      
      ids <- NULL
      normalGroupNames <- NULL
      optimalTimes <- NULL
      takenDateTimes <- NULL
      timeDifferences <- NULL
      
      counter=1
      for (i in theJsonMedicationGroups) {
        #print("a")
        ids[counter]=theJsonMedicationGroups[[counter]]$id
        #['id']
        #print(ids[counter])
        normalGroupNames[counter]=theJsonMedicationGroups[[counter]]$normal_group_name
        optimalTimes[counter]=theJsonMedicationGroups[[counter]]$optimal_time
        takenDateTimes[counter]=theJsonMedicationGroups[[counter]]$taken_dateTime
        
        takenDateTimeString=as.POSIXct(takenDateTimes[counter], format="%Y-%m-%dT%H:%M:%S")
        
        optimalTimeDate<-paste(as.Date(takenDateTimes[counter]), optimalTimes[counter])
        
        timeDifference1=difftime(takenDateTimeString, optimalTimeDate, units = "mins")
        if (timeDifference1 < 0) {
          timeDifference1=timeDifference1+1440
        }
        
        timeDifference2=difftime(optimalTimeDate, takenDateTimeString, units = "mins")
        if (timeDifference2 < 0) {
          timeDifference2=timeDifference2+1440
        }
        
        if (timeDifference1 < timeDifference2) {
          timeDifferences[counter]=timeDifference1
        }
        if (timeDifference2 < timeDifference1) {
          timeDifferences[counter]=timeDifference2
        }
        if (timeDifference1 == timeDifference2) {
          timeDifferences[counter]=timeDifference1
        }
        
        counter=counter+1
      }
      
      mydf2=data.frame(id=ids,Date=takenDateTimes, Diff=timeDifferences,
                       GroupMedName=normalGroupNames, OptimalTimes=optimalTimes)
      #print("gg")
      #print(mydf2)
      #mydf2$Diff <- (difftime(as.POSIXct(mydf2$Date, format="%Y-%m-%dT%H:%M:%S"), 
      #                       paste(as.Date(mydf2$Date), mydf2$OptimalTimes), units = "mins"))
      #print("hh")
      #print(mydf2)
      
      
      
      
      
      
      
      date_error = as.numeric(input$daterange1[2]-as.Date(mydf2[c(1),"Date"]))+1
      if (date_error<1) {
        stop(safeError("No data for the patient in this time period."))
      }
      maximum=as.numeric(count(mydf2))
      date_error = as.numeric(as.Date(mydf2[c(maximum),"Date"])-input$daterange1[1])+1
      if (date_error<1) {
        stop(safeError("No data for the patient in this time period."))
      }
      
      mydf <-
        mydf2 %>%
        filter(Date >= input$daterange1[1],
               Date <= input$daterange1[2])
      
      
      
      if (as.numeric(count(mydf))==0) {
        stop(safeError("No data for the patient in this time period."))
      }
      
      
      #days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
      #if (days_count<1) {
      # stop(safeError("End date should be after start date."))
      
      #}
      
      #mydf$ddate <- factor(strftime(mydf$Date,format="%d"))
      
      #mydf$day <- factor(strftime(mydf$Date,format="%a"),
      #                  levels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
      
      #mydf$day <- factor(mydf$day,levels=unique(mydf$day))
      
      #mydf$week <- factor(strftime(mydf$Date,format="%V"))
      #mydf$week <- factor(mydf$week,levels=unique(mydf$week))
      
      #mydf$month <- strftime(mydf$Date,format="%b")
      #mydf$month <- factor(mydf$month,levels=unique(mydf$month))
      
      #mydf$year <- strftime(mydf$Date,format="%Y")
      #mydf$year <- factor(mydf$year,levels=unique(mydf$year))
      
      #mydf2 = mydf[!duplicated(mydf[ , c("ddate")]),]
      #days_count = count(mydf2)
      
      
      
      filtered2 <- mydf2
      filtered2 <- filtered2[ ,c("Diff", "Date")]
      filtered2 <- unique(filtered2)
      #print(filtered2)
      
      #print(count(filtered2))
      first_point_intake = filtered2[c(1),2]
      
      count = count(filtered2)
      count = as.numeric(count)
      
      one_count_flag=0
      if (count<1) {
        stop(safeError("Empty data file for the patient."))
      }
      last_point_intake = filtered2[c(count),2]
      if (count==1) {
        count=2
        one_count_flag=1
      }
      
      half_point_intake = round(count/2)
      half_point_intake = as.numeric(half_point_intake)
      half_point_intake = filtered2[c(half_point_intake),2]
      #print(half_point_intake)
      
      if (one_count_flag==1) {
        half_point_intake_plus_one = round(count/2)
      } else {
        half_point_intake_plus_one = round(count/2) + 1
      }
      
      half_point_intake_plus_one = as.numeric(half_point_intake_plus_one)
      half_point_intake_plus_one = filtered2[c(half_point_intake_plus_one),2]
      #print(half_point_intake_plus_one)
      
      first_half <- 
        filtered2  %>%
        filter(Date >= first_point_intake,
               Date <= half_point_intake)
      
      #print(first_half)
      average1 = mean(as.numeric(first_half$Diff))
      #print(average1)
      
      
      second_half <-
        filtered2  %>%
        filter(Date >= half_point_intake_plus_one,
               Date <= last_point_intake)
      
      #print(second_half)
      average2 = mean(as.numeric(second_half$Diff))
      #print(average2)
      
      
      #if (input$input3Id == TRUE) {
      # mydf$Date <- factor(mydf$Date, levels = mydf$Date[order(mydf$Diff)])
      #}
      
      
      last=as.numeric(count(mydf))
      for (i in 1:last) {
        #mydf$Date=as.Date(mydf$Date)
        mydf$Date=as.POSIXct(mydf$Date, format="%Y-%m-%dT%H:%M:%S")
        #day_and_date=paste(mydf$day, mydf$ddate, sep = "\n")
        #mydf$day_and_date=paste(day_and_date, mydf$month, sep = "\n")
      }
      
      
      if (days_count < 8) {
        for (i in 1:last) {
          mydf$Date[i]=mydf$Date[i]+ lubridate::days(0)
        }
        first_date=first(as.Date(mydf$Date))
        first_date=paste(first_date, "00:00:00", sep = " ")
        first_date=as.POSIXct(first_date)
        
        last_day=last(as.Date(mydf$Date))+ lubridate::days(1)
        last_day=paste(last_day, "00:00:00", sep = " ")
        last_day=as.POSIXct(last_day)
        
        ggplot(mydf, aes(x=Date, y=Diff))+
          scale_x_datetime(breaks = "1 day", minor_breaks = "1 days", labels = date_format("%a\n%d\n%b"),name="Date range (Day)")+
          #trendline(x1,y1,model="line2P",summary=TRUE) #geom_count()
          #geom_smooth(data = mydf, aes(x=game_id, y=Scores), method = lm, se = FALSE)
          theme_minimal() +
          #scale_x_date(breaks = "1 day",minor_breaks = "1 days",labels = date_format("%a\n%d\n%b"),name="Day")+
          theme(axis.text=element_text(size=10),
                axis.title=element_text(size=12,face="bold"))+
          geom_point() + xlab("Date range (Day)") + ylab("Time deviation (minutes)") +
          expand_limits(x=c(first_date,last_day))+
          #scale_y_continuous(breaks = seq(0, 10000, 5)) +
          #geom_segment(aes(x = 0, y = as.numeric(average1), xend = Inf, yend = as.numeric(average2)), size = 1, color="Blue", arrow = arrow(length = unit(0.5,"cm"))) +
          #annotate("text", x = last(mydf$day), y = (as.numeric(average2)+20), label = "Trend line of time adherence")+
          geom_hline(yintercept=15, color = "Red", size = 1)+
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_day, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(day),15,label = "15 Minutes", vjust = "top", hjust="right"), size=3)
        #geom_text(aes(last(day_and_date),15,label = "15 Minutes", vjust = 1))
      }
      
      else if (days_count < 29) {
        for (i in 1:last) {
          mydf$Date[i]=mydf$Date[i]+ lubridate::days(0)
        }
        first_date=cut(first(as.Date(mydf$Date)), "week")
        first_date=paste(first_date, "00:00:00", sep = " ")
        first_date=as.POSIXct(first_date)
        
        last_week=last(as.Date(mydf$Date))+ lubridate::days(7)
        last_week=paste(last_week, "00:00:00", sep = " ")
        last_week=as.POSIXct(last_week)
        
        ggplot(mydf, aes(x=Date, y=Diff)) +
          scale_x_datetime(breaks = "1 week",minor_breaks = "1 weeks",labels = date_format("%W"),name="Date range (Week)")+
          #scale_x_date(labels=date_format("%a"))+
          #scale_x_date(breaks = "1 day",minor_breaks = "1 days",labels = date_format("%a\n%d"),name="")+
          theme_minimal() +
          theme(axis.text=element_text(size=10),
                axis.title=element_text(size=12,face="bold"))+
          #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
          geom_point() + xlab("Date range (Week)") + ylab("Time deviation (minutes)") +
          expand_limits(x=c(first_date,last_week))+
          #scale_y_continuous(breaks = seq(0, 10000, 5)) +
          #geom_segment(aes(x = 0, y = as.numeric(average1), xend = Inf, yend = as.numeric(average2)), size = 1, color="Blue", arrow = arrow(length = unit(0.5,"cm")))+
          geom_hline(yintercept=15, color = "Red", size = 1)+
          geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_week, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(week),15,label = "15 Minutes", vjust = 1))
      }
      
      else if (days_count > 28 & days_count < 366 ) {
        for (i in 1:last) {
          mydf$Date[i]=mydf$Date[i]%m+% lubridate:::months.numeric(0)
        }
        first_date=cut(first(as.Date(mydf$Date)), "month")
        first_date=paste(first_date, "00:00:00", sep = " ")
        first_date=as.POSIXct(first_date)
        
        last_month=last(as.Date(mydf$Date))%m+% lubridate:::months.numeric(1)
        last_month=paste(last_month, "00:00:00", sep = " ")
        last_month=as.POSIXct(last_month)
        
        ggplot(mydf, aes(x=Date, y=Diff)) +
          scale_x_datetime(breaks = "1 month",minor_breaks = "1 months",labels = date_format("%b"),name="Date range (Month)")+
          #scale_x_date(breaks = "1 day",minor_breaks = "1 days",labels = date_format("%a %d %b"),name="Date range")+
          theme_minimal() +
          theme(axis.text=element_text(size=10),
                axis.title=element_text(size=12,face="bold"))+
          #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
          geom_point() + xlab("Date range (Month)") + ylab("Time deviation (minutes)")+
          expand_limits(x=c(first_date,last_month))+
          #scale_y_continuous(breaks = seq(0, 10000, 5)) +
          #geom_segment(aes(x = 0, y = as.numeric(average1), xend = Inf, yend = as.numeric(average2)), size = 1, color="Blue", arrow = arrow(length = unit(0.5,"cm")))+
          geom_hline(yintercept=15, color = "Red", size = 1)+
          geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_month, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(month),15,label = "15 Minutes", vjust = 1))
      }
      
      else if (days_count > 365) {
        for (i in 1:last) {
          mydf$Date[i]=mydf$Date[i]%m+% lubridate::years(0)
        }
        first_date=cut(first(as.Date(mydf$Date)), "year")
        first_date=paste(first_date, "00:00:00", sep = " ")
        first_date=as.POSIXct(first_date)
        
        last_year=last(as.Date(mydf$Date))%m+% lubridate::years(1)
        last_year=paste(last_year, "00:00:00", sep = " ")
        last_year=as.POSIXct(last_year)
        
        ggplot(mydf, aes(x=Date, y=Diff)) +
          #scale_x_date(breaks = "1 day",minor_breaks = "1 days",labels = date_format("%a\n%d"),name="")+
          scale_x_datetime(breaks = "1 year",minor_breaks = "1 years",labels = date_format("%Y"),name="Date range (Year)")+
          theme_minimal() +
          theme(axis.text=element_text(size=10),
                axis.title=element_text(size=12,face="bold"))+
          #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
          geom_point() + xlab("Date range (Year)") + ylab("Time deviation (minutes)") +
          expand_limits(x=c(first_date,last_year))+
          #scale_y_continuous(breaks = seq(0, 10000, 5)) +
          #geom_segment(aes(x = 0, y = as.numeric(average1), xend = Inf, yend = as.numeric(average2)), size = 1, color="Blue", arrow = arrow(length = unit(0.5,"cm")))+
          geom_hline(yintercept=15, color = "Red", size = 1)+
          geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_year, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(year),15,label = "15 Minutes", vjust = 1))
      }
    }
    
  })
  
  output$plot10 <- renderPlot({
    
    days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
    if (days_count<1) {
      stop(safeError("End date should be after start date."))
      
    }
    
    #b=input$list_of_patients
    #if (b=="Select"||b==0) {
    # photo=paste(photos_directoy, "med_adh.png", sep="/")
    #pic = readPNG(photo)
    #plot.new()
    #return(grid::grid.raster(pic))
    #}
    #if (b!="Select"&&b!=0)
    #{
    #c=paste("p0", b, sep="")
    #if (as.numeric(b)>9) {
    # c=paste("p", b, sep="")
    #}
    #d=paste(c, "csv", sep = ".")
    #a=paste(adh_directory, d, sep = "/")
    
    #if (file.exists(a)) {
    # adhFile <- read.csv(a, stringsAsFactors = FALSE, sep="\t")
    #}
    #else
    # stop(safeError("Missing data file for the patient."))
    
    #filtered <-
    # adhFile %>%
    #filter(intake_time >= input$daterange1[1],
    #      intake_time <= input$daterange1[2])
    #filtered <- filtered[ ,c("diff_min", "intake_time")]
    #filtered <- unique(filtered)
    
    #a <- filtered[ ,c("intake_time")]
    #b <- filtered[ ,c("diff_min")]
    #mydf = data.frame(Date=a, Diff=b)
    
    deviceId=input$textInputDeviceId
    if (deviceId!="") {
      
      #deviceId= "f2cdccbde923a7bb"
      a=str_interp("${baseUrl}/${deviceId}/normalmedicationsgroups.json")
      #theJsonSymptoms <- fromJSON(a)
      
      tryCatch(
        {
          theJsonMedicationGroups <- fromJSON(a)
        },
        error= function(error_condition) {
          stop(safeError("Wrong patient id, please check patient id again."))
        }
      )
      if (is.null(theJsonMedicationGroups)) {
        stop(safeError("Wrong patient id, please check patient id again."))
      }
      
      
      ids <- NULL
      normalGroupNames <- NULL
      optimalTimes <- NULL
      takenDateTimes <- NULL
      timeDifferences <- NULL
      
      counter=1
      for (i in theJsonMedicationGroups) {
        #print("a")
        ids[counter]=theJsonMedicationGroups[[counter]]$id
        #['id']
        #print(ids[counter])
        normalGroupNames[counter]=theJsonMedicationGroups[[counter]]$normal_group_name
        optimalTimes[counter]=theJsonMedicationGroups[[counter]]$optimal_time
        takenDateTimes[counter]=theJsonMedicationGroups[[counter]]$taken_dateTime
        
        takenDateTimeString=as.POSIXct(takenDateTimes[counter], format="%Y-%m-%dT%H:%M:%S")
        
        optimalTimeDate<-paste(as.Date(takenDateTimes[counter]), optimalTimes[counter])
        
        timeDifference1=difftime(takenDateTimeString, optimalTimeDate, units = "mins")
        if (timeDifference1 < 0) {
          timeDifference1=timeDifference1+1440
        }
        
        timeDifference2=difftime(optimalTimeDate, takenDateTimeString, units = "mins")
        if (timeDifference2 < 0) {
          timeDifference2=timeDifference2+1440
        }
        
        if (timeDifference1 < timeDifference2) {
          timeDifferences[counter]=timeDifference1
        }
        if (timeDifference2 < timeDifference1) {
          timeDifferences[counter]=timeDifference2
        }
        if (timeDifference1 == timeDifference2) {
          timeDifferences[counter]=timeDifference1
        }
        
        counter=counter+1
      }
      
      mydf2=data.frame(id=ids,Date=takenDateTimes, Diff=timeDifferences,
                       GroupMedName=normalGroupNames, OptimalTimes=optimalTimes)
      #print("gg")
      #print(mydf2)
      
      
      date_error = as.numeric(input$daterange1[2]-as.Date(mydf2[c(1),"Date"]))+1
      if (date_error<1) {
        stop(safeError("No data for the patient in this time period."))
      }
      maximum=as.numeric(count(mydf2))
      date_error = as.numeric(as.Date(mydf2[c(maximum),"Date"])-input$daterange1[1])+1
      if (date_error<1) {
        stop(safeError("No data for the patient in this time period."))
      }
      
      mydf <-
        mydf2 %>%
        filter(Date >= input$daterange1[1],
               Date <= input$daterange1[2])
      
      if (as.numeric(count(mydf))==0) {
        stop(safeError("No data for the patient in this time period."))
      }
      
      
      #mydf$ddate <- factor(strftime(mydf$Date,format="%d"))
      
      #mydf$day <- factor(strftime(mydf$Date,format="%a"),
      #                  levels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
      
      #mydf$day <- factor(mydf$day,levels=unique(mydf$day))
      
      #mydf$week <- factor(strftime(mydf$Date,format="%V"))
      #mydf$week <- factor(mydf$week,levels=unique(mydf$week))
      
      #mydf$month <- strftime(mydf$Date,format="%b")
      #mydf$month <- factor(mydf$month,levels=unique(mydf$month))
      
      #mydf$year <- strftime(mydf$Date,format="%Y")
      #mydf$year <- factor(mydf$year,levels=unique(mydf$year))
      
      #mydf2 = mydf[!duplicated(mydf[ , c("ddate")]),]
      #days_count = count(mydf2)
      
      
      
      filtered2 <- mydf2
      filtered2 <- filtered2[ ,c("Diff", "Date")]
      filtered2 <- unique(filtered2)
      #print(filtered2)
      
      #print(count(filtered2))
      first_point_intake = filtered2[c(1),2]
      
      count = count(filtered2)
      count = as.numeric(count)
      
      one_count_flag=0
      if (count<1) {
        stop(safeError("Empty data file for the patient."))
      }
      last_point_intake = filtered2[c(count),2]
      if (count==1) {
        count=2
        one_count_flag=1
      }
      
      half_point_intake = round(count/2)
      half_point_intake = as.numeric(half_point_intake)
      half_point_intake = filtered2[c(half_point_intake),2]
      #print(half_point_intake)
      
      if (one_count_flag==1) {
        half_point_intake_plus_one = round(count/2)
      } else {
        half_point_intake_plus_one = round(count/2) + 1
      }
      
      half_point_intake_plus_one = as.numeric(half_point_intake_plus_one)
      half_point_intake_plus_one = filtered2[c(half_point_intake_plus_one),2]
      #print(half_point_intake_plus_one)
      
      first_half <- 
        filtered2  %>%
        filter(Date >= first_point_intake,
               Date <= half_point_intake)
      
      #print(first_half)
      average1 = mean(as.numeric(first_half$Diff))
      #print(average1)
      
      
      second_half <-
        filtered2  %>%
        filter(Date >= half_point_intake_plus_one,
               Date <= last_point_intake)
      
      #print(second_half)
      average2 = mean(as.numeric(second_half$Diff))
      #print(average2)
      
      
      #if (input$input3Id == TRUE) {
      # mydf$Date <- factor(mydf$Date, levels = mydf$Date[order(mydf$Diff)])
      #}
      
      last=as.numeric(count(mydf))
      for (i in 1:last) {
        #mydf$Date=as.Date(mydf$Date)
        mydf$Date=as.POSIXct(mydf$Date, format="%Y-%m-%dT%H:%M:%S")
        mydf$Date[i]=mydf$Date[i]+ lubridate::days(0)
        #day_and_date=paste(mydf$day, mydf$ddate, sep = " ")
        #mydf$day_and_date=paste(day_and_date, mydf$month, sep = " ")
      }
      #scaling=data.frame(asdf=unique(as.Date(mydf$Date)))
      #scale_count=count(scaling)
      scale_count=days_count
      Switch2=F
      Switch=F
      Switch3=F
      Switch4=F
      if (scale_count>90) {
        Switch4=T
        Switch2=T
      }
      if (scale_count>60 & scale_count<91) {
        Switch3=T
        Switch=T
      }
      if (scale_count>45 & scale_count<61) {
        Switch2=T
      }
      if (scale_count>7 & scale_count<46) {
        Switch=T
      }
      
      #print(scale_count)
      
      first_date=first(as.Date(mydf$Date))
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_date=last(as.Date(mydf$Date))+ lubridate::days(1)
      last_date=paste(last_date, "00:00:00", sep = " ")
      last_date=as.POSIXct(last_date)
      #print(mydf)
      ggplot(mydf, aes(x=Date, y=Diff))+ #trendline(x1,y1,model="line2P",summary=TRUE) #geom_count()
        #scale_x_date(breaks = "1 day",minor_breaks = "1 days",labels = date_format("%a %d %b"),name="Date range")+
        scale_x_datetime(breaks = "1 day", minor_breaks = "1 days",labels=date_format("%a %d %b"),name="Date range")+
        {if(Switch3)scale_x_datetime(breaks = "2 days", minor_breaks = "1 days",labels=date_format("%a %d %b"),name="Date range")}+
        {if(Switch4)scale_x_datetime(breaks = "7 days", minor_breaks = "1 days",labels=date_format("%a %d %b"),name="Date range")}+
        #geom_smooth(data = mydf, aes(x=game_id, y=Scores), method = lm, se = FALSE)
        theme_minimal() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        {if(Switch)theme(axis.text.x = element_text(angle = 45, hjust = 1))}+
        {if(Switch2)theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))}+
        #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
        geom_point() + xlab("Date range") + ylab("Time deviation (minutes)") +
        expand_limits(x=c(first_date,last_date))+
        #scale_y_continuous(breaks = seq(0, 10000, 5)) +
        #geom_segment(aes(x = 0, y = as.numeric(average1), xend = Inf, yend = as.numeric(average2)), size = 1, color="Blue", arrow = arrow(length = unit(1,"cm"))) +
        geom_hline(yintercept=15, color = "Red", size = 1)+
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_date, yend = as.numeric(average2)), size = 0.5, color="Blue")
      #geom_text(aes(last(day_and_date),15,label = "15 Minutes", vjust = 1))
      
    }
  })
  
  output$plot2 <- renderPlot({
    
    days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
    if (days_count<1) {
      stop(safeError("End date should be after start date."))
      
    }
    
    #b=input$list_of_patients
    #if (b=="Select"||b==0) {
    # photo=paste(photos_directoy, "symptoms.png", sep="/")
    #pic = readPNG(photo)
    #plot.new()
    #return(grid::grid.raster(pic))
    #}
    #if (b=="Select"||b==0)
    #{
    #c=paste("p0", b, sep="")
    #if (as.numeric(b)>9) {
    # c=paste("p", b, sep="")
    #}
    #d=paste(c, "csv", sep = ".")
    #a=paste(pd_directory, d, sep = "/")
    
    #if (file.exists(a)) {
    # pdFile <- read.csv(a, stringsAsFactors = FALSE, sep="\t")
    #}
    #else
    # stop(safeError("Missing data file for the patient."))
    
    
    #filtered <-
    #pdFile %>%
    #filter(date_time >= input$daterange1[1],
    #        date_time <= input$daterange1[2])
    #filtered <- filtered[ ,c("pd_value", "date_time")]
    #filtered <- unique(filtered)
    
    #a <- filtered[ ,c("date_time")]
    #b <- filtered[ ,c("pd_value")]
    #mydf = data.frame(Date=a, PD=b)
    deviceId=input$textInputDeviceId
    if (deviceId!="") {
      
      #deviceId= "f2cdccbde923a7bb"
      a=str_interp("${baseUrl}/${deviceId}/symptoms.json")
      #theJsonSymptoms <- fromJSON(a)
      
      tryCatch(
        {
          theJsonSymptoms <- fromJSON(a)
        },
        error= function(error_condition) {
          stop(safeError("Wrong patient id, please check patient id again."))
        }
      )
      if (is.null(theJsonSymptoms)) {
        stop(safeError("Wrong patient id, please check patient id again."))
      }
      
      ids <- NULL
      dateTimes <- NULL
      symptoms <- NULL
      tags <- NULL
      
      counter=1
      for (i in theJsonSymptoms) {
        ids[counter]=theJsonSymptoms[[counter]]$id
        dateTimes[counter]=theJsonSymptoms[[counter]]$dateTime
        symptoms[counter]=theJsonSymptoms[[counter]]$symptom
        tags[counter]=theJsonSymptoms[[counter]]$tag
        counter=counter+1
      }
      
      
      mydf2=data.frame(id=ids, Date=dateTimes, PD=symptoms, tag=tags)
      mydf2$PD[mydf2$PD == "No symptoms"] <- (0)
      mydf2$PD[mydf2$PD == "Light symptoms"] <- (1)
      mydf2$PD[mydf2$PD == "Mild symptoms"] <- (2)
      mydf2$PD[mydf2$PD == "Strong symptoms"] <- (3)
      mydf2$PD[mydf2$PD == "Severe symptoms"] <- (4)
      
      #print(ids)
      
      #print(mydf2)
      
      
      date_error = as.numeric(input$daterange1[2]-as.Date(mydf2[c(1),"Date"]))+1
      if (date_error<1) {
        stop(safeError("No data for the patient in this time period."))
      }
      maximum=as.numeric(count(mydf2))
      date_error = as.numeric(as.Date(mydf2[c(maximum),"Date"])-input$daterange1[1])+1
      if (date_error<1) {
        stop(safeError("No data for the patient in this time period."))
      }
      
      mydf <-
        mydf2 %>%
        filter(Date >= input$daterange1[1],
               Date <= input$daterange1[2])
      
      if (as.numeric(count(mydf))==0) {
        stop(safeError("No data for the patient in this time period."))
      }
      
      #print("a")
      
      
      ###mydf$ddate <- factor(strftime(mydf$Date,format="%d"))
      
      ###mydf$day <- factor(strftime(mydf$Date,format="%a"),
      ###                levels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
      
      ###mydf$day <- factor(mydf$day,levels=unique(mydf$day))
      
      ###mydf$week <- factor(strftime(mydf$Date,format="%V"))
      ###mydf$week <- factor(mydf$week,levels=unique(mydf$week))
      
      ###mydf$month <- strftime(mydf$Date,format="%b")
      ###mydf$month <- factor(mydf$month,levels=unique(mydf$month))
      
      ###mydf$year <- strftime(mydf$Date,format="%Y")
      ###mydf$year <- factor(mydf$year,levels=unique(mydf$year))
      
      #mydf2 = mydf[!duplicated(mydf[ , c("ddate")]),]
      #days_count = count(mydf2)
      
      
      
      
      filtered2 <- mydf2
      filtered2 <- filtered2[ ,c("PD", "Date")]
      filtered2 <- unique(filtered2)
      #print(filtered2)
      
      
      
      
      first_point_intake = filtered2[c(1),2]
      
      count = count(filtered2)
      count = as.numeric(count)
      one_count_flag=0
      if (count<1) {
        stop(safeError("Empty data file for the patient."))
      }
      last_point_intake = filtered2[c(count),2]
      if (count==1) {
        count=2
        one_count_flag=1
      }
      
      half_point_intake = round(count/2)
      half_point_intake = as.numeric(half_point_intake)
      half_point_intake = filtered2[c(half_point_intake),2]
      #print(half_point_intake)
      
      if (one_count_flag==1) {
        half_point_intake_plus_one = round(count/2)
      } else {
        half_point_intake_plus_one = round(count/2) + 1
      }
      
      half_point_intake_plus_one = as.numeric(half_point_intake_plus_one)
      half_point_intake_plus_one = filtered2[c(half_point_intake_plus_one),2]
      #print(half_point_intake_plus_one)
      
      first_half <- 
        filtered2  %>%
        filter(Date >= first_point_intake,
               Date <= half_point_intake)
      
      
      #print("aaa")
      #print(first_half[["PD"]])
      average1 = mean(as.numeric(first_half$PD))
      #print(average1)
      #print("aaaa")
      
      
      second_half <-
        filtered2  %>%
        filter(Date >= half_point_intake_plus_one,
               Date <= last_point_intake)
      
      #print(second_half)
      average2 = mean(as.numeric(second_half$PD))
      #print(average2)
      
      #print("c")
      #print(mydf)
      
      #if (input$input4Id == TRUE) {
      # mydf$Date <- factor(mydf$Date, levels = mydf$Date[order(mydf$PD)])
      #}
      #cc=as.numeric(count(mydf))
      #print(cc)
      
      ###mydf$`Frequency` <- c(1)
      ###mydf$`Frequency ` <- c(1)
      ###mydf$`Frequency  ` <- c(1)
      ###mydf$`Frequency   ` <- c(1)
      
      #print(mydf)
      
      ###mydf_day_count = data.frame(PD = mydf$PD, day=mydf$day)
      ###mydf_day_count$count <- c(1)
      ###mydf_day_count$flag <- c(0)
      ###last=as.numeric(count(mydf_day_count))
      ###if (last>1) {
      ###last_minus_one=as.numeric(count(mydf_day_count))-1
      ###for (i in 1:last_minus_one) {
      ###for (j in (i+1):last) {
      ###   if (as.numeric(mydf_day_count$day[i])==as.numeric(mydf_day_count$day[j]) && as.numeric(mydf_day_count$PD[i])==as.numeric(mydf_day_count$PD[j]) && as.numeric((mydf_day_count$flag[i])==0) && as.numeric((mydf_day_count$flag[j])==0)) {
      ###     mydf$`Frequency`[i]=mydf$`Frequency`[i]+1
      ###     mydf$`Frequency`[j]=mydf$`Frequency`[j]-1
      ###     mydf_day_count$flag[j]=mydf_day_count$flag[j]+1
      ###   }
      ### }
      ###}
      ###}
      
      
      
      ###mydf_week_count = data.frame(PD = mydf$PD, week=mydf$week)
      ###mydf_week_count$count <- c(1)
      ###mydf_week_count$flag <- c(0)
      ###last=as.numeric(count(mydf_week_count))
      ###if (last>1) {
      ###last_minus_one=as.numeric(count(mydf_week_count))-1
      ###for (i in 1:last_minus_one) {
      ###  for (j in (i+1):last) {
      ###   if (as.numeric(mydf_week_count$week[i])==as.numeric(mydf_week_count$week[j]) && as.numeric(mydf_week_count$PD[i])==as.numeric(mydf_week_count$PD[j]) && as.numeric((mydf_week_count$flag[i])==0) && as.numeric((mydf_week_count$flag[j])==0)) {
      ###     mydf$`Frequency `[i]=mydf$`Frequency `[i]+1
      ###     mydf$`Frequency `[j]=mydf$`Frequency `[j]-1
      ###     mydf_week_count$flag[j]=mydf_week_count$flag[j]+1
      ###   }
      ### }
      ###}
      ###}
      
      
      
      
      ###mydf_month_count = data.frame(PD = mydf$PD, month=mydf$month)
      ###mydf_month_count$count <- c(1)
      ###mydf_month_count$flag <- c(0)
      ###last=as.numeric(count(mydf_month_count))
      ###if (last>1) {
      ###last_minus_one=as.numeric(count(mydf_month_count))-1
      ###for (i in 1:last_minus_one) {
      ### for (j in (i+1):last) {
      ###   if (as.numeric(mydf_month_count$month[i])==as.numeric(mydf_month_count$month[j]) && as.numeric(mydf_month_count$PD[i])==as.numeric(mydf_month_count$PD[j]) && as.numeric((mydf_month_count$flag[i])==0) && as.numeric((mydf_month_count$flag[j])==0)) {
      ###     mydf$`Frequency  `[i]=mydf$`Frequency  `[i]+1
      ###     mydf$`Frequency  `[j]=mydf$`Frequency  `[j]-1
      ###     mydf_month_count$flag[j]=mydf_month_count$flag[j]+1
      ###   }
      ### }
      ###}
      ###}
      
      ###mydf_year_count = data.frame(PD = mydf$PD, year=mydf$year)
      ###mydf_year_count$count <- c(1)
      ###mydf_year_count$flag <- c(0)
      ###last=as.numeric(count(mydf_year_count))
      ###if (last>1) {
      ###last_minus_one=as.numeric(count(mydf_year_count))-1
      ###for (i in 1:last_minus_one) {
      ### for (j in (i+1):last) {
      ###   if (as.numeric(mydf_year_count$year[i])==as.numeric(mydf_year_count$year[j]) && as.numeric(mydf_year_count$PD[i])==as.numeric(mydf_year_count$PD[j]) && as.numeric((mydf_year_count$flag[i])==0) && as.numeric((mydf_year_count$flag[j])==0)) {
      ###     mydf$`Frequency   `[i]=mydf$`Frequency   `[i]+1
      ###     mydf$`Frequency   `[j]=mydf$`Frequency   `[j]-1
      ###     mydf_year_count$flag[j]=mydf_year_count$flag[j]+1
      ###   }
      ### }
      ###}
      ###}
      
      last=as.numeric(count(mydf))
      for (i in 1:last) {
        #mydf$Date=as.Date(mydf$Date)
        mydf$Date=as.POSIXct(mydf$Date, format="%Y-%m-%dT%H:%M:%S")
        #day_and_date=paste(mydf$day, mydf$ddate, sep = "\n")
        #mydf$day_and_date=paste(day_and_date, mydf$month, sep = "\n")
      }
      #print("d")
      #print(mydf)
      if (days_count < 8) {
        for (i in 1:last) {
          mydf$Date[i]=mydf$Date[i]+ lubridate::days(0)
        }
        first_date=first(as.Date(mydf$Date))
        first_date=paste(first_date, "00:00:00", sep = " ")
        first_date=as.POSIXct(first_date)
        
        last_day=last(as.Date(mydf$Date))+ lubridate::days(1)
        last_day=paste(last_day, "00:00:00", sep = " ")
        last_day=as.POSIXct(last_day)
        
        #mydf=mydf[mydf$`Frequency`!=0, ]
        #, color=`Frequency` in aes()
        ggplot(mydf, aes(x=Date, y=as.numeric(PD)))+ #trendline(x1,y1,model="line2P",summary=TRUE) #geom_count()
          scale_x_datetime(breaks = "1 day", minor_breaks = "1 days", labels = date_format("%a\n%d\n%b"),name="Date range (Day)")+
          #scale_x_date(breaks = "1 day",minor_breaks = "1 days",labels = date_format("%a\n%d\n%b"),name="Day")+
          #geom_smooth(data = mydf, aes(x=game_id, y=Scores), method = lm, se = FALSE)
          theme_minimal() +
          theme(axis.text=element_text(size=10),
                axis.title=element_text(size=12,face="bold"))+
          geom_point() + xlab("Date range (Day)") + ylab("Score") +
          #scale_y_continuous(breaks = seq(0, 4, 1)) +
          scale_y_continuous(breaks = c(0,1,2,3,4), labels=c("0" = "No Symptoms", "1" = "Light Symptoms","2" = "Mild Symptoms","3" = "Strong Symptoms","4" = "Severe Symptoms") ) +
          expand_limits(y=c(0,4))+
          #annotate("text", x = last(mydf$day), y = (as.numeric(average2)), label = "Trend line of ratings")+
          geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_day, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(day_and_date),as.numeric(average2),label = "Trend line of ratings", vjust = "top", hjust="right"), size=3)
      }
      
      else if (days_count < 29) {
        for (i in 1:last) {
          mydf$Date[i]=mydf$Date[i]+ lubridate::days(0)
        }
        first_date=cut(first(as.Date(mydf$Date)), "week")
        first_date=paste(first_date, "00:00:00", sep = " ")
        first_date=as.POSIXct(first_date)
        
        last_week=last(as.Date(mydf$Date))+ lubridate::days(7)
        last_week=paste(last_week, "00:00:00", sep = " ")
        last_week=as.POSIXct(last_week)
        
        #mydf=mydf[mydf$`Frequency `!=0, ]
        #, color=`Frequency ` in aes()
        #print("e")
        #print(mydf)
        ggplot(mydf, aes(x=Date, y=as.numeric(PD))) +
          scale_x_datetime(breaks = "1 week",minor_breaks = "1 weeks",labels = date_format("%W"),name="Date range (Week)")+
          theme_minimal() +
          theme(axis.text=element_text(size=10),
                axis.title=element_text(size=12,face="bold"))+
          geom_point() + xlab("Date range (Week)") + ylab("Score") +
          #scale_y_continuous(breaks = seq(0, 4, 1)) +
          scale_y_continuous(breaks = c(0,1,2,3,4),labels=c("0" = "No Symptoms", "1" = "Light Symptoms","2" = "Mild Symptoms","3" = "Strong Symptoms","4" = "Severe Symptoms") ) +
          expand_limits(y=c(0,4))+
          geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_week, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(week),as.numeric(average2),label = "Trend line of ratings", vjust = "top", hjust="right"), size=3)
      }
      
      else if (days_count > 28 & days_count < 366 ) {
        for (i in 1:last) {
          mydf$Date[i]=mydf$Date[i]%m+% lubridate:::months.numeric(0)
        }
        first_date=cut(first(as.Date(mydf$Date)), "month")
        first_date=paste(first_date, "00:00:00", sep = " ")
        first_date=as.POSIXct(first_date)
        
        last_month=last(as.Date(mydf$Date))%m+% lubridate:::months.numeric(1)
        last_month=paste(last_month, "00:00:00", sep = " ")
        last_month=as.POSIXct(last_month)
        
        #mydf=mydf[mydf$`Frequency  `!=0, ]
        #, color=`Frequency  ` in aes()
        ggplot(mydf, aes(x=Date, y=as.numeric(PD))) +
          scale_x_datetime(breaks = "1 month",minor_breaks = "1 months",labels = date_format("%b"),name="Date range (Month)")+
          theme_minimal() +
          theme(axis.text=element_text(size=10),
                axis.title=element_text(size=12,face="bold"))+
          geom_point() + xlab("Date range (Month)") + ylab("Score")+
          #scale_y_continuous(breaks = seq(0, 4, 1)) +
          scale_y_continuous(breaks = c(0,1,2,3,4),labels=c("0" = "No Symptoms", "1" = "Light Symptoms","2" = "Mild Symptoms","3" = "Strong Symptoms","4" = "Severe Symptoms") ) +
          expand_limits(y=c(0,4))+
          geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_month, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(month),as.numeric(average2),label = "Trend line of ratings", vjust = "top", hjust="right"), size=3)
      }
      
      else if (days_count > 365) {
        for (i in 1:last) {
          mydf$Date[i]=mydf$Date[i]%m+% lubridate::years(0)
        }
        first_date=cut(first(as.Date(mydf$Date)), "year")
        first_date=paste(first_date, "00:00:00", sep = " ")
        first_date=as.POSIXct(first_date)
        
        last_year=last(as.Date(mydf$Date))%m+% lubridate::years(1)
        last_year=paste(last_year, "00:00:00", sep = " ")
        last_year=as.POSIXct(last_year)
        
        #mydf=mydf[mydf$`Frequency   `!=0, ]
        #, color=`Frequency   ` in aes()
        ggplot(mydf, aes(x=Date, y=as.numeric(PD))) +
          scale_x_datetime(breaks = "1 year",minor_breaks = "1 years",labels = date_format("%Y"),name="Date range (Year)")+
          theme_minimal() +
          theme(axis.text=element_text(size=10),
                axis.title=element_text(size=12,face="bold"))+
          geom_point() + xlab("Date range (Year)") + ylab("Score") +
          #scale_y_continuous(breaks = seq(0, 4, 1)) +
          scale_y_continuous(breaks = c(0,1,2,3,4),labels=c("0" = "No Symptoms", "1" = "Light Symptoms","2" = "Mild Symptoms","3" = "Strong Symptoms","4" = "Severe Symptoms") ) +
          expand_limits(y=c(0,4))+
          geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_year, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(year),as.numeric(average2),label = "Trend line of ratings", vjust = "top", hjust="right"), size=3)
      }
    }
  })
  
  output$plot20 <- renderPlot({
    
    days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
    if (days_count<1) {
      stop(safeError("End date should be after start date."))
      
    }
    
    #b=input$list_of_patients
    #if (b=="Select"||b==0) {
    # photo=paste(photos_directoy, "symptoms.png", sep="/")
    #pic = readPNG(photo)
    #plot.new()
    #return(grid::grid.raster(pic))
    #}
    #if (b!="Select"&&b!=0)
    #{
    #c=paste("p0", b, sep="")
    #if (as.numeric(b)>9) {
    # c=paste("p", b, sep="")
    #}
    #d=paste(c, "csv", sep = ".")
    #a=paste(pd_directory, d, sep = "/")
    
    #if (file.exists(a)) {
    # pdFile <- read.csv(a, stringsAsFactors = FALSE, sep="\t")
    #}
    #else
    # stop(safeError("Missing data file for the patient."))
    
    
    #filtered <-
    # pdFile %>%
    #filter(date_time >= input$daterange1[1],
    #      date_time <= input$daterange1[2])
    #filtered <- filtered[ ,c("pd_value", "date_time")]
    #filtered <- unique(filtered)
    
    #a <- filtered[ ,c("date_time")]
    #b <- filtered[ ,c("pd_value")]
    #mydf = data.frame(Date=a, PD=b)
    
    deviceId=input$textInputDeviceId
    if (deviceId!="") {
      
      a=str_interp("${baseUrl}/${deviceId}/symptoms.json")
      
      #theJsonSymptoms <- fromJSON(a)
      tryCatch(
        {
          theJsonSymptoms <- fromJSON(a)
        },
        error= function(error_condition) {
          stop(safeError("Wrong patient id, please check patient id again."))
        }
      )
      if (is.null(theJsonSymptoms)) {
        stop(safeError("Wrong patient id, please check patient id again."))
      }
      
      ids <- NULL
      dateTimes <- NULL
      symptoms <- NULL
      tags <- NULL
      
      counter=1
      for (i in theJsonSymptoms) {
        ids[counter]=theJsonSymptoms[[counter]]$id
        dateTimes[counter]=theJsonSymptoms[[counter]]$dateTime
        symptoms[counter]=theJsonSymptoms[[counter]]$symptom
        tags[counter]=theJsonSymptoms[[counter]]$tag
        counter=counter+1
      }
      
      mydf2=data.frame(id=ids, Date=dateTimes, PD=symptoms, tag=tags)
      mydf2$PD[mydf2$PD == "No symptoms"] <- (0)
      mydf2$PD[mydf2$PD == "Light symptoms"] <- (1)
      mydf2$PD[mydf2$PD == "Mild symptoms"] <- (2)
      mydf2$PD[mydf2$PD == "Strong symptoms"] <- (3)
      mydf2$PD[mydf2$PD == "Severe symptoms"] <- (4)
      
      
      date_error = as.numeric(input$daterange1[2]-as.Date(mydf2[c(1),"Date"]))+1
      if (date_error<1) {
        stop(safeError("No data for the patient in this time period."))
      }
      maximum=as.numeric(count(mydf2))
      date_error = as.numeric(as.Date(mydf2[c(maximum),"Date"])-input$daterange1[1])+1
      if (date_error<1) {
        stop(safeError("No data for the patient in this time period."))
      }
      
      mydf <-
        mydf2 %>%
        filter(Date >= input$daterange1[1],
               Date <= input$daterange1[2])
      
      if (as.numeric(count(mydf))==0) {
        stop(safeError("No data for the patient in this time period."))
      }
      
      
      
      
      #mydf$ddate <- factor(strftime(mydf$Date,format="%d"))
      
      #mydf$day <- factor(strftime(mydf$Date,format="%a"),
      #                  levels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
      
      #mydf$day <- factor(mydf$day,levels=unique(mydf$day))
      
      #mydf$week <- factor(strftime(mydf$Date,format="%V"))
      #mydf$week <- factor(mydf$week,levels=unique(mydf$week))
      
      #mydf$month <- strftime(mydf$Date,format="%b")
      #mydf$month <- factor(mydf$month,levels=unique(mydf$month))
      
      #mydf$year <- strftime(mydf$Date,format="%Y")
      #mydf$year <- factor(mydf$year,levels=unique(mydf$year))
      #mydf2 = mydf[!duplicated(mydf[ , c("ddate")]),]
      #days_count = count(mydf2)
      
      
      
      
      filtered2 <- mydf2
      filtered2 <- filtered2[ ,c("PD", "Date")]
      filtered2 <- unique(filtered2)
      #print(filtered2)
      
      #print(count(filtered2))
      first_point_intake = filtered2[c(1),2]
      
      count = count(filtered2)
      count = as.numeric(count)
      one_count_flag=0
      if (count<1) {
        stop(safeError("Empty data file for the patient."))
      }
      last_point_intake = filtered2[c(count),2]
      if (count==1) {
        count=2
        one_count_flag=1
      }
      
      half_point_intake = round(count/2)
      half_point_intake = as.numeric(half_point_intake)
      half_point_intake = filtered2[c(half_point_intake),2]
      #print(half_point_intake)
      
      if (one_count_flag==1) {
        half_point_intake_plus_one = round(count/2)
      } else {
        half_point_intake_plus_one = round(count/2) + 1
      }
      
      half_point_intake_plus_one = as.numeric(half_point_intake_plus_one)
      half_point_intake_plus_one = filtered2[c(half_point_intake_plus_one),2]
      #print(half_point_intake_plus_one)
      
      first_half <- 
        filtered2  %>%
        filter(Date >= first_point_intake,
               Date <= half_point_intake)
      
      #print(first_half)
      average1 = mean(as.numeric(first_half$PD))
      #print(average1)
      
      
      second_half <-
        filtered2  %>%
        filter(Date >= half_point_intake_plus_one,
               Date <= last_point_intake)
      
      #print(second_half)
      average2 = mean(as.numeric(second_half$PD))
      #print(average2)
      #print(mydf)
      
      #if (input$input4Id == TRUE) {
      # mydf$Date <- factor(mydf$Date, levels = mydf$Date[order(mydf$PD)])
      #}
      #cc=as.numeric(count(mydf))
      #print(cc)
      
      ###mydf$`Frequency` <- c(1)
      #mydf$`Frequency ` <- c(1)
      #mydf$`Frequency  ` <- c(1)
      #mydf$`Frequency   ` <- c(1)
      
      last=as.numeric(count(mydf))
      for (i in 1:last) {
        #mydf$Date=as.Date(mydf$Date)
        mydf$Date=as.POSIXct(mydf$Date, format="%Y-%m-%dT%H:%M:%S")
        mydf$Date[i]=mydf$Date[i]+ lubridate::days(0)
        #day_and_date=paste(mydf$day, mydf$ddate, sep = " ")
        #mydf$day_and_date=paste(day_and_date, mydf$month, sep = " ")
      }
      
      ###mydf_Date_count = data.frame(PD = mydf$PD, Date=mydf$Date)
      ###mydf_Date_count$count <- c(1)
      ###mydf_Date_count$flag <- c(0)
      ###last=as.numeric(count(mydf_Date_count))
      ###if (last>1) {
      ###last_minus_one=as.numeric(count(mydf_Date_count))-1
      ###for (i in 1:last_minus_one) {
      ### for (j in (i+1):last) {
      ###   if (mydf_Date_count$Date[i]==mydf_Date_count$Date[j] && mydf_Date_count$PD[i]==mydf_Date_count$PD[j] && (mydf_Date_count$flag[i])==0 && (mydf_Date_count$flag[j])==0) {
      ###     mydf$`Frequency`[i]=mydf$`Frequency`[i]+1
      ###     mydf$`Frequency`[j]=mydf$`Frequency`[j]-1
      ###     mydf_Date_count$flag[j]=mydf_Date_count$flag[j]+1
      ###   }
      ### }
      ###}
      ###}
      #print(mydf)
      #scaling=data.frame(asdf=unique(as.Date(mydf$Date)))
      #scale_count=count(scaling)
      scale_count=days_count
      Switch2=F
      Switch=F
      Switch3=F
      Switch4=F
      if (scale_count>90) {
        Switch4=T
        Switch2=T
      }
      if (scale_count>60 & scale_count<91) {
        Switch3=T
        Switch=T
      }
      if (scale_count>45 & scale_count<61) {
        Switch2=T
      }
      if (scale_count>7 & scale_count<46) {
        Switch=T
      }
      
      
      first_date=first(as.Date(mydf$Date))
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_date=last(as.Date(mydf$Date))+ lubridate::days(1)
      last_date=paste(last_date, "00:00:00", sep = " ")
      last_date=as.POSIXct(last_date)
      
      
      #mydf=mydf[mydf$`Frequency`!=0, ]
      #, color=`Frequency` in aes()
      ggplot(mydf, aes(x=Date, y=as.numeric(PD)))+ #trendline(x1,y1,model="line2P",summary=TRUE) #geom_count()
        #geom_smooth(data = mydf, aes(x=game_id, y=Scores), method = lm, se = FALSE)
        #scale_x_date(breaks = "1 day",minor_breaks = "1 days",labels = date_format("%a %d %b"),name="Date range")+
        scale_x_datetime(breaks = "1 day", minor_breaks = "1 days",labels=date_format("%a %d %b"),name="Date range")+
        {if(Switch3)scale_x_datetime(breaks = "2 days", minor_breaks = "1 days",labels=date_format("%a %d %b"),name="Date range")}+
        {if(Switch4)scale_x_datetime(breaks = "7 days", minor_breaks = "1 days",labels=date_format("%a %d %b"),name="Date range")}+
        theme_minimal() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        {if(Switch)theme(axis.text.x = element_text(angle = 45, hjust = 1))}+
        {if(Switch2)theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))}+
        geom_point() + xlab("Date range") + ylab("Score") +
        #scale_y_continuous(breaks = seq(0, 4, 1)) +
        scale_y_continuous(breaks = c(0,1,2,3,4), labels=c("0" = "No Symptoms", "1" = "Light Symptoms","2" = "Mild Symptoms","3" = "Strong Symptoms","4" = "Severe Symptoms") ) +
        expand_limits(y=c(0,4))+
        #annotate("text", x = last(mydf$day), y = (as.numeric(average2)), label = "Trend line of ratings")+
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_date, yend = as.numeric(average2)), size = 0.5, color="Blue")
      #geom_text(aes(last(day_and_date),as.numeric(average2),label = "Trend line of ratings", vjust = "top", hjust="right"), size=3)
      
    }
  })
  
  
  
  
  
  
  
  output$plot6 <- renderPlot({
    
    days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
    if (days_count<1) {
      stop(safeError("End date should be after start date."))
      
    }
    
    
    deviceId=input$textInputDeviceId
    if (deviceId!="")
    {
      
      
      a=str_interp("${baseUrl}/${deviceId}/games.json")
      #theJsonSymptoms <- fromJSON(a)
      
      tryCatch(
        {
          theJsonGames <- fromJSON(a)
        },
        error= function(error_condition) {
          stop(safeError("Wrong patient id, please check patient id again."))
        }
      )
      if (is.null(theJsonGames)) {
        stop(safeError("Wrong patient id, please check patient id again."))
      }
      
      #print(theJsonGames)
      
      
      ids <- NULL
      dateTimes <- NULL
      scores <- NULL
      levels <- NULL
      
      counter=1
      for (i in theJsonGames) {
        ids[counter]=theJsonGames[[counter]]$id
        dateTimes[counter]=theJsonGames[[counter]]$dateTime
        scores[counter]=theJsonGames[[counter]]$score
        levels[counter]=theJsonGames[[counter]]$level
        counter=counter+1
      }
      
      
      
      mydf2=data.frame(id=ids, Date=dateTimes, Scores=scores, Levels=levels)
      #print(mydf2)
      mydf2$Scores<-(as.double(mydf2$Scores))
      
      #print(mydf2)
      
      
      date_error = as.numeric(input$daterange1[2]-as.Date(mydf2[c(1),"Date"]))+1
      if (date_error<1) {
        stop(safeError("No data for the patient in this time period."))
      }
      maximum=as.numeric(count(mydf2))
      date_error = as.numeric(as.Date(mydf2[c(maximum),"Date"])-input$daterange1[1])+1
      if (date_error<1) {
        stop(safeError("No data for the patient in this time period."))
      }
      
      #a <- filtered[ ,c("date_time")]
      #b <- filtered[ ,c("score")]
      #mydf = data.frame(Date=a, Scores=b)
      
      mydf <-
        mydf2 %>%
        filter(Date >= input$daterange1[1],
               Date <= input$daterange1[2])
      
      if (as.numeric(count(mydf))==0) {
        stop(safeError("No data for the patient in this time period."))
      }
      
      
      
      
      
      filtered2 <- mydf2
      filtered2 <- filtered2[ ,c("Scores", "Date")]
      filtered2 <- unique(filtered2)
      
      first_point_intake = filtered2[c(1),2]
      
      count = count(filtered2)
      count = as.numeric(count)
      one_count_flag=0
      if (count<1) {
        stop(safeError("Empty data file for the patient."))
      }
      last_point_intake = filtered2[c(count),2]
      if (count==1) {
        count=2
        one_count_flag=1
      }
      
      half_point_intake = round(count/2)
      half_point_intake = as.numeric(half_point_intake)
      half_point_intake = filtered2[c(half_point_intake),2]
      #print(half_point_intake)
      
      if (one_count_flag==1) {
        half_point_intake_plus_one = round(count/2)
      } else {
        half_point_intake_plus_one = round(count/2) + 1
      }
      
      half_point_intake_plus_one = as.numeric(half_point_intake_plus_one)
      half_point_intake_plus_one = filtered2[c(half_point_intake_plus_one),2]
      #print(half_point_intake_plus_one)
      
      first_half <- 
        filtered2  %>%
        filter(Date >= first_point_intake,
               Date <= half_point_intake)
      #first_half=filtered2
      #first_half$Date[first_half$Date>= first_point_intake]
      #first_half$Date[first_half$Date<= half_point_intake]
      #print(first_half)
      
      #print("aaa")
      #print(first_half[["PD"]])
      average1 = mean(as.numeric(first_half$Scores))
      #print(average1)
      #print("aaaa")
      
      
      second_half <-
        filtered2  %>%
        filter(Date >= half_point_intake_plus_one,
               Date <= last_point_intake)
      
      #print(second_half)
      average2 = mean(as.numeric(second_half$Scores))
      #print(average2)
      
      
      
      
      last=as.numeric(count(mydf))
      for (i in 1:last) {
        #mydf$Date=as.Date(mydf$Date)
        mydf$Date=as.POSIXct(mydf$Date, format="%Y-%m-%dT%H:%M:%S")
        #mydf$Date[i]=mydf$Date[i]+ lubridate::days(1)
        #mydf$Date[i]=mydf$Date[i]%m+% lubridate:::months.numeric(1)
        #day_and_date=paste(mydf$day, mydf$ddate, sep = "\n")
        #mydf$day_and_date=paste(day_and_date, mydf$month, sep = "\n")
      }
      
      
      if (days_count < 8) {
        for (i in 1:last) {
          mydf$Date[i]=mydf$Date[i]+ lubridate::days(0)
        }
        first_date=first(as.Date(mydf$Date))
        first_date=paste(first_date, "00:00:00", sep = " ")
        first_date=as.POSIXct(first_date)
        
        last_day=last(as.Date(mydf$Date))+ lubridate::days(1)
        last_day=paste(last_day, "00:00:00", sep = " ")
        last_day=as.POSIXct(last_day)
        #print(mydf)
        ggplot(mydf, aes(x=Date, y=Scores))+ #trendline(x1,y1,model="line2P",summary=TRUE) #geom_count()
          scale_x_datetime(breaks = "1 day", minor_breaks = "1 days", labels = date_format("%a\n%d\n%b"),name="Date range (Day)")+
          #geom_smooth(data = mydf, aes(x=game_id, y=Scores), method = lm, se = FALSE)
          theme_light() +
          theme(axis.text=element_text(size=10),
                axis.title=element_text(size=12,face="bold"))+
          #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
          geom_point() + xlab("Date range (Day)") + ylab("Game Score (0-100)") +
          #scale_y_continuous(breaks = seq(0, 100, 2)) +
          geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_day, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(day_and_date),as.numeric(average2),label = "Trend line of scores", vjust = "top", hjust="right"), size=3)
      }
      
      else if (days_count < 29) {
        for (i in 1:last) {
          mydf$Date[i]=mydf$Date[i]+ lubridate::days(0)
        }
        first_date=cut(first(as.Date(mydf$Date)), "week")
        first_date=paste(first_date, "00:00:00", sep = " ")
        first_date=as.POSIXct(first_date)
        
        last_week=last(as.Date(mydf$Date))+ lubridate::days(7)
        last_week=paste(last_week, "00:00:00", sep = " ")
        last_week=as.POSIXct(last_week)
        ggplot(mydf, aes(x=Date, y=Scores)) +
          scale_x_datetime(breaks = "1 week",minor_breaks = "1 weeks",labels = date_format("%W"),name="Date range (Week)")+
          theme_light() +
          theme(axis.text=element_text(size=10),
                axis.title=element_text(size=12,face="bold"))+
          geom_point() + xlab("Date range (Week)") + ylab("Game Score (0-100)") +
          #scale_y_continuous(breaks = seq(0, 100, 2)) +
          geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_week, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(week),as.numeric(average2),label = "Trend line of scores", vjust = "top", hjust="right"), size=3)
      }
      #expand = c(0, 0), put in scalexdatetime
      else if (days_count > 28 & days_count < 366 ) {
        for (i in 1:last) {
          mydf$Date[i]=mydf$Date[i]%m+% lubridate:::months.numeric(0)
        }
        first_date=cut(first(as.Date(mydf$Date)), "month")
        first_date=paste(first_date, "00:00:00", sep = " ")
        first_date=as.POSIXct(first_date)
        
        last_month=last(as.Date(mydf$Date))%m+% lubridate:::months.numeric(1)
        last_month=paste(last_month, "00:00:00", sep = " ")
        last_month=as.POSIXct(last_month)
        #print(mydf)
        ggplot(mydf, aes(x=Date, y=Scores)) +
          scale_x_datetime(breaks = "1 month",minor_breaks = "1 months",labels = date_format("%b"),name="Date range (Month)")+
          theme_light() +
          theme(axis.text=element_text(size=10),
                axis.title=element_text(size=12,face="bold"))+
          #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
          geom_point() + xlab("Date range (Month)") + ylab("Game Score (0-100)")+
          #scale_y_continuous(breaks = seq(0, 100, 2)) +
          geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_month, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(month),as.numeric(average2),label = "Trend line of scores", vjust = "top", hjust="right"), size=3)
      }
      
      else if (days_count > 365) {
        for (i in 1:last) {
          mydf$Date[i]=mydf$Date[i]%m+% lubridate::years(0)
        }
        first_date=cut(first(as.Date(mydf$Date)), "year")
        first_date=paste(first_date, "00:00:00", sep = " ")
        first_date=as.POSIXct(first_date)
        
        last_year=last(as.Date(mydf$Date))%m+% lubridate::years(1)
        last_year=paste(last_year, "00:00:00", sep = " ")
        last_year=as.POSIXct(last_year)
        ggplot(mydf, aes(x=Date, y=Scores)) +
          scale_x_datetime(breaks = "1 year",minor_breaks = "1 years",labels = date_format("%Y"),name="Date range (Year)")+
          theme_light() +
          theme(axis.text=element_text(size=10),
                axis.title=element_text(size=12,face="bold"))+
          geom_point() + xlab("Date range (Year)") + ylab("Game Score (0-100)") +
          #scale_y_continuous(breaks = seq(0, 100, 2)) +
          geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_year, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(year),as.numeric(average2),label = "Trend line of scores", vjust = "top", hjust="right"), size=3)
      }
    }
  })
  
  output$plot60 <- renderPlot({
    
    days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
    if (days_count<1) {
      stop(safeError("End date should be after start date."))
      
    }
    
    deviceId=input$textInputDeviceId
    if (deviceId!="")
    {
      a=str_interp("${baseUrl}/${deviceId}/games.json")
      #theJsonSymptoms <- fromJSON(a)
      
      tryCatch(
        {
          theJsonGames <- fromJSON(a)
        },
        error= function(error_condition) {
          stop(safeError("Wrong patient id, please check patient id again."))
        }
      )
      if (is.null(theJsonGames)) {
        stop(safeError("Wrong patient id, please check patient id again."))
      }
      #print(theJsonGames)
      
      
      ids <- NULL
      dateTimes <- NULL
      scores <- NULL
      levels <- NULL
      
      counter=1
      for (i in theJsonGames) {
        ids[counter]=theJsonGames[[counter]]$id
        dateTimes[counter]=theJsonGames[[counter]]$dateTime
        scores[counter]=theJsonGames[[counter]]$score
        levels[counter]=theJsonGames[[counter]]$level
        counter=counter+1
      }
      
      
      
      mydf2=data.frame(id=ids, Date=dateTimes, Scores=scores, Levels=levels)
      #print(mydf2)
      mydf2$Scores<-(as.double(mydf2$Scores))
      
      
      
      date_error = as.numeric(input$daterange1[2]-as.Date(mydf2[c(1),"Date"]))+1
      if (date_error<1) {
        stop(safeError("No data for the patient in this time period."))
      }
      maximum=as.numeric(count(mydf2))
      date_error = as.numeric(as.Date(mydf2[c(maximum),"Date"])-input$daterange1[1])+1
      if (date_error<1) {
        stop(safeError("No data for the patient in this time period."))
      }
      
      #a <- filtered[ ,c("date_time")]
      #b <- filtered[ ,c("score")]
      #mydf = data.frame(Date=a, Scores=b)
      mydf <-
        mydf2 %>%
        filter(Date >= input$daterange1[1],
               Date <= input$daterange1[2])
      
      #print(mydf)
      if (as.numeric(count(mydf))==0) {
        stop(safeError("No data for the patient in this time period."))
      }
      
      
      
      
      
      filtered2 <- mydf2
      filtered2 <- filtered2[ ,c("Scores", "Date")]
      filtered2 <- unique(filtered2)
      
      first_point_intake = filtered2[c(1),2]
      
      count = count(filtered2)
      count = as.numeric(count)
      one_count_flag=0
      if (count<1) {
        stop(safeError("Empty data file for the patient."))
      }
      last_point_intake = filtered2[c(count),2]
      if (count==1) {
        count=2
        one_count_flag=1
      }
      
      half_point_intake = round(count/2)
      half_point_intake = as.numeric(half_point_intake)
      half_point_intake = filtered2[c(half_point_intake),2]
      #print(half_point_intake)
      
      if (one_count_flag==1) {
        half_point_intake_plus_one = round(count/2)
      } else {
        half_point_intake_plus_one = round(count/2) + 1
      }
      
      half_point_intake_plus_one = as.numeric(half_point_intake_plus_one)
      half_point_intake_plus_one = filtered2[c(half_point_intake_plus_one),2]
      #print(half_point_intake_plus_one)
      
      first_half <- 
        filtered2  %>%
        filter(Date >= first_point_intake,
               Date <= half_point_intake)
      
      
      #print("aaa")
      #print(first_half[["PD"]])
      average1 = mean(as.numeric(first_half$Scores))
      #print(average1)
      #print("aaaa")
      
      
      second_half <-
        filtered2  %>%
        filter(Date >= half_point_intake_plus_one,
               Date <= last_point_intake)
      
      #print(second_half)
      average2 = mean(as.numeric(second_half$Scores))
      #print(average2)
      
      
      
      #if (input$input2Id == TRUE) {
      # mydf$Date <- factor(mydf$Date, levels = mydf$Date[order(mydf$Scores)])
      #}
      
      
      last=as.numeric(count(mydf))
      for (i in 1:last) {
        #mydf$Date=as.Date(mydf$Date)
        #mydf$Date=as.POSIXlt(mydf$Date, format="%Y-%m-%dT%H:%M")
        mydf$Date=as.POSIXct(mydf$Date, format="%Y-%m-%dT%H:%M:%S")
        mydf$Date[i]=mydf$Date[i]+ lubridate::days(0)
        #day_and_date=paste(mydf$day, mydf$ddate, sep = " ")
        #mydf$day_and_date=paste(day_and_date, mydf$month, sep = " ")
        #mydf$day_and_date=paste(mydf$day_and_date, mydf$time, sep = " ")
      }
      #print(mydf)
      #scaling=data.frame(asdf=unique(as.Date(mydf$Date)))
      #scaling=as.numeric(as.Date(first(mydf$Date))-as.Date(last(mydf$Date)))
      #scaling=days_count
      #print(scaling)
      scale_count=days_count
        #count(scaling)
      Switch2=F
      Switch=F
      Switch3=F
      Switch4=F
      if (scale_count>90) {
        Switch4=T
        Switch2=T
      }
      if (scale_count>60 & scale_count<91) {
        Switch3=T
        Switch=T
      }
      if (scale_count>45 & scale_count<61) {
        Switch2=T
      }
      if (scale_count>7 & scale_count<46) {
        Switch=T
      }
      
      
      first_date=first(as.Date(mydf$Date))
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_date=last(as.Date(mydf$Date))+ lubridate::days(1)
      last_date=paste(last_date, "00:00:00", sep = " ")
      last_date=as.POSIXct(last_date)
      
      
      ggplot(mydf, aes(x=Date, y=Scores))+ #trendline(x1,y1,model="line2P",summary=TRUE) #geom_count()
        #scale_x_datetime(breaks = "1 day",minor_breaks = "1 days",labels = date_format("%a %d %b"),name="Date range")+
        scale_x_datetime(breaks = "1 day", minor_breaks = "1 days",labels=date_format("%a %d %b"),name="Date range")+
        {if(Switch3)scale_x_datetime(breaks = "2 days", minor_breaks = "1 days",labels=date_format("%a %d %b"),name="Date range")}+
        {if(Switch4)scale_x_datetime(breaks = "7 days", minor_breaks = "1 days",labels=date_format("%a %d %b"),name="Date range")}+
        #scale_x_date(breaks = "1 day",minor_breaks = "1 days",labels = date_format("%a %d %b"),name="Date range")+
        #geom_smooth(data = mydf, aes(x=game_id, y=Scores), method = lm, se = FALSE)
        theme_light() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        {if(Switch)theme(axis.text.x = element_text(angle = 45, hjust = 1))}+
        {if(Switch2)theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))}+
        geom_point() + xlab("Date range") + ylab("Game Score (0-100)")+
        #scale_y_continuous(breaks = seq(0, 100, 2)) +
        #geom_segment(aes(x = 0, y = as.numeric(average1), xend = Inf, yend = as.numeric(average2)), size = 1, color="Blue", arrow = arrow(length = unit(1,"cm")))+
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_date, yend = as.numeric(average2)), size = 0.5, color="Blue")
      #geom_text(aes(last(day_and_date),as.numeric(average2),label = "Trend line of scores", vjust = "top", hjust="right"), size=3)
      
      
    }
  })
  
  
}




shinyApp(ui = ui, server = server)
#}
