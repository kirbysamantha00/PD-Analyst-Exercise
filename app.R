## setting work directory, App-1 contains the 3 files as well as the app
#setwd("~/Desktop/App-1") 

# Load packages for APP ----------------------------------------------------------------
library(dplyr)
library(stringr)
library(shiny)
library(ggplot2)
library(bslib)
library(DT)
## Input csv files into environment
goals <- read.csv("pd_analyst_goals.csv")
events <- read.csv("pd_analyst_events.csv")
pitches <- read.csv("pd_analyst_pitches.csv")
##
#install.packages("rlang")
#install.packages("ggplot2",dependencies = TRUE)
#install.packages("DT")
#install.packages("stringr")
#install.packages("shiny")
#install.packages("bslib")
#install.packages("dplyr")
## adding all necessary libraries


##
playerIDs<- unique(events$pitcher_id) # Identifying all unique pitcher IDs
rowlist <- list()
rowlist_2 <- list()
count=1;
# Calculates all stats across each individual date for each player
for (i in 1:NROW(playerIDs)){
  status <- ifelse(events$pitcher_id == as.character(playerIDs[i]), "1","0") # finds where playerID exists in events
  loc <- which(status != 0) #locates the above finds
  uni_dates <- unique(events$sched_date[loc]) #finds all unique dates for that player
  for (j in 1:NROW(uni_dates)){
    status2 <- ifelse(events$sched_date == uni_dates[j] & events$pitcher_id == as.character(playerIDs[i]), "1", "0") #finds where the players and dates are equal in events
    loc2 <- which(status2 != 0) # locates the above finds
    pa_tot <- round(sum(events$pa[loc2]),digits=2) #summing for individual player and date
    bb_tot <- round(sum(events$bb[loc2]),digits=2) #summing for individual player and date
    so_tot <- round(sum(events$so[loc2]),digits=2) #summing for individual player and date
    oneB <- round(sum(events$X1b[loc2]),digits=2) #summing for individual player and date
    twoB <- round(sum(events$X2b[loc2]),digits=2) #summing for individual player and date
    threeB <- round(sum(events$X3b[loc2]),digits=2) #summing for individual player and date
    HR <- round(sum(events$hr[loc2]),digits=2) #summing for individual player and date
    AB_tot <- round(sum(events$ab[loc2]),digits=2) #summing for individual player and date
    BB_per <- round((bb_tot/pa_tot)*100,digits=2) #calculating BB% for individual date 
    K_per <- round((so_tot/pa_tot)*100,digits=2) #calculating K% for individual date
    SLG_per <- round((((oneB*1)+(twoB*2)+(threeB*3)+(HR*4))/AB_tot)*100,digits=2) #calculating SLG% for indiviudal date
    status3 <- ifelse(pitches$sched_date == uni_dates[j] & pitches$pitcher_id == as.character(playerIDs[i]),"1","0") #finds where the players and dates are in pitch csv
    loc3 <- which(status3 != 0) # locates above finds
    balls <- pitches$balls_before[loc3] # calculating ball and strike totals to find first pitches
    strikes <- pitches$strikes_before[loc3]
    note <- pitches$pitch_result[loc3]
    status4 <- ifelse(balls == 0 & strikes == 0 ,"1","0")
    loc4 <- which(status4 != 0)
    status5 <- ifelse(balls == 0 & strikes == 0 & grepl("strike",note),"1","0") # identifying 1st pitch strikes
    loc5 <- which(status5 != 0)
    tot_fp <- NROW(loc4)
    tot_fps <- NROW(loc5)
    fps <- round((tot_fps/tot_fp)*100,digits=2) # FPS calculating
    dates_date <- as.Date(uni_dates[j], format = "%m/%d/%Y") # adjusting dates for organization
    rowlist[[count]] <- data.frame(as.character(playerIDs[i]),dates_date,BB_per,K_per,SLG_per,fps)
    rowlist_2[[count]] <- data.frame(as.character(playerIDs[i]),dates_date,AB_tot,pa_tot,oneB,twoB,threeB,HR,BB_per,K_per,SLG_per,fps)
    count <- count + 1
    
  }
}
df_final_asc <- do.call(rbind, rowlist) #storing stats calculated above
sum_final_asc <-do.call(rbind, rowlist_2) #storing stats calculated above
colnames(df_final_asc) <-c("PlayerID","Dates","BBper","Kper","SLGper","FPSper")
colnames(sum_final_asc) <-c("PlayerID","Dates","AB","PA","X1B","X2B","X3B","HR","BB_percent","K_percent","SLG_percent","FPS_percent")
rowlist_5 <- list()
count=1;

## finding full season stats for player 
for (i in 1:NROW(playerIDs)){
  status <- ifelse(events$pitcher_id == as.character(playerIDs[i]), "1","0") # identifying unique playerid location
  loc2 <- which(status != 0)
  pa_tot <- round(sum(events$pa[loc2]),digits=2) # stat calculations
  bb_tot <- round(sum(events$bb[loc2]),digits=2)
  so_tot <- round(sum(events$so[loc2]),digits=2)
  oneB <- round(sum(events$X1b[loc2]),digits=2)
  twoB <- round(sum(events$X2b[loc2]),digits=2)
  threeB <- round(sum(events$X3b[loc2]),digits=2)
  HR <- round(sum(events$hr[loc2]),digits=2)
  AB_tot <- round(sum(events$ab[loc2]),digits=2)
  BB_per <- round((bb_tot/pa_tot)*100,digits=2)
  K_per <- round((so_tot/pa_tot)*100,digits=2)
  SLG_per <- round((((oneB*1)+(twoB*2)+(threeB*3)+(HR*4))/AB_tot)*100,digits=2)
  status3 <- ifelse(pitches$pitcher_id == as.character(playerIDs[i]),"1","0") # identifying unique playerid locations in pitch csv
  loc3 <- which(status3 != 0)
  balls <- pitches$balls_before[loc3] # stat calculations
  strikes <- pitches$strikes_before[loc3]
  note <- pitches$pitch_result[loc3]
  status4 <- ifelse(balls == 0 & strikes == 0 ,"1","0")
  loc4 <- which(status4 != 0)
  status5 <- ifelse(balls == 0 & strikes == 0 & grepl("strike",note),"1","0")
  loc5 <- which(status5 != 0)
  tot_fp <- NROW(loc4)
  tot_fps <- NROW(loc5)
  fps <- round((tot_fps/tot_fp)*100,digits=2)
  rowlist_5[[count]] <- data.frame(as.character(playerIDs[i]),AB_tot,pa_tot,oneB,twoB,threeB,HR,BB_per,K_per,SLG_per,fps)
  count <- count + 1
}
full_season <-do.call(rbind, rowlist_5) # saving above stats for full season play
colnames(full_season) <-c("PlayerID","AB","PA","X1B","X2B","X3B","HR","BB_percent","K_percent","SLG_percent","FPS_percent")

## finding last two weeks of game for each player
rowlist_6 <- list()
count <- 1
for (i in 1:NROW(playerIDs)){
  events$sched_date <- as.Date(events$sched_date, format = "%m/%d/%Y") #sorting dates for use to find last two weeks
  event_arr <- arrange(events,sched_date) # rearranging oldest to most recent
  status7 <- ifelse(event_arr$pitcher_id == playerIDs[i], "1", "0")
  loc7 <- which(status7 != 0)
  data_need <- event_arr[loc7,]
  two_weeks <- data_need$sched_date[NROW(data_need)]-14 # finding two weeks prior to last date for player
  loc8 <- which(between(data_need$sched_date,two_weeks,data_need$sched_date[NROW(data_need)])) # find in between of the two weeks
  pitches$sched_date <- as.Date(pitches$sched_date, format = "%m/%d/%Y") # same process for pitch csv
  pitches_arr <- arrange(pitches,sched_date)
  status10 <- ifelse(pitches_arr$pitcher_id == playerIDs[i], "1", "0")
  loc10 <- which(status10 != 0)
  data_need_pit <- pitches_arr[loc10,]
  two_weeks_pit <- data_need_pit$sched_date[NROW(data_need_pit)]-14
  loc11 <- which(between(data_need_pit$sched_date,two_weeks_pit,data_need_pit$sched_date[NROW(data_need_pit)]))
  pa_tot <- round(sum(data_need$pa[loc8]),digits=2) # calculations for the two week stats
  bb_tot <- round(sum(data_need$bb[loc8]),digits=2)
  so_tot <- round(sum(data_need$so[loc8]),digits=2)
  oneB <- round(sum(data_need$X1b[loc8]),digits=2)
  twoB <- round(sum(data_need$X2b[loc8]),digits=2)
  threeB <- round(sum(data_need$X3b[loc8]),digits=2)
  HR <- round(sum(data_need$hr[loc8]),digits=2)
  AB_tot <- round(sum(data_need$ab[loc8]),digits=2)
  BB_per <- round((bb_tot/pa_tot)*100,digits=2)
  K_per <- round((so_tot/pa_tot)*100,digits=2)
  SLG_per <- round((((oneB*1)+(twoB*2)+(threeB*3)+(HR*4))/AB_tot)*100,digits=2)
  balls <- data_need_pit$balls_before[loc11]
  strikes <- data_need_pit$strikes_before[loc11]
  note <- data_need_pit$pitch_result[loc11]
  status4 <- ifelse(balls == 0 & strikes == 0 ,"1","0")
  loc4 <- which(status4 != 0)
  status5 <- ifelse(balls == 0 & strikes == 0 & grepl("strike",note),"1","0")
  loc5 <- which(status5 != 0)
  tot_fp <- NROW(loc4)
  tot_fps <- NROW(loc5)
  fps <- round((tot_fps/tot_fp)*100,digits=2)
  rowlist_6[[count]] <- data.frame(as.character(playerIDs[i]),AB_tot,pa_tot,oneB,twoB,threeB,HR,BB_per,K_per,SLG_per,fps)
  count <- count + 1
}
Last_two_season <-do.call(rbind, rowlist_6) # saving the two week stat data
colnames(Last_two_season) <-c("PlayerID","AB","PA","X1B","X2B","X3B","HR","BB_percent","K_percent","SLG_percent","FPS_percent")

rowlist_3 <- list()
count3=1;
sum_final <- arrange(sum_final_asc,Dates)
df_final <- arrange(df_final_asc,Dates)
## finding the goals and breaking them down to be digestible
## filtered through Decrease and Increase if statements 
## followed by extracting the values in the statemnets 
## and identifying which KPI the goal is looking at 
for (i in 1:NROW(goals)){
  if (grepl("Decrease",goals$Primary.Goal[i]) == TRUE){
    P_1 <- "Decrease"
    P_3 <- str_extract(goals$Primary.Goal[i], "[0-9.]+")
    P_4 <- (-1)*as.numeric(P_3)
    if (grepl("K",goals$Primary.Goal[i]) == TRUE){
      P_2 <- "K"
    } else if (grepl("BB",goals$Primary.Goal[i])== TRUE){
      P_2 <- "BB"
    }
  } else if (grepl("Increase",goals$Primary.Goal[i]) == TRUE){
    P_1 <- "Increase"
    P_3 <- str_extract(goals$Primary.Goal[i], "[0-9.]+")
    P_4 <- as.numeric(P_3)
    if (grepl("K",goals$Primary.Goal[i]) == TRUE){
      P_2 <- "K"
    } else if (grepl("BB",goals$Primary.Goal[i])== TRUE){
      P_2 <- "BB"
    }
  }
  if (grepl("Decrease",goals$Secondary.Goal[i]) == TRUE){
    S_1 <- "Decrease"
    S_3 <- str_extract(goals$Secondary.Goal[i], "[0-9.]+")
    S_4 <- (-1)*as.numeric(S_3)
    if (grepl("FPinZ",goals$Secondary.Goal[i]) == TRUE){
      S_2 <- "FPinZ"
    } else if (grepl("BB",goals$Secondary.Goal[i])== TRUE){
      S_2 <- "BB"
    }
  } else if (grepl("Increase",goals$Secondary.Goal[i]) == TRUE){
    S_1 <- "Increase"
    S_3 <- str_extract(goals$Secondary.Goal[i], "[0-9.]+")
    S_4 <- as.numeric(S_3)
    if (grepl("FPinZ",goals$Secondary.Goal[i]) == TRUE){
      S_2 <- "FPinZ"
    } else if (grepl("BB",goals$Primary.Goal[i])== TRUE){
      S_2 <- "BB"
    }
  }
  if (grepl("Decrease",goals$Tertiary.Goal[i]) == TRUE){
    T_1 <- "Decrease"
    T_3 <- str_extract(goals$Tertiary.Goal[i], "[0-9.]+")
    T_4 <- (-1)*as.numeric(T_3)
    if (grepl("FPinZ",goals$Tertiary.Goal[i]) == TRUE){
      T_2 <- "FPinZ"
    } else if (grepl("K",goals$Tertiary.Goal[i])== TRUE){
      T_2 <- "K"
    } 
  } else if (grepl("Increase",goals$Tertiary.Goal[i]) == TRUE){
    T_1 <- "Increase"
    T_3 <- str_extract(goals$Tertiary.Goal[i], "[0-9.]+")
    T_4 <- as.numeric(T_3)
    if (grepl("FPinZ",goals$Tertiary.Goal[i]) == TRUE){
      T_2 <- "FPinZ"
    } else if (grepl("K",goals$Tertiary.Goal[i])== TRUE){
      T_2 <- "K"
    }
  }
  rowlist_3[[count3]] <- data.frame(goals$player_id[i],P_1,P_2,P_3,P_4,S_1,S_2,S_3,S_4,T_1,T_2,T_3,T_4)
  count3 <- count3 + 1
}
goal_final <-do.call(rbind, rowlist_3) ## saving the broken down goals
colnames(goal_final) <-c("PlayerID","P1","P2","P3","P4","S1","S2","S3","S4","T1","T2","T3","T4")
note7 <- list()
note8<-list()
note9<-list()
## identifying if the goals have been achieved or not 
## breaks down each player from goal_final and for each 
## goal cycling through to determine if the goal target is 
## has been met, identifying those none met by outputting difference 
## between goal and current status
for (i in 1:NROW(goal_final)){
  status7 <- ifelse(goal_final$PlayerID[i] == sum_final$PlayerID, "1", "0")
  loc7 <- which(status7 != 0)
  if (goal_final$P2[i] == "BB"){
    needed_loc <- tail(loc7,n=1)
    if (sum_final$BB_percent[needed_loc] <= goal_final$P3[i]){
      note7[i] <- "Goal Complete"
    } else{
      note7[i] <-  paste0(round(abs(as.numeric(goal_final$P3[i]) - sum_final$BB_percent[needed_loc]),digits=2),"% away")
    }
  }else if (goal_final$P2[i] == "K"){
    needed_loc <- tail(loc7, n=1)
    if (sum_final$K_percent[needed_loc] >= goal_final$P3[i]){
      note7[i] <- "Goal Complete"
    } else{
      note7[i] <- paste0(round(abs(as.numeric(goal_final$P3[i]) -  sum_final$K_percent[needed_loc]),digits=2),"% away")
    }
  }
  if (goal_final$S2[i] == "BB"){
    needed_loc <- tail(loc7,n=1)
    if (sum_final$BB_percent[needed_loc] <= goal_final$S3[i]){
      note8[i] <- "Goal Complete"
      print(i)
    } else{
      note8[i] <-  paste0(round(abs(as.numeric(goal_final$S3[i]) - sum_final$BB_percent[needed_loc]),digits=2),"% away")
      print(i)
    }
  }else if (goal_final$S2[i] == "FPinZ"){
    needed_loc <- tail(loc7, n=1)
    if (sum_final$FPS_percent[needed_loc] >= goal_final$S3[i]){
      note8[i] <- "Goal Complete"
      print(i)
    } else{
      note8[i] <-  paste0(round(abs(as.numeric(goal_final$S3[i]) - sum_final$FPS_percent[needed_loc]),digits=2),"% away")
      print(i)
    }
  }
  if (goal_final$T2[i] == "K"){
    needed_loc <- tail(loc7,n=1)
    if (sum_final$K_percent[needed_loc] >= goal_final$T3[i]){
      note9[i] <- "Goal Complete"
    } else{
      note9[i] <-  paste0(round(abs(as.numeric(goal_final$T3[i]) - sum_final$K_percent[needed_loc]),digits=2),"% away")
    }
  }else if (goal_final$T2[i] == "FPinZ"){
    needed_loc <- tail(loc7, n=1)
    if (sum_final$FPS_percent[needed_loc] >= goal_final$T3[i]){
      note9[i] <- "Goal Complete"
    } else{
      note9[i] <-  paste0(round(abs(as.numeric(goal_final$T3[i]) - sum_final$FPS_percent[needed_loc]),digits=2),"% away")
    }
  }
}
goal_final$comp1 <- note7
goal_final$comp2 <- note8
goal_final$comp3 <- note9
## adjusting column titles for above data collection
colnames(Last_two_season) <- c("PlayerID","AB","PA","X1B","X2B","X3B","HR","BB%","K%","SLG%","FPS%")
colnames(full_season) <- c("PlayerID","AB","PA","X1B","X2B","X3B","HR","BB%","K%","SLG%","FPS%")
colnames(goals) <- c("player_id","Primary","Secondary","Tertiary")



# Define UI ---
ui <- page_sidebar(
  title = "Player Dashboard", # dashboard title
  sidebar = sidebar( # creates a sidebar panel to select player and trend
    selectInput(
      inputId = "Player",
      label = "Select Player:",
      choices = as.character(unique(pitches$pitcher_id)),
      
      selected = 77028
    ),
    br(),
    radioButtons(inputId = "trend","KPI Trends:",
                 
                 c("BB%"="BB_percent",
                   "K%"="K_percent",
                   "SLG%"="SLG_percent",
                   "FPS%"="FPS_percent")
                 ,selected = "BB_percent")
  ),
                 
                    
                            navset_card_underline( ## creates tabs within the main panel to find necessary data
                              nav_panel("Player Overview",
                                        accordion( ## collapsible cards to make navigation easier 
                                          open = c("Summary Card", "Goals Overview"),
                                          accordion_panel(
                                            "Summary Card",
                                            h3("Full Season"),
                                            DTOutput("sum"),
                                            h3("Last Two Weeks"),
                                            DTOutput("sum2")
                                          ),
                                          accordion_panel(
                                            "Individual Apperances Log",
                                            DTOutput("summary")
                                          ),
                                          accordion_panel(
                                            "Goal Overview",
                                            DTOutput("goal")
                                          )
                                        )
                                        #width=10,
                                       # h2("Summary Card"),
                                       # DTOutput("summary"),
                                        #h2("Goal Overview"),
                                        #DTOutput("goal")
                              ),
                              nav_panel("Goals", ## outputting goals in a tab with completion values
                                        h2("Goals:"),
                                        uiOutput("goal1"),uiOutput("goal1_c"),
                                        uiOutput("goal2"),uiOutput("goal2_c"),
                                        uiOutput("goal3"),uiOutput("goal3_c")),
                              nav_panel("Trends", ## plotting trends with use of the sidepanel selections
                                        width = 12,
                                        plotOutput("scatter_plot"))
                            )
                         
                          
                 )
# Define server logic ---
server <- function(input, output){
  ## These use data points from the selected sidebar above to identify the players and trends and 
  ## extracts the data from that found above in order to output the correct plots and tables
  reactive_data=reactive({
    selected_player=as.numeric(input$Player)
    used <- sum_final
    colnames(used) <- c("PlayerID","Dates","AB","PA","X1B","X2B","X3B","HR","BB%","K%","SLG%","FPS%")
    return(used[used$PlayerID == selected_player,])
  })
  output$summary <- renderDT({
    datatable(reactive_data()[,2:NCOL(sum_final)],
              options = list(
                searching = FALSE
              ), 
              rownames = FALSE)
  })
  data1 <- reactive ({
    selected_player=as.numeric(input$Player)
    return(full_season[full_season$PlayerID == selected_player,])
  })
  data2 <- reactive({
    selected_player=as.numeric(input$Player)
    return(Last_two_season[Last_two_season$PlayerID == selected_player,])
  })
  output$sum <- renderDT({
   datatable(
     df <- data1()[,2:NCOL(full_season)],
     options = list(
       searching = FALSE,
       paging = FALSE,
       info = FALSE
     ),
      rownames = FALSE)
  })
  output$sum2 <- renderDT({
    datatable(
      df <- data2()[,2:NCOL(Last_two_season)],
      options = list(
        searching = FALSE,
        paging = FALSE,
        info = FALSE
      ),
      rownames = FALSE)
  })
  goal_reactive_data=reactive({
    selected_player=as.numeric(input$Player)
    return(goals[goals$player_id == selected_player,])
  })
  goal_comp_reactive_data=reactive({
    selected_player=as.numeric(input$Player)
    return(goal_final[goal_final$PlayerID == selected_player,])
  })
  output$goal <- renderDT({
    datatable(goal_reactive_data()[1,2:NCOL(goals)], 
              options = list(
                searching = FALSE,
                paging = FALSE, 
                info = FALSE
              ),
              rownames=FALSE)
  })
  trend_reactive_data=reactive({
    selected_player=as.numeric(input$Player)
    return(sum_final[sum_final$PlayerID == selected_player,])
  })
  output$scatter_plot <- renderPlot({
    if (input$trend == "BB_percent"){
      label_val <- "BB%"
    } else if (input$trend == "K_percent"){
      label_val <- "K%"
    } else if (input$trend == "SLG_percent"){
      label_val <- "SLG%"
    } else if (input$trend == "FPS_percent"){
      label_val <- "FPS%"
    }
    ggplot(trend_reactive_data(), aes_string(x = "Dates",y = input$trend)) +
     geom_point(size = 3) +
    labs(title = paste("Dates vs", label_val),
           x = "Date",
           y = label_val) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+ # makes x font angles to read easier
              theme(axis.text = element_text(size = 15))+ # change x and y size
      theme(plot.title = element_text(size = 20,hjust=0.5))+ # Change title location and size
      theme(axis.title.x = element_text(size = 15,face="bold"), # Change x-axis title size and boldness
            axis.title.y = element_text(size = 15,face="bold")) # Change y-axis title size and boldness
  })
  ## below contain the use of HTML in order to format the page correct with indents and bold font
  output$goal1 <- renderUI({
    goal_1 <- goal_reactive_data()[1, 2] 
    text_2_disp <- paste0("<strong>","Goal 1: ",goal_1,"</strong>")
    HTML(text_2_disp)
  })
  output$goal2 <- renderUI({
    goal_2 <- goal_reactive_data()[1,3]
    text_2_disp <- paste0("<strong>","Goal 2: ",goal_2,"</strong>")
    HTML(text_2_disp)
  })
  output$goal3 <- renderUI({
    goal_3 <- goal_reactive_data()[1,4]
    text_2_disp <- paste0("<strong>","Goal 3: ",goal_3,"</strong>")
    HTML(text_2_disp)
  })
  output$goal1_c <- renderUI({
    goal_1c <- goal_comp_reactive_data()[1, 14] 
    indent_style <- paste0("padding-left: ", 15, "px;")
      text_2_disp <- paste0("Progress: ", goal_1c)
      tags$div(text_2_disp, style = indent_style)
  })
  output$goal2_c <- renderUI({
    goal_2c <- goal_comp_reactive_data()[1,15]
    indent_style <- paste0("padding-left: ", 15, "px;")
    text_2_disp <- paste0("Progress: ", goal_2c)
    tags$div(text_2_disp, style = indent_style)
  })
  output$goal3_c <- renderUI({
    goal_3c <- goal_comp_reactive_data()[1,16]
    indent_style <- paste0("padding-left: ", 15, "px;")
    text_2_disp <- paste0("Progress: ", goal_3c)
    tags$div(text_2_disp, style = indent_style)
  })
}

# Run the app ---
shinyApp(ui = ui, server = server)






