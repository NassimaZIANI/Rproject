#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(RcppRoll)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        output$contents <- renderTable({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        data = read.csv(inFile$datapath, header = input$header)

        norm_acceleration <- function(gFx,gFy,gFz){
            gN <- abs(gFx^2+gFy^2+gFz^2-0.8)
            
        }
        
        
        left_rolling_average <- function(gN){
            gNm <- roll_meanl(gN,10)
            
        }
        
        left_rolling_standard_deviation <- function(gN){
            gNsd <- roll_sdl(gN,10)
            
        }
        
        filter_data <- function(data,mean,sd){
            res = data %>% filter(gNm > mean) %>% filter(gNsd > sd)
            
        }
        
        compute_time <- function(data){
            dt <- data$time - data$time_m_1 
        }
        
        set_pid <- function(data,time){
            pid_data = data %>% mutate(pid = ifelse(gNm < time, 1, 0))
        }
        
        extract_feature_long <- function(data,threeshold_mean,threeshold_sd,threeshold_t) {
            gN <- norm_acceleration(as.numeric(data$gFx),as.numeric(data$gFy),as.numeric(data$gFz))
            gNm <- left_rolling_average(gN)
            gNsd <- left_rolling_standard_deviation(gN)
            add_data = data %>% mutate("gNm" = gNm, "gNsd" = gNsd)
            filtered_data <- filter_data(add_data, threeshold_mean, threeshold_sd)
            lag_data = filtered_data %>% mutate (time_m_1 = lag(time))
            dt <- compute_time(lag_data)
            time_diff_data = lag_data %>% mutate("dt" = dt)
            pid_data <- set_pid(time_diff_data, threeshold_t)
            pid_data <- subset(pid_data,select = -time_m_1)
            str(dim(data))
            str(dim(add_data))
            str(dim(filtered_data))
            str(dim(pid_data))
            res = bind_rows(pid_data, add_data)
            str(dim(res))
            res
        }
    })
})
