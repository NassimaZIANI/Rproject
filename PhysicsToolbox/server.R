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
library(dplyr)
library(RcppRoll)
library(ggplot2)


shinyServer(function(input, output) {
        
        datasetInput <- reactive({
            switch(input$dataset,
                   "omov" = omov,
                   "xmov" = xmov,
                   "walkmov" = walkmov)
        })
        
        omov = read_delim("./omov.csv",delim=",",locale = locale(dec=","))
        xmov = read_delim("./xmov.csv",delim=",",locale = locale(dec=","))
        walkmov = read_delim("./walkmov.csv",delim=",",locale = locale(dec=","))
        
        output$summary <- renderPrint({
            dataset <- datasetInput()
            summary(dataset)
        })
        
        # Show the first "n" observations
        output$view <- renderTable({
            head(datasetInput(), n = input$obs)
        })

        norm_acceleration <- function(gFx,gFy,gFz){
            gN <- abs(gFx^2+gFy^2+gFz^2-0.8)
            
        }
        
        
        left_rolling_average <- function(gN){
            gNm <- roll_meanl(gN,10)
            
        }
        
        left_rolling_standard_deviation <- function(gN){
            gNsd <- roll_sdl(gN,10)
            
        }
        
        compute_time <- function(data){
            dt <- as.numeric(difftime(data$time,data$time_m_1),units="secs")
        }
        
        set_pid <- function(data,timediff,mean,sd){
            res = data %>% mutate(pid = ifelse(gNm < mean & gNsd < sd & timediff > dt, 0, 1 + seq(1, nrow(data)) %/% (nrow(data)/20)))
        }
        
        extract_feature_long <- function(data,threeshold_mean,threeshold_sd,threeshold_t) {
            gN <- norm_acceleration(as.numeric(data$gFx),as.numeric(data$gFy),as.numeric(data$gFz))
            gNm <- left_rolling_average(gN)
            gNsd <- left_rolling_standard_deviation(gN)
            add_data = data %>% mutate("gN"=gN, "gNm" = gNm, "gNsd" = gNsd, "time_m_1" = lag(time))
            dt <- compute_time(add_data)
            time_diff_data = add_data %>% mutate("dt" = dt)
            pid_data <- set_pid(time_diff_data,threeshold_mean, threeshold_sd, threeshold_t)
            pid_data <- subset(pid_data,select = -time_m_1)
            pid_data 
        }
        
        X_long <- reactive({extract_feature_long(datasetInput(),input$meanthresh,input$sdthresh,input$timethresh)})
        
        
        output$graph <- renderPlot({
            X_long = X_long()
            ggplot(X_long %>% filter(pid>0)) + geom_line(aes(x=time,y=gNsd,color=as.factor(pid),group=pid)) + geom_point(data=X_long %>% filter(pid==00),aes(x=time,y=gNsd),color="black") 
        })
        
        output$title <- renderText({
            input$dataset
        })
        
        output_with_label <- reactive({
            X_long = X_long()
            X_long %>% mutate(label = ifelse(pid != 0, input$label, "N"))
        })
        
        output$downloadData <- downloadHandler(
            filename = function() {
                X_long = X_long()
                paste(input$dataset, "_output.csv", sep = "")
            },
            content = function(file) {
                write.csv(output_with_label(), file, row.names = FALSE)
            }
        )
})
