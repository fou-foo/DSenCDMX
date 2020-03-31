library(dplyr)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(reshape2)
library(plotly)# Define server logic required to draw a histogram
#input <- list()
#input[['days']] <- 30
#input[['sigma']] <- 0.18
#input[['gamma_s']] <- 1
#input[['gamma_a']] <- 0.5
#input[['alpha']] <- 0.0
#input[['w']] <- 0.0
#input[['estado']] <- "San Luis Potosi"
shinyServer(function(input, output) {
  output$beta <-  renderPrint({ as.character( 2*2.28*input$gamma_s) })
  # funcion para resolver el sistema ODE
  output$ia_plot <- renderPlotly({
    sigma <- round(input$sigma, 2)
    gamma_a <- gamma_s <- round(input$gamma_s, 2)
    #gamma_a <- round( input$gamma_a,2)
    days <- round(input$days, 2)
    string <- paste0('Data/days_', input$days, '|sigma_', sigma, '|gamma_s_', gamma_s,
                     '|gamma_a', gamma_a, '|alpha_', input$alpha,
                     '|w_', input$w, '.csv')
    data <- read.csv(file = string)
    index <- grep('^ia',names(data))
    data <- data[, c(1, index)]
    if( input$estado=='Todos')
    {
      sim2 <- melt( data , id.vars = 'time')
      p <- ggplot(sim2, aes(x =time, y = value, color=variable)) + geom_line() + theme_minimal() +
        theme(legend.position="bottom", legend.title = element_text(color = "blue", size = 0)) +
        ggtitle('Pacientes asintomaticos ') + ylab('')
    } else{
      index <- grep(input$estado, names(data) )
      data <- data[, c(1, index)]
      sim2 <- melt( data , id.vars = 'time')
      p <- ggplot(sim2, aes(x =time, y = value, color=variable)) + geom_line() + theme_minimal() +
        theme(legend.position="bottom", legend.title = element_text(color = "blue", size = 0)) +
        ggtitle('Pacientes asintomaticos ') + ylab('')
    }
    p <- ggplotly(p)
    return(p)
    })
  
  
  
  
  output$is_plot <- renderPlotly({
    sigma <- round(input$sigma, 2)
    gamma_a <- gamma_s <- round(input$gamma_s, 2)
    #gamma_a <- round( input$gamma_a,2)
    days <- round(input$days, 2)
    
    string <- paste0('Data/days_',  days, '|sigma_',  sigma, '|gamma_s_',  gamma_s,
                     '|gamma_a',  gamma_a, '|alpha_', input$alpha,
                     '|w_', input$w, '.csv')
    data <- read.csv(file = string)
    index <- grep('^is',names(data))
    data <- data[, c(1, index)]
    if( input$estado=='Todos')
    {
      sim2 <- melt( data , id.vars = 'time')
      p <- ggplot(sim2, aes(x =time, y = value, color=variable)) + geom_line() + theme_minimal() +
        theme(legend.position="bottom", legend.title = element_text(color = "blue", size = 0)) +
        ggtitle('Pacientes sintomaticos ') + ylab('')
    } else{
      index <- grep(input$estado, names(data) )
      data <- data[, c(1, index)]
      sim2 <- melt( data , id.vars = 'time')
      p <- ggplot(sim2, aes(x =time, y = value, color=variable)) + geom_line() + theme_minimal() +
        theme(legend.position="bottom", legend.title = element_text(color = "blue", size = 0)) +
        ggtitle('Pacientes asintomaticos ') + ylab('')
    }
    p <- ggplotly(p)
    return(p)
  })
  
  output$y_plot <- renderPlotly({
    sigma <- round(input$sigma, 2)
    gamma_a <- gamma_s <- round(input$gamma_s, 2)
    #gamma_a <- round( input$gamma_a,2)
     
    days <- round(input$days, 2)
    
    string <- paste0('Data/days_', days, '|sigma_', sigma, '|gamma_s_', gamma_s,
                     '|gamma_a', gamma_a, '|alpha_', input$alpha,
                     '|w_', input$w, '.csv')
    data <- read.csv(file = string)
    index <- grep('^y',names(data))
    data <- data[, c(1, index)]
    if( input$estado=='Todos')
    {
      sim2 <- melt( data , id.vars = 'time')
      p <- ggplot(sim2, aes(x =time, y = value, color=variable)) + geom_line() + theme_minimal() +
        theme(legend.position="bottom", legend.title = element_text(color = "blue", size = 0)) +
        ggtitle('Pacientes infectados por dia') + ylab('')
    } else{
      index <- grep(input$estado, names(data) )
      data <- data[, c(1, index)]
      sim2 <- melt( data , id.vars = 'time')
      p <- ggplot(sim2, aes(x =time, y = value, color=variable)) + geom_line() + theme_minimal() +
        theme(legend.position="bottom", legend.title = element_text(color = "blue", size = 0)) +
        ggtitle('Pacientes infectados por dia') + ylab('')
    }
    p <- ggplotly(p)
    return(p)
  })
  
})
