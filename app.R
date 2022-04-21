#reading in data that includes "plant seed counts" by LF and Volume
setwd("~/Desktop/Anthro Article Work")
dhiban_count <- read.csv("dhiban_count.csv")
las_capas_count <- read.csv("las_capas_count.csv")

#downloading needed packages
library(shiny)
library(dplyr)

#downloading more packages
library(ggplot2)
library(pscl)
library(MASS)
library(boot)
library(DT)

#creating dataframes count2
dhiban_count2 <- dhiban_count %>% 
    mutate(SeedDensity = plant_seed_counts/Vol_L)

las_capas_count2 <- las_capas_count %>% 
    mutate(SeedDensity = plant_seed_counts/Volume)

#creating new data set without outliers for las capas with Stratum as a factor 
las_capas_count3 <- las_capas_count2 %>% filter(plant_seed_counts < 2500)
las_capas_count3$Stratum <- as.factor(las_capas_count3$Stratum)

#creating log() variables
dhiban_count2 <- dhiban_count2 %>% 
    mutate(log_Volume = log(Vol_L))

las_capas_count3 <- las_capas_count3 %>% 
    mutate(log_Volume = log(Volume))

dhiban_count2$log_plant_seed_counts <- log(dhiban_count2$plant_seed_counts+1)

las_capas_count3$log_plant_seed_counts <- log(las_capas_count3$plant_seed_counts+1)


#4 models for dhiban
linear_dhiban <- lm(plant_seed_counts~Vol_L + Period, data=dhiban_count2)

log_dhiban <- lm(log_plant_seed_counts~log_Volume + Period, data=dhiban_count2)

nb_dhiban <- glm.nb(plant_seed_counts ~  Vol_L + Period, data= dhiban_count2)

pois_dhiban <- glm(plant_seed_counts ~ Vol_L + Period, family=poisson, data= dhiban_count2)

#4 models for las capas
linear_capas <- lm(plant_seed_counts~Volume + Stratum, data=las_capas_count3)

log_capas <- lm(log_plant_seed_counts~log_Volume + Stratum, data=las_capas_count3)

nb_capas <- glm.nb(plant_seed_counts ~  Volume + Stratum, data= las_capas_count3)

pois_capas <- glm(plant_seed_counts ~ Volume + Stratum, family=poisson, data= las_capas_count3)

#prepering for actual data graphs
dhiban_count2$Period <- factor(dhiban_count2$Period, 
                               levels = c("Iron I", "Iron II", "Nabataean-Roman","Late Byzantine", "Late Antique Transitional", "Middle Islamic I", "Middle Islamic II")) 


#Preparing for four graphs 
new_dhiban <- data.frame(Vol_L = rep(seq(1,62, 1),7),
                         Period= rep(levels(dhiban_count2$Period), each=62))
new_dhiban$log_Volume <- log(new_dhiban$Vol_L)
new_dhiban$Period <- factor(new_dhiban$Period, levels = c("Iron I", "Iron II", "Nabataean-Roman","Late Byzantine", "Late Antique Transitional", "Middle Islamic I", "Middle Islamic II")) 

new_las_capas <- data.frame(Volume = rep(seq(1,12.5, 1),3),
                            Stratum= rep(levels(las_capas_count3$Stratum), each=12.5))
new_las_capas$log_Volume <- log(new_las_capas$Volume)

#preparing for all data graphs
data_predict <- list(list(linear_dhiban), list(log_dhiban), list(pois_dhiban), 
                     list(nb_dhiban), list(linear_capas), list(log_capas), 
                     list(pois_capas), list(nb_capas))
new_data_model <- list()

for(i in 1:8){
    data_predict2 <- data_predict[[i]]
    if(i<5){
        new_data_model[i] <- list(cbind(new_dhiban, predict(data_predict2[[1]], new_dhiban, se.fit=TRUE)))
    }else if(i>=5){
        new_data_model[i] <- list(cbind(new_las_capas, predict(data_predict2[[1]], new_las_capas, se.fit=TRUE)))
    }
    
    
    if(i==1 || i==5){
        new_data_model[i] <- list(new_data_model[[i]] %>% mutate(plant_seed_counts = fit, upper_seed_counts=(fit+1.96*se.fit), lower_seed_counts=(fit-1.96*se.fit)))
    }else{
        new_data_model[i] <- list(new_data_model[[i]] %>% mutate(plant_seed_counts = exp(fit), upper_seed_counts=exp(fit+1.96*se.fit), lower_seed_counts=exp(fit-1.96*se.fit)))
    }
}

new_data_model[9:16] <- new_data_model[1:8]
#saving graphs to variables 
dhiban_actual <- (dhiban_count2 %>% ggplot(aes(x=Vol_L, y=plant_seed_counts, colour=Period)) + 
                      geom_point(aes(y=plant_seed_counts)) + 
                      labs(x="Volume", y="Observed Plant Seed Counts")+
                      facet_wrap(~Period, nrow = 2)+ theme(legend.position = "none", text=element_text(size=18)))

dhiban_empty <- (dhiban_count2 %>% ggplot(aes(x=Vol_L, y=plant_seed_counts, colour=Period)) + 
                     labs(x="Volume", y="Observed Plant Seed Counts")+
                     facet_wrap(~Period, nrow = 2)+ theme(legend.position = "none", text=element_text(size=18)))

capas_actual <- ggplot(las_capas_count3, aes(x=Volume, y=plant_seed_counts, colour=Stratum)) + 
    geom_point(aes(y=plant_seed_counts)) + 
    labs(x="Volume", y="Observed Plant Seed Counts")+facet_wrap(~Stratum)+ 
    theme(legend.position = "none", text=element_text(size=18), plot.margin=margin(1,.5,1,.5,"cm"))+
    scale_x_continuous(breaks=c(2.5,5.0,7.5,10.0,12.5),labels=c("2.5"="2.5","5.0"="5.0","7.5"="7.5","10.0"="10.0","12.5"="12.5"))

capas_empty <- ggplot(las_capas_count3, aes(x=Volume, y=plant_seed_counts, colour=Stratum)) + 
    labs(x="Volume", y="Observed Plant Seed Counts")+facet_wrap(~Stratum)+ 
    theme(legend.position = "none", text=element_text(size=18), plot.margin=margin(1,.5,1,.5,"cm"))+
    scale_x_continuous(breaks=c(2.5,5.0,7.5,10.0,12.5),labels=c("2.5"="2.5","5.0"="5.0","7.5"="7.5","10.0"="10.0","12.5"="12.5"))

data_model <- list()
for(i in 1:16){ 
    if(i<5){
        data_model[[i]] <- (new_data_model[[i]] %>% ggplot(aes(Vol_L, plant_seed_counts))+
                                geom_line(aes(colour=Period)) +
                                geom_ribbon(aes(ymin=lower_seed_counts, ymax= upper_seed_counts, fill=Period), alpha=.5)+ 
                                facet_wrap(~Period, nrow = 2) + 
                                labs(x="Volume", y="Expected Plant Seed Counts") + 
                                theme(legend.position = "none", text=element_text(size=18)))
    }else if(i>4 & i<9){
        data_model[[i]] <- (new_data_model[[i]] %>% ggplot(aes(Volume, plant_seed_counts))+
                                geom_line(aes(colour=Stratum)) + 
                                geom_ribbon(aes(ymin=lower_seed_counts, ymax= upper_seed_counts, fill=Stratum), alpha=.5)+ 
                                facet_wrap(~Stratum) + labs(x="Volume", y="Expected Plant Seed Counts")+ 
                                theme(legend.position = "none", text=element_text(size=18), plot.margin=margin(1,.5,1,.5,"cm")))
    }else if(i>8 & i<13){
        data_model[[i]] <- (new_data_model[[i]] %>% ggplot(aes(Vol_L, plant_seed_counts))+
                                geom_line(aes(colour=Period)) +
                                geom_ribbon(aes(ymin=lower_seed_counts, ymax= upper_seed_counts, fill=Period), alpha=.5)+ 
                                facet_wrap(~Period, nrow = 2) + 
                                labs(x="Volume", y="Expected Plant Seed Counts") + 
                                theme(legend.position = "none", text=element_text(size=18)) + 
                                geom_point(data= dhiban_count2, aes(y=plant_seed_counts), alpha = 0.5))
    }else if(i>12){
        data_model[[i]] <- (new_data_model[[i]] %>% ggplot(aes(Volume, plant_seed_counts))+
                                geom_line(aes(colour=Stratum)) + 
                                geom_ribbon(aes(ymin=lower_seed_counts, ymax= upper_seed_counts, fill=Stratum), alpha=.5)+ 
                                facet_wrap(~Stratum) + labs(x="Volume", y="Expected Plant Seed Counts")+ 
                                theme(legend.position = "none", text=element_text(size=18), plot.margin=margin(1,.5,1,.5,"cm")) + 
                                geom_point(data=las_capas_count3, aes(y=plant_seed_counts), alpha = 0.5))
    
    } 
}

##Creating data tables for dhiban and las capas
data_model_coef <- list()
data_model_r <- list()
data_reference <- list(list(summary(linear_dhiban)), list(summary(log_dhiban)), list(summary(pois_dhiban)), 
                       list(summary(nb_dhiban)), list(summary(linear_capas)), list(summary(log_capas)), 
                       list(summary(pois_capas)), list(summary(nb_capas)))

for(i in 1:8){
    sum_model_data <- data_reference[[i]]
    data_model_coef[i] <- list(round(sum_model_data[[1]]$coefficients,1))
    data_model_coef[i] <- list(as.data.frame(data_model_coef[i]))
    
    if(i==1 || i==2 || i==5 || i==6){
        data_model_r[i] <- round(sum_model_data[[1]]$r.squared,1)
        data_model_r[i] <- paste("R Squared =", data_model_r[i], sep=" ")
    } else{
        data_model_null <- sum_model_data[[1]]$null.deviance
        data_model_dev <- sum_model_data[[1]]$deviance
        data_model_r[i] <- paste("Deviance =", round(data_model_dev,1), ";", "Null Deviance=", round(data_model_null,1))
    }
    
}

#creating titles
title_string <- c("Dhiban: Observed Data", "Dhiban: Linear Predictions", "Dhiban: Log Linear Predictions", 
                  "Dhiban: Poisson Predictions", "Dhiban: Negative Binomial Predictions", "Las Capas: Observed Data", 
                  "Las Capas: Linear Predictions", "Las Capas: Log Linear Predictions", "Las Capas: Poisson Predictions",
                  "Las Capas: Negative Binomial Predictions"  )

title_variables <- vector(length=10)
for(i in 1:10){
    title_variables[i] <- title_string[i]
}



# Define UI for application that draws a histogram
ui <- fluidPage(
    headerPanel("Anthropology Count Data Analysis"), 
    sidebarPanel(
        selectInput("datapick", label="Select Data Set", choices= list("Dhiban"="dhiban",  "Las Capas"= "las_capas"))
    ),
    sidebarPanel(
        selectInput("pointspick", label="Show Data Points", choices= list("Yes"="yes",  "No"= "no"))
    ),
    sidebarPanel(
        selectInput("plotpick", label="Select Model", choices= list("No Model"="actual",  "Linear"= "new_linear", "Log Linear"= "new_log", "Poisson"= "new_poisson", "Negative Binomial"= "new_nb"))
    ),
    
    mainPanel(
        h2(textOutput("title"))
    ),
    
    mainPanel(
        plotOutput("plot1")
    ),
    
    mainPanel(
        textOutput("R_deviance")
    ), 
    
    
    mainPanel(
        h2("Model Coefficients"),
        DT::dataTableOutput("mytable")
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    datasetInput_data <- reactive({
        switch(input$datapick, 
               "dhiban" = switch(input$pointspick, "yes" = switch(input$plotpick, "actual" = dhiban_actual,
                                                                  "new_linear" = data_model[[9]], 
                                                                  "new_log" = data_model[[10]],
                                                                  "new_poisson" = data_model[[11]],
                                                                  "new_nb" = data_model[[12]]), 
                                 "no"=switch(input$plotpick, "actual" = dhiban_empty,
                                             "new_linear" = data_model[[1]], 
                                             "new_log" = data_model[[2]],
                                             "new_poisson" = data_model[[3]],
                                             "new_nb" = data_model[[4]])),
               "las_capas"= switch(input$pointspick, "yes" = switch(input$plotpick, 
                                                                    "actual" = capas_actual,
                                                                    "new_linear" = data_model[[13]], 
                                                                    "new_log" = data_model[[14]],
                                                                    "new_poisson" = data_model[[15]],
                                                                    "new_nb" = data_model[[16]]), 
                                   "no" = switch(input$plotpick, 
                                                 "actual" = capas_empty,
                                                 "new_linear" = data_model[[5]], 
                                                 "new_log" = data_model[[6]],
                                                 "new_poisson" = data_model[[7]],
                                                 "new_nb" = data_model[[8]])) )
        
        
    })
    
    
    dataInput_rdev <- reactive({
        switch(input$datapick,
               "dhiban"= switch(input$plotpick, 
                                "new_linear" =  data_model_r[[1]],
                                "new_log"= data_model_r[[2]],
                                "new_poisson"= data_model_r[[3]],
                                "new_nb"= data_model_r[[4]]),
               "las_capas"=switch(input$plotpick, 
                                  "new_linear"= data_model_r[[5]],
                                  "new_log"= data_model_r[[6]],
                                  "new_poisson"= data_model_r[[7]],
                                  "new_nb"= data_model_r[[8]])
        )
    })
    
    dataInput_coef <- reactive({
        switch(input$datapick,
               "dhiban"= switch(input$plotpick, "new_linear" =  data_model_coef[[1]],
                                "new_log"=data_model_coef[[2]],
                                "new_poisson"=data_model_coef[[3]],
                                "new_nb"=data_model_coef[[4]]),
               "las_capas"=switch(input$plotpick, "new_linear"=data_model_coef[[5]],
                                  "new_log"=data_model_coef[[6]],
                                  "new_poisson"=data_model_coef[[7]],
                                  "new_nb"=data_model_coef[[8]])
        )
    })
    
    dataInput_title <- reactive({
        switch(input$datapick,
               "dhiban"= switch(input$plotpick, "actual" =title_variables[1], 
                                "new_linear" = title_variables[2],
                                "new_log"=title_variables[3],
                                "new_poisson"=title_variables[4],
                                "new_nb"=title_variables[5]),
               "las_capas"=switch(input$plotpick, "actual" = title_variables[6],
                                  "new_linear"=title_variables[7],
                                  "new_log"=title_variables[8],
                                  "new_poisson"=title_variables[9],
                                  "new_nb"=title_variables[10])
        )
    })
    
    
    
    # output$text <- renderText({
    
    
    
    
    
    
    output$plot1 <- renderPlot({
        dataset <- datasetInput_data() 
        dataset
    })
    
    
    output$mytable = DT::renderDataTable({
        Coef <- dataInput_coef()
        Coef
    })
    
    output$title = renderText({
        title <- dataInput_title()
        title
    })
    
    output$R_deviance = renderText({
        R_dev <- dataInput_rdev()
        R_dev
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)



