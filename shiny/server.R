library(data.table)
library(HistData)
library(plotly)

library(shiny)

data(GaltonFamilies)
dt_heights <- as.data.table(GaltonFamilies)

#First convert all heights to cms (because I don't use inches)
inch_to_cm <- 2.54
dt_heights[, father := father * inch_to_cm]
dt_heights[, mother := mother * inch_to_cm]
dt_heights[, midparentHeight := midparentHeight * inch_to_cm]
dt_heights[, childHeight := childHeight * inch_to_cm]

#Create the model
lm_model <- lm(data = dt_heights, formula = childHeight ~ father + mother + gender)

shinyServer(function(input, output) {
    output$parentPlot <- renderPlotly({
        plot_data_parents <- data.table(individual = c("Father", "Mother"),
                                        height = c(input$slider_father, input$slider_mother))
        plot_data_parents$individual <- factor(plot_data_parents$individual, levels = plot_data_parents$individual)
        
        fig <- plot_ly(
            data = plot_data_parents,
            x = ~individual,
            y = ~height,
            type = "bar",
            color = ~individual,
            colors = c("#2da9d9", "#b73377"),
            hoverinfo = "text", text = ~paste0(individual,
                                               "\nHeight: ", height, " cm")
        )
        fig <- fig %>% layout(
            title = "<b>Parents</b>",
            xaxis = list(
                title = "Individual"
            ),
            yaxis = list(
                title = "Height (cm)",
                range = c(100, 220)
            ),    
            showlegend = FALSE
        )
        
        fig
    })
    
    output$childrenPlot <- renderPlotly({
        #Predict new values
        dt_new_height <- data.table(father = input$slider_father,
                                    mother = input$slider_mother,
                                    gender = c("male", "female"))
        prediction <- predict.lm(object = lm_model, newdata = dt_new_height, interval = "prediction")
        prediction <- as.data.table(prediction)
        prediction[, gender := c("male", "female")]
        prediction[, individual := c("Son", "Daughter")]
        prediction[, colors := c("#2da9d9", "#b73377")]
        prediction[, sd := round(fit - lwr, 2)]
        prediction[, fit := round(fit, 2)]
        
        if (input$radio_gender != "both") {
            prediction <- prediction[gender == input$radio_gender]
        }
        
        prediction$individual <- factor(prediction$individual, levels = prediction$individual)
        

        
        fig2 <- plot_ly(
            data = prediction,
            x = ~individual,
            y = ~fit,
            error_y = list(array = ~sd,
                           color = "black"),            
            type = "bar",
            color = ~individual,
            colors = ~colors,
            hoverinfo = "text", text = ~paste0(individual,
                                               "\nHeight: ", fit, " +/- ", sd ," cm")      
        )
        fig2 <- fig2 %>% layout(
            title = "<b>Children</b>",
            xaxis = list(
                title = "Individual"
            ),
            yaxis = list(
                title = "Height (cm)",
                range = c(100, 220)
            ),    
            showlegend = FALSE
        )
        
        fig2
    })
})
