library(data.table)
library(HistData)

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

summary(lm_model)

#Predict new value
dt_new_height <- data.table(father = 180,
                            mother = 170,
                            gender = c("male", "female"))
prediction <- predict.lm(object = lm_model, newdata = dt_new_height, interval = "prediction")
prediction <- as.data.table(prediction)
prediction[, gender := c("male", "female")]
prediction[, gender_aux := c("Son", "Daughter")]
prediction[, sd := fit - lwr]

#Plot
library(plotly)

#Parents
plot_data_parents <- data.table(individual = c("Father", "Mother"),
                                height = c(180, 170))
plot_data_parents$individual <- factor(plot_data_parents$individual, levels = plot_data_parents$individual)

fig <- plot_ly(
    data = plot_data_parents,
    x = ~individual,
    y = ~height,
    type = "bar",
    color = ~individual,
    colors = c("#2da9d9", "#b73377")
)
fig <- fig %>% layout(
    title = "<b>Parents</b>",
    xaxis = list(
        title = "Individual"
    ),
    yaxis = list(
        title = "Height (cm)",
        range = c(0, 220)
    ),    
    showlegend = FALSE
)

fig

#Children
plot_data_children <- data.table(individual = c("Son", "Daughter"),
                                 height = c(154, 167),
                                 sd = c(10, 20))
plot_data_children$individual <- factor(plot_data_children$individual, levels = plot_data_children$individual)

fig2 <- plot_ly(
    data = plot_data_children,
    x = ~individual,
    y = ~height,
    error_y = list(array = ~sd,
                   color = "black"),
    type = "bar",
    color = ~individual,
    colors = c("#2da9d9", "#b73377")
)
fig2 <- fig2 %>% layout(
    title = "<b>Children</b>",
    xaxis = list(
        title = "Individual"
    ),
    yaxis = list(
        title = "Height (cm)",
        range = c(0, 220)
    ),    
    showlegend = FALSE
)

fig2
