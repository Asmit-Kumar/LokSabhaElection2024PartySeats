library(randomForest)

df <- read.csv('ElectionData.csv')

years <- c(1999, 2004, 2009, 2014, 2019, 2024)

features <- c('Year', 'Party', 'ExitPoll', 'Actual')

bjp_poll <- df[df$Party == 'BJP', ]$ExitPoll

df_filtered <- df[features]

train_data <- df[features]

test_data <- train_data[train_data$Year == 2019, ]

test_result <- c(test_data$Actual)


parties <- test_data$Party

train_data <- train_data[train_data$Year !=  2024 & train_data$Year !=  2019, ]
train_data <- rbind(train_data, df_filtered[df_filtered$Year == 2019 & 
                                              df_filtered$Party == 'BJP',])

bjp_actual <- train_data[train_data$Party == 'BJP', ]$Actual

target <- "Actual"

target_features <- c('Year', 'Party', 'ExitPoll')

test_data <- test_data[target_features]

pred_data <- df[target_features]

pred_data <- pred_data[pred_data$Year == 2024, ]

# Fit random forest model
model <- randomForest(Actual ~ Party + ExitPoll, data = train_data)

# Testing
test <- round(predict(model, newdata = test_data))
test <- unname(test)
test

# Make predictions on new data
predictions <- unname(round(predict(model, newdata = pred_data)))
predictions 

#Graphs
bjp_actual <- c(bjp_actual, predictions[1])
bjp_actual


plot(years, bjp_actual, type = 'o', col = 'red', ylab = 'Bjp Seats')
lines(years, bjp_poll, type = 'o', col = 'blue')

results <- data.frame(Parties = parties,Seats = predictions)
results
barplot(results$Seats, names.arg = results$Parties, col = c('orange', 'darkgreen', 
        'grey', 'green', 'red', 'blue', 'yellow'))
