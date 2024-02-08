library(tidyverse)
library(e1071)
library(caret)
library(readxl)
library(ggplot2)

data <- read_excel("D:/Data for Research/Energy/World Development Indicators/Classification of WDI/WDI_Classification.xlsx")

#Transforming data from wide to long format
data_long <- data %>%
  gather(key = "Year", value = "Consumption", -Country)

data_long$Year <- as.numeric(as.character(data_long$Year))

#Creating quartile classification
data_long <- data_long %>%
  group_by(Country) %>%
  mutate(Quartile = ntile(Consumption, 4))

data_long$Quartile <- factor(data_long$Quartile, levels = 1:4)


#Splitting data into training and testing sets
set.seed(123) # for reproducibility
training_indices <- createDataPartition(data_long$Quartile, p = 0.8, list = FALSE)
training_data <- data_long[training_indices, ]
training_data$Quartile <- factor(training_data$Quartile, levels = 1:4)
testing_data <- data_long[-training_indices, ]
testing_data$Quartile <- factor(testing_data$Quartile, levels = 1:4)


#Tuning SVM parameters using cross-validation
tune_grid <- expand.grid(
  sigma = seq(0.01, 1, by = 0.1),
  C = 2^(2:10)
)

tune_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
svm_tune <- train(
  Quartile ~ ., data = training_data,
  method = "svmRadial",
  tuneGrid = tune_grid,
  preProcess = c("center", "scale"),
  trControl = tune_control
)

#Training the SVM model with the best parameters
svm_model <- svm(
  formula = Quartile ~ .,
  data = training_data,
  type = 'C-classification',
  kernel = 'radial',
  cost = svm_tune$bestTune$C,
  gamma = 1/(svm_tune$bestTune$sigma^2)
)

#Prediction on testing data
predictions <- predict(svm_model, testing_data)

#Evaluating the model performance
confusionMatrix(predictions, testing_data$Quartile)

cm <- confusionMatrix(predictions, testing_data$Quartile)

cm_matrix <- cm$table

cm_long <- as.data.frame(as.table(cm_matrix))

names(cm_long) <- c("Reference", "Prediction", "Freq")

cm_plot <- ggplot(cm_long, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() + 
  geom_text(aes(label = Freq), vjust = 1, size = 6) +  # Increased text size
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Actual", y = "Predicted", fill = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 12))

print(cm_plot)


all_predictions <- predict(svm_model, data_long)

data_long$PredictedQuartile <- factor(all_predictions, levels = levels(data_long$Quartile))

#Now, we determine which countries belong to each classification
#We summarized the data to see the mode predicted quartile for each country
country_classification <- data_long %>%
  group_by(Country) %>%
  summarise(MostCommonQuartile = as.integer(names(which.max(table(PredictedQuartile)))))

print(country_classification)


#Visualizing each country's electricity consumption classification changes over time according to the SVM model

country_year_classification <- data_long %>%
  group_by(Country, Year) %>%
  summarise(PredictedQuartile = as.integer(PredictedQuartile)) %>%
  ungroup()

print(country_year_classification, n = 352)

#Heatmap
heatmap_plot <- ggplot(country_year_classification, aes(x = Year, y = Country, fill = as.factor(PredictedQuartile))) +
  geom_tile() + 
  scale_fill_manual(values = c("red", "blue", "green", "yellow"), 
                    name = "Quartile Classification",
                    labels = c("Quartile 1", "Quartile 2", "Quartile 3", "Quartile 4")) +
  labs(x = "Year", y = "Country", title = "Heatmap of Electricity Consumption Classification") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(heatmap_plot)
