library(readxl)
library(tidyverse)
library(randomForest)

# Import the waste, disease, and other variables sheets.
WASTE <- read_excel("data.xlsx", sheet = "Waste")
DISEASE <- read_excel("data.xlsx", sheet = "Disease")
OTHER <- read_excel("data.xlsx", sheet = "Other Variables")


# Clean the data sheets
# Remove Place Name Variable across all sheets
WASTE$Name.of.Place  <- NULL
DISEASE$Name.Of.Place <- NULL
OTHER$Name.Of.Place <- NULL

# WASTE: Remove Percentage Variables
WASTE$`B.%` <- NULL
WASTE$`Rec.%` <- NULL
WASTE$`Res.%` <- NULL
WASTE$`S.%` <- NULL

# WASTE: Treat column 2, 3, and 5-14 as logical data.
WASTE <- WASTE %>% mutate(across(c(2:3, 5:14), as.logical))

# WASTE: Treat column 4 as factor data.
WASTE <- WASTE %>% mutate(across(c(4), as.factor))

# OTHER: Remove column 1.
OTHER$Landfill.Site <- NULL

# OTHER: Treat column 2 as logical data.
OTHER <- OTHER %>% mutate(across(c(2), as.logical))


# Prepare the data sets, one for each disease (Thank you Gemini)
dataSets <- map(DISEASE, ~ {
  cbind(WASTE, OTHER) %>%
    as.data.frame() %>%
    mutate(Disease = .x)
})


# Split data for training and testing
set.seed(10000)

trainIndex <- sample(1:50, 0.7 * 50)
trainList <- map(dataSets, ~ .x[trainIndex, ])
testList <- map(dataSets, ~ .x[-trainIndex, ])


# Make a random forest model for each disease
modelList <- trainList %>%
  map(~ randomForest(Disease ~ ., 
                     data = .x, 
                     ntree = 500, 
                     importance = TRUE))


# Print the importance of every variable in every disease
modelList %>% 
  map("importance") %>% 
  print()


# Draw the Variance importance plot per model
modelList %>%
  iwalk(~ varImpPlot(.x, main = paste("Variable Importance:",
                                      gsub("\\.", " ", .y))))
