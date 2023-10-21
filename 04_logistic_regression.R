# CH-04-----

# Load Packages----
library(mlr)
library(tidyverse)

# Load data-----
install.packages("titanic")

data(titanic_train, package = "titanic")

titanicTib <- as_tibble(titanic_train)

titanicTib

# Clean data----
fctrs <- c("Survived", "Sex", "Pclass")

titanicClean <- titanicTib |>
  mutate_at(.vars = fctrs, .funs = factor) |>
  mutate(FamSize = SibSp + Parch) |>
  select(Survived, Pclass, Sex, Age, Fare, FamSize)

titanicClean

# Plot data----
titanicUntidy <- gather(titanicClean, key = "Variable", value = "Value", 
                        -Survived)
titanicUntidy 

titanicUntidy %>%
  filter(Variable != "Pclass" & Variable != "Sex") %>%
  ggplot(aes(Survived, as.numeric(Value))) +
  facet_wrap(~ Variable, scales = "free_y") +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  theme_bw()

titanicUntidy %>%
  filter(Variable == "Pclass" | Variable == "Sex") %>%
  ggplot(aes(Value, fill = Survived)) +
  facet_wrap(~ Variable, scales = "free_x") +
  geom_bar(position = "fill") +
  theme_bw()

# Create task and learner, and train the model----
titanicTask <- makeClassifTask(data = titanicClean, target = "Survived")

logReg <- makeLearner("classif.logreg", predict.type = "prob")

logRegModel <- train(logReg, titanicTask)

# Count missing values in Age variable ----
titanicClean$Age

sum(is.na(titanicClean$Age))

# Impute missing values ----
imp <- impute(titanicClean, cols = list(Age = imputeMean()))

sum(is.na(titanicClean$Age))

sum(is.na(imp$data$Age))

# Create task with imputed data and train model ----
titanicTask <- makeClassifTask(data = imp$data, target = "Survived")

logRegModel <- train(logReg, titanicTask)

# Wrap Learner ----
logRegWrapper <- makeImputeWrapper("classif.logreg",
                                   cols = list(Age = imputeMean()))
logRegWrapper

# Cross-Validate ----
kFold <- makeResampleDesc(method = "RepCV", folds = 10, reps = 50, 
                          stratify = TRUE)

logRegwithImpute <- resample(logRegWrapper, titanicTask, resampling = kFold, 
                             measures = list(acc, fpr, fnr))
logRegwithImpute

# Extract odds ratios
logRegModelData <- getLearnerModel(logRegModel)

coef(logRegModelData)

exp(cbind(Odds_Ratio = coef(logRegModelData), confint(logRegModelData)))

# Using the model to make predictions ----
data(titanic_test, package = "titanic")

titanicNew <- as_tibble(titanic_test)

titanicNewClean <- titanicNew %>%
  mutate_at(.vars = c("Sex", "Pclass"), .funs = factor) %>%
  mutate(FamSize = SibSp + Parch) %>%
  select(Pclass, Sex, Age, Fare, FamSize)

predict(logRegModel, newdata = titanicNewClean)

# Exercises ----
# 1
titanicUntidy %>%
  filter(Variable != "Pclass" & Variable != "Sex") %>%
  ggplot(aes(Survived, as.numeric(Value))) +
  facet_wrap(~ Variable, scales = "free_y") +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_point(alpha = 0.05, size = 3) +
  theme_bw()

# 2
titanicUntidy %>%
  filter(Variable == "Pclass" | Variable == "Sex") %>%
  ggplot(aes(Value, fill = Survived)) +
  facet_wrap(~ Variable, scales = "free_x") +
  geom_bar(position = "dodge") +
  theme_bw()

titanicUntidy %>%
  filter(Variable == "Pclass" | Variable == "Sex") %>%
  ggplot(aes(Value, fill = Survived)) +
  facet_wrap(~ Variable, scales = "free_x") +
  geom_bar(position = "stack") +
  theme_bw()

# 3
titanicNoFare <- select(titanicClean, -Fare)
titanicNoFareTask <- makeClassifTask(data = titanicNoFare, 
                                     target = "Survived")
logRegNoFare <- resample(logRegWrapper, titanicNoFareTask, 
                         resampling = kFold, 
                         measures = list(acc, fpr, fnr))
logRegNoFare

# 4
surnames <- map_chr(str_split(titanicTib$Name, "\\."), 1)

salutations <- map_chr(str_split(surnames, ", "), 2)

salutations[!(salutations %in% c("Mr", "Dr", "Master", 
                                 "Miss", "Mrs", "Rev"))] <- "Other"
# 5
fctrsInclSals <- c("Survived", "Sex", "Pclass", "Salutation")

titanicWithSals <- titanicTib %>%
  mutate(FamSize = SibSp + Parch, Salutation = salutations) %>%
  mutate_at(.vars = fctrsInclSals, .funs = factor) %>%
  select(Survived, Pclass, Sex, Age, Fare, FamSize, Salutation)

titanicTaskWithSals <- makeClassifTask(data = titanicWithSals, 
                                       target = "Survived")

logRegWrapper <- makeImputeWrapper("classif.logreg",
                                   cols = list(Age = imputeMean()))

kFold <- makeResampleDesc(method = "RepCV", folds = 10, reps = 50, 
                          stratify = TRUE)

logRegWithSals <- resample(logRegWrapper, titanicTaskWithSals, 
                           resampling = kFold, 
                           measures = list(acc, fpr, fnr))
logRegWithSals

