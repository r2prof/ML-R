# CH-03 - Classifying based on similarities with 
#         k-nearest neighbors

# Installing and loading packages----
# install.packages("mlr", dependencies = TRUE) 
# only needed once on any R installation
# library(mlr3)
library(mlr)

library(tidyverse)

# Loading diabetes data ----
data(diabetes, package = "mclust")

diabetesTib <- as_tibble(diabetes)

summary(diabetesTib)

diabetesTib

# Plot the relationships in the data----
ggplot(diabetesTib, aes(glucose, insulin, col = class)) + 
  geom_point()  +
  theme_bw()

ggplot(diabetesTib, aes(sspg, insulin, col = class)) + 
  geom_point() +
  theme_bw()

ggplot(diabetesTib, aes(sspg, glucose, col = class)) + 
  geom_point() +
  theme_bw()

# Defining the diabetes task ----
diabetesTask <- makeClassifTask(data = diabetesTib, target = "class")

diabetesTask

# Defining the knn learner ----
knn <- makeLearner("classif.knn", par.vals = list("k" = 2))

# Listing all of mlr's learners ----
listLearners()$class

# or list them by function:
listLearners("classif")$class

listLearners("regr")$class

listLearners("cluster")$class

# Define model ----
knnModel <- train(knn, diabetesTask)

# Testing performance on training data (very bad practice) ----
knnPred <- predict(knnModel, newdata = diabetesTib)

# mean misclassification error; and accuracy.

# MMCE is simply the proportion of cases classified as a class other than 
# their true class. 

# Accuracy is the opposite of this: the proportion of cases that were 
# correctly classified by the model. 

# The two values add up to 1.00:
performance(knnPred, measures = list(mmce, acc))

# Performance Hold-out cross validation ----
holdout <- makeResampleDesc(method = "Holdout", split = 2/3, 
                            stratify = TRUE)

holdoutCV <- resample(learner = knn, task = diabetesTask, 
                      resampling = holdout,
                      measures = list(mmce, acc))

holdoutCV$aggr

calculateConfusionMatrix(holdoutCV$pred, relative = TRUE)

# Performing repeated k-fold cross-validation ----
kFold <- makeResampleDesc(method = "RepCV", folds = 10, reps = 50, 
                          stratify = TRUE)

kFoldCV <- resample(learner = knn, task = diabetesTask, 
                    resampling = kFold, measures = list(mmce, acc))

kFoldCV$aggr

kFoldCV$measures.test

calculateConfusionMatrix(kFoldCV$pred, relative = TRUE)

# Performing leave-one-out cross validation ----
LOO <- makeResampleDesc(method = "LOO")

LOOCV <- resample(learner = knn, task = diabetesTask, resampling = LOO,
                  measures = list(mmce, acc))

LOOCV$aggr

calculateConfusionMatrix(LOOCV$pred, relative = TRUE)

# Hyperparameter tuning of k ----
knnParamSpace <- makeParamSet(makeDiscreteParam("k", values = 1:10))

gridSearch <- makeTuneControlGrid()

cvForTuning <- makeResampleDesc("RepCV", folds = 10, reps = 20)

tunedK <- tuneParams("classif.knn", task = diabetesTask, 
                     resampling = cvForTuning, 
                     par.set = knnParamSpace, 
                     control = gridSearch)

tunedK

tunedK$x

knnTuningData <- generateHyperParsEffectData(tunedK)

plotHyperParsEffect(knnTuningData, x = "k", y = "mmce.test.mean",
                    plot.type = "line") +
                    theme_bw()

# Training final model with tuned k ----
tunedKnn <- setHyperPars(makeLearner("classif.knn"), par.vals = tunedK$x)

tunedKnnModel <- train(tunedKnn, diabetesTask)

# Including hyperparameter tuning inside nested cross-validationi ----
inner <- makeResampleDesc("CV")

outer <- makeResampleDesc("RepCV", folds = 10, reps = 5)

knnWrapper <- makeTuneWrapper("classif.knn", resampling = inner, 
                              par.set = knnParamSpace, 
                              control = gridSearch) 

cvWithTuning <- resample(knnWrapper, diabetesTask, resampling = outer)

cvWithTuning

# Using the model to make predictions ----
newDiabetesPatients <- tibble(glucose = c(82, 108, 300), 
                              insulin = c(361, 288, 1052),
                              sspg = c(200, 186, 135))

newDiabetesPatients

newPatientsPred <- predict(tunedKnnModel, newdata = newDiabetesPatients)

getPredictionResponse(newPatientsPred)

# Exercises ----
# 1
ggplot(diabetesTib, aes(glucose, insulin, 
                        shape = class)) + 
  geom_point()  +
  theme_bw()

ggplot(diabetesTib, aes(glucose, insulin, 
                        shape = class, col = class)) + 
  geom_point()  +
  theme_bw()

# 2
holdoutNoStrat <- makeResampleDesc(method = "Holdout", split = 0.9, 
                            stratify = FALSE)

# 3
kFold500 <- makeResampleDesc(method = "RepCV", folds = 3, reps = 500, 
                          stratify = TRUE)

kFoldCV500 <- resample(learner = knn, task = diabetesTask, 
                    resampling = kFold500, measures = list(mmce, acc))

kFold5 <- makeResampleDesc(method = "RepCV", folds = 3, reps = 5, 
                             stratify = TRUE)

kFoldCV5 <- resample(learner = knn, task = diabetesTask, 
                       resampling = kFold5, measures = list(mmce, acc))

kFoldCV500$aggr
kFoldCV5$aggr

calculateConfusionMatrix(kFoldCV$pred, relative = TRUE)

# 4
makeResampleDesc(method = "LOO", stratify = TRUE)

makeResampleDesc(method = "LOO", reps = 5)

# both will result in an error as LOO cross-validation cannot
# be stratified or repeated

# 5
data(iris)

irisTask <- makeClassifTask(data = iris, target = "Species")

knnParamSpace <- makeParamSet(makeDiscreteParam("k", values = 1:25))

gridSearch <- makeTuneControlGrid()

cvForTuning <- makeResampleDesc("RepCV", folds = 10, reps = 20)

tunedK <- tuneParams("classif.knn", task = irisTask, 
                     resampling = cvForTuning, 
                     par.set = knnParamSpace, 
                     control = gridSearch)

tunedK

tunedK$x

knnTuningData <- generateHyperParsEffectData(tunedK)

plotHyperParsEffect(knnTuningData, x = "k", y = "mmce.test.mean",
                    plot.type = "line") +
                    theme_bw()

tunedKnn <- setHyperPars(makeLearner("classif.knn"), par.vals = tunedK$x)

tunedKnnModel <- train(tunedKnn, irisTask)

# 6
inner <- makeResampleDesc("CV")

outerHoldout <- makeResampleDesc("Holdout", split = 2/3, stratify = TRUE)

knnWrapper <- makeTuneWrapper("classif.knn", resampling = inner, 
                              par.set = knnParamSpace, 
                              control = gridSearch) 

holdoutCVWithTuning <- resample(knnWrapper, irisTask, 
                                resampling = outerHoldout)

holdoutCVWithTuning

# 7
outerKfold <- makeResampleDesc("CV", iters = 5, stratify = TRUE)

kFoldCVWithTuning <- resample(knnWrapper, irisTask, 
                              resampling = outerKfold)

kFoldCVWithTuning

resample(knnWrapper, irisTask, resampling = outerKfold)

# repeat each validation procedure 10 times and save the mmce value 
# WARNING: this may take a few minutes to complete

kSamples <- map_dbl(1:10, ~resample(
  knnWrapper, irisTask, resampling = outerKfold)$aggr
)

hSamples <- map_dbl(1:10, ~resample(
  knnWrapper, irisTask, resampling = outerHoldout)$aggr
)

hist(kSamples, xlim = c(0, 0.11))
hist(hSamples, xlim = c(0, 0.11))

# holdout CV gives more variable estimates of model performance 