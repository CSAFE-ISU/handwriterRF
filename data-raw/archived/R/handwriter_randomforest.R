#------------------------------------------------------------------------------
# title: "Random Forests for Handwriter"
# author: "Maureen Gallagher"
#------------------------------------------------------------------------------


library("tidyverse")
library("randomForest")
library("gtools")
library("caret")


#------------------------------------------------------------------------------


# Read in the datasets
#   See other file to create .csv's

train_writers_all <- read.csv("train_writers_LND.csv")
test_writers2 <- read.csv("test_writers_LND.csv")


# ==============================================================================


# Random forest

#------------------------------------------------------------------------------

# Method 1
#   - using all clusters and euclidian distance
#   - y as factor

formula1 <- as.formula(
  paste(
    "factor(y) ~",
    paste(
      c(
        paste("c", c(1:40), sep = ""),
        "euch_dist",
        "cos_similarity",
        "spearman"
      ),
      collapse = "+"
    )
  )
)

rf1 <- randomForest(formula1, data = train_writers_all, ntree = 200)

rf1
importance(rf1)
plot(rf1)
varImpPlot(rf1, main = "Variable Importance in Random Forest")

#------------------------------------------------------------------------------

# Method 2
#   - using most important features from method 1
#   - y as factor
# See other file for determining these features
# factor(y) ~ c3 + c5 + c8 + c27 + c29 + c31 + c34 + euch_dist + cos_similarity + spearman
# factor(y) ~ c3 + c27 + c34 + euch_dist + cos_similarity + spearman

formula2 <- as.formula("factor(y) ~ c3 + c27 + c34 + euch_dist + cos_similarity + spearman")

rf2 <- randomForest(formula2, data = train_writers_all, ntree = 200)

rf2
importance(rf2)
plot(rf2)
varImpPlot(rf2, main = "Variable Importance in Random Forest")


# ==============================================================================


prediction_writers2rf1 <- predict(rf1, newdata = subset(test_writers2,
  select = -c(y)
))

prediction_writers2rf2 <- predict(rf2, newdata = subset(test_writers2,
  select = -c(y)
))


table(test_writers2[, "y"], prediction_writers2rf1)
table(test_writers2[, "y"], prediction_writers2rf2)

confusionMatrix(as.factor(test_writers2[, "y"]), prediction_writers2rf1)
confusionMatrix(as.factor(test_writers2[, "y"]), prediction_writers2rf2)

# ==============================================================================

# Compare effectiveness of methods with PHR data

train_writers_all <- read.csv("train_writers_PHR.csv")
test_writers2 <- read.csv("test_writers_PHR.csv")

#------------------------------------------------------------------------------

# Method 1
#   - using all clusters and euclidian distance
#   - y as factor

formula1 <- as.formula(
  paste(
    "factor(y) ~",
    paste(
      c(
        paste("c", c(1:40), sep = ""),
        "euch_dist",
        "cos_similarity",
        "spearman"
      ),
      collapse = "+"
    )
  )
)

rf1 <- randomForest(formula1, data = train_writers_all, ntree = 200)

rf1
importance(rf1)
plot(rf1)
varimportanceplot <- varImpPlot(rf1, main = "Variable Importance in Random Forest")
varimportanceplot

#------------------------------------------------------------------------------

# Method 2
#   - using most important features from method 1
#   - y as factor
# See other file for determining these features

# factor(y) ~ c3 + c27 + c34 + euch_dist + cos_similarity + spearman
# factor(y) ~ c3 + c5 + c8 + c27 + c29 + c31 + c34 + euch_dist + cos_similarity + spearman

formula2 <- as.formula("factor(y) ~ c3 + c31 + c34 + euch_dist + cos_similarity + spearman")

rf2 <- randomForest(formula2, data = train_writers_all, ntree = 200)

rf2
importance(rf2)
plot(rf2)
varimportanceplot <- varImpPlot(rf2, main = "Variable Importance in Random Forest")
varimportanceplot

#------------------------------------------------------------------------------

prediction_writers2rf1 <- predict(rf1, newdata = subset(test_writers2,
  select = -c(y)
))

prediction_writers2rf2 <- predict(rf2, newdata = subset(test_writers2,
  select = -c(y)
))


table(test_writers2[, "y"], prediction_writers2rf1)
table(test_writers2[, "y"], prediction_writers2rf2)

confusionMatrix(as.factor(test_writers2[, "y"]), prediction_writers2rf1)
confusionMatrix(as.factor(test_writers2[, "y"]), prediction_writers2rf2)


# When trained and tested on PHR, both performed worse than on LND
#   (~=.87 and ~=0.84 respectively)
# When trained on LND and tested on PHR, accuracy greatly increased (~=0.96)
#   Model performs well, just not when trained with samples of short writing


# ==============================================================================

# Compare effectiveness of methods with OZ data

train_writers_all <- read.csv("train_writers_OZ.csv")
test_writers2 <- read.csv("test_writers_OZ.csv")

#------------------------------------------------------------------------------

# Method 1
#   - using all clusters and euclidian distance
#   - y as factor

formula1 <- as.formula(
  paste(
    "factor(y) ~",
    paste(c(paste("c", c(1:40), sep = ""), "euch_dist"), collapse = "+")
  )
)

rf1 <- randomForest(formula1, data = train_writers_all, ntree = 200)

rf1
importance(rf1)
plot(rf1)
varimportanceplot <- varImpPlot(rf1, main = "Variable Importance in Random Forest")
varimportanceplot

#------------------------------------------------------------------------------

# Method 2
#   - using most important features from method 1
#   - y as factor
# See other file for determining these features

formula2 <- as.formula("factor(y) ~ c3 + c31 + c34 + euch_dist")

rf2 <- randomForest(formula2, data = train_writers_all, ntree = 200)

rf2
importance(rf2)
plot(rf2)
varimportanceplot <- varImpPlot(rf2, main = "Variable Importance in Random Forest")
varimportanceplot

#------------------------------------------------------------------------------

prediction_writers2rf1 <- predict(rf1, newdata = subset(test_writers2,
  select = -c(y)
))

prediction_writers2rf2 <- predict(rf2, newdata = subset(test_writers2,
  select = -c(y)
))


table(test_writers2[, "y"], prediction_writers2rf1)
table(test_writers2[, "y"], prediction_writers2rf2)

confusionMatrix(as.factor(test_writers2[, "y"]), prediction_writers2rf1)
confusionMatrix(as.factor(test_writers2[, "y"]), prediction_writers2rf2)


# (~=.98 accuracy)



# ==============================================================================

# Compare effectiveness with all prompts

train_writers_all <- read.csv("train_writers_all.csv")
test_writers2 <- read.csv("test_writers_all.csv")

train_writers_all <- read.csv("train_writers_LND_OZ.csv")

#------------------------------------------------------------------------------

# Method 1
#   - using all clusters and euclidian distance
#   - y as factor

formula1 <- as.formula(
  paste(
    "factor(y) ~",
    paste(c(paste("c", c(1:40), sep = ""), "euch_dist"), collapse = "+")
  )
)

rf1 <- randomForest(formula1, data = train_writers_all, ntree = 200)

rf1
importance(rf1)
plot(rf1)
varimportanceplot <- varImpPlot(rf1, main = "Variable Importance in Random Forest")
varimportanceplot

#------------------------------------------------------------------------------

# Method 2
#   - using most important features from method 1
#   - y as factor
# See other file for determining these features

formula2 <- as.formula("factor(y) ~ c3 + c31 + c34 + euch_dist")

rf2 <- randomForest(formula2, data = train_writers_all, ntree = 200)

rf2
importance(rf2)
plot(rf2)
varimportanceplot <- varImpPlot(rf2, main = "Variable Importance in Random Forest")
varimportanceplot

#------------------------------------------------------------------------------

prediction_writers2rf1 <- predict(rf1, newdata = subset(test_writers2,
  select = -c(y)
))

prediction_writers2rf2 <- predict(rf2, newdata = subset(test_writers2,
  select = -c(y)
))


table(test_writers2[, "y"], prediction_writers2rf1)
table(test_writers2[, "y"], prediction_writers2rf2)

confusionMatrix(as.factor(test_writers2[, "y"]), prediction_writers2rf1)
confusionMatrix(as.factor(test_writers2[, "y"]), prediction_writers2rf2)


# When all documents are used to train the forest:
# (~=.88 and ~=.84 accuracy, respectively)

# When LND or OZ is used to train and model is tested on all documents:
# (~=.96)

# When LND and OZ are used to train and model is tested on all documents:
# (~=.96 and ~=.97 accuracy, respectively)


#------------------------------------------------------------------------------
