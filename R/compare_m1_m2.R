#------------------------------------------------------------------------------
#title: "Compare Handwriter Methods 1 and 2"
#author: "Maureen Gallagher"
#------------------------------------------------------------------------------


library("tidyverse")
library("randomForest")
library("gtools")
library("caret")



#train_data test_data accuracy_method1 accuracy_method2f1 accuracy_method2f2
#comparison <- data.frame(matrix(nrow = 0, ncol = 4))


#------------------------------------------------------------------------------


compare_formulae <- function(train_name, test_name, df_comparison) {
  train_file <- paste("train_writers_", train_name, ".csv", sep="")
  test_file  <- paste("test_writers_", train_name, ".csv", sep="")
  train_writers_all <- read.csv(train_file)
  test_writers2     <- read.csv(test_file)
  
  formula1 <- as.formula(
    paste("factor(y) ~",
          paste(c(paste("c", c(1:40), sep=""),
                  'euch_dist',
                  'cos_similarity',
                  'spearman'),
                collapse= "+")))
  rf1 <- randomForest(formula1, data=train_writers_all, ntree=200)
  
  formula2f1 <- as.formula("factor(y) ~ c3 + c27 + c34 + euch_dist + cos_similarity + spearman")
  rf2f1 <- randomForest(formula2f1, data=train_writers_all, ntree=200)
  
  formula2f2 <- as.formula("factor(y) ~ c3 + c5 + c8 + c27 + c29 + c31 + c34 + euch_dist + cos_similarity + spearman")
  rf2f2 <- randomForest(formula2f2, data=train_writers_all, ntree=200)
  
  prediction_writers2rf1 <- predict(rf1, newdata = subset(test_writers2,
                                                          select=-c(y)))
  prediction_writers2rf2f1 <- predict(rf2f1, newdata = subset(test_writers2,
                                                              select=-c(y)))
  prediction_writers2rf2f2 <- predict(rf2f2, newdata = subset(test_writers2,
                                                              select=-c(y)))
  
  c1   <- confusionMatrix(as.factor(test_writers2[,"y"]), prediction_writers2rf1)
  c2f1 <- confusionMatrix(as.factor(test_writers2[,"y"]), prediction_writers2rf2f1)
  c2f2 <- confusionMatrix(as.factor(test_writers2[,"y"]), prediction_writers2rf2f2)
  
  df_comparison <- rbind(df_comparison, c(train_name,
                                          test_name,
                                          unname(c1$overall[1]),
                                          unname(c2f1$overall[1]),
                                          unname(c2f2$overall[1])))
  return(df_comparison)
}
#------------------------------------------------------------------------------

#train_data test_data accuracy_method1 accuracy_method2f1 accuracy_method2f2
comparison <- data.frame(matrix(nrow = 0, ncol = 4))

data_names <- c("LND", "WOZ", "PHR", "LND_WOZ", "all")

for (i in seq(1, length(data_names), by=1)) {
  for (j in seq(1, length(data_names), by=1)) {
    comparison <- compare_formulae(data_names[i], data_names[j], comparison)
  }
}


colnames(comparison) <- c("train_data",
                          "test_data",
                          "accuracy_m1",
                          "accuracy_m2f1",
                          "accuracy_m2f2")

#comparison
#save(comparison, file = "compare_m1_m2.RData")
load("compare_m1_m2.RData")
comparison






#------------------------------------------------------------------------------
#title: "Compare Handwriter Methods 1 and 2"
#author: "Maureen Gallagher"
#------------------------------------------------------------------------------


library("tidyverse")
library("randomForest")
library("gtools")
library("caret")


#------------------------------------------------------------------------------


compare_formulae <- function(train_name, test_name, df_comparison) {
  train_file <- paste("train_writers_", train_name, ".csv", sep="")
  test_file  <- paste("test_writers_", train_name, ".csv", sep="")
  train_writers_all <- read.csv(train_file)
  test_writers2     <- read.csv(test_file)
  
  formula1 <- as.formula(
    paste("factor(y) ~",
          paste(c(paste("c", c(1:40), sep=""),
                  'euch_dist'),
                collapse= "+")))
  rf1 <- randomForest(formula1, data=train_writers_all, ntree=200)
  
  formula2f1 <- as.formula(
    paste("factor(y) ~",
          paste(c(paste("c", c(1:40), sep=""),
                  'euch_dist',
                  'cos_similarity'),
                collapse= "+")))
  rf2f1 <- randomForest(formula2f1, data=train_writers_all, ntree=200)
  
  formula2f2 <- as.formula(
    paste("factor(y) ~",
          paste(c(paste("c", c(1:40), sep=""),
                  'euch_dist',
                  'spearman'),
                collapse= "+")))
  rf2f2 <- randomForest(formula2f2, data=train_writers_all, ntree=200)
  
  formula2f3 <- as.formula(
    paste("factor(y) ~",
          paste(c(paste("c", c(1:40), sep=""),
                  'euch_dist',
                  'cos_similarity',
                  'spearman'),
                collapse= "+")))
  rf2f3 <- randomForest(formula2f3, data=train_writers_all, ntree=200)
  
  prediction_writers2rf1 <- predict(rf1, newdata = subset(test_writers2,
                                                          select=-c(y)))
  prediction_writers2rf2f1 <- predict(rf2f1, newdata = subset(test_writers2,
                                                              select=-c(y)))
  prediction_writers2rf2f2 <- predict(rf2f2, newdata = subset(test_writers2,
                                                              select=-c(y)))
  prediction_writers2rf2f3 <- predict(rf2f3, newdata = subset(test_writers2,
                                                              select=-c(y)))
  
  c1   <- confusionMatrix(as.factor(test_writers2[,"y"]), prediction_writers2rf1)
  c2f1 <- confusionMatrix(as.factor(test_writers2[,"y"]), prediction_writers2rf2f1)
  c2f2 <- confusionMatrix(as.factor(test_writers2[,"y"]), prediction_writers2rf2f2)
  c2f3 <- confusionMatrix(as.factor(test_writers2[,"y"]), prediction_writers2rf2f2)
  
  df_comparison <- rbind(df_comparison, c(train_name,
                                          test_name,
                                          unname(c1$overall[1]),
                                          unname(c2f1$overall[1]),
                                          unname(c2f2$overall[1]),
                                          unname(c2f3$overall[1])))
  return(df_comparison)
}
#------------------------------------------------------------------------------

#train_data test_data accuracy_method1 accuracy_method2f1 accuracy_method2f2
comparison <- data.frame(matrix(nrow = 0, ncol = 0))

data_names <- c("LND", "WOZ", "PHR")

for (i in seq(1, length(data_names), by=1)) {
  for (j in seq(1, length(data_names), by=1)) {
    comparison <- compare_formulae(data_names[i], data_names[j], comparison)
  }
}

comparison <- compare_formulae("LND_WOZ", "LND_WOZ", comparison)
comparison <- compare_formulae("all", "all", comparison)



colnames(comparison) <- c("train_data",
                          "test_data",
                          "euch",
                          "euch_cos",
                          "euch_spear",
                          "euch_cos_spear")

#comparison
save(comparison, file = "compare_dist_meas.RData")
load("compare_dist_meas.RData")
comparison
