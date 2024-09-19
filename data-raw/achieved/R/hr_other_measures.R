#------------------------------------------------------------------------------
#title:  "Random Forests for Handwriter: measures"
#author: "Maureen Gallagher"
#Purpose "To find measures other than Euclidian Distance,that are effective at
#           prediction.
#------------------------------------------------------------------------------


library(lsa)


#==============================================================================

train_writers_all <- read.csv("train_writers_other_meas.csv")
test_writers2     <- read.csv("test_writers_other_meas.csv")

#==============================================================================

calc_mean_acc <- function(formula) {
  accuracies <- c()
  for (i in seq(1, 1000, by=1)) {
    
    rf1 <- randomForest(formula, data=train_writers_all, ntree=200)
    
    prediction_writers2rf1 <- predict(rf1, newdata = subset(test_writers2,
                                                              select=-c(y)))
    
    cm1 <- confusionMatrix(as.factor(test_writers2[,"y"]), prediction_writers2rf1)
    
    accuracies <- c(accuracies, cm1$overall[1])
  }
  return(mean(accuracies))
}


formula1a <- as.formula(
  paste("factor(y) ~",
        paste(c(paste("c", c(1:40), sep=""), 'euch_dist'),
              collapse= "+")))
ma_1a <- calc_mean_acc(formula1a)


formula1b <- as.formula(
  paste("factor(y) ~",
        paste(c(paste("c", c(1:40), sep=""), 'cos_similarity'),
              collapse= "+")))
ma_1b <- calc_mean_acc(formula1b)


formula1c <- as.formula(
  paste("factor(y) ~",
        paste(c(paste("c", c(1:40), sep=""), 'spearman'),
              collapse= "+")))
ma_1c <- calc_mean_acc(formula1c)


formula1ab <- as.formula(
  paste("factor(y) ~",
        paste(c(paste("c", c(1:40), sep=""), 'euch_dist', 'cos_similarity'),
              collapse= "+")))
ma_1ab <- calc_mean_acc(formula1ab)


formula1ac <- as.formula(
  paste("factor(y) ~",
        paste(c(paste("c", c(1:40), sep=""), 'euch_dist', 'spearman'),
              collapse= "+")))
ma_1ac <- calc_mean_acc(formula1ac)


formula1bc <- as.formula(
  paste("factor(y) ~",
        paste(c(paste("c", c(1:40), sep=""), 'cos_similarity', 'spearman'),
              collapse= "+")))
ma_1bc <- calc_mean_acc(formula1bc)


formula1all <- as.formula(
  paste("factor(y) ~",
        paste(c(paste("c", c(1:40), sep=""),
                'euch_dist',
                'cos_similarity',
                'spearman'),
              collapse= "+")))
ma_1all <- calc_mean_acc(formula1all)



ma_1a   #0.9389976
ma_1ab  #0.9489608
ma_1c   #0.9507616
ma_1b   #0.9531843
ma_1bc  #0.9605812
ma_1all #0.9622831
ma_1ac  #0.96356
