#------------------------------------------------------------------------------
#title: "Random Forests for Handwriter test method 2"
#author: "Maureen Gallagher"
#purpose: To find the most accurate methods
#------------------------------------------------------------------------------


library("tidyverse")
library("randomForest")
library("gtools")
library("caret")


#------------------------------------------------------------------------------


# Read in the datasets
#   See other file to create .csv's

train_writers_all <- read.csv("train_writers_other_meas.csv")
test_writers2     <- read.csv("test_writers_other_meas.csv")


#==============================================================================

# Currently Method 1, which uses y as factor
#   and uses all 40 clusters plus the Euclidean Distance,
#   is most accurate.

# The goal is to find a Method 2, which uses y as factor
#   but has fewer explanatory variables than Method 1,
#   that is comparable in accuracy to Method 1
# Method 1 typically has accuracy of 95%


# Method 2 will be found by generating several random forests and obtaining
#   importances for each variable, then removing the consistently least
#   important variable then starting the process 

#==============================================================================

find_formula <- function(vars_orig, save_name) {
  # Dataframe to hold all formulae
  formulae <- c()
  vars     <- vars_orig
  
  # Systematically removing least important variables
  while(length(vars) >= 1) {
    n <- length(vars)
    
    # Formula
    formula2 <- as.formula(paste("factor(y) ~", paste(vars, collapse= "+")))
    formulae <- c(formulae, formula2)
    
    
    # Dataframe to store the importances for each variable
    importances <- data.frame(matrix(nrow = 0, ncol = n))
    
    for (i in seq(1, 100, by=1)) {
      rf2 <- randomForest(formula2, data=train_writers_all, ntree=200)
      
      importances <- rbind(importances, importance(rf2)[1:n])
    }
    
    colnames(importances) <- vars
    importances$min_var <- apply(importances,
                                 1,
                                 function(row) names(importances)[which.min(row)])
    
    # Counts of vars with min importances
    min_counts <- table(importances$min_var)
    
    # Removes variable of least importance
    vars <- vars[vars != names(min_counts)[which.max(min_counts)]]
  }
  
  
  # calculate accuracy for each formula
  df_accuracies <- data.frame(matrix(nrow = 0, ncol = length(vars_orig)))
  
  for (j in seq(1, 100, by=1)) {
    accuracies <- c()
    
    for (i in seq(1, length(formulae), by=1)) {
      formula2 <- as.formula(formulae[[i]])
      
      rf2 <- randomForest(formula2, data=train_writers_all, ntree=200)
      prediction_writers2rf2 <- predict(rf2, newdata = subset(test_writers2,
                                                              select=-c(y)))
      cm <- confusionMatrix(as.factor(test_writers2[,"y"]),
                            prediction_writers2rf2)
      
      accuracies <- c(accuracies, as.numeric(cm$overall[1]))
    }
    
    df_accuracies <- rbind(df_accuracies, accuracies)
  }
  
  save(formulae, df_accuracies, file = save_name)
}

#==============================================================================


vars <- c(paste("c", c(1:40), sep=""),
          'euch_dist')
find_formula(vars, "method2_form_and_acc.RData")
load("method2_form_and_acc.RData")
formulae1   <- formulae
accuracies1 <- df_accuracies


vars <- c(paste("c", c(1:40), sep=""),
          'euch_dist',
          'cos_similarity')
find_formula(vars, "method2_cos_form_and_acc.RData")
load("method2_cos_form_and_acc.RData")
formulae2   <- formulae
accuracies2 <- df_accuracies


vars <- c(paste("c", c(1:40), sep=""),
          'euch_dist',
          'cos_similarity',
          'spearman')
find_formula(vars, "method2_all_form_and_acc.RData")
load("method2_all_form_and_acc.RData")
formulae
colnames(df_accuracies) <- c(1:43)
colMeans(df_accuracies)

formulae3   <- formulae
accuracies3 <- df_accuracies


as.numeric(colMeans(accuracies1))
as.numeric(colMeans(accuracies2))
as.numeric(colMeans(accuracies3))

formulae1[20]
formulae2[37]
formulae3[39]


formulae3[34]
formulae3[38]
