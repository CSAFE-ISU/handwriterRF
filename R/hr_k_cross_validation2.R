#------------------------------------------------------------------------------
#title: "Handwriter k-fold cross validation test"
#author: "Maureen Gallagher"
#------------------------------------------------------------------------------

library(tidyverse)
library(caret)
library(randomForest)
library(gtools)
library(lsa)

#==============================================================================



create_datasets <- function(test_writers, d) {
  samp_test_writers2  <- d |> filter(writer %in% test_writers)
  
  samp_writers2 <- anti_join(d, samp_test_writers2)
  
  # dataset with the documents from only the sampled (training) writers
  d_writers2 <- d %>% filter(writer %in% samp_writers2$writer)
  
  
  # all document combinations of size of the sampled dataset
  pairs_writers2    <- combinations(nrow(d_writers2), 2)
  pairs_writers2_df <- as.data.frame(combinations(nrow(d_writers2), 2)) %>%
    rename(row1 = V1, row2 = V2) %>%
    mutate(row1 = as.numeric(row1), row2 = as.numeric(row2))
  
  
  #count number of combinations from same and different writers
  row_writer    <- as.data.frame(cbind(c(1:dim(d_writers2)[1]),
                                       d_writers2$writer)) %>%
    rename(row=V1, writer=V2) %>%
    #mutate(row=as.numeric(levels(row))[row])
    mutate(row=as.numeric(row))
  
  writers_combo <- left_join(pairs_writers2_df,
                             row_writer,
                             by=c("row1" = "row")) %>% 
                   left_join(row_writer,
                             by=c("row2" = "row"))
  
  same_number   <- writers_combo %>%
    filter(writer.x==writer.y) %>%
    summarise(n())
  
  
  # random sample of the same number of different sources
  diff_sources <- writers_combo %>%
    filter(!writer.x==writer.y) %>%
    sample_n(same_number[[1]])
  
  # create training set with equal number of different source and same source
  train_writers2 <- writers_combo %>%
    filter(writer.x==writer.y) %>%
    rbind(diff_sources)
  
  # pairs to compare based on rows in sampled dataset
  pairs_writers2 <- train_writers2 %>%
    dplyr::select(c("row1", "row2")) %>%
    as.matrix()
  
  
  # distance measures 
  train_writers_all <- calc_dist(d_writers2, pairs_writers2)
  assign("train_writers_all", train_writers_all, envir = .GlobalEnv)
  #----------------------------------------------------------------------------
  
  # From Mattie's Code
  # create a testing dataset
  #samp_test_writers2  <- anti_join(d, samp_writers2)
  pairs_test_writers2 <- combinations(dim(samp_test_writers2)[1], 2)
  test_writers2       <- calc_dist(samp_test_writers2, pairs_test_writers2)
  
  assign("test_writers2", test_writers2, envir = .GlobalEnv)
}

#==============================================================================

calc_dist <- function(data, combinations){
  
  #parameters
  samp    <- data
  pairs   <- combinations
  
  #empty dataset to add to 
  train   <- NULL
  
  #loop that for each combination pair calculates distance measures of interest
  for(i in c(1:dim(pairs)[1])){
    y         <- ifelse(samp[pairs[i, 1], "writer"] ==
                          samp[pairs[i, 2], "writer"],
                        's',
                        'd');
    ab_diffs  <- abs(samp[pairs[i, 1], as.character(c(1:40))] -
                       samp[pairs[i, 2], as.character(c(1:40))]);
    
    # Measures ----------------------------------------------------------------
    euch_dist <- dist(rbind(samp[pairs[i, 1], as.character(c(1:40))],
                            samp[pairs[i, 2], as.character(c(1:40))]));
    
    cos_similarity <- cosine(as.numeric(samp[pairs[i,1],
                                             as.character(c(1:40))]),
                             as.numeric(samp[pairs[i,2],
                                             as.character(c(1:40))]))
    
    spearman <- cor(as.numeric(samp[pairs[i,1], as.character(c(1:40))]),
                    as.numeric(samp[pairs[i,2], as.character(c(1:40))]),
                    method = "spearman")
    
    
    train     <- rbind(train, cbind(ab_diffs,
                                    euch_dist[1],
                                    cos_similarity, 
                                    spearman,
                                    y))
  }
  
  train   <- train %>% rename("euch_dist" = "euch_dist[1]") %>%
    rename_at(vars(c(1:40)), ~paste("c", c(1:40), sep=""))
  return(train)
}

#==============================================================================

# Set formulae
formula1 <- paste("factor(y) ~",
                  paste(c(paste("c", c(1:40), sep=""),
                          'spearman'),
                        collapse= "+"))
formula2 <- paste("factor(y) ~",
                  paste(c(paste("c", c(1:40), sep=""),
                          'cos_similarity',
                          'spearman'),
                        collapse= "+"))
formula3 <- paste("factor(y) ~",
                  paste(c(paste("c", c(1:40), sep=""),
                          'euch_dist',
                          'spearman'),
                        collapse= "+"))
formula4 <- paste("factor(y) ~",
                  paste(c(paste("c", c(1:40), sep=""),
                          'euch_dist',
                          'cos_similarity',
                          'spearman'),
                        collapse= "+"))
formula5 <- "factor(y) ~ c3 + c27 + c34 +
                euch_dist + cos_similarity + spearman"
formula6 <- "factor(y) ~ c3 + c5 + c8 + c27 + c29 + c31 + c34 +
                euch_dist + cos_similarity + spearman"


formulae <- c(
  formula1,
  formula2,
  formula3,
  formula4,
  formula5,
  formula6
)


#==============================================================================

# Read in data
#d <- readRDS("d1.Rdata")
load("cvl_cluster_fill_counts.Rdata")
d <- cluster_fill_counts

# Set value of k (groups)
k <- 5 # recommended for large datasets because this is computationally expensive
writers_per_group <- length(unique(d$writer)) / k

# Shuffle writers
shuffled_writers <- sample(unique(d$writer), replace = FALSE)

# Create writer groups
for (i in seq(1, k, by=1)) {
  
  a <- writers_per_group*(i-1) + 1
  b <- writers_per_group*(i)
  group <- shuffled_writers[a : b]
  
  # Name for each group
  group_name <- paste("group", i, sep = "_")
  ## Or using sprintf: df_name <- sprintf("df_%d", i)
  assign(group_name, group)
}

#------------------------------------------------------------------------------




# Create training and testing data sets
options(expressions= 500000)
for (i in seq(1, k, by=1)) {
  group_name <- paste("group", i, sep = "_")
  group <- get(group_name)
  
  create_datasets(group, d)
  
  assign(paste("test",  i, sep = "_"), test_writers2)
  assign(paste("train", i, sep = "_"), train_writers_all)
}
#options(expressions= 5000)

#------------------------------------------------------------------------------



all_accuracies <- data.frame()
for (i in seq(1, k, by=1)) {
  test_name <- paste("test", i, sep = "_")
  test_data <- get(test_name)
  names(test_data)[names(test_data) == "writer"] <- "y" # You need this for cvl, not for csafe data, I'm not sure why
  
  train_name <- paste("train", i, sep = "_")
  train_data <- get(train_name)
  names(train_data)[names(train_data) == "writer"] <- "y" # You need this for cvl, not for csafe data, I'm not sure why
  
  group_accuracies <- c()

  for (j in seq(1, length(formulae), by=1)) {
    f <- as.formula(formulae[j])
    
    rf <- randomForest(f, data=train_data, ntree=200)
    prediction_writers2rf <- predict(rf, newdata = subset(test_data,
                                                            select=-c(y)))
    
    c <- confusionMatrix(as.factor(test_data[,"y"]), prediction_writers2rf)
    group_accuracies <- c(group_accuracies, c$overall[1])
  }
  
  all_accuracies <- rbind(all_accuracies, group_accuracies)
}


colnames(all_accuracies) = c("formula1",
                             "formula2",
                             "formula3",
                             "formula4",
                             "formula5",
                             "formula6")

colMeans(all_accuracies)

