#------------------------------------------------------------------------------
#title: "Creating Training and Test Datasets"
#author: "Maureen Gallagher"
#------------------------------------------------------------------------------


library("tidyverse")
library("randomForest")
library("gtools")
library("lsa")


#------------------------------------------------------------------------------


# Read in the data
d1 <- readRDS("d1.Rdata")


# Filter the data to include only london letter documents
# - The dataset `d` contains 90 total writers and just the London Letter prompt
# - There are three repetitions per writer, with a total of 261 documents.
# - Not all writers wrote the London Letter 3 times.
#d <- d1 %>% filter(prompt == "LND")
#train_name <- "train_writers_LND.csv"
#test_name  <- "test_writers_LND.csv"

# Filter the data to include only phrase documents
# - The dataset `d` contains 90 total writers and just the phrase prompt
# - There are three repetitions per writer, with a total of 270 documents.
#d <- d1 %>% filter(prompt == "PHR")
#train_name <- "train_writers_PHR.csv"
#test_name  <- "test_writers_PHR.csv"

# Filter the data to include only wizard of oz documents
# - The dataset `d` contains 90 total writers and just the wizard of oz prompt
# - There are three repetitions per writer, with a total of 265 documents.
# - Not all writers wrote the Wizard of Oz 3 times.
#d <- d1 %>% filter(prompt == "WOZ")
#train_name <- "train_writers_WOZ.csv"
#test_name  <- "test_writers_WOZ.csv"

# Do not filter the data
# - The dataset 'd' contains 90 total writers and all three prompts
# - There are three repetitions per document for each writer,
#      with a total of 796 documents
# - Not all writers wrote the London letter or the Wizard of Oz 3 times
#d <- d1
load("cvl_cluster_fill_counts.RData")
d <- cluster_fill_counts
train_name <- "train_writers_all.csv"
test_name  <- "test_writers_all.csv"

# Filter the data to include only LND and OZ prompts
# - The dataset 'd' contains 90 total writers and both the London Letter and
#     the Wizard of Oz prompts
# - There are three repetitions per document for each writer,
#      with a total of 526 documents
# - Not all writers wrote the London letter or the Wizard of Oz 3 times
#d <- d1 %>% filter(prompt != "PHR")
#train_name <- "train_writers_LND_OZ.csv"
#test_name  <- "test_writers_LND_OZ.csv"



#==============================================================================


# Start with 80% of all writers but then downcounting the number of different
#   source comparisons to equal that of the same source by randomly selecting
#   out of the sample

# Method 1
# The first step is to randomly select 80% of the writers in the data to
#   use as the training dataset for the random forest. All pairwise distance
#   measures will be computed which includes the absolute difference for each
#   cluster and the euclidean distance.



#------------------------------------------------------------------------------

# From Mattie's Code
# Function to create a dataset with distance measures and if measures were from
#   the same writer or different

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



#------------------------------------------------------------------------------


# Creating training data set from a sample of 80% of the writers

# Number of writers in dataset (90)
n_writers <- 90

# From Mattie's Code

# randomly select 80% of the total writers
samp_writers2 <- as.data.frame(d) %>% 
  dplyr::select(writer) %>%
  unique() %>%
  sample_n(ceiling(.8*n_writers))

# dataset with the documents from only the sampled writers
d_writers2 <- d %>% filter(writer %in% samp_writers2$writer)


# all document combinations of size of the sampled dataset
#pairs_writers2    <- combinations(nrow(d_writers2), 2)
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

#write.csv(train_writers_all, train_name, row.names = TRUE)



#------------------------------------------------------------------------------


# From Mattie's Code
# create a testing dataset
samp_test_writers2  <- anti_join(d, samp_writers2)
options(expressions= 100000)
pairs_test_writers2 <- combinations(dim(samp_test_writers2)[1], 2)
test_writers2       <- calc_dist(samp_test_writers2, pairs_test_writers2)



#write.csv(test_writers2, test_name, row.names = TRUE)

