devtools::load_all()

all_profiles <- readRDS("data-raw/new/cvl_writer_profiles.rds")

# Drop writer with only 2 prompts
missing_prompts <- all_profiles %>%
  dplyr::group_by(writer) %>%
  dplyr::summarize(n = dplyr::n()) %>%
  dplyr::filter(n == 3) %>%
  dplyr::pull(writer)
rp <- all_profiles %>% dplyr::filter(writer != missing_prompts)
print(paste("Number of writers remaining:", length(unique(rp$writer))))

# Assign the 27 writers who wrote prompts 7 and 8 to the test set. Use German
# prompt in test set as well to boost numbers and diversity.
test678_writers <- rp %>%
  dplyr::filter(doc == 7 | doc == 8) %>% dplyr::pull(writer) %>% unique()
test678_prompts <- rp %>%
  dplyr::filter(writer %in% test678_writers) %>%
  dplyr::filter(doc >= 6)
rp <- rp %>% dplyr::filter(!(writer %in% test678_writers))
print(paste("Number of writers in test678_prompts:", length(unique(test678_prompts))))
print(paste("Number of writers remaining:", length(unique(rp$writer))))
test6_prompts <- test678_prompts %>%
  dplyr::filter(doc == 6)
test78_prompts <- test678_prompts %>%
  dplyr::filter(doc != 6)

# Drop German prompts
rp <- rp %>%
  dplyr::filter(doc != 6)
rw <- unique(rp$writer)
print(paste("Number of writers remaining:", length(unique(rp$writer))))

# Assign remaining writers to train, valid, and test
set.seed(111817)

# Train set
train_writers <- sample(rw, size = 100)
rw <- rw[!(rw %in% train_writers)]
train <- rp %>%
  dplyr::filter(writer %in% train_writers)
rp <- rp %>% dplyr::filter(writer %in% rw)
print(paste("Number of writers in train set:", length(unique(train$writer))))
print(paste("Number of writers remaining:", length(unique(rp$writer))))

# Validation set
valid_writers <- sample(rw, size = 150)
rw <- rw[!(rw %in% valid_writers)]
valid <- rp %>%
  dplyr::filter(writer %in% valid_writers)
rp <- rp %>% dplyr::filter(writer %in% rw)
print(paste("Number of writers in valid set:", length(unique(valid$writer))))
print(paste("Number of writers remaining:", length(unique(rp$writer))))

# Second test set consists of same prompts as the training and validation sets
test1234_writers <- rw
rw <- rw[!(rw %in% test1234_writers)]
test1234_prompts <- rp %>%
  dplyr::filter(writer %in% test1234_writers)
rp <- rp %>% dplyr::filter(writer %in% rw)
print(paste("Number of writers in test same prompts set:", length(unique(test1234_prompts$writer))))
print(paste("Number of writers remaining:", length(unique(rp$writer))))

cvl_sets <- list()
cvl_sets$train <- train
cvl_sets$valid <- valid
cvl_sets$test <- list("prompts1234" = test1234_prompts, "prompt6" = test6_prompts, "prompts78" = test78_prompts)

# Checks
train_writers <- unique(cvl_sets$train$writer)
valid_writers <- unique(cvl_sets$valid$writer)
test1234_writers <- unique(cvl_sets$test$prompts1234$writer)
test6_writers <- unique(cvl_sets$test$prompt6$writer)
test78_writers <- unique(cvl_sets$test$prompts78$writer)

intersect(train_writers, valid_writers)
intersect(train_writers, test1234_writers)
intersect(train_writers, test6_writers)
intersect(train_writers, test78_writers)
intersect(valid_writers, test1234_writers)
intersect(valid_writers, test6_writers)
intersect(valid_writers, test78_writers)
intersect(test1234_writers, test6_writers)
intersect(test1234_writers, test78_writers)
# writers should be same in both sets
intersect(test6_writers, test78_writers)

unique(cvl_sets$train$doc)
unique(cvl_sets$valid$doc)
unique(cvl_sets$test$prompts1234$doc)
unique(cvl_sets$test$prompt6$doc)
unique(cvl_sets$test$prompts78$doc)

# Save
saveRDS(cvl_sets, "data-raw/new/cvl_sets.rds")
