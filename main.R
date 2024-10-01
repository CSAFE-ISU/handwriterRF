devtools::load_all()


project_dir <- file.path("experiments", "testing2")

# experiments_only ----
train <- get_csafe_train_set(df = cfr, train_prompt_code = c("pLND", "pWOZ"))
rf_ranger <- train_rf(df = train,
                      ntrees = 200,
                      distance_measures = c("abs", "euc", "man", "max", "cos"),
                      output_dir = "experiments/testing2",
                      run_number = 1,
                      downsample = TRUE)

# test samples ----
# All test pairs are between two different prompts. E.g. "pLND" versus "pWOZ."
samples <- get_test_samples(train,
                            db_path = "path/to/csafe_docs",
                            sessions = c("s02"),  # can choose multiple sessions
                            prompts = c("pLND", "pWOZ"),  # must be two prompts
                            seed = 100)

# get slrs ----
# Choose a project directory where the experiment results will be saved
get_slrs(samples = samples, rforest = rf_ranger, experiment_dir = project_dir)


# analysis ----
df <- load_slrs(experiment_dir = project_dir)
