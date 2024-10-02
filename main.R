devtools::load_all()

library(pROC)

project_dir <- file.path("experiments", "testing2")

# experiments_only ----
train <- get_csafe_train_set(df = cfr, train_prompt_code = c("pWOZ")) #check which session and maybe mess with that
rf_ranger <- train_rf(df = train,
                      ntrees = 200,
                      distance_measures = c("abs", "euc", "man", "max", "cos"),
                      output_dir = "experiments/testing2",
                      run_number = 1,
                      downsample = TRUE)

# test samples ----
# All test pairs are between two different prompts. E.g. "pLND" versus "pWOZ."
# The CSAFE database png images needs to be saved somewhere on your computer.
# Inside the CSAFE database folder, each writer needs their own folder.
samples <- get_test_samples(train,
                            db_path = "C:/Users/anyes/Documents/CSAFEComputer/DataCollection/All_Images_Cropped",
                            sessions = c("s02"),  # can choose multiple sessions
                            prompts = c("pLND", "pWOZ"),  # must be two prompts
                            seed = 100)

# get slrs ----
# Choose a project directory where the experiment results will be saved
get_slrs(samples = samples, rforest = rf_ranger, experiment_dir = project_dir)


# analysis ----
df <- load_slrs(experiment_dir = project_dir)

calculate_errors(df)


#look at cvl data for testing


## Method 2


sample1_path <- "path/to/png1"
sample2_path <- "path/to/png2"
calculate_slr(sample1_path = sample1_path,
              sample2_path = sample2_path,
              rforest = rf_ranger,
              project_dir = "path/to/output_folder") # each one needs its own folder



