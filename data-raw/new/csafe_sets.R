devtools::load_all()

all_profiles <- readRDS("data-raw/new/csafe_writer_profiles.rds")

rw <- unique(all_profiles$writer)
print(paste("Number of writers remaining:", length(rw)))

# Get session, prompt, and rep
all_profiles <- all_profiles %>% dplyr::select(-tidyselect::any_of(c("writer", "doc")))
all_profiles <- expand_docnames(all_profiles)

# Select two LND, two WOZ, and two PHR from each writer
rp <- all_profiles %>%
  dplyr::group_by(writer, prompt) %>%
  dplyr::slice_sample(n=2) %>%
  dplyr::ungroup()
print(paste("Number of writers remaining:", length(unique(rp$writer))))

# Assign writers to sets
set.seed(111817)
rw <- unique(rp$writer)
train_writers <- sample(rw, size=100)
rw <- rw[!(rw %in% train_writers)]
valid_writers <- sample(rw, size=150)
rw <- rw[!(rw %in% valid_writers)]
test_writers <- rw
rw <- rw[!(rw %in% test_writers)]

csafe_sets <- list()
csafe_sets$train <- rp %>%
  dplyr::filter(writer %in% train_writers) %>%
  dplyr::filter(prompt != "pPHR")
csafe_sets$valid <- rp %>%
  dplyr::filter(writer %in% valid_writers) %>%
  dplyr::filter(prompt != "pPHR")
csafe_sets$test_lnd <- rp %>%
  dplyr::filter(writer %in% test_writers, prompt == "pLND")
csafe_sets$test_phr <- rp %>%
  dplyr::filter(writer %in% test_writers, prompt == "pPHR")
csafe_sets$test_woz <- rp %>%
  dplyr::filter(writer %in% test_writers, prompt == "pWOZ")

saveRDS(csafe_sets, "data-raw/new/csafe_sets.rds")
