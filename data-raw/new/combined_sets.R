devtools::load_all()
library(dplyr)
library(tidyselect)

make_doc_column <- function(df) {
  if ("session" %in% colnames(df)) {
    df$doc <- paste(df$session, df$prompt, df$rep, sep = "_")
    df <- df %>% select(-any_of(c("session", "prompt", "rep")))
    df <- df %>% select(any_of(c("docname", "writer", "doc")), everything())

  }
  return(df)
}

csafe <- readRDS("data-raw/new/csafe_sets.rds")
cvl <- readRDS("data-raw/new/cvl_sets.rds")

csafe$train <- make_doc_column(csafe$train)
csafe$valid <- make_doc_column(csafe$valid)
csafe$test_lnd <- make_doc_column(csafe$test_lnd)
csafe$test_phr <- make_doc_column(csafe$test_phr)
csafe$test_woz <- make_doc_column(csafe$test_woz)

train <- rbind(csafe$train, cvl$train)
validation <- rbind(csafe$valid, cvl$valid)

test <- list()
test$csafe_lnd <- csafe$test_lnd
test$csafe_phr <- csafe$test_phr
test$csafe_woz <- csafe$test_woz
test$cvl_1_2_3_4 <- cvl$test$prompts1234
test$cvl_6 <- cvl$test$prompt6
test$cvl_7_8 <- cvl$test$prompts78

usethis::use_data(train, overwrite = TRUE)
usethis::use_data(validation, overwrite = TRUE)
usethis::use_data(test, overwrite = TRUE)
