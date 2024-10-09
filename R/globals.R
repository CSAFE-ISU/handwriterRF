# The following variables are loaded from the data folder so we
# can't fix the note 'no visible binding for global variable' by adding
# variable <- NULL at the beginning of the function that uses the variable.
# Instead, we declare the variables here to fix the note.
utils::globalVariables(c("templateK40"))
utils::globalVariables(c("random_forest"))
