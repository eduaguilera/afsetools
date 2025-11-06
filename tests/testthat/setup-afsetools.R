# Setup file for testthat
# This runs once before all tests

# Load all data objects into the global environment
# so they're accessible to package functions during testing
load_general_data()

# Assign all objects from current environment to global environment
# This ensures package functions can find them
test_env <- environment()
for (obj_name in ls(test_env, all.names = TRUE)) {
  if (obj_name != "test_env" && !grepl("^\\.", obj_name)) {
    assign(obj_name, get(obj_name, envir = test_env), envir = .GlobalEnv)
  }
}
