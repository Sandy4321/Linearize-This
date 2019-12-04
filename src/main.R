# traceback errors for debugging
options(error=function()traceback(2))

source("src/util.R")
source("src/columns.R")
source("src/logistic.R")

TRAIN_SET = file.path("data", "trainset")
TEST_SET = file.path("data", "testset")
EXTENSION = "csv"

# # only run once to get data
# save_builtin_datasets_to_file(5, TRAIN_SET, EXTENSION)

filenames = list_data_filenames(TRAIN_SET, EXTENSION)
filenames = sapply(filenames, function(name) file.path(TRAIN_SET, name))
stats = iterate_columns(filenames)
save_dataframe(stats, timestamp("column_stats", EXTENSION), "data")