source("src/columns.R")
source("src/logistic.R")

INPUTS = "data"

filenames = list_data_filenames(INPUTS, ".csv")
for (i in 1:length(filenames)) {
    filename = file.path(INPUTS, filenames[i])
    columns = load_columns(filename)
    print(columns$info)
    print(head(columns$data))
}