# copied code from linception
# use this instead of original files in case changes needed
# and to avoid namespace conflicts

find_prefixed_var_index = function(dataframe, prefix) {
    columns = colnames(dataframe)
    return(grep(paste(prefix, ".*", sep=""), columns))
}

# removes columns named as empty string ""
remove_null = function(dataframe, null_prefix) {
    df = dataframe
    indexes = find_prefixed_var_index(df, null_prefix)
    df[indexes] = NULL
    return(df)
}

list_data_filenames = function(path, target_extension) {
    filenames = list.files(path=path)

    valid_indexes = vector()
    for (i in 1:length(filenames)) {
        extension = strsplit(filenames[i], "\\.")[[1]]
        extension = extension[[length(extension)]]
        if (extension == target_extension) {
            valid_indexes = append(valid_indexes, i)
        }
    }
    
    if (length(valid_indexes) > 0) {
        return(filenames[valid_indexes])
    }
    else {
        return(NULL)
    }
}