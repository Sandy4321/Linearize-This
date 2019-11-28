# load and clean data

load_columns = function(filename) {
    dataframe = read.table(filename, header=TRUE, sep=",")

    # discard any columns which have less than 3 categories

    # discard any columns where there are less than 3 rows per category
    # 2 for fitting, one for testing
    # save these columns as response variables for testing
}


# reserve test set by a simple split of each category (not k-fold)
make_factors = function(df, cols){
    new_df = df
    for (col in cols) {
        new_df[,col] = as.factor(df[,col])
    }
    return(new_df)
}

# remove columns which are factors
remove_factors = function(df, names) {
    remove_indexes = vector(mode="logical", length=length(names))
    for (i in 1:length(names)) {
        remove_indexes[[i]] = !is.factor(df[, names[[i]]])
    }
    new_names = names[remove_indexes]
    return(new_names)
}


# utility function to plot pairwise comparisons by factor column "col"
plot_by_factor = function(df, col, responses=NULL, predictors=NULL) {
    if (is.null(responses)) {
        responses = remove_factors(df, colnames(df))
    }
    num_resp = length(responses)
    if (is.null(predictors)) {
        predictors = remove_factors(df, colnames(df))
    }
    num_pred = length(predictors)
    factor_levels = levels(df[,col])
    num_levels = length(factor_levels)
    coefficients = vector(mode="numeric", length=num_levels)
    par(mfrow=c(num_resp, num_pred))

    for (r in 1:num_resp) {
        for (c in 1:num_pred) {
            response = responses[[r]]
            predictor = predictors[[c]]
            # invisible plot to get proper range
            plot(df[,response], df[,predictor], type="n", xlab=predictor, ylab=response)
            for (i in 1:num_levels) {
                subset = df[df[,col]==factor_levels[[i]],]
                points(subset[,response], subset[,predictor], pch=i, col=i)
                if (nrow(subset) > 1) {
                    fitline = lsfit(subset[,response], subset[,predictor])
                    abline(fitline, col=i)
                    coefficients[i] = fitline$coefficients[2]
                }
                else {
                    coefficients[i] = NA
                }
            }
        }
    }
    return(unlist(coefficients))
}