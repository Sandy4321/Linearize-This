# these functions create fake factors of different types
# to aid preliminary analysis of how to detect the different types
# there are 3 types: no correlation, linear correlation, non-linear correlation

# bin columns to make fake factor which is linear in behaviour
# noise is added to make edges of bins fuzzy
fake_linear = function(dataframe, num_factors, cols=NULL, noise=0, prefix="LINEAR_FACTORx"){
    df = dataframe

    if (is.null(cols)) {
        cols = colnames(dataframe)
    }

    n = rep(num_factors, ceiling(length(cols) / length(num_factors)))

    for (i in 1:length(cols)) {
        num = n[[i]]
        name = cols[[i]]
        noisy_col = df[, name] + rnorm(nrow(dataframe), mean=0, sd=noise)
        factors = cut(noisy_col, num)
        df[, paste(prefix, name, sep="")] = as.integer(factors)
    }

    return(df)
}

# shuffle columns (first column to end), add and multiply by a bias, and append to dataframe
# to create fake interaction term with a new categorical variable shuffle_index
fake_interaction = function(dataframe, num_shuffle=NULL, bias_slope=1, bias_sum=0, do_shuffle=TRUE){
    df = dataframe
    colname = "SHUFFLE_INDEX"
    df[,colname] = rep(1, nrow(df))

    if (is.null(num_shuffle)) {
        num_shuffle = ncol(dataframe)
    }

    for (i in 2:num_shuffle) {
        new_df = dataframe * bias_slope^i + bias_sum*i
        if (do_shuffle) {
            new_df = new_df[, c(i:ncol(dataframe), 1:(i-1))]
            colnames(new_df) = colnames(dataframe)
        }
        new_df[,colname] = rep(i, nrow(dataframe))
        df = rbind(df, new_df)
    }

    return(df)
}

# make a fake column with fake factors (randomly distributed)
fake_factor = function(dataframe, num_factors, prefix="FAKE_FACTORx"){
    new_df = dataframe
    set.seed(RANDOM_SEED)
    for (i in num_factors) {
        new_df[,paste(prefix, toString(i), sep="")] = sample(1:i, nrow(dataframe), replace=TRUE)
    }
    return(new_df)
}

# set cols as factors
as_factors = function(df, cols){
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
plot_by_factor = function(df, factor_col, responses=NULL, predictors=NULL) {
    if (is.null(responses)) {
        responses = remove_factors(df, colnames(df))
    }
    num_resp = length(responses)
    if (is.null(predictors)) {
        predictors = remove_factors(df, colnames(df))
    }
    num_pred = length(predictors)
    col = as.factor(df[,factor_col])
    factor_levels = levels(col)
    num_levels = length(factor_levels)
    coefficients = vector(mode="numeric", length=num_levels)
    # par(mfrow=c(num_resp, num_pred))
    par(mfrow=c(1, num_pred))

    # for (r in 1:num_resp) {
    for (r in 1:1) {
        for (c in 1:num_pred) {
            response = responses[[r]]
            predictor = predictors[[c]]
            # invisible plot to get proper range
            plot(df[,predictor], df[,response], type="n", xlab=predictor, ylab=response)
            for (i in 1:num_levels) {
                subset = df[col==factor_levels[[i]],]
                points(subset[,predictor], subset[,response], pch=i, col=i)
                if (nrow(subset) > 1) {
                    fitline = lsfit(subset[,predictor], subset[,response])
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
