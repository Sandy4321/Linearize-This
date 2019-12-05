library("car")
source("src/util.R")

combine_response = function(df, logical_or=TRUE, remove_columns=TRUE) {
    if (logical_or) {
        df$is_factor = (df$NULL_PREDICTORxis_factor_by_mse + df$NULL_PREDICTORxis_factor_by_prop) != 0
    }
    else {
        df$is_factor = (df$NULL_PREDICTORxis_factor_by_mse + df$NULL_PREDICTORxis_factor_by_prop) == 2
    }
    # remove the non-predictor columns
    if (remove_columns) {
        df = remove_null(df, DEFAULT_REQUIRES$NULL_PREDICTOR_PREFIX)
    }
    # remove na values
    df = df[complete.cases(df),]
    print(summary(df))
    print(nrow(df))
    return(df)
}

DO_PRINT = FALSE
print_and_plot = function(model, do_print=DO_PRINT) {
    if (do_print) {
        summary(model)
        mmps(model)
    }
}

split_and_test = function(df, do_print=DO_PRINT) {
    df_no_predictor = df[df$predictor_is_null,]
    df_no_predictor$predictor_is_null = NULL
    glm_no_predictor = glm("is_factor~.", data=df_no_predictor)

    df_with_predictor = df[!df$predictor_is_null,]
    df_with_predictor$predictor_is_null = NULL
    glm_with_predictor = glm("is_factor~.", data=df_with_predictor)

    print_and_plot(glm_no_predictor, do_print)
    print_and_plot(glm_with_predictor, do_print)
    return(list("FALSE"=glm_no_predictor, "TRUE"=glm_with_predictor))
}

boxplot_by_factor = function(df) {
    par(mfrow=c(4, ncol(df)/4))
    for (name in colnames(df)) {
        boxplot(df[,name]~df$predictor_is_null, xlab=NULL, ylab=NULL, main=name)
    }
}

features_as_diff = function(df, name, null_to_fit=TRUE, subtract=TRUE, remove_columns=TRUE) {
    name_null = paste(name, "_null", sep="")
    name_fit = paste(name, "_fit", sep="")
    if (null_to_fit) {
        first = df[,name_null]
        second = df[,name_fit]
        order = "_null_to_fit"
    }
    else {
        first = df[,name_fit]
        second = df[,name_null]
        order = "_fit_to_null"
    }
    if (subtract) {
        df[,paste(name, order, "_diff", sep="")] = first - second
    }
    else {
        df[,paste(name, order, "_ratio", sep="")] = first / second
    }
    if (remove_columns) {
        df[,name_null] = NULL
        df[,name_fit] = NULL
    }
    return(df)
}

feature_engineering = function(df) {
    df = features_as_diff(df, "adj_rsq")
    df = features_as_diff(df, "n_slopes", subtract=FALSE)
    df = features_as_diff(df, "adj_n_slopes", subtract=FALSE)
    df = features_as_diff(df, "df", null_to_fit=FALSE, subtract=FALSE)
    df = features_as_diff(df, "df_ratio", null_to_fit=FALSE, subtract=FALSE, remove_columns=FALSE)
    df = features_as_diff(df, "mean_pval", remove_columns=FALSE)
    df = features_as_diff(df, "median_pval", remove_columns=FALSE)
    df = features_as_diff(df, "max_pval")
    df = features_as_diff(df, "sd_pval")
    # remove since they are always 0
    df$na_null = NULL
    df$na_fit = NULL
    df$t_na_null = NULL
    df$t_na_fit = NULL  # there is only one nonzero
    # these are almost always zero, so turn into factors indicating nonzero
    df$alias_null = as.integer(as.factor(df$alias_null != 0))
    df$alias_fit = as.integer(as.factor(df$alias_fit != 0))
    return(df)
}

remove_constant_cols = function(df) {
    for (name in colnames(df)) {
        # turn logical to numerical
        if (length(unique(df[,name])) < 2) {
            df[,name] = NULL
        }
    }
    return(df)
}

intersect_coefficients = function(glm1, glm2) {
    names1 = names(glm1$coefficients)
    names2 = names(glm2$coefficients)
    intersection = intersect(names1, names2)
    print(paste(length(intersection), "in commmon out of ", length(names1), "in first,", length(names2), "in second."))
    return(intersection)
}