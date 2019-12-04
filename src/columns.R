# load columns, analyze them
source("src/util.R")

RANDOM_SEED = 1

MIN_FACTORS = 3     # min number of uniques
FACTOR_P = 0.1      # min ratio of uniques to nrow to be considered numeric
SIGNIFICANCE = 0.05

# keep these the same length!
F_PREFIX =  "FACTORx"
FN_PREFIX = "F_AS_Nx"
N_PREFIX =  "NUMBERx"
NF_PREFIX = "N_AS_Fx"
PREFIX_LENGTH = nchar(F_PREFIX)

# TODO: if it's a string, need to sort them somehow and apply factors
# or use PCA on other columns to find vector of greatest variation, sort
# factor that way... more complicated
rank_column = function(column) {
    # for now just return as factor with default ordering
    return(as.factor(column))
}

# discard_factors = function(column, min_rows_per_factor) {
#     col_as_factor = as.factor(column)
#     factor_count = table(col_as_factor)
#     drop_factors = as.integer(names(factor_count < min_rows_per_factor))
#     drop_indexes = is.na(droplevels(col_as_factor, drop_factors))
#     return(column[drop_indexes])
# }

num_unique = function(column) {
    return(length(levels(as.factor(column))))
}

classify_columns = function(dataframe) {
    df = dataframe
    columns = colnames(dataframe)
    for (i in 1:length(columns)) {
        name = columns[i]
        col = dataframe[,i]
        factorizable = is_factorizable(dataframe, name)
        num_factors = num_unique(col)

        # NUMBERx a numeric or factor with lots of levels
        # must be numeric to begin with
        # minimum FACTOR_P of nrow uniques
        if (is.numeric(col)) {
            if (num_factors >= MIN_FACTORS && num_factors >= FACTOR_P * length(col)) {
                class = N_PREFIX
                new_class = NF_PREFIX
                if (factorizable) {
                    new_col = as.factor(col)
                }
                else {
                    # create N_AS_Fx the numeric made into factor for comparison, do not use as response
                    # TODO: if column has less than MIN_ROWS_PER_FACTOR categories
                    # bin those multiple values to one factor
                    # for now just don't create
                    new_col = NULL
                }
            }
            else {
                # FACTORx a string or a factor with less than % levels, do not use as response
                # since this was originally numeric, create a new col which is factor
                class = FN_PREFIX
                new_class = F_PREFIX
                new_col = NULL
                if (factorizable) {
                    new_col = as.factor(col)
                }
            }
        }
        else {
            class = F_PREFIX
            col = as.factor(col)
            # create F_AS_Nx the factor made numeric for comparison, do not use as response
            # since it could be logistic/multilogit
            # TODO: if it's a string, need to sort them, see rank_column()
            new_class = FN_PREFIX
            new_col = NULL
            if (factorizable) {
                new_col = rank_column(col)
            }
        }
        # rename column
        colnames(df)[colnames(df)==name] = paste(class, name, sep="")
        # add new_col to df if not NULL
        if (!is.null(new_col)) {
            df[,paste(new_class, name, sep="")] = new_col
        }
    }
    return(df)
}

get_col_type = function(dataframe, prefix) {
    indexes = find_prefixed_var_index(dataframe, prefix)
    return(colnames(dataframe)[indexes])
}

strip_prefix_col_name = function(colname) {
    return(substring(colname, PREFIX_LENGTH + 1))
}

# find column with same name but one of the 2 factor prefixes
factor_equivalent = function(dataframe, colname) {
    if (is.null(colname) || is.na(colname)) {
        return(NULL)
    }
    prefix = substring(colname, 1, PREFIX_LENGTH)
    name = strip_prefix_col_name(colname)
    new_prefix = NULL
    if (prefix == FN_PREFIX) {
        new_prefix = F_PREFIX
    }
    else if (prefix == N_PREFIX) {
        new_prefix = NF_PREFIX
    }
    else {
        return(NULL)
    }
    columns = colnames(dataframe)
    index = match(paste(new_prefix, name, sep=""), columns)
    if (is.na(index)) {
        return(NULL)
    }
    return(columns[index])
}

iterate_columns = function(filenames) {
    stat_rows = NULL
    # for each dataset
    for (i in 1:length(filenames)) {
        filename = filenames[i]
        dataframe = load_dataframe(filename)
        dataframe = classify_columns(dataframe)

        # find columns with N_PREFIX
        numeric_columns = get_col_type(dataframe, N_PREFIX)

        # all numeric type columns can be response
        for (j in 1:length(numeric_columns)) {
            response = numeric_columns[j]
            f_as_n_cols = get_col_type(dataframe, FN_PREFIX)
            predictors = c(numeric_columns[-j], f_as_n_cols)

            # all numeric and factor as numeric columns can be predictor
            # include the null predictor
            for (k in 1:(length(predictors)+1)) {
                predictor = predictors[k] # NA when k > length(predictors)
                candidates = predictors[-k]

                if (length(candidates) > 0) {
                    for (l in 1:length(candidates)) {
                        candidate_as_numeric = candidates[l]
                        # the candidate must have both numeric and factor forms
                        candidate_as_factor = factor_equivalent(dataframe, candidate_as_numeric)
                        if (!is.null(candidate_as_factor)) {
                            stat_row = evaluate_columns(dataframe, response, predictor, candidate_as_numeric, candidate_as_factor)
                            stat_rows = append_list_of_lists_rows(stat_rows, stat_row)
                        }
                    }
                }
            }
        }
    }
    stat_df = make_data_frame(stat_rows)
    return(stat_df)
}

# tests this combination of columns and outputs stats for model fitting
# pass in names of columns
evaluate_columns = function(dataframe, response, predictor, candidate_as_numeric, candidate_as_factor) {
    candidate = candidate_as_factor
    if (is.null(predictor) || is.na(predictor)) {
        formula_null = paste(response, "~", candidate_as_numeric, sep="")
        formula_fit = paste(response, "~", candidate, sep="")
        predictor_is_null = TRUE
    }
    else {
        formula_null = paste(response, "~", predictor, "+", candidate, sep="")
        formula_fit = paste(response, "~", predictor, "+", candidate, "+", predictor, ":", candidate, sep="")
        predictor_is_null = FALSE
    }
    test_ids = split_test_set(dataframe[,candidate])
    train = dataframe[-test_ids,]
    test = dataframe[test_ids,]
    lm_null = lm(formula_null, data=train)
    lm_fit = lm(formula_fit, data=train)

    n = nrow(test)
    test_error_null = predict(lm_null, test) - test[,response]
    test_error_fit = predict(lm_fit, test) - test[,response]
    num_null = sum(abs(test_error_null) >= abs(test_error_fit))
    num_fit = n - num_null
    # response under testing
    test_mse_null = sum( test_error_null^2 ) / n
    test_mse_fit = sum( test_error_fit^2 ) / n
    test_pbest_null = num_null / n
    test_pbest_fit = 1 - test_pbest_null

    # to decide if this is a factor, apply equality test to mse and prop
    pval_test_mse_diff = test_error_diff(test_error_null, test_error_fit)
    pval_test_prop_diff = test_prop_diff(num_null, num_fit)
    is_factor_by_mse = pval_test_mse_diff < SIGNIFICANCE
    is_factor_by_prop = pval_test_prop_diff < SIGNIFICANCE


    summary_null = summary(lm_null)
    summary_fit = summary(lm_fit)
    # Rsq and df
    adj_rsq_null = summary_null$adj.r.squared
    if (is.na(adj_rsq_null)) { adj_rsq_null = 1 }
    adj_rsq_fit = summary_fit$adj.r.squared
    if (is.na(adj_rsq_fit)) { adj_rsq_fit = 1 }
    n_slopes_null = summary_null$df[3]
    n_slopes_fit = summary_fit$df[3]
    adj_n_slopes_null = summary_null$df[1]
    adj_n_slopes_fit = summary_fit$df[1]
    df_null = summary_null$df[2]
    df_fit = summary_fit$df[2]
    df_ratio_null = df_null / adj_n_slopes_null
    df_ratio_fit = df_fit / adj_n_slopes_fit
    alias_null = n_slopes_null - adj_n_slopes_null
    alias_fit = n_slopes_fit - adj_n_slopes_fit

    param_null = summary_null$coefficients
    param_fit = summary_fit$coefficients
    pvalue_null = param_null[,4]
    pvalue_null[is.na(pvalue_null)] = 0
    pvalue_fit = param_fit[,4]
    pvalue_fit[is.na(pvalue_fit)] = 0
    # distribution of parameters
    na_null = sum(is.na(param_null[,1]))
    na_fit = sum(is.na(param_fit[,1]))
    mean_pval_null = mean(pvalue_null)
    mean_pval_fit = mean(pvalue_fit)
    median_pval_null = median(pvalue_null)
    median_pval_fit = median(pvalue_fit)
    max_pval_null = max(pvalue_null)
    max_pval_fit = max(pvalue_fit)
    sd_pval_null = max(pvalue_null)
    sd_pval_fit = max(pvalue_fit)

    # AIC
    aic_null = extractAIC(lm_null, k=2)[2]
    aic_fit = extractAIC(lm_fit, k=2)[2]
    aic_null_to_fit_diff = NA
    if (!is.infinite(aic_null) && !is.infinite(aic_fit)) {
        aic_null_to_fit_diff = aic_null - aic_fit
    }

    anova_results = anova(lm_null, lm_fit)
    # anova between models
    anova_p = anova_results[2,6]
    if (is.na(anova_p)) { anova_p = 0 }

    added_params = subset_difference(param_null, param_fit)[,1]
    added_params = sort(added_params[!is.null(added_params)])
    ranks = c(1:length(added_params))
    # linearity of added parameters using lm on parameters
    pval_param = 1
    adj_rsq_param = 0
    if (length(added_params) > 1) {
        lm_param = lm(added_params~ranks)
        summary_param = summary(lm_param)
        pval_param = summary_null$coefficients[2,4]
        adj_rsq_param = summary_param$adj.r.squared
    }
    if (is.na(adj_rsq_param)) { adj_rsq_param = 1 }

    list_stats = list(
        "column"=strip_prefix_col_name(candidate),
        "is_factor_by_mse"=is_factor_by_mse,
        "is_factor_by_prop"=is_factor_by_prop,
        "pval_test_mse_diff"=pval_test_mse_diff,
        "pval_test_prop_diff"=pval_test_prop_diff,
        "formula_null"=formula_null,
        "formula_fit"=formula_fit,
        "test_mse_null"=test_mse_null,
        "test_mse_fit"=test_mse_fit,
        "test_pbest_null"=test_pbest_null,
        "test_pbest_fit"=test_pbest_fit,
        "adj_rsq_null"=adj_rsq_null,
        "adj_rsq_fit"=adj_rsq_fit,
        "n_slopes_null"=n_slopes_null,
        "n_slopes_fit"=n_slopes_fit,
        "adj_n_slopes_null"=adj_n_slopes_null,
        "adj_n_slopes_fit"=adj_n_slopes_fit,
        "df_null"=df_null,
        "df_fit"=df_fit,
        "df_ratio_null"=df_ratio_null,
        "df_ratio_fit"=df_ratio_fit,
        "alias_null"=alias_null,
        "alias_fit"=alias_fit,
        "na_null"=na_null,
        "na_fit"=na_fit,
        "mean_pval_null"=mean_pval_null,
        "mean_pval_fit"=mean_pval_fit,
        "median_pval_null"=median_pval_null,
        "median_pval_fit"=median_pval_fit,
        "max_pval_null"=max_pval_null,
        "max_pval_fit"=max_pval_fit,
        "sd_pval_null"=sd_pval_null,
        "sd_pval_fit"=sd_pval_fit,
        "aic_null_to_fit_diff"=aic_null_to_fit_diff,
        "anova_p"=anova_p,
        "pval_param"=pval_param,
        "adj_rsq_param"=adj_rsq_param,
        "predictor_is_null"=predictor_is_null
        )

    return(list_stats)
}

test_error_diff = function(test_error_null, test_error_fit) {
    wtest = wilcox.test(test_error_null, test_error_fit, paired=TRUE, correct=TRUE, alternative="less")
    return(wtest$p.value)
}

test_prop_diff = function(num_null, num_fit) {
    total = num_null + num_fit
    ptest = prop.test(x=c(num_null, num_fit), n=c(total, total), correct=TRUE, alternative="less")
    return(ptest$p.value)
}

# returns named rows which are only present in superset
subset_difference = function(subset, superset) {
    sub = data.frame(subset)
    sup = data.frame(superset)
    only_in_superset = sup[!(rownames(sup) %in% rownames(sub)),]
    return(only_in_superset)
}

# reserve test set by a simple split of each category (not k-fold)
# return indexes for a split which contain every factor in the candidate column num_reserved times
split_test_set = function(column, prop=0.5, num_reserved=DEFAULT_REQUIRES$MIN_ROWS_PER_FACTOR) {
    set.seed(RANDOM_SEED) # make output consistent
    indexes = c(1:length(column))
    shuffled_indexes = sample(indexes)
    remaining = column
    shuffled = remaining[shuffled_indexes] # shuffle so reserved is random
    # reserve every factor num_reserved times
    if (num_reserved > 1) {
        for (i in 1:num_reserved) {
            # find a set of every factor in the remaining items
            reserved_indexes = shuffled_indexes[!duplicated(shuffled)]
            # remove them from the remaining
            remaining[reserved_indexes] = NA
            # updated the shuffled list
            shuffled = remaining[shuffled_indexes]
        }
    }
    non_na = !is.na(remaining)
    split_size = min(length(column) * prop, sum(non_na))
    # sample from remaining non-NA
    ids = sample(indexes[non_na], size=split_size)

    return(ids)
}