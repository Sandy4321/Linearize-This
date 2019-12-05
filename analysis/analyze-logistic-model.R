# predict using logistic regression

library("car")
source("src/util.R")
source("src/logistic.R")

dataframe = read.table(file.path("data", "column_stats_2019-12-04-14-57-26.csv"), header=TRUE, sep=",")

# let's suppose if either test passes the column is a factor
df = combine_response(dataframe)


# fit a full model
glm_full = glm("is_factor~.", data=df)
print_and_plot(glm_full)
# analysis: there are a lot of null slopes
# very poor fit!

# try to split data between null and non-null predictor column
boxplot_by_factor(df)
split_glms = split_and_test(df)
# analysis: slightly better fit
# still lots of null slopes

# feature engineering:
# modified dataset
dataframe = read.table(file.path("data", "column_stats_2019-12-04-16-15-46.csv"), header=TRUE, sep=",")
df = combine_response(dataframe)
df = feature_engineering(df)
split_glms = split_and_test(df)
# this is slightly better!
# looks like mean/median pval aren't useful (will remove later in AIC/BIC step)

# see if changing the predictors will help
responses = combine_response(dataframe, logical_or=FALSE, remove_columns=FALSE)
df$is_factor = responses$is_factor
split_glms = split_and_test(df)
# not enough factors with both TRUE
df$is_factor = responses$NULL_PREDICTORxis_factor_by_prop
split_glms = split_and_test(df)
# this works ok when handling the NULL predictor case
df$is_factor = responses$NULL_PREDICTORxis_factor_by_mse
split_glms = split_and_test(df)
# not enough factors with mse TRUE
# analysis: changing predictors doesn't help much

# see if modifying the dataset by removing default values for NA will help
dataframe = read.table(file.path("data", "column_stats_2019-12-04-16-47-30.csv"), header=TRUE, sep=",")
df = combine_response(dataframe)
df = feature_engineering(df)
split_glms = split_and_test(df)
# analysis: slightly better fits, but less good at predicting due to less TRUE

# try power transform
list_of_predictors = df[,1]
col_names = colnames(df)[1]
for (i in 2:ncol(df)) {
    col = df[,i]
    if (!is.logical(col)) {
        if (min(col) <= 0) {
            col = col - min(col) + 0.0001
        }
        list_of_predictors = cbind(list_of_predictors, col)
        col_names = cbind(col_names, colnames(df)[i])
    }
}
colnames(list_of_predictors) = col_names
# summary(powerTransform(list_of_predictors))
# basically, doesn't work because there are too many columns

# try combining the presence of NA by adding in factors
dataframe = read.table(file.path("data", "column_stats_2019-12-04-17-28-03.csv"), header=TRUE, sep=",")
dataframe = read.table(file.path("data", "column_stats_2019-12-05-05-07-10.csv"), header=TRUE, sep=",")
df = combine_response(dataframe)
df = feature_engineering(df)
df = remove_constant_cols(df)
engineered_df = df
split_glms = split_and_test(df)
# seems to help somewhat

# try to recombine the split models using predictor_is_null as a factor
predictors = colnames(df)[colnames(df) != "is_factor"]
formula_str = paste(predictors, collapse=" + ")
formula_str_factor = paste(predictors[predictors != "predictor_is_null"], collapse=":predictor_is_null + ")
formula = as.formula(paste("is_factor ~ ", formula_str, " + ", formula_str_factor, ":predictor_is_null", sep=""))
n = nrow(df)
attach(df)
null_model = glm(as.formula("is_factor ~ 1"), data=df)
full_model = glm(formula, data=df)
# AIC/BIC step selection
aic_back_glm = eval(step(full_model, direction="backward", k=2)$call)
bic_back_glm = eval(step(full_model, direction="backward", k=log(n))$call)
aic_forward_glm = eval(step(null_model, scope=formula, direction="forward", k=2)$call)
bic_forward_glm = eval(step(null_model, scope=formula, direction="forward", k=log(n))$call)

summary(aic_back_glm)$deviance # lowest deviance here
summary(bic_back_glm)$deviance
summary(aic_forward_glm)$deviance
summary(bic_forward_glm)$deviance
anova(aic_back_glm, aic_forward_glm, test="Chisq")
anova(aic_back_glm, bic_back_glm, test="Chisq")
anova(aic_back_glm, bic_forward_glm, test="Chisq")
# analysis: they all differ

# mean_pval_fit is not significant, try removing
glm_combined = glm("is_factor ~ df_ratio_null + df_ratio_fit + alias_fit + 
    median_pval_null + pval_param + predictor_is_null + 
    is_na_anova_p + is_na_pval_param + is_na_adj_rsq_param + 
    adj_rsq_null_to_fit_diff + adj_n_slopes_null_to_fit_ratio + 
    df_fit_to_null_ratio + df_ratio_fit_to_null_ratio + df_ratio_null:predictor_is_null + 
    df_ratio_fit:predictor_is_null + 
    predictor_is_null:is_na_adj_rsq_param + predictor_is_null:adj_rsq_null_to_fit_diff + 
    predictor_is_null:df_ratio_fit_to_null_ratio", data = df)
# not significant difference, slightly worse residual
anova(aic_back_glm, glm_combined, test="Chisq")

# try power transform on reduced model
list_of_columns = cbind(
    df$df_ratio_null,
    df$df_ratio_fit,
    df$alias_fit,
    df$median_pval_null + 0.0001,
    df$pval_param + 0.0001,
    df$adj_rsq_null_to_fit_diff + 1,
    df$adj_n_slopes_null_to_fit_ratio,
    df$df_fit_to_null_ratio,
    df$df_ratio_fit_to_null_ratio)
transforms = summary(powerTransform(list_of_columns))$result
# don't use alias_fit since this isn't really a numerical thing
# some of the other powers are problematic, let's just try log transform
df$log_df_ratio_null = log(df$df_ratio_null)
df$log_df_ratio_fit = log(df$df_ratio_fit)
df$pow_alias_fit = df$alias_fit^(-3)
df$log_median_pval_null = log(df$median_pval_null + 0.0001)
df$log_pval_param = log(df$pval_param + 0.0001)
df$pow_adj_rsq_null_to_fit_diff = df$adj_rsq_null_to_fit_diff^(3)
df$log_adj_n_slopes_null_to_fit_ratio = log(df$adj_n_slopes_null_to_fit_ratio)
df$pow_df_fit_to_null_ratio = df$df_fit_to_null_ratio^(3)
df$log_df_ratio_fit_to_null_ratio = log(df$df_ratio_fit_to_null_ratio)

glm_transformed_1 = glm("is_factor ~ log_df_ratio_null + log_df_ratio_fit + alias_fit + 
    log_median_pval_null + log_pval_param + predictor_is_null + 
    is_na_anova_p + is_na_pval_param + is_na_adj_rsq_param + 
    adj_rsq_null_to_fit_diff + log_adj_n_slopes_null_to_fit_ratio + 
    df_fit_to_null_ratio + log_df_ratio_fit_to_null_ratio + df_ratio_null:predictor_is_null + 
    df_ratio_fit:predictor_is_null + 
    predictor_is_null:is_na_adj_rsq_param + predictor_is_null:adj_rsq_null_to_fit_diff + 
    predictor_is_null:df_ratio_fit_to_null_ratio", data = df)
anova(glm_combined, glm_transformed_1, test="Chisq")
# not significant!

# try transforming the other powers of 3
glm_transformed_2 = glm("is_factor ~ df_ratio_null + df_ratio_fit + pow_alias_fit + 
    median_pval_null + pval_param + predictor_is_null + 
    is_na_anova_p + is_na_pval_param + is_na_adj_rsq_param + 
    pow_adj_rsq_null_to_fit_diff + adj_n_slopes_null_to_fit_ratio + 
    pow_df_fit_to_null_ratio + df_ratio_fit_to_null_ratio + df_ratio_null:predictor_is_null + 
    df_ratio_fit:predictor_is_null + 
    predictor_is_null:is_na_adj_rsq_param + predictor_is_null:adj_rsq_null_to_fit_diff + 
    predictor_is_null:df_ratio_fit_to_null_ratio", data = df)
anova(glm_combined, glm_transformed_2, test="Chisq")
# not significant!

# try transforming everything
glm_transformed_3 = glm("is_factor ~ log_df_ratio_null + log_df_ratio_fit + pow_alias_fit + 
    log_median_pval_null + log_pval_param + predictor_is_null + 
    is_na_anova_p + is_na_pval_param + is_na_adj_rsq_param + 
    pow_adj_rsq_null_to_fit_diff + log_adj_n_slopes_null_to_fit_ratio + 
    pow_df_fit_to_null_ratio + log_df_ratio_fit_to_null_ratio + df_ratio_null:predictor_is_null + 
    df_ratio_fit:predictor_is_null + 
    predictor_is_null:is_na_adj_rsq_param + predictor_is_null:adj_rsq_null_to_fit_diff + 
    predictor_is_null:df_ratio_fit_to_null_ratio", data = df)
anova(glm_combined, glm_transformed_2, test="Chisq")
# still not significant!

# still a lot of covariance
sort(vif(glm_combined))
glm_pruned = glm("is_factor ~ df_ratio_null + alias_fit + 
    median_pval_null + pval_param + predictor_is_null + 
    is_na_anova_p + is_na_pval_param + is_na_adj_rsq_param + 
    adj_rsq_null_to_fit_diff + 
    df_fit_to_null_ratio + df_ratio_null:predictor_is_null + 
    predictor_is_null:is_na_adj_rsq_param + predictor_is_null:adj_rsq_null_to_fit_diff + 
    predictor_is_null:df_ratio_fit_to_null_ratio", data = df)
summary(glm_pruned)
anova(glm_combined, glm_pruned, test="Chisq")
# unfortunately it's not significantly similar, and also has worse residuals

# vif is too high for most
max(vif(aic_back_glm))
max(vif(bic_back_glm))  # only these are reasonable
max(vif(aic_forward_glm))
max(vif(bic_forward_glm))  # only these are reasonable

# the recombined model is too hard to interpret
# so split models again
df_no_predictor = engineered_df[engineered_df$predictor_is_null,]
df_no_predictor$predictor_is_null = NULL
full_model_no_predictor = glm(as.formula("is_factor ~ ."), data=df_no_predictor)
null_model_no_predictor = glm(as.formula("is_factor ~ 1"), data=df_no_predictor)

aic_back_glm_no_predictor = eval(step(full_model_no_predictor, direction="backward", k=2)$call)
bic_back_glm_no_predictor = eval(step(full_model_no_predictor, direction="backward", k=log(n))$call)
aic_forward_glm_no_predictor = eval(step(null_model_no_predictor, scope=formula(full_model_no_predictor), direction="forward", k=2)$call)
bic_forward_glm_no_predictor = eval(step(null_model_no_predictor, scope=formula(full_model_no_predictor), direction="forward", k=log(n))$call)
max(vif(aic_back_glm_no_predictor)) # this is no good
max(vif(bic_back_glm_no_predictor))
max(vif(aic_forward_glm_no_predictor))
max(vif(bic_forward_glm_no_predictor))
summary(aic_back_glm_no_predictor)$deviance
summary(bic_back_glm_no_predictor)$deviance
summary(aic_forward_glm_no_predictor)$deviance
summary(bic_forward_glm_no_predictor)$deviance
mmps(aic_back_glm_no_predictor)
mmps(bic_back_glm_no_predictor) # these are terrible
mmps(aic_forward_glm_no_predictor) # this one is the tightest
mmps(bic_forward_glm_no_predictor) # these are terrible
intersect_coefficients(aic_back_glm_no_predictor, aic_forward_glm_no_predictor)
# try to remove the non-significant variables (last 2)
summary(aic_back_glm_no_predictor)
aic_back_glm_no_predictor$call
glm(formula = is_factor ~ df_ratio_fit + aic_null_to_fit_diff + 
    anova_p + adj_rsq_param + is_na_anova_p + is_na_pval_param + 
    adj_rsq_null_to_fit_diff + max_pval_null_to_fit_diff, data = df_no_predictor)

pruned_aic_back_glm_no_predictor = glm(formula = is_factor ~ df_ratio_fit + aic_null_to_fit_diff + 
    anova_p + adj_rsq_param + is_na_anova_p + is_na_pval_param, data = df_no_predictor)
# this is just barely not significantly different, pick simpler one
anova(aic_back_glm_no_predictor, pruned_aic_back_glm_no_predictor, test="Chisq")
# final model in the case where predictor is null (only response ~ candidate)
summary(pruned_aic_back_glm_no_predictor)
mmps(pruned_aic_back_glm_no_predictor)

df_with_predictor = engineered_df[!engineered_df$predictor_is_null,]
df_with_predictor$predictor_is_null = NULL
full_model_with_predictor = glm(as.formula("is_factor ~ ."), data=df_with_predictor)
null_model_with_predictor = glm(as.formula("is_factor ~ 1"), data=df_with_predictor)

aic_back_glm_with_predictor = eval(step(full_model_with_predictor, direction="backward", k=2)$call)
bic_back_glm_with_predictor = eval(step(full_model_with_predictor, direction="backward", k=log(n))$call)
aic_forward_glm_with_predictor = eval(step(null_model_with_predictor, scope=formula(full_model_with_predictor), direction="forward", k=2)$call)
bic_forward_glm_with_predictor = eval(step(null_model_with_predictor, scope=formula(full_model_with_predictor), direction="forward", k=log(n))$call)
max(vif(aic_back_glm_with_predictor)) # too high
max(vif(bic_back_glm_with_predictor)) # too high
max(vif(aic_forward_glm_with_predictor))
max(vif(bic_forward_glm_with_predictor))
summary(aic_back_glm_with_predictor)$deviance
summary(bic_back_glm_with_predictor)$deviance
summary(aic_forward_glm_with_predictor)$deviance
summary(bic_forward_glm_with_predictor)$deviance
# the regression is not quite as good as the no_predictor case
mmps(aic_back_glm_with_predictor)
mmps(bic_back_glm_with_predictor)
mmps(aic_forward_glm_with_predictor)
mmps(bic_forward_glm_with_predictor)

# bic_back_glm_with_predictor is a strict subset with only slightly higher deviance
intersect_coefficients(aic_back_glm_with_predictor, bic_back_glm_with_predictor)
# see if we can prune bic_back_glm_with_predictor to get rid of vif issue
vif(bic_back_glm_with_predictor)
bic_back_glm_with_predictor$call

pruned_bic_back_glm_with_predictor = glm(formula = is_factor ~ df_ratio_null + aic_null_to_fit_diff + 
    is_na_anova_p + adj_rsq_null_to_fit_diff + df_ratio_fit_to_null_ratio, 
    data = df_with_predictor)
# they are different, because the two collinear terms have opposite slopes, meaning they are a nonlinearity!
anova(bic_back_glm_with_predictor, pruned_bic_back_glm_with_predictor, test="Chisq")
# take the most interpretable model
# final model in the case where predictor is not null (response ~ predictor + candidate + candidate:predictor)
summary(bic_back_glm_with_predictor)
mmps(bic_back_glm_with_predictor)

pruned_aic_back_glm_no_predictor$call
bic_back_glm_with_predictor$call

final_no_predictor = glm(formula = is_factor ~ df_ratio_fit + aic_null_to_fit_diff + 
    anova_p + adj_rsq_param + is_na_anova_p + is_na_pval_param, data = df_no_predictor)

final_with_predictor = glm(formula = is_factor ~ df_ratio_null + df_ratio_fit + aic_null_to_fit_diff + 
    is_na_anova_p + adj_rsq_null_to_fit_diff + df_ratio_fit_to_null_ratio, 
    data = df_with_predictor)