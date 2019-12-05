# traceback errors for debugging
options(error=function()traceback(2))

source("src/util.R")
source("src/columns.R")
source("src/logistic.R")

TRAIN_SET = file.path("data", "trainset")
TEST_SET = file.path("data", "testset")
EXTENSION = "csv"

# # only run once to get data
save_builtin_datasets_to_file(5, TRAIN_SET, EXTENSION)

filenames = list_data_filenames(TRAIN_SET, EXTENSION)
filenames = sapply(filenames, function(name) file.path(TRAIN_SET, name))
stats = iterate_columns(filenames)
save_dataframe(stats, timestamp("column_stats", EXTENSION), "data")

quakes = read.table("data/testset/quakes.csv", sep=",", header=TRUE)
quakes$AS_NUMERICxmag = as.numeric(quakes$MAYBE_FACTORxmag)
quakes$AS_FACTORxmag = as.factor(quakes$MAYBE_FACTORxmag)
quakes$AS_NUMERICxlong = as.numeric(quakes$NOT_FACTORxlong)
quakes$AS_FACTORxlong = as.factor(cut(quakes$NOT_FACTORxlong, breaks=10))
quakes$AS_NUMERICxlat = as.numeric(quakes$NOT_FACTORxlat)
quakes$AS_FACTORxlat = as.factor(cut(quakes$NOT_FACTORxlat, breaks=10))
test_df = data.frame(evaluate_columns(quakes, "MAYBE_FACTORxstations", NULL, "AS_NUMERICxmag", "AS_FACTORxmag", do_test=FALSE))
test_df = rbind(test_df, evaluate_columns(quakes, "MAYBE_FACTORxstations", "NOT_FACTORxdepth", "AS_NUMERICxmag", "AS_FACTORxmag", do_test=FALSE))
test_df = rbind(test_df, evaluate_columns(quakes, "NOT_FACTORxdepth", NULL, "AS_NUMERICxlong", "AS_FACTORxlong", do_test=FALSE))
test_df = rbind(test_df, evaluate_columns(quakes, "NOT_FACTORxdepth", "AS_NUMERICxmag", "AS_NUMERICxlong", "AS_FACTORxlong", do_test=FALSE))
test_df = rbind(test_df, evaluate_columns(quakes, "AS_NUMERICxmag", NULL, "AS_NUMERICxlong", "AS_FACTORxlong", do_test=FALSE))
test_df = rbind(test_df, evaluate_columns(quakes, "AS_NUMERICxmag", "MAYBE_FACTORxstations", "AS_NUMERICxlat", "AS_FACTORxlat", do_test=FALSE))
test_df = remove_null(test_df, DEFAULT_REQUIRES$NULL_PREDICTOR_PREFIX)
test_df = feature_engineering(test_df)
print(head(test_df))

df = read.table(file.path("data", "column_stats_2019-12-04-17-28-03.csv"), header=TRUE, sep=",")
df = combine_response(df)
df = feature_engineering(df)
df = remove_constant_cols(df)
glm_final = glm("is_factor ~ df_ratio_null + df_ratio_fit + alias_fit + 
    median_pval_null + pval_param + predictor_is_null + 
    is_na_anova_p + is_na_pval_param + is_na_adj_rsq_param + 
    adj_rsq_null_to_fit_diff + adj_n_slopes_null_to_fit_ratio + 
    df_fit_to_null_ratio + df_ratio_fit_to_null_ratio + df_ratio_null:predictor_is_null + 
    df_ratio_fit:predictor_is_null + 
    predictor_is_null:is_na_adj_rsq_param + predictor_is_null:adj_rsq_null_to_fit_diff + 
    predictor_is_null:df_ratio_fit_to_null_ratio", data = df)
predictions = predict(glm_final, newdata=test_df)
print(predictions)