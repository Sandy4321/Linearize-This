# traceback errors for debugging
options(error=function()traceback(2))

source("src/util.R")
source("src/columns.R")
source("src/logistic.R")

TRAIN_SET = file.path("data", "trainset")
TEST_SET = file.path("data", "testset")
EXTENSION = "csv"

# only run once to get data
save_builtin_datasets_to_file(5, TRAIN_SET, EXTENSION)

# generate meta data
filenames = list_data_filenames(TRAIN_SET, EXTENSION)
filenames = sapply(filenames, function(name) file.path(TRAIN_SET, name))
stats = iterate_columns(filenames)
save_dataframe(stats, timestamp("column_stats", EXTENSION), "data")

# test example
quakes = read.table("data/testset/quakes.csv", sep=",", header=TRUE)
quakes$AS_NUMERICxmag = as.numeric(quakes$MAYBE_FACTORxmag)
quakes$AS_FACTORxmag = as.factor(quakes$MAYBE_FACTORxmag)
quakes$AS_NUMERICxlong = as.numeric(quakes$NOT_FACTORxlong)
quakes$AS_FACTORxlong = as.factor(cut(quakes$NOT_FACTORxlong, breaks=10))
quakes$AS_NUMERICxlat = as.numeric(quakes$NOT_FACTORxlat)
quakes$AS_FACTORxlat = as.factor(cut(quakes$NOT_FACTORxlat, breaks=10))

# this case should be linear (not factor)
test_df_no_predictor = data.frame(evaluate_columns(quakes, "MAYBE_FACTORxstations", NULL, "AS_NUMERICxmag", "AS_FACTORxmag", do_test=FALSE))
# this case should be factor
test_df_no_predictor = rbind(test_df_no_predictor, evaluate_columns(quakes, "NOT_FACTORxdepth", NULL, "AS_NUMERICxlong", "AS_FACTORxlong", do_test=FALSE))
# this case should be neutral (not correlated)
test_df_no_predictor = rbind(test_df_no_predictor, evaluate_columns(quakes, "AS_NUMERICxmag", NULL, "AS_NUMERICxlong", "AS_FACTORxlong", do_test=FALSE))

# this case should be linear (not factor)
test_df_with_predictor = data.frame(evaluate_columns(quakes, "MAYBE_FACTORxstations", "NOT_FACTORxdepth", "AS_NUMERICxmag", "AS_FACTORxmag", do_test=FALSE))
# this case should be factor
test_df_with_predictor = rbind(test_df_with_predictor, evaluate_columns(quakes, "NOT_FACTORxdepth", "AS_NUMERICxlat", "AS_NUMERICxlong", "AS_FACTORxlong", do_test=FALSE))
# this case should be factor leaning
test_df_with_predictor = rbind(test_df_with_predictor, evaluate_columns(quakes, "MAYBE_FACTORxstations", "AS_NUMERICxmag", "AS_NUMERICxlong", "AS_FACTORxlong", do_test=FALSE))

test_df_no_predictor = remove_null(test_df_no_predictor, DEFAULT_REQUIRES$NULL_PREDICTOR_PREFIX)
test_df_with_predictor = remove_null(test_df_with_predictor, DEFAULT_REQUIRES$NULL_PREDICTOR_PREFIX)
test_df_no_predictor = feature_engineering(test_df_no_predictor)
test_df_with_predictor = feature_engineering(test_df_with_predictor)

df = read.table(file.path("data", "column_stats_2019-12-05-05-07-10.csv"), header=TRUE, sep=",")
df = combine_response(df)
df = feature_engineering(df)
df = remove_constant_cols(df)

# the final models from analysis/analyze-logistic-model.R
df_no_predictor = df[df$predictor_is_null,]
df_with_predictor = df[!df$predictor_is_null,]
final_no_predictor = glm(formula = is_factor ~ df_ratio_fit + aic_null_to_fit_diff + 
    anova_p + adj_rsq_param + is_na_anova_p + is_na_pval_param, data = df_no_predictor)

final_with_predictor = glm(formula = is_factor ~ df_ratio_null + df_ratio_fit + aic_null_to_fit_diff + 
    is_na_anova_p + adj_rsq_null_to_fit_diff + df_ratio_fit_to_null_ratio, 
    data = df_with_predictor)

# predictions
predictions_no_predictor = predict(final_no_predictor, newdata=test_df_no_predictor)
print(predictions_no_predictor)
predictions_with_predictor = predict(final_with_predictor, newdata=test_df_with_predictor)
print(predictions_with_predictor)