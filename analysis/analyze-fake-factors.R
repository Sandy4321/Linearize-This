source("src/factor.R")

df_c = mtcars
pred = c("mpg", "hp", "wt")

df_cf = fake_factor(df_c, c(2,4,6))
df_ci2 = fake_interaction(df_c, num_shuffle=2, bias_slope=-2, bias_sum=5)
df_ci4 = fake_interaction(df_c, num_shuffle=4, bias_slope=-2, bias_sum=5)
df_ci6 = fake_interaction(df_c, num_shuffle=6, bias_slope=-2, bias_sum=5)
df_cl = fake_linear(df_c, c(2:5))
head(df_cf)
head(df_ci2)
head(df_cl)

plot_by_factor(df_cf, "SHUFFLE_INDEX", responses=pred, predictors=pred)
plot_by_factor(df_cf, "FAKE_FACTORx2", responses=pred, predictors=pred)
plot_by_factor(df_cf, "FAKE_FACTORx4", responses=pred, predictors=pred)
plot_by_factor(df_cf, "FAKE_FACTORx6", responses=pred, predictors=pred)
plot_by_factor(df_ci2, "SHUFFLE_INDEX", responses=pred, predictors=pred)
plot_by_factor(df_ci4, "SHUFFLE_INDEX", responses=pred, predictors=pred)
plot_by_factor(df_ci6, "SHUFFLE_INDEX", responses=pred, predictors=pred)
plot_by_factor(df_cl, "LINEAR_FACTORxmpg", responses=pred, predictors=pred)
plot_by_factor(df_cl, "LINEAR_FACTORxhp", responses=pred, predictors=pred)
plot_by_factor(df_cl, "LINEAR_FACTORxwt", responses=pred, predictors=pred)


nrow(quakes)
df_q = quakes

df_qi = fake_interaction(df_q, bias_slope=-1.5, bias_sum=1)
df_qf = fake_factor(df_q, c(2,4,8,16,32))
df_ql = fake_linear(df_q, c(2,4,8,16,32))
head(df_qi)
head(df_qf)
head(df_ql)

df_qi$SHUFFLE_INDEX = as.factor(df_qi$SHUFFLE_INDEX)
df_qf$FAKE_FACTORx2 = as.factor(df_qf$FAKE_FACTORx2)
df_qf$FAKE_FACTORx4 = as.factor(df_qf$FAKE_FACTORx4)
df_qf$FAKE_FACTORx8 = as.factor(df_qf$FAKE_FACTORx8)
df_qf$FAKE_FACTORx16 = as.factor(df_qf$FAKE_FACTORx16)
df_qf$FAKE_FACTORx32 = as.factor(df_qf$FAKE_FACTORx32)
df_ql$LINEAR_FACTORxmag = as.factor(df_ql$LINEAR_FACTORxmag)
df_ql$LINEAR_FACTORxstations = as.factor(df_ql$LINEAR_FACTORxstations)
df_ql$LINEAR_FACTORxdepth = as.factor(df_ql$LINEAR_FACTORxdepth)
df_ql$LINEAR_FACTORxlat = as.factor(df_ql$LINEAR_FACTORxlat)
df_ql$LINEAR_FACTORxlong = as.factor(df_ql$LINEAR_FACTORxlong)

pred = c("depth", "mag", "stations")
plot_by_factor(df_qi, "SHUFFLE_INDEX", responses=pred, predictors=pred)
plot_by_factor(df_qf, "FAKE_FACTORx2", responses=pred, predictors=pred)
plot_by_factor(df_qf, "FAKE_FACTORx4", responses=pred, predictors=pred)
plot_by_factor(df_qf, "FAKE_FACTORx8", responses=pred, predictors=pred)
plot_by_factor(df_qf, "FAKE_FACTORx16", responses=pred, predictors=pred)
plot_by_factor(df_qf, "FAKE_FACTORx32", responses=pred, predictors=pred)
plot_by_factor(df_ql, "LINEAR_FACTORxmag", responses=pred, predictors=pred)
plot_by_factor(df_ql, "LINEAR_FACTORxstations", responses=pred, predictors=pred)
plot_by_factor(df_ql, "LINEAR_FACTORxdepth", responses=pred, predictors=pred)
plot_by_factor(df_ql, "LINEAR_FACTORxlat", responses=pred, predictors=pred)
plot_by_factor(df_ql, "LINEAR_FACTORxlong", responses=pred, predictors=pred)

pred = c("depth", "lat", "long")
plot_by_factor(df_qi, "SHUFFLE_INDEX", responses=pred, predictors=pred)
plot_by_factor(df_qf, "FAKE_FACTORx2", responses=pred, predictors=pred)
plot_by_factor(df_qf, "FAKE_FACTORx4", responses=pred, predictors=pred)
plot_by_factor(df_qf, "FAKE_FACTORx8", responses=pred, predictors=pred)
plot_by_factor(df_qf, "FAKE_FACTORx16", responses=pred, predictors=pred)
plot_by_factor(df_qf, "FAKE_FACTORx32", responses=pred, predictors=pred)
plot_by_factor(df_ql, "LINEAR_FACTORxmag", responses=pred, predictors=pred)
plot_by_factor(df_ql, "LINEAR_FACTORxstations", responses=pred, predictors=pred)
plot_by_factor(df_ql, "LINEAR_FACTORxdepth", responses=pred, predictors=pred)
plot_by_factor(df_ql, "LINEAR_FACTORxlat", responses=pred, predictors=pred)
plot_by_factor(df_ql, "LINEAR_FACTORxlong", responses=pred, predictors=pred)

# these are very obvious different slopes and intercepts
# to detect interaction terms: different slopes
summary(lm("mag~SHUFFLE_INDEX", data=df_qi))

# to detect linearity: different "intercepts" i.e. slopes as factors
# these are not significant slopes
summary(lm("mag~FAKE_FACTORx2", data=df_qf))
summary(lm("mag~FAKE_FACTORx4", data=df_qf))
summary(lm("mag~FAKE_FACTORx8", data=df_qf))
summary(lm("mag~FAKE_FACTORx16", data=df_qf))
summary(lm("mag~FAKE_FACTORx32", data=df_qf))

# these are linear intercepts (significant)
summary(lm("mag~LINEAR_FACTORxmag", data=df_ql))
summary(lm("mag~LINEAR_FACTORxdepth", data=df_ql))
summary(lm("mag~LINEAR_FACTORxstations", data=df_ql))
summary(lm("mag~LINEAR_FACTORxlat", data=df_ql))
summary(lm("mag~LINEAR_FACTORxlong", data=df_ql))

# these are identical slopes with the simple linear model
lm_qmf = lm("mag~stations", data=df_qf)
lm_qmf2 = lm("mag~stations:FAKE_FACTORx2+FAKE_FACTORx2", data=df_qf)
summary(lm_qmf2)
anova(lm_qmf, lm_qmf2)

lm_qmf4 = lm("mag~stations:FAKE_FACTORx4+FAKE_FACTORx4", data=df_qf)
summary(lm_qmf4)
anova(lm_qmf, lm_qmf4)

lm_qmf8 = lm("mag~stations:FAKE_FACTORx8+FAKE_FACTORx8", data=df_qf)
summary(lm_qmf8)
anova(lm_qmf, lm_qmf8)

lm_qmf16 = lm("mag~stations:FAKE_FACTORx16+FAKE_FACTORx16", data=df_qf)
summary(lm_qmf16)
anova(lm_qmf, lm_qmf16)

# this one performs very badly due to artificial interactions
summary(lm("mag~stations", data=df_qi))
# this one performs much better - compare the R sq
summary(lm("mag~stations:SHUFFLE_INDEX+SHUFFLE_INDEX", data=df_qi))


# how to compare simple linear vs interaction?
# can use ANOVA of linear vs nonlinear model
# can compare R squared change

# these should not be interactions
lm_qml_sm = lm("mag~stations+LINEAR_FACTORxmag", data=df_ql)
summary(lm_qml_sm)
lm_qml_smi = lm("mag~stations+LINEAR_FACTORxmag", data=df_ql)
summary(lm_qml_smi)
anova(lm_qml_sm, lm_qml_smi)

lm_qml_sd = lm("mag~stations+LINEAR_FACTORxdepth", data=df_ql)
summary(lm_qml_sd)
lm_qml_sdi = lm("mag~stations+LINEAR_FACTORxdepth", data=df_ql)
summary(lm_qml_sdi)
anova(lm_qml_sd, lm_qml_sdi)

lm_qdl_sl = lm("depth~mag+LINEAR_FACTORxlong", data=df_ql)
summary(lm_qdl_sl)
lm_qdl_sli = lm("depth~mag:LINEAR_FACTORxlong+LINEAR_FACTORxlong", data=df_ql)
summary(lm_qdl_sli)
anova(lm_qdl_sl, lm_qdl_sli)

# this should be interaction, and anova supports it!
lm_qll_ll = lm("lat~long+LINEAR_FACTORxlong", data=df_ql)
summary(lm_qll_ll)
lm_qll_lli = lm("lat~long:LINEAR_FACTORxlong+LINEAR_FACTORxlong", data=df_ql)
summary(lm_qll_lli)
anova(lm_qll_ll, lm_qll_lli)
