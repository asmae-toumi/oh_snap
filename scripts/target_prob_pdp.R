
source('scripts/target_prob_setup.R')
source('scripts/xgboost_pdp_helpers.R')
library(xgboost)
library(DALEX)

fit_tp <- .path_data_small('fit_target_prob_all') %>% xgboost::xgb.load()

.suffix <- 'all'
plays <- import_plays()

features <-
  import_target_prob_features(suffix = .suffix) %>%
  semi_join(plays %>% select(game_id, play_id), by = c('game_id', 'play_id')) %>%
  distinct(game_id, play_id, frame_id, nfl_id, .keep_all = TRUE) %>%
  mutate(idx = row_number()) %>%
  relocate(idx)

features_start <-
  features %>%
  filter(event == 'ball_snap')

set.seed(42)

features_x <- features %>% sample_frac(0.1)

cols_lst <-
  list(
    col_y = 'is_target',
    cols_id = c('game_id', 'play_id', 'frame_id', 'nfl_id'),
    cols_id_model = c('idx'),
    cols_keep = c(
      'is_target',
      'idx_o',
      'game_id',
      'play_id',
      'nfl_id',
      'frame_id',
      'idx',
      'los',
      'qb_o',
      'qb_x',
      'qb_y',
      'x_rusher',
      'y_rusher',
      'dist_rusher',
      'idx_o',
      'x',
      'y',
      'o',
      'o_d1_naive',
      'dist_ball',
      'dist_d1_naive',
      'dist_ball_d1_naive',
      'dist_d2_naive',
      'dist_ball_d2_naive',
      'sec'
    )
  )

features_df <-
  features_x %>%
  left_join(
    features_start %>%
      select(game_id, play_id, nfl_id, frame_id_start = frame_id)
  ) %>%
  mutate(sec = 0.1 * (frame_id - frame_id_start)) %>%
  select(any_of(cols_lst$cols_keep)) %>%
  # drop_na() %>%
  filter(!is.na(idx_o) & !is.na(qb_o)) %>%
  mutate(idx = row_number()) %>%
  relocate(idx)
features_df

features_mat <-
  model.matrix(
    ~ . + 0,
    data =
      features_df %>%
        select(one_of(cols_lst$cols_keep)) %>%
        select(-one_of(c(cols_lst$col_y, cols_lst$cols_id, cols_lst$cols_id_model)))
  )
features_mat

features_dmat <-
  xgboost::xgb.DMatrix(
    features_mat,
    label = features_df[[cols_lst$col_y]]
  )

col_y_sym <- cols_lst$col_y %>% sym()
features_df_filt <-
  features_df %>%
  filter(!is.na(!!col_y_sym)) %>%
  distinct(game_id, play_id, frame_id, nfl_id, .keep_all = TRUE) %>%
  # Choose last frame.
  group_by(game_id, play_id, nfl_id) %>% 
  slice_max(frame_id, with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(idx = row_number()) %>%
  relocate(idx)
features_df_filt

features_mat_filt <-
  model.matrix(
    ~ . + 0,
    data =
      features_df_filt %>%
        select(one_of(cols_lst$cols_keep)) %>%
        select(-one_of(c(cols_lst$col_y, cols_lst$cols_id, cols_lst$cols_id_model)))
  )

# pdp, dalex ----
cols_x <- c('y', 'dist_ball', 'dist_d1_naive')
feature_labs <- .get_feature_labs()
x_labs <- feature_labs %>% filter(feature %in% cols_x) %>% deframe()

explainer <-
  DALEX::explain(
    fit_tp,
    data = features_mat_filt,
    y = features_df_filt[[cols_lst$col_y]],
    predict_function = .predict_logit,
    link = .logit,
    colorize = FALSE
  )

set.seed(42)
part <-
  DALEX::model_profile(
    explainer,
    N = 1000, # How many lines
    variable = cols_x,
    type = 'partial'
  )

nms <- part$cp_profiles %>% names()
match_idx <- which(nms %in% cols_x)
nms[match_idx] <- x_labs
names(part$cp_profiles) <- nms

.change_vname <- function(data) {
  data %>% 
    left_join(feature_labs %>% rename(`_vname_` = feature), by = '_vname_') %>% 
    select(-`_vname_`) %>% 
    rename(`_vname_` = feature_lab)
}

part$cp_profiles <- part$cp_profiles %>% .change_vname()
part$agr_profiles <- part$agr_profiles %>% .change_vname()

p <- 
  part %>% 
  .plot_pdp(variables = x_labs) +
  labs(
    title = 'Target Probability Features', 
    y = 'Probability',
    x = NULL
  )

ggsave(
  plot = p,
  filename = .path_figs_png('tp_pdp_dalex'),
  width = 7,
  height = 10,
  # cairo for the sharper image quality
  type = 'cairo'
)

# pdp, flashlight ----
# library(flashlight)
# prep_xgb <- function(data, x) {
#   data %>%
#     select_at(x) %>%
#     # mutate_if(Negate(is.numeric), as.integer) %>%
#     data.matrix()
# }
# 
# fl_xgb <- flashlight::flashlight(
#   model = fit_tp, 
#   label = 'xgb',
#   predict_function = function(mod, X) predict(mod, prep_xgb(X, x))
# )
# fl_xgb
# 
# pdp, pdp ----
# # library(pdp)
# # debugonce(pdp::partial)
# # debugonce(pdp:::getParDepCls)
# # debugonce(pdp:::getIceClsLogit)
# # Need this and it wasn't saved for some reason.
# fit_tp$params$objective <- 'binary:logistic'
# .f_predict <- function(object, newdata) {
#   newdata <- xgboost::xgb.DMatrix(features_mat_filt, missing = NA)
#   predict(fit_tp, newdata)
# }
# 
# col_x <- 'dist_rusher'
# col_x_sym <- col_x %>% sym()
# # debugonce(pdp:::partial.default)
# part <- 
#   pdp::partial(
#     fit,
#     pred.var = col_x,
#     # quantiles = TRUE,
#     ice = TRUE, 
#     # center = TRUE,
#     # ice = FALSE,
#     # type = 'classification',
#     # pred.fun = .f_predict,
#     center = FALSE,
#     plot = FALSE,
#     train = features_mat_filt
#   ) %>% 
#   as_tibble() %>% 
#   rename(x = !!col_x_sym)
# part
# 
# part_agg <-
#   part %>% 
#   as_tibble() %>% 
#   group_by(x) %>% 
#   summarize(
#     across(yhat, list(median = median, sd = sd, q90 = ~quantile(.x, 0.9), q10 = ~quantile(.x, 0.1)), .names = '{fn}')
#   ) %>% 
#   ungroup() %>% 
#   pivot_longer(
#     -x,
#     names_to = 'key',
#     values_to = 'value'
#   )
# part_agg
# 
# part_agg %>% 
#   filter(key != 'sd') %>% 
#   ggplot() +
#   aes(x = x, y = value, group = key) +
#   geom_line()
# 
# p1 %>% 
#   as_tibble() %>% 
#   filter(dist_ball < 30) %>% 
#   ggplot() +
#   aes(x = dist_ball, y = yhat, group = yhat.id) +
#   geom_line(alpha = 0.1)
# 
# ggsave(plot = print(p1), filename = 'temp.png', width = 10, height = 6)

# pdp, iml ----
# .f_predict <- function(object, newdata) {
#   newdata <- xgboost::xgb.DMatrix(features_mat_filt, missing = NA)
#   predict(fit_tp, newdata)
# }
# 
# set.seed(42)
# # folds <- caret::createFolds(model_data_bdb$epa_bdb, k = 10, returnTrain = FALSE)
# # names(folds) <- NULL
# # idx <- folds[[1]]
# idx <- 1:nrow(features_mat_filt)
# features_mat_filt_sample <- features_mat_filt[idx, ]
# df <-
#   features_mat_filt_sample %>%
#   as.data.frame()
# 
# pred <-
#   iml::Predictor$new(
#     fit_tp,
#     data = df,
#     y = features_df_filt[[cols_lst$col_y]],
#     predict.fun = .f_predict
#   )
# 
# fe <-
#   iml::FeatureEffect$new(
#     pred,
#     feature = c('dist_rusher')
#   )
# fe$
# plot(fe)
