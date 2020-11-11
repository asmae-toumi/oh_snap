
library(tidyverse)
source('scripts/read_files.R')
positions <- read_positions()
plays <- read_plays()

.weeks <- 1L:17L
features <-
  .weeks %>%
  tibble(week = .) %>% 
  mutate(
    data = 
      map(week, 
          ~vroom::vroom(
            file.path(
              'data',
              sprintf('coverage_identification_features_week%d.csv', .x)
            )
          )
      )
  ) %>%
  unnest(data) %>% 
  inner_join(
    positions %>% 
      mutate(across(label, ~if_else(position == 'DB', 'CB', .x))) %>% 
      select(
        side,
        position_category = category,
        position_label = label,
        position
      )
  ) %>% 
  filter(event == 'postsnap-prethrow') %>% 
  mutate(idx = row_number()) %>% 
  relocate(idx)
features

sklearn <- reticulate::import('sklearn')

.valid_events <- c('postsnap-prethrow') # , 'postthrow-preoutcome')
.valid_position_labels <- c('CB') # , 'S', 'LB')
.valid_covariance_types <- c('full') # , 'spherical', 'tied', 'diag')

generate_gmm_params_grid <- function() {
  gmm_params_grid <-
    crossing(
      event = .valid_events,
      position_label = .valid_position_labels,
      covariance_type = .valid_covariance_types,
      # covariance_type = c('full', 'diag'),
      # covariance_type = 'full',
      # n_components = rev(c(2L, 3L, 4L, 5L)) # , 8L)
      n_components = seq.int(2L, 9L)
    )
}

compute_cv_ari <- function(data) {
  sklearn$metrics$adjusted_rand_score(labels_true = as.integer(data$trn), labels_pred = as.integer(data$tst))
}

.select_mat <- function(mat, cols) {
  mat[,cols, drop = FALSE]
}

.unselect_mat <- function(mat, cols) {
  mat[,setdiff(colnames(mat), cols), drop = FALSE]
}

.prepare_results <- function(clf, mat, ..., tol = 1e-3) {
  # browser()
  mat_noidx <- mat %>% .unselect_mat(c('idx', 'week'))
  bic <- clf$bic(mat_noidx)
  
  mat_idx <- mat %>% .select_mat('idx') %>% as_tibble()
  
  # NOTE: Can't directly assign column names to clf$means_, so need to make a copy.
  means <- clf$means_
  colnames(means) <- colnames(mat_noidx)
  # browser()
  means <-
    means %>% 
    as_tibble() %>%
    rownames_to_column(var = 'cluster') %>% 
    mutate(across(cluster, as.integer))
  
  preds <- clf$predict(mat_noidx)
  preds <- 
    (preds + 1L) %>% 
    tibble(cluster = .) %>% 
    bind_cols(mat_idx)
  
  probs <- clf$predict_proba(mat_noidx)
  colnames(probs) <- sprintf('%d', 1:ncol(probs))
  
  probs <-
    probs %>% 
    as_tibble() %>%
    bind_cols(mat_idx) %>% 
    pivot_longer(
      -c(idx),
      names_to = 'cluster',
      values_to = 'prob'
    ) %>% 
    mutate(
      across(prob, ~case_when(.x > (1 - tol) ~ 1, .x < tol ~ 0, TRUE ~ .x)),
      across(cluster, as.integer)
    )
  res <- list(bic = bic, probs = probs, preds = preds, means = means)
  res
}

.evaluate_split <- function(clf, split, ..., verbose = TRUE) {
  # if(verbose) {
  #   cat(glue::glue('Fitting and predicting for week {w} at {Sys.time()}.'), sep = '\n')
  # }
  trn_mat <- split %>% rsample::analysis() %>% as.matrix()
  tst_mat <- split %>% rsample::assessment() %>% as.matrix()
  # trn_mat <- split$trn %>% as.matrix()
  # tst_mat <- split$tst %>% as.matrix()
  # Doesn't work due to python copying semantics (the same fit is made).
  # clf_trn <- clf
  # clf_tst <- clf
  # if(verbose) {
  #   cat(glue::glue('Fitting training model (without {w}) at {Sys.time()}.'), sep = '\n')
  # }
  clf$fit(trn_mat %>% .unselect_mat(c('idx', 'week')))
  # if(verbose) {
  #   cat(glue::glue('Fitting testing model (with only {w}) at {Sys.time()}.'), sep = '\n')
  # }

  # res <- .prepare_results(clf = clf, mat = tst_mat)
  res <- 
    map(
      list('trn' = trn_mat, 'tst' = tst_mat), 
      ~.prepare_results(clf = clf, mat = .x, ...)
    )
  res <- res %>% enframe(name = 'set', value = 'value') %>% unnest_wider(value)
  res
}

.fit_sklearn_gmm_cv <-
  function(data, clf, ..., weeks = .weeks, seed = 0L, verbose = TRUE) {

    set.seed(seed)
    folds <- 
      rsample::vfold_cv(data, v = 10) %>% 
      mutate(across(id, ~str_remove(.x, 'Fold') %>% as.integer())) %>% 
      rename(fold = id, split = splits)
    
    res <-
      folds %>% 
      mutate(
        res = 
          map2(
            split, fold, 
            ~.evaluate_split(clf = clf, split = ..1, ..., verbose = verbose)
          )
      )
    # browser()
    res <-
      res %>% 
      select(-split) %>% 
      # unnest_longer(res, indices_to = 'set') %>% 
      unnest_wider(res)
    res
  }

get_feature_cols <- function() {
  c(
    'x_mean',
    'x_var',
    'y_mean',
    'y_var',
    's_mean',
    's_var',
    'off_mean',
    'off_var',
    'def_mean',
    'rat_mean',
    'rat_var',
    'off_dir_mean',
    'off_dir_var' # ,
    # 'off_o_mean',
    # 'off_o_var'
  )
}

.prepare_sklearn_gmm <-
  function(features,
           event = .valid_events,
           position_label = .valid_position_labels,
           covariance_type = .valid_covariance_types,
           n_components = 2L,
           features_to_exclude = NULL, 
           ...,
           seed = 0L,
           verbose = TRUE,
           msg = '') {
    if(FALSE) {
      event = 'postsnap-prethrow'
      position_label = 'CB'
      covariance_type = 'full'
      n_components = 3L
      seed = 0L
    }
    event <- match.arg(event)
    position_label <- match.arg(position_label)
    covariance_type <- match.arg(covariance_type)
    # browser()
    if(verbose) {
      cat(glue::glue('Evaluating `event = "{event}"`, `position_label = "{position_label}"`, `covariance_type = "{covariance_type}"`, `n_components = {n_components}`.{msg}'), sep = '\n')
    }
    feature_cols_init <- get_feature_cols()
    feature_cols <- setdiff(feature_cols_init, features_to_exclude)
    # browser()
    extra_cols <- c('idx', 'week')
    data <-
      features %>%
      filter(event == !!event & position_label == !!position_label) %>% 
      select(one_of(c(extra_cols, feature_cols)))
    
    clf <-
      sklearn$mixture$GaussianMixture(
        n_components = n_components, 
        covariance_type = covariance_type,
        random_state = seed
      )

    res <-
      list(
        data = data,
        clf = clf
      )
    res
  }

fit_sklearn_gmm <-
  function(...,
           # Use 'none' after choosing "best" parameters given CV evaluation.
           # Output format is slightly different for 'cv' compared to the other two `how`s, which I should fix.
           how = c('cv', 'none'),
           seed = 0L,
           verbose = TRUE) {
    how <- match.arg(how)
    
    res_prep <- .prepare_sklearn_gmm(seed = seed, verbose = verbose, ...)
    if(how == 'cv') {
      # browser()
      res <- .fit_sklearn_gmm_cv(clf = res_prep$clf, data = res_prep$data, verbose = verbose, seed = seed, ...)
    } else {
      # TODO: Test this at some point.
      mat <- res_prep$data %>% as.matrix()
      res_prep$clf$fit(mat %>% .unselect_mat(c('idx', 'week')))
      res <- .prepare_results(clf = res_prep$clf, mat = mat, ...)
    }
    res
  }

# feature_cols <- get_feature_cols()
# res_gmm_cv_vi <-
#   feature_cols %>% 
#   tibble(excluded_feature = .) %>% 
#   add_row(excluded_feature = NULL) %>% 
#   mutate(
#     res = 
#       pmap(
#         list(excluded_feature),
#         ~fit_sklearn_gmm(
#           features = features,
#           event = 'postsnap-prethrow',
#           position_label = 'CB',
#           covariance_type = 'full',
#           n_components = 4L,
#           how = 'cv',
#           msg = glue::glue(' Feature: `{..1}`.'),
#           features_to_exclude = ..1
#         )
#       )
#   )
# res_gmm_cv_vi

gmm_params_grid <- generate_gmm_params_grid()
res_gmm_cv <-
  gmm_params_grid %>%
  # slice(1) %>%
  mutate(
    res =
      pmap(
        list(event, position_label, covariance_type, n_components),
        ~fit_sklearn_gmm(
          features = features,
          event = ..1,
          position_label = ..2,
          covariance_type = ..3,
          n_components = ..4,
          how = 'cv'
        )
      )
  )
res_gmm_cv

# # Comparing cluster means...
# res_gmm_cv %>% 
#   unnest_wider(res) %>% 
#   select(event, position_label, covariance_type, n_components, fold, set, means) %>% 
#   unnest(cols = c(fold, set, means)) %>% 
#   unnest(cols = c(set, means)) %>% 
#   filter(set == 'tst', n_components == 2L) %>% 
#   unnest(means) %>% 
#   filter(off_dir_mean > 0)

res_gmm_cv_unnested <-
  res_gmm_cv %>% 
  unnest_wider(res) %>% 
  # slice(1) %>% 
  unnest(cols = c(fold, set, bic, probs, preds, means)) %>% 
  unnest(cols = c(set, bic, probs, preds, means))
res_gmm_cv_unnested

baseline_means <-
  res_gmm_cv_unnested %>% 
  # filter(set == 'trn') %>% 
  group_by(event, position_label, covariance_type, n_components, set) %>% 
  slice_min(fold) %>% 
  ungroup() %>% 
  select(event, position_label, covariance_type, n_components, set, baseline_means = means)
baseline_means

.to_mat <- function(data) {
  data %>% select(-matches('cluster')) %>% as.matrix()
}

.fix_cluster_col <- function(data, clusters_reordered) {
  data %>% 
    left_join(clusters_reordered, by = 'cluster') %>% 
    select(-cluster) %>% 
    rename(cluster = cluster_reordered) %>% 
    relocate(cluster)
}

.get_new_cluster_order <- function(x, y) {
  dists <- pracma::distmat(.to_mat(x), .to_mat(y))
  idx_min <- clue::solve_LSAP(dists, maximum = FALSE)
  tibble(
    cluster = seq_along(idx_min), 
    cluster_reordered = as.vector(idx_min)
  )
}

# res_gmm_cv_unnested %>% 
#   left_join(baseline_means) %>% 
#   filter(n_components == 3, fold != 1, set == 'tst') %>% 
#   mutate(
#     clusters_reordered = map2(means,baseline_means, .get_new_cluster_order)
#   )

res_gmm_cv_unnested_reordered <-
  res_gmm_cv_unnested %>% 
  left_join(baseline_means) %>% 
  mutate(
    clusters_reordered = map2(baseline_means, means, .get_new_cluster_order),
    means = map2(means, clusters_reordered, ~.fix_cluster_col(..1, ..2)), #  %>% arrange(cluster)),
    probs = map2(probs, clusters_reordered, .fix_cluster_col),
    preds = map2(preds, clusters_reordered, .fix_cluster_col)
  )
res_gmm_cv_unnested_reordered

# res_gmm_cv_unnested_reordered %>% 
#   select(event, position_label, covariance_type, n_components, fold, set, clusters_reordered) %>% 
#   slice_max(fold) %>% 
#   unnest(clusters_reordered) %>% 
#   tail(9)
# 
# res_gmm_cv_unnested_reordered %>% 
#   filter(n_components == 3L, fold <= 6L, set == 'tst') %>% 
#   select(clusters_reordered, means, baseline_means) %>% 
#   mutate(
#     clusters_reordered = map(clusters_reordered, ~rename_all(.x, ~sprintf('%s_0', .x))),
#     means = map(means, ~rename_all(.x, ~sprintf('%s_1', .x))),
#     baseline_means = map(baseline_means, ~rename_all(.x, ~sprintf('%s_2', .x)))
#   ) %>% 
#   unnest(cols = c(clusters_reordered, means, baseline_means)) %>% 
#   mutate(idx = row_number()) %>% 
#   pivot_longer(-c(idx)) %>% 
#   arrange(name) %>% 
#   pivot_wider(names_from = name, values_from = value) -> x
# x %>% 
#   pivot_longer(-idx) %>% 
#   mutate(
#     grp = name %>% str_replace_all('(^.*_)([0-2]$)', '\\2') %>% as.factor(),
#     name = name %>% str_replace_all('(^.*)(_[0-2]$)', '\\1'),
#     idx = ordered(idx)
#   ) %>% 
#   ggplot() +
#   aes(x = idx, y = value) +
#   geom_point(aes(color = grp)) +
#   facet_wrap(~name, scales = 'free')

# res_gmm_cv_unnested %>% 
#   select(event, position_label, covariance_type, n_components, fold, set, means) %>% 
#   unnest(means)
# res_gmm_cv_unnested_reordered %>% 
#   select(event, position_label, covariance_type, n_components, fold, set, means) %>% 
#   unnest(means)

.identify_max_prob <- function(data, idx_grp = 1, idx_grp_max = Inf) {
  cat(glue::glue('Working on group {idx_grp} of {idx_grp_max} at {Sys.time()}.'), sep = '\n')
  # browser()
  data %>% 
    group_by(idx, cluster) %>% 
    summarize(
      n_fold = n(), # This should be just `n_folds` - 1
      across(prob, mean)
    ) %>% 
    # filter(prob == max(prob)) %>% 
    slice_max(prob, with_ties = FALSE) %>% 
    ungroup()
}

res_gmm_cv_unnested_reordered %>% 
  mutate(n_row = map_int(probs, nrow)) %>% 
  group_by(set) %>% 
  summarize(across(n_row, sum)) %>% 
  ungroup()

# library(data.table)
# res_gmm_cv_agg_by_idx_sample <-
#   res_gmm_cv_unnested_reordered %>% 
#   head(20) %>% 
#   select(event, position_label, covariance_type, n_components, fold, set, probs) %>% 
#   mutate(across(probs, ~map(.x, data.table::as.data.table))) %>% 
#   data.table::as.data.table() %>% 
#   .[,`:=`(probs = map(probs, .identify_max_prob))]
# res_gmm_cv_agg_by_idx_sample

res_gmm_cv_unnested_reordered %>% 
  head(20) %>% 
  mutate(n_row = map_int(probs, nrow)) %>% 
  summarize(across(n_row, sum))

res_gmm_cv_agg_by_idx <-
  res_gmm_cv_unnested_reordered %>% 
  # head(20) %>% 
  select(event, position_label, covariance_type, n_components, set, probs) %>% 
  unnest(probs) %>% 
  # group_by(event, position_label, covariance_type, n_components, set, idx, cluster) %>% 
  # summarize(
  #   # n_fold = n(), # This should be just `n_folds` - 1
  #   across(prob, mean)
  # ) %>% 
  # # filter(prob == max(prob)) %>% 
  # slice_max(prob, with_ties = FALSE) %>% 
  # ungroup()
  nest(probs = c(idx, cluster, prob)) %>% 
  mutate(idx_grp = row_number(), idx_grp_max = max(idx_grp)) %>% 
  mutate(
    probs = pmap(list(probs, idx_grp, idx_grp_max), .identify_max_prob)
  ) %>% 
  unnest(probs)
res_gmm_cv_agg_by_idx %>% count(idx)
res_gmm_cv_agg_by_idx %>% count(idx, cluster) %>% count(n, name = 'nn')
res_gmm_cv_agg_by_idx %>% filter(idx == 6L)
# res_gmm_cv_agg_by_idx <-
#   res_gmm_cv_unnested_reordered %>% 
#   data.table::as.data.table() %>%
#   .[,.(max_mpg = max(mpg)), keyby = .(event, position_label, covariance_type, n_components, fold, set, probs)]

res_gmm_cv_agg_by_idx %>% 
  select(-prob) %>% 
  pivot_wider(names_from = set, values_from = cluster) %>% 
  filter(n_components == 2)

gmm_cv_aris <-
  res_gmm_cv_agg_by_idx %>% 
  select(-matches('idx_')) %>% 
  # filter(set == 'tst') %>% 
  select(event, position_label, covariance_type, n_components, set, idx, cluster) %>% 
  pivot_wider(names_from = set, values_from = cluster) %>% 
  nest(data = -c(event, position_label, covariance_type, n_components)) %>% 
  mutate(ari = map_dbl(data, compute_cv_ari)) %>% 
  select(-data)
gmm_cv_aris

.identify_best_tst_reordering <- function(data) {
  browser()
  mat <-
    # hypo_best_init %>% 
    data %>% 
    # mutate(tst = trn) %>% 
    count(trn, tst, sort = TRUE) %>%
    arrange(tst) %>% 
    pivot_wider(names_from = tst, values_from = n, values_fill = 0L) %>% 
    arrange(trn) %>% 
    select(-trn) %>% 
    set_names(NULL) %>% 
    as.matrix()
  idx_min <- mat %>% clue::solve_LSAP(maximum = TRUE)
  tibble(
    tst = seq_along(idx_min), 
    tst_reordered = as.vector(idx_min)
  )
}

.fix_tst_col <- function(data, clusters_reordered) {

  res <-
    data %>%
    left_join(clusters_reordered, by = 'tst') %>%
    select(-tst) %>%
    rename(tst = tst_reordered)
  res
  browser()
  anti_join(res, data)
  res
  # data %>% mutate(tst = trn)
}

hypo_best_init <-
  res_gmm_cv_agg_by_idx %>% 
  select(-matches('idx_')) %>% 
  # filter(set == 'tst') %>% 
  filter(n_components == 9L) %>% 
  select(event, position_label, covariance_type, n_components, set, idx, cluster) %>% 
  pivot_wider(names_from = set, values_from = cluster) %>% 
  nest(data = -c(event, position_label, covariance_type, n_components)) %>% 
  mutate(
    tst_reordered = map(data, .identify_best_tst_reordering)
  ) %>% 
  mutate(
    data = map2(data, tst_reordered, .fix_tst_col)
  )
hypo_best_init

gmm_cv_aris_best <-
  hypo_best_init %>% 
  select(event, position_label, covariance_type, n_components, data) %>% 
  mutate(ari = map_dbl(data, compute_cv_ari)) %>% 
  select(-data)
gmm_cv_aris_best

