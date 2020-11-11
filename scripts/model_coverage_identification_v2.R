
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
.valid_position_labels <- c('CB', 'S', 'LB')
.valid_covariance_types <- c('full') # , 'spherical', 'tied', 'diag')

generate_gmm_params_grid <- function() {
  gmm_params_grid <-
    crossing(
      event = .valid_events,
      position_label = .valid_position_labels,
      covariance_type = .valid_covariance_types,
      # covariance_type = c('full', 'diag'),
      # covariance_type = 'full',
      n_components = rev(c(2L, 3L, 4L, 5L)) # , 8L)
      # n_components = seq.int(2L, 9L)
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

.evaluate_split <- function(clf_trn, clf_tst, split, ..., verbose = TRUE) {
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
  clf_trn$fit(trn_mat %>% .unselect_mat(c('idx', 'week')))
  # if(verbose) {
  #   cat(glue::glue('Fitting testing model (with only {w}) at {Sys.time()}.'), sep = '\n')
  # }
  clf_tst$fit(tst_mat %>% .unselect_mat(c('idx', 'week')))
  
  res_trn <- .prepare_results(clf = clf_trn, mat = tst_mat)
  res_tst <- .prepare_results(clf = clf_tst, mat = tst_mat)

  .to_mat <- function(data) {
    data %>% select(-matches('cluster')) %>% as.matrix()
  }

  dists <- pracma::distmat(.to_mat(res_trn$means), .to_mat(res_tst$means))
  idx_min <- clue::solve_LSAP(dists, maximum = FALSE)
  labs <- tibble(cluster = seq_along(idx_min), cluster_reordered = as.vector(idx_min))

  .fix_cluster_col <- function(data) {
    data %>% 
      left_join(labs, by = 'cluster') %>% 
      select(-cluster) %>% 
      rename(cluster = cluster_reordered) %>% 
      relocate(cluster)
  }
  res_trn$means <- res_trn$means %>% .fix_cluster_col() %>% arrange(cluster)
  res_trn$probs <- res_trn$probs %>% .fix_cluster_col()
  res_trn$preds <- res_trn$preds %>% .fix_cluster_col()

  .rename_cols <- function(data, suffix = c('trn', 'tst'), cols) {
    data %>% rename_with(~sprintf('%s_%s', .x, suffix), .cols = any_of(cols))
  }
  
  preds_joined <- 
    inner_join(
      res_trn$probs %>% .rename_cols('trn', 'prob'),
      res_tst$probs %>% .rename_cols('tst', 'prob'),
      by = c('cluster', 'idx')
    )
  
  preds_joined <- 
    inner_join(
      res_trn$preds %>% .rename_cols('trn', c('pred', 'cluster')),
      res_tst$preds %>% .rename_cols('tst', c('pred', 'cluster')),
      by = 'idx'
    )
  preds_joined %>% filter(cluster_trn == cluster_tst)
  # sklearn$metrics$adjusted_rand_score(preds_joined$cluster_trn, preds_joined$cluster_tst)
  res <- list('trn' = res_trn, 'tst' = res_tst)
  res
}

.fit_sklearn_gmm_cv <-
  function(data, clf_trn, clf_tst, ..., weeks = .weeks, verbose = TRUE) {
    
    # .make_split <- function(week) {
    #   trn <- data %>% filter(week != !!week) %>% select(-week)
    #   tst <- data %>% filter(week == !!week) %>% select(-week)
    #   list(trn = trn, tst = tst)
    # }
    # folds <-
    #   weeks %>% 
    #   # 1L %>% 
    #   tibble(fold = .) %>% 
    #   mutate(split = map(fold, .make_split))
    
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
            ~.evaluate_split(clf_trn = clf_trn, clf_tst = clf_tst, split = ..1, ..., verbose = verbose)
          )
      )
    # browser()
    res <-
      res %>% 
      select(-split) %>% 
      unnest_longer(res, indices_to = 'set') %>% 
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
           msg = NULL) {
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
    
    clf_trn <-
      sklearn$mixture$GaussianMixture(
        n_components = n_components, 
        covariance_type = covariance_type,
        random_state = seed
      )
    
    clf_tst <-
      sklearn$mixture$GaussianMixture(
        n_components = n_components, 
        covariance_type = covariance_type,
        random_state = seed
      )

    res <-
      list(
        data = data,
        clf_trn = clf_trn,
        clf_tst = clf_tst
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
      res <- .fit_sklearn_gmm_cv(clf_trn = res_prep$clf_trn, clf_tst = res_prep$clf_tst, data = res_prep$data, verbose = verbose, ...)
    } else {
      # TODO: Test this at some point.
      mat <- res_prep$data %>% as.matrix()
      res_prep$clf_trn$fit(mat %>% .unselect_mat(c('idx', 'week')))
      res <- .prepare_results(clf = res_prep$clf_trn, mat = mat, ...)
    }
    res
  }

feature_cols <- get_feature_cols()
res_gmm_cv_vi <-
  feature_cols %>% 
  tibble(excluded_feature = .) %>% 
  add_row(excluded_feature = NULL) %>% 
  mutate(
    res = 
      pmap(
        list(excluded_feature),
        ~fit_sklearn_gmm(
          features = features,
          event = 'postsnap-prethrow',
          position_label = 'CB',
          covariance_type = 'full',
          n_components = 4L,
          how = 'cv',
          msg = glue::glue(' Feature: `{..1}`.'),
          features_to_exclude = ..1
        )
      )
  )
res_gmm_cv_vi

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
# 
# res_gmm_cv_vi %>% 
#   unnest_wider(res) %>% 
#   select(excluded_feature, preds) %>% 
#   # unnest(set) %>% 
#   unnest(preds) %>% 
#   pivot_wider(names_from = set, values_from = cluster)

res_gmm_cv_vi_agg_by_idx <-
  res_gmm_cv_vi %>% 
  unnest(res) %>% 
  select(excluded_feature, set, preds) %>% 
  # unnest(set) %>% 
  unnest(preds) %>% 
  pivot_wider(names_from = set, values_from = cluster)
res_gmm_cv_vi_agg_by_idx

gmm_cv_vi_aris <-
  res_gmm_cv_vi_agg_by_idx %>% 
  nest(data = -c(excluded_feature)) %>% 
  mutate(ari = map_dbl(data, compute_cv_ari)) %>% 
  select(-data) %>% 
  arrange(desc(ari))
gmm_cv_vi_aris

res_gmm_cv_agg_by_idx <- read_rds('data/res_gmm_cv_agg_by_idx_v2.rds')
res_gmm_cv_agg_by_idx

res_gmm_cv_agg_by_idx <-
  res_gmm_cv %>% 
  unnest(res) %>% 
  select(event, position_label, covariance_type, n_components, fold, set, preds) %>% 
  # unnest(set) %>% 
  unnest(preds) %>% 
  pivot_wider(names_from = set, values_from = cluster)
res_gmm_cv_agg_by_idx
# save(res_gmm_cv_agg_by_idx, file = 'data/res_gmm_cv_agg_by_idx_v2.rda')
write_rds(res_gmm_cv_agg_by_idx, 'data/res_gmm_cv_agg_by_idx_v2.rds')

# res_gmm_cv_agg_by_idx %>% 
#   filter(n_components == 2L) %>% 
#   filter(position_label == 'S') %>% 
#   # count(is_same = trn == tst)
#   # rename(z = trn) %>% 
#   # rename(trn = tst) %>% 
#   # rename(tst = z) %>% 
#   # mutate(across(tst, ~if_else(.x == 2, 3, 2))) %>% 
#   # mutate(tst = 1) %>% 
#   # count(trn, tst)
#   mutate(across(tst, ~case_when(trn == 2 ~ 2, TRUE ~ .x))) %>% 
#   nest(data = -c(event, position_label, covariance_type, n_components)) %>% 
#   mutate(ari = map_dbl(data, compute_cv_ari)) %>% 
#   select(-data)

gmm_cv_aris <-
  res_gmm_cv_agg_by_idx %>% 
  nest(data = -c(event, position_label, covariance_type, n_components)) %>% 
  mutate(ari = map_dbl(data, compute_cv_ari)) %>% 
  select(-data)
gmm_cv_aris
