
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
  preds <- clf$predict(mat_noidx)
  
  mat_idx <- mat %>% .select_mat('idx') %>% as_tibble()
  preds <- 
    (preds + 1L) %>% 
    tibble(cluster = .) %>% 
    bind_cols(mat_idx)

  probs <- clf$predict_proba(mat_noidx)
  colnames(probs) <- sprintf('%d', 1:ncol(probs))
  
  # NOTE: Can't directly assign column names to clf$means_, so need ot make a copy.
  means <- clf$means_
  colnames(means) <- colnames(mat_noidx)

  means <-
    means %>% 
    as_tibble() %>%
    rownames_to_column(var = 'cluster') %>% 
    # pivot_longer(
    #   -c(cluster),
    #   names_to = 'feature',
    #   values_to = 'mean'
    # ) %>% 
    mutate(across(cluster, as.integer))
  
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

.evaluate_split <- function(clf, split, ..., .week = 1L, verbose = TRUE) {
  if(verbose) {
    cat(glue::glue('Fitting and predicting for week {.week} at {Sys.time()}.'), sep = '\n')
  }
  # split <- folds %>% slice(1) %>% pull(split) %>% pluck(1)
  trn_mat <- split$trn %>% as.matrix()
  tst_mat <- split$tst %>% as.matrix()
  # clf_trn <- clf
  # clf_tst <- clf
  if(verbose) {
    cat(glue::glue('Fitting training model (without {.week}) at {Sys.time()}.'), sep = '\n')
  }
  clf_trn$fit(trn_mat %>% .unselect_mat(c('idx', 'week')))
  if(verbose) {
    cat(glue::glue('Fitting testing model (with only {.week}) at {Sys.time()}.'), sep = '\n')
  }
  clf_tst$fit(tst_mat %>% .unselect_mat(c('idx', 'week')))
  
  res_trn <- .prepare_results(clf = clf_trn, mat = tst_mat)
  res_tst <- .prepare_results(clf = clf_tst, mat = tst_mat)

  .to_mat <- function(data) {
    data %>% select(-matches('cluster')) %>% as.matrix()
  }
  # mat_sim <- text2vec::sim2(mat_x, mat_y)
  # mat_sim %>% apply(MARGIN=2, max)
  # cbind(t(mat_x), t(mat_y)) %>% cor()
  dists <- pracma::distmat(.to_mat(res_trn$means), .to_mat(res_tst$means))
  idx_min <- clue::solve_LSAP(dists, maximum = FALSE)
  # labs <- set_names(as.vector(idx_min), seq_along(idx_min))
  labs <- tibble(cluster = seq_along(idx_min), cluster_reordered = as.vector(idx_min))
  # labs <- 
  #   set_names(seq_along(idx_min), as.vector(idx_min)) %>% 
  #   enframe(name = 'cluster_reordered', value = 'cluster') %>% 
  #   mutate(across(cluster_reordered, as.integer))
  # labs

  .fix_cluster_col <- function(data) {
    data %>% 
      # mutate(cluster_reordered = labs[cluster]) %>% 
      # mutate(cluster_reordered = cluster %>% recode(labs))
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
      res_tst$probs %>% .rename_cols('tst', 'prob')
    )
  
  preds_joined <- 
    inner_join(
      res_trn$preds %>% .rename_cols('trn', c('pred', 'cluster')),
      res_tst$preds %>% .rename_cols('tst', c('pred', 'cluster'))
    )
  preds_joined %>% filter(cluster_trn == cluster_tst)
  # sklearn$metrics$adjusted_rand_score(preds_joined$cluster_trn, preds_joined$cluster_tst)

  # res <- res %>% enframe(name = 'set', value = 'value') %>% unnest_wider(value)
  res <- list('trn' = res_trn, 'tst' = res_tst)
  res
}

.fit_sklearn_gmm_cv <-
  function(data, clf, ..., verbose = TRUE) {
    
    .make_split <- function(week) {
      trn <- data %>% filter(week != !!week) %>% select(-week)
      tst <- data %>% filter(week == !!week) %>% select(-week)
      list(trn = trn, tst = tst)
    }
    folds <-
      # .weeks %>% 
      # 1L:17L %>% 
      1L %>% 
      tibble(fold = .) %>% 
      mutate(split = map(fold, .make_split))
    
    # if(verbose) {
    #   cat(glue::glue('Fitting and predicting for week {.week} at {Sys.time()}.'), sep = '\n')
    # }
    # browser()  
    res <-
      folds %>% 
      mutate(
        res = 
          map2(
            split, fold, 
            ~.evaluate_split(clf = clf, split = ..1, ..., .week = ..2, verbose = verbose)
          )
      ) %>% 
      select(-split) %>% 
      unnest(res)
    res
  }

.prepare_sklearn_gmm <-
  function(features,
           event = .valid_events,
           position_label = .valid_position_labels,
           covariance_type = .valid_covariance_types,
           n_components = 2L,
           ...,
           seed = 0L) {
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
    
    data <-
      features %>%
      filter(event == !!event & position_label == !!position_label) %>% 
      select(
        idx,
        week,
        x_mean,
        x_var,
        y_mean,
        y_var,
        s_mean,
        s_var,
        off_mean,
        off_var,
        def_mean,
        rat_mean,
        rat_var,
        off_dir_mean,
        off_dir_var,
        off_o_mean,
        off_o_var
      )
    
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
        clf = clf
      )
    res
  }

fit_sklearn_gmm <-
  function(...,
           # Use 'none' after choosing "best" parameters given CV evaluation.
           # Output format is slightly different for 'cv' compared to the other two `how`s, which I should fix.
           how = c('cv', 'none'),
           seed = 0L) {
    how <- match.arg(how)
    
    res_prep <- .prepare_sklearn_gmm(seed = seed, ...)
    data <- res_prep$data
    clf <- res_prep$clf
    # split <- rsample::initial_split(data, prop = prop)
    if(how == 'cv') {
      res <- .fit_sklearn_gmm_cv(clf = clf, data = data, ...)
    } else {
      mat <- data %>% as.matrix()
      clf$fit(mat %>% .unselect_mat('idx'))
      res <- .prepare_results(clf = clf, mat = mat, ...)
    }
    res
  }

gmm_params_grid <- generate_gmm_params_grid()

