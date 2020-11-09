
library(tidyverse)
source('scripts/read_files.R')
positions <- read_positions()
plays <- read_plays()

weeks <- 1
features <-
  weeks %>%
  map_dfr( ~ read_csv(file.path(
    'data',
    sprintf('coverage_identification_features_week%d.csv', .x)
  ))) %>%
  inner_join(
    positions %>% 
      mutate(across(label, ~if_else(position == 'DB', 'CB', .x))) %>% 
      select(
        side,
        position_category = category,
        position_label = label,
        position
      )
  )
features

bad_plays <-
  features %>% 
  count(game_id, play_id, nfl_id) %>% 
  # Identify plays with only one event. Probably need to move this cleaning to prior script.
  filter(n == 1L) %>% 
  distinct(game_id, play_id)
bad_plays
features <- features %>% anti_join(bad_plays) %>% mutate(idx = row_number())
features %>% count(game_id, play_id, nfl_id, event) %>% filter(n != 1L)
features %>% count(event)
# plays_w_defender <-
#   plays %>% 
#   select(game_id, play_id, play_description) %>% 
#   filter(play_description %>% str_detect('incomplete')) %>% 
#   filter(play_description %>% str_detect('\\(.*\\)[.]$'))

# library(reticulate)
sklearn <- reticulate::import('sklearn')

.valid_events <- c('postsnap-prethrow', 'postsnap-prethrow')
.valid_position_labels <- c('CB', 'S', 'LB')
.valid_covariance_types <- c('full', 'spherical', 'tied', 'diag')
generate_gmm_params_grid <- function() {
  gmm_params_grid <-
    crossing(
      event = .valid_events,
      position_label = .valid_position_labels,
      covariance_type = .valid_covariance_types,
      # covariance_type = 'full',
      n_components = c(2L, 3L, 4L, 6L, 8L) # seq.int(2L, 9L)
    )
}

.select_mat <- function(mat, cols) {
  mat[,cols, drop = FALSE]
}

.unselect_mat <- function(mat, cols) {
  mat[,setdiff(colnames(mat), cols), drop = FALSE]
}

.prepare_results <- function(clf, mat, ..., save_preds = TRUE, save_probs = FALSE) {
  # browser()
  mat_noidx <- mat %>% .unselect_mat('idx')
  bic <- clf$bic(mat_noidx)
  res <- list(bic = bic)
  # mat[,setdiff(colnames(mat), 'idx'), drop = FALSE]
  preds <- clf$predict(mat_noidx)
  # browser()
  # ari <- sklearn$metrics$adjusted_rand_score(mat, preds)

  mat_idx <- mat %>% .select_mat('idx') %>% as_tibble()
  if(save_preds) {
    preds <- 
      preds %>% 
      tibble(cl_best = .) %>% 
      bind_cols(mat_idx)
    res <- c(res, list(preds = preds))
  }

  if(save_probs) {
    probs <- 
      clf$predict_proba(mat) %>% 
      as_tibble() %>%
      set_names(sprintf('cl_%d', 1:ncol(probs))) %>% 
      bind_cols(mat_idx)
    res <- c(res, list(probs = probs))
  }
  res
}

.evaluate_split <- function(clf, split, ...) {
  # split <- trn_folds %>% slice(1) %>% pull(splits) %>% pluck(1)
  # browser()
  trn <- split %>% rsample::analysis()
  tst <- split %>% rsample::assessment()
  
  # # Don't need to scale? 
  # # Reference: https://stats.stackexchange.com/questions/371333/is-it-important-to-make-a-feature-scaling-before-using-gaussian-mixture-model/371336
  # trn_mat <- trn %>% as.matrix()
  # tst_mat <- tst %>% as.matrix()
  # 
  # # Reference: https://stackoverflow.com/questions/35276490/scikit-learn-predicting-new-raw-and-unscaled-instance-using-models-trained-with
  # scaler <- sklearn$preprocessing$StandardScaler()
  # # mat <- scaler$transform(mat)
  # # trn_mat <- scaler$fit_transform(trn_mat)
  # # tst_mat <- scaler$fit_transform(tst_mat)
  # trn_mat <- scaler$fit_transform(trn_mat)
  # tst_mat <- scaler$transform(tst_mat)
  
  trn_mat <- trn %>% as.matrix()
  tst_mat <- tst %>% as.matrix()
  clf$fit(trn_mat %>% .unselect_mat('idx'))
  
  res <- map(list('trn' = trn_mat, 'tst' = tst_mat), ~.prepare_results(clf = clf, mat = .x, ...))
  # res <- .prepare_results(clf = clf, mat = tst_mat, ...)
  # browser()
  res <- res %>% enframe(name = 'set', value = 'value') %>% unnest_wider(value)
  # browser()
  # res <- res %>% enframe() %>% unnest_wider(value)
  res
}

.fit_sklearn_gmm_cv <-
  function(data, clf, n_folds = 10L, ...) {
    folds <- 
      rsample::vfold_cv(data, v = n_folds) %>% 
      mutate(across(id, ~str_remove(.x, 'Fold') %>% as.integer())) %>% 
      rename(fold = id)

    # browser()
    res <-
      folds %>% 
      mutate(res = map(splits, ~.evaluate_split(clf = clf, split = .x, ...))) %>% 
      select(-splits)
    # browser()
    # res %>% unnest_wider(res)
    res %>% unnest(res)
  }

.prepare_sklearn_gmm <-
  function(features,
           event = .valid_events,
           position_label = .valid_position_labels,
           covariance_type = .valid_covariance_types,
           n_components = 2L,
           ...,
           seed = 0L) {
    # event = 'postsnap-prethrow'
    # position_label = 'CB'
    # covariance_type = 'full'
    # n_components = 3L
    # save_preds = FALSE
    # save_probs = FALSE
    # n_folds = 10L
    # prop = 0.75
    # seed = 0L
    event <- match.arg(event)
    position_label <- match.arg(position_label)
    covariance_type <- match.arg(covariance_type)
    
    data <-
      features %>%
      filter(event == !!event & position_label == !!position_label) %>% 
      select(
        idx,
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
    
    clf <- sklearn$mixture$GaussianMixture(
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
           # 'holdout' probably shouldn't be used since CV is generally better for choosing metrics.
           # Output format is slightly different for 'cv' compared to the other two `how`s, which I should fix.
           how = c('cv', 'none'),
           prop = 0.8,
           seed = 0L) {
    how <- match.arg(how)

    res_prep <- .prepare_sklearn_gmm(seed = seed, ...)
    data <- res_prep$data
    clf <- res_prep$clf
    # split <- rsample::initial_split(data, prop = prop)
    if(how == 'cv') {
      # res <- .fit_sklearn_gmm_cv(clf = clf, data = split %>% rsample::analysis(), ...)
      res <- .fit_sklearn_gmm_cv(clf = clf, data = data, ...)
    } else {
      mat <- data %>% as.matrix()
      clf$fit(mat %>% .unselect_mat('idx'))
      res <- .prepare_results(clf = clf, mat = mat, ...)
    }
    res
  }

gmm_params_grid <- generate_gmm_params_grid()
res_gmm_cv <-
  gmm_params_grid %>% 
  slice(1) %>% 
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
          cv = TRUE
        )
      )
  )
res_gmm_cv

res_gmm_cv_unnested <-
  res_gmm_cv %>% 
  unnest(res)
res_gmm_cv_unnested %>% unnest(set) # %>% unnest(bic)

res_gmm_cv_unnested %>% 
  head(10) %>% 
  select(event, position_label, covariance_type, n_components, fold, set, preds) %>% 
  # pivot_wider(names_from = id, values_from = preds)
  unnest(preds) %>% 
  # group_by(idx) %>% 
  count(set, idx, sort = TRUE) %>% 
  count(n, name = 'nn')

res_gmm_cv_agg <-
  res_gmm_cv_unnested %>% 
  select(-splits, -id) %>% 
  group_by_at(vars(-bic)) %>% 
  summarize(
    n = n(),
    across(bic, mean)
  ) %>% 
  ungroup()
res_gmm_cv_agg

res_gmm_cv_agg %>% 
  select(event, position_label, covariance_type, n_components, bic) %>% 
  filter(covariance_type %in% c('full', 'diag')) %>% 
  mutate(across(n_components, ordered)) %>% 
  ggplot() +
  aes(x = n_components, y = bic, group = covariance_type, fill = covariance_type) +
  geom_col(position = 'dodge') +
  facet_grid(event~position_label)

res_gmm_holdout <-
  gmm_params_grid %>% 
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
          cv = FALSE,
          save_preds = TRUE
        )
      )
  )
res_gmm_holdout %>% unnest_wider(res)

gmm_unnested_best <-
  res_gmm_unnested %>%
  filter(set == 'tst') %>% 
  group_by(event, position_label) %>% 
  slice_min(bic) %>% 
  ungroup()
gmm_unnested_best

# mclust ----
# library(mclust)
fit <- data %>% select(matches('_(mean|var)')) %>% mclust::Mclust(G = 2)
fit
colnames(fit$z) <- sprintf('p_%d', 1:fit$G)
probs <- fit$z %>% as_tibble()
probs_tidy <-
  probs %>% 
  mutate(idx = row_number()) %>% 
  pivot_longer(-idx, names_to = 'cluster', values_to = 'p') %>% 
  mutate(across(cluster, ~str_remove(.x, 'p_') %>% as.integer())) %>% 
  group_by(idx) %>% 
  filter(p == max(p)) %>% 
  ungroup()
probs_tidy
probs_tidy %>% skimr::skim()
probs_tidy %>% ggplot() + aes(x = p) + geom_histogram(binwidth = 0.1)

features_assigned <- data %>% bind_cols(probs_tidy) %>% relocate(cluster, p)
features_assigned 
probs_tidy %>% count(cluster)


fit_all <- data %>% mclust::Mclust()
n_optimal_cluster <- fit_all$G
n_optimal_cluster
fit_all$z

rec_unsup <- 
  features %>% 
  filter(event == 'postsnap-prethrow') %>% 
  recipes::recipe(formula( ~ .), data = .) %>% 
  recipes::update_role(
    -matches('_(mean|var)'),
    new_role = 'extra'
  ) %>% 
  # recipes::step_normalize(recipes::all_predictors()) %>% 
  # recipes::step_dummy(event, one_hot = TRUE) %>% 
  embed::step_umap(recipes::all_predictors())
rec_unsup
# library(recipes)
prep_unsup <- rec_unsup %>% recipes::prep()
juice_unsup <- prep_unsup %>% recipes::juice()
juice_unsup
viz_umap <-
  juice_unsup %>% 
  ggplot() +
  aes(x = umap_1, y = umap_2) +
  geom_point(aes(color = position), alpha = 0.5, size = 1) +
  facet_wrap(~event) + 
  labs(color = NULL)
viz_umap

prep_unsup
# mixtools::mvnormalmixEM()
fit

library(RouteIdentification)
library(tidyverse)

# Generate data
nested_trajectory_data <- rand_centred_curves(n_clust = 3, n_curves = 20)
nested_trajectory_data
# Apply EM algorithm, either to generated data or appropriately formatted data
# em_results <- driver_em_nested(nested_trajectory_data, K = 3)

# debugonce(cluster_trajectory_data)
cluster_trajectory_data <- function(trajectory_data, P = 3, K = 3, niter = 20){
  
  trajectory_data <- nested_trajectory_data %>% unnest(c(x, y))
  trajectory_data
  
  P = 5
  # create data for em algorithm
  prepared_trajectory_data <-
    trajectory_data %>%
    tidyr::nest(data = c(x, y)) %>%
    dplyr::mutate(curve_i = row_number()) %>%
    dplyr::mutate(n_i = purrr::map_dbl(data, nrow),
                  t_i = purrr::map(n_i, ~ tibble::tibble(t = (1:. - 1)/(.-1))),
                  T_i = purrr::map(t_i, ~ dplyr::mutate(., p = list(0:P)) %>%
                                     tidyr::unnest(cols = c(p)) %>%
                                     dplyr::mutate(D_p_of_t = choose(P, p) * t^p * (1 - t)^(P - p)) %>%
                                     tidyr::spread(p, D_p_of_t, sep = "_") %>%
                                     dplyr::select(-t))) %>%
    dplyr::select(data, curve_i, n_i, t_i, T_i) %>%
    dplyr::arrange(curve_i) %>%
    ungroup()
  
  prepared_trajectory_data %>% 
    slice(2) %>% 
    pull(T_i) %>% 
    pluck(1) %>% 
    mutate(idx = row_number()) %>% 
    pivot_longer(-idx) %>%
    mutate(across(name, ~str_remove(.x, 'p_') %>% as.integer())) %>% 
    ggplot() +
    aes(x = name, y = idx) +
    geom_tile(aes(fill = value))
  
  # initial values, event spread across `p_i` columns.
  X <-
    prepared_trajectory_data %>%
    dplyr::select(T_i) %>%
    tidyr::unnest(cols = c(T_i)) %>%
    Matrix::as.matrix()
  
  # original data
  Y <-
    prepared_trajectory_data %>%
    dplyr::select(data) %>%
    tidyr::unnest(cols = data) %>%
    Matrix::as.matrix()
  
  # index indicating row index of each curve in the X and Y matrix (which combines data across curves)
  SEQ <-
    prepared_trajectory_data %>%
    dplyr::select(n_i) %>%
    dplyr::mutate(n_i = cumsum(n_i)) %>%
    Matrix::as.matrix()
  
  # another index, essentially marking which curve each row belongs to, e.g. if the first curve has 35 points, then this is 1 up thru row 35, then goes to 2, etc.
  INDEX <-
    prepared_trajectory_data %>%
    dplyr::select(curve_i, t_i) %>%
    tidyr::unnest(cols = c(t_i)) %>%
    dplyr::select(curve_i)
  
  ##################### INIT
  
  # last x,y pair in each curve 
  kmean_data <-
    prepared_trajectory_data %>%
    dplyr::transmute(ends = purrr::map(data, ~ filter(., row_number() == max(row_number())) %>% dplyr::select(x, y))) %>%
    tidyr::unnest(cols = c(ends))
  
  
  kmeans_results <- kmeans(kmean_data, centers = K, iter.max = 100)
  kmeans_results
  
  kmean_data %>%
    dplyr::mutate(cluster = kmeans_results$cluster) %>%
    ggplot2::ggplot(aes(x = x, y = y, colour = factor(cluster))) +
    ggplot2::geom_point() +
    ggplot2::theme_bw()
  
  # cluster numbers, "stacked" such than the number of rows is equivalent to the trajectory data unnested
  init_clusters <-
    prepared_trajectory_data %>%
    dplyr::mutate(cluster = kmeans_results$cluster) %>%
    tidyr::unnest(data) %>%
    dplyr::select(cluster)
  
  # relative proportion of curves in each cluster (weighted by number of points across all curves in each cluster)
  Alpha <-
    init_clusters %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(prop = n/sum(n)) %>%
    dplyr::pull(prop)
  
  # same thing as `init_cluster`, just as a plain vector.
  c <- init_clusters %>% dplyr::pull(cluster)
  
  
  calc_Piik <- function(data, Sigma){
    data %>%
      transmute(Piik = purrr::pmap_dbl(list(x, y, x1, y1), ~ mvtnorm::dmvnorm(c(..1, ..2),
                                                                              c(..3, ..4),
                                                                              Sigma))) %>%
      dplyr::bind_cols(as_tibble(INDEX))
  }
  
  Beta <- list()
  Sigma <- list()
  tic()
  for(k in 1:K){
    Beta[[k]] <- solve(t(X[k == c, ]) %*% X[k == c, ]) %*% t(X[k == c, ]) %*% Y[k == c, ]
    Sigma[[k]] <- diag(diag(t(Y[k == c, ] - X[k == c, ] %*% Beta[[k]]) %*% (Y[k == c, ] - X[k == c, ] %*% Beta[[k]])/ nrow(X[k == c, ])))
  }
  toc()
  
  n <- length(SEQ)
  N <- SEQ[n]
  curve_lengths <- prepared_trajectory_data %>% dplyr::select(n_i)
  
  #################################################################################
  l_hood <- -Inf
  tic()
  
  
  for(i in 1:niter){
    print(i)
    
    #############################################################################
    # Expectation Step
    print("e_step time")
    tic()
    
    data_Piik <-
      tibble::tibble(Beta, Sigma) %>%
      dplyr::mutate(k = row_number()) %>%
      #partition() %>%
      dplyr::mutate(X_Beta = purrr::map(Beta,
                                        ~ Matrix::as.matrix(X) %*% .x %>%
                                          tibble::as_tibble() %>%
                                          dplyr::bind_cols(rename(tibble::as_tibble(Matrix::as.matrix(Y)), x1 = x, y1 = y)))) %>%
      dplyr::mutate(Piik = purrr::map2(X_Beta, Sigma, calc_Piik)) %>%
      dplyr::select(k, Piik) %>%
      #collect() %>%
      tidyr::unnest(cols = c(Piik))
    
    scale_m <-
      data_Piik %>%
      dplyr::ungroup() %>%
      dplyr::summarise(mean = mean(Piik)) %>%
      dplyr::pull(mean)
    
    Pik <-
      data_Piik %>%
      dplyr::mutate(Piik = Piik/scale_m) %>%
      dplyr::group_by(curve_i, k) %>%
      dplyr::summarise(Pik = prod(Piik))  %>%
      tidyr::spread(k, Pik) %>%
      dplyr::ungroup() %>%
      dplyr::select(-curve_i) %>%
      Matrix::as.matrix()
    
    Pik <- Pik * Alpha
    
    
    
    toc()
    #############################################################################
    
    # Calculate Log Likelihood
    
    # Calculate Probability of data over all clusters
    s <- rowSums(Pik)
    
    # Since we're not on the log scale we might get 0
    if(any(s == 0)){
      # replace 0 with the smallest number possible
      # then weight by the alphas
      Pik[s == 0, ] <- .Machine$double.xmin * Alpha
      # recalculate the probability of observing this data over all clusters
      s <- rowSums(Pik)
    }
    
    # Now calculate the new log likelihood
    l_hood_new <- sum(log(s)) + N * log(scale_m)
    
    # If we've reached our tolerance stop the loop
    if(abs(l_hood - l_hood_new) < 1e-6){
      break
    }
    
    # For monitoring
    print(l_hood)
    # overwrite the old log likelihood
    l_hood <- l_hood_new
    
    # Calculate the Pi_ik
    Pik <- Pik/s
    
    # Perform Maximization Step
    
    print("m_step time")
    tic()
    #############################################################################
    Alpha <- colSums(Pik) / n
    
    
    weights <-
      Pik %>%
      tibble::as_tibble() %>%
      dplyr::mutate(curve_i = row_number()) %>%
      dplyr::right_join(INDEX, by = "curve_i") %>%
      dplyr::select(matches("\\d")) %>%
      as.list()
    
    param_updates <-
      tibble::tibble(k = 1:K, weights = weights) %>%
      dplyr::mutate(weights = purrr::map(weights, ~ Matrix::Diagonal(x = .))) %>%
      #partition() %>%
      dplyr::mutate(Beta_new  = purrr::map(weights, ~ calc_new_beta(., X, Y)),
                    Sigma_new = purrr::map2(weights, Beta_new, ~ calc_new_sigma(.x, X, Y, .y)),
                    Beta_new  = purrr::map(Beta_new, as.matrix)) %>%
      #collect() %>%
      dplyr::arrange(k)
    
    Beta <- param_updates %>% dplyr::pull(Beta_new)
    Sigma <- param_updates %>% dplyr::pull(Sigma_new)
    
    #############################################################################
    toc()
    
    
    
  }
  toc()
  
  em_results <-
    list("l_hood" = l_hood_new,
         "Pik"    = Pik,
         "Beta"   = Beta,
         "Sigma"  = Sigma,
         "Alpha"  = Alpha)
  
  em_results <-
    em_results %>%
    reorder_clusters()
  
  return(em_results)
}

# em_results <- 
#   nested_trajectory_data %>% 
#   tidyr::unnest(cols = c(x, y)) %>% 
#   cluster_trajectory_data(K = 3)
