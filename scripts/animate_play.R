
library(tidyverse)
source("scripts/gg_field.R")
source("scripts/read_files.R")
# remotes::install_github("dreamRs/prefixer") # to help with package namespacing

.coerce_to_mat <- function(data) {
  res <- data %>%
    dplyr::select(x, y) %>%
    as.matrix()
  rownames(res) <- data[["nfl_id"]]
  res
}

#' Compute min distances between players
#' 
#' @description Compute "optimal" nearest assignments of defensive players to individual offensive and defensive players (not including QB) in a single frame using the Hungarian method
#' @param data A data.frame with 4 columns: `nfl_id`, `side` (either "O" or "D"), `x`, `y`.
#' @return A tibble with 6 columns: `nfl_id_o`, `nfl_id_d`, `x_o`, `x_d`, `y_o`, `y_d`.
.compute_min_distances <- function(data) {
  o <- data %>%
    dplyr::filter(side == "O") %>%
    .coerce_to_mat()
  d <- data %>%
    dplyr::filter(side == "D") %>%
    .coerce_to_mat()
  dists <- fields::rdist(o, d)
  rows <- rownames(o)
  cols <- rownames(d)
  rownames(dists) <- rows
  colnames(dists) <- cols
  idx_min <- clue::solve_LSAP(dists, maximum = FALSE)
  cols_min <- cols[idx_min]
  pairs <-
    tibble::tibble(nfl_id_d = rows, nfl_id_o = cols_min) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("nfl_id"), as.integer))
  dists_tidy <-
    dists %>%
    tibble::as_tibble(rownames = "nfl_id_d") %>%
    tidyr::pivot_longer(-c(nfl_id_d), names_to = "nfl_id_o", values_to = "dist") %>%
    dplyr::relocate(nfl_id_o, nfl_id_d) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("nfl_id"), as.integer)) %>%
    dplyr::inner_join(data %>% dplyr::select(nfl_id_o = nfl_id, x_o = x, y_o = y), by = "nfl_id_o") %>%
    dplyr::inner_join(data %>% dplyr::select(nfl_id_d = nfl_id, x_d = x, y_d = y), by = "nfl_id_d")
  res <-
    dists_tidy %>%
    dplyr::inner_join(pairs, by = c("nfl_id_o", "nfl_id_d"))
  res
}

#' @source https://stackoverflow.com/a/17313561/120898
pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, "pt"), "mm"))
}

#' @source https://stackoverflow.com/questions/43627679/round-any-equivalent-for-dplyr/46489816#46489816
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

# TODO: Document this and add validation.
animate_play <-
  function(game_id = 2018090600,
           play_id = 75,
           tracking = read_week1(),
           positions = read_positions(),
           plays = read_plays(),
           games = read_games(),
           nearest_defender = TRUE,
           yardmin = NULL,
           yardmax = NULL,
           field_color = "#b3e3d6",
           line_color = "black",
           sideline_color = "white",
           ...,
           save = TRUE,
           dir = "figs",
           filename = sprintf("%s-%s.gif", game_id, play_id),
           path = file.path(dir, filename),
           end_pause = 3, # In seconds
           height = 600,
           width = 600,
           fps = 10) {
    
    meta <- tibble::tibble(game_id = game_id, play_id = play_id)
    
    tracking <-
      tracking %>% 
      dplyr::left_join(positions %>% dplyr::select(position, side), by = "position") %>%
      dplyr::inner_join(meta, by = c("game_id", "play_id"))
    
    ball <- tracking %>% dplyr::filter(display_name == "Football")
    
    tracking <-
      tracking %>%
      dplyr::filter(display_name != "Football") %>%
      dplyr::inner_join(ball %>% dplyr::select(game_id, play_id, frame_id, ball_x = x, ball_y = y))
    
    play <- plays %>% dplyr::inner_join(meta, by = c("game_id", "play_id"))
    game <- games %>% dplyr::inner_join(meta, by = "game_id")
    assertthat::assert_that(nrow(play) == 1L)
    
    if(nearest_defender) {
      events_end_route <-
        c(
          "pass_outcome_caught",
          "pass_outcome_incomplete",
          "qb_sack",
          "pass_outcome_interception",
          "pass_outcome_touchdown",
          "qb_strip_sack",
          "qb_spike"
        )
      
      tracking_dists <-
        tracking %>%
        # head(300) %>% 
        dplyr::group_by(game_id, play_id, nfl_id) %>%
        dplyr::mutate(
          group = dplyr::case_when(
            frame_id == min(frame_id) ~ 1L,
            dplyr::lag(event) %in% events_end_route ~ 1L,
            TRUE ~ 0L
          ),
          group = cumsum(group)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(group == 1L) %>%
        dplyr::select(-group)
      
      min_dists <-
        tracking_dists %>%
        dplyr::filter(position != "QB") %>%
        dplyr::select(game_id, play_id, nfl_id, frame_id, side, x, y) %>%
        tidyr::nest(data = -c(frame_id)) %>%
        dplyr::mutate(data = purrr::map(data, .compute_min_distances)) %>%
        tidyr::unnest(data)
    }
    
    line_of_scrimmage <- play$absolute_yardline_number
    play_direction <- tracking$play_direction[[1]]
    first_down_line <- line_of_scrimmage + ifelse(play_direction == "left", -1, 1) * play$yards_to_go
    
    ball <-
      tracking %>%
      dplyr::distinct(frame_id, x = ball_x, y = ball_y) %>%
      dplyr::mutate(nfl_id = NA_real_)
    
    if(is.null(yardmin) | is.null(yardmax)) {
      yardminmax <-
        tracking %>% 
        dplyr::summarize(dplyr::across(x, list(min = min, max = max)))
      
      if(is.null(yardmin)) {
        yardmin <- yardminmax$x_min
        yardmin <- round_any(yardmin, 10, floor)
      }
      
      if(is.null(yardmax)) {
        yardmax <- yardminmax$x_max
        yardmax <- round_any(yardmax, 10, ceiling)
      }
    }
    
    home_team <- game$home_team_abbr
    away_team <- game$visitor_team_abbr
    
    colors <- read_colors()
    home_color <- colors %>% dplyr::filter(team == home_team) %>% dplyr::pull(color)
    away_color <- colors %>% dplyr::filter(team == away_team) %>% dplyr::pull(color)
    if(play$possession_team == home_team) {
      offense_color <- home_color
      defense_color <- away_color
    } else {
      offense_color <- away_color
      defense_color <- home_color
    }
    
    p <-
      tracking %>%
      ggplot2::ggplot() +
      gg_field(
        yardmin = yardmin,
        yardmax = yardmax,
        field_color = field_color,
        line_color = line_color,
        sideline_color = sideline_color # ,
        # ...
      ) +
      ggplot2::aes(x = x, y = y, group = nfl_id) +
      ggplot2::geom_segment(
        data = tibble::tibble(),
        inherit.aes = FALSE,
        ggplot2::aes(x = line_of_scrimmage, y = 0, xend = line_of_scrimmage, yend = 0 + 160 / 3),
        size = 1.25
      ) +
      ggplot2::geom_segment(
        data = tibble::tibble(),
        inherit.aes = FALSE,
        ggplot2::aes(x = first_down_line, y = 0, xend = first_down_line, yend = 0 + 160 / 3),
        color = "#ffff7f",
        size = 2
      ) +
      ggplot2::geom_point(
        data = ball,
        inherit.aes = TRUE,
        size = 3,
        color = "brown"
      ) +
      # geom_point(
      #   aes(fill = side),
      #   size = 6,
      #   shape = 21,
      #   color = "black",
      #   show.legend = FALSE
      # ) +
      ggplot2::geom_text(
        ggplot2::aes(label = "\u25A0", color = side, angle = o),
        # size = 8,
        size = pts(32),
        show.legend = FALSE
      ) +
      ggplot2::geom_text(
        # aes(label = "\u0332", color = side, angle = o),
        ggplot2::aes(label = "\u2039", color = side, angle = o + 90),
        # size = 8,
        size = pts(32),
        vjust = 0.3,
        hjust = 2,
        show.legend = FALSE
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = jersey_number, angle = o),
        color = "white",
        # size = 3,
        size = pts(12),
        vjust = 1,
      ) +
      ggplot2::scale_color_manual(values = c("O" = offense_color, "D" = defense_color)) +
      ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 12)) +
      ggplot2::labs(
        caption = glue::glue("{game$visitor_team_abbr} @ {game$home_team_abbr}, Week {game$week}
                             Q{play$quarter}: {stringr::str_wrap(play$play_description, 100)}
                             EPA: {round(play$epa, 2)}
                             game_id = {game$game_id}, play_id = {play$play_id}"),
        x = NULL, y = NULL
      )
    p
    
    if(nearest_defender) {
      p <-
        p +
        ggplot2::geom_segment(
          data = min_dists,
          inherit.aes = FALSE,
          ggplot2::aes(x = x_o, y = y_o, xend = x_d, yend = y_d),
          color = "grey50"
        )
    }
    anim <- p + gganimate::transition_manual(frame_id)
    if(!save) {
      return(anim)
    }
    
    seconds <- ball %>% nrow() %>% {. / 10}
    nframe <- (seconds + end_pause) * fps
    
    if(!dir.exists(dirname(path))) {
      dir.create(dirname(path))
    }
    
    gganimate::animate(
      anim,
      nframe = nframe,
      fps = fps,
      height = height,
      width = width,
      renderer = gganimate::gifski_renderer(path),
      end_pause = end_pause * fps
    )
  }

