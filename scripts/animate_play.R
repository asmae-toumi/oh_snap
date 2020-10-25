
library(tidyverse)
source("scripts/gg_field.R")

read_colors <- memoise::memoise({function() {
  file.path("data", "teamcolors.csv") %>% 
    read_csv()
}})

read_positions <- memoise::memoise({function() {
  file.path("data", "positions.csv") %>% 
    read_csv()
}})

read_plays <- memoise::memoise({function() {
  file.path("data", "plays.csv") %>% 
    read_csv() %>%
    janitor::clean_names() %>%
    filter(!is.na(pass_result))
}})

read_games <- memoise::memoise({function() {
  file.path("data", "games.csv") %>% 
    read_csv() %>%
    janitor::clean_names()
}})

read_week1 <- memoise::memoise({function() {
  file.path("data", "week1.csv") %>% 
    vroom::vroom() %>%
    janitor::clean_names()
}})


.coerce_to_mat <- function(data) {
  res <- data %>%
    select(x, y) %>%
    as.matrix()
  rownames(res) <- data[["nfl_id"]]
  res
}

.compute_min_distances <- function(data) {
  o <- data %>%
    filter(side == "O") %>%
    .coerce_to_mat()
  d <- data %>%
    filter(side == "D") %>%
    .coerce_to_mat()
  dists <- fields::rdist(o, d)
  rows <- rownames(o)
  cols <- rownames(d)
  rownames(dists) <- rows
  colnames(dists) <- cols
  idx_min <- clue::solve_LSAP(dists, maximum = FALSE)
  cols_min <- cols[idx_min]
  pairs <-
    tibble(nfl_id_d = rows, nfl_id_o = cols_min) %>%
    mutate(across(starts_with("nfl_id"), as.integer))
  dists_tidy <-
    dists %>%
    as_tibble(rownames = "nfl_id_d") %>%
    pivot_longer(-c(nfl_id_d), names_to = "nfl_id_o", values_to = "dist") %>%
    relocate(nfl_id_o, nfl_id_d) %>%
    mutate(across(starts_with("nfl_id"), as.integer)) %>%
    inner_join(data %>% select(nfl_id_o = nfl_id, x_o = x, y_o = y), by = "nfl_id_o") %>%
    inner_join(data %>% select(nfl_id_d = nfl_id, x_d = x, y_d = y), by = "nfl_id_d")
  res <-
    dists_tidy %>%
    inner_join(pairs, by = c("nfl_id_o", "nfl_id_d"))
  res
}

#" @source https://stackoverflow.com/a/17313561/120898
pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, "pt"), "mm"))
}

#" @source https://stackoverflow.com/questions/43627679/round-any-equivalent-for-dplyr/46489816#46489816
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
           field_color = "#7fc47f",
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
    
    meta <- tibble(game_id = game_id, play_id = play_id)
    
    tracking <-
      tracking %>% 
      left_join(positions %>% select(position, side), by = "position") %>%
      inner_join(meta, by = c("game_id", "play_id"))
    
    ball <- tracking %>% filter(display_name == "Football")
    
    tracking <-
      tracking %>%
      filter(display_name != "Football") %>%
      inner_join(ball %>% select(game_id, play_id, frame_id, ball_x = x, ball_y = y))
    
    play <- plays %>% inner_join(meta, by = c("game_id", "play_id"))
    game <- games %>% inner_join(meta, by = "game_id")
    assertthat::assert_that(nrow(play) == 1L)
    
    if(nearest_defender) {
      min_dists <-
        tracking %>%
        filter(position != "QB") %>%
        select(game_id, play_id, nfl_id, frame_id, side, x, y) %>%
        nest(data = -c(frame_id)) %>%
        mutate(data = map(data, .compute_min_distances)) %>%
        unnest(data)
    }
    
    line_of_scrimmage <- play$absolute_yardline_number
    play_direction <- tracking$play_direction[[1]]
    first_down_line <- line_of_scrimmage + ifelse(play_direction == "left", -1, 1) * play$yards_to_go
    
    ball <- tracking %>%
      distinct(frame_id, x = ball_x, y = ball_y) %>%
      mutate(nfl_id = NA_real_)
    
    if(is.null(yardmin) | is.null(yardmax)) {
      yardminmax <-
        tracking %>% 
        summarize(across(x, list(min = min, max = max)))
      
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
    colors
    home_color <- colors %>% filter(team == home_team) %>% pull(color)
    away_color <- colors %>% filter(team == away_team) %>% pull(color)
    if(play$possession_team == home_team) {
      offense_color <- home_color
      defense_color <- away_color
    } else {
      offense_color <- away_color
      defense_color <- home_color
    }
    p <-
      tracking %>%
      ggplot() +
      gg_field(yardmin = yardmin, yardmax = yardmax, field_color = field_color, line_color = line_color, sideline_color = sideline_color, ...) +
      aes(x = x, y = y, group = nfl_id) +
      geom_segment(
        data = tibble(),
        inherit.aes = FALSE,
        aes(x = line_of_scrimmage, y = 0, xend = line_of_scrimmage, yend = 0 + 160 / 3),
        size = 2
      ) +
      geom_segment(
        data = tibble(),
        inherit.aes = FALSE,
        aes(x = first_down_line, y = 0, xend = first_down_line, yend = 0 + 160 / 3),
        color = "#ffff7f",
        size = 2
      ) +
      geom_point(
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
      geom_text(
        aes(label = "\u25A0", color = side, angle = o),
        # size = 8,
        size = pts(24),
        show.legend = FALSE
      ) +
      geom_text(
        aes(label = "\u0332", color = side, angle = o),
        # size = 8,
        size = pts(24),
        # vjust = 0.4,
        show.legend = FALSE
      ) +
      geom_text(
        aes(label = jersey_number, angle = o),
        color = "white",
        # size = 3,
        size = pts(8),
        vjust = 1,
      ) +
      # TODO: Use team colors.
      scale_color_manual(values = c("O" = offense_color, "D" = defense_color)) +
      theme(plot.caption = element_text(hjust = 0, size = 12)) +
      labs(
        caption = glue::glue("{game$visitor_team_abbr} @ {game$home_team_abbr}, Week {game$week} (game_id = {game$game_id})
                             Q{play$quarter}: {str_wrap(paste0(play$play_description, ' ', '(play_id = ', play$play_id, '))'), 80)}
                             EPA: {round(play$epa, 2)}"),
        x = NULL, y = NULL
      )
    p
    
    if(nearest_defender) {
      p <-
        p +
        geom_segment(
          data = min_dists,
          inherit.aes = FALSE,
          aes(x = x_o, y = y_o, xend = x_d, yend = y_d),
          color = "grey50"
        )
    }
    anim <- p + gganimate::transition_manual(frame_id)
    browser()
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
    anim
  }

