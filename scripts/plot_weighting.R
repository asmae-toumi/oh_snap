
source('scripts/target_prob_setup.R')


probs_dists <- do_combine_target_probs_and_dists()
probs_dists_filt <- probs_dists %>% slice_max(frame_id)
frame_id <- probs_dists_filt %>% distinct(frame_id) %>% pull(frame_id)
assertthat::assert_that(length(frame_id) == 1L)

game_id = 2018090600
play_id = 75
# frame_id = 41
plays = import_plays()
games = import_games()
positions = import_positions()
team_colors = TRUE
yardmin = NULL
yardmax = NULL
# field_color = '#b3e3d6'
field_color = 'white'
line_color = 'black'
sideline_color = 'white'
endzone_color = NULL
buffer = NULL
colors = import_colors()
cnd = 'all'
which = NA_character_
target_prob = TRUE
personnel_and_rushers = import_personnel_and_rushers()
target_probs = import_target_probs(cnd = cnd, which = which)
clip = TRUE
at = 'end_routes'
init_cnd = quos(frame_id == 1L)
save = TRUE
dir = file.path('figs', 'target_prob')
ext = 'gif'
filename = sprintf('%s-%s.%s', game_id, play_id, ext)
path = file.path(dir, filename)
width = 10
height = 10

# game_id = 2018090905; play_id = 783
meta <- tibble(game_id = game_id, play_id = play_id, frame_id = frame_id)

game <- games %>% filter(game_id == !!game_id)
assertthat::assert_that(nrow(game) == 1L)
week <- game$week

tracking <- import_tracking(week = week, positions = positions, standardize = FALSE)

tracking <-
  tracking %>%
  inner_join(meta %>% rename(frame_id_clip = frame_id), by = c('game_id', 'play_id')) %>% 
  filter(frame_id <= frame_id_clip) %>% 
  select(-frame_id_clip)

play <- plays %>% inner_join(meta %>% select(game_id, play_id), by = c('game_id', 'play_id'))
assertthat::assert_that(nrow(play) == 1L)

game <- games %>% filter(game_id == !!game_id)
assertthat::assert_that(nrow(game) == 1L)

target_id <- play$target_nfl_id
has_target <- !is.na(target_id)

if(!has_target) {
  .display_warning('No target receiver.')
  # return(ggplot())
  target <- tibble(display_name = '?', jersey_number = -1)
  has_target <- FALSE
} else {
  target <-
    tracking %>%
    filter(nfl_id == !!target_id) %>%
    distinct(display_name, jersey_number)
  assertthat::assert_that(nrow(target) == 1L)
}

if(target_prob) {
  personnel_and_rushers_min <-
    personnel_and_rushers %>%
    inner_join(meta %>% select(game_id, play_id), by = c('game_id', 'play_id')) %>% 
    filter(n_rusher > 0L) %>%
    select(game_id, play_id, n_rusher, rushers)
  
  if(nrow(personnel_and_rushers_min) > 0L) {
    personnel_and_rushers_min <-
      personnel_and_rushers_min %>% 
      mutate(rushers = purrr::map(rushers, ~select(.x, nfl_id, idx_closest_to_ball)))
  }
}

tracking_clipped <-
  tracking %>%
  # clip_tracking_at_events(at = 'throw', init_cnd = quos(frame_id == 1L)) %>%
  arrange(game_id, play_id, nfl_id, frame_id)


if(target_prob) {
  probs <-
    target_probs %>% 
    semi_join(meta, by = c('game_id', 'play_id', 'frame_id'))
  
  if(nrow(probs) >= 0) {
    probs <-
      probs %>% 
      distinct(game_id, play_id, frame_id, nfl_id, prob = .prob_1_norm)
  } else {
    target_prob <- FALSE
  }
}

line_of_scrimmage <- play$absolute_yardline_number
play_direction <- tracking$play_direction[[1]]
sign <- ifelse(play_direction == 'left', -1, 1)
first_down_line <- line_of_scrimmage + sign * play$yards_to_go

target_tracking <-
  tracking %>%
  filter(nfl_id == !!target_id)

nontarget_tracking <-
  tracking %>%
  filter(nfl_id != !!target_id)

target_tracking_clipped <-
  tracking_clipped %>%
  filter(nfl_id == !!target_id)

nontarget_tracking_clipped <-
  tracking_clipped %>%
  filter(nfl_id != !!target_id)

ball <-
  tracking %>%
  distinct(game_id, play_id, frame_id, x = ball_x, y = ball_y) %>%
  mutate(nfl_id = NA_integer_) %>% 
  slice_max(frame_id)

if(is.null(yardmin) | is.null(yardmax)) {
  yardminmax <-
    tracking %>%
    summarize(across(x, list(min = min, max = max)))
  
  if(is.null(yardmin)) {
    yardmin <- yardminmax$x_min
    yardmin <- .round_any(yardmin, 10, floor)
  }
  
  if(is.null(yardmax)) {
    yardmax <- yardminmax$x_max
    yardmax <- .round_any(yardmax, 10, ceiling)
  }
}

max_y <- 160 / 3
if(is.null(buffer)) {
  yminmax <-
    tracking %>%
    summarize(across(y, list(min = min, max = max)))
  ymin <- yminmax$y_min
  ymin <- .round_any(ymin, 1, floor)
  ymax <- yminmax$y_max
  ymax <- .round_any(ymax, 1, ceiling)
  ymin_bound <- 0 - ymin
  ymax_bound <- ymax - max_y
  buffer <- max(0, ymin_bound, ymax_bound)
}

home_team <- game$home_team_abbr
away_team <- game$visitor_team_abbr

home_team <- game$home_team_abbr
away_team <- game$visitor_team_abbr

if(team_colors) {
  
  home_color <- colors %>% filter(team == !!home_team) %>% pull(color)
  away_color <- colors %>% filter(team == !!away_team) %>% pull(color2)
  if(play$possession_team == home_team) {
    offense_color <- home_color
    defense_color <- away_color
  } else {
    offense_color <- away_color
    defense_color <- home_color
  }
} else {
  offense_color <- 'red'
  defense_color <- 'blue'
  if(play$possession_team == home_team) {
    home_color <- offense_color
    away_color <- defense_color
  } else {
    home_color <- defense_color
    away_color <- offense_color
  }
}

nontarget_tracking_between <-
  nontarget_tracking %>% 
  anti_join(
    nontarget_tracking_clipped %>% 
      select(game_id, play_id, nfl_id, frame_id),
    by = c('nfl_id', 'frame_id', 'game_id', 'play_id')
  )

p <-
  ggplot() +
  gg_field(
    yardmin = yardmin,
    yardmax = yardmax,
    buffer = buffer,
    field_color = field_color,
    line_color = line_color,
    sideline_color = sideline_color # ,
    # ...
  ) +
  aes(x = x, y = y) +
  geom_segment(
    data = tibble(x = !!line_of_scrimmage),
    inherit.aes = FALSE,
    aes(x = x, y = 0, xend = x, yend = !!max_y),
    size = 1.25
  ) +
  geom_segment(
    data = tibble(x = !!first_down_line),
    inherit.aes = FALSE,
    aes(x = x, y = 0, xend = x, yend = !!max_y),
    color = '#ffff7f',
    size = 2
  ) +
  geom_point(
    data = ball,
    inherit.aes = TRUE,
    size = 3,
    color = 'brown'
  ) +
  geom_path(
    data = nontarget_tracking_clipped %>% select(-frame_id),
    aes(color = side, group = nfl_id),
    size = 1,
    alpha = 0.3,
    show.legend = FALSE
  ) +
  geom_path(
    data = nontarget_tracking_between %>% select(-frame_id),
    aes(color = side, group = nfl_id),
    size = 1,
    alpha = 0.3,
    linetype = 2,
    show.legend = FALSE
  ) +
  geom_text(
    data = nontarget_tracking %>% slice_max(frame_id),
    aes(label = jersey_number, color = side),
    size = pts(14), 
    show.legend = FALSE,
    fontface = 'bold'
  )
p

if(has_target) {
  
  target_tracking_between <-
    target_tracking %>% 
    anti_join(
      target_tracking_clipped %>% 
        select(game_id, play_id, nfl_id, frame_id),
      by = c('nfl_id', 'frame_id', 'game_id', 'play_id')
    )
  
  p <-
    p +
    geom_path(
      data = target_tracking_clipped %>% select(-frame_id),
      aes(color = side),
      size = 2,
      alpha = 0.3,
      show.legend = FALSE
    ) +
    geom_path(
      data = target_tracking_between %>% select(-frame_id),
      aes(color = side),
      size = 2,
      alpha = 0.3,
      linetype = 2,
      show.legend = FALSE
    ) +
    geom_text(
      data = target_tracking %>% slice_max(frame_id),
      aes(label = jersey_number, color = side),
      size = pts(14),
      show.legend = FALSE,
      fontface = 'bold'
    )
}
p

p <-
  p +
  scale_color_manual(values = c('O' = offense_color, 'D' = defense_color))

tracking_d_clipped <-
  nontarget_tracking_clipped %>% 
  filter(side == 'D')
tracking_d_clipped

tracking_d_clipped_annotate <-
  tracking_d_clipped %>% 
  slice_max(frame_id) %>% 
  select(frame_id, nfl_id, display_name, x, y) %>% 
  left_join(
    target_tracking_clipped %>% 
      select(frame_id, x_target = x, y_target = y)
  ) %>% 
  mutate(
    r = .dist(x, x_target, y, y_target)
  ) %>% 
  filter(r < 20) %>% 
  mutate(idx = row_number(r)) %>% 
  arrange(r)
tracking_d_clipped_annotate %>% 
  ggplot() +
  aes(x = 1, y = max(idx) - idx + 1) +
  geom_text(
    aes(label = glue::glue('d_{idx}j \text{and} {scales::number(r, accuracy = 1)}')),
    parse = TRUE
  ) +
  theme_void()
ggplot() +
  annotate(
    geom = 'text',
    x = 1,
    y = 5,
    label = glue::glue('d_{tracking_d_clipped_annotate[1, ]$idx}j = {scales::number(tracking_d_clipped_annotate[1, ]$r, accuracy = 1)}'),
    parse = TRUE
  ) +
  theme_void()

p +
  ggforce::geom_circle(
    inherit.aes = FALSE,
    data = tracking_d_clipped_annotate %>% slice(1),
    aes(x0 = x, y0 = y, r = r, fill = r),
    alpha = 0.2,
    show.legend = FALSE
  )


p +
  ggforce::geom_circle(
    inherit.aes = FALSE,
    data = tracking_d_clipped_annotate,
    aes(x0 = x, y0 = y, r = r, fill = r),
    alpha = 0.2,
    show.legend = FALSE
  )

# Lets make some data
circles <- data.frame(
  x0 = rep(1:3, 3),
  y0 = rep(1:3, each = 3),
  r = seq(0.1, 1, length.out = 9)
)
circles

# Behold the some circles
ggplot() +
  ggforce::geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = r), data = circles)

# Theme stuff
title <- ifelse(is.null(title), glue::glue("<b><span style='color:{away_color};'>{away_team}</span></b> @ <b><span style='color:{home_color};'>{home_team}</span></b>, Week {week}"), title)
caption <- ifelse(is.null(caption), glue::glue('Q{play$quarter}: {play$play_description}'), caption)
p <-
  p +
  theme(
    plot.title = ggtext::element_markdown(),
    plot.title.position = 'plot',
    strip.background = element_rect(fill = NA),
    # strip.text = element_text()
    plot.caption = ggtext::element_markdown(
      size = 12,
      hjust = 0,
      lineheight = 0
    ),
    plot.caption.position = 'plot'
  ) +
  labs(
    subtitle = subtitle,
    title = title,
    caption = caption,
    x = NULL, y = NULL
  )

anim <- p + gganimate::transition_manual(frame_id)

seconds <- ball %>% nrow() %>% {. / 10}
nframe <- (seconds + end_pause) * fps

if(!save) {
  return(anim)
}

if(!dir.exists(dirname(path))) {
  dir.create(dirname(path))
}

res <- 
  save_animation(
    anim,
    nframe = nframe,
    fps = fps,
    height = height,
    width = width,
    path = path,
    renderer = gganimate::gifski_renderer(file = path),
    end_pause = end_pause * fps
  )
