
source('scripts/target_prob_setup.R')
# pak::pkg_install("jthomasmock/espnscrapeR")
teams <- espnscrapeR::get_nfl_teams() %>% select(team = team_short_name, logo)
teams

tpoe <- file.path('data', 'target_prob', 'small', 'tpoe_player_rankings.csv') %>% readr::read_csv()
cpoe <- file.path('data', 'catch_prob', 'dpoe_arrival_player_rankings.csv') %>% readr::read_csv()
# roster <- nflfastR::fast_scraper_roster(seasons = 2018)
# roster %>% filter(team == 'NE', position %in% c('SS', 'FS'))
db_grps <- import_nflfastr_db_groups()

labels <-
  tpoe %>% 
  filter(grp %in% c('CB', 'S')) %>% 
  distinct(nfl_id, display_name, grp, team)
labels

xpoe <-
  bind_rows(
    tpoe %>%
      select(nfl_id, xpoe = tpoe) %>% 
      mutate(prefix = 't'),
    cpoe %>% 
      select(nfl_id, xpoe = dpoe_arr) %>% 
      mutate(prefix = 'c')
  ) %>% 
  left_join(labels) %>% 
  filter(grp %in% c('CB', 'S')) %>% 
  group_by(grp, prefix) %>% 
  mutate(grp_rnk = row_number(xpoe)) %>% 
  ungroup()
xpoe

xpoe_wide <-
  xpoe %>%
  pivot_wider(
    names_from = prefix,
    values_from = c(xpoe, grp_rnk)
  ) %>% 
  group_by(grp) %>% 
  mutate(grp_rnk = row_number(grp_rnk_t + grp_rnk_c)) %>% 
  ungroup() %>% 
  arrange(grp, grp_rnk) %>% 
  left_join(teams)
xpoe_wide

defenders_top_pff <-
  tibble(
    display_name = c('Stephon Gilmore', 'Desmond King', 'Chris Harris', 'Kareem Jackson', 'Byron Jones', 'Jason McCourty', 'Kyle Fuller', 'Patrick Peterson', 'Bryce Callahan', 'Johnathan Joseph', 'Prince Amukamara', 'Denzel Ward', 'Marlon Humphrey', 'Casey Hayward', 'Pierre Desir', 'Xavien Howard', 'A.J. Bouye', 'Darius Slay', 'Trumaine Johnson', 'Marshon Lattimore', 'Steven Nelson', 'William Jackson', 'Adoree\' Jackson', 'Jalen Ramsey', 'Jaire Alexander')
  ) %>% 
  mutate(rnk_pff = row_number())
defenders_top_pff

xpoe_wide_filt <-
  xpoe_wide %>% 
  filter(grp_rnk <= 10) %>%
  select(
    display_name,
    grp,
    team,
    logo,
    grp_rnk_t,
    grp_rnk_c,
    grp_rnk
  )

xpoe_wide_pff <-
  xpoe_wide %>% 
  inner_join(defenders_top_pff) %>% 
  select(
    display_name,
    team,
    logo,
    grp_rnk_t,
    grp_rnk_c,
    grp_rnk,
    rnk_pff
  ) %>% 
  arrange(rnk_pff)

.add_logo_transform <- function(tab) {
  tab %>% 
    gt::text_transform(
      locations = gt::cells_body(
        vars(logo)
      ),
      fn = function(x) {
        gt::web_image(
          url = x,
          height = 25
        )
      }
    )
}

.add_common_tab_options <- function(tab, ...) {
  tab %>% 
    gt::tab_options(
      heading.title.font.size = gt::px(30),
      ...
    )
}

.get_valid_grps <- function() {
  c('CB', 'S')
}
.validate_grp <- function(x = .get_valid_grps(), ...) {
  match.arg(x, ...)
}

grps <- .get_valid_grps()
.do_gt_by_grp <- function(grp = .get_valid_grps()) {
  .validate_grp(grp)
  grp_lab <- switch(grp, 'S' = 'Safeties', 'CB' = 'Cornerbacks')
  res <-
    xpoe_wide_filt %>% 
    filter(grp == !!grp) %>% 
    select(-grp) %>% 
    gt::gt() %>% 
    gt::cols_label(
      .list = 
        list(
          display_name = 'Name',
          team = 'Team',
          logo = ' ',
          grp_rnk_t = 'Coverage Rank',
          grp_rnk_c = 'Contest Rank',
          grp_rnk = gt::md('**Group Rank**')
        )
    ) %>% 
    gt::tab_source_note(
      source_note = gt::md('{nflfastR} is source for position groups and teams, with the exception of Kareem Jackson.')
    ) %>% 
    gt::tab_header(
      title = gt::md(glue::glue('**Top 10 {grp_lab}**'))
    ) %>% 
    .add_logo_transform() %>% 
    .add_common_tab_options()
  gt::gtsave(res, filename = file.path('figs', glue::glue('final_{tolower(grp)}_rankings.png')))
  res
}

.get_valid_pages <- function() {
  c('1', '2')
}
.validate_page <- function(x = .get_valid_pages(), ...) {
  match.arg(x, ...)
}

pages <- .get_valid_pages()
.do_gt_pff <- function(page = c('1', '2')) {
  .validate_page(page)
  if(page == '1') {
    page_min <- 1
    page_max <- 12
    page_lab <- '1-12'
  } else {
    page_min <- 13
    page_max <- 24
    page_lab <- '13-24'
  }
  res <-
    xpoe_wide_pff %>% 
    filter(rnk_pff >= page_min & rnk_pff <= page_max) %>% 
    gt::gt() %>% 
    gt::cols_label(
      .list = 
        list(
          display_name = 'Name',
          team = 'Team',
          logo = ' ',
          grp_rnk_t = 'Coverage Rank',
          grp_rnk_c = 'Contest Rank',
          grp_rnk = gt::md('**Group Rank**'),
          rnk_pff = gt::md('**PFF Rank**')
        )
    ) %>% 
    gt::tab_header(
      title = gt::md(glue::glue('**Top {page_lab} PFF Cornerbacks**')),
    ) %>% 
    .add_logo_transform() %>% 
    .add_common_tab_options()
  gt::gtsave(res, filename = file.path('figs', glue::glue('final_rankings_pff_{page_lab}.png')))
  res
}

grps %>% walk(.do_gt_by_grp)
pages %>% walk(.do_gt_pff)
