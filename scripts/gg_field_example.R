
source("gg_field.R")

## reminders: 
### x-axis goes from 0 (back of left endzone) to 120 (back of right end zone)
### left goal line (x=10), 50-yard line (x=60), right goal line (x=110)
### y-axis goes from 0 to 53.33

## example of full field
field_full <- gg_field()
field_full

## example, clipped at 50 yardline
field_clipped_at_50 <- gg_field(yardmin=60)
field_clipped_at_50


## example, no sideline buffer (not recommended since players often go out-of-bounds)
field_no_buffer <- gg_field(buffer=0)
field_no_buffer


## example frame taken from a play in the dataset
ex_frame <- readr::read_csv(file='../data/gg_field_example_data.csv', col_types = cols()) %>% 
            janitor::clean_names()

ex_frame <- ex_frame %>% mutate(team_colors = case_when(
              team == "football" ~ "brown",
              team == "away" ~ "blue",
              team == "home" ~ "gold"
            ))

## add players to full field
field_full + 
  geom_point(data=ex_frame, aes(x=x, y=y, col=team_colors), cex=3) + 
  scale_color_identity()

## add players to half field (50 yl to end zone)
field_clipped_at_50 + 
  geom_point(data=ex_frame, aes(x=x, y=y, col=team_colors), cex=3) + 
  scale_color_identity()
