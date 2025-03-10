#Import libraries
library(tidyverse)
library(rvest)
library(lubridate)
library(janitor)
#library(cfbplotR)
library(formattable)
library(sparkline)

# Offense Table Scraping/Plotting -----------------------------------------


#Specify webpage to scrape from

url = "https://www.sports-reference.com/cfb/schools/washington-state/2024/gamelog/"

page_table <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)

offense_table <- page_table |> 
  html_nodes("table") %>%
  .[[1]] |> 
  html_table(fill=T) |> row_to_names(1) |> 
  clean_names()

#rename to include pass/rush etc.
new <- c("rank","date","home_or_away","opponent","result","pass_cmp"
         ,"pass_att","pass_pct","pass_yds","pass_td","rush_att","rush_yds","rush_avg",
         "rush_td","tot_off_plys","tot_off_yds","tot_off_avg","fd_pass","fd_rush","fd_pen",
         "fd_tot","num_pen","pen_yds","turn_fum","turn_int","turn_tot")

old <- c(colnames(offense_table))

offense_table <- offense_table |> 
  rename_with(~ new, all_of(old)) |> 
  filter(row_number() <= n()-1)|> #drop the last row of summary stats
  mutate(
    home_or_away = if_else(home_or_away=="@","away","home"),
    outcome = str_extract(result, "[WL()]"),
    points_scored = as.numeric(sub(".*\\((\\d+)-.*", "\\1", result)),
    points_recieved = as.numeric(sub(".*-(\\d+)\\)", "\\1", result)),
    date= ymd(date)
    # ratio_outcome = if_else(outcome=="W", 1,0)
  ) |> 
  select(-c(result))|> 
  mutate(
    across(c("rank", "pass_cmp", "pass_att", "pass_pct", "pass_yds", "pass_td", "rush_att", "rush_yds", 
                  "rush_avg", "rush_td", "tot_off_plys", "tot_off_yds", "tot_off_avg", "fd_pass", "fd_rush", 
                  "fd_pen", "fd_tot", "num_pen", "pen_yds", "turn_fum", "turn_int", "turn_tot", "points_scored", 
                  "points_recieved"), as.numeric))

# offense_table |> 
#   ggplot(aes(x = rush_att, y = reorder(opponent, rush_att))) +
#   geom_col(width = 0.3) + 
#   geom_cfb_logos(aes(team = opponent), width = 0.075) +
#   xlab("Rush Attempts")+ylab("Opponents")+
#   theme(axis.text.y = element_blank(),axis.ticks.y = element_blank(),)
# 
# 
# offense_table |> 
#   ggplot(aes(rush_att,rush_yds))+geom_cfb_logos(aes(team = opponent), width = 0.075)

smalltable<-offense_table |> 
  select(date,home_or_away,opponent,pass_pct,pass_yds,
         pass_td,rush_yds,rush_td,outcome,points_scored) |> 
  rename_with(toupper)

customGreen = "#71CA97"
customRed = "#ff7f7f"

improvement_formatter <- formatter("span", 
                                   style = x ~ style(
                                                     color = ifelse(x > 50, customGreen, ifelse(x <= 50, customRed, "black"))), 
                                   x ~ icontext(ifelse(x>50, "arrow-up", "arrow-down"), x)
)

improvement_formatter_score <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold", 
              color = ifelse(x == 'W', customGreen, ifelse(x == 'L', customRed, "black"))))

smalltable |> 
  formattable(
    align = c('c','c','l','c','r','c','r','c','c','c'),
    list(
      OPPONENT = formatter(
        "span", style = ~ style(color = "grey", font.weight = "bold")
      ),
      POINTS_SCORED = color_tile('grey','grey'),
      PASS_PCT = improvement_formatter,
      OUTCOME = improvement_formatter_score,
      PASS_YDS = color_bar('orange'),
      RUSH_YDS = color_bar('orange')
    )
  )

# Player Table Scraping --------------------------------------------------



url2 = "https://www.sports-reference.com/cfb/years/2024-scoring.html"

page_table2 <- read_html(url2, as.data.frame=T, stringsAsFactors = TRUE)

player_table2 <- page_table2 |> 
  html_nodes("table") %>%
  .[[1]] |> 
  html_table(fill=T) |> row_to_names(1) |> 
  clean_names()|> 
  filter(!conf=="Conf") |> 
  mutate(across(c(1,5:21), ~ as.numeric(.x)))

player_table2 |> 
  filter(td>10) |> 
  arrange(rk) |> 
  ggplot(aes(td,player))+geom_col()


# player_table2 |> 
#   filter(pts>100) |> 
#   ggplot(aes(pts,school))+geom_col(width = .25)+geom_cfb_logos(aes(team = school), width = 0.075)
# 
# player_table2 |> 
#   filter(rec > 9) |> 
#   ggplot(aes(reorder(player, rec), rec)) +  # Reorder player by rec in descending order
#   geom_col(width = 0.4, alpha = 0.6) +  # Increase column width for more space
#   geom_cfb_logos(aes(team = school), width = 0.09) +  # Increase logo width for better clarity
#   theme_minimal() +
#   theme(
#     panel.grid.major = element_blank(),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     text = element_text(family = "Arial"),  # Set font to Arial
#     plot.title = element_text(face = "bold", size = 16),  # Increase font size for title
#     axis.title = element_text(face = "bold", size = 14),  # Increase font size for axis titles
#     axis.text = element_text(size = 12),
#   ) + 
#   ggtitle("Top Player Receiving Touchdowns | 2024 Season") +
#   coord_flip()  # Flip coordinates to make the chart horizontal
# 



