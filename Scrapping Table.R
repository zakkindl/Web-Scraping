# Offense Table Scraping/Plotting -----------------------------------------

#Import libraries
library(tidyverse)
library(rvest)
library(lubridate)
library(janitor)
library(formattable)

#Specify webpage to scrape from

url = "https://www.sports-reference.com/cfb/schools/washington-state/2024/gamelog/"

#Save tables as data frame
page_table <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)

#Save first table and convert row to names
offense_table <- page_table |> 
  html_nodes("table") %>%
  .[[1]] |> 
  html_table(fill=T) |> row_to_names(1) |> 
  clean_names()

#Rename to include additional headers

#list of new names
new <- c("rank","date","home_or_away","opponent","result","pass_cmp"
         ,"pass_att","pass_pct","pass_yds","pass_td","rush_att","rush_yds","rush_avg",
         "rush_td","tot_off_plys","tot_off_yds","tot_off_avg","fd_pass","fd_rush","fd_pen",
         "fd_tot","num_pen","pen_yds","turn_fum","turn_int","turn_tot")

#list of old names
old <- c(colnames(offense_table))

#Rename, drop last row of summary stats, convert @ to home or away, extract score and W/L, convert to date, convert specified names to numeric 
offense_table <- offense_table |> 
  rename_with(~ new, all_of(old)) |> 
  filter(row_number() <= n()-1)|> #drop the last row of summary stats
  mutate(
    home_or_away = if_else(home_or_away=="@","away","home"),
    outcome = str_extract(result, "[WL()]"),
    points_scored = as.numeric(sub(".*\\((\\d+)-.*", "\\1", result)),
    points_recieved = as.numeric(sub(".*-(\\d+)\\)", "\\1", result)),
    date= ymd(date)
  ) |> 
  select(-c(result))|> 
  mutate(
    across(c("rank", "pass_cmp", "pass_att", "pass_pct", "pass_yds", "pass_td", "rush_att", "rush_yds", 
             "rush_avg", "rush_td", "tot_off_plys", "tot_off_yds", "tot_off_avg", "fd_pass", "fd_rush", 
             "fd_pen", "fd_tot", "num_pen", "pen_yds", "turn_fum", "turn_int", "turn_tot", "points_scored", 
             "points_recieved"), as.numeric))

#Create a smaller table to work with & convert names to uppercase
smalltable<-offense_table |> 
  select(date,home_or_away,opponent,pass_pct,pass_yds,
         pass_td,rush_yds,rush_td,outcome,points_scored) |> 
  rename_with(toupper)

#Specify colors to call to later
customGreen = "#71CA97"
customRed = "#ff7f7f"

#Create condition to apply to table
improvement_formatter <- formatter("span", 
                                   style = x ~ style(
                                     color = ifelse(x > 50, customGreen, ifelse(x <= 50, customRed, "black"))), 
                                   x ~ icontext(ifelse(x>50, "arrow-up", "arrow-down"), x)
)

#Create condition to apply to score
improvement_formatter_score <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold", 
              color = ifelse(x == 'W', customGreen, ifelse(x == 'L', customRed, "black"))))

#Create table and specify design changes
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


