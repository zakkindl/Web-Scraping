#Import libraries
library(tidyverse)
library(rvest)
library(lubridate)

#Specify webpage to scrape from

link = "https://www.lanetitans.com/sports/wsoc/2024-25/schedule"

webpage <- read_html(link)

#specify what components to pull from webpage
scores = webpage |> 
  html_nodes(".result") |> 
  html_text()

team = webpage |> 
  html_nodes(".team-name") |> 
  html_text()

# record = webpage |> 
#   html_nodes(".cat") |> 
#   html_text()
# 
# seasonrecord =data.frame(record,stringsAsFactors = FALSE)

#Create a data frame of components
seasonscores = data.frame(team,scores, stringsAsFactors = FALSE)
  
