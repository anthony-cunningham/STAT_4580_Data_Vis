# Entering and Cleaning Data #

## CITATION: https://stats.idre.ucla.edu/r/codefragments/read_multiple/ for help in automating reading in multiple files

## For Personal Labtop Use ##
text_file_names <- c("2002_Teams.txt", "2003_Teams.txt", "2004_Teams.txt", "2005_Teams.txt", "2006_Teams.txt", "2007_Teams.txt", 
                     "2008_Teams.txt", "2009_Teams.txt", "2010_Teams.txt", "2011_Teams.txt", "2012_Teams.txt", "2013_Teams.txt", 
                     "2014_Teams.txt", "2015_Teams.txt", "2016_Teams.txt", "2017_Teams.txt", "2018_Teams.txt", "2019_Teams.txt")


f <- file.path("C:/Users/Anthony/OneDrive/Documents/STAT 4580 Project/Text Files", c(text_file_names))
teams <- lapply(f, readr::read_tsv)

## For GitHub Use ##      Need to Change after each push
github_text_files <- c("2002_Teams.txt?token=AAAGvn-ibkG_X8TQVZP6KOJ5RCZtPXRrks5cuLaLwA%3D%3D", 
                       "2003_Teams.txt?token=AAAGvoTCw-_6-ZYVKkSwmS766s0VZwJ0ks5cuLbSwA%3D%3D", 
                       "2004_Teams.txt?token=AAAGvrwoKb7Zeo6l1vDkaX110cUtOk9Bks5cuLb1wA%3D%3D", 
                       "2005_Teams.txt?token=AAAGvuAqtd-cLHw0_d-dJ-9J2UqUmrcdks5cuLcXwA%3D%3D", 
                       "2006_Teams.txt?token=AAAGvlGMn5Wl7u_zjN90Ta-a_kvBgvhwks5cuLc3wA%3D%3D", 
                       "2007_Teams.txt?token=AAAGvunC7gNAuWIfgYyf6JEKTN3tvGm_ks5cuLdcwA%3D%3D", 
                       "2008_Teams.txt?token=AAAGvgDisI_zoyczWq_g0ttwQnT4P9Ygks5cuLd8wA%3D%3D", 
                       "2009_Teams.txt?token=AAAGvq3PR9S0knvgTQCHXrdd4d2BQNzEks5cuLefwA%3D%3D", 
                       "2010_Teams.txt?token=AAAGvmDLrBOoQIMyq0HQFgyVoHqQeKYOks5cuLfBwA%3D%3D", 
                       "2011_Teams.txt?token=AAAGvianOr1g0RcptGREgEeSTLkxM8x5ks5cuLfjwA%3D%3D", 
                       "2012_Teams.txt?token=AAAGvgZq0__80lRI2G1I_R-GxvsBZAf_ks5cuLgPwA%3D%3D", 
                       "2013_Teams.txt?token=AAAGvuhtZ7CgdjkLlFvVPQhZzTsYKtseks5cuLg1wA%3D%3D", 
                       "2014_Teams.txt?token=AAAGvhRQIaUpnB1BP7TB_hyWmCt2sGt6ks5cuLhXwA%3D%3D", 
                       "2015_Teams.txt?token=AAAGvihMCLurSvT2U5fxte7ZRS7sY7k9ks5cuLh3wA%3D%3D", 
                       "2016_Teams.txt?token=AAAGvuqs1yzR9Gl5iISaSLixW0JpYfbqks5cuLiawA%3D%3D", 
                       "2017_Teams.txt?token=AAAGvn4pfXpK5zJW12q-cEIqdUPR88L8ks5cuLi8wA%3D%3D", 
                       "2018_Teams.txt?token=AAAGvk0K6vE9nk3KDUBPgu4ip-49P82Zks5cuLjewA%3D%3D",
                       "2019_Teams.txt?token=AAAGvqUNpKEo56PtbDbcfPrgQELouIGvks5cuLSKwA%3D%3D")
f <- file.path("https://github.uiowa.edu/raw/STAT4580-Spring-2019/acnnngham/master/Project/Text%20Files", c(github_text_files))
teams <- lapply(f, readr::read_tsv)

## CITATION: https://www.datacamp.com/community/tutorials/tutorial-on-loops-in-r for the loop help
## CITATION: https://stackoverflow.com/questions/13590139/remove-numbers-from-alphanumeric-characters by 
## Stack Overflow user Cleonidas for the "removeNumbers" recommendation

## Getting tourney seed numbers out of "Team" variable ##
library(tm)
for(i in 1:18){
  teams[[i]]$Team <- removeNumbers(teams[[i]]$Team)
  teams[[i]]$Team <- trimws(teams[[i]]$Team)
}

## Add "Year" variable and add Year to "Team" variable ##
library(dplyr)
for(i in 1:18){
  for(j in 2002:2019){
    if(i == j - 2001){
      teams[[i]] <- mutate(teams[[i]], Year = j, Team = paste0(j, " ", Team))
      teams[[i]] <- select(teams[[i]], Rk, Year, everything())
    } else{
      next
    }
  }
}

## Combine All Years of Data into One Tibble ##
teams_agg <- teams[[1]]
for (i in 2:18){
  teams_agg <- rbind(teams_agg, teams[[i]])
}

## Check for Missing Values ##
missing <- for(i in 1:22){
  filter(teams_agg, is.na(teams_agg[i]))
}
length(missing)

## Check Overall Distribution
### Histogram of AdjEM, O and D data
library(ggplot2)

ggplot(teams_agg) + geom_histogram(aes(x = AdjEM), binwidth = 2, fill = "grey", color = "black")
summary(teams_agg$AdjEM)
# AdjEM data slightly skewed to the left, but still pretty symmetrical (1st and 3rd quartiles almost the same in absolute value).

ggplot(teams_agg) + geom_histogram(aes(x = AdjO), binwidth = 2, fill = "grey", color = "black")
summary(teams_agg$AdjO)
# AdjO data symmetrical, but mean and median both 103.5, above 100.

ggplot(teams_agg) + geom_histogram(aes(x = AdjD), binwidth = 2, fill = "grey", color = "black")
summary(teams_agg$AdjD)
# AdjD data symmetrical, once again around 103.5.

### Density Plots of AdjEM, O and D data
ggplot(teams_agg) + geom_density(aes(x = AdjEM))
ggplot(teams_agg) + geom_density(aes(x = AdjO))
ggplot(teams_agg) + geom_density(aes(x = AdjD))

### Strip Plots
ggplot(teams_agg) + geom_point(aes(x = AdjEM, y = "All"))
ggplot(teams_agg) + geom_point(aes(x = AdjO, y = "All"))
ggplot(teams_agg) + geom_point(aes(x = AdjD, y = "All"))

## Question 1: Who is the Best Team in College Basketball from 2002-2018, according to kenpom Data? ##

best10_teams_ever <- top_n(teams_agg, 10, AdjEM)
ranked_best10_ever <- mutate(best10_teams_ever, Team = reorder(Team, AdjEM))
ranked_best10_ever

### Dot Plot
best_dot <- ggplot(ranked_best10_ever) + geom_point(aes(x = Team, y = AdjEM)) + coord_flip()

### Bar Chart
## CITATION: https://www.colorcodehex.com/html-color-picker.html for colors
top10_colors <- c("#072D75", "#D42029", "#0307EB", "#C2062F", "#ED0C32", "#113F9C", "#0307EB", "#DE6B07", "#3D92ED", "#0616C4")
best_bar <- ggplot(ranked_best10_ever) + geom_bar(aes(x = Team, y = AdjEM), fill = top10_colors, stat = "identity") + coord_flip() + 
  theme_void() + 
  geom_text(aes(x = Team, y = 3, label = Team), hjust = "left", color = "white") +  
  geom_text(aes(x = Team, y = AdjEM - 3, label = AdjEM), hjust = "right", color = "white")

### Put on same page
## CITATION: http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/#arrange-on-one-page 
## for both of these function
figure <- cowplot::plot_grid(best_dot, best_bar, ncol = 2, nrow = 1)
ggpubr::annotate_figure(figure, top = ggpubr::text_grob("Top 10 Kenpom Teams, By Adjusted Efficiency Margin", hjust = 0.4, 
                                                        face = "bold", size = 14))
## Question 2: What has been the most difficult conference to play in, according to Kenpom?
### Group and Rank Conferences by AdjEM
conference <- summarize(group_by(teams_agg, Conf), avg_conf_AdjEM = mean(AdjEM))
ranked_conf <- mutate(conference, Conf = reorder(Conf, avg_conf_AdjEM))

### Dot Plot
ggplot(ranked_conf) + geom_point(aes(x = Conf, y = avg_conf_AdjEM)) + coord_flip() + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(title = "Kenpom's Best Conferences, by Average Adjusted Efficiency Margin", y = "Conference-Averaged AdjEM", 
       x = "Conference Abbreviation") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5))

### Rank Top 4
top4_conf <- top_n(conference, 4, avg_conf_AdjEM)
arrange(top4_conf, desc(avg_conf_AdjEM))

## Question 3: How has Competitive Balance Changed?
### Convert 'W-L' to Separate Variables for Wins and Losses
test_set <- head(teams_agg)
foo <- strsplit(test_set$`W-L`, "[-]")
wins_test <- NULL
loss_test <- NULL
for(i in 1:length(foo)){
  wins_test[i] <- foo[[i]][1]
  loss_test[i] <- foo[[i]][2]
}

## Using Full Dataset ##
teams_cb <- teams_agg
foo <- strsplit(teams_agg$`W-L`, "[-]")
wins <- NULL
losses <- NULL
for(i in 1:length(foo)){
  wins[i] <- foo[[i]][1]
  losses[i] <- foo[[i]][2]
}


### Add Wins, Losses, and Total Games Played as New Variables
## CITATION: http://teamsportsanalysis.blogspot.com/2012/04/how-to-calculate-noll-scully.html for calculating Noll-Scully Comp-Balance Measure
wins_test <- as.numeric(wins_test)
loss_test <- as.numeric(loss_test)
test_set <- mutate(test_set, Wins = wins_test, Losses = loss_test, Games_Played = wins_test + loss_test, Win_Perc = Wins/Games_Played)
test_set <- select(test_set, Rk, Year, Team, Conf, Win_Perc, Wins, Losses, Games_Played, everything())
sd <- sd(test_set$Win_Perc)
ideal_sd <- mean(test_set$Win_Perc)/sqrt(mean(test_set$Total_Games))
noll_scully <- sd/ideal_sd

## Using Full Dataset ##
wins <- as.numeric(wins)
losses <- as.numeric(losses)
teams_cb <- mutate(teams_cb, Wins = wins, Losses = losses, Games_Played = Wins + Losses, Win_Perc = Wins/Games_Played)
teams_cb <- select(teams_cb, Rk, Year, Team, Conf, Win_Perc, Wins, Losses, Games_Played, everything())

## Group By Year ##
cb_year <- summarize(group_by(teams_cb, Year), sd = sd(Win_Perc), ideal_sd = mean(Win_Perc)/sqrt(mean(Games_Played)), 
                     noll_scully = sd/ideal_sd)

## Create a Time Series ##
ggplot(cb_year, aes(y = noll_scully, x = Year)) + geom_line() + geom_point()
