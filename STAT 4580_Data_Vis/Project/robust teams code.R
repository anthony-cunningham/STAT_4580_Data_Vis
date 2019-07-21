library(rvest)

## Read in 2019 Table from KenPom website

kenpom_url <- "https://kenpom.com/index.php"
kenpom_page <- read_html(kenpom_url)
kenpom_tbl <- html_table(kenpom_page)[[1]]

## Add 2019 Table as 18th item in a list
teams <- NULL
teams[[18]] <- kenpom_tbl

## Add 2002-2018 to the list
years <- as.character(c(2002:2018))
kenpom_url <- "https://kenpom.com/index.php?y="
for(i in 1:length(years)){
  kenpom_page <- read_html(paste0(kenpom_url, years[i]))
  kenpom_tbl <- html_table(kenpom_page)[[1]]
  teams[[i]] <- kenpom_tbl
}

## Clean List

# Delete Wrong Observations
library(dplyr)

for(i in 1:length(teams)){
  # Rename Duplicate Column Variables
  colnames(teams[[i]]) <- teams[[i]][1,]
  colnames(teams[[i]])[7] <- "AdjO_Rk"
  colnames(teams[[i]])[9] <- "AdjD_Rk"
  colnames(teams[[i]])[11] <- "AdjT_Rk"
  colnames(teams[[i]])[13] <- "Luck_Rk"
  colnames(teams[[i]])[14] <- "Opp_AdjEM"
  colnames(teams[[i]])[15] <- "Opp_Rk"
  colnames(teams[[i]])[17] <- "OppO_Rk"
  colnames(teams[[i]])[19] <- "OppD_Rk"
  colnames(teams[[i]])[20] <- "NCOpp_AdjEM"
  colnames(teams[[i]])[21] <- "NCOpp_Rk"
  # Get Rid of Non-Observations
  teams[[i]] <- filter(teams[[i]], Opp_AdjEM != "Strength of Schedule")
  teams[[i]] <- filter(teams[[i]], Rk != "Rk")
}

# Remove Seed Numbers from Team Names
library(tm)
for(i in 1:length(teams)){
  teams[[i]]$Team <- sub("[[:digit:]]", "", teams[[i]]$Team)
  teams[[i]]$Team <- trimws(teams[[i]]$Team)
} 

# Add "Year" variable and add Year to "Team" variable ##
for(i in 1:length(teams)){
  for(j in 2002:2019){
    if(i == j - 2001){
      teams[[i]] <- mutate(teams[[i]], Year = j, Team = paste0(j, " ", Team))
      teams[[i]] <- select(teams[[i]], Rk, Year, everything())
    } else{
      next
    }
  }
}

# Coerce Appropriate Variables to Type "Numeric"
for(i in 1:length(teams)){
  for(j in 6:22){
    teams[[i]][j] <- as.numeric(teams[[i]][,j])
  }
}

## Combine All Years of Data into One Tibble ##
teams_agg <- teams[[1]]
for (i in 2:length(teams)){
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
ideal_sd <- mean(test_set$Win_Perc)/sqrt(mean(test_set$Games_Played))
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

library(forecast)
fit <- ets(cb_year$noll_scully)
fc <- forecast(cb_year$noll_scully, model=fit)
plot(fc)
