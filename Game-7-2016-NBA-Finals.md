Game 7 2016 NBA Finals
================

# Introduction

The [2016 NBA
Finals](https://www.espn.com.au/nba/playoffs/2016/story/_/id/16362353/nba-finals-2016-lebron-james-cleveland-cavaliers-bask-nba-title-glory)
was arguably one of the most iconic NBA Finals series’ in the history of
the NBA, with LeBron James, Kyrie Irving and the rest of the Cleveland
Cavaliers squad pulling off a very unlikely upset against Stephen
Curry’s Golden State Warriors.

After leaving the Cleveland Cavaliers in 2010 to join the Miami Heat -
where he won 2 championships - James made a promise to come back to his
hometown team and win them a championship. After falling short 4-2 in
2015, *the Cavaliers looked to avenge their loss against a Warriors team
who held the best regular season record in NBA history (73-9)*.

Led by Stephen Curry, Klay Thompson and Draymond Green, the Warriors led
the 2016 NBA Finals 3-1 - 1 win away from reaching a consecutive
championship. *No team NBA history has ever come down from a 3-1 deficit
to win the NBA Finals*. Earlier in the playoffs, the Warriors were the
[10th team in NBA history to overcome a 3-1 deficit against the Oklahoma
City
Thunder](https://www.sportingnews.com/ca/nba/news/nba-playoffs-teams-to-comeback-from-3-1-series-deficit-in-postseason-history-cavaliers-warriors-celtics-suns-rockets/1dn5yjwd5hke812b67ocn8bit3).

After a back-and-forth game, the Cleveland Cavaliers made history and
went on to win Game 7, breaking the 52-year drought for any Cleveland
major sports team to attain a championship.

Using the [box score from
ESPN](https://www.espn.com.au/nba/boxscore?gameId=400878160), I am
looking to create meaningful visualisations regarding players individual
performances to summarise this game. This will be done through Tableau,
after obtaining and cleaning the data in R.

# Methodology: Web Scraping

**Web scraping** is the process of extracting data from a website,
specifically the underlying HTML code used to create the website. This
is important since not all times data will be available in a convenient
format like a .csv or .xlsx file, and instead will be posted on the
internet for people to view. This provides access to a substantially
larger range of data, allowing us to perform more meaningful analyses
and visualisations.

# Data

### Preparation

``` r
#Loading packages
library(tidyverse)
library(rvest)
library(knitr)
library(writexl)

#Creating function to scrape data from any website
scraper <- function(url, selector, output, all_nodes = TRUE) {
    #Loading the web page content
    content <- read_html(url)

    #Getting one or all nodes
    if(all_nodes == TRUE){
        data <- content %>%
            html_nodes(selector)
        }
    else{
        data <- content %>%
            html_node(selector)
        }
    #Outputting text, table, attributes, or attribute

    if(output == "text"){
        data <- data %>%
            html_text()
    } else if(output == "table"){
        data <- data %>%
            html_table()
    } else if(output == "attrs"){
        data <- data %>%
            html_attrs()
    } else{
        data <- data %>%
            html_attr(output)
        }
        #Final output
    data
}

#Creating function to convert columns into desired numeric format
convert_to_numeric <- function(column){
  if(any(str_detect(column, "\\d-"))){
    column <- str_replace(column, "-", "/")
    column <- sapply(column, function(x) eval(parse(text = x)))
  } else if(any(str_detect(column, "[[+-]]"))){
    column <- sapply(column, function(x) eval(parse(text = x)))
  } else if(any(str_detect(column, "%"))){
    column <- str_replace(column, "%", "/100")
    column <- sapply(column, function(x) eval(parse(text = x)))
  } else {
    column <- sapply(column, as.numeric)
  }
}
```

### Scraping the data

``` r
#Scraping the data using the function we created
box_score <- scraper(url = "https://www.espn.com.au/nba/boxscore?gameId=400878160",
                     selector = "#gamepackage-boxscore-module .desktop",
                     output = "table")
```

### Cleaning the Cavaliers data

``` r
#Cleveland

#Extracting box score for Cleveland
cleveland <- tibble(box_score[1]) %>%
  #Important to unnest since we had a list
  unnest() %>%
  #Changing column of players to be labelled to Player
  rename(Player = Starters) %>%
  #Removing unnecessary column
  filter(Player != "Bench") %>%
  #Since position with the name of the player, need to extract it
  mutate(Position = str_match(Player, "[[A-Z]]+$"),
         #When table was scraped, the name was also duplicated, so needed to remove duplcate
         Player = str_sub(str_match(Player, "\\w\\.\\s[[A-Z]][[A-Za-z]]+"), 1, -2))

#Separating the players into starters, bench, DNP as well as a table for overall team stats, and a table for all players that played after cleaning

#Starters
cleveland_starters <- cleveland[1:5, ]
#Converting columns into desired numeric format
cleveland_starters[2:15] <- map(cleveland_starters[2:15], convert_to_numeric) %>%
  as.tibble() 
#Replacing any NaN values with 0
cleveland_starters[is.na(cleveland_starters)] = 0
#Adding a column so we can indicate which team player is from
cleveland_starters <- cleveland_starters %>%
  mutate(Team = "Cleveland")
  
#Bench
cleveland_bench <- cleveland[6:8, ] 
#Converting columns into desired numeric format
cleveland_bench[2:15] <- map(cleveland_bench[2:15], convert_to_numeric) %>%
  as.tibble() 
#Replacing any NaN values with 0
cleveland_bench[is.na(cleveland_bench)] = 0
#Adding a column so we can indicate which team player is from
cleveland_bench <- cleveland_bench %>%
  mutate(Team = "Cleveland")

#DNP
cleveland_dnp <- cleveland[9:13, ] %>%
  #Adding a column so we can indicate which team player is from
  mutate(Team = "Cleveland")

#All players that played
cleveland_all_players_played <- bind_rows(cleveland_starters, cleveland_bench) %>%
  #Adding a column so we can indicate which team player is from
  mutate(Team = "Cleveland")

#Team
cleveland_team <- cleveland[14:15, ] %>%
  #Changing Player column to contain team name
  rename(Team = Player) %>%
  #Since our percentages are in the second row, had to move them first into first row so everything is in one row
  mutate(`FG PCT` = FG[2],
         `3PT PCT` = `3PT`[2],
         `FT PCT` = FT[2],
         #Separating FG, 3PT and FT Column to have separate columns for makes and attempts
         `FG MADE` = str_match(FG, "\\d+"),
         `FG ATT` = str_match(FG, "\\d+$"),
         `3PT MADE` = str_match(`3PT`, "\\d+"),
         `3PT ATT` = str_match(`3PT`, "\\d+$"),
         `FT MADE` = str_match(FT, "\\d+"),
         `FT ATT` = str_match(FT, "\\d+$"),
         #Adding team name
         Team = "Cleveland") %>%
  #Selecting needed columns and ordering them in a logical format
  select(Team, `FG MADE`, `FG ATT`, `FG PCT`, `3PT MADE`, `3PT ATT`, `3PT PCT`, `FT MADE`, `FT ATT`, `FT PCT`, `OREB`, `DREB`, `REB`, `AST`, `STL`, `BLK`, `TO`, `PF`, `PTS`) %>%
  #Dropping second row which does not contain any useful information after cleaning since everything is now on one row
  drop_na()

#Converting columns into desired numeric format
cleveland_team[2:19] <- map(cleveland_team[2:19], convert_to_numeric)
```

### Cleaning the Warriors data

``` r
#Golden State Warriors

#Extracting box score for Golden State Warriors
gsw <- tibble(box_score[2]) %>%
  #Important to unnest since we had a list
  unnest() %>%
  #Changing column of players to be labelled to Player
  rename(Player = Starters) %>%
  #Removing unnecessary column
  filter(Player != "Bench") %>%
  #Since position with the name of the player, need to extract it
  mutate(Position = str_match(Player, "[[A-Z]]+$"),
         #When table was scraped, the name was also duplicated, so needed to remove duplcate
         Player = str_sub(str_match(Player, "\\w\\.\\s[[A-Z]][[A-Za-z]]+"), 1, -2))
 
#Separating the players into starters, bench, DNP as well as a table for overall team stats, and a table for all players that played after cleaning

#Starters
gsw_starters <- gsw[1:5, ]
#Converting columns into desired numeric format
gsw_starters[2:15] <- map(gsw_starters[2:15], convert_to_numeric) %>%
  as.tibble() 
#Replacing any NaN values with 0
gsw_starters[is.na(gsw_starters)] = 0
#Adding a column so we can indicate which team player is from
gsw_starters <- gsw_starters %>%
  mutate(Team = "GSW")

#Bench
gsw_bench <- gsw[6:10, ]
#Converting columns into desired numeric format
gsw_bench[2:15] <- map(gsw_bench[2:15], convert_to_numeric) %>%
  as.tibble() 
#Replacing any NaN values with 0
gsw_bench[is.na(gsw_bench)] = 0
#Adding a column so we can indicate which team player is from
gsw_bench <- gsw_bench %>%
  mutate(Team = "GSW")

#DNP
gsw_dnp <- gsw[11:13, ] %>%
  #Adding a column so we can indicate which team player is from
  mutate(Team = "GSW")

#All players that played
gsw_all_players_played <- bind_rows(gsw_starters, gsw_bench) %>%
  #Adding a column so we can indicate which team player is from
  mutate(Team = "GSW")

#Team
gsw_team <- gsw[14:15, ] %>%
  #Changing Player column to contain team name
  rename(Team = Player) %>%
  #Since our percentages are in the second row, had to move them first into first row so everything is in one row
  mutate(`FG PCT` = FG[2],
         `3PT PCT` = `3PT`[2],
         `FT PCT` = FT[2],
         #Separating FG, 3PT and FT Column to have separate columns for makes and attempts
         `FG MADE` = str_match(FG, "\\d+"),
         `FG ATT` = str_match(FG, "\\d+$"),
         `3PT MADE` = str_match(`3PT`, "\\d+"),
         `3PT ATT` = str_match(`3PT`, "\\d+$"),
         `FT MADE` = str_match(FT, "\\d+"),
         `FT ATT` = str_match(FT, "\\d+$"),
         #Adding team name
         Team = "GSW") %>%
  #Selecting needed columns and ordering them in a logical format
  select(Team, `FG MADE`, `FG ATT`, `FG PCT`, `3PT MADE`, `3PT ATT`, `3PT PCT`, `FT MADE`, `FT ATT`, `FT PCT`, `OREB`, `DREB`, `REB`, `AST`, `STL`, `BLK`, `TO`, `PF`, `PTS`) %>%
  #Dropping second row which does not contain any useful information after cleaning since everything is now on one row
  drop_na()

#Converting columns into desired numeric format
gsw_team[2:19] <- map(gsw_team[2:19], convert_to_numeric)
```

### Team total stats

``` r
#Combining both teams total stats
team_stats <- bind_rows(cleveland_team, gsw_team)
```

### Combined tables

``` r
#Combining both teams starters

all_starters <- bind_rows(cleveland_starters, gsw_starters)

#Combining both teams bench players

all_bench <- bind_rows(cleveland_bench, gsw_bench)

#Combining all players that played

all_played <- bind_rows(cleveland_all_players_played, gsw_all_players_played)
```

# Final look at all tables created

### Cleveland

``` r
cleveland_starters %>%
  kable()
```

| Player      | MIN |        FG |  3PT |   FT | OREB | DREB | REB | AST | STL | BLK |  TO |  PF | +/- | PTS | Position | Team      |
|:------------|----:|----------:|-----:|-----:|-----:|-----:|----:|----:|----:|----:|----:|----:|----:|----:|:---------|:----------|
| K. Love     |  30 | 0.3333333 | 0.00 | 0.75 |    4 |   10 |  14 |   3 |   2 |   0 |   1 |   2 |  19 |   9 | PF       | Cleveland |
| L. James    |  47 | 0.3750000 | 0.20 | 0.80 |    1 |   10 |  11 |  11 |   2 |   3 |   5 |   1 |   4 |  27 | SF       | Cleveland |
| T. Thompson |  32 | 1.0000000 | 0.00 | 0.75 |    0 |    3 |   3 |   0 |   0 |   2 |   0 |   4 |   2 |   9 | C        | Cleveland |
| K. Irving   |  43 | 0.4347826 | 0.40 | 1.00 |    3 |    3 |   6 |   1 |   1 |   1 |   2 |   3 |  10 |  26 | PG       | Cleveland |
| J. Smith    |  39 | 0.3846154 | 0.25 | 0.00 |    0 |    4 |   4 |   2 |   1 |   0 |   2 |   3 |   7 |  12 | G        | Cleveland |

``` r
cleveland_bench %>%
  kable()
```

| Player       | MIN |        FG |       3PT |  FT | OREB | DREB | REB | AST | STL | BLK |  TO |  PF | +/- | PTS | Position | Team      |
|:-------------|----:|----------:|----------:|----:|-----:|-----:|----:|----:|----:|----:|----:|----:|----:|----:|:---------|:----------|
| R. Jefferson |  26 | 0.2500000 | 0.0000000 |   0 |    1 |    8 |   9 |   0 |   1 |   0 |   0 |   1 |  -8 |   2 | F        | Cleveland |
| M. Williams  |   5 | 0.3333333 | 0.0000000 |   0 |    0 |    0 |   0 |   0 |   0 |   0 |   1 |   1 |  -5 |   2 | PG       | Cleveland |
| I. Shumpert  |  19 | 0.3333333 | 0.3333333 |   1 |    0 |    1 |   1 |   0 |   0 |   0 |   0 |   0 |  -9 |   6 | G        | Cleveland |

``` r
cleveland_dnp %>%
  kable()
```

| Player         | MIN                  | FG                   | 3PT                  | FT                   | OREB                 | DREB                 | REB                  | AST                  | STL                  | BLK                  | TO                   | PF                   | +/-                  | PTS                  | Position | Team      |
|:---------------|:---------------------|:---------------------|:---------------------|:---------------------|:---------------------|:---------------------|:---------------------|:---------------------|:---------------------|:---------------------|:---------------------|:---------------------|:---------------------|:---------------------|:---------|:----------|
| C. Frye        | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | F        | Cleveland |
| T. Mozgov      | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | C        | Cleveland |
| M. Dellavedova | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | PG       | Cleveland |
| D. Jones       | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | SG       | Cleveland |
| J. Jones       | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | SG       | Cleveland |

``` r
cleveland_all_players_played %>%
  kable()
```

| Player       | MIN |        FG |       3PT |   FT | OREB | DREB | REB | AST | STL | BLK |  TO |  PF | +/- | PTS | Position | Team      |
|:-------------|----:|----------:|----------:|-----:|-----:|-----:|----:|----:|----:|----:|----:|----:|----:|----:|:---------|:----------|
| K. Love      |  30 | 0.3333333 | 0.0000000 | 0.75 |    4 |   10 |  14 |   3 |   2 |   0 |   1 |   2 |  19 |   9 | PF       | Cleveland |
| L. James     |  47 | 0.3750000 | 0.2000000 | 0.80 |    1 |   10 |  11 |  11 |   2 |   3 |   5 |   1 |   4 |  27 | SF       | Cleveland |
| T. Thompson  |  32 | 1.0000000 | 0.0000000 | 0.75 |    0 |    3 |   3 |   0 |   0 |   2 |   0 |   4 |   2 |   9 | C        | Cleveland |
| K. Irving    |  43 | 0.4347826 | 0.4000000 | 1.00 |    3 |    3 |   6 |   1 |   1 |   1 |   2 |   3 |  10 |  26 | PG       | Cleveland |
| J. Smith     |  39 | 0.3846154 | 0.2500000 | 0.00 |    0 |    4 |   4 |   2 |   1 |   0 |   2 |   3 |   7 |  12 | G        | Cleveland |
| R. Jefferson |  26 | 0.2500000 | 0.0000000 | 0.00 |    1 |    8 |   9 |   0 |   1 |   0 |   0 |   1 |  -8 |   2 | F        | Cleveland |
| M. Williams  |   5 | 0.3333333 | 0.0000000 | 0.00 |    0 |    0 |   0 |   0 |   0 |   0 |   1 |   1 |  -5 |   2 | PG       | Cleveland |
| I. Shumpert  |  19 | 0.3333333 | 0.3333333 | 1.00 |    0 |    1 |   1 |   0 |   0 |   0 |   0 |   0 |  -9 |   6 | G        | Cleveland |

``` r
cleveland_team %>%
  kable()
```

| Team      | FG MADE | FG ATT | FG PCT | 3PT MADE | 3PT ATT | 3PT PCT | FT MADE | FT ATT | FT PCT | OREB | DREB | REB | AST | STL | BLK |  TO |  PF | PTS |
|:----------|--------:|-------:|-------:|---------:|--------:|--------:|--------:|-------:|-------:|-----:|-----:|----:|----:|----:|----:|----:|----:|----:|
| Cleveland |      33 |     82 |  0.402 |        6 |      25 |    0.24 |      21 |     25 |   0.84 |    9 |   39 |  48 |  17 |   7 |   6 |  11 |  15 |  93 |

### Golden State Warriors

``` r
gsw_starters %>%
  kable()
```

| Player      | MIN |        FG |       3PT |  FT | OREB | DREB | REB | AST | STL | BLK |  TO |  PF | +/- | PTS | Position | Team |
|:------------|----:|----------:|----------:|----:|-----:|-----:|----:|----:|----:|----:|----:|----:|----:|----:|:---------|:-----|
| D. Green    |  47 | 0.7333333 | 0.7500000 |   1 |    1 |   14 |  15 |   9 |   2 |   0 |   2 |   3 |  -1 |  32 | PF       | GSW  |
| H. Barnes   |  29 | 0.3000000 | 0.5000000 |   1 |    0 |    2 |   2 |   1 |   1 |   0 |   0 |   4 |  -6 |  10 | SF       | GSW  |
| F. Ezeli    |  11 | 0.0000000 | 0.0000000 |   0 |    0 |    1 |   1 |   1 |   0 |   0 |   0 |   2 |  -9 |   0 | C        | GSW  |
| S. Curry    |  39 | 0.3157895 | 0.2857143 |   1 |    0 |    5 |   5 |   2 |   1 |   1 |   4 |   4 |  -3 |  17 | PG       | GSW  |
| K. Thompson |  42 | 0.3529412 | 0.2000000 |   0 |    1 |    1 |   2 |   2 |   1 |   0 |   3 |   2 | -11 |  14 | SG       | GSW  |

``` r
gsw_bench %>%
  kable()
```

| Player        | MIN |        FG | 3PT |  FT | OREB | DREB | REB | AST | STL | BLK |  TO |  PF | +/- | PTS | Position | Team |
|:--------------|----:|----------:|----:|----:|-----:|-----:|----:|----:|----:|----:|----:|----:|----:|----:|:---------|:-----|
| A. Iguodala   |  38 | 0.3333333 |   0 | 0.0 |    2 |    7 |   9 |   4 |   2 |   2 |   0 |   3 |   3 |   4 | F        | GSW  |
| M. Speights   |   5 | 0.0000000 |   0 | 0.0 |    2 |    2 |   4 |   0 |   0 |   2 |   0 |   0 |   3 |   0 | F        | GSW  |
| A. Varejao    |   8 | 0.0000000 |   0 | 0.5 |    0 |    0 |   0 |   1 |   0 |   0 |   0 |   3 |  -9 |   1 | C        | GSW  |
| L. Barbosa    |   4 | 0.5000000 |   1 | 0.0 |    0 |    0 |   0 |   0 |   0 |   0 |   0 |   1 |   5 |   3 | G        | GSW  |
| S. Livingston |  16 | 0.4285714 |   0 | 1.0 |    1 |    0 |   1 |   2 |   0 |   0 |   1 |   1 |   8 |   8 | G        | GSW  |

``` r
gsw_dnp %>%
  kable()
```

| Player    | MIN                  | FG                   | 3PT                  | FT                   | OREB                 | DREB                 | REB                  | AST                  | STL                  | BLK                  | TO                   | PF                   | +/-                  | PTS                  | Position | Team |
|:----------|:---------------------|:---------------------|:---------------------|:---------------------|:---------------------|:---------------------|:---------------------|:---------------------|:---------------------|:---------------------|:---------------------|:---------------------|:---------------------|:---------------------|:---------|:-----|
| B. Rush   | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | F        | GSW  |
| J. McAdoo | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | SF       | GSW  |
| I. Clark  | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | DNP-COACH’S DECISION | G        | GSW  |

``` r
gsw_all_players_played %>%
  kable()
```

| Player        | MIN |        FG |       3PT |  FT | OREB | DREB | REB | AST | STL | BLK |  TO |  PF | +/- | PTS | Position | Team |
|:--------------|----:|----------:|----------:|----:|-----:|-----:|----:|----:|----:|----:|----:|----:|----:|----:|:---------|:-----|
| D. Green      |  47 | 0.7333333 | 0.7500000 | 1.0 |    1 |   14 |  15 |   9 |   2 |   0 |   2 |   3 |  -1 |  32 | PF       | GSW  |
| H. Barnes     |  29 | 0.3000000 | 0.5000000 | 1.0 |    0 |    2 |   2 |   1 |   1 |   0 |   0 |   4 |  -6 |  10 | SF       | GSW  |
| F. Ezeli      |  11 | 0.0000000 | 0.0000000 | 0.0 |    0 |    1 |   1 |   1 |   0 |   0 |   0 |   2 |  -9 |   0 | C        | GSW  |
| S. Curry      |  39 | 0.3157895 | 0.2857143 | 1.0 |    0 |    5 |   5 |   2 |   1 |   1 |   4 |   4 |  -3 |  17 | PG       | GSW  |
| K. Thompson   |  42 | 0.3529412 | 0.2000000 | 0.0 |    1 |    1 |   2 |   2 |   1 |   0 |   3 |   2 | -11 |  14 | SG       | GSW  |
| A. Iguodala   |  38 | 0.3333333 | 0.0000000 | 0.0 |    2 |    7 |   9 |   4 |   2 |   2 |   0 |   3 |   3 |   4 | F        | GSW  |
| M. Speights   |   5 | 0.0000000 | 0.0000000 | 0.0 |    2 |    2 |   4 |   0 |   0 |   2 |   0 |   0 |   3 |   0 | F        | GSW  |
| A. Varejao    |   8 | 0.0000000 | 0.0000000 | 0.5 |    0 |    0 |   0 |   1 |   0 |   0 |   0 |   3 |  -9 |   1 | C        | GSW  |
| L. Barbosa    |   4 | 0.5000000 | 1.0000000 | 0.0 |    0 |    0 |   0 |   0 |   0 |   0 |   0 |   1 |   5 |   3 | G        | GSW  |
| S. Livingston |  16 | 0.4285714 | 0.0000000 | 1.0 |    1 |    0 |   1 |   2 |   0 |   0 |   1 |   1 |   8 |   8 | G        | GSW  |

``` r
gsw_team %>%
  kable()
```

| Team | FG MADE | FG ATT | FG PCT | 3PT MADE | 3PT ATT | 3PT PCT | FT MADE | FT ATT | FT PCT | OREB | DREB | REB | AST | STL | BLK |  TO |  PF | PTS |
|:-----|--------:|-------:|-------:|---------:|--------:|--------:|--------:|-------:|-------:|-----:|-----:|----:|----:|----:|----:|----:|----:|----:|
| GSW  |      32 |     83 |  0.386 |       15 |      41 |   0.366 |      10 |     13 |  0.769 |    7 |   32 |  39 |  22 |   7 |   5 |  10 |  23 |  89 |

### Team Totals

``` r
team_stats %>%
  kable()
```

| Team      | FG MADE | FG ATT | FG PCT | 3PT MADE | 3PT ATT | 3PT PCT | FT MADE | FT ATT | FT PCT | OREB | DREB | REB | AST | STL | BLK |  TO |  PF | PTS |
|:----------|--------:|-------:|-------:|---------:|--------:|--------:|--------:|-------:|-------:|-----:|-----:|----:|----:|----:|----:|----:|----:|----:|
| Cleveland |      33 |     82 |  0.402 |        6 |      25 |   0.240 |      21 |     25 |  0.840 |    9 |   39 |  48 |  17 |   7 |   6 |  11 |  15 |  93 |
| GSW       |      32 |     83 |  0.386 |       15 |      41 |   0.366 |      10 |     13 |  0.769 |    7 |   32 |  39 |  22 |   7 |   5 |  10 |  23 |  89 |

``` r
all_starters %>%
  kable()
```

| Player      | MIN |        FG |       3PT |   FT | OREB | DREB | REB | AST | STL | BLK |  TO |  PF | +/- | PTS | Position | Team      |
|:------------|----:|----------:|----------:|-----:|-----:|-----:|----:|----:|----:|----:|----:|----:|----:|----:|:---------|:----------|
| K. Love     |  30 | 0.3333333 | 0.0000000 | 0.75 |    4 |   10 |  14 |   3 |   2 |   0 |   1 |   2 |  19 |   9 | PF       | Cleveland |
| L. James    |  47 | 0.3750000 | 0.2000000 | 0.80 |    1 |   10 |  11 |  11 |   2 |   3 |   5 |   1 |   4 |  27 | SF       | Cleveland |
| T. Thompson |  32 | 1.0000000 | 0.0000000 | 0.75 |    0 |    3 |   3 |   0 |   0 |   2 |   0 |   4 |   2 |   9 | C        | Cleveland |
| K. Irving   |  43 | 0.4347826 | 0.4000000 | 1.00 |    3 |    3 |   6 |   1 |   1 |   1 |   2 |   3 |  10 |  26 | PG       | Cleveland |
| J. Smith    |  39 | 0.3846154 | 0.2500000 | 0.00 |    0 |    4 |   4 |   2 |   1 |   0 |   2 |   3 |   7 |  12 | G        | Cleveland |
| D. Green    |  47 | 0.7333333 | 0.7500000 | 1.00 |    1 |   14 |  15 |   9 |   2 |   0 |   2 |   3 |  -1 |  32 | PF       | GSW       |
| H. Barnes   |  29 | 0.3000000 | 0.5000000 | 1.00 |    0 |    2 |   2 |   1 |   1 |   0 |   0 |   4 |  -6 |  10 | SF       | GSW       |
| F. Ezeli    |  11 | 0.0000000 | 0.0000000 | 0.00 |    0 |    1 |   1 |   1 |   0 |   0 |   0 |   2 |  -9 |   0 | C        | GSW       |
| S. Curry    |  39 | 0.3157895 | 0.2857143 | 1.00 |    0 |    5 |   5 |   2 |   1 |   1 |   4 |   4 |  -3 |  17 | PG       | GSW       |
| K. Thompson |  42 | 0.3529412 | 0.2000000 | 0.00 |    1 |    1 |   2 |   2 |   1 |   0 |   3 |   2 | -11 |  14 | SG       | GSW       |

``` r
#Combining both teams bench players

all_bench %>% 
  kable()
```

| Player        | MIN |        FG |       3PT |  FT | OREB | DREB | REB | AST | STL | BLK |  TO |  PF | +/- | PTS | Position | Team      |
|:--------------|----:|----------:|----------:|----:|-----:|-----:|----:|----:|----:|----:|----:|----:|----:|----:|:---------|:----------|
| R. Jefferson  |  26 | 0.2500000 | 0.0000000 | 0.0 |    1 |    8 |   9 |   0 |   1 |   0 |   0 |   1 |  -8 |   2 | F        | Cleveland |
| M. Williams   |   5 | 0.3333333 | 0.0000000 | 0.0 |    0 |    0 |   0 |   0 |   0 |   0 |   1 |   1 |  -5 |   2 | PG       | Cleveland |
| I. Shumpert   |  19 | 0.3333333 | 0.3333333 | 1.0 |    0 |    1 |   1 |   0 |   0 |   0 |   0 |   0 |  -9 |   6 | G        | Cleveland |
| A. Iguodala   |  38 | 0.3333333 | 0.0000000 | 0.0 |    2 |    7 |   9 |   4 |   2 |   2 |   0 |   3 |   3 |   4 | F        | GSW       |
| M. Speights   |   5 | 0.0000000 | 0.0000000 | 0.0 |    2 |    2 |   4 |   0 |   0 |   2 |   0 |   0 |   3 |   0 | F        | GSW       |
| A. Varejao    |   8 | 0.0000000 | 0.0000000 | 0.5 |    0 |    0 |   0 |   1 |   0 |   0 |   0 |   3 |  -9 |   1 | C        | GSW       |
| L. Barbosa    |   4 | 0.5000000 | 1.0000000 | 0.0 |    0 |    0 |   0 |   0 |   0 |   0 |   0 |   1 |   5 |   3 | G        | GSW       |
| S. Livingston |  16 | 0.4285714 | 0.0000000 | 1.0 |    1 |    0 |   1 |   2 |   0 |   0 |   1 |   1 |   8 |   8 | G        | GSW       |

``` r
#Combining all players that played

all_played %>%
  kable()
```

| Player        | MIN |        FG |       3PT |   FT | OREB | DREB | REB | AST | STL | BLK |  TO |  PF | +/- | PTS | Position | Team      |
|:--------------|----:|----------:|----------:|-----:|-----:|-----:|----:|----:|----:|----:|----:|----:|----:|----:|:---------|:----------|
| K. Love       |  30 | 0.3333333 | 0.0000000 | 0.75 |    4 |   10 |  14 |   3 |   2 |   0 |   1 |   2 |  19 |   9 | PF       | Cleveland |
| L. James      |  47 | 0.3750000 | 0.2000000 | 0.80 |    1 |   10 |  11 |  11 |   2 |   3 |   5 |   1 |   4 |  27 | SF       | Cleveland |
| T. Thompson   |  32 | 1.0000000 | 0.0000000 | 0.75 |    0 |    3 |   3 |   0 |   0 |   2 |   0 |   4 |   2 |   9 | C        | Cleveland |
| K. Irving     |  43 | 0.4347826 | 0.4000000 | 1.00 |    3 |    3 |   6 |   1 |   1 |   1 |   2 |   3 |  10 |  26 | PG       | Cleveland |
| J. Smith      |  39 | 0.3846154 | 0.2500000 | 0.00 |    0 |    4 |   4 |   2 |   1 |   0 |   2 |   3 |   7 |  12 | G        | Cleveland |
| R. Jefferson  |  26 | 0.2500000 | 0.0000000 | 0.00 |    1 |    8 |   9 |   0 |   1 |   0 |   0 |   1 |  -8 |   2 | F        | Cleveland |
| M. Williams   |   5 | 0.3333333 | 0.0000000 | 0.00 |    0 |    0 |   0 |   0 |   0 |   0 |   1 |   1 |  -5 |   2 | PG       | Cleveland |
| I. Shumpert   |  19 | 0.3333333 | 0.3333333 | 1.00 |    0 |    1 |   1 |   0 |   0 |   0 |   0 |   0 |  -9 |   6 | G        | Cleveland |
| D. Green      |  47 | 0.7333333 | 0.7500000 | 1.00 |    1 |   14 |  15 |   9 |   2 |   0 |   2 |   3 |  -1 |  32 | PF       | GSW       |
| H. Barnes     |  29 | 0.3000000 | 0.5000000 | 1.00 |    0 |    2 |   2 |   1 |   1 |   0 |   0 |   4 |  -6 |  10 | SF       | GSW       |
| F. Ezeli      |  11 | 0.0000000 | 0.0000000 | 0.00 |    0 |    1 |   1 |   1 |   0 |   0 |   0 |   2 |  -9 |   0 | C        | GSW       |
| S. Curry      |  39 | 0.3157895 | 0.2857143 | 1.00 |    0 |    5 |   5 |   2 |   1 |   1 |   4 |   4 |  -3 |  17 | PG       | GSW       |
| K. Thompson   |  42 | 0.3529412 | 0.2000000 | 0.00 |    1 |    1 |   2 |   2 |   1 |   0 |   3 |   2 | -11 |  14 | SG       | GSW       |
| A. Iguodala   |  38 | 0.3333333 | 0.0000000 | 0.00 |    2 |    7 |   9 |   4 |   2 |   2 |   0 |   3 |   3 |   4 | F        | GSW       |
| M. Speights   |   5 | 0.0000000 | 0.0000000 | 0.00 |    2 |    2 |   4 |   0 |   0 |   2 |   0 |   0 |   3 |   0 | F        | GSW       |
| A. Varejao    |   8 | 0.0000000 | 0.0000000 | 0.50 |    0 |    0 |   0 |   1 |   0 |   0 |   0 |   3 |  -9 |   1 | C        | GSW       |
| L. Barbosa    |   4 | 0.5000000 | 1.0000000 | 0.00 |    0 |    0 |   0 |   0 |   0 |   0 |   0 |   1 |   5 |   3 | G        | GSW       |
| S. Livingston |  16 | 0.4285714 | 0.0000000 | 1.00 |    1 |    0 |   1 |   2 |   0 |   0 |   1 |   1 |   8 |   8 | G        | GSW       |

### Creating Excel File

This file will be imported in Tableau to create a dashboard

``` r
#Creating a list of all of our tables created
sheets <- list("Cleveland Starters" = cleveland_starters, 
               "Cleveland Bench" = cleveland_bench,
               "Cleveland DNP" = cleveland_dnp,
               "Cleveland All Players that Played" = cleveland_all_players_played,
               "Cleveland Team Stats" = cleveland_team,
               "GSW Starters" = gsw_starters, 
               "GSW Bench" = gsw_bench,
               "GSW DNP" = gsw_dnp,
               "GSW All Players that Played" = gsw_all_players_played,
               "GSW Team Stats" = gsw_team,
               "Overall Team Stats" = team_stats,
               "All Starters" = all_starters,
               "All Bench" = all_bench,
               "All Players that Played" = all_played)

#Creating an excel file with all of our sheets
write_xlsx(sheets, path = "Game 7 2016 NBA Finals", col_names = TRUE)
```

# Tableau

After creating my excel spreadsheet, I exported it into Tableau to
create a dashboard highlighting some of the important stats for the
game. I created my own boxscore, highlighting the player that attained
the highest in each statistics, along with multiple bar charts to
clearly show the comparison between players on different teams, as well
as overall team statistics.

# Conclusion

**Web scraping** is an extremely important skill to know to perform
meaningful analysis. Combining this with data cleaning skills, **web
scraping** gives us the ability to access much more data that may not be
compiled in a clean .csv or .xlsx file ready for analysis, ultimately
allowing us to delve deeper into any topic.
