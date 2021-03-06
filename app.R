##### 2020-21 NHL Odds App #####
## By: Stephan Teodosescu

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyscreenshot)

library(tidyverse)
library(teamcolors)
library(gt) #for 538-themed tables
library(extrafont) #for adding in new fonts
library(ggtext)
library(RCurl)
library(magick)
library(ggimage) #for working with team logos
library(webshot) #saving high quality images of gt tables
library(glue)
library(reactable)
library(reactablefmtr)
library(htmltools)
library(plotly)
library(prismatic)

##### Load datasets #####

#Load Neil Paine's (538) historical NHL Elo ratings from his Github
url_historical <- getURL("https://raw.githubusercontent.com/NeilPaine538/NHL-Player-And-Team-Ratings/master/Old%20NHL%20Elo%20data/nhl_elo_historical.csv")
elo_historical <- read_csv(url_historical)

#Load Neil Paine's (538) current NHL Elo ratings and playoff odds from his Github
url <- getURL("https://raw.githubusercontent.com/NeilPaine538/NHL-Player-And-Team-Ratings/master/Old%20NHL%20Elo%20data/NHL-odds-current.csv")
odds_current <- read_csv(url)

#Load Neil Paine's historical odds for this season
url_odds <- getURL("https://raw.githubusercontent.com/NeilPaine538/NHL-Player-And-Team-Ratings/master/Old%20NHL%20Elo%20data/NHL-odds-history.csv")
odds_2021 <- read_csv(url_odds)

#Install the development version of hockeyR (requires R 3.5) from GitHub with:
# install.packages("devtools")
devtools::install_github("danmorse314/hockeyR") #NHL

# Load Vegas win totals (from BetMGM as of Sept. 26) https://www.vegasinsider.com/nhl/odds/point-totals/
vegas_totals <- 'https://raw.githubusercontent.com/steodose/BlogPosts/master/NHL%202022/vegas%20win%20totals.csv' %>% 
    read_csv()


##### Data Pre-processing #####

#Fetch current date for updating visualizations

update_date <- max(elo_historical$Date) %>% 
    format(format="%B %d")

# Filter for NHL colors and logos
nhl_colors <- teamcolors %>%
    filter(league == "nhl")

# Join team colors and logos to current playoff odds dataset. This will be used in the summary table below
odds_current <- odds_current %>% 
    left_join(nhl_colors, by = c("Team Name_1" = "name"))


##### Custom gt table themes for graphics. Inspired by Tom Mock's excellent blog posts #####

gt_theme_538 <- function(data,...) {
    data %>%
        # Add team logos w/ web_image
        text_transform(
            locations = cells_body(
                columns = logo
            ),
            fn = function(x) {
                web_image(
                    url = x,
                    height = 25
                )
            }
        ) %>%
        # Relabel columns
        cols_label(
            logo = ""
        ) %>%
        opt_all_caps()  %>%
        opt_table_font(
            font = list(
                google_font("Chivo"),
                default_fonts()
            )
        ) %>%
        tab_style(
            style = cell_borders(
                sides = "bottom", color = "transparent", weight = px(2)
            ),
            locations = cells_body(
                columns = TRUE,
                # This is a relatively sneaky way of changing the bottom border
                # Regardless of data size
                rows = nrow(data$`_data`)
            )
        )  %>% 
        tab_options(
            column_labels.background.color = "white",
            table.border.top.width = px(3),
            table.border.top.color = "transparent",
            table.border.bottom.color = "transparent",
            table.border.bottom.width = px(3),
            column_labels.border.top.width = px(3),
            column_labels.border.top.color = "transparent",
            column_labels.border.bottom.width = px(3),
            column_labels.border.bottom.color = "black",
            data_row.padding = px(3),
            source_notes.font.size = 12,
            table.font.size = 16,
            heading.align = "left",
            ...
        ) 
}


# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Chivo") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
        )
}


##### Create Playoff Probabilities line graphs and tables #####

# North Division Playoff chances over the course of the season
odds_canada <- odds_2021 %>% 
    filter(Division == "North")

# Initialize the colors for each team in the Canadian division
canada_manual_colors <- c("Maple Leafs" = "#00205B",
                          "Jets" = "#041E42",
                          "Oilers" = "#FF4C00",
                          "Canadiens" = "#AF1E2D",
                          "Flames" = "#F1BE48",
                          "Canucks" = "#00843D",
                          "Senators" = "#000000")

# Create ggplot for Canadian Division
playoff_odds_canada <- ggplot(odds_canada, aes(x=datestamp, y = `Playoffs%`, color = `Team Name`, group = `Team Name`)) +
    geom_line(size = 1.2) +
    labs(x = "", y = "Playoff Odds (%)",
         title = "How Canada's playoff picture has developed, 2020-21",
         subtitle = glue("Daily playoff odds (based on Elo Ratings) for teams in the NHL's North Division. Through regular season games."),
         caption = "Data: Neil Paine (FiveThirtyEight.com)\nGraphic: @steodosescu") +
    scale_color_manual(name="", values = canada_manual_colors) +
    theme(plot.title = element_text(face="bold")) +
    theme(plot.subtitle = element_markdown()) +
    geom_hline(yintercept = 50.0, color = "red", linetype = "dashed", alpha=0.5)


# Canadian teams playoff odds table
playoff_odds_table_canada <- odds_current %>%
    select(`logo`, `Team Name`, Division, `Elo Rating`, `Win Div.%`, `Playoffs%`, `Make 2nd Rd`, `Make 3rd Rd`, `Make Finals`, `Win Finals`) %>%
    arrange(desc(`Playoffs%`)) %>%
    filter(Division == "North") %>%
    gt() %>%
    data_color(columns = 5:10,
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = NULL)) %>%
    gt_theme_538() %>%
    tab_spanner(label = "Average of 1,000x simulations", 
                columns = 5:10) %>%
    cols_align(align = "left",
               columns = 1) %>%
    tab_header(title = md("**2020-21 NHL North Division Playoff Odds**"),
               subtitle = glue("Probabilities based on Elo ratings, courtesy of Neil Paine (FiveThirtyEight.com). Elo measures a team's strength over time, accounting for quality of opponents, locations of games and margin of victory. As of games through {update_date}.")) %>%
    tab_source_note(
        source_note = md("DATA: Neil Paine (FiveThirtyEight.com)<br>TABLE: @steodosescu | Inspired by Tom Mock"))

# Pacific Division Playoff chances over the course of the season
odds_west <- odds_2021 %>% 
    filter(Division == "West")

# Initialize the colors for each team in the West division
west_manual_colors <- c("Avalanche" = "#00205B",
                        "Golden Knights" = "#B4975A",
                        "Wild" = "#154734",
                        "Blues" = "#002F87",
                        "Coyotes" = "#8C2633",
                        "Sharks" = "#006D75",
                        "Kings" = "#000000",
                        "Ducks" = "#F47A38")

# Create ggplot for West Division
playoff_odds_west <- ggplot(odds_west, aes(x=datestamp, y = `Playoffs%`, color = `Team Name`, group = `Team Name`)) +
    geom_line(size = 1.2) +
    labs(x = "", y = "Playoff Odds (%)",
         title = "How the West Division's playoff picture has developed, 2020-21",
         subtitle = glue("Daily playoff odds (based on Elo Ratings) for teams in the West. Through regular season games."),
         caption = "Data: Neil Paine (FiveThirtyEight.com)\nGraphic: @steodosescu") +
    scale_color_manual(name="", values = west_manual_colors) +
    theme(plot.title = element_text(face="bold")) +
    theme(plot.subtitle = element_markdown()) +
    geom_hline(yintercept = 50.0, color = "red", linetype = "dashed", alpha=0.5)

# West Division teams playoff odds table
playoff_odds_table_west <- odds_current %>%
    select(logo, `Team Name`, Division, `Elo Rating`, `Win Div.%`, `Playoffs%`, `Make 2nd Rd`, `Make 3rd Rd`, `Make Finals`, `Win Finals`) %>%
    arrange(desc(`Playoffs%`)) %>%
    filter(Division == "West") %>%
    gt() %>%
    data_color(columns = 5:10,
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = NULL)) %>%
    gt_theme_538() %>%
    tab_spanner(label = "Average of 1,000x simulations", 
                columns = 5:10) %>%
    cols_align(align = "left",
               columns = 1) %>%
    tab_header(title = md("**2020-21 NHL West Division Playoff Odds**"),
               subtitle = glue("Probabilities based on Elo ratings, courtesy of Neil Paine (FiveThirtyEight.com). Elo measures a team's strength over time, accounting for quality of opponents, locations of games and margin of victory. As of games through {update_date}.")) %>%
    tab_source_note(
        source_note = md("DATA: Neil Paine (FiveThirtyEight.com)<br>TABLE: @steodosescu | Inspired by Tom Mock"))


# Central Division Playoff chances over the course of the season
odds_central <- odds_2021 %>% 
    filter(Division == "Central")

# Initialize the colors for each team in the Central Division
central_manual_colors <- c("Lightning" = "#002868",
                           "Hurricanes" = "#CC0000",
                           "Panthers" = "#041E42",
                           "Stars" = "#006847",
                           "Predators" = "#FFB81C",
                           "Blackhawks" = "#CF0A2C",
                           "Blue Jackets" = "#002654",
                           "Red Wings" = "#CE1126")

# Create ggplot for Central Division
playoff_odds_central <- ggplot(odds_central, aes(x=datestamp, y = `Playoffs%`, color = `Team Name`, group = `Team Name`)) +
    geom_line(size = 1.2) +
    labs(x = "", y = "Playoff Odds (%)",
         title = "How the Central Division's playoff picture has developed, 2020-21",
         subtitle = glue("Daily playoff odds (based on Elo Ratings) for teams in the Central. Through regular season games."),
         caption = "Data: Neil Paine (FiveThirtyEight.com)\nGraphic: @steodosescu") +
    scale_color_manual(name="", values = central_manual_colors) +
    theme(plot.title = element_text(face="bold")) +
    theme(plot.subtitle = element_markdown()) +
    geom_hline(yintercept = 50.0, color = "red", linetype = "dashed", alpha=0.5)

# Central Division teams playoff odds table
playoff_odds_table_central <- odds_current %>%
    select(logo, `Team Name`, Division, `Elo Rating`, `Win Div.%`, `Playoffs%`, `Make 2nd Rd`, `Make 3rd Rd`, `Make Finals`, `Win Finals`) %>%
    arrange(desc(`Playoffs%`)) %>%
    filter(Division == "Central") %>%
    gt() %>%
    data_color(columns = 5:10,
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = NULL)) %>%
    gt_theme_538() %>%
    tab_spanner(label = "Average of 1,000x simulations", 
                columns = 5:10) %>%
    cols_align(align = "left",
               columns = 1) %>%
    tab_header(title = md("**2020-21 NHL Central Division Playoff Odds**"),
               subtitle = glue("Probabilities based on Elo ratings, courtesy of Neil Paine (FiveThirtyEight.com). Elo measures a team's strength over time, accounting for quality of opponents, locations of games and margin of victory. As of games through {update_date}.")) %>%
    tab_source_note(
        source_note = md("DATA: Neil Paine (FiveThirtyEight.com)<br>TABLE: @steodosescu | Inspired by Tom Mock"))

# East Division Playoff chances over the course of the season
odds_east <- odds_2021 %>% 
    filter(Division == "East")

# Initialize the colors for each team in the East Division
east_manual_colors <- c("Bruins" = "#000000",
                        "Islanders" = "#F47D30",
                        "Penguins" = "#CFC493",
                        "Capitals" = "#C8102E",
                        "Rangers" = "#0038A8",
                        "Flyers" = "#F74902",
                        "Devils" = "#CE1126",
                        "Sabres" = "#002654")

# Create ggplot for East Division
playoff_odds_east <- ggplot(odds_east, aes(x=datestamp, y = `Playoffs%`, color = `Team Name`, group = `Team Name`)) +
    geom_line(size = 1.2) +
    labs(x = "", y = "Playoff Odds (%)",
         title = "How the East Division's playoff picture has developed, 2020-21",
         subtitle = glue("Daily playoff odds (based on Elo Ratings) for teams in the East. As of games through Through regular season games."),
         caption = "Data: Neil Paine (FiveThirtyEight.com)\nGraphic: @steodosescu") +
    scale_color_manual(name="", values = east_manual_colors) +
    theme(plot.title = element_text(face="bold")) +
    theme(plot.subtitle = element_markdown()) +
    geom_hline(yintercept = 50.0, color = "red", linetype = "dashed", alpha=0.5)


# East Division teams playoff odds table
playoff_odds_table_east <- odds_current %>%
    select(logo, `Team Name`, Division, `Elo Rating`, `Win Div.%`, `Playoffs%`, `Make 2nd Rd`, `Make 3rd Rd`, `Make Finals`, `Win Finals`) %>%
    arrange(desc(`Playoffs%`)) %>%
    filter(Division == "East") %>%
    gt() %>%
    data_color(columns = 5:10,
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = NULL)) %>%
    gt_theme_538() %>%
    tab_spanner(label = "Average of 1,000x simulations", 
                columns = 5:10) %>%
    cols_align(align = "left",
               columns = 1) %>%
    tab_header(title = md("**2020-21 NHL East Division Playoff Odds**"),
               subtitle = glue("Probabilities based on Elo ratings, courtesy of Neil Paine (FiveThirtyEight.com). Elo measures a team's strength over time, accounting for quality of opponents, locations of games and margin of victory. As of games through {update_date}.")) %>%
    tab_source_note(
        source_note = md("DATA: Neil Paine (FiveThirtyEight.com)<br>TABLE: @steodosescu | Inspired by Tom Mock"))

##### NHL Standings/Rankings Table using gt table #####
gt_tbl <- odds_current %>%
    select(logo, `Team Name`, Division, `Elo Rating`, Games, Wins, Losses, Points) %>%
    arrange(desc(`Elo Rating`)) %>%
    gt() %>%
    data_color(columns = 4,
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = NULL)) %>%
    gt_theme_538() %>%
    cols_align(align = "left",
               columns = 1) %>%
    tab_spanner(label = "Average of 1,000x simulations", 
                columns = 5:8) %>%
    tab_header(title = md("**NHL Power Rankings**"),
               subtitle = glue("Elo ratings courtesy of Neil Paine. Elo measures a team's strength over time, accounting for the strength of opponents, locations of games and margin of victory. As of games through {update_date}.")) %>%
    tab_source_note(
        source_note = md("DATA: Neil Paine (FiveThirtyEight.com)<br>TABLE: @steodosescu"))

##### Reactable NHL Standings/Rankings Table #####

# load font from google fonts
htmltools::tags$link(href = "https://fonts.googleapis.com/css?family=Chivo:400,600,700&display=swap", rel = "stylesheet")

# match 538's table theme for the reactable table
theme_538 <- function() {
    reactable::reactableTheme(
        searchInputStyle = list(width = "31%", backgroundColor = "#F9F9F9"),
        style = list(
            fontFamily = "Chivo"
        ),
        headerStyle = list(
            "&:hover[aria-sort]" = list(
                background = "hsl(0, 0%, 80%)"),
            "&[aria-sort='ascending'], &[aria-sort='descending']" = list(
                background = "#555",
                color = "#FFF"
            ),
            borderColor = "#333"
        ),
        borderColor = "#CDCDCD"
    )
}

###### Create Reactable table #####
odds_current <- odds_current %>%
    select(logo, `Team Name`, Division, `Elo Rating`, `Win Div.%`, `Playoffs%`, 
           `Make 2nd Rd`, `Make 3rd Rd`, `Make Finals`, `Win Finals`) %>% 
    arrange(desc(`Elo Rating`)) 

#Define color palette to use in table
my_color_pal <- c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab")


#1. Create current NHL standings table (using hockeyR package)
records <- hockeyR::get_team_records()

# Load NHL colors and logos
nhl_logos_colors <- hockeyR::team_logos_colors

# Join standings and logos dataset
df_nhl <- records %>% 
    left_join(nhl_logos_colors, by = c("team_name" = "full_team_name")) %>% 
    mutate(games = w+l+otl,
           st_points_perc = st_points/(games*2)) %>% 
    select(team_logo_espn,team_name,conference, division, games, overall,st_points,st_points_perc) %>% 
    arrange(desc(st_points))


#2. Points Percentage Plot

# Define an aspect ratio to use throughout. This value is the golden ratio which provides a wider than tall rectangle
asp_ratio <- 1.618 

pts_perc_df <- records %>% 
    left_join(vegas_totals, by = c("team_name" = "Team")) %>% 
    select(team_name, team_abbr, st_points, `Win Total`, everything()) %>% 
    mutate(games = w+l+otl,
           st_points_perc = st_points/(games*2),
           vegas_points_perc = `Win Total`/(82*2),
           points_delta =  st_points_perc - vegas_points_perc) %>% 
    select(team_name, team_abbr, st_points, `Win Total`, games, st_points_perc, vegas_points_perc, points_delta, everything())

# Join in team colors and logos
pts_perc_df <- pts_perc_df %>% 
    left_join(nhl_logos_colors, by = c("team_name" = "full_team_name"))

# Visualize in bar graph with logos as points. Inspired by this tutorial Thomas Mock put together on plotting images as points in ggplot2.
pts_perc_plot <- pts_perc_df %>% 
    ggplot(aes(x = fct_reorder(team_name, -points_delta), y = points_delta)) +
    geom_col(
        aes(
            fill = team_color1, 
            color = after_scale(clr_darken(fill, 0.3))
        ),
        width = 0.4, 
        alpha = .75,
    ) + 
    scale_color_identity(aesthetics =  c("fill"))  +
    geom_image(
        aes(
            image = team_logo_espn                                  
        ), 
        size = 0.035, 
        by = "width", 
        asp = asp_ratio
    ) +
    geom_hline(yintercept = 0, color = "black", size = 1) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme_custom() + 
    theme(axis.text.x = element_blank(), 
          panel.grid.major.x = element_blank(),
          plot.title = element_text(face = 'bold', size = 16), 
          plot.title.position = 'plot') + 
    labs(x = "", 
         y = "Points Percentage Difference (%)", 
         title = "Performance Relative to Expectations", 
         subtitle = paste0("Difference between current points percentage and expected points percentage, according to bookmakers. As of ", format.Date(Sys.Date(), "%b. %d, %Y")), 
         caption = "Source: hockeyR/NHL.com/BetMGM\nPlot: @steodosescu")


#####  Historical NHL Elo Ratings Reactive Plot #####

# Initialize the color scheme
historical_manual_colors <- c("Toronto Maple Leafs" = "#00205B",
                              "Winnipeg Jets" = "#041E42",
                              "Edmonton Oilers" = "#FF4C00",
                              "Montreal Canadiens" = "#AF1E2D",
                              "Calgary Flames" = "#F1BE48",
                              "Vancouver Canucks" = "#00843D",
                              "Ottawa Senators" = "#000000",
                              "Colorado Avalanche" = "#00205B",
                              "Vegas Golden Knights" = "#B4975A",
                              "Minnesota Wild" = "#154734",
                              "St. Louis Blues" = "#002F87",
                              "Arizona Coyotes" = "#8C2633",
                              "San Jose Sharks" = "#006D75",
                              "Los Angeles Kings" = "#000000",
                              "Anaheim Ducks" = "#F47A38",
                              "Tampa Bay Lightning" = "#002868",
                              "Carolina Hurricanes" = "#CC0000",
                              "Florida Panthers" = "#041E42",
                              "Dallas Stars" = "#006847",
                              "Nashville Predators" = "#FFB81C",
                              "Chicago Blackhawks" = "#CF0A2C",
                              "Columbus Blue Jackets" = "#002654",
                              "Detroit Red Wings" = "#CE1126",
                              "Boston Bruins" = "#000000",
                              "New York Islanders" = "#F47D30",
                              "Pittsburgh Penguins" = "#CFC493",
                              "Washington Capitals" = "#C8102E",
                              "New York Rangers" = "#0038A8",
                              "Philadelphia Flyers" = "#F74902",
                              "New Jersey Devils" = "#CE1126",
                              "Buffalo Sabres" = "#002654",
                              "Montreal Wanderers" = "grey",
                              "Toronto Arenas" = "grey",
                              "Toronto St. Patricks" = "grey",
                              "Quebec Athletic Club/Bulldogs" = "grey",
                              "Hamilton Tigers" = "grey",
                              "Montreal Maroons" = "grey",
                              "Pittsburgh Pirates" = "grey",
                              "New York Americans" = "grey",
                              "Chicago Black Hawks" = "grey",
                              "Detroit Cougars" = "grey",
                              "Philadelphia Quakers" = "grey",
                              "Detroit Falcons" = "grey",
                              "St. Louis Eagles" = "grey",
                              "Brooklyn Americans" = "grey",
                              "Oakland Seals" = "grey",
                              "Minnesota North Stars" = "grey",
                              "California Golden Seals" = "grey",
                              "Atlanta Flames" = "grey",
                              "Kansas City Scouts" = "grey",
                              "Colorado Rockies" = "grey",
                              "Cleveland Barons" = "grey",
                              "Quebec Nordiques" = "grey",
                              "Hartford Whalers" = "grey",
                              "Mighty Ducks of Anaheim" = "grey",
                              "Phoenix Coyotes" = "grey",
                              "Atlanta Thrashers" = "grey"
)

# Uncomment this code if needed (it was from before I added reactivity to the historical Elo plot)
## historical_elo_plot <- ggplot(elo_historical, aes(x = Date, y = Rating.A.Post, color = Team.A, group = Team.A)) +
##    geom_line() +
##    labs(x = "", y = "ELO Rating",
##        title = "Historical NHL Elo Ratings",
##         subtitle = glue("Elo ratings measure a team's strength over time, accounting for the strength of opponents, locations of games and margin of victory. Thru {update_date} of the current season."),
##         caption = "Data: Neil Paine (Fivethirtyeight.com)\nGraphic: @steodosescu") +
##    scale_color_manual(values = historical_manual_colors) +
##    theme(plot.title = element_text(face="bold")) +
##    theme(legend.position="none") +
##    geom_hline(yintercept = 1500, color = "red", linetype = "dashed", alpha=0.5)


##### Define UI for application #####

ui <- tags$head(
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "/NHL.png"), #Getting the NHL logo in the browser window
    
    navbarPage(theme = shinytheme("spacelab"), #navbar theme at the top
               title = tags$div(img(src="NHL.png", height=30), "2021-22 NHL"),
               tabPanel("Standings", icon = icon("signal"), 
                        h1("Current NHL Standings"),
                        glue("Results accessed using the hockeyR package. Data as of ", format.Date(Sys.Date(), "%b. %d, %Y.")),
                        reactableOutput("table"),
                        screenshotButton()
               ),
               navbarMenu("Statistics", icon = icon("percent"), #creates dropdown menu
                          tabPanel("Team Stats",
                                   plotOutput("plot_points_perc", width = "50%")),
                          tabPanel("Season Trends",
                                   plotOutput("plot_north", width = "80%"),
                                   plotOutput("plot_west", width = "80%"),
                                   plotOutput("plot_central", width = "80%"),
                                   plotOutput("plot_east", width = "80%"))), #navbarMenu end
               tabPanel("Historical Elo",icon = icon("hockey-puck"),
                        h1("Complete History of the NHL"),
                        "Every NHL franchise's relative strength after every game dating back to the league's inception. An Elo rating of ~1500 is considered average. An expansion team's initial Elo is set to be 1380, and a team's final Elo from the end of one season is reverted toward a mean of 1505 by 30 percent at the start of the following season.",
                        "Elo ratings measure a team's strength over time, accounting for the strength of opponents, locations of games and margin of victory. Information is collected from Neil Paine (FiveThirtyEight).",
                        br(),
                        br(),
                        sidebarPanel(
                            helpText("Select a team (or multiple) to examine Elo ratings over time. 
                            You can also hover to reveal that team's change in Elo rating from game to game on the plot to the right."),
                            selectizeInput("teamInput", "Team",
                                           choices = unique(elo_historical$Team.A),  
                                           selected="Tampa Bay Lightning", multiple = TRUE),
                            sliderInput("yearInput", "Year", min=1918, max=2021, 
                                        value=c(1950, 2021), sep=""), width = 4),
                        
                        mainPanel(withSpinner(plotlyOutput("elo_plot")), width = 8),
               ),
               tabPanel("About", icon = icon("info-circle"),
                        fluidRow(
                            column(8,
                                   includeMarkdown("about.md")
                            )
                        )
               ),
               
               # Footer for web app
               hr(style = "border-color: #cbcbcb;"),
               fluidRow(
                   column(9,
                          p("App created by ", tags$a(href = "https://steodose.github.io/steodosescu.github.io/", 'Stephan Teodosescu', target = '_blank'),", April 2021", HTML("&bull;"),
                            "Find the code on Github:", tags$a(href = "https://github.com/steodose/NHL-Odds", tags$i(class = 'fa fa-github', style = 'color:#5000a5'), target = '_blank'), style = "font-size: 100%"),
                          p("Questions? Comments? Reach out on Twitter", tags$a(href = "https://twitter.com/steodosescu", tags$i(class = 'fa fa-twitter', style = 'color:#1DA1F2'), target = '_blank'), style = "font-size: 100%"),
                          p(tags$em("Last updated: June 2021"), style = 'font-size:85%'))),
               windowTitle = "2021-22 NHL"
    ) #navbarPage bracket
) #END UI function


##### Define server logic required ####
server <- function(input, output) {
    
    output$table <- renderReactable({
        reactable(df_nhl,
                  theme = theme_538,
                  showSortIcon = TRUE,
                  searchable = TRUE,
                  language = reactableLang(
                      searchPlaceholder = "SEARCH FOR A TEAM..."),
                  defaultPageSize = 100,
                  columns = list(
                      team_name = colDef(minWidth = 180,
                                           name = "Team",
                                           align = "left"),
                      division = colDef(align = "left",
                                        name = "Division"),
                      conference = colDef(align = "left",
                                        name = "Conference"),
                      games = colDef(maxWidth = 90,
                                            name = "GP",
                                     style = list(borderLeft = "2px solid #555"),
                                            align = "right"),
                      overall = colDef(align = "right",
                                       name = "Overall Record"),
                      st_points = colDef(align = "right",
                                      name = "Points",
                                      style = color_scales(df_nhl),
                                           format =  colFormat(digits = 0)),
                      st_points_perc= colDef(align = "right",
                                      name = "Points Percentage",
                                      format =  colFormat(digits = 3)),
                      ### add logos using embed_img()
                      team_logo_espn = colDef(
                          name = "",
                          maxWidth = 70,
                          align = "center",
                          cell = embed_img(height = "30", width = "30")
                      )),
                  pagination = FALSE,
                  compact = TRUE, 
                  borderless = FALSE, 
                  striped = FALSE,
                  fullWidth = FALSE, 
                  defaultColDef = colDef(align = "center", minWidth = 100)
        )
    })

    output$plot_points_perc <- renderPlot({
        plot(pts_perc_plot)
    })
    
    
    output$plot_north <- renderPlot({
        plot(playoff_odds_canada)
    })
    
    output$plot_west <- renderPlot({
        plot(playoff_odds_west)
    })
    
    output$plot_central <- renderPlot({
        plot(playoff_odds_central)
    })
    
    output$plot_east <- renderPlot({
        plot(playoff_odds_east)
    })
    
    output$elo_plot <- renderPlot({
        plot(historical_elo_plot)
    })
    
    #Reactive Elo historical plot (Complete history of the NHL)
    d <- reactive({
        elo_historical %>%
            filter(Team.A == input$teamInput,
                   Year >= input$yearInput[1],
                   Year <= input$yearInput[2]
            )
    })
    
    output$elo_plot <- renderPlotly({
        
        ggplotly(ggplot(d(), aes(x = Date, y = Rating.A.Post, color = Team.A)) +
                     geom_line(size = 1.2) +
                     labs(x = "", y = "ELO Rating",
                          title = "Historical NHL Elo Ratings",
                          subtitle = glue("Data as of games through **{update_date}** of 2020-21 season."),
                          caption = "Data: Neil Paine (Fivethirtyeight.com)\nGraphic: @steodosescu") +
                     scale_color_manual(values = historical_manual_colors) +
                     theme(plot.title = element_text(face="bold")) +
                     theme(plot.subtitle = element_markdown()) +
                     theme(legend.position="none") +
                     geom_hline(yintercept = 1500, color = "red", linetype = "dashed", alpha=0.5))
    })
} #END Server function


##### Run the application #####
shinyApp(ui = ui, server = server)
