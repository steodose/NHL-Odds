##### 2020-21 NHL Odds App #####
## By: Stephan Teodosescu

library(shiny)
library(shinythemes)

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

##### Load datasets #####

#Load Neil Paine's (538) historical NHL Elo ratings from his Github
url_historical <- getURL("https://raw.githubusercontent.com/NeilPaine538/NHL-Player-And-Team-Ratings/master/nhl_elo_historical.csv")
elo_historical <- read_csv(url_historical)

#Load Neil Paine's (538) current NHL Elo ratings and playoff odds from his Github
url <- getURL("https://raw.githubusercontent.com/NeilPaine538/NHL-Player-And-Team-Ratings/master/NHL-odds-current.csv")
odds_current <- read_csv(url)

#Load Neil Paine's historical odds for this season
url_odds <- getURL("https://raw.githubusercontent.com/NeilPaine538/NHL-Player-And-Team-Ratings/master/NHL-odds-history.csv")
odds_2021 <- read_csv(url_odds)


##### Data Pre-processing #####

#Fetch current date for updating visualizations
update_date <- Sys.Date() %>%
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
                vars(logo)
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
         subtitle = glue("Daily playoff odds (based on Elo Ratings) for teams in the NHL's North Division. As of games prior to {update_date}."),
         caption = "Data: Neil Paine (FiveThirtyEight.com)\nGraphic: @steodosescu") +
    scale_color_manual(name="", values = canada_manual_colors) +
    theme(plot.title = element_text(face="bold")) +
    geom_hline(yintercept = 50.0, color = "red", linetype = "dashed", alpha=0.5)


# Canadian teams playoff odds table
playoff_odds_table_canada <- odds_current %>%
    select(logo, `Team Name`, Division, `Elo Rating`, `Win Div.%`, `Playoffs%`, `Make 2nd Rd`, `Make 3rd Rd`, `Make Finals`, `Win Finals`) %>%
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
               subtitle = glue("Probabilities based on Elo ratings, courtesy of Neil Paine (FiveThirtyEight.com). Elo measures a team's strength over time, accounting for quality of opponents, locations of games and margin of victory. As of games prior to {update_date}.")) %>%
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
         subtitle = glue("Daily playoff odds (based on Elo Ratings) for teams in the West. As of games prior to {update_date}."),
         caption = "Data: Neil Paine (FiveThirtyEight.com)\nGraphic: @steodosescu") +
    scale_color_manual(name="", values = west_manual_colors) +
    theme(plot.title = element_text(face="bold")) +
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
               subtitle = glue("Probabilities based on Elo ratings, courtesy of Neil Paine (FiveThirtyEight.com). Elo measures a team's strength over time, accounting for quality of opponents, locations of games and margin of victory. As of games prior to {update_date}.")) %>%
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
         subtitle = glue("Daily playoff odds (based on Elo Ratings) for teams in the Central. As of games prior to {update_date}."),
         caption = "Data: Neil Paine (FiveThirtyEight.com)\nGraphic: @steodosescu") +
    scale_color_manual(name="", values = central_manual_colors) +
    theme(plot.title = element_text(face="bold")) +
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
               subtitle = glue("Probabilities based on Elo ratings, courtesy of Neil Paine (FiveThirtyEight.com). Elo measures a team's strength over time, accounting for quality of opponents, locations of games and margin of victory. As of games prior to {update_date}.")) %>%
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
         subtitle = glue("Daily playoff odds (based on Elo Ratings) for teams in the East. As of games prior to {update_date}."),
         caption = "Data: Neil Paine (FiveThirtyEight.com)\nGraphic: @steodosescu") +
    scale_color_manual(name="", values = east_manual_colors) +
    theme(plot.title = element_text(face="bold")) +
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
               subtitle = glue("Probabilities based on Elo ratings, courtesy of Neil Paine (FiveThirtyEight.com). Elo measures a team's strength over time, accounting for quality of opponents, locations of games and margin of victory. As of games prior to {update_date}.")) %>%
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
               subtitle = glue("Elo ratings courtesy of Neil Paine. Elo measures a team's strength over time, accounting for the strength of opponents, locations of games and margin of victory. As of games prior to {update_date}.")) %>%
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
                              "Detriot Red Wings" = "#CE1126",
                              "Boston Bruins" = "#000000",
                              "New York Islanders" = "#F47D30",
                              "Pittsburg Penguins" = "#CFC493",
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
    
    navbarPage(theme = shinytheme("spacelab"), #blue navbar theme at the top
               title = tags$div(img(src="NHL.png", height=30), "2020-21 NHL Odds"),
               tabPanel("Power Rankings", icon = icon("signal"), 
                        h1("NHL Power Rankings"),
                        glue("Based on Neil Paine's Elo model at FiveThirtyEight.com. Data as of games prior to {update_date}."),
                        reactableOutput("table")
               ),
               navbarMenu("Division Odds", icon = icon("percent"), #creates dropdown menu
                          tabPanel("Current Chances",
                                   gt_output("plot_table_north"),
                                   gt_output("plot_table_west"),
                                   gt_output("plot_table_central"),
                                   gt_output("plot_table_east")),
                          tabPanel("Season Trends",
                                   plotOutput("plot_north", width = "80%"),
                                   plotOutput("plot_west", width = "80%"),
                                   plotOutput("plot_central", width = "80%"),
                                   plotOutput("plot_east", width = "80%"))), #navbarMenu end
               tabPanel("Historical Elo",icon = icon("hockey-puck"),
                        h1("Complete History of the NHL"),
                        selectizeInput("teamInput", "Team",
                                       choices = unique(elo_historical$Team.A),  
                                       selected="Tampa Bay Lightning", multiple =FALSE),
                        plotOutput("elo_plot", width = "80%")
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
                          p("App created by ", tags$a(href = "https://steodose.github.io/steodosescu.github.io/", 'Stephan Teodosescu', target = '_blank'), ", April 2021", HTML("&bull;"),
                            "Find the code on Github:", tags$a(href = "https://github.com/steodose/BlogPosts", tags$i(class = 'fa fa-github', style = 'color:#5000a5'), target = '_blank'), style = "font-size: 100%"),
                          p("Questions? Comments? Reach out on Twitter", tags$a(href = "https://twitter.com/steodosescu", tags$i(class = 'fa fa-twitter', style = 'color:#1DA1F2'), target = '_blank'), style = "font-size: 100%"),
                          p(tags$em("Last updated: April 2021"), style = 'font-size:85%'))),
               windowTitle = "2020-21 NHL Odds"
    ) #navbarPage bracket
) #END UI function


##### Define server logic required ####
server <- function(input, output) {
    
    output$table <- renderReactable({
        reactable(odds_current,
                  theme = theme_538,
                  ### add column group header
                  columnGroups = list(
                      colGroup(name = "Chances based on average of 1,000x simulations", 
                               columns = c("Win Div.%","Playoffs%","Make 2nd Rd",
                                           "Make 3rd Rd","Make Finals","Win Finals"))
                  ),
                  showSortIcon = TRUE,
                  searchable = TRUE,
                  language = reactableLang(
                      searchPlaceholder = "SEARCH FOR A TEAM..."),
                  defaultPageSize = 100,
                  columns = list(
                      `Team Name` = colDef(minWidth = 140,
                                           name = "Team Name",
                                           align = "left"),
                      Division = colDef(align = "left"),
                      `Elo Rating` = colDef(maxWidth = 90,
                                            name = "Elo Rating",
                                            style = color_scales(odds_current, colors = my_color_pal),
                                            align = "right"),
                      `Win Div.%` = colDef(align = "right",
                                           style = list(borderLeft = "2px solid #555"),
                                           format =  colFormat(digits = 1)),
                      `Playoffs%` = colDef(align = "right",
                                           format =  colFormat(digits = 1)),
                      `Make 2nd Rd` = colDef(align = "right",
                                             format =  colFormat(digits = 1)),
                      `Make 3rd Rd` = colDef(align = "right",
                                             format =  colFormat(digits = 1)),
                      `Make Finals` = colDef(align = "right",
                                             format =  colFormat(digits = 1)),
                      `Win Finals` = colDef(align = "right",
                                            format =  colFormat(digits = 1)),
                      ### add logos using embed_img()
                      logo = colDef(
                          name = "",
                          maxWidth = 70,
                          align = "center",
                          cell = embed_img(height = "25", width = "40")
                      )),
                  pagination = FALSE,
                  compact = TRUE, 
                  borderless = FALSE, 
                  striped = FALSE,
                  fullWidth = FALSE, 
                  defaultColDef = colDef(align = "center", minWidth = 100)
        )
    })
    
    output$plot_table_north <-
        render_gt(
            expr = playoff_odds_table_canada,
            height = px(700),
            width = px(900)
        )
    
    output$plot_table_west <-
        render_gt(
            expr = playoff_odds_table_west,
            height = px(700),
            width = px(900)
        )
    
    output$plot_table_central <-
        render_gt(
            expr = playoff_odds_table_central,
            height = px(700),
            width = px(900)
        )
    
    output$plot_table_east <-
        render_gt(
            expr = playoff_odds_table_east,
            height = px(700),
            width = px(900)
        )
    
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
    
    #Reactive Elo historical plot
    d <- reactive({
        elo_historical %>%
            filter(Team.A == input$teamInput
            )
    })
    
    output$elo_plot <- renderPlot({
        
        ggplot(d(), aes(x = Date, y = Rating.A.Post, color = Team.A, group = Team.A)) +
            geom_line(size = 1.2) +
            labs(x = "", y = "ELO Rating",
                 title = "Historical NHL Elo Ratings",
                 subtitle = glue("Elo ratings measure a team's strength over time, accounting for the strength of opponents, locations of games and margin of victory. Data as of games prior to {update_date} of current season."),
                 caption = "Data: Neil Paine (Fivethirtyeight.com)\nGraphic: @steodosescu") +
            scale_color_manual(values = historical_manual_colors) +
            theme(plot.title = element_text(face="bold")) +
            theme(legend.position="none") +
            geom_hline(yintercept = 1500, color = "red", linetype = "dashed", alpha=0.5)
    })
} #END Server function


##### Run the application #####
shinyApp(ui = ui, server = server)
