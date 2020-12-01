#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rvest)
library(stringr)
library(XML)
library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)
library(flextable)
library(emo)
library(ggimage)

palette <- c("#264653","#2a9d8f","#8ab17d","#e9c46a","#f4a261","#ee8959","#e76f51")


options(stringsAsFactors = FALSE)

bracket_links <- data.frame(
    link = c("https://www.polltab.com/bracket-poll/URrvtsIld9",
             "https://www.polltab.com/bracket-poll/2AUgl6QF-N",
             "https://www.polltab.com/bracket-poll/tKFzVsuwVh",
             "https://www.polltab.com/bracket-poll/nPYwNZxY24",
             "https://www.polltab.com/bracket-poll/1OlCW40NWW",
             "https://www.polltab.com/bracket-poll/mMlfvDpIal",
             "https://www.polltab.com/bracket-poll/jKtNX35kKL",
             "https://www.polltab.com/bracket-poll/vsWlnB7ri4"),
    bracket = seq(1:8))

rounds <- 1:3

kogmoji_data <- NULL

for(i in seq_len(nrow(bracket_links))){
    link <- bracket_links[i, "link"]
    
    bracket_page <- read_html(link)
    
    # html_structure(bracket_page)
    
    for(round in rounds){
        
        items <- bracket_page %>%
            html_nodes(css = paste0(".round", round)) %>%
            html_nodes(css = ".bracketpoll-group-bracketbox") %>%
            html_nodes(css = ".bracketbox-team-item-label-text") %>%
            html_text("div")
        
        
        votes <- bracket_page %>%
            html_nodes(css = paste0(".round", round)) %>%
            html_nodes(css = ".bracketpoll-group-bracketbox") %>%
            html_nodes(css = ".bracketbox-team-item-label-vote-text") %>%
            html_text("span")
        
        image_urls <- bracket_page %>%
            html_nodes(css = paste0(".round", round)) %>%
            html_nodes(css = ".bracketpoll-group-bracketbox") %>%
            html_nodes(css = ".bracketbox-team-item-image") %>% 
            html_nodes(xpath = "img") %>% 
            html_attr("src")
        
        out <- data.frame(bracket = bracket_links[i, "bracket"],
                          round = round,
                          Matchup = rep(1:(length(items)/2), each = 2),
                          kogmoji = items,
                          kogmoji_url = image_urls,
                          votes = votes) %>%
            mutate(votes = str_replace(votes, " votes", "")) %>%
            mutate(votes = str_replace(votes, " vote", "")) %>%
            mutate(votes = as.numeric(votes))
        
        kogmoji_data <- bind_rows(kogmoji_data, out)
    }
}


kogmoji_data <- kogmoji_data %>%
    group_by(bracket, round, Matchup) %>%
    mutate(Percent = votes/sum(votes)*100) %>% 
    mutate(Winner = case_when(Percent == max(Percent) ~ 1,
                              TRUE ~ 0)) %>%
    ungroup() %>%
    group_by(kogmoji) %>%
    mutate(max_round = max(round)) %>%
    ungroup %>%
    mutate(bracket = factor(paste0("Bracket ", bracket))) %>%
    mutate(round = factor(paste0("Round ", round))) %>%
    rename(Bracket = bracket,
           Round = round,
           Matchup = Matchup,
           Kogmoji = kogmoji,
           Votes = votes,
           Percent = Percent) %>%
    group_by(Kogmoji) %>%
    mutate(Percent = round(Percent, 2)) %>%
    mutate(highlight = case_when(max(Percent) >= 95 | min(Percent) <= 5 ~ 1,
                                 TRUE ~ 0)) %>%
    ungroup()



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Kogmoji Throwdown"),
    
   

    fluidRow(
        column(8, offset = 2, 
               wellPanel(
                   plotOutput("line_plot", height = "600px")
               ))
    ),
    
    fluidRow(
        
        column(3, offset = 1,
               wellPanel(
                   selectInput("bracket", label = "Which Bracket?",
                               choices = unique(kogmoji_data$Bracket), selected = "Bracket 1"),
                   
                   selectInput("round", label = "Which Round?",
                               choices = unique(kogmoji_data$Round), selected = "Round 1")
               )
        ),
               
        
        column(7,
               
               tabsetPanel(type = "tabs",
                           
                           tabPanel("Bracket Plot", plotOutput("bracketplot", 
                                                               width = "800px", height = "600px")),
                           tabPanel("Close Races", tableOutput("close_races")),
                           tabPanel("Top Performers", tableOutput("top_performers")),
                           tabPanel("Worst Performers", tableOutput("worst_performers"))
               )
               
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    bracket_dat <- reactive({
        filter(kogmoji_data, Bracket == input$bracket & Round == input$round)
    })
    
    output$bracketplot = renderPlot({
        ggplot(bracket_dat()) +
            geom_bar(aes(y = forcats::fct_reorder(Kogmoji, as.numeric(Matchup)), x = Percent, fill = factor(Matchup)), stat = "identity") +
            geom_text(aes(y = forcats::fct_reorder(Kogmoji, as.numeric(Matchup)), x = Percent, label = paste0(round(Percent, 1), "%")),
                      nudge_x = 5) +
            theme_bw() +
            facet_grid(Bracket ~ Round, scales = "free_y", switch = "y", space = "free") +
            labs(title = paste0("Kogmoji Showdown: ", input$bracket,
                                ", ", input$round),
                 x = "Percent of votes received",
                 y = "",
                 fill = "Matchup") +
            theme(legend.position = "bottom") +
            guides(fill = guide_legend(nrow = 1)) +
            scale_fill_brewer(palette = "Dark2") 
    })
    
    output$top_performers = renderTable({
        bracket_dat() %>%
            select(-Bracket, -Round) %>%
            slice_max(n = 3, Percent, with_ties = TRUE) %>%
            arrange(desc(Percent))
    })
    
    output$worst_performers = renderTable({
        bracket_dat() %>%
            select(-Bracket, -Round) %>%
            slice_min(n = 3, Percent, with_ties = TRUE) %>%
            arrange(Percent)
    })
    
    output$close_races = renderTable({
        bracket_dat() %>%
            select(-Bracket, -Round) %>%
            filter(Percent >= 45 & Percent <= 55)
    })
    
    output$line_plot = renderPlot(
        
        ggplot(kogmoji_data) +
            geom_line(aes(x = Round, y = Percent, group = Kogmoji,
                          color = Kogmoji), size = 0.5) +
            geom_image(data = kogmoji_data, 
                       aes(x = Round, y = Percent, group = Kogmoji,
                           image = kogmoji_url),
                       size = .02, #by = "height",
                       position = position_jitter(width = 0.1, height = 2)) +
            ggrepel::geom_text_repel(data = filter(kogmoji_data, highlight == 1),
                                     aes(x = Round, y = Percent, group = Kogmoji,
                                         label = paste0(Kogmoji, ": ", round(Percent, 1), "%")),
                                     size = 2) +
            theme_bw() +
            theme(legend.position = "none", 
                  panel.grid.major.x = element_blank()) +
            labs(title = "Overall Results by Round",
                 x = "")
        
    )

}

# Run the application 
shinyApp(ui = ui, server = server)
