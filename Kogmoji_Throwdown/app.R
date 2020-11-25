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

rounds <- 1

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
        
        out <- data.frame(bracket = bracket_links[i, "bracket"],
                          round = round,
                          pairing = rep(1:(length(items)/2), each = 2),
                          kogmoji = items,
                          votes = votes) %>%
            mutate(votes = str_replace(votes, " votes", "")) %>%
            mutate(votes = str_replace(votes, " vote", "")) %>%
            mutate(votes = as.numeric(votes))
        
        kogmoji_data <- bind_rows(kogmoji_data, out)
    }
}


kogmoji_data <- kogmoji_data %>%
    group_by(bracket, round, pairing) %>%
    mutate(pct = votes/sum(votes)*100) %>% 
    ungroup() %>%
    mutate(bracket = factor(paste0("Bracket ", bracket))) %>%
    mutate(round = factor(paste0("Round ", round)))



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Kogmoji Throwdown"),

    
    sidebarLayout(
        sidebarPanel(
            selectInput("bracket", label = "Which Bracket?",
                        choices = unique(kogmoji_data$bracket), selected = "Bracket 1"),
            
            selectInput("round", label = "Which Round?",
                        choices = unique(kogmoji_data$round), selected = "Round 1")
        ),

       
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Bracket Plot", plotOutput("bracketplot")),
                        tabPanel("Close Races", tableOutput("close_races")),
                        tabPanel("Top Performers", tableOutput("top_performers")),
                        tabPanel("Worst Performers", tableOutput("worst_performers"))
            )
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   
    output$bracketplot = renderPlot({
        ggplot(filter(kogmoji_data, bracket == input$bracket & round == input$round)) +
            geom_bar(aes(y = forcats::fct_reorder(kogmoji, as.numeric(pairing)), x = pct, fill = factor(pairing)), stat = "identity") +
            geom_text(aes(y = forcats::fct_reorder(kogmoji, as.numeric(pairing)), x = pct, label = paste0(round(pct, 1), "%")),
                      nudge_x = 5) +
            theme_bw() +
            facet_grid(bracket ~ round, scales = "free_y", switch = "y", space = "free") +
            labs(title = paste0("Kogmoji Showdown: ", input$bracket,
                                ", ", input$round),
                 x = "Percent of votes received",
                 y = "",
                 fill = "Matchup") +
            theme(legend.position = "bottom") +
            guides(fill = guide_legend(nrow = 1)) +
            scale_fill_manual(values = palette[c(7, 1, 4, 2, 5, 3, 6)])
    })
    
    output$top_performers = renderTable({
        kogmoji_data %>%
            filter(bracket == input$bracket & round == input$round) %>%
            slice_max(n = 3, pct, with_ties = TRUE) %>%
            arrange(desc(pct))
    })
    
    output$worst_performers = renderTable({
        kogmoji_data %>%
            filter(bracket == input$bracket & round == input$round) %>%
            slice_min(n = 3, pct, with_ties = TRUE) %>%
            arrange(desc(pct))
    })
    
    output$close_races = renderTable({
        kogmoji_data %>%
            filter(bracket == input$bracket & round == input$round) %>%
            filter(pct > 45 & pct < 55)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)