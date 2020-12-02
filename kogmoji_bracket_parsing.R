library(tidyverse)
library(rvest)
library(stringr)
library(XML)
library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)

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

################################################################################
#### SOME GRAPHS SHOWING PERFORMANCE OF ALL EMOJIS #############################
################################################################################


# ggplot(kogmoji_data) +
#   geom_bar(aes(y = forcats::fct_reorder(kogmoji, pct), x = pct, fill = factor(pairing)), stat = "identity") +
#   geom_text(aes(y = forcats::fct_reorder(kogmoji, pct), x = pct, label = paste0(round(pct, 1), "%")),
#             nudge_x = 3, size = 3) +
#   theme_bw() +
#   facet_grid(bracket ~ ., scales = "free_y", switch = "y", space = "free") +
#   labs(title = "Kogmoji Showdown",
#        x = "Percent of votes received",
#        y = "",
#        fill = "Matchup") +
#   theme(legend.position = "bottom") +
#   guides(fill = guide_legend(nrow = 1)) +
#   scale_fill_manual(values = palette)

ggplot(kogmoji_data) +
  geom_bar(aes(y = forcats::fct_reorder(kogmoji, as.numeric(pairing)), x = pct, fill = factor(pairing)), stat = "identity") +
  geom_text(aes(y = forcats::fct_reorder(kogmoji, as.numeric(pairing)), x = pct, label = paste0(round(pct, 1), "%")),
            nudge_x = 3, size = 3) +
  theme_bw() +
  facet_grid(bracket ~ round, scales = "free_y", switch = "y", space = "free") +
  labs(title = "Kogmoji Showdown: Complete Results",
       x = "Percent of votes received",
       y = "",
       fill = "Matchup") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_manual(values = palette[c(7, 1, 4, 2, 5, 3, 6)])

# 
# ggplot(kogmoji_data) +
#   geom_bar(aes(y = forcats::fct_reorder(kogmoji, as.numeric(pairing)), x = pct, fill = pct), stat = "identity") +
#   geom_text(aes(y = forcats::fct_reorder(kogmoji, as.numeric(pairing)), x = pct, label = paste0(round(pct, 1), "%")),
#             nudge_x = 3, size = 3) +
#   theme_bw() +
#   facet_grid(bracket ~ ., scales = "free_y", switch = "y", space = "free") +
#   labs(title = "Kogmoji Showdown",
#        x = "Percent of votes received",
#        y = "",
#        fill = "Matchup") +
#   theme(legend.position = "bottom") +
#   scale_fill_viridis_c(direction = -1)


top_performers <- kogmoji_data %>%
  slice_max(n = 10, pct, with_ties = TRUE) %>%
  arrange(desc(pct))

worst_performers <- kogmoji_data %>%
  slice_min(n = 10, pct, with_ties = TRUE) %>%
  arrange(pct)

close_races <- kogmoji_data %>%
  filter(pct > 45 & pct < 55)



######################## app troubleshooting ###################################
ggplot(filter(kogmoji_data, Bracket == "Bracket 1" & Round == "Round 2")) +
  geom_bar(aes(y = forcats::fct_reorder(Kogmoji, as.numeric(Matchup)), x = Percent, fill = factor(Matchup)), stat = "identity") +
  geom_text(aes(y = forcats::fct_reorder(Kogmoji, as.numeric(Matchup)), x = Percent, label = paste0(round(Percent, 1), "%")),
            nudge_x = 5) +
  theme_bw() +
  facet_grid(Bracket ~ Round, scales = "free_y", switch = "y", space = "free") +
  labs(title = paste0("Kogmoji Showdown: ", 1,
                      ", ", 2),
       x = "Percent of votes received",
       y = "",
       fill = "Matchup") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_brewer(palette = "Dark2")


temp <- kogmoji_data %>%
  group_by(Kogmoji) %>%
  mutate(Percent = round(Percent, 2)) %>%
  mutate(highlight = case_when(max(Percent) >= 95 | min(Percent) <= 5 ~ 1,
                               TRUE ~ 0)) 

test <- ggplot() +
  geom_line(data = filter(temp, highlight == 0),
            aes(x = Round, y = Percent, group = Kogmoji),
            color = "darkgray", size = 0.5) +
  geom_line(data = filter(temp, highlight == 1),
            aes(x = Round, y = Percent, group = Kogmoji,
            color = Kogmoji), size = 0.75) +
  geom_point(data = temp, aes(x = Round, y = Percent, group = Kogmoji),
             alpha = 0.5, color = "darkgray") +
  # ggrepel::geom_text_repel(data = filter(temp, highlight == 1),
  #                          aes(x = Round, y = Percent, group = Kogmoji,
  #                              label = paste0(Kogmoji, ": ", round(Percent, 1), "%"))) +
  theme_bw() +
  # facet_grid(Bracket ~ Round, scales = "free_y", switch = "y", space = "free") +
  theme(legend.position = "none") 

test <- ggplot() +
  geom_line(data = filter(kogmoji_data, max_round == max(rounds) & Winner == 1),
            aes(x = Round, y = Percent, group = Kogmoji),
            color = "darkgray",
            size = 0.5) +
  geom_jitter(data = kogmoji_data, 
              aes(x = Round, y = Percent, group = Kogmoji,
                  fill = factor(Winner)),
             stroke = 0, alpha = 0.4, width = 0.05) +
  # ggrepel::geom_text_repel(data = filter(temp, highlight == 1),
  #                          aes(x = Round, y = Percent, group = Kogmoji,
  #                              label = paste0(Kogmoji, ": ", round(Percent, 1), "%"))) +
  theme_bw() +
  # facet_grid(Bracket ~ Round, scales = "free_y", switch = "y", space = "free") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("red", "green"))

plotly::ggplotly(test, tooltip = c("y", "x", "group"), mode = "lines")


test <- bracket_page %>%
  html_nodes(css = paste0(".round", round)) %>%
  html_nodes(css = ".bracketpoll-group-bracketbox") %>%
  html_nodes(css = ".bracketbox-team-item-image") %>% 
  html_nodes(xpath = "img") %>% 
  html_attr("src")


kogmoji_data <- kogmoji_data %>%
  mutate(highlight = case_when(Percent >= 95 | Percent <= 5 ~ 1,
                               TRUE ~ 0))

test <- ggplot() +
  #geom_line(data = filter(kogmoji_data, max_round == max(rounds) & Winner == 1),
  geom_line(data = kogmoji_data, 
            aes(x = Round, y = Percent, group = Kogmoji),
            color = "darkgray",
            size = 0.6) +
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
  # facet_grid(Bracket ~ Round, scales = "free_y", switch = "y", space = "free") +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank()) 

