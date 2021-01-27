# ---- Setup ----
rm(list = ls())
devtools::install_github("GuangchuangYu/ggimage")
library(ggplot2)
library(ggimage)
library(stringr)
library(viridis)
library(reshape2)
library(sysfonts)
library(ggimage)
library(ggrepel)
library(RCurl)
library(tidyverse)
library(gganimate)
font_add_google("Source Sans Pro")

# ---- Pandemics attitudes ---- 
attitude.topics <- c(rep("Coronavirus impact", 12), 
                     rep("Coronavirus changes", 12),
                     rep("Coronavirus break consequences", 4), 
                     rep("Economic fallout changes", 7), 
                     rep("Begin in-person dating", 5), 
                     rep("In-person dating changes", 10)
                     )

attitude.names <- c(rep("To what extend Coronavirus has affected your dating life?", 12),
                    rep("I purposely stopped dating when the pandemic started", 4),
                    rep("I stopped dating to focus on other areas", 4),
                    rep("I was in a relationship but the other person ended it when the pandemic started", 4),
                    # Did your time off from dating result in any of the following?
                    "It made me feel rejuvenated to date again", 
                    "It made me realize how time-consuming or burdensome dating was", 
                    "It made me realize that I don't want to date right now", 
                    "It made me realize I was focusing on the wrong things in my life",
                    # How has the pandemic and the economic fallout affected your views on dating?
                    "I'm more likely to date someone who is employed full-time", 
                    "I'm more open to someone who is unemployed right now", 
                    "I'm more eager to find a financially stable partner", 
                    "I'm more apreciative of frugal people", 
                    "I feel my frugality has become a dating asset", 
                    "I feel my wealth has become more of a dating asset", 
                    "I'm more inclined to spend less money on dating",
                    rep("How ready are you to begin dating in-person again?", 5),
                    # When you begin in-person dating again, if you haven't already, 
                    # how do you plan to proceed compared to dating pre-pandemic? 
                    "I'll be more selective about who I choose to go on a first date with", 
                    "I will insist that we both wear masks throughout the date", 
                    "I will only meet in-person with people who have quarantined or been lockdown for at least 14 days",
                    "I will ask if they're experiencing any physical symptoms before I agree to meet in person", 
                    "I will ask if they've been practicing social distancing", 
                    "I will ask if they've been tested for Covid-19", 
                    "I'll be more selective about where I go on a first date", 
                    "If we go to a restaurant, I will insist we eat outside", 
                    "I will only go on dates that involve outdoor activities", 
                    "I'll be more selective about who I hugm kiss or shake hands with"
                    )
attitude.scales <- c(c("0 - Not at all affected", c(1:10), "11 - Very much affected"),
                     c("Not at all", "Not very", "Somewhat", "Very"), 
                     c("Not at all", "Not very", "Somewhat", "Very"), 
                     c("Not at all", "Not very", "Somewhat", "Very"), 
                     rep("Yes", 4), 
                     rep("Yes", 7),
                     "Not at all ready, it's too soon", 
                     "Somewhat ready, but with precautions", 
                     "Very ready", 
                     "I have already started", 
                     "I never stopped dating in-person", 
                     rep("Yes", 10)
                     )
attitude.values <- c(c(.360, .054, .035, .032, .028, .081, .072, .072, .076, .059, .043, .07),
                        c(.515, .132, .192, .161), 
                        c(.492, .147, .205, .157),
                        c(.708, .105, .125, .062), 
                        round(390/(1376+390), digits = 3),
                        round(553/(1213+553), digits = 3),
                        round(643/(1123+643), digits = 3),
                        round(370/(1396+370), digits = 3),
                        
                        round(1307/(3693+1307), digits = 3),
                        round(622/(4378+622), digits = 3),
                        round(1307/(3693+1307), digits = 3),
                        round(861/(4139+861), digits = 3),
                        round(417/(4583+417), digits = 3),
                        round(295/(4705+295), digits = 3),
                        round(1156/(3844+1156), digits = 3),
                        
                        c(.388, .286, .198, .047, .080), 
                        c(.359, .204, .105, .172, .211, .163, .232, .138, .128, .203)
                        )

pandemic.df <- data.frame(Topic = attitude.topics, 
                          Statement = attitude.names, 
                          Answer = attitude.scales, 
                          Value = attitude.values)
                            


# Plot: To what extend Coronavirus has affected your dating life? 
# pandemic.df %>% 
#   filter(Topic == "Coronavirus impact") %>% 
#   mutate(Category = plyr::mapvalues(Answer, from = 0:11, to = c(rep("Not at all / Slightly", 7),  
#                                                                 rep("Neutral", 2), 
#                                                                 rep("Very much", 3)))) %>%
#   group_by(Category) %>% 
#   summarise(cumulative = sum(Value)) %>% 
#   ggplot(aes(x = Category, y = cumulative)) + 
#   geom_bar(stat = "identity", position = position_dodge(-.9))

pandemic.df %>% 
  filter(Topic == "Coronavirus impact") %>% 
  mutate(Answer = factor(Answer, levels = Answer)) %>% 
  ggplot(aes(x = forcats::fct_rev(Answer), y = Value)) + 
  geom_bar(stat = "identity", fill = "#00a779") + 
  ylim(c(0, .4)) + 
  geom_text(aes(x = forcats::fct_rev(Answer), 
                y = Value, 
                label = paste0(Value*100, "%")), 
            size = 3.3, fontface = "bold", 
            position = position_dodge(-.9), 
            hjust = -.3, vjust = +.45) + 
  coord_flip() + 
  theme_minimal() +   
  theme(text = element_text(family = "Source Sans Pro"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 10),
        panel.grid = element_blank(), 
        axis.line.y.left = element_line(colour = "grey", size = 0.2),
        legend.title = element_blank(),
        legend.position = c(.9, .9),
        strip.text = element_blank()) + 
  ggtitle(label = stringr::str_wrap("To what extend Coronavirus has affected your dating life?",
                                    width = 50)) 


# Coronavirus changes
pandemic.df %>% 
  filter(Topic == "Coronavirus changes") %>% 
  ggplot(aes(x = forcats::fct_rev(Answer), y = Value)) + 
  geom_bar(stat = "identity", fill = "#3078b4") + 
  geom_text(aes(x = forcats::fct_rev(Answer), 
                y = Value, 
                label = paste0(Value*100, "%")), 
            size = 3.3, fontface = "bold", 
            position = position_dodge(-.9), 
            hjust = -.3, vjust = +.45) + 
  scale_y_continuous(limits = c(0, .9)) + 
  coord_flip() + 
  facet_grid(~ Statement, labeller=label_wrap_gen(width = 40)) + 
  theme_minimal() +   
  theme(text = element_text(family = "Source Sans Pro"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 10),
        panel.grid = element_blank(), 
        strip.background = element_rect(fill = "grey90", size = 0),
        axis.line.y.left = element_line(colour = "grey", size = 0.2),
        legend.title = element_blank()) +
  ggtitle(label = stringr::str_wrap("To what extend Coronavirus has affected your dating life?",
                                    width = 70)) 

# Did your time off from dating result in any of the following?
fallout_changes <- pandemic.df %>% 
  filter(Topic == "Economic fallout changes") %>% 
  mutate(Statement = factor(Statement, levels = Statement)) %>% 
  arrange(Value)
  
fallout_changes %>% 
  ggplot(aes(x = reorder(Statement, Value), y = Value)) + 
  geom_bar(stat = "identity", fill = "#3078b4", width = .8) + 
  geom_text(aes(x = reorder(Statement, Value), 
                y = Value, 
                label = paste0(Value*100, "%")), 
            size = 3.3, fontface = "bold", 
            position = position_dodge(-.9), 
            hjust = -.3, vjust = +.45) + 
  scale_y_continuous(limits = c(0, .3)) + 
  scale_x_discrete(labels = stringr::str_wrap(fallout_changes$Statement, width = 25), 
                   name = element_blank()) + 
  coord_flip() + 
  theme_minimal() +   
  theme(text = element_text(family = "Source Sans Pro"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 10),
        panel.grid = element_blank(), 
        strip.background = element_rect(fill = "grey90", size = 0),
        axis.line.y.left = element_line(colour = "grey", size = 0.2),
        legend.title = element_blank()) +
  ggtitle(label = stringr::str_wrap("Did your time off from dating result in any of the following?",
                                    width = 50)) 


# How ready are you to begin dating in-person again?

inperson_dating <- pandemic.df %>% 
  filter(Topic == "Begin in-person dating") %>% 
  mutate(Answer = factor(Answer, levels = Answer))

inperson_dating %>% 
  ggplot(aes(x = reorder(Answer, Value), y = Value)) + 
  geom_bar(stat = "identity", fill = "#f0777b", width = .8) + 
  geom_text(aes(x = reorder(Answer, Value), 
                y = Value, 
                label = paste0(Value*100, "%")), 
            size = 3.3, fontface = "bold", 
            position = position_dodge(-.9), 
            hjust = -.3, vjust = +.45) + 
  scale_y_continuous(limits = c(0, 1)) 
  coord_flip() + 
  theme_minimal() +   
  theme(text = element_text(family = "Source Sans Pro"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 10),
        panel.grid = element_blank(), 
        axis.line.y.left = element_line(colour = "grey", size = 0.2),
        legend.title = element_blank(),
        legend.position = c(.9, .9),
        strip.text = element_blank()) + 
  ggtitle(label = stringr::str_wrap("How ready are you to begin dating in-person again?",
                                    width = 50)) 

pandemic.df %>% 
  filter(Topic == "Begin in-person dating") %>% 
  mutate(Answer = factor(Answer, levels = Answer)) %>% 
  ggplot(aes(x = reorder(Answer, Value), y = Value)) + 
  #geom_bar(stat = "identity", fill = "#9369a8") + 
  geom_image(aes(image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQhykDIl_wkEHLXfantG4oOF9PgNxxPZBIOUA&usqp=CAU"), 
             asp = .7) + 
  ylim(c(0, .45)) + 
  geom_text(aes(x = reorder(Answer, Value), 
                y = Value, 
                label = paste0(Value*100, "%")), 
            size = 3.3, fontface = "bold", 
            position = position_dodge(-.9), 
            hjust = -.5, vjust = +.45) + 
  coord_flip() + 
  theme_minimal() +   
  theme(text = element_text(family = "Source Sans Pro"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 10),
        panel.grid = element_blank(), 
        axis.line.y.left = element_line(colour = "grey", size = 0.2),
        legend.title = element_blank(),
        legend.position = c(.9, .9),
        strip.text = element_blank()) + 
  ggtitle(label = stringr::str_wrap("How ready are you to begin dating in-person again?",
                                    width = 50)) 

# how do you plan to proceed compared to dating pre-pandemic
inperson_changes <- pandemic.df %>% 
  filter(Topic == "In-person dating changes") %>% 
  mutate(Statement = factor(Statement, levels = Statement)) %>% 
  arrange(Value)

inperson_changes %>% 
  ggplot(aes(x = reorder(Statement, Value), y = Value)) + 
  geom_bar(stat = "identity", fill = "#ec8b83") + 
  geom_text(aes(x = reorder(Statement, Value), 
                y = Value, 
                label = paste0(Value*100, "%")), 
            size = 3.3, fontface = "bold", 
            position = position_dodge(-.9), 
            hjust = -.3, vjust = +.45) + 
  scale_y_continuous(limits = c(0, .5)) + 
  scale_x_discrete(labels = stringr::str_wrap(inperson_changes$Statement, width = 25), 
                   name = element_blank()) + 
  coord_flip() + 
  theme_minimal() +   
  theme(text = element_text(family = "Source Sans Pro"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 10),
        panel.grid = element_blank(), 
        strip.background = element_rect(fill = "grey90", size = 0),
        axis.line.y.left = element_line(colour = "grey", size = 0.2),
        legend.title = element_blank()) +
  ggtitle(label = stringr::str_wrap("When you begin in-person dating again, how do you plan to proceed compared to dating pre-pandemic?",
                                    width = 70)) 


inperson_changes[-1,] %>% 
  ggplot(aes(x = reorder(Statement, Value), y = Value)) + 
  #geom_point(shape = 8, size = 2, colour = "#f78295") + 
  geom_image(aes(image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQhykDIl_wkEHLXfantG4oOF9PgNxxPZBIOUA&usqp=CAU"), 
            asp = .8) +
  geom_text(aes(x = reorder(Statement, Value), 
                y = Value, 
                label = paste0(Value*100, "%")), 
            size = 3.3, fontface = "bold", 
            position = position_dodge(-.9), 
            hjust = -.5, vjust = +.45) + 
  scale_y_continuous(limits = c(0, .4)) + 
  scale_x_discrete(labels = stringr::str_wrap(inperson_changes$Statement[-1], width = 35), 
                   name = element_blank()) + 
  coord_flip() + 
  theme_minimal() +   
  theme(text = element_text(family = "Source Sans Pro"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 10),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(fill = "grey90", size = 0),
        axis.line.y.left = element_line(colour = "grey", size = 0.2),
        legend.title = element_blank()) +
  ggtitle(label = stringr::str_wrap("When you begin in-person dating again, how do you plan to proceed compared to dating pre-pandemic?",
                                    width = 50)) 
