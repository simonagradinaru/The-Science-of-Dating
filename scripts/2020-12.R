# ---- analysis ----
# Setup
library(ggplot2)
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

# ---- Christmas presents ---- 
xmas_labels <- c("Don't know",
                 "You have been going out for longer than a few months", 
                 "You have been going out for a few months", 
                 "You have been going out for a few weeks", 
                 "You have been on 3-4 dates but are not yet going out",
                 "You have been on 1-2 dates", 
                 "You are due to go on your 1st date around Christmas time"
                 )
                 
xmas_male.values <- c(.3, .05, .12, .27, .09, .11, .06)
xmas_female.values <- c(.25, .09, .2, .3, .07, .05, .04)

xmas.data <- data.frame("Reason" = xmas_labels, "Male" = xmas_male.values, "Female" = xmas_female.values)
xmas.df <- reshape2::melt(xmas.data, id.vars = "Reason") %>% 
  magrittr::set_names(c("Reason", "Gender", "Value"))

# When do buy a present - ggplot
ggplot(data = xmas.df, aes(x = Reason, y = Value, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(-.9)) +
  scale_x_discrete(labels = stringr::str_wrap(xmas_labels, width = 25), 
                   name = element_blank()) + 
  scale_y_continuous(name = element_blank()) + 
  geom_text(aes(x = Reason, y = Value, fill = Gender, 
                label = paste0(Value*100, "%")), 
            colour = "white", size = 3.3, fontface = "bold", 
            position = position_dodge(-.9), 
            hjust = +1.3, vjust = +.45) + 
  coord_flip() +
  scale_fill_manual(values = c("#28543C", # Christmas green
                               "#901A1A"  # Christmas Red
                               ), aesthetics = "fill") + 
  theme_minimal() +   
  theme(text = element_text(family = "Source Sans Pro"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        panel.grid = element_blank(), 
        axis.line.y.left = element_line(colour = "grey", size = 0.2),
        legend.title = element_blank(),
        legend.position = c(.9, .9),
        strip.text = element_blank()) + 
  ggtitle(label = stringr::str_wrap("You should you get a Christmas present if...",
                                    width = 50)) +
  guides(guide_legend(reverse = TRUE)) + 
  facet_wrap(~ Gender)


# ---- Chrismas ideas -----
# 10 gift ideas
xmas_gift_ideas <- c("Games, toys",
                     #"DIY and home improvement products",
                     #"None",
                     #"Furniture and decoration",
                     #"Household appliances",
                     "Consumer electronics, media, video and audio",
                     "Mobile phones, tablets, accessories",
                     "Computers and accessories",
                     "Food, liquor",
                     "Jewelery and watches",
                     "Cosmetics, fragrances, heath & beauty aids",
                     "Books",
                     "Clothing, textiles, shoes",
                     "Money",   
                     "Gift cards or gift certificates"
                     ) %>% factor(., levels = .)

xmas_gift_male.values <- c(.16, #.13, .17, .15, .13,
  .30, .20, .31, .24, .17, .17, .23, .33, 
  .23, .36)
xmas_gift_female.values <- c(.08, #.11, .12, .13, .13, 
  .17, .18, .18, .18, .22, .26, .27, .36, 
  .36, .51)
image.paths <- c("https://www.clipartmax.com/png/middle/215-2151452_cash-dollars-money-vector-money.png", # money
                 "https://img.icons8.com/ios/452/gift-card.png" #gift card
                 )

icons <- c("woman-outline", 
           "man-outline",
           "woman-outline", 
           "man-outline"
           #"card-outline"
           )
female <- jpeg::readJPEG(source = "/Users/simonagradinaru/Documents/The Science of Dating Docs/Icons/triangle-woman.jpg")
male <- jpeg::readJPEG(source = "/Users/simonagradinaru/Documents/The Science of Dating Docs/Icons/triangle-man.jpg")

xmas_gift.data <- data.frame(Preference = xmas_gift_ideas, 
                             Male = xmas_gift_male.values, 
                             Female = xmas_gift_female.values) 
                             #Image = image.paths)
xmas_gift.df <- reshape2::melt(xmas_gift.data, id.vars = c("Preference")) %>% 
  magrittr::set_names(c("Preference", "Gender", "Value"))

# When do buy a present - ggplot
ggplot(data = xmas_gift.df, aes(x = Preference, y = Value, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(-.9), guide = FALSE) +
  #geom_image(aes(image = Image), size = 0.04) + 
  #geom_text_repel(point.padding = 0.9, segment.alpha = 0) +
  #geom_icon(aes(image = icons))
  scale_x_discrete(labels = stringr::str_wrap(xmas_gift_ideas, width = 25), 
                   name = element_blank()) + 
  scale_y_continuous(name = element_blank()) +
  geom_text(aes(x = Preference, y = Value, fill = Gender, 
                label = paste0(Value*100, "%")), 
            colour = "white", size = 3.3, fontface = "bold", 
            position = position_dodge(-.9), 
            hjust = +1.3, vjust = +.45) + 
  coord_flip() +
  scale_fill_manual(values = c("#28543C", # Christmas green
                               "#901A1A"  # Christmas Red
                               ), aesthetics = "fill") + 
  theme_minimal() +   
  theme(text = element_text(family = "Source Sans Pro"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        panel.grid = element_blank(), 
        axis.line.y.left = element_line(colour = "grey", size = 0.2),
        strip.text = element_blank(),
        #legend.title = element_blank(),
        legend.position = c(.9, .08)) + 
  ggtitle(label = stringr::str_wrap("Which types of gifts would you like to receive for Christmas?",
                                    width = 60)) +
  facet_wrap(~ Gender)
  #geom_hline(data = data.frame(yint = .5, z = "Female"), aes(yintercept = yint), colour = "grey", size = 0.2)
  #geom_line(aes(x = Preference, y = Value, fill = Gender)
  #transition_states(Preference)  
  #ease_aes('sine-in-out')
