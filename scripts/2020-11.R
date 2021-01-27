# ---- analysis ----
# Setup
library(ggplot2)
library(stringr)
library(viridis)
library(reshape2)
library(sysfonts)
font_add_google("Source Sans Pro")

# ---- Online dating ----
# Reasons for online dating - data
usage.reasons <- c("To check my partner is not cheating on me",
                   "To find a marriage partner",
                   "For sex",
                   "To find a partner",
                   "To have a good time with an interesting person", 
                   "To find friends", 
                   "For fun"
                   ) %>% factor(., levels = .)
male.values <- c(.05, .12, .18, .2, .28, .41, .51)
female.values <- c(.05, .1, .05, .17, .18, .41, .43)

dating.data <- data.frame("Reason" = usage.reasons, "Male" = male.values, "Female" = female.values)
dating.df <- reshape2::melt(dating.data, id.vars = "Reason") %>% 
  magrittr::set_names(c("Reason", "Gender", "Value"))

# Reasons for online dating - ggplot
ggplot(data = dating.df, aes(x = Reason, y = Value, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(-.9)) +
  scale_x_discrete(labels = stringr::str_wrap(usage.reasons, width = 25), 
                   name = element_blank()) + 
  scale_y_continuous(name = element_blank()) + 
  geom_text(aes(x = Reason, y = Value, fill = Gender, 
                label = paste0(Value*100, "%")), 
            colour = "white", size = 3.3, fontface = "bold", 
            position = position_dodge(-.9), 
            hjust = +1.3, vjust = +.45) + 
  coord_flip() +
  scale_fill_manual(values = c(viridis::viridis(20)[11],
                               viridis::magma(20)[17]),
                    aesthetics = "fill") + 
  theme_minimal() +   
  theme(text = element_text(family = "Source Sans Pro"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        panel.grid = element_blank(), 
        axis.line.y.left = element_line(colour = "grey", size = 0.2)) + 
  ggtitle(label = "Reasons for online dating") +
  guides(guide_legend(reverse = TRUE)) 

# ---- lying online ----
# Reasons for lying while dating online - data
lie.reasons <- c("I was trying to catch my partner cheating on me", 
                 "I didn't want my current partner to recognize me", 
                 "I didn't want my friends or acquaintances to recognize me", 
                 "I wanted to look more interesting for potential matches", 
                 "I was afraid someone could misuse accurate information about me (blackmail, fraud)",
                 "For fun"
                 ) %>% factor(., levels = .)
male.values <- c(.03, .06, .15, .19, .2, .36)
female.values <- c(.03, .04, .15, .11, .34, .31)

lie.data <- data.frame("Reason" = lie.reasons, "Male" = male.values, "Female" = female.values)
lie.df <- reshape2::melt(lie.data, id.vars = "Reason") %>% 
  magrittr::set_names(c("Reason", "Gender", "Value"))

# Reasons for lying while dating online - ggplot
ggplot(data = lie.df, aes(x = Reason, y = Value, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(-.9)) +
  scale_x_discrete(labels = stringr::str_wrap(lie.reasons, width = 25), 
                   name = element_blank()) + 
  scale_y_continuous(name = element_blank()) + 
  geom_text(aes(x = Reason, y = Value, fill = Gender, 
                label = paste0(Value*100, "%")), 
            colour = "white", size = 3.3, fontface = "bold", 
            position = position_dodge(-.9), 
            hjust = +1.2, vjust = +.45) + 
  coord_flip() +
  scale_fill_manual(values = c(viridis::viridis(20)[11],
                               viridis::magma(20)[17]),
                    aesthetics = "fill") + 
  theme_minimal() + 
  theme(text = element_text(family = "Source Sans Pro"),
        plot.title = element_text(hjust = 0.4, face = "bold", size = 15),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        panel.grid = element_blank()) +
  ggtitle(label = "Reasons for lying online") +
  guides(guide_legend(reverse = TRUE))
