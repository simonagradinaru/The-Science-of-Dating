library(dplyr)
library(magrittr)
library(ggplot2)

# ---- Create color palette ----

# Simona corporate colors
simona_colors <- c(
  `juneberry`   = "#854158",
  `purewhite`  = "#edece6",
  `erospink`   = "#c84f68",
  `gambolgold` = "#e1b047",
  `oceanside`   = "#015a6b",
  `rockcandy`  = "#dee1df",
  `auric`       = "#c48919",
  `aquarium`    = "#3aa9ae",
  `caviar`      = "#313031")

# Function to extract simona colors as hex codes
#' @param ... Character names of drsimonj_colors 
simona_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (simona_colors)
  simona_colors[cols]
}

# Combine colors into palettes 
simona_palettes <- list(
  `main`  = simona_cols("juneberry", "aquarium", "gambolgold", "rockcandy", "erospink", "oceanside", "auric")
)

# Return function to interpolate a drsimonj color palette
#' @param palette Character name of palette in drsimonj_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
simona_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- simona_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}
simona_pal("main")(10)

# Color scale constructor for drsimonj colors
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
scale_color_simona <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- simona_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("simona_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

# Fill scale constructor for drsimonj colors
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
scale_fill_simona <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- simona_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("simona_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}



# ---- First chart ----
# Label the odds in chart
label.odds <- c("Meeting people via online dating site"
                ,"Meeting colleagues and friends of colleagues"
                ,"Talking to people at the gym"
                ,"Meeting people through shared hobbies/societies"
                ,"Talking to people at clubs/bars"
                ,"Meeting friends of friends"
                ,"Being set up by family members"
                ,"Fate only"
                )

# Standard fate odd
odds <- 1/562 * 365

# Increased odds by action undertaken
all.odds <- c(odds * 1.17
              , odds * 1.16
              , odds * 1.15
              , odds * 1.11
              , odds * 1.09
              , odds * 1.04
              , odds * 1.01
              , odds) %>%
  set_names( label.odds )

# Numeric labels for odds
label_num.odds <- c(paste0("+", 100 * round(all.odds[-8] - odds, digits = 3)), round(100 * odds, digits = 1)) %>% paste0("%")
x.val <- 1:length( all.odds )
data.df <- data.frame( as.factor(x.val), all.odds, label.odds ) %>%
  set_names( c("x", "odds", "label") )

# Create chart
ggplot(data = data.df, aes(x = x, y = odds, fill = x)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  # scale_fill_viridis_d(direction = -1, alpha = 0.95, option = "D") +
  scale_fill_manual(values = viridis::viridis(9)[-9] %>% rev(), aesthetics = "fill") + 
  coord_flip() +
  ggtitle("Actions to increase your odds of finding love in an year") + 
  geom_text( aes( label = label.odds ), colour = "white", hjust = 1.1, fontface = "bold") + 
  geom_text( aes ( label = label_num.odds ), hjust = -0.1) +
  theme_void() +
  scale_y_continuous(name = "Odds of finding love in 365 days", limits = c(0, 0.81)) +
  theme(plot.title = element_text(hjust = 0.4, face = "bold"))
   




# Configure Theme
# kobe_theme <- function() {
#   theme(
#     plot.background = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
#     panel.background = element_rect(fill = "#E2E2E3"),
#     panel.background = element_rect(fill = "white"),
#     axis.text = element_text(colour = "#E7A922", family = "Impact"),
#     plot.title = element_text(colour = "#552683", face = "bold", size = 18, vjust = 1, family = "Impact"),
#     axis.title = element_text(colour = "#552683", face = "bold", size = 13, family = "Impact"),
#     panel.grid.major.x = element_line(colour = "#E7A922"),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     strip.text = element_text(family = "Impact", colour = "white"),
#     strip.background = element_rect(fill = "#E7A922"),
#     axis.ticks = element_line(colour = "#E7A922")
#   )
# }

