rm(list=ls())

# =========================================================
# preliminaries
# =========================================================
pacman::p_load(tidyverse,
               magrittr,
               ggplot2,
               extrafont)


# Define function to subset using 'not in', similarly to 'in' (%in%)
'%!in%' <- function(x,y)!('%in%'(x,y))

# Set R language to english
Sys.setenv(LANG = "en")

# define function for scaling axis labels
scaleFUN <- function(x) sprintf("%.2f", x)

# define scientific notation
options(scipen=10000)

# define function for cutting a character variable from the right
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# import fonts for ggplot template (the ttf folder in 'Data and code')
font_import(paths = c("ttf"),
            #pattern = "lmroman*",
            pattern = "lmroman10-regular-webfont",
            prompt = F)

# =========================================================
# create ggplot templates for a unified visual style
# =========================================================
theme_list <- list()

# baseline
theme_list$theme_anatem <- theme_bw() +
  theme(#axis.line = element_blank(),
        #panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.title.y = element_text(size = 12,
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = 12,
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "LM Roman Caps 10"),
        legend.position = "bottom",
        strip.background =element_rect(fill="white"))

# for maps
theme_list$theme_anatem_map <- theme_classic() +
  theme(axis.title.y = element_text(size = 12,
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = 12,
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        text = element_text(family = "LM Roman Caps 10"),
        legend.position = "bottom")

# for ggplot with facet_wrap
theme_list$theme_anatem_facet <- theme_bw() +
  theme(axis.title.y = element_text(size = 12,
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = 12,
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        text = element_text(family = "LM Roman Caps 10"),
        legend.position = "bottom")

# for 'A Basket of Apples paper'
theme_list$theme_apples <- theme_classic() +
  theme(#axis.line = element_blank(),
    #panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.y = element_text(size = 12,
                                margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(size = 12,
                                margin = margin(t = 20, r = 0, b = 0, l = 0)),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.title = element_blank(),
    axis.ticks = element_blank(),
    text = element_text(family = "LM Roman Caps 10"),
    legend.position = "right")

# =========================================================
# check that fonts are working
# =========================================================
data.frame(a = rnorm(100, 1, 1),
           b = rnorm(100, 2, 2)) %>% 
  ggplot(., aes(x = a, y = b)) +
  geom_point() +
  theme_list$theme_anatem

# clear plot
dev.off()

# set working directory
raw_wd <- getwd()
setwd(paste0(raw_wd,
             "/Dropbox/Under Empire/Data and code"))
getwd()

# set path to dropbox
# dropbox_path <- paste0(raw_wd,
#                        "/Dropbox")
# 
# dropbox_path

# clean up
rm(raw_wd)

