# Title: Educational attainment of young people in English towns
# Date created: 23 January 2024

# Load libraries -------------------------------------------------------------------------------------
library(readr)
library(dplyr)
library(ggplot2)

# Load in data ---------------------------------------------------------------------------------------
english_education <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-23/english_education.csv')

# Local variables ------------------------------------------------------------------------------------
maroon <- "#871A5B"
blue <- "#206095"
grey <- "#A09FA0"
primary_text <- "#222222"
labels_grey <- "#414042"
axis_label_source_grey <- "#707071"
grid_tick_grey <- "#D9D9D9"


# Data wrangling -------------------------------------------------------------------------------------
df <- na.omit(english_education[c("size_flag", "income_flag")])
df$deprivation_indicator <- case_match(df$income_flag,
                                       c("Cities", "Higher deprivation towns") ~ "Higher income deprivation",
                                       c("Mid deprivation towns") ~ "Mid income deprivation",
                                       c("Lower deprivation towns") ~ "Lower income deprivation")
df$town_size <- case_match(df$size_flag,
                           c("City") ~ "Cities",
                           c("Large Towns") ~ "Large towns",
                           c("Medium Towns") ~ "Medium towns",
                           c("Small Towns") ~ "Small towns")
plotting_df <- as.data.frame(table(df$town_size, df$deprivation_indicator))
names(plotting_df) <- c("Size", "Deprivation", "Count")
plotting_df$Deprivation <- factor(plotting_df$Deprivation, levels = c("Lower income deprivation", "Mid income deprivation", "Higher income deprivation"))

# Create plot ----------------------------------------------------------------------------------------
ggplot(plotting_df, aes(fill = Deprivation, color = Deprivation, y = Size, x = Count)) + 
geom_bar(position = "fill", stat = "identity", width = 0.7, key_glyph = draw_key_point) + 
scale_x_continuous(labels = function(x) paste0(x*100), breaks = seq(0, 1, 0.2)) + 
scale_color_manual(values = c(blue, grey, maroon)) + scale_fill_manual(values = c(blue, grey, maroon)) + 
labs(x = "", 
     y = "",
     caption = "Source: Office for National Statistics analysis using Longitudinal Educational Outcomes (LEO)\nfrom the Department for Education (DfE)", 
     title =  "Income deprivation group by town size, England") + theme_void() + 
theme(legend.position = "top", 
      legend.justification='left',
      legend.box = "horizontal",
      legend.margin = margin(0),
      legend.title = element_blank(),
      legend.text = element_text(size = 13, color = labels_grey, family = "open sans,Helvetica,Arial,sans-serif"),
      axis.text.y = element_text(size = 13, color = labels_grey, family = "open sans,Helvetica,Arial,sans-serif", hjust=1),
      axis.text.x = element_text(size = 13, color = axis_label_source_grey, family = "open sans,Helvetica,Arial,sans-serif", vjust=-1),
      plot.title =  element_text(size = 13, vjust= 6, color = primary_text, family = "open sans,Helvetica,Arial,sans-serif"),
      plot.caption = element_text(size = 13, vjust = -4, hjust = 0, color = axis_label_source_grey, family = "open sans,Helvetica,Arial,sans-serif"),
      plot.caption.position = "plot",
      plot.title.position = "plot",
      aspect.ratio = 1/3,
      panel.grid.major.x = element_line(color = grid_tick_grey,
                                                size = 0.5,
                                                linetype = 1),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "inches")
      ) + 
geom_vline(xintercept = 0, linetype = 1, color = grid_tick_grey, linewidth = 0.75) + 
guides(color = guide_legend(reverse = TRUE, keywidth = unit(10, units="mm")), fill = FALSE)
