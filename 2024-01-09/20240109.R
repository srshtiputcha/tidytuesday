# Title: Canadian Hockey Player Birth Months
# Date created: 08 January 2024

# Load libraries -------------------------------------------------------------------------------------
library(readr)
library(dplyr)
library(ggplot2)

# Load in data ---------------------------------------------------------------------------------------
canada_births_1991_2022 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/canada_births_1991_2022.csv')
nhl_player_births <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_player_births.csv')
nhl_rosters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_rosters.csv')
nhl_teams <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_teams.csv')

# Local variables ------------------------------------------------------------------------------------
red <- "#bf3f3f"
dark_red <- "#C02C38"
blue <- "#293276"
grey <- "#999999"

# Data wrangling -------------------------------------------------------------------------------------
genpop_birth_months <- canada_births_1991_2022 %>% group_by(month) %>%
                       summarise(total_births = sum(births))
genpop_birth_months$prop_total_births <- genpop_birth_months$total_births / sum(genpop_birth_months$total_births)
genpop_birth_months$month <- month.abb
genpop_birth_months$month <- factor(genpop_birth_months$month, levels = unique(genpop_birth_months$month))
genpop_birth_months$data_id <- "Canadian population\n(birth years: 1991 - 2022)\n"

canadian_nhl_players <- nhl_player_births %>% filter(grepl('CAN', birth_country))
canadian_nhl_birth_months <- as.data.frame(table(canadian_nhl_players$birth_month))
names(canadian_nhl_birth_months) <- c("month", "total_births")
canadian_nhl_birth_months$month <- month.abb
canadian_nhl_birth_months$month <- factor(canadian_nhl_birth_months$month, levels = unique(canadian_nhl_birth_months$month))
canadian_nhl_birth_months$prop_total_births <- canadian_nhl_birth_months$total_births / sum(canadian_nhl_birth_months$total_births)
canadian_nhl_birth_months$data_id <- "Canadian NHL players\n(birth years: 1879 - 2005)\n"

merged_df = merge(x = genpop_birth_months, y = canadian_nhl_birth_months, all = TRUE)
rm(canada_births_1991_2022, nhl_player_births, nhl_rosters, nhl_teams, genpop_birth_months, canadian_nhl_players, canadian_nhl_birth_months)

# Create plot ----------------------------------------------------------------------------------------
annotation <- data.frame(
  x = c(1/12+0.015),
  y = c(2.5),
  label = c("Equal likelihood")
)

ggplot() +
  geom_col(data = merged_df, 
         aes(x = prop_total_births, 
             y = reorder(month, desc(month)), 
             fill = data_id, 
             color = data_id), 
         width = 0.5, 
         position = "dodge") + 
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.3))) +
  labs(x = "Historical proportion of individuals", y = "Birth month") + 
  labs(caption = "S. Putcha | Sources: Statistics Canada, NHL team list endpoint, NHL API",
       title =  "🇨🇦 Are Canadian hockey players born earlier in the year? 🇨🇦",
       subtitle = "Compared to modern Canadian birth patterns, it seems that NHL players\nwere historically more likely to be born in the winter and spring." ) +
  theme_void() +
  theme(
    axis.text.y = element_text(size = 10, hjust = 1, color = blue, family = "Helvetica"),
    axis.text.x =  element_text(size = 10, hjust = 1, color = blue, family = "Helvetica", ),
    plot.title = element_text(size = 11, vjust = 2, color = blue, family = "Helvetica", face = "bold"),
    plot.subtitle = element_text(size = 9, vjust = 2, color = blue, family = "Helvetica"),
    plot.caption = element_text(size = 8, vjust =  -5, color = blue, family = "Helvetica"),
    plot.margin = margin(rep(15, 4)),
    axis.title.x =  element_text(size = 10, color = blue, vjust = -3, hjust = 0.3, family = "Helvetica-Narrow"),
    axis.title.y =  element_text(size = 10, angle = 90, color = blue, vjust = 3, family = "Helvetica-Narrow"),
    legend.text = element_text(size = 8, family = "Helvetica", color = blue),
    legend.title = element_blank()
    ) +  
  geom_vline(xintercept = 1/12, linetype = "twodash", color = blue,linewidth = 0.75) +
  geom_label(data = annotation, 
             aes(x = x, y = y, label = label), 
             colour = blue, 
             fontface = "italic", 
             size = 3, 
             angle = 45) + 
  scale_fill_manual(values = c(grey, red)) + scale_color_manual(values = c("black", dark_red))
