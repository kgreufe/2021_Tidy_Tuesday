library(tidyverse)
library(tidytuesdayR)
library(DescTools)
library(ggridges)
library(extrafont)

# LOAD DATA ---------------------------------------------------------------

tt <- tidytuesdayR::tt_load("2021", week = 2)

transit_cost <- tt$transit_cost

# adding miles, because it makes more sense to me (approximate rounding to two decimals)
transit_cost <- 
  transit_cost %>% 
  mutate(length_miles = length * 0.62, 
         tunnel_miles = tunnel * 0.62, 
         real_cost = parse_number(real_cost),
         start_year = parse_number(start_year), 
         end_year = parse_number(end_year),
         cost_mi_millions = real_cost / length_miles, 
         flag_us = if_else(country == "US", TRUE, FALSE)) %>% 
  # also, remove weird rows (appear to be summary calculations for dataset)
  filter(!is.na(e))


# EXPLORE -----------------------------------------------------------------

# data here is from all over the world, but not entirely sure where they put the limits on data collection, or whether this data set is still getting updated.  There's a bunch of data in China/Asia and Europe but sparse data in the Americas and only one data point in Africa, which seems odd. Denver, for example, had a commuter rail line (A Line) open in 2016 that is not in this data set.  All this to say, this data is probably useful, but incomplete. 

transit_cost %>% filter(country == "US") %>% pull("city") %>% unique()

# Around 80% of the rows are for projects in East Asia, Central Asia, Europe, or the Pacific. Only around 4% of rows are for projects in North America which makes me a bit leery to make broad comparisons.  It may be that there are truly that many more projects in those regions comparatively, but I do also think this dataset is missing data for some regions and projects around the world so I'm a bit uncomfortable making regional comparisons on limited data. 

transit_cost %>% 
  left_join(select(DescTools::d.countries, a2, name, region), by = c("country" = "a2")) %>% 
  count(region) %>% 
  mutate(pct = n / sum(n))

# Maybe this data only includes rail projects that have tunnels???

transit_cost %>% 
  filter(parse_number(tunnel_per) == 100, 
         !near(round(length, 1), round(tunnel, 1))) 

# I don't think this is clear from their website or from the Tidy Tuesday documentation, but it appears to me that this data is only SPECIFICALLY for tunnel construction rail projects. Maybe this is what is meant when the Transit Costs Projects says it's data for "urban rail"? This would help explain what I perceived to be gaps in the data. 


# PLOT -------------------------------------------------------------

transit_cost %>% 
  inner_join(DescTools::d.countries, by = c("country" = "a2")) %>% 
  filter(!is.na(region)) %>% 
  mutate(region_split = if_else(str_detect(region, regex("north america", ignore_case = TRUE)), name, region), 
         region_split = fct_reorder(factor(region_split), cost_mi_millions, mean, .desc = TRUE)) %>% 
  ggplot(aes(cost_mi_millions / 1000, region_split)) + 
  geom_density_ridges(aes(fill = region_split)) + 
  theme(legend.position = "none") + 
  ggsci::scale_fill_nejm() + 
  scale_x_continuous(minor_breaks = NULL, breaks = seq(0, 10, 1)) + 
  labs(x = "Cost Per Mile (Billions US$)", 
       y = NULL, 
       title = "Urban Rail Tunnel Projects By Region", 
       subtitle = "\nThe United States spends a significantly higher amount per mile on building new urban rail systems \ncompared to the rest of the world -- it's the only location that has any projects costing over\n$2 Billion USD per mile constructed.\n\nNorth America region split here between Canada and the United States to highlight this difference.\n", 
       caption = "Data from Transit Cost Project (https://transitcosts.com/)\nvia Tidy Tuesday (https://github.com/rfordatascience/tidytuesday)\nPlot by @greufek") +
  theme(text = element_text(family = "Segoe UI Light"), 
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10.5), 
        axis.text = element_text(color = "black"),
        axis.title.x = element_text(face = "bold"), 
        panel.background =  element_blank())


# ggsave("21-0105_Transit_Costs.jpeg", dpi = "retina")
