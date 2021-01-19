library(tidytuesdayR)
library(tidyverse)
library(extrafont)
library(janitor)
library(ggtext)

# LOAD DATA ---------------------------------------------------------------

tt <- tidytuesdayR::tt_load("2021", week = 3)

artwork <- tt$artwork %>% clean_names()
artists <- tt$artists %>% clean_names()

# "Here we present the metadata for around 70,000 artworks that Tate owns or jointly owns with the National Galleries of Scotland as part of ARTIST ROOMS. Metadata for around 3,500 associated artists is also included."

extrafont::loadfonts()

# CLEAN -------------------------------------------------------------------



# EXPLORE -----------------------------------------------------------------

artwork %>% 
  ggplot(aes(acquisition_year)) + 
  geom_histogram()

# There's a huge chunk of art acquired in 1856.  The Tate didn't open until 1897 (https://www.tate.org.uk/about-us/history-tate). These appear to come from the "Turner Bequest" and the credit_line reads that they were given to the nation (and then made their way somehow to this collection?) 

artwork %>% 
  filter(acquisition_year < 1875) %>% 
  count(acquisition_year, sort = TRUE)


artwork %>% 
  filter(acquisition_year > 1875) %>% 
  ggplot(aes(acquisition_year)) +
  geom_histogram()

artwork %>% glimpse()



# mostly "artist" roles, some other oddities
count(artwork, artist_role, sort = TRUE) %>% 
  mutate(pct = n / sum(n))

# mix of artwork (sculpture, drawings, paintings, etc.)
# oddly, some works that have dimensions do not have data for width, height, depth, units
artwork %>% distinct(units)
artwork %>% count(units)
artwork %>% filter(is.na(units))  

artwork %>% 
  mutate(credit_line_simple = case_when(str_detect(credit_line, regex("purchased", ignore_case = TRUE)) ~ "purchased", 
                                        str_detect(credit_line, regex("turner bequest 1856", ignore_case = TRUE)) ~ "turner bequest 1856",
                                        str_detect(credit_line, regex("bequeathed", ignore_case = TRUE)) ~ "bequeathed",
                                        str_detect(credit_line, regex("presented", ignore_case = TRUE)) ~ "presented",
                                        str_detect(credit_line, regex("exchange", ignore_case = TRUE)) ~ "exchanged", 
                                        str_detect(credit_line, regex("in lieu of.*tax", ignore_case = TRUE)) ~ "lieu tax",
                                        str_detect(credit_line, regex("transferred", ignore_case = TRUE)) ~ "transferred",
                                        TRUE ~ credit_line)) %>% 
  count(credit_line_simple, sort = TRUE) %>% print(n = 25)

  

artwork %>% 
  filter(is.na(units), 
         !is.na(dimensions), 
         !str_detect(dimensions, regex("variable", ignore_case = TRUE))) %>% 
  relocate(dimensions) 

# videos/films
artwork %>% 
  filter(str_detect(dimensions, regex("duration", ignore_case = TRUE))) %>% 
  ggplot(aes(acquisition_year)) +
  geom_histogram()

# photos
artwork %>% 
  filter(str_detect(medium, regex("photo", ignore_case = TRUE))) %>% 
  ggplot(aes(acquisition_year)) +
  geom_histogram()

# watercolors
artwork %>% 
  filter(str_detect(medium, regex("watercol", ignore_case = TRUE))) %>% 
  ggplot(aes(acquisition_year)) + 
  geom_histogram()

# does size indicate medium? (nope)
artwork <- 
  artwork %>% 
  filter(!is.na(medium)) %>%
  mutate(medium_simple = case_when(str_detect(medium, regex("paint.*canvas", ignore_case = TRUE)) ~ "painting",
                                   str_detect(medium, regex("oil paint|tempera", ignore_case = TRUE)) ~ "painting", 
                                   str_detect(medium, regex("(screenprint|lithograph|etching|intaglio|engrav|print|aquatint|linocut|monotype|woodcut).*on paper", ignore_case = TRUE)) ~ "print",
                                   str_detect(medium, regex("watercol|gouache", ignore_case = TRUE)) ~ "watercolor", 
                                   str_detect(medium, regex("(graphite|ink|chalk|pen|charcoal|pastel|crayon).*on paper", ignore_case = TRUE)) ~ "drawing", 
                                   str_detect(medium, regex("photo", ignore_case = TRUE)) ~ "photograph", 
                                   TRUE ~ "other")) 

# PLOT -------------------------------------------------------------

artwork %>% 
  filter(medium_simple != "other", 
         !is.na(dimensions), 
         !is.na(width), 
         !is.na(height)) %>% 
  # remove outliers over 1.5x IQR above 75th percentile
  filter(width <= (1.5 * IQR(width)) + quantile(width, 0.75), 
         height <= (1.5 * IQR(height)) + quantile(height, 0.75)) %>% 
  ggplot(aes(width, height, color = medium_simple)) +
  geom_count(alpha = 1/10) +
  facet_wrap(~ medium_simple, nrow = 2)



max(artwork$acquisition_year[!is.na(artwork$acquisition_year)])

artwork %>% 
  # filter(acquisition_year >= 1860) %>% 
  # mutate(year_group = (acquisition_year %/% 10) * 10) %>% 
  filter(!is.na(year)) %>% 
  mutate(year_group = (year %/% 10) * 10) %>% 
  count(medium_simple, year_group) %>% 
  complete(year_group, medium_simple, fill = list(n = 0)) %>% 
  mutate(medium_simple = factor(medium_simple)) %>% 
  ggplot(aes(year_group, n, fill = fct_reorder(medium_simple, n, max))) + 
  geom_area(position = "fill") + 
  ggsci::scale_fill_npg()


artwork %>% 
  count(artist_id, sort = TRUE) %>% 
  left_join(artists, by = c("artist_id" = "id")) %>% 
  filter(!is.na(year_of_birth), 
         artist_id != 558) %>% 
  mutate(yr_group = (year_of_birth %/% 10) * 10) %>% 
  count(yr_group, wt = n) %>%
  mutate(pct = n / sum(n)) %>% 
  ggplot(aes(yr_group, pct)) + 
  geom_line(size = 1) + 
  geom_point(size = 1.5, shape = 21, fill = "white") + 
  scale_x_continuous(breaks = seq(1500, 2020, 50), 
                     minor_breaks = seq(1500, 2020, 10), 
                     expand = expansion(mult = c(.01, .025))) + 
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(0, 1, .05), 
                     expand = expansion(mult = c(0, .1))) + 
  theme(text = element_text(family = "Corbel"), 
        axis.text = element_text(family = "Consolas"), 
        plot.caption = element_text(size = 8, family = "Corbel Light"), 
        panel.background = element_blank(), 
        panel.grid.major = element_line(color = "grey", size = 0.25), 
        panel.grid.minor.y = element_line(color = "light grey", size = 0.1), 
        plot.subtitle = element_markdown(family = "Corbel Light", size = 13)) +
  labs(y = "% of Collection", 
       x = "Artist Birth Decade", 
       title = "The Tate Gallery Collection Skews Towards Artists Born Early 20th Century", 
       subtitle = "<br>A full **15%** of the collection is from artists born in the 1930s<br><br><span style='font-size:9pt'>The largest portion of the entire collection is by artist Joseph Mallord William Turner, born in 1775, and accounts for 53% of the total collection.<br>That collection removed from analysis here as it is an outlier and buries other trends.</span>", 
       caption = "Plot by @greufek\nData from Tate Gallery Collection (github.com/tategallery/collection)\nvia Tidy Tuesday (github.com/rfordatascience/tidytuesday)") 




#ignoring Joseph Turner who donated a BUNCH of artwork in 1856 that skews that data to 1770 birth decade
# 53% of all pieces are attributed to this artist.
artwork %>% 
  count(artist_id, sort = TRUE) %>% 
  mutate(pct = n / sum(n))
artists %>% filter(id == 558) 

# ggsave(filename = "21-0112_Art_Collections/21-0112_tate_collection.jpeg", dpi = "retina", device = "jpeg")