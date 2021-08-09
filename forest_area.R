
# setup -------------------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(skimr)


# loading the data --------------------------------------------------------

tuesdata      <- tidytuesdayR::tt_load(2021, week = 15)

# we are only using forest_area for this plot
forest_area   <- tuesdata$forest_area


### path plot of forest_area from 1990 to 2020 in the EU



# selecting countries -----------------------------------------------------


eu <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", 
        "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", 
        "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", 
        "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")

# should the arrows all point in the same direction?
forest_area %>% 
  filter(entity %in% eu, year %in% c(1990,2020)) %>% 
  ggplot(aes(x = year, y = forest_area)) +
  geom_col() +
  facet_wrap(.~entity)

#' I think the area grew in every country
#' there is only 1 data point for: belgium, croatia, czechia, estonia, ireland, latvia, lithuania, slovakia and slovenia
#' there is no data for cyprus, malta, netherlands and luxembourg
#' -> I am omitting all of these countries 

eu_with_data <- c("Austria", "Bulgaria", "Denmark", "Finland", "France", "Germany", 
                  "Greece", "Hungary", "Italy", "Poland", "Portugal", 
                  "Romania", "Spain", "Sweden")

# should the arrows all point in the same direction? new country vector
forest_area %>% 
  filter(entity %in% eu_with_data, year %in% c(1990,2020)) %>%
  ggplot(aes(x = year, y = forest_area)) +
  geom_col() +
  facet_wrap(.~entity)
# forest area grew in all of these countries from 1990 to 2020 (I also checked the numbers)



# plot --------------------------------------------------------------------

forest_area %>% 
  filter(entity %in% eu_with_data, year %in% c(1990, 2020)) %>% 
  mutate(forest_area = forest_area * 100,
         entity = fct_reorder(entity, forest_area, median)) %>% 
  ggplot() +
  geom_path(aes(x = forest_area, y = entity),
            arrow = arrow(type = "open", 
                          length = unit(1.5, "mm"))) +
  geom_text(aes(x = forest_area, y = entity, 
                label = round(forest_area, 2),
                hjust = ifelse(year == "1990", 1.5, -0.6)),
            size = 3,
            family = "serif",
            color = "gray25") +
  coord_cartesian(xlim = c(-5, 80)) +
  labs(x = "Forest Area",
       y = "",
       title = "Forest Area in Selected European Countries",
       subtitle = "in Percent (1990 - 2020)",
       caption = "Source: Our World in Data") +
  theme_minimal()
