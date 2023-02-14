library(tidyverse)
gapminder_data = read.csv("gapminder_data.csv")
summarize(gapminder_data , averageLifeExp = mean(lifeExp))
gapminder_data_summarized = gapminder_data %>% 
  summarize(averageLifeExp = mean(lifeExp))
#overwrite gapminder_data#
gapminder_data_summarized = gapminder_data %>% 
  summarize(averageLifeExp = mean(lifeExp))

#practice 1#
gapminder_data_summarized = gapminder_data %>% 
  summarize(averagelifeExp = mean(lifeExp) , recent_year = max(year))

gapminder_data %>% 
  filter(year == 2007) %>% 
  summarize(average = mean(lifeExp))

#practice 2#
gapminder_data %>% 
  filter(year == min(year)) %>% 
  summarize(averageGDP = mean(gdpPercap))

gapminder_data %>% 
  group_by(year) %>% 
  summarize(average = mean(lifeExp))

#practice 3#
gapminder_data %>% 
  group_by(continent) %>% 
  summarize(average = mean(lifeExp))

gapminder_data %>% 
  group_by(continent) %>% 
  summarize(average = mean(lifeExp) , min = min(lifeExp))

gapminder_data %>% 
  mutate(gdp = pop * gdpPercap)

gapminder_data %>% 
  mutate(gdp = pop * gdpPercap , popInMillions = pop/1000000)

gapminder_data %>% 
  select(pop , year)

gapminder_data %>% 
  select(-continent)

#practice 4#
gapminder_data_new = gapminder_data %>% 
  select(country , continent , year , lifeExp)

gapminder_data %>%
  select(year, starts_with("c"))

gapminder_data %>%
  select(ends_with("p"))

gapminder_data %>% 
  select(country , continent , year , lifeExp) %>% 
  pivot_wider(names_from = year , values_from = lifeExp)

#practice 5#
gapminder_data_2007 = gapminder_data %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-year , -continent)

co2_emissions_dirty <- read_csv("co2-un-data.csv", skip=2,
                                col_names=c("region", "country", "year", "series", "value", "footnotes", "source"))

co2_emissions_dirty %>%
  select(country, year, series, value)

co2_emissions_dirty %>% 
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  count(year)

co2_emissions_dirty %>% 
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year)

co2_emission = co2_emissions_dirty %>% 
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year)
  
gapminder_data_2007 = gapminder_data %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-year , -continent)

inner_join(gapminder_data , co2_emission , by = "country")

anti_join(gapminder_data , co2_emission , by = "country")

co2_emissions <- read_csv("co2-un-data.csv", skip=2,
                          col_names=c("region", "country", "year",
                                      "series", "value", "footnotes", "source")) %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita")) %>%
  pivot_wider(names_from=series, values_from=value) %>%
  filter(year==2005) %>%
  select(-year) %>%
  mutate(country=recode(country,
                        "Bolivia (Plurin. State of)" = "Bolivia",
                        "United States of America" = "United States",
                        "Venezuela (Boliv. Rep. of)" = "Venezuela")
  )

anti_join(gapminder_data, co2_emissions, by="country")

gapminder_data <- read_csv("gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent) %>%
  mutate(country = recode(country, "Puerto Rico" = "United States"))


gapminder_data <- read_csv("gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent) %>%
  mutate(country = recode(country, "Puerto Rico" = "United States")) %>%
  group_by(country) %>%
  summarize(lifeExp = sum(lifeExp * pop)/sum(pop),
            gdpPercap = sum(gdpPercap * pop)/sum(pop),
            pop = sum(pop)
)

anti_join(gapminder_data, co2_emissions, by="country")

gapminder_co2 <- inner_join(gapminder_data, co2_emissions, by="country")

gapminder_co2 %>%  
  mutate(region = if_else(country == "Canada" | country == "United States" | country == "Mexico", "north", "south"))

write_csv(gapminder_co2, "gapminder_co2.csv")

ggplot(gapminder_co2, aes(x=gdpPercap, y=per_capita)) +
  geom_point() +
  labs(x="GDP (per capita)",
       y="CO2 emitted (per capita)",
       title="There is a strong association between a nation's GDP \nand the amount of CO2 it produces"
  )

ggplot(gapminder_co2, aes(x=gdpPercap, y=per_capita)) +
  geom_point() +
  labs(x="GDP (per capita)",
       y="CO2 emitted (per capita)",
       title="There is a strong association between a nation's GDP \nand the amount of CO2 it produces"
  ) +
  geom_smooth()

ggplot(gapminder_co2, aes(x=gdpPercap, y=per_capita)) +
  geom_point() +
  labs(x="GDP (per capita)",
       y="CO2 emitted (per capita)",
       title="There is a strong association between a nation's GDP \nand the amount of CO2 it produces"
  ) +
  geom_smooth(method="lm")