# install.packages("tidyverse") # Installera tidyverse
library(tidyverse)              # Ladda ett paket

library(gapminder)
gapminder                       # Skriv ut objektet gapminder

mean(gapminder$lifeExp, na.rm = T) # Beräkna medel av lifeExp

gapminder %>%
  filter(country == "Norway", year > 1972) %>%
  select(country, lifeExp)

dat_small <- gapminder %>% filter(year == 2007)
ggplot(dat_small, aes(x = gdpPercap, y = lifeExp, 
                      size = pop, color = continent)) +
  geom_point()

library(readxl)  # Ladda readxl

# gapminder <- read_excel("C:/Users/Name/Downloads/Gapminder.xlsx")
# # Läs in från en lokal excelfil
# gapminder

# getwd() # Ange working directory

# gapminder <- read_excel("Data/Gapminder.xlsx")
# # Läs in från en lokal excelfil (relativt wd)
# gapminder

# # install.packages("gapminder")
# library(gapminder)
# gapminder

gapminder <- gapminder %>%             # Ta datan, och sen
  mutate(gdp = gdpPercap * pop)        # Beräkna bnp

gapminder

gapminder <- gapminder %>% 
  mutate(`National GDP` = gdpPercap * pop)
gapminder

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, 
                      size = pop, color = continent)) +
  geom_point() +
  facet_wrap(~ year)

# g <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, size = pop,
#                            color = continent, text = country)) +
#   geom_point() +
#   facet_wrap(~ year)
# 
# # install.packages("plotly")  # Kör om ej installerat tidigare
# library(plotly)               # Ladda paketet plotly
# ggplotly(g)                   # Interaktiv graf g

gapminder$gdpPercap         # Vektorn med data
mean(gapminder$gdpPercap)   # Beräkna medelvärdet
median(gapminder$gdpPercap) # Beräkna medianen

gapminder %>%
  summarise(Mean = mean(gdpPercap),
            Median = median(gdpPercap))

gapminder %>%                           # Ta datan, och sen
  group_by(year) %>%                    # gruppera efter år, och
  summarise(Mean = mean(gdpPercap),     # beräkna medelvärde och
            Median = median(gdpPercap)) # medianen av gdpPercap

dat_gdp_2007 <- gapminder %>%       # Ta datan, och sen
  filter(year == 2007) %>%          # filtrera för 2007, och sen
  group_by(continent) %>%           # gruppera kontinenter, och
  summarise(Mean = mean(gdpPercap)) # beräkna medelvärdet

ggplot(dat_gdp_2007, aes(continent, Mean)) +
  geom_col()                  # Illustrera med kolumner (columns)

gapminder$gdpPercap      # Vektorn med data

var(gapminder$gdpPercap) # Beräkna varians
sd(gapminder$gdpPercap)  # Beräkna standardavvikelse
IQR(gapminder$gdpPercap) # Beräkna kvartilavstånd

gapminder %>%                       # Ta datan, och
  summarise(Var = var(gdpPercap),   # beräkna varians,
            Sd = sd(gdpPercap),     # standardavvikelse,
            IQR = IQR(gdpPercap))   # och kvartilavstånd

gapminder %>%
  group_by(year) %>%
  summarise(Varians = var(gdpPercap),
            Standardavvikelse = sd(gdpPercap),
            Kvartilavstånd = IQR(gdpPercap))

dat_year_continent <- gapminder %>%
  group_by(year, continent) %>%
  summarise(Mean = mean(gdpPercap),
            SE = sd(gdpPercap) / sqrt(n()))
dat_year_continent

ggplot(dat_year_continent, aes(x = year, y = Mean, 
                               color = continent)) +
  geom_line() +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE))

dat_2007 <- gapminder %>%          # Ta datan, och sen
  filter(year == 2007) %>%        # filtrera på år, och sen
  group_by(continent) %>%         # gruppera efter kontinent,
  summarise(Mean = mean(lifeExp), # summera med medelvärde,
            SD = sd(lifeExp))     # och standardavvikelse
dat_2007

ggplot(dat_2007, aes(continent, Mean, fill = continent)) +
  geom_col()+                                            
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD)) +
  labs(title = "Average life expectancy by continent, 2007",
       caption = "Errorbars given by mean +/- standard deviation.
       Source: Gapminder")

ggplot(gapminder, aes(year, lifeExp, fill = continent, 
                      group = year)) +
  geom_boxplot() +                                                     
  facet_wrap(~ continent)                                              

dat_2007 <- gapminder %>%         # Ta datan, och sen
  filter(year == 2007) %>%        # filtrera på år, och sen
  group_by(continent) %>%         # gruppera efter kontinent,
  summarise(Mean = mean(lifeExp), # summera med medelvärde,
            SD = sd(lifeExp))     # och standardavvikelse
dat_2007

# getwd()                          # Se nuvarande working directory
# write_csv(dat_2007, "Exporterad data från R.csv") # Exportera data

dat_dice <- data.frame(Utfall = c(6,3,2,3,5)) %>%
  mutate(Kast = 1:n())                           
dat_dice

dat_dice <- dat_dice %>%
  mutate(kumulativ_summa = cumsum(Utfall),  
         kumulativt_medelvärde = kumulativ_summa / Kast)
dat_dice

# ggplot(dat_dice, aes(x = Kast, y = ___)) +
#   ___()

# dat_dice <- data.frame(Utfall = c(___)) %>%
#   mutate(Kast = 1:n(),
#          kumulativ_summa = cumsum(Utfall),
#          kumulativt_medelvärde = kumulativ_summa / Kast)
# dat_dice

# dat_dice <- data.frame(Utfall = c(___)) %>%
#   mutate(Kast = 1:n(),
#          positivt_utfall = Utfall == 1,
#          kumulativt_antal = cumsum(positivt_utfall),
#          kumulativ_frekvens = kumulativt_antal / Kast)
# dat_dice
# 
# ggplot(dat_dice, aes(x = Kast, y = kumulativ_frekvens)) +
#   geom_line()

# library(plotly)

# dat_ex <- data.frame(Var1 = c(1,2,3), Var2 = c(3,1,2),
#                      Var3 = c(2,3,1), Type = c("A", "B", "C"))
# dat_ex
# 
# plot_ly(dat_ex, x = ~Var1, y = ~Var2,
#         z = ~Var3, color = ~Type) %>%
#   add_markers()

# dat_2007 <- gapminder %>%
#   filter(year == ___)

# plot_ly(data_2007, x = ~pop, y = ~___, z = ~___,
#         color = ~continent, text = ~country) %>%
#   add_markers()

# plot_ly(dat_2007, x = ~log10(pop), y = ~log10(___),
#         z = ~___, color = ~___, text = ~country) %>%
#   add_markers()

# plot_ly(dat_2007, x = ~log10(pop), y = ~log10(___),
#         z = ~___, color = ~continent, text = ~country) %>%
#   add_markers() %>%
#   add_lines(data = gapminder %>% filter(country == "Costa Rica"))

