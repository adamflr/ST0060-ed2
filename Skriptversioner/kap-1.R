library(tidyverse)
library(gapminder)


a <- 5


# install.packages("tidyverse")


# install.packages("___")


library(tidyverse)


# library(___)


# install.packages("ThisIsNotTheNameOfAnyPackage")


# library(ThisIsNotTheNameOfAnyPackage)


a <- 5


b <- c(3, 1, 4, 1, 5, 9)


b[3]               # Det tredje värdet i vektorn b
b[c(3,5)]          # Det tredje och femte värdet i b


# new_vector <- c(_, _, _)


sum(b)


# plot(b)


sqrt(b)


# sum(___)


b <- c(3, 1, 4, 1, 5, 9, NA)  # Lägger till ett saknat värde
sum(b)                        # na.rm = FALSE är grundinställning
sum(b, na.rm = TRUE)          # na.rm sätts till TRUE


c <- c(-4, -2, -1, 1, 2, 4)   # Skapa en vektor av värden
c_absolute <- abs(c)          # Ta absolutvärden. Skapa c_absolut
sum(c_absolute)               # Summera värden i c_absolut


# new_vector_squared <- new_vector^2 # Ta kvadraten av varje värde
# new_vector_squared_sum <- sum(___) # Summera kvadraterna
# sqrt(___)                          # Ta kvadratroten ur summan


# sum(abs(c(-4, -2, -1, 1, 2, 4)))
# # Ta summan av absolutvärden av vektorn


# sqrt(sum(new_vector^2)) # Ta roten ur summan av vektorn i kvadrat


library(tidyverse)

c(-4, -2, -1, 1, 2, 4) %>%        # Skapa en datamängd och sen
  abs() %>%                       # ta absolutvärden, och sen
  sum()                           # beräkna summan.


# new_vector^2 %>% # Ta kvadraterna av new_vector, och sen
#   ___() %>%      # beräkna summan, och sen
#   ____()         # Ta kvadratroten med sqrt()


dat <- data.frame(Vecka = c(7,7,7,7,7,7,
                            11,11,11,11,11,11),
                  Behandling = c("A","A","A","B","B","B",
                                 "A","A","A","B","B","B"),
                  Vikt = c(232,161,148,368,218,257,
                           1633,2213,972,2560,2430,855),
                  N = c(2.63,2.90,2.99,3.54,3.30,2.85,
                        1.53,1.90,NA,2.58,NA,NA))
dat


# dat_dice <- data.frame(Kast = c(1,2,3,4,5,6,7,8,9,10),
#                        Utfall = c(_,_,_,_,_,_,_,_,_,_))
# dat_dice


dat$Vikt


# dat[2,3]          # Andra raden och tredje kolumnen
# dat[2, ]          # Tomt värde ger alla värden.
# dat[ ,3]          # Alla värden i kolumn 3


# dat_dice$___


mean(dat$Vikt)
sd(dat$Vikt)


# plot(dat$Vecka, dat$Vikt)


# plot(dat_dice$___, dat_dice$___)


gapminder %>%                     # Ta gapminder-datan och sen
  filter(country == "Sweden")     # filtrera för ett land


# gapminder %>%                     # Ta gapminder-datan och sen
#   filter(country == "Sweden")     # filtrera för ett land


# gapminder %>%
#   filter(country %in% c("Sweden", "Finland"))


# gapminder %>%                                 # Ta datan, och sen
#   filter(country %in% c("Sweden", "Finland"), # filtrera för land
#          year == 2002)                        # och för år


gapminder %>%
  filter(country %in% c("Sweden", "Finland")) %>%
  print(n = 5)


gapminder %>%                   # Ta datan, och sen
  select(country, lifeExp)      # välj kolumner


# gapminder %>%
#   filter(continent == "Europe", year == 1962) %>%
#   select(country, lifeExp, pop) %>%
#   arrange(-pop)


gm_2002 <- gapminder %>% filter(year == 2002)


# plot(gm_2002$lifeExp, gm_2002$gdpPercap)


ggplot(gm_2002, aes(x = lifeExp, y = gdpPercap)) + 
  geom_point()


ggplot(gm_2002, aes(x = lifeExp, y = gdpPercap, 
                    color = continent, size = pop)) +
  geom_point() +
  facet_wrap(~ continent)


# ggplot(gm_2002, aes(x = ____, y = ____)) +
#   geom_point() +
#   facet_wrap(~ continent)


dat_small <- gapminder %>% 
  filter(continent == "Europe", year == "2002")

ggplot(dat_small, aes(pop, country, fill = gdpPercap)) +
  geom_col(color = "black")


dat_small <- gapminder %>% 
  filter(year == 2002)

ggplot(dat_small, aes(x = lifeExp, y = continent)) + 
  geom_boxplot()



ggplot(dat_small, aes(lifeExp, continent)) +
  geom_boxplot(fill = "lightblue") +
  theme(panel.background = element_rect(fill = "red3"),
        text = element_text(size = 15, 
                            color = "white", family = "serif"),
        axis.text = element_text(color = "white"),
        plot.background = element_rect(fill = "grey30", 
                                       color = "black"),
        panel.grid.major.y = element_blank())


counts <- c("Sweden", "Norway")
col <- "lightblue"
ggplot(gapminder %>% filter(continent == "Europe", year == 2007), aes(gdpPercap, lifeExp)) +
  geom_point(alpha = 0.4) +
  geom_point(aes(color = country), data = gapminder %>% filter(country %in% counts, year == 2007)) +
  geom_line(aes(color = country), data = gapminder %>% filter(country %in% counts) %>% select(-year)) +
  ggrepel::geom_text_repel(aes(label = country), size = 2.5, hjust = -0.1, family = "serif") +
  geom_text(aes(label = year, color = country), data = gapminder %>% filter(country %in% counts), hjust = 1.1, show.legend = F, family = "serif") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), text = element_text(family = "serif", size = 15),
        plot.background = element_rect(fill = col, color = col),
        panel.background = element_rect(fill = col),
        legend.background = element_rect(color = col, fill = col)) +
  labs(title = "Utveckling i två länder 1952 - 2007", subtitle = "Med 2007-års värden för europeiska länder som jämförelse",
       x = "BNP per capita", y = "Medellivslängd", color = "Land") +
  scale_color_manual(values = c("red", "darkblue"), labels = c("Norge", "Sverige"))

