PlantGrowth


ggplot(PlantGrowth, aes(group, weight)) +
  geom_point()


mod <- lm(weight ~ group, data = PlantGrowth)


library(car)
Anova(mod)


qf(0.95, 2, 27)


# install.packages("emmeans")
library(emmeans)
emmeans(mod, pairwise ~ group)


emmeans(mod, pairwise ~ group, adjust = "none")


em <- emmeans(mod, pairwise ~ group)

# install.packages("multcomp")
# install.packages("multcompView")
library(multcomp)
cld(em, Letters = letters)


dat_two <- PlantGrowth %>% filter(group %in% c("trt1", "trt2"))


# t.test(___ ~ group, data = dat_two, var.equal = T)
# t.test(weight ~ ___, data = dat_two, var.equal = F)
# 
# mod <- lm(weight ~ group, data = dat_two)
# Anova(mod)


n_groups <- 5
dat_sim <- expand_grid(obs = 1:10, group = letters[1:n_groups]) %>% 
  mutate(y = rnorm(n()))
mod <- lm(y ~ group, dat_sim)
emmeans(mod, pairwise ~ group, adjust = "none")


# library(readxl)
# dat_apple <- read_excel("___", sheet = "Äppelangrepp")
# dat_apple


# ggplot(___, aes(x = ___, y = ___)) +
#   geom_point()


# mod <- lm(___ ~ ___, data = dat_apple)
# Anova(mod)


library(MASS)
oats_marvel <- oats %>% filter(V == "Marvellous")
oats_marvel


ggplot(oats_marvel, aes(N, Y, color = B, group = B)) +
  geom_point(size = 4) +
  geom_line()


mod_bl <- lm(Y ~ N + B, data = oats_marvel)
Anova(mod_bl)


mod_wo_block <- lm(Y ~ N, data = oats_marvel)
Anova(mod_wo_block)


cld(emmeans(mod_bl, ~ N), Letters = letters)


cld(emmeans(mod_wo_block, ~ N), Letters = letters)


dat_small_block <- oats %>% 
  filter(V == "Marvellous", N %in% c("0.6cwt", "0.0cwt"))
dat_small_block


# ggplot(dat_small_block, aes(x = ___, y = ___, group = ___)) +
#   geom_point() +
#   geom_line()


# mod <- lm(___ ~ ___ + B, data = dat_small_block)
# Anova(mod)
# 
# t.test(___ ~ ___, data = dat_small_block, paired = ___)


ggplot(oats, aes(N, Y, color = B)) +
  geom_point(size = 4) +
  facet_wrap(~ V)


mod_two_fact <- lm(Y ~ N * V + B, data = oats)


Anova(mod_two_fact)


emmeans(mod_two_fact, ~ N)
emmeans(mod_two_fact, ~ N + V)
emmeans(mod_two_fact, ~ N | V)


# cld(emmeans(mod_two_fact, ~ N | V), Letters = letters)


# mod_two_fact <- lm(Y ~ N * V * B, data = oats)


library(gapminder)
dat_eu07 <- gapminder %>% 
  filter(year == 2007, continent == "Europe") %>% 
  mutate(gdpPercap = gdpPercap / 1000)

library(ggrepel)
ggplot(dat_eu07, aes(gdpPercap, lifeExp)) +
  geom_point() +
  geom_text_repel(aes(label = country), size = 3)


# dat_eu57 <- gapminder %>%
#   filter(year == 2007, continent == "Europe") %>%
#   mutate(gdpPercap = gdpPercap / 1000)
# 
# ggplot(dat_eu57, aes(gdpPercap, lifeExp)) +
#   geom_point() +
#   geom_text_repel(aes(label = country), size = 3)


mod <- lm(lifeExp ~ gdpPercap, data = dat_eu07)
summary(mod)


ggplot(dat_eu07, aes(gdpPercap, lifeExp)) +
  geom_point() +
  geom_text_repel(aes(label = country), size = 3) +
  geom_smooth(method = lm)


library(car)
Anova(mod)


dat_eu07 <- dat_eu07 %>% 
  mutate(Residualer = residuals(mod),
         Skattade = fitted(mod))

ggplot(dat_eu07, aes(sample = Residualer)) + 
  geom_qq() + geom_qq_line()
ggplot(dat_eu07, aes(Skattade, Residualer)) + geom_point()


# dat_2007 <- gapminder %>% filter(year == 2007)
# ggplot(dat_2007, aes(gdpPercap, lifeExp)) + geom_point()


# dat_2007 <- gapminder %>% filter(year == 2007)
# ggplot(dat_2007, aes(gdpPercap, lifeExp)) + geom_point()


cor(dat_eu07$lifeExp, dat_eu07$gdpPercap)
cor.test(dat_eu07$lifeExp, dat_eu07$gdpPercap)


dat_eu07[, 4:6]
cor(dat_eu07[, 4:6])


# anscombe


# ggplot(anscombe, aes(x1, y1)) + geom_point()
# cor(anscombe$x1, anscombe$y1)

