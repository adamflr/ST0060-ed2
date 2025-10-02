# install.packages("tidyverse")
library(tidyverse)


# install.packages("palmerpenguins")
# library(palmerpenguins)
# penguins


# install.packages("palmerpenguins")
library(palmerpenguins)
penguins


penguins %>% 
  drop_na() %>% 
  filter(year == 2007) %>% 
  group_by(species, sex) %>% 
  summarise(`Vikt, medel` = mean(body_mass_g),
            `Flipperlängd, median` = median(flipper_length_mm),
            `Näbblängd, standardavvikelse` = sd(bill_length_mm))


ggplot(penguins, aes(body_mass_g, species, fill = sex)) +
  geom_boxplot() +
  facet_wrap(~ year)


dat_sum <- penguins %>% 
  drop_na() %>% 
  group_by(species, sex) %>% 
  summarise(Mean = mean(body_mass_g),
            SD = sd(body_mass_g))
dat_sum

ggplot(dat_sum, aes(sex, Mean)) +
  geom_col() +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                width = 0.3) +
  facet_wrap(~ species)


set.seed(437)
dat <- slice_sample(penguins, n = 30)
dat %>% print(n = 30)


ggplot(dat, aes(body_mass_g, 0)) + 
  geom_point() +
  geom_vline(xintercept = 4500, color = "red", linewidth = 2)


t.test(dat$body_mass_g, mu = 4500)   # Ett t-test på variabeln body_mass_g


mean(dat$body_mass_g)
sd(dat$body_mass_g)


ggplot() +
  geom_function(fun = dt, args = list(df = 29)) +
  xlim(-5,5)

ggplot() +
  geom_function(fun = pt, args = list(df = 29)) +
  xlim(-5,5)


ggplot() +
  geom_function(fun = dt, args = list(df = 29)) +
  geom_vline(xintercept = c(-1.6203, 1.6203), 
             color = "red") +
  xlim(-5,5)


qt(0.975, 29)


t.test(dat$body_mass_g, conf.level = 0.95)


dat %>% count(sex)


prop.test(x = 16, n = 30, p = 0.5, correct = F)


p0 <- 0.5
p <- 16/30
n <- 30
z_value <- (p - p0) / sqrt(p0 * (1 - p0) / n)
z_value^2


#install.packages("binom")
library(binom)
binom.asymp(x = 16, n = 30)


# binom.asymp(x = 16, n = 30, conf.level = 0.95)


dat %>% count(species)


chisq.test(x = c(10, 8, 12), p = c(1/3, 1/3, 1/3))


chisq.test(x = c(16, 14), p = c(0.5, 0.5), correct = F)


chisq.test(c(6,4), p = c(0.51, 0.49))


dat_mnm <- data.frame(Color = c("blue", "brown", "green", 
                                "orange", "red", "yellow"),
                      Count = c(180, 80, 88, 160, 134, 166))

ggplot(dat_mnm, aes(Color, Count, fill = Color)) +
  geom_col() +
  scale_fill_manual(values = dat_mnm$Color)


dat_apple <- tibble(Tree = 1:4, 
              Before = c(48, 43, 30, 47), 
              After = c(51, 44, 42, 54))
dat_apple


dat_long <- dat_apple %>% 
  pivot_longer(-Tree, names_to = "Time", values_to = "Height")
dat_long


# ggplot(dat_long, aes(___, ___, group = ___)) +
#   geom_point() +
#   geom_line()


t.test(dat_apple$Before - dat_apple$After)


t.test(dat_apple$Before, dat_apple$After, paired = T)


ggplot(dat, aes(sex, body_mass_g)) +
  geom_point()

ggplot(dat, aes(sex, body_mass_g)) +
  geom_boxplot(width = 0.3)


t.test(body_mass_g ~ sex, data = dat, var.equal = T)


vikt_hon <- dat$body_mass_g[dat$sex == "female"]

vikt_han <- dat$body_mass_g[dat$sex == "male"]

t.test(vikt_hon, vikt_han, var.equal = T)


pairwise.t.test(dat$body_mass_g, dat$species, 
                p.adjust.method = "none", pool.sd = F)


dat %>% count(sex, species)


prop.test(x = c(6, 10), n = c(10, 20), correct = F)


dat_bird <- data.frame(Burfågel = c("Burfågel", "Ej_burfågel"),
              Lungcancer = c(98, 141),
              Ej_lungcancer = c(101, 328))
dat_bird


# prop.test(x = c(___, ___), n = c(___, ___), correct = F)


dat_table <- dat %>% count(sex, species)
dat_table
dat_table_wide <- dat_table %>% 
  pivot_wider(values_from = n, names_from = sex)
dat_table_wide


ggplot(dat_table, aes(species, n, fill = sex)) +
  geom_col(position = position_fill(), color = "black") +
  geom_text(aes(label = n), position = position_fill(0.5), size = 7) +
  scale_fill_manual(values = c("hotpink", "green"))


dat_table_wide[, 2:3] # De två numeriska kolumnerna

chisq.test(dat_table_wide[, 2:3])


dat_bird_swe <- data.frame(Burfågel = c("Burfågel", "Ej_burfågel"),
              Lungcancer = c(108, 144),
              Ej_lungcancer = c(171, 256))
dat_bird_swe


# dat_bird_swe[, c(2,3)]
# chisq.test(___, correct = F)

