# bug things

library(dplyr)

setwd("F:/HABITAT ANALYSIS/Used")

bugs <- read.csv("Invertebrates AMTECH Feb 2023.csv")

glimpse(bugs)

# convert all NA or plant spaces with 0
bugs <- bugs |>
  mutate(across(where(is.character), ~ replace_na(na_if(., ""), "0"))) |>
  glimpse()

bugs <- bugs |>
  mutate(across(everything(), ~ replace_na(., 0))) |>
  glimpse()

head(bugs)

bugs <- bugs |>
  rename(
    ants_small = ants...5mm,
    ants_medium = ants.5.10.mm,
    ants_large = large.ants...10.mm,
    beetles = Beetles
  )

bugs <- bugs |>
  mutate(
    TRAPLINE = as.factor(TRAPLINE),
    Vegetation = as.factor(Vegetation)
  )


colnames(bugs)

bugs$Spiders <- as.factor(bugs$Spiders)
bugs$scorpion <- as.factor(bugs$scorpion)
bugs$beetles <- as.factor(bugs$beetles)
bugs$Crickets <- as.factor(bugs$Crickets)
bugs$Flies <- as.factor(bugs$Flies)
bugs$unidentified <- as.factor(bugs$unidentified)
bugs$Total.ants <- as.factor(bugs$Total.ants)
bugs$ants_large <- as.factor(bugs$ants_large)
bugs$ants_medium <- as.factor(bugs$ants_medium)
bugs$ants_small <- as.factor(bugs$ants_small)
bugs$larvae <- as.factor(bugs$larvae)
bugs$earwig <- as.factor(bugs$earwig)
bugs$cockroach <- as.factor(bugs$cockroach)
str(bugs)

table(bugs$Spiders)

# Remove words like "medium", "moderate", "small", etc., from all columns
bugs <- bugs |>
  mutate(across(ants_small:unidentified, ~ gsub(" .*", "", as.character(.)))) |>
  mutate(across(ants_small:unidentified, as.numeric))  # Convert back to numeric where possible

colnames(bugs)
bugs <- bugs[,-c(7)] # removed large ants column bc there are none

# Check the result
glimpse(bugs)
head(bugs)

bugs <- bugs |>
  dplyr::mutate(Vegetation = dplyr::case_when(
    Vegetation == "NatPast" ~ "Native Pasture",
    Vegetation == "Disturb NatPast" ~ "Exotic pasture",
    Vegetation == "Mix NTG/NatPast" ~ "Natural Temperate Grassland",
    Vegetation == "NTG" ~ "Natural Temperate Grassland",
    Vegetation == "NP" ~ "Native Pasture",
    TRUE ~ Vegetation  # Preserve original values for unmatched cases
  ))

bugs <- bugs |>
  mutate(TOTAL.CAPTURES = as.numeric(TOTAL.CAPTURES))

hist(log(bugs$TOTAL.CAPTURES))

anova_result <- aov(log(TOTAL.CAPTURES) ~ Vegetation, data = bugs)
summary(anova_result)
TukeyHSD(anova_result)


ggplot(bugs, aes(x = Vegetation, y = TOTAL.CAPTURES, fill = Vegetation)) +
  stat_summary(fun = mean, geom = "bar") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(x = "Vegetation Type", y = "Invertebrate Abundance") +
  theme_classic() +
  #theme(axis.text.x = element_text()) +
  guides(fill = "none")+
  scale_fill_viridis(discrete = TRUE)

preyavail <- ggplot(bugs, aes(x=Vegetation, y=log(TOTAL.CAPTURES), fill = Vegetation))+
  geom_boxplot()+
  labs(x="Vegetation Type", y="Invertebrate Abundance (Logged)") +
  theme_classic()+
  guides(fill="none") +
  scale_fill_viridis(discrete=TRUE)

ggsave(preyavail, file="preyavail.png", dpi=600)
