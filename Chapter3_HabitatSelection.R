library(viridis)
library(lubridate)
library(tidyverse)
library(car)
library(patchwork)
library(lmerTest)
library(ggplot2)
library(ggh4x)

setwd("F:/HABITAT ANALYSIS/Used")

used <- read.csv("used3.csv") |>
  mutate(season = substr(release_season, 1, nchar(release_season) - 4),
         date = as.Date(date, format = "%d/%m/%Y")  # Replace "%d/%m/%Y" with the actual format
  ) |>
  filter(!is.na(date) & format(date, "%Y") != "2022") |>
  rename_with(~ gsub("\\.", "_", .), everything()) |>
  rename(ave_grass_height = av_grass_height) |>
  glimpse()

used$Habitat_type[used$Habitat_type == "Phalaris"] <- "Exotic pasture"
used$Habitat_type[used$Habitat_type == "Exotic Pasture"] <- "Exotic pasture"


avail <- read.csv("avail3.csv") |>
  dplyr::mutate(date = as.Date(date, format = "%d/%m/%Y")) |>
  filter(!is.na(date) & format(date, "%Y") != "2022") |>
  glimpse()

avail$Habitat_type[avail$Habitat_type == "Phalaris"] <- "Exotic pasture"
avail$Habitat_type[avail$Habitat_type == "Exotic Pasture"] <- "Exotic pasture"

#-------------------------------------------------------------------------------
# subset to variables of interest and combine available and used data frames
sub.aval <- avail |>
  dplyr::select(grass_cover, forb_cover, thatch_cover, bare_ground, rock_cover, ave_grass_height, thatch_depth, Habitat_type, season) |>
  mutate(pres = 0) |>
  glimpse()

sub.used <- used |>
  dplyr::select(grass_cover, forb_cover, thatch_cover, bare_ground, rock_cover, ave_grass_height, thatch_depth, Habitat_type, season) |>
  mutate(pres = 1) |>
  glimpse()

all <- bind_rows(sub.aval, sub.used) |>
  glimpse()

#-------------------------------------------------------------------------------
# selection for habitat type by season (do this for each season)

temp <- filter(all, season == "spring")
a <- table(temp$Habitat_type, temp$pres)
a

# proportions for available and used
pa <- a
pa[, 1] <- pa[, 1] / sum(pa[, 1]) #available
pa[, 2] <- pa[, 2] / sum(pa[, 2]) #used
round(pa, 2)

# test
chisq.test(a)
#p = 0.004

# selection for habitat type: summer
summer.temp <- filter(all, season == "summer")
b <- table(summer.temp$Habitat_type, summer.temp$pres)
b

# proportions for available and used
pas <- b
pas[, 1] <- pas[, 1] / sum(pas[, 1])
pas[, 2] <- pas[, 2] / sum(pas[, 2])
round(pas, 2)

# test
chisq.test(b)

# Calculate standardized residuals
std_res <- chisq.test(b)$stdres

# Display standardized residuals
print(round(std_res, 2))

# Highlight cells with significant deviations (e.g., residuals > |2|)
significant_cells <- abs(std_res) > 2
print(significant_cells)

# selection for habitat type: autumn
aut.temp <- filter(all, season == "autumn")
c <- table(aut.temp$Habitat_type, aut.temp$pres)
c

# proportions for available and used
paa <- c
paa[, 1] <- paa[, 1] / sum(paa[, 1])
paa[, 2] <- paa[, 2] / sum(paa[, 2])
round(paa, 2)

# test
chisq.test(c)


#-------------------------------------------------------------------------------
# changes in availability over seasons for cover variables
glimpse(sub.aval)
sub.aval$season <- factor(sub.aval$season, levels = c("summer", "autumn", "spring"))
sub.used$season <- factor(sub.used$season, levels = c("summer", "autumn", "spring"))

# function to get means and CIs for key variables in each season
mci <- function(var) {
  fm <- formula(paste(var, "~", "season - 1"))
  temp <- lm(fm, data = sub.aval)
  ci <- as.data.frame(confint(temp))
  names(ci) <- c("lcl", "ucl")
  ci$season <- levels(sub.aval$season)
  ci$est <- coef(temp)
  ci$var <- var
  rownames(ci) <- NULL
  return(ci)
}


# get values for the first 8 columns
vars <- colnames(sub.aval)[1:7]
avail <- mci(vars[1])

for(i in 2:length(vars)) {
  avail <- bind_rows(avail, mci(vars[i]))
}

avail$season <- factor(avail$season, levels = c("summer", "autumn","spring"))
avail$var <- factor(avail$var, levels = c("grass_cover", "forb_cover", "thatch_cover", "bare_ground", "rock_cover", "ave_grass_height", "thatch_depth"))

levels(avail$var) <- c("Grass Cover", 
                       "Forb Cover", 
                       "Thatch Cover", 
                       "Bare Ground", 
                       "Rock Cover", 
                       "Avg Grass Height", 
                       "Thatch Depth")

#levels(avail$season) <- c("Summer", "Autumn", "Spring")



ggplot(avail, aes(y = est, x = season, color = season)) +  # Map color to season
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0, lwd = 1.1) +  # Adjust error bar width
  facet_wrap(~ var, nrow = 3, scales = "free_y", 
             labeller = label_wrap_gen(width = 20)) +  # Improve facet label readability
  theme_classic() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis text for clarity
  labs(title = "Available Vegetation per Season with 95% CI", 
       x = "Season",
       y = "Mean ± 95% CI") +
  scale_color_viridis(discrete = TRUE)
  
  
# function to get means and CIs for key variables in each season
mci <- function(var) {
  fm <- formula(paste(var, "~", "season - 1")) # formula: var ~ season-1
  temp <- lm(fm, data = sub.used) # fit linear model (without intercept)
  ci <- as.data.frame(confint(temp)) # compute 95% CI intervals
  names(ci) <- c("lcl", "ucl") # rename ucl and lcl
  ci$season <- levels(sub.used$season) # add names to the CI data
  ci$est <- coef(temp) # extract mean estimates (coefficients)
  ci$var <- var # add variable names as a column
  rownames(ci) <- NULL # remove row names for the CI data
  return(ci) # return data with CIs, means, and seasons
}

# get values for the first 8 columns
vars <- colnames(sub.used)[1:7] # estract the names of the first 7 columns
used <- mci(vars[1]) # apply the mci function to the first variable

for(i in 2:length(vars)) {
  used <- bind_rows(used, mci(vars[i])) # append results for each variable to 'used'
}

#used$season <- factor(used$season, levels = c("summer", "autumn", "spring"))
used$var <- factor(used$var, levels = c("grass_cover", "forb_cover", "thatch_cover", "bare_ground", "rock_cover", "ave_grass_height", "thatch_depth"))

levels(used$var) <- c("Grass Cover", 
                       "Forb Cover", 
                       "Thatch Cover", 
                       "Bare Ground", 
                       "Rock Cover", 
                       "Avg Grass Height", 
                       "Thatch Depth")

#levels(used$season) <- c("Summer", "Autumn", "Spring")

ggplot(used, aes(y = est, x = season, colour = season)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0, lwd = 1.1) +
  facet_wrap(~ var, nrow = 3, scale = "free_y") +
  theme_classic() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis text for clarity
  labs(title = "Used Vegetation per Season with 95% CI", 
       x = "Season",
       y = "Mean ± 95% CI") +
  scale_color_viridis(discrete = TRUE)
#-------------------------------------------------------------------------------
# comparing availability across seasons

ggplot(avail, aes(x=est)) +
  geom_histogram(binwidth=25, fill="blue", alpha=0.5)+
  facet_wrap(~season, scales = "free")+
  theme_classic()
# not normally distributed

# wilcoxon rank sum-test
pairwise.wilcox.test(avail$est, avail$season, p.adjust.method = "bonferroni")

kruskal.test(est~season, data=avail) # p = 0.7 no significant differences in veg structure

#-------------------------------------------------------------------------------
# fit models to estimate resource selection functions for each season

m.summer <- glm(pres ~ grass_cover + forb_cover + thatch_cover + bare_ground + rock_cover +  ave_grass_height- 1,
                data = filter(all, season == "summer"),
                family = binomial)

m.autumn <- glm(pres ~ grass_cover + forb_cover + thatch_cover + bare_ground + rock_cover +  ave_grass_height- 1,
          data = filter(all, season == "autumn"),
          family = binomial)

m.spring <- glm(pres ~ grass_cover + forb_cover + thatch_cover + bare_ground + rock_cover +  ave_grass_height - 1,
                data = filter(all, season == "spring"),
                family = binomial)


summary(m.summer)
summary(m.autumn)
summary(m.spring)



# all of the cover variables show significant selection in at least one season
#-------------------------------------------------------------------------------

# extract the coefficients and calculate the resource selection functions
# also extract the p values for each coefficient

s <- as.data.frame(confint(m.summer))
names(s) <- c("lcl", "ucl")
s$est <- coef(m.summer)
# convert to resource selection function
s <- exp(s)
s$season <- "summer"
s$var <- rownames(s)
s$p.value <- summary(m.summer)$coef[, 4]
s$significant <- ifelse(s$p.value < 0.05, 1, 0)
s


sp <- as.data.frame(confint(m.spring))
names(sp) <- c("lcl", "ucl")
sp$est <- coef(m.spring)
sp <- exp(sp)
sp$season <- "spring"
sp$var <- rownames(sp)
sp$p.value <- summary(m.spring)$coef[, 4]
sp$significant <- ifelse(sp$p.value < 0.05, 1, 0)
sp

a <- as.data.frame(confint(m.autumn))
names(a) <- c("lcl", "ucl")
a$est <- coef(m.autumn)
a <- exp(a)
a$season <- "autumn"
a$var <- rownames(a)
a$p.value <- summary(m.autumn)$coef[, 4]
a$significant <- ifelse(a$p.value < 0.05, 1, 0)
a

# combine the seasons
out <- bind_rows(s, sp, a)
rownames(out) <- NULL
# put the seasons in order
out$season <- factor(out$season, levels = c("spring", "summer", "autumn"))
out

# put the cover variables in a useful order
out$var <- factor(out$var, levels = c("ave_grass_height", "grass_cover", "forb_cover", "rock_cover", "bare_ground", "thatch_cover", "thatch_depth"))

levels(out$var) <- c("Avg Grass Height", "Grass Cover", 
                      "Forb Cover", 
                     "Rock Cover",
                     "Bare Ground",
                      "Thatch Cover", 
                      "Thatch Depth (cm)")

# a value of 1 indicates no selection preference
# significance indicates resource selection function is significantly <> 1
# also evident if the 95% CIs cross zero or not


ggplot(filter(out, season=="spring"), aes(y = est, x = var)) +
  geom_point(aes(shape = factor(significant)), size = 4,colour = "darkgrey") +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0, lwd = 1.1,colour = "darkgrey") +
  #facet_wrap(~ var, nrow = 3, scale = "free_y") +
  geom_hline(yintercept = 1) +
  theme_classic() +
  theme(legend.position = "top")+
  labs(title="Spring 2024", x="Vegetation variables", y="Selection Estimate", shape="Significance")+
  scale_shape_manual(values = c(16,17), labels = c("Not Significant", "Significant"))

ggplot(filter(out, season=="summer"), aes(y = est, x = var)) +
  geom_point(aes(shape=factor(significant)), size = 4, colour = "red") +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0, lwd = 1.1, colour = "red") +
  #facet_wrap(~ var, nrow = 3, scale = "free_y") +
  geom_hline(yintercept = 1) +
  theme_classic() +
  theme(legend.position = "top")+
  labs(title="Summer 2023", x="Vegetation variables", y="Selection Estimate", shape="Significance")+
  scale_shape_manual(values = c(17), labels = c("Significant"))

ggplot(filter(out, season=="autumn"), aes(y = est, x = var)) +
  geom_point(aes(shape = factor(significant)), size = 4, colour = "blue") +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0, lwd = 1.1, colour = "blue") +
  #facet_wrap(~ var, nrow = 3, scale = "free_y") +
  geom_hline(yintercept = 1) +
  theme_classic() +
  theme(legend.position = "top")+
  labs(title="Autumn 2023", x="Vegetation variables", y="Selection Estimate", shape="Significance")+
  scale_shape_manual(values = c(16,17), labels = c("Not Significant", "Significant"))


#-------------------------------------------------------------------------------
# combined figure so you can compare seasonal availability with selection
out$type <- "Selection"
avail$type <- "Availability"

avail1 <- avail |>
  arrange(season)

avail$season <- factor(avail$season, levels = c("summer", "autumn", "spring"))

oa <- bind_rows(out, avail)
#oa$season <- factor(oa$season, levels = c("summer", "autumn", "spring"))
#levels(oa$season) <- c("Summer 2023", "Autumn 2023", "Spring 2024")

oa1 <- oa |>
  filter(var !="Thatch Depth")|>
  glimpse()

oa1$season <- factor(oa1$season, levels = c("summer", "autumn", "spring"))
levels(oa1$season) <- c("Summer 2023", "Autumn 2023", "Spring 2024")

#oa$season <- factor(oa$season, levels = c("summer", "autumn", "spring"))
#levels(oa$season) <- c("Summer 2023", "Autumn 2023", "Spring 2024")

ggplot(oa1, aes(y = est, x = var, col=season)) +
  geom_point(data = filter(oa1, type == "Availability"), size = 4) +
  geom_errorbar(
    data = filter(oa1, type == "Availability"),
    aes(ymin = lcl, ymax = ucl),
    width = 0, lwd = 1.1) +
  geom_point(data = filter(oa1, type == "Selection"), aes(shape = factor(significant)), size = 4) +
  geom_errorbar(data=filter(oa1, type == "Selection"),
                aes(ymin = lcl, ymax = ucl), 
                width = 0, lwd = 1.1) +
  facet_wrap(~type+season, scale = "free_y", nrow = 2) +
  geom_hline(data = filter(oa1, type == "Selection"), aes(yintercept = 1)) +
  theme_classic() +
  theme(legend.position = "top", axis.text.x=element_text(angle=25, hjust=1))+
    labs(title="Availability vs. Selection", x=
          "Microhabitat variables", y="")+
  scale_color_manual(values=c("Summer 2023"="red", "Autumn 2023"="blue", "Spring 2024"="darkgrey"))+
  scale_shape_manual(values = c(16,17), labels = c("Not Significant", "Significant"))

p1<- ggplot(filter(oa1, type == "Availability"), aes(y = est, x = var, col = season)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0, lwd = 1.1) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        legend.position = "none",
        plot.title=element_text(hjust=-0.05)) +
  labs(title="A)",y = "Availability measurements", x= "") +
  scale_color_manual(values=c("Summer 2023"="red", "Autumn 2023"="blue", "Spring 2024"="darkgrey"))+
  facet_wrap(~season)

# Selection Plot
p2 <- ggplot(filter(oa1, type == "Selection"), aes(y = est, x = var, col = season)) +
  geom_point(aes(shape = factor(significant)), size = 4) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0, lwd = 1.1) +
  geom_hline(aes(yintercept = 1)) +
  theme_classic() +
  theme(legend.position = "bottom",
        plot.title=element_text(hjust=-0.05),
        axis.text.x = element_text(angle = 35, hjust = 1),
        strip.text=element_text(face="bold")) +
  labs(title="B)",x="Microhabitat variables",y = "Selection Ratio") +
  scale_color_manual(values=c("Summer 2023"="red", "Autumn 2023"="blue", "Spring 2024"="darkgrey"), name="Season")+
  facet_wrap(~season)+
  scale_shape_manual(name="Significance",
    values = c(16,17), labels = c("Not Significant", "Significant"))+
  theme(plot.margin = margin(0.5, 1, 0.5, 1,"cm"))

library(patchwork)
combined_plot <- p1/p2
combined_plot

#ggsave("microhabitatselection.png", combined_plot, device="png", dpi=600)

#-------------------------------------------------------------------------------
# 1) Do sexes differ in microhabitat use? 
# - compare vegetation variables between sexes

# 2) Does microhabitat use differ by sex across seasons?
# - test is sex-related microhabiat use changes across seasons

#-------------------------------------------------------------------------------
used <- read.csv("used3.csv") |>
  mutate(season = substr(release_season, 1, nchar(release_season) - 4),
         date = as.Date(date, format = "%d/%m/%Y")  # Replace "%d/%m/%Y" with the actual format
  ) |>
  filter(!is.na(date) & format(date, "%Y") != "2022") |>
  rename_with(~ gsub("\\.", "_", .), everything()) |>
  rename(ave_grass_height = av_grass_height) |>
  glimpse()

used$Habitat_type[used$Habitat_type == "Phalaris"] <- "Exotic pasture"
used$Habitat_type[used$Habitat_type == "Exotic Pasture"] <- "Exotic pasture"

sub.used1 <- used |>
  dplyr::select(grass_cover, forb_cover, thatch_cover, bare_ground, rock_cover, thatch_depth, 
                ave_grass_height, season, sex) |>
  mutate(pres=1)|>
  glimpse()



used_males <- filter(sub.used1, sex == "M")
used_females <- filter(sub.used1, sex == "F")



table(used_females$season)
table(used_males$season)

all_males <- bind_rows(sub.aval, used_males)

all_females <- bind_rows(sub.aval, used_females)



#---------------------------SEX OVER ALL SEASONS-----------------------------------------
# Male RSF model
males_model <- glm(pres ~ grass_cover + forb_cover + thatch_cover + bare_ground + rock_cover + ave_grass_height - 1,
                          data = all_males,
                          family = binomial)

summary(males_model)$deviance


# Female RSF model
females_model <- glm(pres ~ grass_cover + forb_cover + thatch_cover + bare_ground + rock_cover +  ave_grass_height - 1,
                            data = all_females,
                            family = binomial)

summary(females_model)$deviance

# Males
males_coefficients <- as.data.frame(confint(males_model))
names(males_coefficients) <- c("lcl", "ucl")
males_coefficients$est <- coef(males_model)
males_coefficients <- exp(males_coefficients)
males_coefficients$sex <- "Male"
males_coefficients$var <- rownames(males_coefficients)
males_coefficients$p.value <- summary(males_model)$coef[, 4]
males_coefficients$significant <- ifelse(males_coefficients$p.value < 0.05, 1, 0)


# Females
females_coefficients <- as.data.frame(confint(females_model))
names(females_coefficients) <- c("lcl", "ucl")
females_coefficients$est <- coef(females_model)
females_coefficients <- exp(females_coefficients)
females_coefficients$sex <- "Female"
females_coefficients$var <- rownames(females_coefficients)
females_coefficients$p.value <- summary(females_model)$coef[, 4]
females_coefficients$significant <- ifelse(females_coefficients$p.value < 0.05, 1, 0)

# Combine results
rsf_results <- bind_rows(males_coefficients, females_coefficients)

rsf_results <- rsf_results |>
  mutate(var = factor(var, 
                      levels = c("grass_cover", "forb_cover", "thatch_cover", 
                                 "bare_ground", "rock_cover", "ave_grass_height"),
                      labels = c("Grass Cover", "Forb Cover", "Thatch Cover", 
                                 "Bare Ground", "Rock Cover", "Average Grass Height")))

ggplot(rsf_results, aes(x = var, y = est, color = sex)) +
  geom_point(aes(shape = factor(significant)), size = 4) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.2, lwd = 1) +
  theme_classic() +
  facet_wrap(~sex) +
  geom_hline(yintercept = 1, linetype = "solid") +
  labs(
    title = "",
    x = "Vegetation Variables",
    y = "Selection Ratio",
    shape="Significance"
  ) +
  scale_color_manual(values = c("pink", "blue"), name = "Sex") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")+
  scale_shape_manual(values = c(16,17), labels = c("Not Significant", "Significant"))

kruskal.test(rsf_results$est~rsf_results$sex)
#p = > 0.05

#---------------------------SUMMER----------------------------------------------
# Male RSF model
summer_males_model <- glm(pres ~ grass_cover + forb_cover + thatch_cover + bare_ground + rock_cover + ave_grass_height - 1,
                   data = filter(all_males, season == "summer"),
                   family = binomial)
summary(summer_males_model)
#deviance = 388.46
# Female RSF model
summer_females_model <- glm(pres ~ grass_cover + forb_cover + thatch_cover + bare_ground + rock_cover + ave_grass_height - 1,
                     data = filter(all_females, season == "summer"),
                     family = binomial)
summary(summer_females_model)
# deviance = 354.18

# Males
males_coefficients <- as.data.frame(confint(summer_males_model))
names(males_coefficients) <- c("lcl", "ucl")
males_coefficients$est <- coef(summer_males_model)
males_coefficients <- exp(males_coefficients)
males_coefficients$sex <- "Male"
males_coefficients$var <- rownames(males_coefficients)
males_coefficients$p.value <- summary(summer_males_model)$coef[, 4]
males_coefficients$significant <- ifelse(males_coefficients$p.value < 0.05, 1, 0)


# Females
females_coefficients <- as.data.frame(confint(summer_females_model))
names(females_coefficients) <- c("lcl", "ucl")
females_coefficients$est <- coef(summer_females_model)
females_coefficients <- exp(females_coefficients)
females_coefficients$sex <- "Female"
females_coefficients$var <- rownames(females_coefficients)
females_coefficients$p.value <- summary(summer_females_model)$coef[, 4]
females_coefficients$significant <- ifelse(females_coefficients$p.value < 0.05, 1, 0)

# Combine results
summer_rsf_results <- bind_rows(males_coefficients, females_coefficients)

summer_rsf_results <- summer_rsf_results |>
  mutate(var = factor(var, 
                      levels = c("grass_cover", "forb_cover", "thatch_cover", 
                                 "bare_ground", "rock_cover", "ave_grass_height"),
                      labels = c("Grass Cover", "Forb Cover", "Thatch Cover", 
                                 "Bare Ground", "Rock Cover", "Average Grass Height")))

p1 <- ggplot(summer_rsf_results, aes(x = var, y = est, color = sex)) +
  geom_point(aes(shape = factor(significant)), size = 4) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.2, lwd = 1) +
  theme_classic() +
  facet_wrap(~sex, scales="fixed") +
  geom_hline(yintercept = 1, linetype = "solid") +
  labs(
    title = "Summer",
    x = "",
    y = "Selection Ratio"
  ) +
  scale_color_manual(values = c("pink", "blue"), name = "Sex") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(
    axis.text.x = element_blank(),       # Remove x-axis text
    axis.ticks.x = element_blank(),      # Remove x-axis ticks
    axis.title.x = element_blank(),      # Remove x-axis title
    legend.position = "none"             # Remove legend
  )



#-------------------------AUTUMN------------------------------------------------
# Male RSF model
autumn_males_model <- glm(pres ~ grass_cover + forb_cover + thatch_cover + bare_ground + rock_cover + ave_grass_height - 1,
                          data = filter(all_males, season == "autumn"),
                          family = binomial)
summary(autumn_males_model)
# deviance = 367.07

# Female RSF model
autumn_females_model <- glm(pres ~ grass_cover + forb_cover + thatch_cover + bare_ground + rock_cover + ave_grass_height - 1,
                            data = filter(all_females, season == "autumn"),
                            family = binomial)
summary(autumn_females_model)
# deviance = 239.98


# Males
autumn_males_coefficients <- as.data.frame(confint(autumn_males_model))
names(autumn_males_coefficients) <- c("lcl", "ucl")
autumn_males_coefficients$est <- coef(autumn_males_model)
autumn_males_coefficients <- exp(autumn_males_coefficients)
autumn_males_coefficients$sex <- "Male"
autumn_males_coefficients$var <- rownames(autumn_males_coefficients)
autumn_males_coefficients$p.value <- summary(autumn_males_model)$coef[, 4]
autumn_males_coefficients$significant <- ifelse(autumn_males_coefficients$p.value < 0.05, 1, 0)


# Females
autumn_females_coefficients <- as.data.frame(confint(autumn_females_model))
names(autumn_females_coefficients) <- c("lcl", "ucl")
autumn_females_coefficients$est <- coef(autumn_females_model)
autumn_females_coefficients <- exp(autumn_females_coefficients)
autumn_females_coefficients$sex <- "Female"
autumn_females_coefficients$var <- rownames(autumn_females_coefficients)
autumn_females_coefficients$p.value <- summary(autumn_females_model)$coef[, 4]
autumn_females_coefficients$significant <- ifelse(autumn_females_coefficients$p.value < 0.05, 1, 0)

# Combine results
autumn_rsf_results <- bind_rows(autumn_males_coefficients, autumn_females_coefficients)

autumn_rsf_results <- autumn_rsf_results |>
  mutate(var = factor(var, 
                      levels = c("grass_cover", "forb_cover", "thatch_cover", 
                                 "bare_ground", "rock_cover", "ave_grass_height"),
                      labels = c("Grass Cover", "Forb Cover", "Thatch Cover", 
                                 "Bare Ground", "Rock Cover", "Average Grass Height")))

p2 <- ggplot(autumn_rsf_results, aes(x = var, y = est, color = sex)) +
  geom_point(aes(shape = factor(significant)), size = 4) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.2, lwd = 1) +
  theme_classic() +
  facet_wrap(~sex, scales="fixed") +
  geom_hline(yintercept = 1, linetype = "solid") +
  labs(
    title = "Autumn",
    x = "",
    y = "Selection Ratio"
  ) +
  scale_color_manual(values = c("pink", "blue"), name = "Sex") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(
    axis.text.x = element_blank(),       # Remove x-axis text
    axis.ticks.x = element_blank(),      # Remove x-axis ticks
    axis.title.x = element_blank(),      # Remove x-axis title
    legend.position = "none"             # Remove legend
  )

#-------------------------SPRING------------------------------------------------
# Male RSF model
spring_males_model <- glm(pres ~ grass_cover + forb_cover + thatch_cover + bare_ground + rock_cover + ave_grass_height - 1,
                          data = filter(all_males, season == "spring"),
                          family = binomial)
summary(spring_males_model)
# deviance = 363.16

# Female RSF model
spring_females_model <- glm(pres ~ grass_cover + forb_cover + thatch_cover + bare_ground + rock_cover + ave_grass_height - 1,
                            data = filter(all_females, season == "spring"),
                            family = binomial)
summary(spring_females_model)
# deviance = 262.29


# Males
spring_males_coefficients <- as.data.frame(confint(spring_males_model))
names(spring_males_coefficients) <- c("lcl", "ucl")
spring_males_coefficients$est <- coef(spring_males_model)
spring_males_coefficients <- exp(spring_males_coefficients)
spring_males_coefficients$sex <- "Male"
spring_males_coefficients$var <- rownames(spring_males_coefficients)
spring_males_coefficients$p.value <- summary(spring_males_model)$coef[, 4]
spring_males_coefficients$significant <- ifelse(spring_males_coefficients$p.value < 0.05, 1, 0)


# Females
spring_females_coefficients <- as.data.frame(confint(spring_females_model))
names(spring_females_coefficients) <- c("lcl", "ucl")
spring_females_coefficients$est <- coef(spring_females_model)
spring_females_coefficients <- exp(spring_females_coefficients)
spring_females_coefficients$sex <- "Female"
spring_females_coefficients$var <- rownames(spring_females_coefficients)
spring_females_coefficients$p.value <- summary(spring_females_model)$coef[, 4]
spring_females_coefficients$significant <- ifelse(spring_females_coefficients$p.value < 0.05, 1, 0)

# Combine results
spring_rsf_results <- bind_rows(spring_males_coefficients, spring_females_coefficients)

spring_rsf_results <- spring_rsf_results |>
  mutate(var = factor(var, 
                      levels = c("grass_cover", "forb_cover", "thatch_cover", 
                                 "bare_ground", "rock_cover", "ave_grass_height"),
                      labels = c("Grass Cover", "Forb Cover", "Thatch Cover", 
                                 "Bare Ground", "Rock Cover", "Average Grass Height")))

p3 <- ggplot(spring_rsf_results, aes(x = var, y = est, color = sex)) +
  geom_point(aes(shape = factor(significant)), size = 4) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.2, lwd = 1) +
  theme_classic() +
  facet_wrap(~sex, scales = "fixed") +
  geom_hline(yintercept = 1, linetype = "solid") +
  labs(
    title = "Spring",
    x = "Vegetation Variables",
    y = "Selection Ratio"
  ) +
  scale_color_manual(values = c("pink", "blue"), name = "Sex") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="bottom")
plots <- p1/p2/p3
plots


#-------------------------ALL---------------------------------------------------

all_rsf_results <- bind_rows(
  summer_rsf_results %>% mutate(season = "Summer"),
  autumn_rsf_results %>% mutate(season = "Autumn"),
  spring_rsf_results %>% mutate(season = "Spring")
)

# Adjust the factor levels for proper ordering
all_rsf_results <- all_rsf_results %>%
  mutate(
    var = factor(var, levels = c("Grass Cover", "Forb Cover", "Thatch Cover", 
                                 "Bare Ground", "Rock Cover", "Average Grass Height")),
    season = factor(season, levels = c("Spring", "Summer", "Autumn")),
    sex = factor(sex, levels = c("Female", "Male"))
  )

# Combine datasets for males and females
all_combined <- bind_rows(
  all_males %>% mutate(sex = "M"),
  all_females %>% mutate(sex = "F")
)

# Fit a combined model with sex interaction
combined_model <- glm(pres ~ (grass_cover + forb_cover + thatch_cover + 
                                bare_ground + rock_cover + ave_grass_height) * sex - 1,
                      data = all_combined,
                      family = binomial)

summary(combined_model)

ggplot(all_rsf_results, aes(x = var, y = est, color = sex)) +
  geom_point(aes(shape = factor(significant)), size = 4) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.2, lwd = 1) +
  theme_classic() +
  facet_grid(sex ~ season) +  # Sex as rows, Season as columns
  geom_hline(yintercept = 1, linetype = "solid") +
  labs(
    #title = "Resource Selection Functions by Sex and Season",
    x = "Vegetation Variables",
    y = "Selection Ratio"
  ) +
  scale_color_manual(values = c("red", "blue"), name = "Sex") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="bottom")+
  scale_shape_manual(name="Significance",
                   values = c(16,17), labels = c("Not Significant", "Significant"))+
  theme(plot.margin = margin(0.5, 1, 0.5, 1,"cm"))

# Reduced model without sex interaction
reduced_model <- glm(pres ~ grass_cover + forb_cover + thatch_cover + bare_ground + rock_cover + ave_grass_height - 1,
                     data = all_combined,
                     family = binomial)

summary(reduced_model)

# Compare models
anova(reduced_model, combined_model, test = "LRT")

# Post-hoc pairwise comparisons
pairwise_results <- emmeans::emmeans(combined_model, pairwise ~ sex)
summary(pairwise_results)

plot(residuals(combined_model), main = "Residuals Plot")
qqnorm(residuals(combined_model))
qqline(residuals(combined_model))

residual_dev <- sum(residuals(combined_model, type = "pearson")^2)
df <- nrow(all_combined) - length(coef(combined_model))
overdispersion_ratio <- residual_dev / df
print(overdispersion_ratio) # 1.01

quasi_model <- glm(pres ~ grass_cover * sex + forb_cover * sex + thatch_cover * sex +
                     bare_ground * sex + rock_cover * sex + ave_grass_height * sex - 1,
                   data = all_combined,
                   family = quasibinomial)
summary(quasi_model)

glm_season_sex <- glm(pres ~ (grass_cover + forb_cover + thatch_cover + 
                                bare_ground + rock_cover + ave_grass_height) * 
                        season*sex - 1,
                      family = quasibinomial, data = all_combined)

summary(glm_season_sex)



#-------------------------------------------------------------------------------

all_rsf_results <- bind_rows(
  summer_rsf_results %>% mutate(season = "Summer"),
  autumn_rsf_results %>% mutate(season = "Autumn"),
  spring_rsf_results %>% mutate(season = "Spring")
)

# Adjust the factor levels for proper ordering
all_rsf_results <- all_rsf_results %>%
  mutate(
    var = factor(var, levels = c("Grass Cover", "Forb Cover", "Thatch Cover", 
                                 "Bare Ground", "Rock Cover", "Average Grass Height")),
    season = factor(season, levels = c("Summer", "Autumn", "Spring")),
    sex = factor(sex, levels = c("Female", "Male"))
  )

# Plot with seasons as columns and sex as rows
mfs <- ggplot(all_rsf_results, aes(x = var, y = est, color = sex, group=sex)) +
  geom_point(aes(shape = factor(significant)), 
             size = 4, 
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), 
                width = 0.2, 
                lwd = 1, 
                position = position_dodge(width = 0.5)) +
  theme_classic() +
  facet_wrap(~season) +
  geom_hline(yintercept = 1, linetype = "solid") +
  labs(x = "Vegetation Variables",
       y = "Selection Ratio",
       shape = "Significant") +
  scale_color_manual(values = c("red", "blue"), name = "Sex") +
  scale_shape_manual(values = c(16, 17), 
                     labels = c("Not Significant", "Significant")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

#ggsave(mfs, file="male_female_seasons.png", dpi=600)

#----------------SPRING BOTH SEX------------------------------------------------

spring_model <- glm(pres~ (grass_cover + forb_cover + thatch_cover + bare_ground + rock_cover + ave_grass_height) *sex, family = binomial,
                    data=filter(all_combined, season =="spring"))

summary(spring_model)

spring_model_no_interaction <- glm(pres ~ grass_cover + forb_cover + thatch_cover + 
                                     bare_ground + rock_cover + ave_grass_height + sex, 
                                   family = binomial, 
                                   data = filter(all_combined, season == "spring"))

anova(spring_model_no_interaction, spring_model, test="Chisq")
library(emmeans)
emmeans(spring_model, pairwise ~ sex | grass_cover + bare_ground, adjust = "Bonferroni")

summer_model <- glm(pres~ (grass_cover + forb_cover + thatch_cover + bare_ground + rock_cover + ave_grass_height) *sex, family = binomial,
                    data=filter(all_combined, season =="summer"))

summary(summer_model)

autumn_model <- glm(pres~ (grass_cover + forb_cover + thatch_cover + bare_ground + rock_cover + ave_grass_height) *sex, family = binomial,
                    data=filter(all_combined, season =="autumn"))

summary(autumn_model)


spring_rsf_mf <- as.data.frame(exp(coef(spring_model)))
colnames(spring_rsf_mf) <- c("RSF estimate")
spring_rsf_mf

rsf_ci_spr <- exp(confint(spring_model))
spring_rsf_mf$Lower_CI <- rsf_ci_spr[,1]
spring_rsf_mf$Upper_CI <- rsf_ci_spr[,2]
spring_rsf_mf$p_value <- summary(spring_model)$coefficients[,4]
round(spring_rsf_mf, 5)


summer_rsf_mf <- as.data.frame(exp(coef(summer_model)))
colnames(summer_rsf_mf) <- c("RSF estimate")
summer_rsf_mf

rsf_ci_sum <- exp(confint(summer_model))
summer_rsf_mf$Lower_CI <- rsf_ci_sum[,1]
summer_rsf_mf$Upper_CI <- rsf_ci_sum[,2]
summer_rsf_mf$p_value <- summary(summer_model)$coefficients[,4]
round(summer_rsf_mf, 5)


autumn_rsf_mf <- as.data.frame(exp(coef(autumn_model)))
colnames(autumn_rsf_mf) <- c("RSF estimate")
autumn_rsf_mf

rsf_ci_aut <- exp(confint(autumn_model))
autumn_rsf_mf$Lower_CI <- rsf_ci_aut[,1]
autumn_rsf_mf$Upper_CI <- rsf_ci_aut[,2]
autumn_rsf_mf$p_value <- summary(autumn_model)$coefficients[,4]
round(autumn_rsf_mf, 5)

spring_rsf_mf$Season <- "Spring"
summer_rsf_mf$Season <- "Summer"
autumn_rsf_mf$Season <- "Autumn"

# Combine all into one data frame
all_rsf_results <- bind_rows(spring_rsf_mf, summer_rsf_mf, autumn_rsf_mf)

all_rsf_results$Variable <- rownames(all_rsf_results)

all_rsf_results$Sex <- ifelse(grepl(":sexM", all_rsf_results$Variable), "Male", "Female")

all_rsf_results$Variable <- gsub(":sexM", "", all_rsf_results$Variable)

all_rsf_results$Season <- factor(all_rsf_results$Season, levels = c("Summer", "Autumn", "Spring"))
all_rsf_results$Sex <- factor(all_rsf_results$Sex, levels = c("Female", "Male"))

all_rsf_results$Significant <- ifelse(all_rsf_results$p_value < 0.05, "Significant", "Not Significant")

#--------------------------------------------------------------------------------

used <- read.csv("used3.csv") |>
  mutate(season = substr(release_season, 1, nchar(release_season) - 4),
         date = as.Date(date, format = "%d/%m/%Y")  # Replace "%d/%m/%Y" with the actual format
  ) |>
  filter(!is.na(date) & format(date, "%Y") != "2022") |>
  rename_with(~ gsub("\\.", "_", .), everything()) |>
  rename(ave_grass_height = av_grass_height) |>
  glimpse()

used$Habitat_type[used$Habitat_type == "Phalaris"] <- "Exotic pasture"
used$Habitat_type[used$Habitat_type == "Exotic Pasture"] <- "Exotic pasture"

sub.used_rt <- used |>
  dplyr::select(grass_cover, forb_cover, thatch_cover, bare_ground, rock_cover, thatch_depth, 
                ave_grass_height, season, sex, release_type) |>
  mutate(pres=1)|>
  glimpse()


used_soft <- filter(sub.used_rt, release_type == "SOFT")
used_hard <- filter(sub.used_rt, release_type == "HARD")


table(used_soft$season)
table(used_hard$season)

all_soft <- bind_rows(sub.aval, used_soft)

all_hard <- bind_rows(sub.aval, used_hard)



#---------------------------SEX OVER ALL SEASONS-----------------------------------------
# Male RSF model
soft_model <- glm(pres ~ grass_cover + forb_cover + thatch_cover + bare_ground + rock_cover + ave_grass_height - 1,
                   data = all_soft,
                   family = binomial)

summary(soft_model)$deviance
#925.7316

# Female RSF model
hard_model <- glm(pres ~ grass_cover + forb_cover + thatch_cover + bare_ground + rock_cover +  ave_grass_height - 1,
                     data = all_hard,
                     family = binomial)

summary(hard_model)$deviance
#1321.598

# soft
soft_coefficients <- as.data.frame(confint(soft_model))
names(soft_coefficients) <- c("lcl", "ucl")
soft_coefficients$est <- coef(soft_model)
soft_coefficients <- exp(soft_coefficients)
soft_coefficients$release_type <- "SOFT"
soft_coefficients$var <- rownames(soft_coefficients)
soft_coefficients$p.value <- summary(soft_model)$coef[, 4]
soft_coefficients$significant <- ifelse(soft_coefficients$p.value < 0.05, 1, 0)


# hard
hard_coefficients <- as.data.frame(confint(hard_model))
names(hard_coefficients) <- c("lcl", "ucl")
hard_coefficients$est <- coef(hard_model)
hard_coefficients <- exp(hard_coefficients)
hard_coefficients$release_type <- "HARD"
hard_coefficients$var <- rownames(hard_coefficients)
hard_coefficients$p.value <- summary(hard_model)$coef[, 4]
hard_coefficients$significant <- ifelse(hard_coefficients$p.value < 0.05, 1, 0)

# Combine results
rsf_rt_results <- bind_rows(soft_coefficients, hard_coefficients)

rsf_rt_results <- rsf_rt_results |>
  mutate(var = factor(var, 
                      levels = c("grass_cover", "forb_cover", "thatch_cover", 
                                 "bare_ground", "rock_cover", "ave_grass_height"),
                      labels = c("Grass Cover", "Forb Cover", "Thatch Cover", 
                                 "Bare Ground", "Rock Cover", "Average Grass Height")))

ggplot(rsf_rt_results, aes(x = var, y = est, color = release_type)) +
  geom_point(aes(shape = factor(significant)), size = 4) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.2, lwd = 1) +
  theme_classic() +
  facet_wrap(~release_type) +
  geom_hline(yintercept = 1, linetype = "solid") +
  labs(
    title = "",
    x = "Vegetation Variables",
    y = "Selection Ratio",
    shape="Significance"
  ) +
  scale_color_manual(values = c("pink", "blue"), name = "Release type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")+
  scale_shape_manual(values = c(16,17), labels = c("Not Significant", "Significant"))


kruskal.test(rsf_rt_results$est~rsf_rt_results$release_type)

