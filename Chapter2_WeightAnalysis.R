library(gridExtra)
library(lubridate)
library(tidyverse)
library(brms)
library(viridis)

setwd("F:/DAILY MOVEMENT AND SURVIVAL ANALYSIS/data")


dat <- read.csv("weights_df2.csv")

dat <- dat |>
  filter(!is.na(date) & date !="")

dat <- dat |>
  dplyr::mutate(date = parse_date_time(date, orders =c("dmy", "mdy", "ymd")))

dat <- dat |>
  dplyr::mutate(release_season.x = factor(release_season.x, levels = c("summer2023", "autumn2023", "spring2024")))

glimpse(dat)
  
dat$sex <- ifelse(dat$sex.x == "F", "Female", "Male")

hist(log(dat$weight)) #normal
hist(log(dat$svl+1))

dat$log_weight <- log(dat$weight)
dat$log_svl <- log(dat$svl)

dat$date <- as.Date(dat$date)

plot(dat$svl~dat$weight)



ggplot(dat, aes(y = svl, x = date, group = pit_id, colour = release_type.x)) +
  geom_point(alpha = 0.5, size = 2, position = position_jitter(width = 3, height=0)) +
  stat_summary(aes(group = release_type.x), fun = "mean", geom = "smooth", size = 1.5) +
  #stat_summary(aes(group = release_type.x, fill = release_type.x), fun.data = "mean_sdl", fun.args = list(mult = 1), 
              # geom = "ribbon", alpha = 0.2, color = NA) +
  facet_wrap(~release_season.x, scales = "free_x") +  # free_x removes empty months in each facet
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b",
    expand = expansion(mult = c(0.05,0))
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


  
ggplot(dat, aes(x = date, y = weight, colour = release_type.x)) +
  geom_point(alpha = 0.5, size = 2, position = position_jitter(width = 3, height=0)) +  
  geom_smooth(method = "gam", se = FALSE, size = 1.5) +  
  facet_wrap(~release_season.x, scales = "free_x",
             labeller=as_labeller(c(
               "summer2023" = "Summer 2023",
               "autumn2023" = "Autumn 2023",
               "spring2024" = "Spring 2024"
             ))) +  
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b",
    expand = expansion(mult = c(0.05, 0))
  ) +
  scale_color_manual(
    name = "Release Strategy",  # Change legend title
    labels = c("HARD" = "Hard Release", "SOFT" = "Soft Release"),  # Change legend labels
    values = c("HARD" = "#E69F00", "SOFT" = "#56B4E9")  # Ensure colors are provided
  ) +
  labs(
    x = "Month",
    y = "Weight (g)"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

dat$day_number <- as.numeric(difftime(dat$date, min(dat$date), units="days")) +1


dat <- dat |>
  group_by(release_season.x)|>
  mutate(day_number = as.numeric(difftime(date, min(date), units = "days")) + 1) |>
  ungroup()|>
  filter(day_number <= 42)  # Keep only days ≤ 42



ggplot(dat, aes(x = date, y = weight, colour = release_type.x)) +
  geom_point(alpha = 0.5, size = 2, position = position_jitter(width = 3, height=0)) +
  geom_smooth(aes(group = release_type.x), method = "lm", se=FALSE, size=1.5) +
  #stat_summary(aes(group = release_type.x), fun = mean, geom = "smooth", size = 1.5) +
  facet_wrap(~release_season.x, scales = "free_x",
             labeller = as_labeller(c(
               "summer2023" = "Summer 2023",
               "autumn2023" = "Autumn 2023",
               "spring2024" = "Spring 2024"
             ))) +  
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b",
    expand = expansion(mult = c(0.05, 0))
  ) +
  scale_color_manual(
    name = "Release Strategy",
    labels = c("HARD" = "Hard Release", "SOFT" = "Soft Release"),
    values = c("HARD" = "#FF0000", "SOFT" = "#0000FF")
  ) +
  labs(
    x = "Month",  # Remove x-axis title
    y = "Weight (g)"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.15,0.85),
    #axis.text.x = element_blank(),  # Remove x-axis labels
    #axis.ticks.x = element_blank(), # Remove x-axis ticks
    strip.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size=12, face="bold") # Make facet labels more prominent
  )



#-------------------------------------------------------------------------------
# now exclude spring 2022 & first weight
#sub.dat <- filter(dat, !(release_season == "spring2022" & days_since_release == 0))

# fit bayesian model

m2 <- brm(log_weight ~ days_alive + release_type.x + release_season.x + sex.x + 
                    days_alive:release_type.x + days_alive:release_season.x +
                    days_alive:sex.x + (1|pit_id), data = dat)

summary(m2, digits = 4)


# extract posterior draws/slope parameters from fitted model
cf <- as.data.frame(m2)

# calculate slope parameters of interest
hr.fem <- quantile(cf[, 2], probs = c(0.025, 0.5, 0.975))
sr.fem <- quantile(cf[, 2] + cf[, 7], probs = c(0.025, 0.5, 0.975))
hr.male <- quantile(cf[, 2] + cf[, 10], probs = c(0.025, 0.5, 0.975))
sr.male <- quantile(cf[, 2] + cf[, 7] + cf[, 10], probs = c(0.025, 0.5, 0.975))
dif.sh <- quantile(cf[, 7], probs = c(0.025, 0.5, 0.975))
dif.malefem <- quantile(cf[, 10], probs = c(0.025, 0.5, 0.975))

# seasonal difference relative to summer
dif.aut <- quantile(cf[, 8], probs = c(0.025, 0.5, 0.975))
dif.spr <- quantile(cf[, 9], probs = c(0.025, 0.5, 0.975))


# put in a dataframe
out <- data.frame(rbind(hr.fem, sr.fem, hr.male, sr.male, dif.sh, dif.malefem, dif.aut, dif.spr))
names(out) <- c("lcl", "med", "ucl")
out$x <- c(1,2,4,5,7,8.25,9.75,11)
#out


# plot
png(filename="figure_2.11.png", units = "cm", res = 301, height = 11, width = 15)
par(mfrow = c(1, 1), mar = c(4, 4, 3, 1), mgp = c(3, 1, 0))
plot(out$med ~ out$x, pch = 19, cex=1, ylim = c(min(out$lcl), max(out$ucl)), xlim = c(0.5, 11.5), bty = "l",
     xaxt = "n", xlab = "", ylab = "", cex.lab = 0.55, cex.axis = 0.5)
  arrows(out$x, out$lcl, out$x, out$ucl, length = 0, lwd = 1)
  par(mgp = c(3, 1, 0))
  axis(1, at = c(1,2,4,5,7,8.25,9.75,11), labels = c("Hard\nFemale", "Soft\nFemale",
                                      "Hard\nMale", "Soft\nMale",
                                      "Soft -\nHard", "Male -\nFemale",
                                      "Autumn -\nSummer", "Spring -\nSummer"), cex.axis = 0.5)
  title(ylab="Slope estimate", line= 2, cex.lab=0.55)
  abline(h = 0, col="red")
  abline(v = 6)
  #par(mgp = c(3, 1, 0))
  axis(3, at = c(2.75, 8.75), labels = c("A) Slope of weight change over time",
                                     "B) Slope differences               "), cex.axis = 0.55, tick = F)
#dev.off()
#-------------------------------------------------------------------------------

# Overall slope
overall_slope <- quantile(cf[, 2], probs = c(0.025, 0.5, 0.975))

# Soft vs Hard Release
hard_release <- quantile(cf[, 2], probs = c(0.025, 0.5, 0.975))
soft_release <- quantile(cf[, 2] + cf[, 7], probs = c(0.025, 0.5, 0.975))

# Male vs Female
female_slope <- quantile(cf[, 2], probs = c(0.025, 0.5, 0.975))
male_slope <- quantile(cf[, 2] + cf[, 10], probs = c(0.025, 0.5, 0.975))

# Seasonal Effects
summer_slope <- quantile(cf[, 2], probs = c(0.025, 0.5, 0.975))
spring_slope <- quantile(cf[, 2] + cf[, 9], probs = c(0.025, 0.5, 0.975))
autumn_slope <- quantile(cf[, 2] + cf[, 8], probs = c(0.025, 0.5, 0.975))

# Differences in slopes (Panel B)
dif.soft_hard <- quantile(cf[, 7], probs = c(0.025, 0.5, 0.975))
dif.male_female <- quantile(cf[, 10], probs = c(0.025, 0.5, 0.975))
dif.autumn_summer <- quantile(cf[, 8], probs = c(0.025, 0.5, 0.975))
dif.spring_summer <- quantile(cf[, 9], probs = c(0.025, 0.5, 0.975))

# Create updated dataframe
out <- data.frame(rbind(overall_slope,
                        hard_release, soft_release,
                        female_slope, male_slope,
                        summer_slope, spring_slope, autumn_slope,
                        dif.soft_hard, dif.male_female, dif.autumn_summer, dif.spring_summer))

names(out) <- c("lcl", "med", "ucl")
out$x <- c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13)


#png(filename="figure_2.2.png", units = "cm", res = 301, height = 11, width = 15)
# Update plot labels
par(mfrow = c(1, 1), mar = c(4, 4, 3, 1), mgp = c(3, 1, 0))

plot(out$med ~ out$x, pch = 19, cex=1, ylim = c(min(out$lcl), max(out$ucl)), xlim = c(0.5, 13.5), bty = "l",
     xaxt = "n", xlab = "", ylab = "", cex.lab = 0.75, cex.axis = 0.75)

arrows(out$x, out$lcl, out$x, out$ucl, length = 0, lwd = 1)

axis(1, at = c(1,2,3,4,5,6,7,8,10,11,12,13),
     labels = c("Overall \nchange", "Hard \nRelease", "Soft \nRelease",
                "Female", "Male",
                "Summer \nRelease", "Spring \nRelease", "Autumn \nRelease",
                "Soft -\nHard", "Male - \nFemale", "Autumn - \nSummer", "Spring -\n Summer"),
     cex.axis = 0.75)

title(ylab="Slope estimate", line=2, cex.lab=0.85)

abline(h = 0, col="red")
abline(v = 9)

axis(3, at = c(4.5, 11), labels = c("A) Slope of weight change over time", "B) Slope differences between groups"), cex.axis = 0.85, tick = F)

  
#dev.off()
#--------- bernds code ---------------------------------------------------------
for (i in 1:12)
{
c1 <- m2$fit@sim$samples[[1]][[i]][1001:2000]
c2 <- m2$fit@sim$samples[[2]][[i]][1001:2000]
c3 <- m2$fit@sim$samples[[3]][[i]][1001:2000]
c4 <- m2$fit@sim$samples[[4]][[i]][1001:2000]


cc <- c(c1,c2,c3,c4)

print(round(quantile(cc, c(0.025, 0.5, 0.975)),4))

}

posterior_summary <- posterior_summary(m2)
posterior_summary_rounded <- round(posterior_summary, 4)
print(posterior_summary_rounded)

#------------------------Body condition index-----------------------------------

bci_data <- dat |>
  filter(!is.na(svl), !is.na(weight))


bci_model <- lm(log(weight) ~ log(svl), data=dat)
summary(bci_model)

bci_data$BCI <- residuals(bci_model)

dat <- dat |>
  left_join(bci_data |> select(pit_id, BCI), by="pit_id")

ggplot(dat, aes(x=release_season.x, y=BCI)) +
  geom_boxplot() +
  theme_classic()

summary(lm(days_alive ~BCI, data =dat))

summary(lm(days_alive~BCI + release_type.x +release_season.x, data=dat))

summary(lm(days_alive~BCI * release_type.x *release_season.x, data=dat))

summary(lm(BCI ~ release_type.x * release_season.x, data = dat))

ggplot(dat, aes(x = days_alive, y = BCI, color = release_type.x)) +
  #geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ release_season.x) +
  labs(title = "BCI vs Days Alive", x = "Days Alive", y = "BCI") +
  theme_classic()

table(dat$release_type.x, dat$release_season.x)

summary(dat$days_alive)
hist(dat$days_alive)
hist(dat$BCI)
summary(dat$BCI)

ggplot(dat, aes(x = BCI, y = days_alive)) +
  geom_point() +
  geom_smooth(method = "lm") +
  coord_cartesian(xlim = c(-0.3, 0.3)) +
  theme_classic()

dat$BCI_group <- cut(dat$BCI, breaks = quantile(dat$BCI, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                     labels = c("Low", "Medium", "High"), include.lowest = TRUE)

boxplot(days_alive ~ BCI_group, data = dat,
        main = "Survival Time by BCI Group", ylab = "Days Alive")

