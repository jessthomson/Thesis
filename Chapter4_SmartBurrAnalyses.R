library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(ggdist)

setwd("F:/RINGTANK RAW")


#cleaned combined data 
#saveRDS(cc2, file="cc2.rds") # save cleaned data

combined_data <- readRDS("cc2.rds")


bom <- read.csv("BOM_weather_data.csv")

# Cleaning smart burrow -------------------------------------------------------

# make burrow use column - 0 and 1 - for later analysis
combined_data <- combined_data |>
  mutate(burrow_use = ifelse(pitid_corrected != "0", 1,0))|> # burrow presence (1) and absence (0)
  glimpse() # overview

table(combined_data$burrow_use, combined_data$pitid_corrected) # inspect

# remove unwanted columns
combined_data <- combined_data[-c(5,12:16,18)]
head(combined_data)

#checking burrow monitoring dates - consecutive monitoring dates
valid_pit_dates <- combined_data |>
  filter(!is.na(season), !is.na(date)) |> # keep season and date
  distinct(enclosure, season, date) |> # unique monitoring days
  arrange(enclosure, season, date) 


date_pit_blocks <- valid_pit_dates |>
  group_by(enclosure, season) |>
  arrange(date) |>
  mutate(
    date_diff = as.numeric(date - lag(date, default = first(date) - 1)), # day gaps
    gap_group = cumsum(date_diff != 1)  # new group if gap
  ) |>
  group_by(enclosure, season, gap_group) |>
  summarise(
    start_date = min(date),
    end_date = max(date),
    n_days = n(),
    .groups = "drop"
  ) |>
  arrange(enclosure, season, start_date)

head(combined_data)

#round combined data to date hour
combined_data2 <- combined_data |>
  mutate(datehour = floor_date(datetime, "hour"), # round down to hour
         hour=hour(datetime), # extract hour
         burrow_id2 = paste0(enclosure, "_", burrow_id))|> # unique burrow ID
  filter(hour >= 6, hour <= 20) # only daylight hours


colnames(combined_data2)

# Cleaning bom --------------------------------------

colnames(bom) 

bom <- bom |>
  dplyr::mutate(
    Year = as.integer(Year),
    Month = as.integer(Month),
    Day = as.integer(Day),
    date = as.Date(sprintf("%04d-%02d-%02d", Year, Month, Day))
  )|>
  dplyr::filter(date >= as.Date("2023-08-01") & date <= as.Date("2024-10-31")) # filter out dates not needed


names(bom)[6] <- "Max_daily_ambient_temp" # rename column

ggplot(bom, aes(x=date, y=Max_daily_ambient_temp))+
  geom_line()+
  theme_classic()

bom <- bom |>
  mutate(adjusted_air_temps = Max_daily_ambient_temp -1.38)|> # adjust down to TID temps (lapse rate -1.5 = diff in temp between 500 and 800m elevation)
  glimpse()

# bin temp groups by 5 degrees
bom$max_tempgroup_air <- cut(bom$adjusted_air_temps,
                             breaks = c(-Inf, 15, 20, 25, 30, 35, 40, Inf),
                             labels = c("< 14", "15–19", "20–24", "25–29", "30–34", "35–39", "> 40"),
                             right = FALSE)  # makes intervals like (15-20)

# plot
ggplot(bom, aes(x=Day, y=adjusted_air_temps, col="red"))+
  geom_line()+
  facet_grid(Year~max_tempgroup_air)+
  theme_bw()

table(bom$max_tempgroup_air)

summary(bom)

bom <- bom[-c(1:2,7,8)] # remove cols
colnames(bom)

# -----------------------Joining ----------------------------------

burrow_effort <- left_join(combined_data2, bom, by="date")|> # join by date
  filter(pitid_corrected != "0")|> # only detected pit ID's
  mutate(
    time_5min = floor_date(datetime, unit = "5 minutes"), # round down 5 mins
    datehour = floor_date(datetime, unit = "hour") # round down hour
  )|>
  glimpse()

# Full hourly time grid per dragon --------------------------

full_time_grid <- expand_grid(
  pitid_corrected = unique(combined_data2$pitid_corrected), # per unique pit id
  datehour = seq.POSIXt(
    floor_date(min(combined_data2$datetime), unit = "hour"), # lowest hour
    ceiling_date(max(combined_data2$datetime), unit = "hour"), # highest hour
    by = "hour" # by hour
  )
) |>
  filter(hour(datehour) >= 6, hour(datehour) <= 20) # daylight hours only

# Merge in hourly dragon detections ------------------------------------

dragon_hourly <- burrow_effort |>
  filter(hour >= 6, hour <= 20) |> # daylight hours only
  distinct(pitid_corrected, datehour, time_5min) |> # unqiue 5 min periods per hour
  count(pitid_corrected, datehour, name = "n_5min_bins") |> # count 5-min chunks
  mutate(perc_use = n_5min_bins / 12) # standardise to full hour

full_use_data <- full_time_grid |>
  left_join(dragon_hourly, by = c("pitid_corrected", "datehour")) |> # join by pit ID and datehour
  mutate(perc_use = replace_na(perc_use, 0)) # fill in zeroes for missing observations

# Merge in metadata and temperature -------------------------------------

metadata <- burrow_effort |>
  select(pitid_corrected, datehour, vegetation, season, enclosure, adjusted_air_temps) #|> #keep selected columns
#  distinct() # ensure no repeated data

full_use_data <- full_use_data |>
  left_join(metadata, by = c("pitid_corrected", "datehour")) |> # join by pit ID and datehour
  mutate(
    hour = hour(datehour), # hour format
    date = date(datehour), # date format
    temp_bin = case_when( # create temp bins 
      adjusted_air_temps < 15 ~ "< 14",
      adjusted_air_temps < 20 ~ "15–19",
      adjusted_air_temps < 25 ~ "20–24",
      adjusted_air_temps < 30 ~ "25–29",
      adjusted_air_temps < 35 ~ "30–34",
      adjusted_air_temps < 40 ~ "35–39",
      TRUE ~ "> 40"
    )
  )


# Calculate burrow availability ------------------------

burrow_effort_summary <- burrow_effort |>
  mutate(date = date(datetime)) |> # date format
  group_by(date, vegetation, enclosure) |> # group by date, vegetation, enclosure
  summarise(
    n_burrows_available = n_distinct(burrow_id2), # calc number of distinct/unique burrows available using burrow ID
    .groups = "drop" # ungroup
  )


# Join effort and get true standardised use ------------------------

effort_data <- full_use_data |>
  left_join(burrow_effort_summary, by = c("date", "vegetation", "enclosure")) |> # join by date, vegetation, enclosure
  mutate( 
    # calculate standardised use = proportion of time spent in burrow
    # divided by number of burrow avail that day in that enclosure/vegetation
    # if burrow availability is missing (e.g., NA), return NA instead
    standardised_use = if_else(is.na(n_burrows_available), NA_real_, perc_use / n_burrows_available), 
    temp_bin = factor( # reorder temp bins for plotting
      temp_bin,
      levels = c("< 14", "15–19", "20–24", "25–29", "30–34")
    )
  )


# Summarise for plotting -----------------------------------------------

library(binom)
summary_df <- effort_data |>
  filter(!is.na(temp_bin), !is.na(vegetation)) |> # filter out na values in temp bins and vegetation
  group_by(temp_bin, hour, vegetation) |> # group by temp bin, hour and vegetation
  summarise(
    n_success = sum(standardised_use, na.rm = TRUE), # number of used
    n_total = sum(!is.na(standardised_use)), # number of total available
    .groups = "drop" # ungroup
  ) |>
  rowwise() |> # apply binomial CI per group
  mutate(
    ci = list(binom::binom.confint(x = n_success, n = n_total, methods = "wilson"))
  ) |>
  unnest_wider(ci, names_sep = "_") |> # extract CI bounds
  transmute( # extract and rename cols
    temp_bin,
    hour,
    vegetation, 
    prop_time = ci_mean, # mean proportion of time in burrows
    lower = ci_lower, # lower CI
    upper = ci_upper # upper CI
  )

lizard_days <- effort_data |>
  filter(!is.na(temp_bin)) |> # remove NA in temp bin and vegetation
  mutate(date = as.Date(datehour)) |> # format date
  distinct(pitid_corrected, date, temp_bin) |> 
  count(temp_bin, name = "n_lizard_days") # count unique lizard day per temp x vegetation

summary_df_labeled <- summary_df |>
  left_join(lizard_days, by = c("temp_bin")) # add lizard days to summary df, join by temp bin

# Plot -----------------------------------------------------------------

facet_labels <- lizard_days |> # create a label for plotting
  mutate(
    facet_label = paste0(temp_bin, " (n=", n_lizard_days, ")")
  )

label_vec <- setNames(facet_labels$facet_label, facet_labels$temp_bin)

ggplot(summary_df, aes(x = hour, y = prop_time)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80", alpha = 0.4) +
  geom_line(color = "black", linewidth = 1) +
  geom_vline(xintercept = 12, linetype = "dotted", color = "grey30") +
  facet_grid(vegetation ~ temp_bin, labeller = labeller(temp_bin = label_vec)) +
  scale_x_continuous(breaks = c(6, 12, 18)) +
  labs(
    title = "Proportion of Time Spent in Burrows",
    x = "Time of day (hour)",
    y = "Proportion of Time in Burrows"
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# ------------ Linear model -------------

effort_data <- effort_data |>
  mutate(
    temp = adjusted_air_temps,
    #temp_sq = temp^2, # squared temp to capture non-linear (quadratic) effects
    vegetation = factor(vegetation)  # ensure it's a factor
  )

effort_data_cleaned <- effort_data |>
  filter(!is.na(standardised_use))

# 
# library(lmerTest)
# model_lmer <- lmer(log(standardised_use) ~ (temp + I(temp^2)) * vegetation +
#                      (1 | pitid_corrected),
#                    data = effort_data_cleaned)
# summary(model_lmer)
# plot(model_lmer) # a clear funnel 
# 
# # make proportional data explicitly between 0-1 but not = to.
# effort_data_cleaned <- effort_data_cleaned |>
#   mutate(standardised_use_beta = (standardised_use*(n()-1)+0.5)/n())
# 
# hist(effort_data_cleaned$standardised_use)
# 
# library(glmmTMB)
# model_beta <- glmmTMB(standardised_use_beta~(temp + I(temp^2))*vegetation +
#                         (1|pitid_corrected), data=effort_data_cleaned,
#                       family=beta_family(link="logit"))
# 
# summary(model_beta)
# model_beta$fit$convergence # = 0
# library(DHARMa)
# testDispersion(model_beta)
# res <- simulateResiduals(model_beta)
# plot(res)
# plotResiduals(res, effort_data_cleaned$vegetation)
# plotResiduals(res, effort_data_cleaned$temp)

library(gamlss)

effort_data_cleaned <- effort_data_cleaned |>
  mutate(temp_c = temp - mean(temp, na.rm = TRUE)) # center the temps


model_zoib <- gamlss(
  standardised_use ~ temp_c + I(temp_c^2)*vegetation,
  family = BEINF,  # Zero-One Inflated Beta
  data = effort_data_cleaned
)


model1_zoib_refined <- gamlss(
  standardised_use ~ temp_c + I(temp_c^2)*vegetation,
  tau.formula = ~ temp_c + I(temp_c^2)*vegetation,  # refine one-inflation
  family = BEINF,
  data = effort_data_cleaned
)

plot(
  fitted(model1_zoib_refined), effort_data_cleaned$standardised_use,
  xlab = "Fitted Values", ylab = "Observed Values",
  main = "Observed vs. Fitted (BEINF model)"
)
abline(0, 1, col = "red")

term.plot(model1_zoib_refined, what = "mu", ask = FALSE)

plot(model1_zoib_refined)
resid_zoib1 <- resid(model1_zoib_refined)
hist(resid_zoib1, breaks = 50, main = "Histogram of Normalised Residuals")

plot(effort_data_cleaned$temp_c, resid_zoib1)
abline(h=0, col="red")

summary(model1_zoib_refined)

# Summarise observed data by temp and vegetation
sum.mean <- effort_data_cleaned |>
  filter(!is.na(standardised_use), !is.na(vegetation)) |> # filter out NA rows
  mutate(day = as.Date(datehour)) |>
  group_by(adjusted_air_temps, vegetation) |> # group by temp and vegetation
  summarise(
    prop = mean(standardised_use, na.rm = TRUE), # mean proportion of burrow use
    n_dragons = n_distinct(pitid_corrected), # number of obs in each group
    .groups = "drop" # ungroup
  )

effort_data_cleaned |>
  mutate(day = as.Date(datehour)) |>
  distinct(pitid_corrected, day) |>
  count() # 329

mean_temp <- mean(effort_data_cleaned$temp, na.rm=TRUE)

# Create new data for prediction
newdata <- tidyr::crossing(
  adjusted_air_temps = seq(min(sum.mean$adjusted_air_temps),
                           max(sum.mean$adjusted_air_temps), by = 0.1), # fine-grained temp sequence
  vegetation = unique(sum.mean$vegetation))|> # all veg types
  mutate(
    temp=adjusted_air_temps,
    temp_c=temp-mean_temp)

# Ensure any other model variables are in new data

# newdata <- newdata |>
#   mutate(
#     temp = adjusted_air_temps, # temp
#     temp_sq = temp^2, # squared temo for non-linear temp effect
#     pitid_corrected = "dummy_id")

# Predict using fitted model
newdata$pred <- predict(model1_zoib_refined, newdata = newdata, type="response")  # exclude random effects


# Plot
ggplot(sum.mean, aes(x = adjusted_air_temps, y = prop, color = vegetation)) +
  geom_point(aes(size = n_dragons)) +
  geom_line(data = newdata, aes(y = pred), linewidth = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey40") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "\nDaily Maximum Ambient Temperature (°C)",
    y = "Proportion of Daylight Hours in Burrows\n",
    color = "Vegetation Type",
    size = "Lizard-Days"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.line = element_line(color = "black"),
    legend.position = "bottom"
  )

# ------ Collapsed (no vegetation) -----------------

summary_df1 <- effort_data |>
  filter(!is.na(temp_bin)) |> # filter out rows where temp bin is missing
  group_by(temp_bin, hour) |> # group data by temp bin and hour of day
  summarise(
    n_success = sum(standardised_use, na.rm = TRUE), # total proportion of use
    n_total = sum(!is.na(standardised_use)), # number of observations 
    .groups = "drop" # ungroup
  ) |>
  rowwise() |> # treat each row individually (per-row binomial CI)
  mutate(
    ci = list(binom::binom.confint(x = n_success, n = n_total, methods = "wilson")) # wilson binomial CI to each group
  ) |>
  unnest_wider(ci, names_sep = "_") |> # expand the nested list-column CI into individual columns with unique names
  transmute( # keep relevant columns and rename 
    temp_bin, # temp bin
    hour, # hour of day
    prop_time = ci_mean, # estimated proportion (mean of binomial)
    lower = ci_lower, # lower bound of CI
    upper = ci_upper # upper bound of CI
  )

# get lizard days
lizard_days <- effort_data |>
  filter(!is.na(temp_bin)) |>
  mutate(date = as.Date(datehour)) |>
  distinct(pitid_corrected, date, temp_bin) |> 
  count(temp_bin, name = "n_lizard_days") |> # col name per temp bin
  mutate(temp_bin_label = paste0(temp_bin, " (n = ", n_lizard_days, ")")) # label


label_vec <- setNames(lizard_days$temp_bin_label, lizard_days$temp_bin) # label

# Plot -----------------------------------------------------------------

ggplot(summary_df1, aes(x = hour, y = prop_time)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80", alpha = 0.4) +
  geom_line(color = "black", linewidth = 1) +
  geom_vline(xintercept = 12, linetype = "dotted", color = "grey30") +
  facet_wrap(~ temp_bin, labeller = labeller(temp_bin = label_vec)) +
  scale_x_continuous(breaks = c(6, 12, 18)) +
  labs(
    title = "Proportion of Time Spent in Burrows across temp bins",
    x = "Time of day (hour)",
    y = "Proportion of time in burrows"
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Collapse model ------

effort_data1 <- effort_data |>
  filter(!is.na(standardised_use))|>
  mutate(
    temp = adjusted_air_temps, # rename
    temp_sq = temp^2) # squared temp to capture non-linear effects

# make proportional data explicitly between 0-1 but not = to.
effort_data1 <- effort_data1 |>
  mutate(standardised_use_beta = (standardised_use*(n()-1)+0.5)/n())

effort_data1 <- effort_data1 |>
  mutate(temp_c = temp - mean(temp, na.rm = TRUE))

# model_beta1 <- glmmTMB(standardised_use_beta ~temp + I(temp^2) + (1|pitid_corrected),
#                        data=effort_data1,
#                        family=beta_family(link="logit"))
# summary(model_beta1)
# diagnose(model_beta1)
# 
# model_beta1_c <- glmmTMB(standardised_use_beta ~temp_c + I(temp_c^2),
#                          dispformula = ~1,
#                        data=effort_data1,
#                        family=beta_family(link="logit"))
# summary(model_beta1_c)

hist(effort_data1$standardised_use) # pretty heavily one-inflated

#ZOIB MODEL?
model_zoib <- gamlss(
  standardised_use ~ temp_c + I(temp_c^2),
  family = BEINF,  # Zero-One Inflated Beta
  data = effort_data1
)

model_zoib_refined <- gamlss(
  standardised_use ~ temp_c + I(temp_c^2),
  tau.formula = ~ temp_c + I(temp_c^2),  # refine one-inflation
  family = BEINF,
  data = effort_data1
)

summary(model_zoib_refined)
plot(
  fitted(model_zoib_refined), effort_data1$standardised_use,
  xlab = "Fitted Values", ylab = "Observed Values",
  main = "Observed vs. Fitted (BEINF model)"
)
abline(0, 1, col = "red")

term.plot(model_zoib_refined, what = "mu", ask = FALSE)

plot(model_zoib_refined)
resid_zoib <- resid(model_zoib_refined)
hist(resid_zoib, breaks = 50, main = "Histogram of Normalised Residuals")

plot(effort_data1$temp_c, resid_zoib)
abline(h=0, col="red")


# Summarise observed data by temp and vegetation
effort_data1 |>
  mutate(day = as.Date(datehour)) |>
  distinct(pitid_corrected, day) |>
  count() 
# total of 398 lizard-days

# broken down by temp bin
effort_data1 |>
  mutate(day = as.Date(datehour)) |>
  filter(!is.na(temp_bin)) |>
  distinct(pitid_corrected, day, temp_bin) |>
  count(temp_bin)

# Summarise raw data
sum.mean1 <- effort_data1 |>
  filter(!is.na(standardised_use)) |>
  mutate(day = as.Date(datehour)) |>
  group_by(adjusted_air_temps) |>
  summarise(
    prop = mean(standardised_use, na.rm = TRUE),
    n_dragons = n_distinct(pitid_corrected),
    .groups = "drop"
  )

# Center temperature
mean_temp <- mean(effort_data1$temp, na.rm = TRUE)

# Create new data for prediction
newdata1 <- tidyr::crossing(
  adjusted_air_temps = seq(
    from = floor(min(effort_data1$adjusted_air_temps, na.rm = TRUE)),
    to = ceiling(max(effort_data1$adjusted_air_temps, na.rm = TRUE)),
    by = 0.1
  )
) |>
  mutate(
    temp = adjusted_air_temps,
    temp_c = temp - mean_temp
  )

# Predict fitted response (mean proportion in burrows)
newdata1$pred <- predict(model_zoib_refined, newdata = newdata1, type = "response")

# Plot fitted model and raw data
ggplot(sum.mean1, aes(x = adjusted_air_temps, y = prop)) +
  geom_point(aes(size = n_dragons)) +
  geom_line(data = newdata1, aes(y = pred), color = "red", linewidth = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey40") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "\nDaily Maximum Ambient Temperature (°C)",
    y = "Proportion of Daylight Hours in Burrows\n",
    size = "Lizard-Days"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.line = element_line(color = "black"),
    legend.position = "bottom"
  )

#--------------Burrow fidelity--------------------------------------------------


fidelity_summary <- combined_data2 |>
  filter(pitid_corrected != "0") |>
  group_by(pitid_corrected, season, vegetation) |>
  summarise(
    n_days = n_distinct(date),
    n_unique_burrows = n_distinct(burrow_id),
    most_used_burrow = {
      most_used <- count(cur_data(), burrow_id, sort = TRUE) |> slice(1)
      most_used$burrow_id
    },
    most_used_burrow_count = {
      most_used <- count(cur_data(), burrow_id, sort = TRUE) |> slice(1)
      most_used$n
    },
    fidelity_index = most_used_burrow_count / n(),
    .groups = "drop"
  )

hist(fidelity_summary$fidelity_index) # high 1-inflation

summary(fidelity_summary$fidelity_index)

sum(fidelity_summary$fidelity_index == 1)
mean(fidelity_summary$fidelity_index == 1) # proportion

model_fidelity <- gamlss(
  fidelity_index ~ vegetation * season,
  tau.formula = ~ vegetation * season,
  family = BEINF,
  data = fidelity_summary
)
plot(model_fidelity)

summary(model_fidelity)

model_tau_simple <- gamlss(
  fidelity_index ~ vegetation * season,
  tau.formula = ~ 1,
  family = BEINF,
  data = fidelity_summary
)
summary(model_tau_simple)
plot(model_tau_simple)

model_simplest <- gamlss(
  fidelity_index ~ 1,
  tau.formula = ~ 1,
  family = BEINF,
  data = fidelity_summary
)
plot(model_simplest, main="simplest")
summary(model_simplest)

AIC(model_fidelity, model_tau_simple,model_simplest)

ggplot(fidelity_summary, aes(x = season, y = fidelity_index, fill = vegetation)) +
  geom_boxplot() +
  facet_wrap(~vegetation)+
  theme_minimal()

combined_data |>
  filter(pitid_corrected != "0") |>
  distinct(pitid_corrected, burrow_id) |>
  count(pitid_corrected, name = "n_burrows") |>
  summarise(
    mean_burrows = mean(n_burrows),
    median_burrows = median(n_burrows),
    sd_burrows = sd(n_burrows),
    max_burrows = max(n_burrows),
    min_burrows = min(n_burrows)
  )
# average burrows used = 3.29
# min 1, max 8 (1 dragon used 8)




# --------------- Burrow use frequency -----------------------------------------

burrow_frequency <- combined_data2 |>
  filter(pitid_corrected != "0") |> # don't include zero reads
  group_by(pitid_corrected, vegetation) |> # group by veg and pit ID
  summarise(
    n_unique_burrows = n_distinct(burrow_id), # number of 5-min intervals with PIT detections
    .groups = "drop" # ungroup
  )
# calc burrow use freq per vegetation, date, enclosure
burrow_use_freq <- burrow_effort |>
  mutate(date = as.Date(datetime)) |>
  group_by(date, vegetation, enclosure) |>
  summarise(
    burrow_use_events = n(),  # total 5-min detections that day
    .groups = "drop"
  )



# join with burrow effort summary
burrow_use_freq_std <- left_join(
  burrow_use_freq,
  burrow_effort_summary,  # has n_burrows_available
  by = c("date", "vegetation", "enclosure")
) |>
  mutate(
    burrow_use_freq_std = burrow_use_events / n_burrows_available
  )
summary(burrow_use_freq_std)

hist(burrow_use_freq_std$burrow_use_freq_std) # right skewed

burrow_use_freq_std |>
  group_by(vegetation) |>
  summarise(
    mean_freq_std = mean(burrow_use_freq_std, na.rm = TRUE),
    sd_freq_std = sd(burrow_use_freq_std, na.rm = TRUE),
    n_days = n()
  )


kruskal.test(burrow_use_freq_std ~ vegetation, data = burrow_use_freq_std)
# no difference in freq across vegetation types

# plot
model_gamma <- glm(burrow_use_freq_std ~ vegetation,
                   family = Gamma(link = "log"),
                   data = burrow_use_freq_std)

summary(model_gamma)

par(mfrow = c(2, 2))
plot(model_gamma)
hist(residuals(model_gamma, type = "deviance"), main = "Deviance Residuals", col = "grey")
plot(cooks.distance(model_gamma), type = "h", main = "Cook's Distance")
abline(h = 4 / nrow(burrow_use_freq_std), col = "red", lty = 2)
plot(
  fitted(model_gamma),
  burrow_use_freq_std$burrow_use_freq_std,
  xlab = "Fitted values",
  ylab = "Observed values",
  main = "Observed vs Fitted"
)
abline(0, 1, col = "red")

mean((burrow_use_freq_std$burrow_use_freq_std - predict(model_gamma, newdata = burrow_use_freq_std, type = "response"))^2)

# Burrow temp v air temp - buffering capacity ----------------


colnames(burrow_effort)

burrow_temp_daily <- burrow_effort |>
     mutate(date = as.Date(datetime)) |>
     group_by(date, burrow_id2, enclosure, vegetation) |>
     summarise(
         max_burrow_temp = max(temp, na.rm = TRUE),
         .groups = "drop")


air_temp_daily <- burrow_effort |>
  mutate(date = as.Date(datetime)) |>
  group_by(date) |>
  summarise(
    max_air_temp = max(adjusted_air_temps, na.rm = TRUE),
    .groups = "drop"
  )


burrow_temp_daily <- left_join(burrow_temp_daily, air_temp_daily, by = "date")

burrow_temp_daily |>
  count(date, burrow_id2) |>
  filter(n > 1)

burrow_temp_daily <- burrow_temp_daily |>
  mutate(buffering_index = max_air_temp - max_burrow_temp,
         vegetation=factor(vegetation))

burrow_temp_daily |>
  group_by(vegetation) |>
  summarise(
    mean_buffering = mean(buffering_index, na.rm = TRUE),
    sd_buffering = sd(buffering_index, na.rm = TRUE),
    n = n()
  )

hist(burrow_temp_daily$buffering_index) # roughly bell shaped

model_buffering <- lm(buffering_index ~ max_air_temp * vegetation, data = burrow_temp_daily)
par(mfrow=c(2,2))
plot(model_buffering)

summary(model_buffering)

ggplot(burrow_temp_daily, aes(x = max_air_temp, y = max_burrow_temp, col=vegetation)) +
  geom_point() +
  geom_smooth(method = "lm", aes(color = vegetation)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", col="grey40") +
  labs(
    x = "Maximum Ambient Temperature (°C)",
    y = "Maximum Burrow Temperature (°C)",
    col="Vegetation Type"
    #title = "Burrow Thermal Buffering"
  ) +
  theme_classic(base_size = 14)+
  theme(legend.position = "bottom")

ggplot(burrow_temp_daily, aes(x = max_air_temp, y = buffering_index, col=vegetation)) +
  geom_point() +
  geom_smooth(method = "lm", aes(color = vegetation)) +
  geom_hline(yintercept=0, linetype = "dashed", col="grey40") +
  labs(
    x = "Maximum Ambient Temperature (°C)",
    y = "Buffering Capacity (air - burrow °C)",
    col="Vegetation Type"
    #title = "Burrow Thermal Buffering"
  ) +
  theme_classic(base_size = 14)+
  theme(legend.position = "bottom")
