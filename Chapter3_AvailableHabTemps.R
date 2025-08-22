library(tidyr)
library(lubridate)
library(dplyr)
library(readr)
library(Hmisc)
devtools::install_github("aammd/ibuttonr")
library(hms)
library(stringr)
library(ggplot2)
library(car) # for Anova
library(FactoMineR)
library(lmerTest)

#-------------------------------------------------------------------------------
# ibutton data for transect quadrats

setwd("F:/HABITAT ANALYSIS/Available")

veg_data <- read.csv("F:/HABITAT ANALYSIS/Available/avail3.csv")
veg_metadata <- read.csv("F:/HABITAT ANALYSIS/Available/metadata.csv")

table(veg_metadata$year)
table(unique(veg_data$date))

#remove 2022 from veg data
veg_data <- veg_data |>
  mutate(date = dmy(date))

veg_data <- veg_data |>
  filter(year(date) !=2022)

table(unique(veg_data$date))

veg_metadata <- veg_metadata |>
  filter(year !=2022)

#--------function for reading ibutton files-------------------------------------
# function to read each file and add the file name as a new column

read_ibutton_file <- function(ibuttonfile) {
  # Extract the file name without the path and extension
  ibutton_name <- tools::file_path_sans_ext(basename(ibuttonfile))
  
  # Read the CSV file, skipping metadata rows
  data <- read_csv(ibuttonfile, skip = 19, col_types = cols(`date/time` = col_character())) |>
    # Rename columns to standard names
    rename(`date/time` = 1, Unit = 2, Value = 3) |>
    # Convert the date column to Date-Time format with an explicit format
    mutate(
      `date/time` = dmy_hms(`date/time`),
      # Correct any misinterpreted years (assume 2022–2024 are valid years)
      `date/time` = if_else(
        year(`date/time`) < 2022 | year(`date/time`) > 2024,
        `date/time` + years(24),  # Adjust year as needed
        `date/time`
      )
    ) |>
    # Filter to remove data after 24/10/2024
    #filter(`date/time` <= as_datetime("2024-10-24 23:59:59")) |>
    # Add iButton name column
    mutate(iButton = ibutton_name)
  
  return(data)
}

# read ibutton file
ibuttonfile <- list.files(path = "F:/HABITAT ANALYSIS/Available/ibutton temps", pattern = "*.csv", full.names = TRUE)

#read and combine all ibutton files 
combined_ibuttons <- lapply(ibuttonfile, read_ibutton_file) |>
  bind_rows()


names(veg_metadata)[1] <- "iButton"

meta_ibutton <- left_join(veg_metadata, combined_ibuttons, by="iButton")|>
  glimpse()


names(meta_ibutton)[13] <- "Temperature"

#-------------------------------------------------------------------------------
# 

meta_ibutton1 <- meta_ibutton |>
  mutate(date=as.Date(`date/time`),
         time=format(`date/time`, format="%H:%M:%S")) |>
  filter(year(date) !=2022)|>
  glimpse()


summary(meta_ibutton1$date) # check that it converted properly

# remove NA dates
meta_ibutton1 <- meta_ibutton1 |>
  filter(!is.na(date)) |>
  glimpse()

table(meta_ibutton1$season)

# remove nighttime data --------------------------------------------------------

# Add seconds to times without seconds
meta_ibutton1 <- meta_ibutton1 |>
  mutate(
    time_clean = if_else(str_detect(time, "^\\d{1,2}:\\d{2}$"), 
                              paste0(time, ":00"), time))|>
  glimpse()

meta_ibutton1 <- meta_ibutton1 |>
  mutate(
    time_clean = as_hms(time_clean)
  ) |>
  glimpse()

# Define sunrise and sunset times by season
meta_ibutton1 <- meta_ibutton1 |>
  mutate(sunrise = case_when(
    season == "spring" ~ as_hms("06:00:00"),
    season == "summer" ~ as_hms("05:30:00"),
    season == "autumn" ~ as_hms("06:30:00")
  ),
  sunset = case_when(
    season == "spring" ~ as_hms("19:30:00"),
    season == "summer" ~ as_hms("20:30:00"),
    season == "autumn" ~ as_hms("18:30:00")
  ))


# filter to remove times outside the range of sunrise to sunset
meta_ibutton_daytime <- meta_ibutton1 |>
  filter(as_hms(`date/time`) >= sunrise & as_hms(`date/time`) <= sunset)|>
  glimpse()

mid <- meta_ibutton_daytime


table(mid$date)

# remove dates outside tracking
ibutton_to_remove <- mid |>
  filter(date >= as.Date("2024-01-06") & date <= as.Date("2024-02-14")) |>
  distinct(iButton)

# make sure all data is between dates where tracking took place 2023-2024
mid <- mid |>
  filter(!iButton %in% ibutton_to_remove)

table(is.na(mid$Temperature)) #no NA values

head(mid)


seasonal_temp_stats <- mid |>
  group_by(season, transect, quadrat, location) |>
  summarise(
    mean_temp = mean(Temperature, na.rm = TRUE),
    min_temp = min(Temperature, na.rm = TRUE),
    max_temp = max(Temperature, na.rm = TRUE),
    sd_temp = sd(Temperature, na.rm = TRUE))|>
  glimpse()

# join temperature statistics with vegetation data
veg_data_with_temps <- veg_data |>
  left_join(seasonal_temp_stats, by = c("transect", "quadrat", "location", "season"))|>
  glimpse()

veg_data_with_temps$Habitat_type[veg_data_with_temps$Habitat_type == "Phalaris"] <- "Exotic Pasture"

summary(veg_data_with_temps)

veg_data_with_temps <- veg_data_with_temps |>
  filter(!is.na(mean_temp))

veg_data_with_temps <- veg_data_with_temps |>
  filter(!is.na(grass_exotic))

veg_data_with_temps$grass_exotic <- ifelse(veg_data_with_temps$grass_exotic == 700, 70, veg_data_with_temps$grass_exotic)
  
 
summary(veg_data_with_temps)

head(veg_data_with_temps)

ggplot(veg_data_with_temps, aes(x = Habitat_type, y = mean_temp, fill = season)) +
  geom_boxplot(outlier.size = 1) +
  theme_classic() +
  labs(
    #title = "Observed Mean Ground Temperature by Habitat Type and Season",
    x = "Macrohabitat Type",
    y = "Mean Ground Temperature (°C)",
    fill = "Season"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("summer" = "#E69F00", 
                               "autumn" = "#56B4E9", 
                               "spring" = "#009E73"),
                    labels = c("Summer 2023", "Autumn 2023", "Spring 2024"))

veg_long_actual <- veg_data_with_temps|>
  pivot_longer(cols=c(
    grass_cover, forb_cover, thatch_cover, bare_ground, rock_cover, ave_grass_height, thatch_depth),
    names_to="veg_var", values_to="veg_value")

ggplot(veg_long_actual, aes(x = veg_var, y = mean_temp, fill = season)) +
  geom_boxplot(outlier.size = 1) +
  theme_classic() +
  labs(
    #title = "Observed Mean Ground Temperature Across Vegetation Variables",
    x = "Vegetation Variable",
    y = "Mean Ground Temperature (°C)",
    fill = "Season"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("summer" = "#E69F00", 
                               "autumn" = "#56B4E9", 
                               "spring" = "#009E73"),
                    labels = c("Summer 2023", "Autumn 2023", "Spring 2024"))


cor(veg_data_with_temps |>
      select(ave_grass_height, mean_temp, sd_temp), use = "complete.obs")

ggplot(veg_data_with_temps, aes(x = ave_grass_height, y = mean_temp, col=season)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()

anova_temp_habitat <- aov(mean_temp ~ Habitat_type*season, data = veg_data_with_temps)
summary(anova_temp_habitat)
TukeyHSD(anova_temp_habitat)


veg_temp_pca <- veg_data_with_temps |>
  select(grass_cover, rock_cover, forb_cover, thatch_cover, ave_grass_height, thatch_depth,bare_ground, mean_temp) |>
  na.omit()
#-----PCA----------
pca_results <- PCA(veg_temp_pca, scale.unit = TRUE)
# ave_grass_height and thatch_depth and thatch_cover contributing to mean temp variation

plot(anova_temp_habitat, which = 1:2)

#LMER-----

hist(veg_data_with_temps$mean_temp)

lmm <- lmer(mean_temp ~ (grass_cover + rock_cover +bare_ground + ave_grass_height+
                                    forb_cover + thatch_cover + thatch_depth)*season +
              (1|transect) + (1|quadrat) , data=veg_data_with_temps)
summary(lmm)

qqnorm(resid(lmm))
qqline(resid(lmm))



#------------------LM-------------------------------------------------------------


lm_model <- lm(mean_temp ~ grass_cover + bare_ground + rock_cover + ave_grass_height+
                 forb_cover + thatch_cover + thatch_depth + season, data = veg_data_with_temps)
summary(lm_model)

lm_model1 <- lm(mean_temp ~ (grass_cover + bare_ground + rock_cover + ave_grass_height+
                                   forb_cover + thatch_cover + thatch_depth)*season, data = veg_data_with_temps)
summary(lm_model1)

lmer_model1 <- lmer(mean_temp ~ (grass_cover + bare_ground + rock_cover + ave_grass_height+
                 forb_cover + thatch_cover + thatch_depth)*season + (1|transect) + (1|quadrat), data = veg_data_with_temps)
summary(lmer_model1)

lm_model2 <- lm(mean_temp ~ season + Habitat_type, data = veg_data_with_temps)
summary(lm_model2)

AIC(lm_model, lm_model1, lm_model2) # lm_model1 has lowest AIC
AIC(lmer_model1, lm_model1)

VarCorr(lmer_model1)
anova(lmer_model1, lm_model1)

par(mfrow=c(2,2))
plot(lm_model1)

fixef(lmer_model1)

lmer_model_simpler <- lmer(mean_temp ~ season + (1 | transect) + (1 | quadrat), data = veg_data_with_temps)
summary(lmer_model_simpler)

AIC(lmer_model1, lmer_model_simpler)

#-------------------------------------------------------------------------------

# predict function
# grass cover
m_gc <-  lm(mean_temp ~ grass_cover + season, data=veg_data_with_temps) 
veg_data_with_temps$mt_gc <- predict(m_gc, newdata = data.frame(grass_cover=veg_data_with_temps$grass_cover, season=veg_data_with_temps$season))

# rock cover
m_rc <-  lm(mean_temp ~ rock_cover + season, data=veg_data_with_temps) 
veg_data_with_temps$mt_rc <- predict(m_rc, newdata = data.frame(rock_cover=veg_data_with_temps$rock_cover, season=veg_data_with_temps$season))

# forb cover
m_fc <-  lm(mean_temp ~ forb_cover + season, data=veg_data_with_temps) 
veg_data_with_temps$mt_fc <- predict(m_fc, newdata = data.frame(forb_cover=veg_data_with_temps$forb_cover, season=veg_data_with_temps$season))

# thatch cover
m_tc <- lm(mean_temp ~ thatch_cover + season, data=veg_data_with_temps) 
veg_data_with_temps$mt_tc <- predict(m_tc, newdata = data.frame(thatch_cover=veg_data_with_temps$thatch_cover, season=veg_data_with_temps$season))

# bare ground
m_bg <-  lm(mean_temp ~ bare_ground + season, data=veg_data_with_temps)
veg_data_with_temps$mt_bg <- predict(m_bg, newdata = data.frame(bare_ground=veg_data_with_temps$bare_ground, season=veg_data_with_temps$season))

# thatch depth
m_td <-  lm(mean_temp ~ thatch_depth + season, data=veg_data_with_temps)
veg_data_with_temps$mt_td <- predict(m_td, newdata = data.frame(thatch_depth=veg_data_with_temps$thatch_depth, season=veg_data_with_temps$season))

# grass height
m_gh <-  lm(mean_temp ~ ave_grass_height + season, data=veg_data_with_temps)
veg_data_with_temps$mt_gh <- predict(m_gh, newdata = data.frame(ave_grass_height=veg_data_with_temps$ave_grass_height, season=veg_data_with_temps$season))

head(veg_data_with_temps)

#write.csv(veg_data_with_temps, "avail3_1.csv")

veg_long <- veg_data_with_temps |>
  pivot_longer(
    cols = starts_with("mt_"), 
    names_to = "predicted_temp_var", 
    values_to = "predicted_temp"
  ) |>
  mutate(
    vegetation_var = case_when(
      predicted_temp_var == "mt_gc" ~ "grass_cover",
      predicted_temp_var == "mt_rc" ~ "rock_cover",
      predicted_temp_var == "mt_fc" ~ "forb_cover",
      predicted_temp_var == "mt_tc" ~ "thatch_cover",
      predicted_temp_var == "mt_bg" ~ "bare_ground",
      predicted_temp_var == "mt_td" ~ "thatch_depth",
      predicted_temp_var == "mt_gh" ~ "ave_grass_height"
    )
  )

veg_long <- veg_long |>
  dplyr::mutate(season = factor(season, levels = c("summer", "autumn", "spring")))

veg_long <- veg_long |>
  dplyr::mutate(vegetation_var = factor(vegetation_var, levels = c(
    "ave_grass_height","grass_cover", 
    "forb_cover", "rock_cover","bare_ground", 
    "thatch_cover","thatch_depth"
  )))


remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm=TRUE)
  Q3 <- quantile(x, 0.75, na.rm =TRUE)
  IQR <- Q3-Q1
  return(x >= (Q1-1.5*IQR) & x <= (Q3 + 1.5 * IQR))
}


veg_long_filtered$
veg_long_filtered <- veg_long |>
  group_by(season, vegetation_var) |>
  filter(remove_outliers(predicted_temp)) |>
  ungroup()

season_colors <- c("summer" = "red", "autumn" = "blue", "spring" = "grey50")

veg_long_filtered <- veg_long_filtered |>
  mutate(vegetation_label = str_replace_all(vegetation_var, "_", " ") |> 
           str_to_title())

pp2 <- ggplot(veg_long_filtered, aes(x = vegetation_label, y = predicted_temp, fill = season)) +
  geom_boxplot(outlier.size = 1) +
  theme_classic(base_size = 12) +
  labs(
    title = "B)",
    x = "Vegetation variables",
    y = "Predicted Mean Temperature (°C)",
    fill = "Season"
  ) +
  theme(legend.position="bottom",axis.text.x = element_text(angle = 15, hjust = 1), plot.title=element_text(hjust=-0.05))+
  scale_fill_manual(values = season_colors, labels=c("Summer 2023", "Autumn 2023", "Spring 2024")) +
  scale_y_continuous(breaks=seq(15,30, by=5),
                     limits=c(15,30))

pp1 <- ggplot(veg_long_filtered, aes(y = mean_temp, x = vegetation_label, fill=season)) +
  geom_boxplot() +
  #facet_wrap(~season)+
  theme_classic(base_size = 12)+
  theme(legend.position = "none",axis.text.x=element_blank(), plot.title=element_text(hjust=-0.05))+
  labs(x="", y = "Mean Temperature (°C)", fill = "Season", title="A)")+
  scale_fill_manual(values = season_colors, labels=c("Summer 2023", "Autumn 2023", "Spring 2024")) +
  scale_y_continuous(breaks=seq(15,30, by=5),
                     limits=c(15,30))+
  theme(plot.margin = margin(0.5, 1, 0.5, 1,"cm"))

pp1/pp2
combined_plot1 <- pp1/pp2
combined_plot1

#ggsave("microhabitattemps.png", combined_plot1, device="png", dpi=600)

#-------------------------------------------------------------------------------
cor_matrix <- cor(veg_data_with_temps |>
                    select(grass_cover, rock_cover, forb_cover, thatch_depth, thatch_cover, ave_grass_height, mean_temp, sd_temp), 
                  use = "complete.obs")

library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper")

#------------------transform veg data to long format for plotting---------------

veg_data_long <- veg_data_with_temps |>
  pivot_longer(cols = c(grass_cover, forb_cover, thatch_cover, bare_ground, rock_cover, ave_grass_height, thatch_depth),
               names_to = "veg_variable", values_to = "veg_value")

ggplot(veg_data_long, aes(x = veg_value, fill = season)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  #facet_wrap(~veg_variable, scales = "free") +
  theme_classic()

# plots

ggplot(veg_data_long, aes(x = veg_value, y = mean_temp, color = season)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Mean Temperature vs Vegetation Variables by Season",
       x = "Vegetation Value",
       y = "Mean Temperature (°C)") +
  theme_classic() +
  facet_wrap(~ veg_variable, scales = "free_x") +
  theme(legend.position = "top") +
  scale_color_manual(values = c("spring" = "green", "summer" = "red", "autumn" = "orange"))

#--------------------------------remove outliers--------------------------------
# Remove outliers in spring for grass height and thatch depth, then re-plot

veg_data_filtered <- veg_data_long |>
  filter(!(veg_variable == "ave_grass_height" & veg_value > 40 & season == "spring")) |>
  filter(!(veg_variable == "thatch_depth" & veg_value > 3 & season == "spring")) |>
  filter(mean_temp <= 40)  # Remove extreme high temperatures


#--------------------plots without outliers-------------------------------------

# Mean temperature plot
ggplot(veg_data_filtered, aes(x = veg_value, y = mean_temp, color = season)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Vegetation Value", y = "Mean Temperature (°C)") +
  theme_classic() +
  facet_wrap(~ veg_variable, scales = "free_x") +
  theme(legend.position = "top") +
  scale_color_manual(values = c("spring" = "green", "summer" = "red", "autumn" = "orange"))

# Max temperature plot
ggplot(veg_data_filtered, aes(x = veg_value, y = max_temp, color = season)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Vegetation Value", y = "Max Temperature (°C)") +
  theme_classic() +
  facet_wrap(~ veg_variable, scales = "free_x") +
  theme(legend.position = "top") +
  scale_color_manual(values = c("spring" = "green", "summer" = "red", "autumn" = "orange"))

# Min temperature plot
ggplot(veg_data_filtered, aes(x = veg_value, y = min_temp, color = season)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Vegetation Value", y = "Min Temperature (°C)") +
  theme_classic() +
  facet_wrap(~ veg_variable, scales = "free_x") +
  theme(legend.position = "top") +
  scale_color_manual(values = c("spring" = "green", "summer" = "red", "autumn" = "orange"))

#----------------------linear model and coeff est plot--------------------------

veg_data_filtered$season <- factor(veg_data_filtered$season, levels = c("summer", "autumn", "spring"), labels =c("Summer 2023", "Autumn 2023", "Spring 2024"))


lm_results <- veg_data_filtered |>
  filter(veg_variable !="thatch_depth") |>
  mutate(veg_variable = str_to_title(gsub("_", " ", veg_variable))) |>
  group_by(season, veg_variable) |>
  summarise(
    model = list(lm(mean_temp ~ veg_value, data = cur_data())),
    coef_estimate = coef(model[[1]])[2],
    std_error = summary(model[[1]])$coefficients[2, 2],
    p_value = summary(model[[1]])$coefficients[2, 4],
    .groups = "drop"
  ) |>
  mutate(significant = p_value < 0.05)

print(lm_results, n=18)

ggplot(lm_results, aes(x = veg_variable, y = coef_estimate, color = season)) +
  geom_point(aes(shape = significant), size = 4, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = coef_estimate - 1.96 * std_error, ymax = coef_estimate + 1.96 * std_error),
                width = 0.2, position = position_dodge(width = 0.5)) +  
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Vegetation Variables",
       y = "Coefficient Estimate",
       shape="Significance:") +
  facet_wrap(~season) +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")+
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73"), 
                     labels= c("Summer 2023", "Autumn 2023", "Spring 2024"), name="Season:")+
  scale_shape_manual(values=c(16, 17),
                     labels =c("Not Significant", "Significant"))


# as grass height decreases, mean temperature increases - significant for all seasons
# as rock cover increases, mean temp increases - autumn only
# as thatch depth decreases, mean temp increases for summer and autumn only
# increased bare ground = increased temps for summer


full_model <- lm(mean_temp ~ veg_variable * season, data = veg_data_filtered)
summary(full_model)

refined_model <- lm(mean_temp ~ veg_variable + season, data = veg_data_filtered)
summary(refined_model)

ggplot(veg_data_filtered, aes(x = season, y = mean_temp, fill = season)) +
  geom_boxplot() +
  labs(title = "Temperature Differences Across Seasons",
       x = "Season",
       y = "Mean Temperature (°C)") +
  theme_classic()

ggplot(veg_data_filtered, aes(x = veg_value, y = mean_temp, color = season)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ veg_variable, scales = "free") +
  labs(title = "Vegetation Variables vs Temperature Across Seasons",
       x = "Vegetation Value",
       y = "Mean Temperature (°C)") +
  theme_classic()


#-------------------------correlation plots-------------------------------------

table(veg_data_filtered$veg_variable)
# Convert from long to wide format
veg_data_wide <- veg_data_filtered |>
  pivot_wider(
    names_from = veg_variable,   # The column that contains variable names
    values_from = veg_value      # The column with values for each variable
  )


# filter data set by seasons
spring <- filter(veg_data_wide, season == "spring")
summer <- filter(veg_data_wide, season == "summer")
autumn <- filter(veg_data_wide, season == "autumn")


#---------------------spring corrplot-------------------------------------------
 

#spring
cor_data_spring <- spring |>
  dplyr::select(mean_temp, max_temp, min_temp, grass_cover, forb_cover, thatch_cover, 
                bare_ground, rock_cover, ave_grass_height, thatch_depth)

# Calculate Pearson correlation matrix
cor_spring_res <- rcorr(as.matrix(cor_data_spring), type="pearson")

cor_matrix_spr <- cor_spring_res$r
p_matrix_spr <- cor_spring_res$P
p_matrix_spr[is.na(p_matrix_spr)] <- 1 # handle missing p values

# plot
corrplot::corrplot(cor_matrix_spr, method = "color", type = "upper", 
                   tl.col = "black", tl.srt = 45, addCoef.col = "black", 
                   p.mat = p_matrix_spr, sig.level = 0.05, insig = "blank", title="")
# values close to 1 indicate strong positive correlation
# values close to -1 indicate strong negative correlation

#-----------------------summer corrplot-----------------------------------------

#summer
cor_data_summer <- summer |>
  dplyr::select(mean_temp, max_temp, min_temp, grass_cover, forb_cover, thatch_cover, 
                bare_ground, rock_cover, ave_grass_height, thatch_depth)

# Calculate Pearson correlation matrix
cor_sum_res <- rcorr(as.matrix(cor_data_summer), type="pearson")

cor_matrix_sum <- cor_sum_res$r
p_matrix_sum <- cor_sum_res$P
p_matrix_sum[is.na(p_matrix_sum)] <- 1  # Handle missing p-values

# plot
corrplot::corrplot(cor_matrix_sum, method = "color", type = "upper", 
                   tl.col = "black", tl.srt = 45, addCoef.col = "black", 
                   p.mat = p_matrix_spr, sig.level = 0.05, insig = "blank", title="")
# values close to 1 indicate strong positive correlation
# values close to -1 indicate strong negative correlation

#------------------------autumn corrplot----------------------------------------

#autumn
cor_data_aut <- autumn |>
  dplyr::select(mean_temp, max_temp, min_temp, grass_cover, forb_cover, thatch_cover, 
                bare_ground, rock_cover, ave_grass_height, thatch_depth)

# Calculate Pearson correlation matrix
cor_aut_res <- rcorr(as.matrix(cor_data_aut), type="pearson")

cor_matrix_aut <- cor_aut_res$r
p_matrix_aut <- cor_aut_res$P
p_matrix_aut[is.na(p_matrix_aut)] <- 1 # Handle missing p-values

# plot
corrplot::corrplot(cor_matrix_aut, method = "color", type = "upper", 
                   tl.col = "black", tl.srt = 45, addCoef.col = "black", title="",
                   p.mat=p_matrix_aut, sig.level = 0.05, insig = "blank")
# values close to 1 indicate strong positive correlation
# values close to -1 indicate strong negative correlation


#  values ranging from -1 to 1 
# positive coefficients = when value of one variable increases so does the value of the other variable
# negative coefficients = when value of one increases, the other decreases.


#----------differences in temperature between habitat types---------------------

veg_data_wide$habitat_type <- as.factor(veg_data_wide$Habitat_type)
veg_data_wide$season <- as.factor(veg_data_wide$season)


anova_meantemp <- aov(mean_temp ~ Habitat_type+season, data=veg_data_wide)
anova_results <- Anova(anova_meantemp, type = "III")
# mean temp differs significantly between habitat types, even when controlling for season
# mean temp varies significantly across seasons

TukeyHSD(anova_meantemp)
# NTG higher temps than NP
# Phalaris lower temps than NP and NTG
# summer temps higher than spring and summer
# autumn temp lower than spring and summer
# spring temps higher than autumn, lower than summer
# all significant

anova_meantemp_int <- aov(mean_temp ~ Habitat_type * season, data = veg_data_wide)
summary(anova_meantemp_int)
# habitat type and season both significant
# habitat_type:season interaction not significant

ggplot(veg_data_wide, aes(x = Habitat_type, y = mean_temp, fill = season)) +
  geom_boxplot() +
  labs(title = "Mean Temperature Across Habitat Types and Seasons",
       x = "Habitat Type",
       y = "Mean Temperature (°C)") +
  theme_classic()


#-------------------------------------------------------------------------------
#weather station data
setwd("F:/HABITAT ANALYSIS/Used")
weatherst <- read.csv("AmtechWeatherLink-2022-2024.csv")
glimpse(weatherst)

summary(weatherst)

plot(weatherst$Temp_out)

head(weatherst)

weatherst$DateTime <- as.POSIXct(paste(weatherst$Date, weatherst$Time), format = "%d/%m/%Y %I:%M %p")

weatherst <- weatherst[order(weatherst$DateTime),]

# Assign seasons
weatherst$Season <- case_when(
  format(weatherst$DateTime, "%m") %in% c("12", "01", "02") ~ "Summer",
  format(weatherst$DateTime, "%m") %in% c("03", "04", "05") ~ "Autumn",
  format(weatherst$DateTime, "%m") %in% c("06", "07", "08") ~ "Winter",
  TRUE ~ "Spring"
)

weatherst$Season <- as.factor(weatherst$Season)

# Define sunrise and sunset times by season
weatherst <- weatherst |>
  mutate(
    Time = hms::as_hms(format(DateTime, "%H:%M:%S")),
    Sunrise = case_when(
      Season == "Spring" ~ hms::as_hms("06:00:00"),
      Season == "Summer" ~ hms::as_hms("05:30:00"),
      Season == "Autumn" ~ hms::as_hms("06:30:00"),
      Season == "Winter" ~ hms::as_hms("07:00:00")
    ),
    Sunset = case_when(
      Season == "Spring" ~ hms::as_hms("19:30:00"),
      Season == "Summer" ~ hms::as_hms("20:30:00"),
      Season == "Autumn" ~ hms::as_hms("18:30:00"),
      Season == "Winter" ~ hms::as_hms("17:30:00")
    )
  )

# Filter out nighttime data
weatherst_daytime <- weatherst |>
  filter(Time >= Sunrise & Time <= Sunset)


# Visualisation: Daytime temperature trend
ggplot(weatherst_daytime, aes(DateTime, y = Temp_out)) +
  geom_line(color = "blue") +
  theme_classic() +
  labs(title = "Daytime Temperature Over Time",
       x = "Date-Time",
       y = "Temperature (°C)")


# Daily summary of daytime data
daily_summary_daytime <- weatherst_daytime |>
  group_by(Date = as.Date(DateTime)) |>
  summarise(
    mean_temp = mean(Temp_out, na.rm = TRUE),
    min_temp = min(Temp_out, na.rm = TRUE),
    max_temp = max(Temp_out, na.rm = TRUE)
  ) |>
  glimpse()

library(forecast)
ts_data <- ts(weatherst_daytime$Temp_out, frequency = 48)
decomposed_daytime <- decompose(ts_data)
plot(decomposed_daytime)

library(zoo)
weatherst_daytime$Temp_out_ma <- rollmean(weatherst_daytime$Temp_out, k=48, fill=NA)

cor(weatherst_daytime$Temp_out, weatherst_daytime$Out_Hum, use = "complete.obs")
# - 0.617

lm_temp_daytime <- lm(Temp_out ~ DateTime, data = weatherst_daytime)
summary(lm_temp_daytime)

# forcasting with ARIMA (datetime data)
auto_model_daytime <- auto.arima(ts_data)
forecast(auto_model_daytime, h = 48) |>
  autoplot()


#---------------------------ambient v ground -----------------------------------


# check date/time formats - make consistent

# convert ibutton data to POSIXct
meta_ibutton_daytime <- meta_ibutton_daytime |>
  mutate(DateTime = as.POSIXct(`date/time`, format="%Y-%m-%d %H:%M:%S"))|>
  filter(year(DateTime) !=2022)

# ensure weather station DateTime in properly formatted
weatherst_daytime <- weatherst_daytime|>
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))|>
  filter(year(DateTime) !=2022)


# aggregate to common time scale eg hourly/daily

# ibutton
ibutton_daily <- meta_ibutton_daytime |>
  group_by(Date = as.Date(DateTime)) |>
  summarise(mean_temp_ibutton = mean(Temperature, na.rm=TRUE),
            min_temp_ibutton = min(Temperature, na.rm=TRUE),
            max_temp_ibutton = max(Temperature, na.rm=TRUE))

# weather station
weather_daily <- weatherst_daytime |>
  group_by(Date = as.Date(DateTime))|>
  summarise(mean_temp_weather = mean(Temp_out, na.rm=TRUE),
            min_temp_weather = min(Temp_out, na.rm=TRUE),
            max_temp_weather = max(Temp_out, na.rm=TRUE))

# merge datasets

combined_temp <- left_join(ibutton_daily, weather_daily, by="Date")

# MEAN: correlation analysis between ibutton and weather stations temps
correlation_results <- cor(combined_temp$mean_temp_ibutton, combined_temp$mean_temp_weather, use="complete.obs")
print(paste("Correlation between iButton and Weather Station temperatures:", round(correlation_results, 3)))
# -0.09 = very weak/negative and non-linear

# MIN: correlation analysis between ibutton and weather stations temps
correlation_results_min <- cor(combined_temp$min_temp_ibutton, combined_temp$min_temp_weather, use="complete.obs")
print(paste("Correlation between iButton and Weather Station temperatures (min):", round(correlation_results_min, 3)))
# -0.189

# MAX: correlation analysis between ibutton and weather stations temps
correlation_results_max <- cor(combined_temp$max_temp_ibutton, combined_temp$max_temp_weather, use="complete.obs")
print(paste("Correlation between iButton and Weather Station temperatures (max):", round(correlation_results_max, 3)))
# -0.204

ggplot(combined_temp, aes(x=mean_temp_weather, y=mean_temp_ibutton)) +
  geom_point(color="blue", alpha = 0.6) +
  geom_smooth(method="lm", se=TRUE, color="red") +
  labs(title="Correlation between ibutton and weather station mean daily temps",
       x="weather station mean daily temp",
       y="ibutton mean daily temp")+
  theme_classic()

ggplot(combined_temp, aes(x=min_temp_weather, y=min_temp_ibutton)) +
  geom_point(color="blue", alpha = 0.6) +
  geom_smooth(method="lm", se=TRUE, color="red") +
  labs(title="Correlation between ibutton and weather station min daily temps",
       x="weather station min daily temp",
       y="ibutton mean daily temp")+
  theme_classic()

ggplot(combined_temp, aes(x=max_temp_weather, y=max_temp_ibutton)) +
  geom_point(color="blue", alpha = 0.6) +
  geom_smooth(method="lm", se=TRUE, color="red") +
  labs(title="Correlation between ibutton and weather station max daily temps",
       x="weather station max daily temp",
       y="ibutton mean daily temp")+
  theme_classic()

# linear modelling
# mean
temp_lm <- lm(mean_temp_ibutton~mean_temp_weather, data=combined_temp)
summary(temp_lm) # p = > 0.05

#min
temp_lm_min <- lm(min_temp_ibutton~min_temp_weather, data=combined_temp)
summary(temp_lm_min) # p = > 0.05

#max
temp_lm_max <- lm(max_temp_ibutton~max_temp_weather, data=combined_temp)
summary(temp_lm_max) # p = > 0.05

# seasonal

# add Seasons to combined data

combined_temp <- combined_temp |>
  mutate(Season = case_when(
    month(Date) %in% c(12, 1, 2) ~ "Summer",
    month(Date) %in% c(3, 4, 5) ~ "Autumn",
    month(Date) %in% c(6, 7, 8) ~ "Winter",
    TRUE ~ "Spring"
  ))

combined_temp$Season <- factor(combined_temp$Season, levels = c("Summer", "Autumn", "Winter", "Spring"))

#------------------- Seasonal Correlation Analysis ----------------------------

# iButton data: Assign seasons
meta_ibutton_daytime <- meta_ibutton_daytime %>%
  mutate(Season = case_when(
    month(DateTime) %in% c(12, 1, 2) ~ "Summer",
    month(DateTime) %in% c(3, 4, 5) ~ "Autumn",
    month(DateTime) %in% c(6, 7, 8) ~ "Winter",
    TRUE ~ "Spring"
  ))

# Weather station data: Assign seasons
weatherst_daytime <- weatherst_daytime %>%
  mutate(Season = case_when(
    month(DateTime) %in% c(12, 1, 2) ~ "Summer",
    month(DateTime) %in% c(3, 4, 5) ~ "Autumn",
    month(DateTime) %in% c(6, 7, 8) ~ "Winter",
    TRUE ~ "Spring"
  ))


# from BoM - Canberra airport
autumn_weather_data <- data.frame(
  Season = rep("Autumn"),
  mean_temp_weather = c(13.38),
  min_temp_weather = c(-5.4),
  max_temp_weather = c(37.4))


meta_ibutton_daytime$Season <- factor(meta_ibutton_daytime$Season, levels = c("Summer", "Autumn", "Winter", "Spring"))
weatherst_daytime$Season <- factor(weatherst_daytime$Season, levels = c("Summer", "Autumn", "Winter", "Spring"))

# iButton seasonal summary
ibutton_seasonal <- meta_ibutton_daytime %>%
  group_by(Season) %>%
  summarise(
    mean_temp_ibutton = mean(Temperature, na.rm = TRUE),
    min_temp_ibutton  = min(Temperature, na.rm = TRUE),
    max_temp_ibutton  = max(Temperature, na.rm = TRUE)
  )

weather_seasonal <- weatherst_daytime %>%
  group_by(Season) %>%
  summarise(
    mean_temp_weather = mean(Temp_out, na.rm = TRUE),
    min_temp_weather  = min(Temp_out, na.rm = TRUE),
    max_temp_weather  = max(Temp_out, na.rm = TRUE)
  )

weather_seasonal_updated <- weather_seasonal %>%
  bind_rows(
    autumn_weather_data %>%
      group_by(Season) %>%
      summarise(
        mean_temp_weather = mean(mean_temp_weather),
        min_temp_weather  = mean(min_temp_weather),
        max_temp_weather  = mean(max_temp_weather)
      )
  )

seasonal_combined <- left_join(ibutton_seasonal, weather_seasonal_updated, by = "Season")

print(seasonal_combined)


# Correlation for Mean Temperatures
mean_corr <- cor(seasonal_combined$mean_temp_ibutton, seasonal_combined$mean_temp_weather, use = "complete.obs")
print(paste("Seasonal Mean Temperature Correlation:", round(mean_corr, 3)))
# 0.99

# Correlation for Min Temperatures
min_corr <- cor(seasonal_combined$min_temp_ibutton, seasonal_combined$min_temp_weather, use = "complete.obs")
print(paste("Seasonal Min Temperature Correlation:", round(min_corr, 3)))
# 0.992

# Correlation for Max Temperatures
max_corr <- cor(seasonal_combined$max_temp_ibutton, seasonal_combined$max_temp_weather, use = "complete.obs")
print(paste("Seasonal Max Temperature Correlation:", round(max_corr, 3)))
# 0.045

# Plot Mean Temperature Comparison
ggplot(seasonal_combined, aes(x = mean_temp_weather, y = mean_temp_ibutton, label = Season)) +
  geom_point(color = "blue", size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  geom_text(vjust = -1) +
  labs(title = "Seasonal Mean Temperature: iButton vs Weather Station",
       x = "Weather Station Mean Temperature (°C)",
       y = "iButton Mean Temperature (°C)") +
  theme_classic()

# Plot Max Temperature Comparison
ggplot(seasonal_combined, aes(x = max_temp_weather, y = max_temp_ibutton, label = Season)) +
  geom_point(color = "darkgreen", size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  geom_text(vjust = -1) +
  labs(title = "Seasonal Max Temperature: iButton vs Weather Station",
       x = "Weather Station Max Temperature (°C)",
       y = "iButton Max Temperature (°C)") +
  theme_classic()

ggplot(seasonal_combined, aes(x = min_temp_weather, y = min_temp_ibutton, label = Season)) +
  geom_point(color = "darkgreen", size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  geom_text(vjust = -1) +
  labs(title = "Seasonal Min Temperature: iButton vs Weather Station",
       x = "Weather Station Min Temperature (°C)",
       y = "iButton Min Temperature (°C)") +
  theme_classic()

# linear modelling
# mean
temp_lm <- lm(mean_temp_ibutton~mean_temp_weather, data=seasonal_combined)
summary(temp_lm) # p = < 0.05

#min
temp_lm_min <- lm(min_temp_ibutton~min_temp_weather, data=seasonal_combined)
summary(temp_lm_min) # p = > 0.05

#max
temp_lm_max <- lm(max_temp_ibutton~max_temp_weather, data=seasonal_combined)
summary(temp_lm_max) # p = > 0.05
#-------------------------------------------------------------------------------


