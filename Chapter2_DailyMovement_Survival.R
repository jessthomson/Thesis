library(tidyr)
library(gridExtra)
library(dplyr)
library(lubridate)
library(ggplot2)
library(pscl)
library(gamlss)
library(adehabitatHR)
library(sp)
library(sf)
library(survival)
library(survminer)
library(glmmTMB)
library(lmerTest)

#-------------------------------------------------------------------------------

setwd("F:/DAILY MOVEMENT AND SURVIVAL ANALYSIS/data")

fixed <- read.csv("Fixed data.csv") |> #fixed variables dataset ie sex, release type, season, id
  filter(!is.na(pit_id)) |>
  mutate(date_released=dmy(date_released),
         birth_year = dmy(birth_year))|>
  glimpse()

table(fixed$tracked) # 64 tracked, 16 not tracked
table(fixed$tracked,fixed$release_season, fixed$sex) # 11 M spr24, # 6 F spr24 
table(fixed$tracked, fixed$release_season, fixed$release_type, fixed$sex)


gps <- read.csv("gpsdata.csv") |> #gps points dataset
  filter(!is.na(pit_id)) |>
  mutate(date = dmy(date)) |>
  glimpse()

gps<-gps[, !(names(gps) %in% c("id","ele", "collector","gps","cmt"))]

session <- read.csv("Session data raw JT.csv") |> #session dataset - variables measured etc
  mutate(date = dmy(date)) |>
  filter(!is.na(pit_id)) |>
  glimpse()

session <- session[, !(names(session) %in% c("collector","gps","cmt", "gps.owner"))] # remove unused columns

#-------------------------------------------------------------------------------


#join the two datasets (session and gps)
df <- left_join(session, gps, by=c("pit_id", "date", "session"))|> #join session df with gps df
  glimpse()

table(df$pit_id)

#remove juveniles
df <- df|>
  filter(!(pit_id %in% c("10", "16", "2", "28", "32", "7")))|>
  glimpse()

df <- full_join(df, fixed, by="pit_id")
#df <- df |> mutate(status=ifelse(is.na(status), 0, status))

df <- df[df$tracked=="y", ] #remove individuals that were not tracked
length(unique(df$pit_id)) # 64

colnames(df)
df <- df[, !(names(df) %in% c("mz_id","facility","birth_year", "telemetry_id", "telemetry.id", 
                              "age", "grid", "enclosure"))]


table(df$status, df$release_season)
# 3 aut, 2 spr 22, 15 spr 24, 5 sum

colnames(df)

df_cleaned <-df[,-c(5:16)] # remove veg variables as not using in this analysis

colnames(df_cleaned)

length(unique(df_cleaned$pit_id)) # 64

table(df_cleaned$status, df_cleaned$release_season)
# 3 deaths in autumn 2023
# 2 in spring 2022
# 15 in spring 2024
# 5 in summer 2023
table(df_cleaned$status, df_cleaned$release_type)
# soft had 13 deaths - hard had 12 deaths
table(df_cleaned$status, df_cleaned$sex)


df_cleaned <- df_cleaned |>
  mutate(date_released = as.Date(date_released), # change dates into date format
         date=as.Date(date))

# get days since released/door opened by using last date and minimum date of individual
df_cleaned1 <- df_cleaned |>
  group_by(pit_id) |>
  mutate(days_since_release = as.numeric(date - date_released))|>
  ungroup() |>
  glimpse()

summary(df_cleaned1$days_since_release)
table(df_cleaned1$status, df_cleaned1$release_type)
# 11 soft, 12 hard

# remove data after 42 days from individuals 
df_cleaned1 <- df_cleaned1 |>
  filter(days_since_release <=42)|>
  glimpse()

df_cleaned1 |>
  group_by(pit_id) |>
  summarise(tracking_days = n_distinct(days_since_release)) |>
  arrange(desc(tracking_days))|>
  print(n=63)

#new column called last date
df_cleaned1 <- df_cleaned1 |>
  group_by(pit_id)|>
  mutate(last_date = max(date))|>
  glimpse()


length(unique(df_cleaned1$pit_id)) # 63 

#-------------------------------------------------------------------------------
# remove spring 2022

df_cleaned1 <- df_cleaned1[df_cleaned1$release_season !="spring2022",]
table(df_cleaned1$release_season)

# use this df (df_cleaned1) for the survival analysis

#-------------------------------------------------------------------------------


filtered_df <- df_cleaned1 |> 
  drop_na(lon)

summary(df_cleaned1$lon)
# NA lat/lon are individuals still in enclosures. 
#removed NA because otherwise can't transform into spatial coords

#----------------------------Start data analysis--------------------------------


#create coordinates data frame lat lon
coords <- filtered_df[,c("lon", "lat")]
head(coords)

length(unique(filtered_df$pit_id)) # 52 deleted 8 because deleted spring22 data


#-------------------------------------------------------------------------------

#object for desired projection WGS84
coord.sp <- SpatialPoints(coords, CRS("+proj=longlat +ellps=WGS84")) 
#transform coordinates into UTM
xy <-spTransform(coord.sp, CRS( "+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84") )
coordinates(xy)

#add x y coordinates to joined dataframe
filtered_df$x <- coordinates(xy)[,1] 
filtered_df$y <- coordinates(xy)[,2]

#store data frame as Simple Feature (sf) data frame with geometry coords
gps_sf <- st_as_sf(as.data.frame(filtered_df), coords = c("x","y") )
st_crs(gps_sf)<- ("+proj=utm +zone=55 +south +datum=WGS84 +units=m +no_defs")

length(unique(filtered_df$pit_id)) # 52

plot(filtered_df$x, filtered_df$y)

#plot release season
ggplot(filtered_df, aes(x=x, y=y, colour=as.factor(release_type)))+
  geom_point()+
  theme_classic()+
  xlab("Area (m)")+
  ylab("Area (m)")

library(plotly)

plotly_plot <- plot_ly(data=filtered_df, x=~x, y=~y,
                       type="scatter", mode="markers", color=~as.factor(release_type), 
                       text=~paste("pit_id:", pit_id),
                       hoverinfo = "text") |>
  layout(title="interactive pot with hover info",
         xaxis = list(title="Area (m)", yaxis = list(title="Area (m)"),
                      showlegend =TRUE))

plotly_plot

# xlab(expression("(text"^2*")") - for superscript

#-------------------------------------------------------------------------------
# Group data by individual and date, and then calculate daily movement in m
dat <- filtered_df |>
  group_by(pit_id) |>
  arrange(date) |>
  mutate(
    # Calculate the differences in x and y coordinates
    diff_x = c(NA, diff(x)),
    diff_y = c(NA, diff(y)),
    # Calculate daily movement as the Euclidean distance between points
    daily_movement = sqrt(diff_x^2 + diff_y^2)
  )

#-------------------------------------------------------------------------------
# data exploration

plot(dat$daily_movement~dat$date)

plot(dat$date, dat$daily_movement, col=as.numeric(as.factor(dat$release_type)))
tapply(dat$days_since_release, dat$release_type,  summary)

boxplot(dat$daily_movement ~dat$days_since_release)
boxplot(dat$daily_movement ~dat$days_since_release+dat$release_type)
plot(dat$days_since_release, dat$daily_movement, col=as.numeric(as.factor(dat$release_type)))


# histogram checks for normality
hist(log(dat$daily_movement)) # adding a log makes the histogram normal

# keep as inflated because 0s are structurally important - i.e., animal didn't move because of reasons.
hist(dat$daily_movement)


ggplot(dat, aes(x = days_since_release, y = log(daily_movement), group = pit_id, colour=release_type)) +
  geom_point()+
  theme_classic()

# Count unique individuals based on sex
unique_individuals <- dat |> 
  distinct(pit_id, sex, release_season, release_type)
table(unique_individuals$sex)
table(unique_individuals$release_type)
table(unique_individuals$release_season)

table(dat$status, dat$sex)
# 7 females, 15 males
# rate of survival

table(dat$status, dat$release_season)

#-------------------------------------------------------------------------------
#get average daily movement for each individual

datf <- dat |>
  group_by(pit_id)|>
  mutate(avg_movement = mean(daily_movement, na.rm=TRUE))|>
  glimpse()

datf$avg_movement <-NULL

glimpse(datf)

mean_move_per_id <- datf |>
  group_by(pit_id, release_type, release_season, sex) |>
  summarise(avg_movement = mean(daily_movement, na.rm=TRUE))|>
  glimpse()

hist(mean_move_per_id$avg_movement) #zero inflated


#----------------Daily movement Analyses----------------------------------------

#keeping zeros are they are significant to the structure


datf <- dat |>
  filter(!is.na(daily_movement)) # remove NA movement from dataset

# Histogram of daily movement
ggplot(datf, aes(x = daily_movement)) +
  geom_histogram(binwidth = 1, fill = "grey", color = "black", alpha=0.7) +
  theme_classic() +
  labs(title = "Histogram of Daily Movement", x = "Daily Movement", y = "Frequency")

summary(datf$daily_movement)

wilcox.test(daily_movement ~ sex, data=datf) # no difference
wilcox.test(daily_movement ~ release_type, data=datf) # difference 
kruskal.test(daily_movement ~ release_season, data = datf) # difference

aggregate(daily_movement ~ sex, data=datf, FUN = mean)
aggregate(daily_movement ~ release_type+release_season, data=datf, FUN = mean)
aggregate(daily_movement ~ release_season, data=datf, FUN = mean)

datf$sex <- factor(datf$sex, levels = c("F", "M"))

table(datf$release_season)

p1 <- ggplot(datf, aes(x=sex, y=log(daily_movement+1), fill=sex))+
  geom_boxplot()+
  theme_classic()+
  stat_summary(fun = median, geom = "point", shape = 20, size = 3, color = "black")+
  labs(title="a) Logged daily movement of sex",
       x="Sex", y="Logged daily movement", fill="Sex")+
  scale_fill_manual(values=c("F"="brown1", "M"="cornflowerblue"),
                    labels=c("Female", "Male"))+
  scale_x_discrete(labels=c("F" = "Female", "M" = "Male"))

p2 <- ggplot(datf, aes(x=release_type, y=log(daily_movement+1), fill=release_type))+
  geom_boxplot()+
  theme_classic()+
  stat_summary(fun = median, geom = "point", shape = 20, size = 3, color = "black")+
  labs(title="b) Logged daily movement of release types",
       x="Release type", y="Logged daily movement", fill="Release Type")+
  scale_fill_manual(values=c("SOFT"="brown1", "HARD"="cornflowerblue"),
                    labels=c("Hard", "Soft"))+
  scale_x_discrete(labels=c("HARD" = "Hard", "SOFT" = "Soft"))

datf$release_season <- factor(datf$release_season, levels = c("summer2023", "autumn2023", "spring2024"))

p3 <- ggplot(datf, aes(x=release_season, y=log(daily_movement+1), fill=release_season))+
  geom_boxplot()+
  theme_classic()+
  stat_summary(fun = median, geom = "point", shape = 20, size = 3, color = "black")+
  labs(title="c) Logged daily movement of release seasons",
       x="Release Season", y="Logged daily movement", fill="Release Season")+
  scale_fill_manual(values=c("summer2023"="cornflowerblue", "autumn2023"="chartreuse1", "spring2024" = "brown1"),
                    labels=c("Summer 2023", "Autumn 2023", "Spring 2024"))+
  scale_x_discrete(labels=c("summer2023" = "Summer 2023", "autumn2023" = "Autumn 2023", "spring2024" = "Spring 2024"))

combined_plots <- grid.arrange(p1, p2, p3, ncol = 2, nrow = 2)

#----------ZERO-INFLATED GAMMA MODEL--------------------------------------------
#GAMMA because expecting non-linear movement because of seasonal changes

# using zero-inflated gamlss model
datf_clean <- na.omit(datf[, c("daily_movement", "release_type", "release_season", "sex")]) # remove NA values


zig_model <- gamlss(daily_movement ~ release_type + release_season +sex, family = ZAGA, data=datf_clean)


summary(zig_model)


# change pit id to factor
datf$pit_id <- as.factor(datf$pit_id)

# Remove rows with NA values in relevant columns
datf_clean <- na.omit(datf[, c("date","sex", "daily_movement", "release_type", "release_season", "pit_id", "days_since_release")])

#------ZERO-INFLATED GAMM-------------------------------------------------------


# Fit a Zero-Inflated Generalized Additive Mixed Model
zigamm_model <- gamlss(log(daily_movement+1) ~ release_type + release_season + sex + days_since_release + random(pit_id), 
                       family = ZAGA, 
                       data = datf_clean)

summary(zigamm_model)

zigamm_model1 <- gamlss(log(daily_movement+1) ~ release_type + release_season + 
                          days_since_release + days_since_release:release_type + 
                          days_since_release:release_season +random(pit_id), 
                       family = ZAGA, 
                       data = datf_clean)

summary(zigamm_model1)

plot(zigamm_model)
plot(zigamm_model1)

AIC(zigamm_model, zigamm_model1) # model 1 without sex has lower AIC

datf_clean$release_season <- factor(datf_clean$release_season, levels = c("summer2023", "autumn2023", "spring2024"))

p1 <- ggplot(datf_clean |>
               filter(release_type =="SOFT"), aes(x=days_since_release, y=log(daily_movement+1), col=as.factor(release_season)))+
  geom_point(alpha=0.5)+
  geom_smooth(method="loess", se=TRUE)+
  labs(title="Logged daily movement over time of release types across seasons",
       subtitle = "Soft-release", x="", y="", col="Release season")+
  scale_color_manual(values=c("summer2023"="brown1","autumn2023"="chartreuse1", "spring2024"="cornflowerblue"),
                     labels=c("Summer 2023", "Autumn 2023", "Spring 2024"))+
  theme_classic()+
  xlim(0, 30)+
  ylim(0, 5)+
  theme(plot.title=element_text(size=24),
        plot.subtitle = element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=16),
        legend.title=element_text(size=20),
        legend.text=element_text(size=18))+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())

p2 <- ggplot(datf_clean |>
               filter(release_type =="HARD"), aes(x=days_since_release, y=log(daily_movement+1), col=as.factor(release_season)))+
  geom_point(alpha=0.5)+
  geom_smooth(method="loess", se=TRUE)+
  labs(title="Hard-release", x="Days since release", y="Logged daily movement",col="Release season")+
  scale_color_manual(values=c("summer2023"="brown1","autumn2023"="chartreuse1", "spring2024"="cornflowerblue"),
                     labels=c("Summer 2023", "Autumn 2023", "Spring 2024"))+
  theme_classic()+
  xlim(0, 30)+
  ylim(0, 5)+
  theme(plot.title=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=16),
        legend.title=element_text(size=20),
        legend.text=element_text(size=18))


combined_plots <- grid.arrange(p1, p2, nrow = 2) # autumn-soft took longer to leave enclosures

datf_clean$release_season <- as.factor(datf_clean$release_season)

#--------------ZERO DAILY MOVEMENT AVANALYSIS-----------------------------------

datf_clean$release_season <- factor(datf_clean$release_season, levels = c("summer2023", "autumn2023", "spring2024"))
# Calculate the proportion of zero daily movement for each group
zero_proportion <- datf_clean |>
  group_by(release_type, release_season, sex) |>
  summarise(proportion_zero = mean(daily_movement == 0)) |>
  arrange(desc(proportion_zero))

zero_proportion

# find the group with the largest proportion of zero daily movement
largest_zero_proportion_group <- zero_proportion[1,]

largest_zero_proportion_group #soft summer female has largest proportion of 0 daily movement

#------------Hurdle models------------------------------------------------------

# Create a binary variable for non-zero daily movement
datf_clean$non_zero_movement <- ifelse(datf_clean$daily_movement > 0, 1, 0)

# Fit a binary logistic regression model for the likelihood of daily movement

logistic_model1 <- glmer(non_zero_movement ~ release_type + release_season + days_since_release + days_since_release:release_type+
                           days_since_release:release_season+ (1 | pit_id), 
                         data = datf_clean, family = binomial)

summary(logistic_model1) # significantly less likely to move over time

datf_clean$scaled_days_since_release <- scale(datf_clean$days_since_release)

logistic_model2 <- update(logistic_model1, control = glmerControl(optimizer = "bobyqa"))
summary(logistic_model2)

# plot results
datf_clean$predicted_probs <- predict(logistic_model2, type = "response")

# Plot predictions
plot_likelihood <- ggplot(datf_clean, aes(x = days_since_release, y = predicted_probs, color = release_season)) +
  geom_smooth(method = "lm", se = FALSE, size=1.2) +
  facet_wrap(~release_type) +
  #scale_color_manual(values = c("brown1", "chartreuse1", "cornflowerblue")) +
  scale_color_manual(values = c("#FF0000", "#999999", "#0000FF"), 
                     labels= c("Summer 2023", "Autumn 2023", "Spring 2024"))+
  xlim(0,35)+
  labs(
    title = "A)",
    x = "Days",
    y = "Probability of Movement",
    color = "Release Season",
    fill = "Release Season"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
      #axis.text.x = element_blank(),  # Remove x-axis labels
      #axis.ticks.x = element_blank(), # Remove x-axis ticks,
      strip.text = element_text(size = 12, face = "bold"),
      legend.title = element_text(size=12, face="bold") # Make facet labels more prominent
  )

#------------------NON-ZERO DATA (MOVED)----------------------------------------
# Filter out zero daily movements
library(lmerTest)
non_zero_data <- datf_clean |>
  filter(daily_movement > 0) #non-zero data

hist(log(non_zero_data$daily_movement)) # logged made it normally distributed

# Fit a linear mixed model to non-zero movements
non_zero_lmm <- lmer(log(daily_movement) ~ release_type + release_season + days_since_release + 
                       days_since_release:release_type + days_since_release:release_season +
                       (1 | pit_id), 
                     data = non_zero_data)

summary(non_zero_lmm)

# post-hoc to compare spring and autumn
aut_coef <- -0.56234
spr_coef <-  -0.24580
aut_se <-  0.24680
spr_se <- 0.25506

diff <- aut_coef - spr_coef
se_diff <- sqrt(aut_se^2 + spr_se^2)

t_val <- diff/se_diff
2*pt(-abs(t_val), df =73.32)
# p = 0.3753

#(exp(-0.56234)-1) * 100 = -43.0126% 
# autumn daily movement is 43.0126%  less than summer

plot(residuals(non_zero_lmm) ~ fitted(non_zero_lmm))

# Add predicted movement values to data
predicted_values <-predict(non_zero_lmm, non_zero_data, re.form = NA)

non_zero_data$predicted_movement <- exp(predict(non_zero_lmm, non_zero_data, re.form = NA))


plot_movement <- ggplot(non_zero_data, aes(x = days_since_release, y = predicted_movement, color = release_season)) +
  geom_line(size=1.5, method="lm")+
  geom_jitter(data = non_zero_data, 
              aes(x = days_since_release, y = daily_movement), 
              width = 0.5, height = 0, alpha = 0.2, size = 2)+
  
  #geom_smooth(method = "lm", se = FALSE, size=1.2) +
  facet_wrap(~release_type) +
  scale_y_log10(breaks=c(1,10,100)) +
  scale_color_manual(values = c("#FF0000", "#999999", "#0000FF"), 
                                labels= c("Summer 2023", "Autumn 2023", "Spring 2024")) +
  xlim(0,35)+
  #ylim(0,100) +
  labs(
    title = "B)",
    color = "Release Season",
    x = "Days",
    y = "Daily Movement (m)"
  ) +
  theme_classic()+
  theme(legend.position = "right",
        #axis.text.x = element_blank(),  # Remove x-axis labels
        #axis.ticks.x = element_blank(), # Remove x-axis ticks,
        strip.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size=12, face="bold"))


library(patchwork)
combined_plot <- plot_likelihood/ plot_movement +
  theme(plot.title = element_text(size = 16))

combined_plot


#ggsave("F:/DAILY MOVEMENT AND SURVIVAL ANALYSIS/figures/figure_3.png", combined_plot, 
 #      dpi = 301)

#------------------------------HOME RANGE---------------------------------------
colnames(datf)


# filter individuals with less than 5 gps points
HR_df_cleaned <- filtered_df |>
  group_by(pit_id)|> 
  filter(n() >=5) |>
  mutate(pit_id=factor(pit_id))|>
  ungroup()

# create coordinates data frame lat lon
coords <- HR_df_cleaned[,c("lon", "lat")]
coordinates(coords) <- ~lon+lat
proj4string(coords) <- CRS("+proj=longlat +datum=WGS84")

# transform coords to UTM
coord.sp <- SpatialPoints(coords, CRS("+proj=longlat +ellps=WGS84")) 
xy <-spTransform(coord.sp, CRS( "+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84") )
coordinates(xy)
HR_df_cleaned$x <- coordinates(xy)[,1] 
HR_df_cleaned$y <- coordinates(xy)[,2]

# plot
ggplot(HR_df_cleaned, aes(x=x, y=y, colour=release_type))+
  geom_point()+
  theme_classic()

coords_utm <- SpatialPointsDataFrame(xy, data=HR_df_cleaned)

# check ids
length(unique(coords_utm$pit_id)) # 40 ids


#------------------------------ MCP calcs --------------------------------------

# Calculate MCP for each individual
mcp <- mcp(coords_utm[, "pit_id"], percent=95, unout="ha")
coords_sf <- st_as_sf(coords_utm)
plot(mcp, axes=TRUE)
head(mcp)
summary(mcp)


# mcp for release type
mcprt <- mcp(coords_utm[,"release_type"], percent = 95, unout = "ha")
head(mcprt)
summary(mcprt)

# mcp for release season
mcprs <- mcp(coords_utm[,"release_season"], percent = 95, unout = "ha")
head(mcprs)
summary(mcprs)

# mcp for sex
mcpsex <- mcp(coords_utm[,"sex"], percent = 95, unout = "ha")
head(mcpsex)
summary(mcpsex)


mcps_df <- as.data.frame(mcp)
#change id column name
colnames(HR_df_cleaned)[2] <- "id"

mcp_hr_df <- left_join(HR_df_cleaned, mcps_df, by="id")
colnames(mcp_hr_df)

summary(mcp_hr_df)

length(unique(mcp_hr_df$id)) # 40
#--------------------- Habitat shapefile ---------------------------------------

setwd("F:/DAILY MOVEMENT AND SURVIVAL ANALYSIS/data/Shapefiles")
library(RColorBrewer)
# load habitat shapefile
habitat_type_sf <- read_rds("habitat_type_shapefile.rds")

# assign colors to habtiat types
unique_habitats <- unique(habitat_type_sf$habitat_type)
hab_colors <- brewer.pal(n = length(unique_habitats), name = "Pastel1") 
habitat_type_sf$hab_color <- hab_colors[as.numeric(factor(habitat_type_sf$habitat_type))]

# match CRS of coords to habitat shapefile
coords_sf <- st_as_sf(coords_utm)
st_crs(coords_sf) <- st_crs(habitat_type_sf)

coords_with_habitat <- st_join(coords_sf, habitat_type_sf["habitat_type"])

# add habitat type back to main dataframe
HR_df_cleaned$habitat_type <- coords_with_habitat$habitat_type

#-------------------------------------------------------------------------------

head(mcprs@data)
head(coords_utm@data)

table(coords_utm@data$pit_id, coords_utm@data$release_season)

unique(mcprs@data$id)
unique(coords_utm@data$release_season)


#--------------------Analyses between groups------------------------------------

mcp_individuals <- mcp(coords_utm[,"pit_id"], percent=95, unout="ha")
# Extract home range areas and individual IDs
home_range_areas <- data.frame(
  pit_id = rownames(as.data.frame(mcp_individuals)),
  area = as.data.frame(mcp_individuals)$area
)

# Merge with the original data to get group information
home_range_data <- merge(home_range_areas, coords_utm@data, by.x = "pit_id", by.y = "pit_id")

# remove duplicates as only want home range for each individual
home_range_data <- home_range_data |>
  group_by(pit_id) |>
  slice(1) |>
  ungroup()

ggplot(home_range_data, aes(log(area))) +
  geom_histogram(bins=15) +
  theme_classic() + 
  labs(y = "Count", x = "Frequency (area)")

summary(home_range_data)


# Mann-Whitney U Test for release type
wilcox.test(log(area) ~ release_type, data = home_range_data) # p = > 0.05

# Kruskal-Wallis Test for release season
kruskal.test(log(area) ~ release_season, data = home_range_data) # p = < 0.05
library(FSA)
dunnTest(log(area)~release_season, data=home_range_data, method="bonferroni")

# mean area used/home range
aggregate(area ~ sex, data=home_range_data, FUN = mean)
aggregate(area ~ release_type, data=home_range_data, FUN = mean)
aggregate(area ~ release_season, data=home_range_data, FUN = mean)


ggplot(home_range_data, aes(release_season, y=log(area), fill=release_season)) +
  geom_boxplot()+
  theme_classic()

#---------------------- Plot parameters ----------------------------------------


release_season_colours <- c("Summer 2023" = "#E69F00", "Autumn 2023" = "#56B4E9", "Spring 2024" = "#009E73")

mcprs@data$release_season <- mcprs@data$id

mcprs@data$release_season <- gsub("summer2023", "Summer 2023", mcprs@data$release_season)
mcprs@data$release_season <- gsub("autumn2023", "Autumn 2023", mcprs@data$release_season)
mcprs@data$release_season <- gsub("spring2024", "Spring 2024", mcprs@data$release_season)

mcprs@data$border_colours <- release_season_colours[mcprs@data$release_season]


coords_utm@data$release_season <- gsub("summer2023", "Summer 2023", coords_utm@data$release_season)
coords_utm@data$release_season <- gsub("autumn2023", "Autumn 2023", coords_utm@data$release_season)
coords_utm@data$release_season <- gsub("spring2024", "Spring 2024", coords_utm@data$release_season)

coords_utm@data$release_seasons <- release_season_colours[coords_utm$release_season]

release_type_colours <- c("Hard" = "#56B4E9", "Soft" = "#E69F00")
mcprt@data$release_type <- mcprt@data$id
mcprt@data$border_colours <- release_type_colours[mcprt@data$release_type]


mcprt@data$release_type <- gsub("HARD", "Hard", mcprt@data$release_type)
mcprt@data$release_type <- gsub("SOFT", "Soft", mcprt@data$release_type)

mcprt@data$border_colours <- release_type_colours[mcprt@data$release_type]

coords_utm@data$release_type <- gsub("HARD", "Hard", coords_utm@data$release_type)
coords_utm@data$release_type <- gsub("SOFT", "Soft", coords_utm@data$release_type)

coords_utm@data$release_types <- release_type_colours[coords_utm$release_type]

sex_colours <- c("Female" = "#E69F00", "Male" = "#56B4E9")

mcpsex@data$sex <- mcpsex@data$id
coords_utm@data$sex <- gsub("F", "Female", coords_utm@data$sex)
coords_utm@data$sex <- gsub("M", "Male", coords_utm@data$sex)

coords_utm@data$sexes <- sex_colours[coords_utm$sex]

#---------------------- MCP Plots-----------------------------------------------

# plots
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(2, 2, 4, 1))

plot(st_geometry(habitat_type_sf), col=habitat_type_sf$hab_color, border="grey", axes=TRUE)
#plot points on shapefile plot
points(coords_utm, pch=20, col='#E69F00')
legend("topright", legend=unique_habitats, fill=hab_colors, title="Habitat Type")
plot(mcp, pch=20, add=TRUE, border="#E69F00")
#plot(ged.kd, add=TRUE, border = c("blue", "red"))http://127.0.0.1:13771/graphics/bccd0d33-c4fe-4912-85ea-383bc97520b6.png
title(main="A)", adj=0)
title(xlab="Area (m)", ylab="Area (m)")

# Plot MCP for release type
plot(st_geometry(habitat_type_sf), col = habitat_type_sf$hab_color, border = "grey", axes = TRUE)
plot(mcprt, add = TRUE, border = mcprt@data$border_colours, lwd = 2)
points(coords_utm, pch = 20, col = coords_utm@data$release_types, cex = 1.2)
title(main = "B)", adj = 0)
title(xlab = "Area (m)", ylab = "Area (m)")
legend("topright", legend = names(release_type_colours), col = release_type_colours, pch = 20)

# Plot MCP for release season
plot(st_geometry(habitat_type_sf), col = habitat_type_sf$hab_color, border = "grey", axes = TRUE)
plot(mcprs, add = TRUE, border = mcprs@data$border_colours, lwd = 2)
points(coords_utm, pch = 20, col = coords_utm@data$release_seasons, cex = 1.2)
title(main = "C)", adj = 0)
title(xlab = "Area (m)", ylab = "Area (m)")
legend("topright", legend = names(release_season_colours), col = release_season_colours, pch = 20)

# Plot MCP for sex
plot(st_geometry(habitat_type_sf), col = habitat_type_sf$hab_color, border = "grey", axes = TRUE)
plot(mcpsex, add = TRUE, border = sex_colours, lwd = 2)
points(coords_utm, pch = 20, col = coords_utm@data$sexes, cex = 1.2)
title(main = "D)", adj = 0)
title(xlab = "Area (m)", ylab = "Area (m)")
legend("topright", legend = names(sex_colours), col = sex_colours, pch = 20)


#-----------------------------Survival Analyses---------------------------------

# using df_cleaned1

colnames(df_cleaned1)
table(df_cleaned1$status) # 23 deaths

# get a unique value of survival for each individual and day
surv_f <- df_cleaned1 |>
  group_by(pit_id, release_type, release_season, sex) |>
  summarise(dead = max(status, na.rm = FALSE), # get a unique value for each individual - maximum time days alive
            date_released =min(date_released),
            last.date=max(date)) |>
  mutate(days_alive = as.numeric(last.date-date_released))|>
  ungroup()

#days alive is day since door opened for soft and day since released in field for hard

hist(surv_f$days_alive, main = "Distribution of Days Alive", xlab = "Days Alive")
table(surv_f$dead) # 23 dead 32 alive
length(unique(surv_f$pit_id)) # 55


ggplot(surv_f, aes(x = days_alive, y = pit_id,color=release_season, group = release_season)) +
  geom_point()+ 
  theme_bw()

#barplot of individuals that died and are still alive - red = dead, grey = alive
days_alive <- surv_f$days_alive
cols <- ifelse(surv_f$dead == 0, "grey", "red")

barplot(days_alive, horiz=TRUE, col= cols, names.arg=surv_f$pit_id, las=1)

#---------------Kaplain Meier survival analyses---------------------------------

# create a survival object  
surv_object <- Surv(time=surv_f$days_alive, event=surv_f$dead)
summary(surv_object) # mean survival time 16.78 days, median = 20, min = 1, max = 35
survvv <- survfit(Surv(time=surv_f$days_alive, event=surv_f$dead)~1)
summary(survvv) # 56% survival overall
plot(survvv)

# survival for release type
survrt <- survfit(Surv(time=surv_f$days_alive, event=surv_f$dead)~surv_f$release_type)
summary(survrt)
log_rt <- survdiff(Surv(surv_f$days_alive, surv_f$dead) ~surv_f$release_type) # log-rank test to determine if sig diff between release type survival curves
print(log_rt) # 0.73 no significant difference in overall survival between soft and hard release
# soft 57% at end of study
# hard 55% at end of study
fit <- survfit(Surv(surv_f$days_alive, surv_f$dead)~release_type, data=surv_f)
ggsurvplot(fit, data=surv_f, pval=TRUE, xlim=c(0,42),
           break.time.by=10,
           ggthem=theme_classic(),
           xlab="Days Alive", ylab= "Survival Probability", legend.title = "Release strategy")


# survival for sex
survsex <- survfit(Surv(time=surv_f$days_alive, event=surv_f$dead)~surv_f$sex)
summary(survsex)
log_sex <- survdiff(Surv(surv_f$days_alive, surv_f$dead) ~surv_f$sex) # log-rank test to determine if sig diff between release type survival curves
print(log_sex) # 0.68
fitsex <- survfit(Surv(surv_f$days_alive, surv_f$dead)~sex, data=surv_f)
ggsurvplot(fitsex, data=surv_f,pval=TRUE, xlim=c(0,42),
           break.time.by=10,
           ggtheme =theme_classic(),
           xlab="Days Alive", ylab= "Survival Probability", legend.title = "Sex")


# survival for season
survrs <- survfit(Surv(time=surv_f$days_alive, event=surv_f$dead)~surv_f$release_season)
summary(survrs)
log_rs <- survdiff(Surv(surv_f$days_alive, surv_f$dead) ~surv_f$release_season) # log-rank test to determine if sig diff between release type survival curves
print(log_rs) # < 0.001 ***

surv_f$release_season <- factor(surv_f$release_season, levels = c("summer2023", "autumn2023", "spring2024"))

fitseason <- survfit(Surv(surv_f$days_alive, surv_f$dead)~release_season, data=surv_f)
ggsurvplot(fitseason, data=surv_f,pval=TRUE,  xlim=c(0,42),
           break.time.by=10,
           ggtheme =theme_classic(),
           xlab="Days Alive", ylab= "Survival Probability", legend.title = "Release Season")

table(surv_f$sex)
table(surv_f$release_type)
table(surv_f$release_season)

# survival for season + release type
survrst <- survfit(Surv(time=surv_f$days_alive, event=surv_f$dead)~surv_f$release_season+surv_f$release_type)
summary(survrst)
log_rst <- survdiff(Surv(surv_f$days_alive, surv_f$dead) ~surv_f$release_season+surv_f$release_type) # log-rank test to determine if sig diff between release type survival curves
print(log_rst) # <0.001***
fitseason_type <- survfit(Surv(surv_f$days_alive, surv_f$dead)~release_season+release_type, data=surv_f)
ggsurvplot(fitseason_type, data=surv_f,pval=TRUE, xlim=c(0,42),
           break.time.by=10,
           ggtheme =theme_classic(),
           xlab="Days Alive", ylab= "Survival Probability", legend.title = "Release Season + release strategy")


plot_sex <- ggsurvplot(fitsex, data=surv_f,pval=TRUE, xlim=c(0,42),
                       break.time.by=10,
                       ggtheme =theme_classic(base_size=12),censor.shape=124,
                       legend.title = "Sex",
                       legend.labs = c("Female [n = 21]", "Male [n = 34]"),
                       #legend.labs=c("Female ~ italic(n) == 21", "Male ~ italic(n) == 34"),
                       palette = c("#FF0000","#0000FF"),
                       xlab="Days", ylab= "Survival Probability")

plot_release_type <- ggsurvplot(fit, data=surv_f, pval=TRUE, xlim=c(0,42),
                                break.time.by=10,
                                ggtheme=theme_classic(base_size=12), censor.shape=124,
                                legend.title = "Release Strategy",
                                legend.labs=c("Hard [n = 28]", "Soft [n = 27]"),
                                palette = c("#FF0000","#0000FF"),
                                xlab="Days", ylab= "Survival Probability")



plot_release_season <- ggsurvplot(fitseason, data=surv_f,pval=TRUE,  xlim=c(0,42),
                                  break.time.by=10,
                                  ggtheme =theme_classic(base_size=12),censor.shape=124,
                                  legend.title = "Release Season",
                                  legend.labs=c("Summer 2023 [n = 19]", "Autumn 2023 [n = 19]", "Spring 2024 [n = 17]"),
                                  palette = c("#FF0000", "#999999", "#0000FF"),
                                  xlab="Days", ylab= "Survival Probability")



# Extract the plots
p3 <- plot_sex$plot + 
  labs(title = "A)") + 
  theme(legend.position = "right")

p1 <- plot_release_type$plot + 
  labs(title = "B)") + 
  theme(legend.position = "right")

p2 <- plot_release_season$plot + 
  labs(title = "C)") + 
  theme(legend.position = "right")

# Combine plots and risk tables
combined_plot <- grid.arrange(
  p3, p1, p2,
  ncol = 1, nrow = 3)



soft_release_survival <- surv_f |> filter(release_type=="SOFT")
hard_release_survival <- surv_f |> filter(release_type=="HARD")

soft_release_survival$release_season <- factor(soft_release_survival$release_season, 
                                               levels = c("summer2023", "autumn2023", "spring2024"))
custom_colors <- c("Summer 2023" = "#E69F00", "Autumn 2023" = "#56B4E9", "Spring 2024" = "#009E73")

hard_release_survival$release_season <- factor(hard_release_survival$release_season, 
                                               levels = c("summer2023", "autumn2023", "spring2024"))
custom_colors1 <- c("Summer 2023" = "#E69F00", "Autumn 2023" = "#56B4E9", "Spring 2024" = "#009E73")



km_fit_soft <- survfit(Surv(soft_release_survival$days_alive, soft_release_survival$dead) ~ release_season, data = soft_release_survival)
summary(km_fit_soft)
log_soft <- survdiff(Surv(soft_release_survival$days_alive, soft_release_survival$dead) ~soft_release_survival$release_season) # log-rank test to determine if sig diff between release type survival curves
print(log_soft) # 0.0011**

p1 <- ggsurvplot(km_fit_soft, data = soft_release_survival,
                 pval = TRUE,
                 xlim = c(0, 42),
                 break.time.by = 10,
                 title = "A)",
                 xlab = "Days",
                 ylab = "Survival Probability",
                 legend.title = "Release season",
                 legend.labs = c("Summer 2023", "Autumn 2023", "Spring 2024"),
                 palette = c("#E69F00", "#56B4E9", "#009E73"),
                 risk.table = TRUE,
                 ggtheme = theme_classic(base_size = 12),
                 censor.shape = 124)

# Modify the plot component
p1$plot <- p1$plot + theme(legend.position = "bottom")
print(p1$plot)


km_fit_hard <- survfit(Surv(hard_release_survival$days_alive, hard_release_survival$dead) ~ release_season, data = hard_release_survival)
log_hard <- survdiff(Surv(hard_release_survival$days_alive, hard_release_survival$dead) ~hard_release_survival$release_season) # log-rank test to determine if sig diff between release type survival curves
print(log_hard) # 0.0075**

p2 <- ggsurvplot(km_fit_hard, data = hard_release_survival,
                 pval=TRUE,
                 xlim=c(0,42),
                 break.time.by=10,
                 title="B)",
                 xlab = "Days",
                 ylab = "Survival Probability",
                 legend.title = "Release season",
                 legend.labs = c("Summer 2023", "Autumn 2023","Spring 2024"),
                 palette = c("#E69F00","#56B4E9","#009E73"),
                 risk.table = TRUE,
                 ggtheme = theme_classic(base_size = 12),
                 censor.shape=124)


# Extract plot and risk table components
p1_main <- p1$plot + theme(legend.position = "none")
p2_main <- p2$plot + theme(legend.position = "none")

p1_risk <- p1$table +
  theme(panel.border = element_blank(), 
                              panel.grid = element_blank(), 
                              axis.line.x = element_line(color="black"),
                              axis.line.y=element_line(color="black"),
                              axis.ticks = element_line(color="black"),
                              axis.title.x=element_text(color="black"),
                              axis.title.y=element_blank(),
                              axis.text = element_text())


p2_risk <- p2$table+
  theme(panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.line.x = element_line(color="black"),
        axis.line.y=element_line(color="black"),
        axis.ticks = element_line(color="black"),
        axis.title.x=element_text(color="black"),
        axis.title.y=element_blank(),
        axis.text = element_text())


combined_p1 <- arrangeGrob(p1_main, p1_risk, ncol = 1, heights = c(3, 1))  # Plot A and its risk table
combined_p2 <- arrangeGrob(p2_main, p2_risk, ncol = 1, heights = c(3, 1))  # Plot B and its risk table

combined_plots <- grid.arrange(
  arrangeGrob(combined_p1, combined_p2, ncol = 2))


#ggsave("F:/DAILY MOVEMENT AND SURVIVAL ANALYSIS/figures/figure_4.png", final_plot, 
 #      dpi = 301)



#-------------------- Cox Proportional Hazards models --------------------------

surv_f$release_type <- as.factor(surv_f$release_type)
surv_f$release_season <- as.factor(surv_f$release_season)
surv_f$sex <- as.factor(surv_f$sex)

surv_f$release_season <- factor(surv_f$release_season, levels=c("autumn2023", "summer2023", "spring2024"))

# Fit the Cox proportional hazards model (risk of death)
cox_model <- coxph(Surv(surv_f$days_alive, surv_f$dead) ~ release_type + release_season +sex, data = surv_f)
summary(cox_model)
# 55 individuals , 23 died
cox.zph(cox_model) # assumption met as all p values are non-significant

library(broom)
hr_table <- tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)

ggplot(hr_table, aes(x=term, y=estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 1, lty = "dashed", color = "red") +
  scale_y_log10()+
  labs(x="Covariate", y="Hazard Ratio (log scale)", title = "Forest Plot of Cox model") +
  theme_classic()


#----------------AVERAGE DAILY MOVEMENT AND WEIGHT ON SURVIVAL------------------
# combine average daily movement and percent weight change and survival
setwd("F:/DAILY MOVEMENT AND SURVIVAL ANALYSIS/data")

weights <- read.csv("WeightsAmtec.csv") |>
  mutate(date = dmy(date)) |>
  filter(year(date) != 2022)|>
  glimpse()

fixed <- read.csv("Fixed data.csv") |>
  filter(release_season != "spring2022",
         tracked != "n")|>
  glimpse()

average_weight <- weights |>
  group_by(pit_id) |>
  summarise(mean_weight = mean(weight, na.rm=TRUE)) |>
  glimpse()

fixed_weight <- fixed |>
  inner_join(average_weight, by="pit_id") |>
  glimpse()

weights_fixed <- fixed |>
  inner_join(weights, by="pit_id") |>
  glimpse()

weights_fixed <- weights_fixed |>
  inner_join(surv_f, by = "pit_id") |>
  glimpse()

#write.csv(weights_fixed, "weights_df2.csv")

# surv_f is survival df
# average_weight has average weight
# datf_clean has daily movement


datf_average <- datf_clean |>
  group_by(pit_id)|>
  summarise(average_movement = mean(daily_movement, na.rm=TRUE))|>
  glimpse()

surv_ff <- left_join(surv_f, average_weight, by="pit_id")|>
  glimpse()

surv_f1 <- left_join(surv_ff, datf_average, by="pit_id")|>
  glimpse()

#remove unecessary columns
surv_f1 <- surv_f1[-c(6:7)] |>
  glimpse()

length(unique(surv_f1$pit_id)) # 55


table(surv_f1$dead) # 32 alive 23 dead

surv_f1$average_movement <- ifelse(surv_f1$average_movement <= 0, NA, surv_f1$average_movement)

hist(log(surv_f1$average_movement)) # log makes for a normal distribution
hist(surv_f1$mean_weight, na.rm=TRUE) # not really that normal but is expected

# create survival object
surv_object1 <- Surv(surv_f1$days_alive, surv_f1$dead)

# cox proportional hazards model using average daily movement and average weight
cox_model1 <- coxph(surv_object1 ~ log(average_movement) + mean_weight, data = surv_f1)
summary(cox_model1)
cox.zph(cox_model1)

# add a time variant to average movement as it violates assumptions in previous model
# meaning that movement effect on death likely changes over time
cox_model_time_dep <- coxph(Surv(days_alive, dead) ~ log(average_movement) + 
                              tt(log(average_movement)) + mean_weight, 
                            data = surv_f1, tt = function(x, t, ...) x * t)
summary(cox_model_time_dep)

ggforest(cox_model_time_dep, data = surv_f1, main = "Hazard Ratios", cpositions = c(0.02, 0.22, 0.4),
         fontsize = 1, refLabel = "Reference", noDigits = 2)


ggcoxdiagnostics(cox_model_time_dep, type="schoenfeld")
summary(cox_model_time_dep)$coefficients

#extract model coefficients
coef_log_movement <- coef(cox_model_time_dep)["log(average_movement)"]
coef_tt_log_movement <- coef(cox_model_time_dep)["tt(log(average_movement))"]

# time points
time_points <- seq(1, 18, length.out = 100) 

#calc time varying effect
time_varying_effect <- coef_log_movement + coef_tt_log_movement * log(time_points + 1)

# put in df
df1 <- data.frame(time_points, time_varying_effect)

# plot average daily movement effect on survival over time
ggplot(df1, aes(x = time_points, y = time_varying_effect)) +
  geom_line(color = "blue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Time-varying effect of log(average_movement) on hazard",
       x = "Time", y = "Effect on Hazard (log scale)") +
  theme_classic()

