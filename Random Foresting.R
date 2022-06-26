install.packages("dplyr")
install.packages("RMySQL")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("grid")
install.packages("gridExtra")
install.packages("ROCR")
install.packages("randomForest")
install.packages("gam")
install.packages("scales")
install.packages("magrittr")
install.packages("plyr")
install.packages("gplots")
installed.packages("MASS")
library(plyr)
library(magrittr)
library(ggplot2)
library(grid)
library(gridExtra)
library(ROCR)
library(randomForest)
library(gam)
library(scales)
library(reshape2)
library(RMySQL)
library(dplyr)
library(gplots)
library(caret)
library(readr)
#library(MASS)
#require(MASS)
#px and py (pitch location), Tunnel Data
#exit speed, angle, distance, hc_x and hc_y (hit coordinates)

statcast.2016 <- read.csv("~/Desktop/pitcher_statcast.csv", stringsAsFactors = FALSE)
View(statcast.2016)#whiffs.data <- data.frame(statcast.2016)
playerid_list <- read_csv("~/Downloads/playerid_list (1).csv")
bpTunnelData <- read_csv("~/Downloads/bpstats_03-14-2017 (1).csv")
#View(bpTunnelData)

#str(bpTunnelData)
#chFA <- subset(bpTunnelData, first_pitch == "CH" & second_pitch == "FA")
#plot(chFA$`Release Diff`, chFA$`Tunnel Differential`, main = "CH-FA Release.to.Tunnel")
#abline(rlm(chFA$`Release Diff` ~ chFA$`Tunnel Differential`), col="red")
#lines(lowess(chFA$`Release Diff` ~ chFA$`Tunnel Differential`), col = "blue")

#chFA_rlm <- rlm(chFA$`Release Diff` ~ chFA$`Tunnel Differential`)
#summary(chFA_rlm)
#plot(chFA_rlm)
######
#	Renaming to match for merges
names(statcast.2016)
statcast.2016 <- plyr::rename(statcast.2016, c("pitcher" = "MLBCODE", "player_name" = "NAME"))
bpTunnelData <- subset(bpTunnelData, PITCHES > 0)
bpTunnelData <- plyr::rename(bpTunnelData, c("1st Pitch Type" = "first_pitch", "2nd Pitch Type" = "second_pitch"))


#	Merge statcast and player_id
statcast.2016_id_merge <- merge(statcast.2016, playerid_list, by = "MLBCODE")

#	Merge Tunnel and Statcast
totalMerge <- merge(statcast.2016_id_merge, bpTunnelData, by = c("first_pitch", "second_pitch", "NAME"), all.y = TRUE)
str(totalMerge)
totalMergeInPlay <- subset(totalMerge, totalMerge$description == c("In play, out(s)", "In play, run(s)", "In play, no out"))
totalMergeInPlay <- plyr::rename(totalMergeInPlay, c(totalMergeInPlay$`Release Diff` == "average.release.variation", totalMergeInPlay$`Flight Time Diff` == "flight.time.differential", totalMergeInPlay$`Post-tunnel Break` == "post.tunnel.break", totalMergeInPlay$`Tunnel Differential` == "pre.post.tunnel.break.ratio", totalMergeInPlay$`Plate Diff` == "plate.differntial"))


glm(totalMergeInPlay$hit ~ totalMergeInPlay$`Release Diff`, family = binomial, data = totalMergeInPlay)
glm(totalMergeInPlay$hit ~ totalMergeInPlay$`Release Diff` + totalMergeInPlay$`Flight Time Diff`, family = binomial, data = totalMergeInPlay)
glm(totalMergeInPlay$hit ~ totalMergeInPlay$`Release Diff` + totalMergeInPlay$`Flight Time Diff` + totalMergeInPlay$`Post-tunnel Break`, family = binomial, data = totalMergeInPlay)
glm(totalMergeInPlay$hit ~ totalMergeInPlay$`Release Diff` + totalMergeInPlay$`Flight Time Diff` + totalMergeInPlay$`Post-tunnel Break` + totalMergeInPlay$`Release:Tunnel` + totalMergeInPlay$`Plate Diff`, family = binomial, data = totalMergeInPlay)


#statcast_2016 <- subset(statcast_2016, select = c(3:61, 1:2))
# code if the batted ball resulted in a hit or an out

totalMergeInPlay$hit_distance_sc <- as.numeric(totalMergeInPlay$hit_distance_sc)
totalMergeInPlay$hit_angle <- as.numeric(totalMergeInPlay$hit_angle)
totalMergeInPlay$hit_speed <- as.numeric(totalMergeInPlay$hit_speed)

totalMergeInPlay$hit <- with(totalMergeInPlay, ifelse(grepl("Single", totalMergeInPlay$events), 1,
															ifelse(grepl("Double", totalMergeInPlay$events), 1,
																		 ifelse(grepl("Triple", totalMergeInPlay$events), 1, 
																		 			 ifelse(grepl("Home Run", totalMergeInPlay$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

totalMergeInPlay$fieldingTeam <- with(totalMergeInPlay, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[totalMergeInPlay$hit_distance_sc == "null"] = NA
#totalMergeInPlay$hit_speed[totalMergeInPlay$hit_speed == "null"] = NA
#totalMergeInPlay$hit_angle[totalMergeInPlay$hit_angle == "null"] = NA

# include row names for unique record identification

totalMergeInPlay$row <- row.names(totalMergeInPlay) %>% as.numeric()

# recode stand and home_team as factors

totalMergeInPlay$stand <- as.factor(totalMergeInPlay$stand)
totalMergeInPlay$home_team <- as.factor(totalMergeInPlay$home_team)


totalMergeInPlay$game_date <- as.Date(totalMergeInPlay$game_date)
str(totalMergeInPlay)

# subset 
totalMergeInPlay$release.diff <- totalMergeInPlay$`Release Diff`

totalMergeInPlay_working_data <- ungroup(totalMergeInPlay) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hit_speed, hc_x, hc_y, hit, stand, fieldingTeam, release.diff, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(release.diff)) %>%
	arrange(desc(hit))
head(totalMergeInPlay_working_data)
str(ungroup(totalMergeInPlay))
table(totalMergeInPlay_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

totalMergeInPlay_working_data <- filter(totalMergeInPlay_working_data, hc_x != 1, hc_y != 1)
head(totalMergeInPlay_working_data)

# create training and test sets
# scaled data
totalMergeInPlay_train <- subset(totalMergeInPlay_train, select = c(release.diff, hit_distance_sc:fieldingTeam, home_team:row))
set.seed(42)
totalMergeInPlay_train <- sample_frac(totalMergeInPlay_working_data, .15, replace = FALSE)
totalMergeInPlay_split <- setdiff(totalMergeInPlay_working_data, totalMergeInPlay_train)
totalMergeInPlay_test <- sample_frac(totalMergeInPlay_split, .50, replace = FALSE)
totalMergeInPlay_validate <- setdiff(totalMergeInPlay_split, totalMergeInPlay_test)

nrow(totalMergeInPlay_train) + nrow(totalMergeInPlay_test) + nrow(totalMergeInPlay_validate) == nrow(totalMergeInPlay_working_data)

with(totalMergeInPlay_train, table(hit)) %>% prop.table()
with(totalMergeInPlay_test, table(hit)) %>% prop.table()
with(totalMergeInPlay_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(totalMergeInPlay_train)

str(totalMergeInPlay)
##as.numeric(totalMergeInPlay$hit_distance_sc, totalMergeInPlay$hit_speed, totalMergeInPlay$hit_angle)

#totalMergeInPlay_train$hit_distance_sc <- as.numeric(totalMergeInPlay_train$hit_distance_sc)
#totalMergeInPlay_train$hit_speed <- as.numeric(totalMergeInPlay_train$hit_speed)
#totalMergeInPlay_train$hit_angle <- as.numeric(totalMergeInPlay_train$hit_angle)
#View(totalMergeInPlay_train)
totalMergeInPlay_scaled_data <- scale(totalMergeInPlay_train[,c(1:5, 9)])
totalMergeInPlay_scale_values <- attr(totalMergeInPlay_scaled_data, 'scaled:scale')
totalMergeInPlay_scale_values
totalMergeInPlay_center_values <- attr(totalMergeInPlay_scaled_data, 'scaled:center')
totalMergeInPlay_center_values
totalMergeInPlay_train <- cbind(totalMergeInPlay_scaled_data, select(totalMergeInPlay_train, hit:row))

# save levels for factor variables
totalMergeInPlay_levels_home_team <- levels(totalMergeInPlay_train$home_team)
totalMergeInPlay_levels_stand <- levels(totalMergeInPlay_train$stand)
totalMergeInPlay_levels_fieldingTeam <- levels(totalMergeInPlay_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(totalMergeInPlay_test)

totalMergeInPlay_test$hit_distance_sc <- (totalMergeInPlay_test$hit_distance_sc - totalMergeInPlay_center_values[1]) / totalMergeInPlay_scale_values[1]

totalMergeInPlay_test$hit_speed <- (totalMergeInPlay_test$hit_speed - totalMergeInPlay_center_values[2]) / totalMergeInPlay_scale_values[2]

totalMergeInPlay_test$hit_angle <- (totalMergeInPlay_test$hit_angle - totalMergeInPlay_center_values[3]) / totalMergeInPlay_scale_values[3]

totalMergeInPlay_test$hc_x <- (totalMergeInPlay_test$hc_x - totalMergeInPlay_center_values[4]) / totalMergeInPlay_scale_values[4]

totalMergeInPlay_test$hc_y <- (totalMergeInPlay_test$hc_y - totalMergeInPlay_center_values[5]) / totalMergeInPlay_scale_values[5]

totalMergeInPlay_test$release.diff <- (totalMergeInPlay_test$release.diff - totalMergeInPlay_center_values[6]) / totalMergeInPlay_scale_values[6]


#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

totalMergeInPlay_validate$hit_distance_sc <- (totalMergeInPlay_validate$hit_distance_sc - totalMergeInPlay_center_values[1]) / totalMergeInPlay_scale_values[1]

totalMergeInPlay_validate$hit_speed <- (totalMergeInPlay_validate$hit_speed - totalMergeInPlay_center_values[2]) / totalMergeInPlay_scale_values[2]

totalMergeInPlay_validate$hit_angle <- (totalMergeInPlay_validate$hit_angle - totalMergeInPlay_center_values[3]) / totalMergeInPlay_scale_values[3]

totalMergeInPlay_validate$hc_x <- (totalMergeInPlay_validate$hc_x - totalMergeInPlay_center_values[4]) / totalMergeInPlay_scale_values[4]

totalMergeInPlay_validate$hc_y <- (totalMergeInPlay_validate$hc_y - totalMergeInPlay_center_values[5]) / totalMergeInPlay_scale_values[5]

totalMergeInPlay_validate$release.diff <- (totalMergeInPlay_validate$release.diff - totalMergeInPlay_center_values[6]) / totalMergeInPlay_scale_values[6]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(totalMergeInPlay_train)
totalMergeInPlay_train <- subset(totalMergeInPlay_train, select = c(1:9, 11:12))
set.seed(42)
totalMergeInPlay_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(totalMergeInPlay_train, -row), ntree = 501, importance = TRUE)

print(totalMergeInPlay_rf.1)

plot(totalMergeInPlay_rf.1)

varImpPlot(totalMergeInPlay_rf.1)

# predict values and tune for optimal out of sample accuracy

opt.cut = function(perf, pred){
	cut.ind = mapply(FUN=function(x, y, p){
		d = (x - 0)^2 + (y-1)^2
		ind = which(d == min(d))
		c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
			cutoff = p[[ind]])
	}, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(roc.perf, pred))

# only speed, angle, release diff, and hc_x stand, fielding team and park

set.seed(42)
totalMergeInPlay_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(totalMergeInPlay_train, -row, -hit_distance_sc, -hc_y), importance = TRUE)

print(totalMergeInPlay_rf.5)

plot(totalMergeInPlay_rf.5)

varImpPlot(totalMergeInPlay_rf.5)

totalMergeInPlay_predict_fit.rf.5 <- data.frame(fits = predict(totalMergeInPlay_rf.5, totalMergeInPlay_test, type = "prob")[,2], actuals = totalMergeInPlay_test$hit)

totalMergeInPlay_pred.rf.5 <- prediction(totalMergeInPlay_predict_fit.rf.5$fits, totalMergeInPlay_predict_fit.rf.5$actuals)

totalMergeInPlay_roc.pred.rf.5 <- performance(totalMergeInPlay_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(totalMergeInPlay_roc.pred.rf.5)
abline(a = 0, b = 1)
totalMergeInPlay_opt <- opt.cut(totalMergeInPlay_roc.pred.rf.5, totalMergeInPlay_pred.rf.5)
totalMergeInPlay_opt
totalMergeInPlay_opt <- totalMergeInPlay_opt[3]
totalMergeInPlay_predict_fit.rf.5$fits <- with(totalMergeInPlay_predict_fit.rf.5, ifelse(fits > totalMergeInPlay_opt, 1, 0)) 

str(totalMergeInPlay_test)

#totalMergeInPlay_test$fieldingTeam <- as.character(totalMergeInPlay_test$fieldingTeam)
#totalMergeInPlay_test$home_team <- as.character(totalMergeInPlay_test$home_team)
#totalMergeInPlay_test$hit <- as.logical(totalMergeInPlay_test$hit)
#totalMergeInPlay_test$stand <- as.logical(totalMergeInPlay_test$stand)
str(totalMergeInPlay_test)
totalMergeInPlay_rf.5_confusion_test <- confusionMatrix(model = totalMergeInPlay_rf.5, x = totalMergeInPlay_test, y = totalMergeInPlay_test$hit)
totalMergeInPlay_rf.5_confusion_validate <- confusionMatrix(model = totalMergeInPlay_rf.5, x = totalMergeInPlay_validate, y = totalMergeInPlay_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


totalMergeInPlay_test_rows <- totalMergeInPlay_test$row
totalMergeInPlay_validate_rows <- totalMergeInPlay_validate$row
totalMergeInPlay_pred_data_emp_1 <- ungroup(totalMergeInPlay) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% totalMergeInPlay_test_rows)

totalMergeInPlay_pred_data_emp <- ungroup(totalMergeInPlay) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% totalMergeInPlay_validate_rows) %>%
	rbind(totalMergeInPlay_pred_data_emp_1)

totalMergeInPlay_out_of_training_rows <- totalMergeInPlay_pred_data_emp$row

totalMergeInPlay_rf.5_full_out_of_sample_data <- ungroup(totalMergeInPlay) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% totalMergeInPlay_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



totalMergeInPlay_rf.5_full_out_of_sample_data_scaled <- totalMergeInPlay_rf.5_full_out_of_sample_data

totalMergeInPlay_rf.5_full_out_of_sample_data_scaled$hit_speed <- (totalMergeInPlay_rf.5_full_out_of_sample_data_scaled$hit_speed - totalMergeInPlay_center_values[2]) / totalMergeInPlay_scale_values[2]

totalMergeInPlay_rf.5_full_out_of_sample_data_scaled$hit_angle <- (totalMergeInPlay_rf.5_full_out_of_sample_data_scaled$hit_angle - totalMergeInPlay_center_values[3]) / totalMergeInPlay_scale_values[3]

totalMergeInPlay_rf.5.prob <- predict(totalMergeInPlay_rf.5, totalMergeInPlay_rf.5_full_out_of_sample_data_scaled, type = "response")

totalMergeInPlay_rf.5_full_out_of_sample_data <- cbind(filter(totalMergeInPlay_rf.5_full_out_of_sample_data, row %in% totalMergeInPlay_out_of_training_rows), totalMergeInPlay_rf.5.prob)

names(totalMergeInPlay_rf.5_full_out_of_sample_data)[65] <- "fits"
names(totalMergeInPlay_rf.5_full_out_of_sample_data)

totalMergeInPlay_rf.5_full_out_of_sample_data_reduced <- totalMergeInPlay_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

totalMergeInPlay_rf.5_mean_table <- totalMergeInPlay_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

totalMergeInPlay_rf.5_full_out_of_sample_data$type <- with(totalMergeInPlay_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

totalMergeInPlay_rf.5_full_out_of_sample_data$hit_label <- with(totalMergeInPlay_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

totalMergeInPlay_rf.5_plot1 <- ggplot(totalMergeInPlay_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

totalMergeInPlay_rf.5_plot1

totalMergeInPlay_rf.5_plot2 <- ggplot(totalMergeInPlay_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

totalMergeInPlay_rf.5_plot2

totalMergeInPlay_rf.5_plot3 <- ggplot(totalMergeInPlay_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

totalMergeInPlay_rf.5_plot3

totalMergeInPlay_grid_plot_rf.5 <- grid.arrange(totalMergeInPlay_rf.5_plot1, totalMergeInPlay_rf.5_plot2, totalMergeInPlay_rf.5_plot3, ncol = 3)
totalMergeInPlay_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

totalMergeInPlay_test_rows <- totalMergeInPlay_test$row
totalMergeInPlay_validate_rows <- totalMergeInPlay_validate$row
totalMergeInPlay_pred_data_emp_1 <- ungroup(totalMergeInPlay) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% totalMergeInPlay_test_rows)

totalMergeInPlay_pred_data_emp <- ungroup(totalMergeInPlay) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% totalMergeInPlay_validate_rows) %>%
	rbind(totalMergeInPlay_pred_data_emp_1)

totalMergeInPlay_out_of_training_rows <- totalMergeInPlay_pred_data_emp$row

totalMergeInPlay_pred_data_emp <- ungroup(totalMergeInPlay) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% totalMergeInPlay_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

totalMergeInPlay_pred_data_emp_scaled <- totalMergeInPlay_pred_data_emp

totalMergeInPlay_pred_data_emp_scaled$hit_speed <- (totalMergeInPlay_pred_data_emp_scaled$hit_speed - totalMergeInPlay_center_values[2]) / totalMergeInPlay_scale_values[2]

totalMergeInPlay_pred_data_emp_scaled$hit_angle <- (totalMergeInPlay_pred_data_emp_scaled$hit_angle - totalMergeInPlay_center_values[3]) / totalMergeInPlay_scale_values[3]

totalMergeInPlay_pred_prob_hit <- predict(totalMergeInPlay_rf.5, totalMergeInPlay_pred_data_emp_scaled, type = "prob")[,2]
totalMergeInPlay_pred_prob_hit_fits <- cbind(totalMergeInPlay_pred_data_emp, totalMergeInPlay_pred_prob_hit)

totalMergeInPlay_pred_prob_hit_fits[,c(1:2)] <- totalMergeInPlay_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

totalMergeInPlay_angle_speed_pred <- totalMergeInPlay_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = totalMergeInPlay_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nSI-FF Hit Probability\n") + theme_bp_grey()

totalMergeInPlay_angle_speed_pred


##########################
# CU

CU$hit_distance_sc <- as.numeric(CU$hit_distance_sc)
CU$hit_angle <- as.numeric(CU$hit_angle)
CU$hit_speed <- as.numeric(CU$hit_speed)

CU$hit <- with(CU, ifelse(grepl("Single", CU$events), 1,
													ifelse(grepl("Double", CU$events), 1,
																 ifelse(grepl("Triple", CU$events), 1, 
																 			 ifelse(grepl("Home Run", CU$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

CU$fieldingTeam <- with(CU, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[CU$hit_distance_sc == "null"] = NA
#CU$hit_speed[CU$hit_speed == "null"] = NA
#CU$hit_angle[CU$hit_angle == "null"] = NA

# include row names for unique record identification

CU$row <- row.names(CU) %>% as.numeric()

# recode stand and home_team as factors

CU$stand <- as.factor(CU$stand)
CU$home_team <- as.factor(CU$home_team)


CU$game_date <- as.Date(CU$game_date)


# subset 

CU_working_data <- ungroup(CU) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(CU_working_data)
str(ungroup(CU))
table(CU_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

CU_working_data <- filter(CU_working_data, hc_x != 1, hc_y != 1)
head(CU_working_data)

# create training and test sets
# scaled data

set.seed(42)
CU_train <- sample_frac(CU_working_data, .15, replace = FALSE)
CU_split <- setdiff(CU_working_data, CU_train)
CU_test <- sample_frac(CU_split, .50, replace = FALSE)
CU_validate <- setdiff(CU_split, CU_test)

nrow(CU_train) + nrow(CU_test) + nrow(CU_validate) == nrow(CU_working_data)

with(CU_train, table(hit)) %>% prop.table()
with(CU_test, table(hit)) %>% prop.table()
with(CU_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(CU_train)

CU_scaled_data <- scale(CU_train[,c(1:5)])
CU_scale_values <- attr(CU_scaled_data, 'scaled:scale')
CU_scale_values

CU_center_values <- attr(CU_scaled_data, 'scaled:center')
CU_center_values

CU_train <- cbind(CU_scaled_data, select(CU_train, hit:row))

# save levels for factor variables
CU_levels_home_team <- levels(CU_train$home_team)
CU_levels_stand <- levels(CU_train$stand)
CU_levels_fieldingTeam <- levels(CU_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data

CU_test$hit_distance_sc <- (CU_test$hit_distance_sc - CU_center_values[1]) / CU_scale_values[1]

CU_test$hit_speed <- (CU_test$hit_speed - CU_center_values[2]) / CU_scale_values[2]

CU_test$hit_angle <- (CU_test$hit_angle - CU_center_values[3]) / CU_scale_values[3]

CU_test$hc_x <- (CU_test$hc_x - CU_center_values[4]) / CU_scale_values[4]

CU_test$hc_y <- (CU_test$hc_y - CU_center_values[5]) / CU_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

CU_validate$hit_distance_sc <- (CU_validate$hit_distance_sc - CU_center_values[1]) / CU_scale_values[1]

CU_validate$hit_speed <- (CU_validate$hit_speed - CU_center_values[2]) / CU_scale_values[2]

CU_validate$hit_angle <- (CU_validate$hit_angle - CU_center_values[3]) / CU_scale_values[3]

CU_validate$hc_x <- (CU_validate$hc_x - CU_center_values[4]) / CU_scale_values[4]

CU_validate$hc_y <- (CU_validate$hc_y - CU_center_values[5]) / CU_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(CU_train)

set.seed(42)
CU_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(CU_train, -row), ntree = 501, importance = TRUE)

print(CU_rf.1)

plot(CU_rf.1)

varImpPlot(CU_rf.1)

# predict values and tune for optimal out of sample accuracy

opt.cut = function(perf, pred){
	cut.ind = mapply(FUN=function(x, y, p){
		d = (x - 0)^2 + (y-1)^2
		ind = which(d == min(d))
		c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
			cutoff = p[[ind]])
	}, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(roc.perf, pred))

# only speed, angle, stand, fielding team and park

set.seed(42)
CU_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(CU_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(CU_rf.5)

plot(CU_rf.5)

varImpPlot(CU_rf.5)

CU_predict_fit.rf.5 <- data.frame(fits = predict(CU_rf.5, CU_test, type = "prob")[,2], actuals = CU_test$hit)

CU_pred.rf.5 <- prediction(CU_predict_fit.rf.5$fits, CU_predict_fit.rf.5$actuals)

CU_roc.pred.rf.5 <- performance(CU_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(CU_roc.pred.rf.5)
abline(a = 0, b = 1)
CU_opt <- opt.cut(CU_roc.pred.rf.5, CU_pred.rf.5)
CU_opt
CU_opt <- CU_opt[3]
CU_predict_fit.rf.5$fits <- with(CU_predict_fit.rf.5, ifelse(fits > CU_opt, 1, 0)) 

CU_rf.5_confusion_test <- confusionMatrix(model = rf.5, x = CU_test, y = CU_test$hit)
CU_rf.5_confusion_validate <- confusionMatrix(model = rf.5, x = CU_validate, y = CU_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


CU_test_rows <- test$row
CU_validate_rows <- validate$row
CU_pred_data_emp_1 <- ungroup(CU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% CU_test_rows)

CU_pred_data_emp <- ungroup(CU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% CU_validate_rows) %>%
	rbind(CU_pred_data_emp_1)

CU_out_of_training_rows <- CU_pred_data_emp$row

CU_rf.5_full_out_of_sample_data <- ungroup(CU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% CU_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



CU_rf.5_full_out_of_sample_data_scaled <- CU_rf.5_full_out_of_sample_data

CU_rf.5_full_out_of_sample_data_scaled$hit_speed <- (CU_rf.5_full_out_of_sample_data_scaled$hit_speed - CU_center_values[2]) / CU_scale_values[2]

CU_rf.5_full_out_of_sample_data_scaled$hit_angle <- (CU_rf.5_full_out_of_sample_data_scaled$hit_angle - CU_center_values[3]) / CU_scale_values[3]

CU_rf.5.prob <- predict(CU_rf.5, CU_rf.5_full_out_of_sample_data_scaled, type = "response")

CU_rf.5_full_out_of_sample_data <- cbind(filter(CU_rf.5_full_out_of_sample_data, row %in% CU_out_of_training_rows), CU_rf.5.prob)

names(CU_rf.5_full_out_of_sample_data)[65] <- "fits"
names(CU_rf.5_full_out_of_sample_data)

CU_rf.5_full_out_of_sample_data_reduced <- CU_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

CU_rf.5_mean_table <- CU_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

CU_rf.5_full_out_of_sample_data$type <- with(CU_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

CU_rf.5_full_out_of_sample_data$hit_label <- with(CU_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

CU_rf.5_plot1 <- ggplot(CU_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

CU_rf.5_plot1

CU_rf.5_plot2 <- ggplot(CU_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

CU_rf.5_plot2

CU_rf.5_plot3 <- ggplot(CU_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

CU_rf.5_plot3

CU_grid_plot_rf.5 <- grid.arrange(CU_rf.5_plot1, CU_rf.5_plot2, CU_rf.5_plot3, ncol = 3)
CU_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

CU_test_rows <- CU_test$row
CU_validate_rows <- CU_validate$row
CU_pred_data_emp_1 <- ungroup(CU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% test_rows)

CU_pred_data_emp <- ungroup(CU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% validate_rows) %>%
	rbind(CU_pred_data_emp_1)

CU_out_of_training_rows <- CU_pred_data_emp$row

CU_pred_data_emp <- ungroup(CU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% CU_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

CU_pred_data_emp_scaled <- CU_pred_data_emp

CU_pred_data_emp_scaled$hit_speed <- (CU_pred_data_emp_scaled$hit_speed - CU_center_values[2]) / CU_scale_values[2]

CU_pred_data_emp_scaled$hit_angle <- (CU_pred_data_emp_scaled$hit_angle - CU_center_values[3]) / CU_scale_values[3]

CU_pred_prob_hit <- predict(CU_rf.5, CU_pred_data_emp_scaled, type = "prob")[,2]
CU_pred_prob_hit_fits <- cbind(CU_pred_data_emp, CU_pred_prob_hit)

CU_pred_prob_hit_fits[,c(1:2)] <- CU_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

CU_angle_speed_pred <- CU_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = CU_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(low = "#006BA4", high = "#C85200", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nPredicted Probability of a Hit Using Statcast Data (2016)\n") + theme_bp_grey()

CU_angle_speed_pred


####
######################
# Slider
SL$hit <- with(SL, ifelse(grepl("Single", SL$events), 1,
													ifelse(grepl("Double", SL$events), 1,
																 ifelse(grepl("Triple", SL$events), 1, 
																 			 ifelse(grepl("Home Run", SL$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

SL$fieldingTeam <- with(SL, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[SL$hit_distance_sc == "null"] = NA
#SL$hit_speed[SL$hit_speed == "null"] = NA
#SL$hit_angle[SL$hit_angle == "null"] = NA

# include row names for unique record identification

SL$row <- row.names(SL) %>% as.numeric()

# recode stand and home_team as factors

SL$stand <- as.factor(SL$stand)
SL$home_team <- as.factor(SL$home_team)


SL$game_date <- as.Date(SL$game_date)


# subset 

SL_working_data <- ungroup(SL) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(SL_working_data)
str(ungroup(SL))
table(SL_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

SL_working_data <- filter(SL_working_data, hc_x != 1, hc_y != 1)
head(SL_working_data)

# create training and test sets
# scaled data

set.seed(42)
SL_train <- sample_frac(SL_working_data, .15, replace = FALSE)
SL_split <- setdiff(SL_working_data, SL_train)
SL_test <- sample_frac(SL_split, .50, replace = FALSE)
SL_validate <- setdiff(SL_split, SL_test)

nrow(SL_train) + nrow(SL_test) + nrow(SL_validate) == nrow(SL_working_data)

with(SL_train, table(hit)) %>% prop.table()
with(SL_test, table(hit)) %>% prop.table()
with(SL_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(SL_train)

SL_scaled_data <- scale(SL_train[,c(1:5)])
SL_scale_values <- attr(SL_scaled_data, 'scaled:scale')
SL_scale_values

SL_center_values <- attr(SL_scaled_data, 'scaled:center')
SL_center_values

SL_train <- cbind(SL_scaled_data, select(SL_train, hit:row))

# save levels for factor variables
SL_levels_home_team <- levels(SL_train$home_team)
SL_levels_stand <- levels(SL_train$stand)
SL_levels_fieldingTeam <- levels(SL_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data

SL_test$hit_distance_sc <- (SL_test$hit_distance_sc - SL_center_values[1]) / SL_scale_values[1]

SL_test$hit_speed <- (SL_test$hit_speed - SL_center_values[2]) / SL_scale_values[2]

SL_test$hit_angle <- (SL_test$hit_angle - SL_center_values[3]) / SL_scale_values[3]

SL_test$hc_x <- (SL_test$hc_x - SL_center_values[4]) / SL_scale_values[4]

SL_test$hc_y <- (SL_test$hc_y - SL_center_values[5]) / SL_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

SL_validate$hit_distance_sc <- (SL_validate$hit_distance_sc - SL_center_values[1]) / SL_scale_values[1]

SL_validate$hit_speed <- (SL_validate$hit_speed - SL_center_values[2]) / SL_scale_values[2]

SL_validate$hit_angle <- (SL_validate$hit_angle - SL_center_values[3]) / SL_scale_values[3]

SL_validate$hc_x <- (SL_validate$hc_x - SL_center_values[4]) / SL_scale_values[4]

SL_validate$hc_y <- (SL_validate$hc_y - SL_center_values[5]) / SL_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(SL_train)

set.seed(42)
SL_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(SL_train, -row), ntree = 501, importance = TRUE)

print(SL_rf.1)

plot(SL_rf.1)

varImpPlot(SL_rf.1)

# predict values and tune for optimal out of sample accuracy

opt.cut = function(perf, pred){
	cut.ind = mapply(FUN=function(x, y, p){
		d = (x - 0)^2 + (y-1)^2
		ind = which(d == min(d))
		c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
			cutoff = p[[ind]])
	}, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(roc.perf, pred))

# only speed, angle, stand, fielding team and park

set.seed(42)
SL_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(SL_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(SL_rf.5)

plot(SL_rf.5)

varImpPlot(SL_rf.5)

SL_predict_fit.rf.5 <- data.frame(fits = predict(SL_rf.5, SL_test, type = "prob")[,2], actuals = SL_test$hit)

SL_pred.rf.5 <- prediction(SL_predict_fit.rf.5$fits, SL_predict_fit.rf.5$actuals)

SL_roc.pred.rf.5 <- performance(SL_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(SL_roc.pred.rf.5)
abline(a = 0, b = 1)
SL_opt <- opt.cut(SL_roc.pred.rf.5, SL_pred.rf.5)
SL_opt
SL_opt <- SL_opt[3]
SL_predict_fit.rf.5$fits <- with(SL_predict_fit.rf.5, ifelse(fits > SL_opt, 1, 0)) 

SL_rf.5_confusion_test <- confusionMatrix(model = rf.5, x = SL_test, y = SL_test$hit)
SL_rf.5_confusion_validate <- confusionMatrix(model = rf.5, x = SL_validate, y = SL_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


SL_test_rows <- SL_test$row
SL_validate_rows <- SL_validate$row
SL_pred_data_emp_1 <- ungroup(SL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% SL_test_rows)

SL_pred_data_emp <- ungroup(SL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% SL_validate_rows) %>%
	rbind(SL_pred_data_emp_1)

SL_out_of_training_rows <- SL_pred_data_emp$row

SL_rf.5_full_out_of_sample_data <- ungroup(SL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% SL_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



SL_rf.5_full_out_of_sample_data_scaled <- SL_rf.5_full_out_of_sample_data

SL_rf.5_full_out_of_sample_data_scaled$hit_speed <- (SL_rf.5_full_out_of_sample_data_scaled$hit_speed - SL_center_values[2]) / SL_scale_values[2]

SL_rf.5_full_out_of_sample_data_scaled$hit_angle <- (SL_rf.5_full_out_of_sample_data_scaled$hit_angle - SL_center_values[3]) / SL_scale_values[3]

SL_rf.5.prob <- predict(SL_rf.5, SL_rf.5_full_out_of_sample_data_scaled, type = "response")

SL_rf.5_full_out_of_sample_data <- cbind(filter(SL_rf.5_full_out_of_sample_data, row %in% SL_out_of_training_rows), SL_rf.5.prob)

names(SL_rf.5_full_out_of_sample_data)[65] <- "fits"
names(SL_rf.5_full_out_of_sample_data)

SL_rf.5_full_out_of_sample_data_reduced <- SL_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

SL_rf.5_mean_table <- SL_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

SL_rf.5_full_out_of_sample_data$type <- with(SL_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

SL_rf.5_full_out_of_sample_data$hit_label <- with(SL_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

SL_rf.5_plot1 <- ggplot(SL_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

SL_rf.5_plot1

SL_rf.5_plot2 <- ggplot(SL_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

SL_rf.5_plot2

SL_rf.5_plot3 <- ggplot(SL_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

SL_rf.5_plot3

SL_grid_plot_rf.5 <- grid.arrange(SL_rf.5_plot1, SL_rf.5_plot2, SL_rf.5_plot3, ncol = 3)
SL_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

SL_test_rows <- SL_test$row
SL_validate_rows <- SL_validate$row
SL_pred_data_emp_1 <- ungroup(SL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% test_rows)

SL_pred_data_emp <- ungroup(SL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% validate_rows) %>%
	rbind(SL_pred_data_emp_1)

SL_out_of_training_rows <- SL_pred_data_emp$row

SL_pred_data_emp <- ungroup(SL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% SL_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

SL_pred_data_emp_scaled <- SL_pred_data_emp

SL_pred_data_emp_scaled$hit_speed <- (SL_pred_data_emp_scaled$hit_speed - SL_center_values[2]) / SL_scale_values[2]

SL_pred_data_emp_scaled$hit_angle <- (SL_pred_data_emp_scaled$hit_angle - SL_center_values[3]) / SL_scale_values[3]

SL_pred_prob_hit <- predict(SL_rf.5, SL_pred_data_emp_scaled, type = "prob")[,2]
SL_pred_prob_hit_fits <- cbind(SL_pred_data_emp, SL_pred_prob_hit)

SL_pred_prob_hit_fits[,c(1:2)] <- SL_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

SL_angle_speed_pred <- SL_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = SL_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(low = "#006BA4", high = "#C85200", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nPredicted Probability of a Hit Using Statcast Data (2016)\n") + theme_bp_grey()

SL_angle_speed_pred


####
##################
# FT

FT$hit <- with(FT, ifelse(grepl("Single", FT$events), 1,
													ifelse(grepl("Double", FT$events), 1,
																 ifelse(grepl("Triple", FT$events), 1, 
																 			 ifelse(grepl("Home Run", FT$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

FT$fieldingTeam <- with(FT, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[FT$hit_distance_sc == "null"] = NA
#FT$hit_speed[FT$hit_speed == "null"] = NA
#FT$hit_angle[FT$hit_angle == "null"] = NA

# include row names for unique record identification

FT$row <- row.names(FT) %>% as.numeric()

# recode stand and home_team as factors

FT$stand <- as.factor(FT$stand)
FT$home_team <- as.factor(FT$home_team)


FT$game_date <- as.Date(FT$game_date)


# subset 

FT_working_data <- ungroup(FT) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(FT_working_data)
str(ungroup(FT))
table(FT_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

FT_working_data <- filter(FT_working_data, hc_x != 1, hc_y != 1)
head(FT_working_data)

# create training and test sets
# scaled data

set.seed(42)
FT_train <- sample_frac(FT_working_data, .15, replace = FALSE)
FT_split <- setdiff(FT_working_data, FT_train)
FT_test <- sample_frac(FT_split, .50, replace = FALSE)
FT_validate <- setdiff(FT_split, FT_test)

nrow(FT_train) + nrow(FT_test) + nrow(FT_validate) == nrow(FT_working_data)

with(FT_train, table(hit)) %>% prop.table()
with(FT_test, table(hit)) %>% prop.table()
with(FT_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(FT_train)

FT_scaled_data <- scale(FT_train[,c(1:5)])
FT_scale_values <- attr(FT_scaled_data, 'scaled:scale')
FT_scale_values

FT_center_values <- attr(FT_scaled_data, 'scaled:center')
FT_center_values

FT_train <- cbind(FT_scaled_data, select(FT_train, hit:row))

# save levels for factor variables
FT_levels_home_team <- levels(FT_train$home_team)
FT_levels_stand <- levels(FT_train$stand)
FT_levels_fieldingTeam <- levels(FT_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data

FT_test$hit_distance_sc <- (FT_test$hit_distance_sc - FT_center_values[1]) / FT_scale_values[1]

FT_test$hit_speed <- (FT_test$hit_speed - FT_center_values[2]) / FT_scale_values[2]

FT_test$hit_angle <- (FT_test$hit_angle - FT_center_values[3]) / FT_scale_values[3]

FT_test$hc_x <- (FT_test$hc_x - FT_center_values[4]) / FT_scale_values[4]

FT_test$hc_y <- (FT_test$hc_y - FT_center_values[5]) / FT_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

FT_validate$hit_distance_sc <- (FT_validate$hit_distance_sc - FT_center_values[1]) / FT_scale_values[1]

FT_validate$hit_speed <- (FT_validate$hit_speed - FT_center_values[2]) / FT_scale_values[2]

FT_validate$hit_angle <- (FT_validate$hit_angle - FT_center_values[3]) / FT_scale_values[3]

FT_validate$hc_x <- (FT_validate$hc_x - FT_center_values[4]) / FT_scale_values[4]

FT_validate$hc_y <- (FT_validate$hc_y - FT_center_values[5]) / FT_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(FT_train)

set.seed(42)
FT_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(FT_train, -row), ntree = 501, importance = TRUE)

print(FT_rf.1)

plot(FT_rf.1)

varImpPlot(FT_rf.1)

# predict values and tune for optimal out of sample accuracy

opt.cut = function(perf, pred){
	cut.ind = mapply(FUN=function(x, y, p){
		d = (x - 0)^2 + (y-1)^2
		ind = which(d == min(d))
		c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
			cutoff = p[[ind]])
	}, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(roc.perf, pred))

# only speed, angle, stand, fielding team and park

set.seed(42)
FT_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(FT_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(FT_rf.5)

plot(FT_rf.5)

varImpPlot(FT_rf.5)

FT_predict_fit.rf.5 <- data.frame(fits = predict(FT_rf.5, FT_test, type = "prob")[,2], actuals = FT_test$hit)

FT_pred.rf.5 <- prediction(FT_predict_fit.rf.5$fits, FT_predict_fit.rf.5$actuals)

FT_roc.pred.rf.5 <- performance(FT_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(FT_roc.pred.rf.5)
abline(a = 0, b = 1)
FT_opt <- opt.cut(FT_roc.pred.rf.5, FT_pred.rf.5)
FT_opt
FT_opt <- FT_opt[3]
FT_predict_fit.rf.5$fits <- with(FT_predict_fit.rf.5, ifelse(fits > FT_opt, 1, 0)) 

FT_rf.5_confusion_test <- confusionMatrix(model = rf.5, x = FT_test, y = FT_test$hit)
FT_rf.5_confusion_validate <- confusionMatrix(model = rf.5, x = FT_validate, y = FT_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


FT_test_rows <- FT_test$row
FT_validate_rows <- FT_validate$row
FT_pred_data_emp_1 <- ungroup(FT) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% FT_test_rows)

FT_pred_data_emp <- ungroup(FT) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% FT_validate_rows) %>%
	rbind(FT_pred_data_emp_1)

FT_out_of_training_rows <- FT_pred_data_emp$row

FT_rf.5_full_out_of_sample_data <- ungroup(FT) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% FT_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



FT_rf.5_full_out_of_sample_data_scaled <- FT_rf.5_full_out_of_sample_data

FT_rf.5_full_out_of_sample_data_scaled$hit_speed <- (FT_rf.5_full_out_of_sample_data_scaled$hit_speed - FT_center_values[2]) / FT_scale_values[2]

FT_rf.5_full_out_of_sample_data_scaled$hit_angle <- (FT_rf.5_full_out_of_sample_data_scaled$hit_angle - FT_center_values[3]) / FT_scale_values[3]

FT_rf.5.prob <- predict(FT_rf.5, FT_rf.5_full_out_of_sample_data_scaled, type = "response")

FT_rf.5_full_out_of_sample_data <- cbind(filter(FT_rf.5_full_out_of_sample_data, row %in% FT_out_of_training_rows), FT_rf.5.prob)

names(FT_rf.5_full_out_of_sample_data)[65] <- "fits"
names(FT_rf.5_full_out_of_sample_data)

FT_rf.5_full_out_of_sample_data_reduced <- FT_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

FT_rf.5_mean_table <- FT_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

FT_rf.5_full_out_of_sample_data$type <- with(FT_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

FT_rf.5_full_out_of_sample_data$hit_label <- with(FT_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

FT_rf.5_plot1 <- ggplot(FT_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

FT_rf.5_plot1

FT_rf.5_plot2 <- ggplot(FT_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

FT_rf.5_plot2

FT_rf.5_plot3 <- ggplot(FT_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

FT_rf.5_plot3

FT_grid_plot_rf.5 <- grid.arrange(FT_rf.5_plot1, FT_rf.5_plot2, FT_rf.5_plot3, ncol = 3)
FT_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

FT_test_rows <- FT_test$row
FT_validate_rows <- FT_validate$row
FT_pred_data_emp_1 <- ungroup(FT) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% test_rows)

FT_pred_data_emp <- ungroup(FT) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% validate_rows) %>%
	rbind(FT_pred_data_emp_1)

FT_out_of_training_rows <- FT_pred_data_emp$row

FT_pred_data_emp <- ungroup(FT) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% FT_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

FT_pred_data_emp_scaled <- FT_pred_data_emp

FT_pred_data_emp_scaled$hit_speed <- (FT_pred_data_emp_scaled$hit_speed - FT_center_values[2]) / FT_scale_values[2]

FT_pred_data_emp_scaled$hit_angle <- (FT_pred_data_emp_scaled$hit_angle - FT_center_values[3]) / FT_scale_values[3]

FT_pred_prob_hit <- predict(FT_rf.5, FT_pred_data_emp_scaled, type = "prob")[,2]
FT_pred_prob_hit_fits <- cbind(FT_pred_data_emp, FT_pred_prob_hit)

FT_pred_prob_hit_fits[,c(1:2)] <- FT_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

FT_angle_speed_pred <- FT_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = FT_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(low = "#006BA4", high = "#C85200", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nPredicted Probability of a Hit Using Statcast Data (2016)\n") + theme_bp_grey()

FT_angle_speed_pred
################
# Changeup

CH$hit <- with(CH, ifelse(grepl("Single", CH$events), 1,
													ifelse(grepl("Double", CH$events), 1,
																 ifelse(grepl("Triple", CH$events), 1, 
																 			 ifelse(grepl("Home Run", CH$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

CH$fieldingTeam <- with(CH, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[CH$hit_distance_sc == "null"] = NA
#CH$hit_speed[CH$hit_speed == "null"] = NA
#CH$hit_angle[CH$hit_angle == "null"] = NA

# include row names for unique record identification

CH$row <- row.names(CH) %>% as.numeric()

# recode stand and home_team as factors

CH$stand <- as.factor(CH$stand)
CH$home_team <- as.factor(CH$home_team)


CH$game_date <- as.Date(CH$game_date)


# subset 

CH_working_data <- ungroup(CH) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(CH_working_data)
str(ungroup(CH))
table(CH_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

CH_working_data <- filter(CH_working_data, hc_x != 1, hc_y != 1)
head(CH_working_data)

# create training and test sets
# scaled data

set.seed(42)
CH_train <- sample_frac(CH_working_data, .15, replace = FALSE)
CH_split <- setdiff(CH_working_data, CH_train)
CH_test <- sample_frac(CH_split, .50, replace = FALSE)
CH_validate <- setdiff(CH_split, CH_test)

nrow(CH_train) + nrow(CH_test) + nrow(CH_validate) == nrow(CH_working_data)

with(CH_train, table(hit)) %>% prop.table()
with(CH_test, table(hit)) %>% prop.table()
with(CH_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(CH_train)

CH_scaled_data <- scale(CH_train[,c(1:5)])
CH_scale_values <- attr(CH_scaled_data, 'scaled:scale')
CH_scale_values

CH_center_values <- attr(CH_scaled_data, 'scaled:center')
CH_center_values

CH_train <- cbind(CH_scaled_data, select(CH_train, hit:row))

# save levels for factor variables
CH_levels_home_team <- levels(CH_train$home_team)
CH_levels_stand <- levels(CH_train$stand)
CH_levels_fieldingTeam <- levels(CH_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data

CH_test$hit_distance_sc <- (CH_test$hit_distance_sc - CH_center_values[1]) / CH_scale_values[1]

CH_test$hit_speed <- (CH_test$hit_speed - CH_center_values[2]) / CH_scale_values[2]

CH_test$hit_angle <- (CH_test$hit_angle - CH_center_values[3]) / CH_scale_values[3]

CH_test$hc_x <- (CH_test$hc_x - CH_center_values[4]) / CH_scale_values[4]

CH_test$hc_y <- (CH_test$hc_y - CH_center_values[5]) / CH_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

CH_validate$hit_distance_sc <- (CH_validate$hit_distance_sc - CH_center_values[1]) / CH_scale_values[1]

CH_validate$hit_speed <- (CH_validate$hit_speed - CH_center_values[2]) / CH_scale_values[2]

CH_validate$hit_angle <- (CH_validate$hit_angle - CH_center_values[3]) / CH_scale_values[3]

CH_validate$hc_x <- (CH_validate$hc_x - CH_center_values[4]) / CH_scale_values[4]

CH_validate$hc_y <- (CH_validate$hc_y - CH_center_values[5]) / CH_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(CH_train)

set.seed(42)
CH_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(CH_train, -row), ntree = 501, importance = TRUE)

print(CH_rf.1)

plot(CH_rf.1)

varImpPlot(CH_rf.1)

# predict values and tune for optimal out of sample accuracy

opt.cut = function(perf, pred){
	cut.ind = mapply(FUN=function(x, y, p){
		d = (x - 0)^2 + (y-1)^2
		ind = which(d == min(d))
		c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
			cutoff = p[[ind]])
	}, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(roc.perf, pred))

# only speed, angle, stand, fielding team and park

set.seed(42)
CH_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(CH_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(CH_rf.5)

plot(CH_rf.5)

varImpPlot(CH_rf.5)

CH_predict_fit.rf.5 <- data.frame(fits = predict(CH_rf.5, CH_test, type = "prob")[,2], actuals = CH_test$hit)

CH_pred.rf.5 <- prediction(CH_predict_fit.rf.5$fits, CH_predict_fit.rf.5$actuals)

CH_roc.pred.rf.5 <- performance(CH_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(CH_roc.pred.rf.5)
abline(a = 0, b = 1)
CH_opt <- opt.cut(CH_roc.pred.rf.5, CH_pred.rf.5)
CH_opt
CH_opt <- CH_opt[3]
CH_predict_fit.rf.5$fits <- with(CH_predict_fit.rf.5, ifelse(fits > CH_opt, 1, 0)) 

CH_rf.5_confusion_test <- confusionMatrix(model = rf.5, x = CH_test, y = CH_test$hit)
CH_rf.5_confusion_validate <- confusionMatrix(model = rf.5, x = CH_validate, y = CH_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


CH_test_rows <- CH_test$row
CH_validate_rows <- CH_validate$row
CH_pred_data_emp_1 <- ungroup(CH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% CH_test_rows)

CH_pred_data_emp <- ungroup(CH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% CH_validate_rows) %>%
	rbind(CH_pred_data_emp_1)

CH_out_of_training_rows <- CH_pred_data_emp$row

CH_rf.5_full_out_of_sample_data <- ungroup(CH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% CH_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



CH_rf.5_full_out_of_sample_data_scaled <- CH_rf.5_full_out_of_sample_data

CH_rf.5_full_out_of_sample_data_scaled$hit_speed <- (CH_rf.5_full_out_of_sample_data_scaled$hit_speed - CH_center_values[2]) / CH_scale_values[2]

CH_rf.5_full_out_of_sample_data_scaled$hit_angle <- (CH_rf.5_full_out_of_sample_data_scaled$hit_angle - CH_center_values[3]) / CH_scale_values[3]

CH_rf.5.prob <- predict(CH_rf.5, CH_rf.5_full_out_of_sample_data_scaled, type = "response")

CH_rf.5_full_out_of_sample_data <- cbind(filter(CH_rf.5_full_out_of_sample_data, row %in% CH_out_of_training_rows), CH_rf.5.prob)

names(CH_rf.5_full_out_of_sample_data)[65] <- "fits"
names(CH_rf.5_full_out_of_sample_data)

CH_rf.5_full_out_of_sample_data_reduced <- CH_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

CH_rf.5_mean_table <- CH_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

CH_rf.5_full_out_of_sample_data$type <- with(CH_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

CH_rf.5_full_out_of_sample_data$hit_label <- with(CH_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

CH_rf.5_plot1 <- ggplot(CH_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

CH_rf.5_plot1

CH_rf.5_plot2 <- ggplot(CH_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

CH_rf.5_plot2

CH_rf.5_plot3 <- ggplot(CH_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

CH_rf.5_plot3

CH_grid_plot_rf.5 <- grid.arrange(CH_rf.5_plot1, CH_rf.5_plot2, CH_rf.5_plot3, ncol = 3)
CH_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

CH_test_rows <- CH_test$row
CH_validate_rows <- CH_validate$row
CH_pred_data_emp_1 <- ungroup(CH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% test_rows)

CH_pred_data_emp <- ungroup(CH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% validate_rows) %>%
	rbind(CH_pred_data_emp_1)

CH_out_of_training_rows <- CH_pred_data_emp$row

CH_pred_data_emp <- ungroup(CH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% CH_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

CH_pred_data_emp_scaled <- CH_pred_data_emp

CH_pred_data_emp_scaled$hit_speed <- (CH_pred_data_emp_scaled$hit_speed - CH_center_values[2]) / CH_scale_values[2]

CH_pred_data_emp_scaled$hit_angle <- (CH_pred_data_emp_scaled$hit_angle - CH_center_values[3]) / CH_scale_values[3]

CH_pred_prob_hit <- predict(CH_rf.5, CH_pred_data_emp_scaled, type = "prob")[,2]
CH_pred_prob_hit_fits <- cbind(CH_pred_data_emp, CH_pred_prob_hit)

CH_pred_prob_hit_fits[,c(1:2)] <- CH_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

CH_angle_speed_pred <- CH_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = CH_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(low = "#006BA4", high = "#C85200", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nPredicted Probability of a Hit Using Statcast Data (2016)\n") + theme_bp_grey()

CH_angle_speed_pred


#########
# FC

FC$hit <- with(FC, ifelse(grepl("Single", FC$events), 1,
													ifelse(grepl("Double", FC$events), 1,
																 ifelse(grepl("Triple", FC$events), 1, 
																 			 ifelse(grepl("Home Run", FC$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

FC$fieldingTeam <- with(FC, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[FC$hit_distance_sc == "null"] = NA
#FC$hit_speed[FC$hit_speed == "null"] = NA
#FC$hit_angle[FC$hit_angle == "null"] = NA

# include row names for unique record identification

FC$row <- row.names(FC) %>% as.numeric()

# recode stand and home_team as factors

FC$stand <- as.factor(FC$stand)
FC$home_team <- as.factor(FC$home_team)


FC$game_date <- as.Date(FC$game_date)


# subset 

FC_working_data <- ungroup(FC) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(FC_working_data)
str(ungroup(FC))
table(FC_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

FC_working_data <- filter(FC_working_data, hc_x != 1, hc_y != 1)
head(FC_working_data)

# create training and test sets
# scaled data

set.seed(42)
FC_train <- sample_frac(FC_working_data, .15, replace = FALSE)
FC_split <- setdiff(FC_working_data, FC_train)
FC_test <- sample_frac(FC_split, .50, replace = FALSE)
FC_validate <- setdiff(FC_split, FC_test)

nrow(FC_train) + nrow(FC_test) + nrow(FC_validate) == nrow(FC_working_data)

with(FC_train, table(hit)) %>% prop.table()
with(FC_test, table(hit)) %>% prop.table()
with(FC_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(FC_train)

FC_scaled_data <- scale(FC_train[,c(1:5)])
FC_scale_values <- attr(FC_scaled_data, 'scaled:scale')
FC_scale_values

FC_center_values <- attr(FC_scaled_data, 'scaled:center')
FC_center_values

FC_train <- cbind(FC_scaled_data, select(FC_train, hit:row))

# save levels for factor variables
FC_levels_home_team <- levels(FC_train$home_team)
FC_levels_stand <- levels(FC_train$stand)
FC_levels_fieldingTeam <- levels(FC_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data

FC_test$hit_distance_sc <- (FC_test$hit_distance_sc - FC_center_values[1]) / FC_scale_values[1]

FC_test$hit_speed <- (FC_test$hit_speed - FC_center_values[2]) / FC_scale_values[2]

FC_test$hit_angle <- (FC_test$hit_angle - FC_center_values[3]) / FC_scale_values[3]

FC_test$hc_x <- (FC_test$hc_x - FC_center_values[4]) / FC_scale_values[4]

FC_test$hc_y <- (FC_test$hc_y - FC_center_values[5]) / FC_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

FC_validate$hit_distance_sc <- (FC_validate$hit_distance_sc - FC_center_values[1]) / FC_scale_values[1]

FC_validate$hit_speed <- (FC_validate$hit_speed - FC_center_values[2]) / FC_scale_values[2]

FC_validate$hit_angle <- (FC_validate$hit_angle - FC_center_values[3]) / FC_scale_values[3]

FC_validate$hc_x <- (FC_validate$hc_x - FC_center_values[4]) / FC_scale_values[4]

FC_validate$hc_y <- (FC_validate$hc_y - FC_center_values[5]) / FC_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(FC_train)

set.seed(42)
FC_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(FC_train, -row), ntree = 501, importance = TRUE)

print(FC_rf.1)

plot(FC_rf.1)

varImpPlot(FC_rf.1)

# predict values and tune for optimal out of sample accuracy

opt.cut = function(perf, pred){
	cut.ind = mapply(FUN=function(x, y, p){
		d = (x - 0)^2 + (y-1)^2
		ind = which(d == min(d))
		c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
			cutoff = p[[ind]])
	}, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(roc.perf, pred))

# only speed, angle, stand, fielding team and park

set.seed(42)
FC_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(FC_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(FC_rf.5)

plot(FC_rf.5)

varImpPlot(FC_rf.5)

FC_predict_fit.rf.5 <- data.frame(fits = predict(FC_rf.5, FC_test, type = "prob")[,2], actuals = FC_test$hit)

FC_pred.rf.5 <- prediction(FC_predict_fit.rf.5$fits, FC_predict_fit.rf.5$actuals)

FC_roc.pred.rf.5 <- performance(FC_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(FC_roc.pred.rf.5)
abline(a = 0, b = 1)
FC_opt <- opt.cut(FC_roc.pred.rf.5, FC_pred.rf.5)
FC_opt
FC_opt <- FC_opt[3]
FC_predict_fit.rf.5$fits <- with(FC_predict_fit.rf.5, ifelse(fits > FC_opt, 1, 0)) 

FC_rf.5_confusion_test <- confusionMatrix(model = rf.5, x = FC_test, y = FC_test$hit)
FC_rf.5_confusion_validate <- confusionMatrix(model = rf.5, x = FC_validate, y = FC_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


FC_test_rows <- FC_test$row
FC_validate_rows <- FC_validate$row
FC_pred_data_emp_1 <- ungroup(FC) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% FC_test_rows)

FC_pred_data_emp <- ungroup(FC) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% FC_validate_rows) %>%
	rbind(FC_pred_data_emp_1)

FC_out_of_training_rows <- FC_pred_data_emp$row

FC_rf.5_full_out_of_sample_data <- ungroup(FC) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% FC_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



FC_rf.5_full_out_of_sample_data_scaled <- FC_rf.5_full_out_of_sample_data

FC_rf.5_full_out_of_sample_data_scaled$hit_speed <- (FC_rf.5_full_out_of_sample_data_scaled$hit_speed - FC_center_values[2]) / FC_scale_values[2]

FC_rf.5_full_out_of_sample_data_scaled$hit_angle <- (FC_rf.5_full_out_of_sample_data_scaled$hit_angle - FC_center_values[3]) / FC_scale_values[3]

FC_rf.5.prob <- predict(FC_rf.5, FC_rf.5_full_out_of_sample_data_scaled, type = "response")

FC_rf.5_full_out_of_sample_data <- cbind(filter(FC_rf.5_full_out_of_sample_data, row %in% FC_out_of_training_rows), FC_rf.5.prob)

names(FC_rf.5_full_out_of_sample_data)[65] <- "fits"
names(FC_rf.5_full_out_of_sample_data)

FC_rf.5_full_out_of_sample_data_reduced <- FC_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

FC_rf.5_mean_table <- FC_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

FC_rf.5_full_out_of_sample_data$type <- with(FC_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

FC_rf.5_full_out_of_sample_data$hit_label <- with(FC_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

FC_rf.5_plot1 <- ggplot(FC_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

FC_rf.5_plot1

FC_rf.5_plot2 <- ggplot(FC_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

FC_rf.5_plot2

FC_rf.5_plot3 <- ggplot(FC_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

FC_rf.5_plot3

FC_grid_plot_rf.5 <- grid.arrange(FC_rf.5_plot1, FC_rf.5_plot2, FC_rf.5_plot3, ncol = 3)
FC_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

FC_test_rows <- FC_test$row
FC_validate_rows <- FC_validate$row
FC_pred_data_emp_1 <- ungroup(FC) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% test_rows)

FC_pred_data_emp <- ungroup(FC) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% validate_rows) %>%
	rbind(FC_pred_data_emp_1)

FC_out_of_training_rows <- FC_pred_data_emp$row

FC_pred_data_emp <- ungroup(FC) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% FC_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

FC_pred_data_emp_scaled <- FC_pred_data_emp

FC_pred_data_emp_scaled$hit_speed <- (FC_pred_data_emp_scaled$hit_speed - FC_center_values[2]) / FC_scale_values[2]

FC_pred_data_emp_scaled$hit_angle <- (FC_pred_data_emp_scaled$hit_angle - FC_center_values[3]) / FC_scale_values[3]

FC_pred_prob_hit <- predict(FC_rf.5, FC_pred_data_emp_scaled, type = "prob")[,2]
FC_pred_prob_hit_fits <- cbind(FC_pred_data_emp, FC_pred_prob_hit)

FC_pred_prob_hit_fits[,c(1:2)] <- FC_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

FC_angle_speed_pred <- FC_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = FC_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(low = "#006BA4", high = "#C85200", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nPredicted Probability of a Hit Using Statcast Data (2016)\n") + theme_bp_grey()

FC_angle_speed_pred

#######
### FS

FS$hit <- with(FS, ifelse(grepl("Single", FS$events), 1,
													ifelse(grepl("Double", FS$events), 1,
																 ifelse(grepl("Triple", FS$events), 1, 
																 			 ifelse(grepl("Home Run", FS$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

FS$fieldingTeam <- with(FS, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[FS$hit_distance_sc == "null"] = NA
#FS$hit_speed[FS$hit_speed == "null"] = NA
#FS$hit_angle[FS$hit_angle == "null"] = NA

# include row names for unique record identification

FS$row <- row.names(FS) %>% as.numeric()

# recode stand and home_team as factors

FS$stand <- as.factor(FS$stand)
FS$home_team <- as.factor(FS$home_team)


FS$game_date <- as.Date(FS$game_date)


# subset 

FS_working_data <- ungroup(FS) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(FS_working_data)
str(ungroup(FS))
table(FS_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

FS_working_data <- filter(FS_working_data, hc_x != 1, hc_y != 1)
head(FS_working_data)

# create training and test sets
# scaled data

set.seed(42)
FS_train <- sample_frac(FS_working_data, .15, replace = FALSE)
FS_split <- setdiff(FS_working_data, FS_train)
FS_test <- sample_frac(FS_split, .50, replace = FALSE)
FS_validate <- setdiff(FS_split, FS_test)

nrow(FS_train) + nrow(FS_test) + nrow(FS_validate) == nrow(FS_working_data)

with(FS_train, table(hit)) %>% prop.table()
with(FS_test, table(hit)) %>% prop.table()
with(FS_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(FS_train)

FS_scaled_data <- scale(FS_train[,c(1:5)])
FS_scale_values <- attr(FS_scaled_data, 'scaled:scale')
FS_scale_values

FS_center_values <- attr(FS_scaled_data, 'scaled:center')
FS_center_values

FS_train <- cbind(FS_scaled_data, select(FS_train, hit:row))

# save levels for factor variables
FS_levels_home_team <- levels(FS_train$home_team)
FS_levels_stand <- levels(FS_train$stand)
FS_levels_fieldingTeam <- levels(FS_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data

FS_test$hit_distance_sc <- (FS_test$hit_distance_sc - FS_center_values[1]) / FS_scale_values[1]

FS_test$hit_speed <- (FS_test$hit_speed - FS_center_values[2]) / FS_scale_values[2]

FS_test$hit_angle <- (FS_test$hit_angle - FS_center_values[3]) / FS_scale_values[3]

FS_test$hc_x <- (FS_test$hc_x - FS_center_values[4]) / FS_scale_values[4]

FS_test$hc_y <- (FS_test$hc_y - FS_center_values[5]) / FS_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

FS_validate$hit_distance_sc <- (FS_validate$hit_distance_sc - FS_center_values[1]) / FS_scale_values[1]

FS_validate$hit_speed <- (FS_validate$hit_speed - FS_center_values[2]) / FS_scale_values[2]

FS_validate$hit_angle <- (FS_validate$hit_angle - FS_center_values[3]) / FS_scale_values[3]

FS_validate$hc_x <- (FS_validate$hc_x - FS_center_values[4]) / FS_scale_values[4]

FS_validate$hc_y <- (FS_validate$hc_y - FS_center_values[5]) / FS_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(FS_train)

set.seed(42)
FS_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(FS_train, -row), ntree = 501, importance = TRUE)

print(FS_rf.1)

plot(FS_rf.1)

varImpPlot(FS_rf.1)

# predict values and tune for optimal out of sample accuracy

opt.cut = function(perf, pred){
	cut.ind = mapply(FUN=function(x, y, p){
		d = (x - 0)^2 + (y-1)^2
		ind = which(d == min(d))
		c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
			cutoff = p[[ind]])
	}, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(roc.perf, pred))

# only speed, angle, stand, fielding team and park

set.seed(42)
FS_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(FS_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(FS_rf.5)

plot(FS_rf.5)

varImpPlot(FS_rf.5)

FS_predict_fit.rf.5 <- data.frame(fits = predict(FS_rf.5, FS_test, type = "prob")[,2], actuals = FS_test$hit)

FS_pred.rf.5 <- prediction(FS_predict_fit.rf.5$fits, FS_predict_fit.rf.5$actuals)

FS_roc.pred.rf.5 <- performance(FS_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(FS_roc.pred.rf.5)
abline(a = 0, b = 1)
FS_opt <- opt.cut(FS_roc.pred.rf.5, FS_pred.rf.5)
FS_opt
FS_opt <- FS_opt[3]
FS_predict_fit.rf.5$fits <- with(FS_predict_fit.rf.5, ifelse(fits > FS_opt, 1, 0)) 

FS_rf.5_confusion_test <- confusionMatrix(model = rf.5, x = FS_test, y = FS_test$hit)
FS_rf.5_confusion_validate <- confusionMatrix(model = rf.5, x = FS_validate, y = FS_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


FS_test_rows <- FS_test$row
FS_validate_rows <- FS_validate$row
FS_pred_data_emp_1 <- ungroup(FS) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% FS_test_rows)

FS_pred_data_emp <- ungroup(FS) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% FS_validate_rows) %>%
	rbind(FS_pred_data_emp_1)

FS_out_of_training_rows <- FS_pred_data_emp$row

FS_rf.5_full_out_of_sample_data <- ungroup(FS) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% FS_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



FS_rf.5_full_out_of_sample_data_scaled <- FS_rf.5_full_out_of_sample_data

FS_rf.5_full_out_of_sample_data_scaled$hit_speed <- (FS_rf.5_full_out_of_sample_data_scaled$hit_speed - FS_center_values[2]) / FS_scale_values[2]

FS_rf.5_full_out_of_sample_data_scaled$hit_angle <- (FS_rf.5_full_out_of_sample_data_scaled$hit_angle - FS_center_values[3]) / FS_scale_values[3]

FS_rf.5.prob <- predict(FS_rf.5, FS_rf.5_full_out_of_sample_data_scaled, type = "response")

FS_rf.5_full_out_of_sample_data <- cbind(filter(FS_rf.5_full_out_of_sample_data, row %in% FS_out_of_training_rows), FS_rf.5.prob)

names(FS_rf.5_full_out_of_sample_data)[65] <- "fits"
names(FS_rf.5_full_out_of_sample_data)

FS_rf.5_full_out_of_sample_data_reduced <- FS_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

FS_rf.5_mean_table <- FS_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

FS_rf.5_full_out_of_sample_data$type <- with(FS_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

FS_rf.5_full_out_of_sample_data$hit_label <- with(FS_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

FS_rf.5_plot1 <- ggplot(FS_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

FS_rf.5_plot1

FS_rf.5_plot2 <- ggplot(FS_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

FS_rf.5_plot2

FS_rf.5_plot3 <- ggplot(FS_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

FS_rf.5_plot3

FS_grid_plot_rf.5 <- grid.arrange(FS_rf.5_plot1, FS_rf.5_plot2, FS_rf.5_plot3, ncol = 3)
FS_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

FS_test_rows <- FS_test$row
FS_validate_rows <- FS_validate$row
FS_pred_data_emp_1 <- ungroup(FS) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% test_rows)

FS_pred_data_emp <- ungroup(FS) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% validate_rows) %>%
	rbind(FS_pred_data_emp_1)

FS_out_of_training_rows <- FS_pred_data_emp$row

FS_pred_data_emp <- ungroup(FS) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% FS_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

FS_pred_data_emp_scaled <- FS_pred_data_emp

FS_pred_data_emp_scaled$hit_speed <- (FS_pred_data_emp_scaled$hit_speed - FS_center_values[2]) / FS_scale_values[2]

FS_pred_data_emp_scaled$hit_angle <- (FS_pred_data_emp_scaled$hit_angle - FS_center_values[3]) / FS_scale_values[3]

FS_pred_prob_hit <- predict(FS_rf.5, FS_pred_data_emp_scaled, type = "prob")[,2]
FS_pred_prob_hit_fits <- cbind(FS_pred_data_emp, FS_pred_prob_hit)

FS_pred_prob_hit_fits[,c(1:2)] <- FS_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

FS_angle_speed_pred <- FS_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = FS_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(low = "#006BA4", high = "#C85200", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nPredicted Probability of a Hit Using Statcast Data (2016)\n") + theme_bp_grey()

FS_angle_speed_pred



##############
###### KN

KN$hit <- with(KN, ifelse(grepl("Single", KN$events), 1,
													ifelse(grepl("Double", KN$events), 1,
																 ifelse(grepl("Triple", KN$events), 1, 
																 			 ifelse(grepl("Home Run", KN$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

KN$fieldingTeam <- with(KN, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[KN$hit_distance_sc == "null"] = NA
#KN$hit_speed[KN$hit_speed == "null"] = NA
#KN$hit_angle[KN$hit_angle == "null"] = NA

# include row names for unique record identification

KN$row <- row.names(KN) %>% as.numeric()

# recode stand and home_team as factors

KN$stand <- as.factor(KN$stand)
KN$home_team <- as.factor(KN$home_team)


KN$game_date <- as.Date(KN$game_date)


# subset 

KN_working_data <- ungroup(KN) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(KN_working_data)
str(ungroup(KN))
table(KN_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

KN_working_data <- filter(KN_working_data, hc_x != 1, hc_y != 1)
head(KN_working_data)

# create training and test sets
# scaled data

set.seed(42)
KN_train <- sample_frac(KN_working_data, .15, replace = FALSE)
KN_split <- setdiff(KN_working_data, KN_train)
KN_test <- sample_frac(KN_split, .50, replace = FALSE)
KN_validate <- setdiff(KN_split, KN_test)

nrow(KN_train) + nrow(KN_test) + nrow(KN_validate) == nrow(KN_working_data)

with(KN_train, table(hit)) %>% prop.table()
with(KN_test, table(hit)) %>% prop.table()
with(KN_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(KN_train)

KN_scaled_data <- scale(KN_train[,c(1:5)])
KN_scale_values <- attr(KN_scaled_data, 'scaled:scale')
KN_scale_values

KN_center_values <- attr(KN_scaled_data, 'scaled:center')
KN_center_values

KN_train <- cbind(KN_scaled_data, select(KN_train, hit:row))

# save levels for factor variables
KN_levels_home_team <- levels(KN_train$home_team)
KN_levels_stand <- levels(KN_train$stand)
KN_levels_fieldingTeam <- levels(KN_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data

KN_test$hit_distance_sc <- (KN_test$hit_distance_sc - KN_center_values[1]) / KN_scale_values[1]

KN_test$hit_speed <- (KN_test$hit_speed - KN_center_values[2]) / KN_scale_values[2]

KN_test$hit_angle <- (KN_test$hit_angle - KN_center_values[3]) / KN_scale_values[3]

KN_test$hc_x <- (KN_test$hc_x - KN_center_values[4]) / KN_scale_values[4]

KN_test$hc_y <- (KN_test$hc_y - KN_center_values[5]) / KN_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

KN_validate$hit_distance_sc <- (KN_validate$hit_distance_sc - KN_center_values[1]) / KN_scale_values[1]

KN_validate$hit_speed <- (KN_validate$hit_speed - KN_center_values[2]) / KN_scale_values[2]

KN_validate$hit_angle <- (KN_validate$hit_angle - KN_center_values[3]) / KN_scale_values[3]

KN_validate$hc_x <- (KN_validate$hc_x - KN_center_values[4]) / KN_scale_values[4]

KN_validate$hc_y <- (KN_validate$hc_y - KN_center_values[5]) / KN_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(KN_train)

set.seed(42)
KN_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(KN_train, -row), ntree = 501, importance = TRUE)

print(KN_rf.1)

plot(KN_rf.1)

varImpPlot(KN_rf.1)

# predict values and tune for optimal out of sample accuracy

opt.cut = function(perf, pred){
	cut.ind = mapply(FUN=function(x, y, p){
		d = (x - 0)^2 + (y-1)^2
		ind = which(d == min(d))
		c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
			cutoff = p[[ind]])
	}, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(roc.perf, pred))

# only speed, angle, stand, fielding team and park

set.seed(42)
KN_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(KN_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(KN_rf.5)

plot(KN_rf.5)

varImpPlot(KN_rf.5)

KN_predict_fit.rf.5 <- data.frame(fits = predict(KN_rf.5, KN_test, type = "prob")[,2], actuals = KN_test$hit)

KN_pred.rf.5 <- prediction(KN_predict_fit.rf.5$fits, KN_predict_fit.rf.5$actuals)

KN_roc.pred.rf.5 <- performance(KN_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(KN_roc.pred.rf.5)
abline(a = 0, b = 1)
KN_opt <- opt.cut(KN_roc.pred.rf.5, KN_pred.rf.5)
KN_opt
KN_opt <- KN_opt[3]
KN_predict_fit.rf.5$fits <- with(KN_predict_fit.rf.5, ifelse(fits > KN_opt, 1, 0)) 

KN_rf.5_confusion_test <- confusionMatrix(model = rf.5, x = KN_test, y = KN_test$hit)
KN_rf.5_confusion_validate <- confusionMatrix(model = rf.5, x = KN_validate, y = KN_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


KN_test_rows <- KN_test$row
KN_validate_rows <- KN_validate$row
KN_pred_data_emp_1 <- ungroup(KN) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% KN_test_rows)

KN_pred_data_emp <- ungroup(KN) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% KN_validate_rows) %>%
	rbind(KN_pred_data_emp_1)

KN_out_of_training_rows <- KN_pred_data_emp$row

KN_rf.5_full_out_of_sample_data <- ungroup(KN) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% KN_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



KN_rf.5_full_out_of_sample_data_scaled <- KN_rf.5_full_out_of_sample_data

KN_rf.5_full_out_of_sample_data_scaled$hit_speed <- (KN_rf.5_full_out_of_sample_data_scaled$hit_speed - KN_center_values[2]) / KN_scale_values[2]

KN_rf.5_full_out_of_sample_data_scaled$hit_angle <- (KN_rf.5_full_out_of_sample_data_scaled$hit_angle - KN_center_values[3]) / KN_scale_values[3]

KN_rf.5.prob <- predict(KN_rf.5, KN_rf.5_full_out_of_sample_data_scaled, type = "response")

KN_rf.5_full_out_of_sample_data <- cbind(filter(KN_rf.5_full_out_of_sample_data, row %in% KN_out_of_training_rows), KN_rf.5.prob)

names(KN_rf.5_full_out_of_sample_data)[65] <- "fits"
names(KN_rf.5_full_out_of_sample_data)

KN_rf.5_full_out_of_sample_data_reduced <- KN_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

KN_rf.5_mean_table <- KN_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

KN_rf.5_full_out_of_sample_data$type <- with(KN_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

KN_rf.5_full_out_of_sample_data$hit_label <- with(KN_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

KN_rf.5_plot1 <- ggplot(KN_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

KN_rf.5_plot1

KN_rf.5_plot2 <- ggplot(KN_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

KN_rf.5_plot2

KN_rf.5_plot3 <- ggplot(KN_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

KN_rf.5_plot3

KN_grid_plot_rf.5 <- grid.arrange(KN_rf.5_plot1, KN_rf.5_plot2, KN_rf.5_plot3, ncol = 3)
KN_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

KN_test_rows <- KN_test$row
KN_validate_rows <- KN_validate$row
KN_pred_data_emp_1 <- ungroup(KN) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% test_rows)

KN_pred_data_emp <- ungroup(KN) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% validate_rows) %>%
	rbind(KN_pred_data_emp_1)

KN_out_of_training_rows <- KN_pred_data_emp$row

KN_pred_data_emp <- ungroup(KN) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% KN_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

KN_pred_data_emp_scaled <- KN_pred_data_emp

KN_pred_data_emp_scaled$hit_speed <- (KN_pred_data_emp_scaled$hit_speed - KN_center_values[2]) / KN_scale_values[2]

KN_pred_data_emp_scaled$hit_angle <- (KN_pred_data_emp_scaled$hit_angle - KN_center_values[3]) / KN_scale_values[3]

KN_pred_prob_hit <- predict(KN_rf.5, KN_pred_data_emp_scaled, type = "prob")[,2]
KN_pred_prob_hit_fits <- cbind(KN_pred_data_emp, KN_pred_prob_hit)

KN_pred_prob_hit_fits[,c(1:2)] <- KN_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

KN_angle_speed_pred <- KN_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = KN_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(low = "#006BA4", high = "#C85200", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nPredicted Probability of a Hit Using Statcast Data (2016)\n") + theme_bp_grey()

KN_angle_speed_pred

######################
### SC

SI$hit_distance_sc <- as.numeric(SI$hit_distance_sc)
SI$hit_angle <- as.numeric(SI$hit_angle)
SI$hit_speed <- as.numeric(SI$hit_speed)
SI$hit <- with(SI, ifelse(grepl("Single", SI$events), 1,
													ifelse(grepl("Double", SI$events), 1,
																 ifelse(grepl("Triple", SI$events), 1, 
																 			 ifelse(grepl("Home Run", SI$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

SI$fieldingTeam <- with(SI, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[SI$hit_distance_sc == "null"] = NA
#SI$hit_speed[SI$hit_speed == "null"] = NA
#SI$hit_angle[SI$hit_angle == "null"] = NA

# include row names for unique record identification

SI$row <- row.names(SI) %>% as.numeric()

# recode stand and home_team as factors

SI$stand <- as.factor(SI$stand)
SI$home_team <- as.factor(SI$home_team)


SI$game_date <- as.Date(SI$game_date)


# subset 

SI_working_data <- ungroup(SI) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(SI_working_data)
str(ungroup(SI))
table(SI_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

SI_working_data <- filter(SI_working_data, hc_x != 1, hc_y != 1)
head(SI_working_data)

# create training and test sets
# scaled data

set.seed(42)
SI_train <- sample_frac(SI_working_data, .15, replace = FALSE)
SI_split <- setdiff(SI_working_data, SI_train)
SI_test <- sample_frac(SI_split, .50, replace = FALSE)
SI_validate <- setdiff(SI_split, SI_test)

nrow(SI_train) + nrow(SI_test) + nrow(SI_validate) == nrow(SI_working_data)

with(SI_train, table(hit)) %>% prop.table()
with(SI_test, table(hit)) %>% prop.table()
with(SI_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(SI_train)

SI_scaled_data <- scale(SI_train[,c(1:5)])
SI_scale_values <- attr(SI_scaled_data, 'scaled:scale')
SI_scale_values

SI_center_values <- attr(SI_scaled_data, 'scaled:center')
SI_center_values

SI_train <- cbind(SI_scaled_data, select(SI_train, hit:row))

# save levels for factor variables
SI_levels_home_team <- levels(SI_train$home_team)
SI_levels_stand <- levels(SI_train$stand)
SI_levels_fieldingTeam <- levels(SI_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data

SI_test$hit_distance_sc <- (SI_test$hit_distance_sc - SI_center_values[1]) / SI_scale_values[1]

SI_test$hit_speed <- (SI_test$hit_speed - SI_center_values[2]) / SI_scale_values[2]

SI_test$hit_angle <- (SI_test$hit_angle - SI_center_values[3]) / SI_scale_values[3]

SI_test$hc_x <- (SI_test$hc_x - SI_center_values[4]) / SI_scale_values[4]

SI_test$hc_y <- (SI_test$hc_y - SI_center_values[5]) / SI_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

SI_validate$hit_distance_sc <- (SI_validate$hit_distance_sc - SI_center_values[1]) / SI_scale_values[1]

SI_validate$hit_speed <- (SI_validate$hit_speed - SI_center_values[2]) / SI_scale_values[2]

SI_validate$hit_angle <- (SI_validate$hit_angle - SI_center_values[3]) / SI_scale_values[3]

SI_validate$hc_x <- (SI_validate$hc_x - SI_center_values[4]) / SI_scale_values[4]

SI_validate$hc_y <- (SI_validate$hc_y - SI_center_values[5]) / SI_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(SI_train)

set.seed(42)
SI_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(SI_train, -row), ntree = 501, importance = TRUE)

print(SI_rf.1)

plot(SI_rf.1)

varImpPlot(SI_rf.1)

# predict values and tune for optimal out of sample accuracy

opt.cut = function(perf, pred){
	cut.ind = mapply(FUN=function(x, y, p){
		d = (x - 0)^2 + (y-1)^2
		ind = which(d == min(d))
		c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
			cutoff = p[[ind]])
	}, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(roc.perf, pred))

# only speed, angle, stand, fielding team and park

set.seed(42)
SI_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(SI_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(SI_rf.5)

plot(SI_rf.5)

varImpPlot(SI_rf.5)

SI_predict_fit.rf.5 <- data.frame(fits = predict(SI_rf.5, SI_test, type = "prob")[,2], actuals = SI_test$hit)

SI_pred.rf.5 <- prediction(SI_predict_fit.rf.5$fits, SI_predict_fit.rf.5$actuals)

SI_roc.pred.rf.5 <- performance(SI_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(SI_roc.pred.rf.5)
abline(a = 0, b = 1)
SI_opt <- opt.cut(SI_roc.pred.rf.5, SI_pred.rf.5)
SI_opt
SI_opt <- SI_opt[3]
SI_predict_fit.rf.5$fits <- with(SI_predict_fit.rf.5, ifelse(fits > SI_opt, 1, 0)) 

SI_rf.5_confusion_test <- confusionMatrix(model = rf.5, x = SI_test, y = SI_test$hit)
SI_rf.5_confusion_validate <- confusionMatrix(model = rf.5, x = SI_validate, y = SI_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


SI_test_rows <- SI_test$row
SI_validate_rows <- SI_validate$row
SI_pred_data_emp_1 <- ungroup(SI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% SI_test_rows)

SI_pred_data_emp <- ungroup(SI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% SI_validate_rows) %>%
	rbind(SI_pred_data_emp_1)

SI_out_of_training_rows <- SI_pred_data_emp$row

SI_rf.5_full_out_of_sample_data <- ungroup(SI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% SI_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



SI_rf.5_full_out_of_sample_data_scaled <- SI_rf.5_full_out_of_sample_data

SI_rf.5_full_out_of_sample_data_scaled$hit_speed <- (SI_rf.5_full_out_of_sample_data_scaled$hit_speed - SI_center_values[2]) / SI_scale_values[2]

SI_rf.5_full_out_of_sample_data_scaled$hit_angle <- (SI_rf.5_full_out_of_sample_data_scaled$hit_angle - SI_center_values[3]) / SI_scale_values[3]

SI_rf.5.prob <- predict(SI_rf.5, SI_rf.5_full_out_of_sample_data_scaled, type = "response")

SI_rf.5_full_out_of_sample_data <- cbind(filter(SI_rf.5_full_out_of_sample_data, row %in% SI_out_of_training_rows), SI_rf.5.prob)

names(SI_rf.5_full_out_of_sample_data)[65] <- "fits"
names(SI_rf.5_full_out_of_sample_data)

SI_rf.5_full_out_of_sample_data_reduced <- SI_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

SI_rf.5_mean_table <- SI_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

SI_rf.5_full_out_of_sample_data$type <- with(SI_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

SI_rf.5_full_out_of_sample_data$hit_label <- with(SI_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

SI_rf.5_plot1 <- ggplot(SI_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

SI_rf.5_plot1

SI_rf.5_plot2 <- ggplot(SI_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

SI_rf.5_plot2

SI_rf.5_plot3 <- ggplot(SI_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

SI_rf.5_plot3

SI_grid_plot_rf.5 <- grid.arrange(SI_rf.5_plot1, SI_rf.5_plot2, SI_rf.5_plot3, ncol = 3)
SI_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

SI_test_rows <- SI_test$row
SI_validate_rows <- SI_validate$row
SI_pred_data_emp_1 <- ungroup(SI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% test_rows)

SI_pred_data_emp <- ungroup(SI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% validate_rows) %>%
	rbind(SI_pred_data_emp_1)

SI_out_of_training_rows <- SI_pred_data_emp$row

SI_pred_data_emp <- ungroup(SI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% SI_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

SI_pred_data_emp_scaled <- SI_pred_data_emp

SI_pred_data_emp_scaled$hit_speed <- (SI_pred_data_emp_scaled$hit_speed - SI_center_values[2]) / SI_scale_values[2]

SI_pred_data_emp_scaled$hit_angle <- (SI_pred_data_emp_scaled$hit_angle - SI_center_values[3]) / SI_scale_values[3]

SI_pred_data_emp_scaled$hit_ <- (SI_pred_data_emp_scaled$hit_speed - SI_center_values[2]) / SI_scale_values[2]##

SI_pred_prob_hit <- predict(SI_rf.5, SI_pred_data_emp_scaled, type = "prob")[,2]
SI_pred_prob_hit_fits <- cbind(SI_pred_data_emp, SI_pred_prob_hit)

SI_pred_prob_hit_fits[,c(1:2)] <- SI_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

SI_angle_speed_pred <- SI_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = SI_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(low = "#006BA4", high = "#C85200", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nPredicted Probability of a Hit Using Statcast Data (2016)\n") + theme_bp_grey()

SI_angle_speed_pred

##
SI_dist_angle_pred <- SI_pred_prob_hit_fits %>% ggplot(aes(hit_distance_sc, hit_angle)) + geom_tile(aes(fill = SI_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(low = "#006BA4", high = "#C85200", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nPredicted Probability of a Hit Using Statcast Data (2016)\n") + theme_bp_grey()
##
SI_dist_angle_pred




#######STOP HERE###########
#####
#####





ggsave("angle_speed_pred.png", angle_speed_pred, scale = 1.2, width = 11, height = 8.5, units = "in")

# calculate average probabilities by bucketed angles and speed, and compare to actual 

pred_prob_hit_fits$angle_buckets <- cut(pred_prob_hit_fits$hit_angle, seq(from = -90, to = 90, by = 10), include.lowest = TRUE)

pred_prob_hit_fits$speed_buckets <- cut(pred_prob_hit_fits$hit_speed, seq(from = 0, to = 140, by = 10), include.lowest = TRUE)

pred_prob_by_buckets <- pred_prob_hit_fits %>% 
	group_by(angle_buckets, speed_buckets) %>% 
	summarise(ave_prob = round(mean(pred_prob_hit),3)) %>%
	ungroup(.) %>%
	arrange(desc(speed_buckets), desc(angle_buckets))

empirical_prob_hit <- select(working_data_original, hit_distance_sc:hit)

empirical_prob_hit$angle_buckets <- cut(empirical_prob_hit$hit_angle, seq(from = -90, to = 90, by = 10), include.lowest = TRUE)

empirical_prob_hit$speed_buckets <- cut(empirical_prob_hit$hit_speed, seq(from = 0, to = 140, by = 10), include.lowest = TRUE)

empirical_prob_hit <- empirical_prob_hit %>% 
	group_by(angle_buckets, speed_buckets) %>% 
	summarise(count = n(), empirical_ave = round(mean(hit),3)) %>%
	ungroup(.) %>%
	arrange(desc(speed_buckets), desc(angle_buckets))

compare_prob <- left_join(pred_prob_by_buckets, empirical_prob_hit, by = c("angle_buckets", "speed_buckets"))

compare_prob <- arrange(compare_prob, angle_buckets, speed_buckets)

# remove apparently miscoded balls with x,y of 1,1

working_data <- filter(working_data, hc_x != 1, hc_y != 1)
head(working_data)

# create training and test sets
# scaled data

set.seed(42)
train <- sample_frac(working_data, .15, replace = FALSE)
split <- setdiff(working_data, train)
test <- sample_frac(split, .50, replace = FALSE)
validate <- setdiff(split, test)

nrow(train) + nrow(test) + nrow(validate) == nrow(working_data)

with(train, table(hit)) %>% prop.table()
with(test, table(hit)) %>% prop.table()
with(validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(train)

scaled_data <- scale(train[,c(1:5)])
scale_values <- attr(scaled_data, 'scaled:scale')
scale_values

center_values <- attr(scaled_data, 'scaled:center')
center_values

train <- cbind(scaled_data, select(train, hit:row))
#train <- train[-c(10:12)]
head(train)

# save levels for factor variables
levels_home_team <- levels(train$home_team)
levels_stand <- levels(train$stand)
levels_fieldingTeam <- levels(train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
l#evels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data

test$hit_distance_sc <- (test$hit_distance_sc - center_values[1]) / scale_values[1]

test$hit_speed <- (test$hit_speed - center_values[2]) / scale_values[2]

test$hit_angle <- (test$hit_angle - center_values[3]) / scale_values[3]

test$hc_x <- (test$hc_x - center_values[4]) / scale_values[4]

test$hc_y <- (test$hc_y - center_values[5]) / scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

validate$hit_distance_sc <- (validate$hit_distance_sc - center_values[1]) / scale_values[1]

validate$hit_speed <- (validate$hit_speed - center_values[2]) / scale_values[2]

validate$hit_angle <- (validate$hit_angle - center_values[3]) / scale_values[3]

validate$hc_x <- (validate$hc_x - center_values[4]) / scale_values[4]

validate$hc_y <- (validate$hc_y - center_values[5]) / scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(train)


# predict values and tune for optimal out of sample accuracy

opt.cut = function(perf, pred){
	cut.ind = mapply(FUN=function(x, y, p){
		d = (x - 0)^2 + (y-1)^2
		ind = which(d == min(d))
		c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
			cutoff = p[[ind]])
	}, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(roc.perf, pred))


# only speed, angle, stand, fielding team and park
set.seed(42)
rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(rf.5)

plot(rf.5)

varImpPlot(rf.5)

predict_fit.rf.5 <- data.frame(fits = predict(rf.5, test, type = "prob")[,2], actuals = test$hit)

pred.rf.5 <- prediction(predict_fit.rf.5$fits, predict_fit.rf.5$actuals)
roc.pred.rf.5 <- performance(pred.rf.5, measure = "tpr", x.measure = "fpr")
plot(roc.pred.rf.5)
abline(a = 0, b = 1)
opt <- opt.cut(roc.pred.rf.5, pred.rf.5)
opt
opt <- opt[3]
predict_fit.rf.5$fits <- with(predict_fit.rf.5, ifelse(fits > opt, 1, 0)) 

rf.5_confusion_test <- confusionMatrix(model = rf.5, x = test, y = test$hit)
rf.5_confusion_validate <- confusionMatrix(model = rf.5, x = validate, y = validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?

tune.rf.2 <- tuneRF((train[,c(2:3, 7:9)], train[,6]), stepFactor = .5, plot = TRUE)

# mtry = 2 is the optimal number

# the optimal model appears to be rf.5

# plot the performance of the model based on the three features included

# examine whether the model is missing systematically for certain records

test_rows <- test$row
validate_rows <- validate$row
pred_data_emp_1 <- ungroup(statcast.2016) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% test_rows)

pred_data_emp <- ungroup(statcast.2016) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% validate_rows) %>%
	rbind(pred_data_emp_1)

out_of_training_rows <- pred_data_emp$row

rf.5_full_out_of_sample_data <- ungroup(statcast.2016) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))

rf.5_full_out_of_sample_data_scaled <- rf.5_full_out_of_sample_data

rf.5_full_out_of_sample_data_scaled$hit_speed <- (rf.5_full_out_of_sample_data_scaled$hit_speed - center_values[2]) / scale_values[2]

rf.5_full_out_of_sample_data_scaled$hit_angle <- (rf.5_full_out_of_sample_data_scaled$hit_angle - center_values[3]) / scale_values[3]


rf.5.prob <- predict(rf.5, rf.5_full_out_of_sample_data_scaled, type = "response")

rf.5_full_out_of_sample_data <- cbind(filter(rf.5_full_out_of_sample_data, row %in% out_of_training_rows), rf.5.prob)


names(rf.5_full_out_of_sample_data)[65] <- "fits"

rf.5_full_out_of_sample_data_reduced <- rf.5_full_out_of_sample_data %>%
	
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

rf.5_mean_table <- rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

rf.5_full_out_of_sample_data$type <- with(rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

rf.5_full_out_of_sample_data$hit_label <- with(rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

rf.5_plot1 <- ggplot(rf.5_full_out_of_sample_data, aes(hit_distance_sc, hit_angle)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

rf.5_plot1

rf.5_plot2 <- ggplot(rf.5_full_out_of_sample_data, aes(hit_distance_sc, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

rf.5_plot2

