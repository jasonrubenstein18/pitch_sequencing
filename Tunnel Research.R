statcast.2016 <- read.csv("~/Desktop/pitcher_statcast.csv", stringsAsFactors = FALSE)
#whiffs.data <- data.frame(statcast.2016)
playerid_list <- read_csv("~/Downloads/playerid_list (1).csv")
bpTunnelData <- read_csv("~/Downloads/bpstats_03-14-2017 (1).csv")

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
#totalMergeInPlay <- plyr::rename(totalMergeInPlay, c(totalMergeInPlay$`Release Diff` == "average.release.variation", totalMergeInPlay$`Flight Time Diff` == "flight.time.differential", totalMergeInPlay$`Post-tunnel Break` == "post.tunnel.break", totalMergeInPlay$`Tunnel Differential` == "pre.post.tunnel.break.ratio", totalMergeInPlay$`Plate Diff` == "plate.differntial"))


#glm(totalMergeInPlay$hit ~ totalMergeInPlay$`Release Diff`, family = binomial, data = totalMergeInPlay)
#glm(totalMergeInPlay$hit ~ totalMergeInPlay$`Release Diff` + totalMergeInPlay$`Flight Time Diff`, family = binomial, data = totalMergeInPlay)
#glm(totalMergeInPlay$hit ~ totalMergeInPlay$`Release Diff` + totalMergeInPlay$`Flight Time Diff` + totalMergeInPlay$`Post-tunnel Break`, family = binomial, data = totalMergeInPlay)
#glm(totalMergeInPlay$hit ~ totalMergeInPlay$`Release Diff` + totalMergeInPlay$`Flight Time Diff` + totalMergeInPlay$`Post-tunnel Break` + totalMergeInPlay$`Release:Tunnel` + totalMergeInPlay$`Plate Diff`, family = binomial, data = totalMergeInPlay)


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
totalMergeInPlay$release.tunnel.diff.ratio <- totalMergeInPlay$`Release:Tunnel`
totalMergeInPlay$post.tunnel.break <- totalMergeInPlay$`Post-tunnel Break`
totalMergeInPlay$flight.time.diff <- totalMergeInPlay$`Flight Time Diff`
totalMergeInPlay$pre.post.tunnel.break.ratio <- totalMergeInPlay$`Break:Tunnel`
totalMergeInPlay$tunnel.differential <- totalMergeInPlay$`Tunnel Differential`
totalMergeInPlay$plate.diff <- totalMergeInPlay$`Plate Diff`
	
head(totalMergeInPlay)
totalMergeInPlay_working_data <- ungroup(totalMergeInPlay) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hit_speed, hc_x, hc_y, hit, stand, 
				 fieldingTeam, release.diff, home_team, release.tunnel.diff.ratio, 
				 post.tunnel.break, flight.time.diff, pre.post.tunnel.break.ratio, 
				 tunnel.differential, plate.diff,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(release.diff)) %>%
	filter(!is.na(release.tunnel.diff.ratio)) %>%
	filter(!is.na(post.tunnel.break)) %>%
	filter(!is.na(flight.time.diff)) %>%
	filter(!is.na(pre.post.tunnel.break.ratio)) %>%
	filter(!is.na(tunnel.differential)) %>%
	filter(!is.na(plate.diff)) %>%
	arrange(desc(hit))
head(totalMergeInPlay_working_data)
str(ungroup(totalMergeInPlay))
table(totalMergeInPlay_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

totalMergeInPlay_working_data <- filter(totalMergeInPlay_working_data, hc_x != 1, hc_y != 1)
head(totalMergeInPlay_working_data)

# create training and test sets
# scaled data
#totalMergeInPlay_train <- subset(totalMergeInPlay_train, select = c(release.diff, hit_distance_sc:fieldingTeam, home_team:row))
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
totalMergeInPlay_scaled_data <- scale(totalMergeInPlay_train[,c(1:5, 9, 11:16)])
totalMergeInPlay_scale_values <- attr(totalMergeInPlay_scaled_data, 'scaled:scale')
totalMergeInPlay_scale_values
totalMergeInPlay_center_values <- attr(totalMergeInPlay_scaled_data, 'scaled:center')
totalMergeInPlay_center_values
totalMergeInPlay_train <- cbind(totalMergeInPlay_scaled_data, select(totalMergeInPlay_train, hit:row))
head(totalMergeInPlay_train)

#	Binomial regression on scaled data
Model1 <- glm(hit ~ release.diff, data = totalMergeInPlay, family = "binomial")
Model1
Model1 <- glm(hit ~ release.diff, release.tunnel.diff.ratio, data = totalMergeInPlay, family = "binomial")

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

totalMergeInPlay_test$release.tunnel.diff.ratio <- (totalMergeInPlay_test$release.tunnel.diff.ratio - totalMergeInPlay_center_values[7]) / totalMergeInPlay_scale_values[7]

totalMergeInPlay_test$post.tunnel.break <- (totalMergeInPlay_test$post.tunnel.break - totalMergeInPlay_center_values[8]) / totalMergeInPlay_scale_values[8]

totalMergeInPlay_test$flight.time.diff <- (totalMergeInPlay_test$flight.time.diff - totalMergeInPlay_center_values[9]) / totalMergeInPlay_scale_values[9]

totalMergeInPlay_test$pre.post.tunnel.break.ratio <- (totalMergeInPlay_test$pre.post.tunnel.break.ratio - totalMergeInPlay_center_values[10]) / totalMergeInPlay_scale_values[10]

totalMergeInPlay_test$tunnel.differential <- (totalMergeInPlay_test$tunnel.differential - totalMergeInPlay_center_values[11]) / totalMergeInPlay_scale_values[11]

totalMergeInPlay_test$plate.diff <- (totalMergeInPlay_test$plate.diff - totalMergeInPlay_center_values[12]) / totalMergeInPlay_scale_values[12]


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

totalMergeInPlay_validate$release.tunnel.diff.ratio <- (totalMergeInPlay_validate$release.tunnel.diff.ratio - totalMergeInPlay_center_values[7]) / totalMergeInPlay_scale_values[7]

totalMergeInPlay_validate$post.tunnel.break <- (totalMergeInPlay_validate$post.tunnel.break - totalMergeInPlay_center_values[8]) / totalMergeInPlay_scale_values[8]

totalMergeInPlay_validate$flight.time.diff <- (totalMergeInPlay_validate$flight.time.diff - totalMergeInPlay_center_values[9]) / totalMergeInPlay_scale_values[9]

totalMergeInPlay_validate$pre.post.tunnel.break.ratio <- (totalMergeInPlay_validate$pre.post.tunnel.break.ratio - totalMergeInPlay_center_values[10]) / totalMergeInPlay_scale_values[10]

totalMergeInPlay_validate$tunnel.differential <- (totalMergeInPlay_validate$tunnel.differential - totalMergeInPlay_center_values[11]) / totalMergeInPlay_scale_values[11]

totalMergeInPlay_validate$plate.diff <- (totalMergeInPlay_validate$plate.diff - totalMergeInPlay_center_values[12]) / totalMergeInPlay_scale_values[12]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]
#validate$pz <- (validate$pz - center_values[7])/scale_values[7]
#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]
# build, test, validate models



# model hit/no-hit random forest
# with all variables
totalMergeInPlay_train <- subset(totalMergeInPlay_train, select = c(1:15, 17, 24))
set.seed(42)
totalMergeInPlay_rf.1 <- randomForest(as.factor(hit) ~ ., data = totalMergeInPlay_train, ntree = 501, importance = TRUE)

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

# factor lowest MeanDecreaseAccuracy 

set.seed(42)
totalMergeInPlay_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 3, ntrees = 501, data = select(totalMergeInPlay_train, -row, -stand, -tunnel.differential, -plate.diff, -pre.post.tunnel.break.ratio, -flight.time.diff, -post.tunnel.break, hit_distance_sc))

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


#totalMergeInPlay_test$fieldingTeam <- as.character(totalMergeInPlay_test$fieldingTeam)
#totalMergeInPlay_test$home_team <- as.character(totalMergeInPlay_test$home_team)
#totalMergeInPlay_test$hit <- as.logical(totalMergeInPlay_test$hit)
#totalMergeInPlay_test$stand <- as.logical(totalMergeInPlay_test$stand)
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

