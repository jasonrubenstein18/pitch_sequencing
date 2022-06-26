SI <- subset(statcast.2016, second_pitch == "SI")

SI$hit_distance_sc <- as.numeric(SI$hit_distance_sc, na.rm = TRUE)
SI$hit_angle <- as.numeric(SI$hit_angle, na.rm = TRUE)
SI$hit_speed <- as.numeric(SI$hit_speed, na.rm = TRUE)

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
str(SI)
##as.numeric(SI$hit_distance_sc, SI$hit_speed, SI$hit_angle)

#SI_train$hit_distance_sc <- as.numeric(SI_train$hit_distance_sc)
#SI_train$hit_speed <- as.numeric(SI_train$hit_speed)
#SI_train$hit_angle <- as.numeric(SI_train$hit_angle)
#View(SI_train)
SI_scaled_data <- scale(SI_train[,c(1:5)])
SI_scale_values <- attr(SI_scaled_data, 'scaled:scale')
SI_scale_values
SI_center_values <- attr(SI_scaled_data, 'scaled:center')
SI_center_values
SI_train <- cbind(SI_scaled_data, select(SI_train, hit:row))

View(statcast.2016)
# save levels for factor variables
SI_levels_home_team <- levels(SI_train$home_team)
SI_levels_stand <- levels(SI_train$stand)
SI_levels_fieldingTeam <- levels(SI_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(SI_test)

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

str(SI_test)

#SI_test$fieldingTeam <- as.character(SI_test$fieldingTeam)
#SI_test$home_team <- as.character(SI_test$home_team)
#SI_test$hit <- as.logical(SI_test$hit)
#SI_test$stand <- as.logical(SI_test$stand)
str(SI_test)
SI_rf.5_confusion_test <- confusionMatrix(model = SI_rf.5, x = SI_test, y = SI_test$hit)
SI_rf.5_confusion_validate <- confusionMatrix(model = SI_rf.5, x = SI_validate, y = SI_validate$hit)

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
	filter(row %in% SI_test_rows)

SI_pred_data_emp <- ungroup(SI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% SI_validate_rows) %>%
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

SI_pred_prob_hit <- predict(SI_rf.5, SI_pred_data_emp_scaled, type = "prob")[,2]
SI_pred_prob_hit_fits <- cbind(SI_pred_data_emp, SI_pred_prob_hit)

SI_pred_prob_hit_fits[,c(1:2)] <- SI_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

SI_angle_speed_pred <- SI_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = SI_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nSI Hit Probability\n") + theme_bp_grey()

SI_angle_speed_pred



######
ffSI$hit_distance_sc <- as.numeric(ffSI$hit_distance_sc, na.rm = TRUE)
ffSI$hit_angle <- as.numeric(ffSI$hit_angle, na.rm = TRUE)
ffSI$hit_speed <- as.numeric(ffSI$hit_speed, na.rm = TRUE)

ffSI$hit <- with(ffSI, ifelse(grepl("Single", ffSI$events), 1,
															ifelse(grepl("Double", ffSI$events), 1,
																		 ifelse(grepl("Triple", ffSI$events), 1, 
																		 			 ifelse(grepl("Home Run", ffSI$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

ffSI$fieldingTeam <- with(ffSI, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[ffSI$hit_distance_sc == "null"] = NA
#ffSI$hit_speed[ffSI$hit_speed == "null"] = NA
#ffSI$hit_angle[ffSI$hit_angle == "null"] = NA

# include row names for unique record identification

ffSI$row <- row.names(ffSI) %>% as.numeric()

# recode stand and home_team as factors

ffSI$stand <- as.factor(ffSI$stand)
ffSI$home_team <- as.factor(ffSI$home_team)


ffSI$game_date <- as.Date(ffSI$game_date)


# subset 

ffSI_working_data <- ungroup(ffSI) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(ffSI_working_data)
str(ungroup(ffSI))
table(ffSI_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

ffSI_working_data <- filter(ffSI_working_data, hc_x != 1, hc_y != 1)
head(ffSI_working_data)

# create training and test sets
# scaled data

set.seed(42)
ffSI_train <- sample_frac(ffSI_working_data, .15, replace = FALSE)
ffSI_split <- setdiff(ffSI_working_data, ffSI_train)
ffSI_test <- sample_frac(ffSI_split, .50, replace = FALSE)
ffSI_validate <- setdiff(ffSI_split, ffSI_test)

nrow(ffSI_train) + nrow(ffSI_test) + nrow(ffSI_validate) == nrow(ffSI_working_data)

with(ffSI_train, table(hit)) %>% prop.table()
with(ffSI_test, table(hit)) %>% prop.table()
with(ffSI_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
str(ffSI)
##as.numeric(ffSI$hit_distance_sc, ffSI$hit_speed, ffSI$hit_angle)

#ffSI_train$hit_distance_sc <- as.numeric(ffSI_train$hit_distance_sc)
#ffSI_train$hit_speed <- as.numeric(ffSI_train$hit_speed)
#ffSI_train$hit_angle <- as.numeric(ffSI_train$hit_angle)
#View(ffSI_train)
ffSI_scaled_data <- scale(ffSI_train[,c(1:5)])
ffSI_scale_values <- attr(ffSI_scaled_data, 'scaled:scale')
ffSI_scale_values
ffSI_center_values <- attr(ffSI_scaled_data, 'scaled:center')
ffSI_center_values
ffSI_train <- cbind(ffSI_scaled_data, select(ffSI_train, hit:row))

# save levels for factor variables
ffSI_levels_home_team <- levels(ffSI_train$home_team)
ffSI_levels_stand <- levels(ffSI_train$stand)
ffSI_levels_fieldingTeam <- levels(ffSI_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(ffSI_test)

ffSI_test$hit_distance_sc <- (ffSI_test$hit_distance_sc - ffSI_center_values[1]) / ffSI_scale_values[1]

ffSI_test$hit_speed <- (ffSI_test$hit_speed - ffSI_center_values[2]) / ffSI_scale_values[2]

ffSI_test$hit_angle <- (ffSI_test$hit_angle - ffSI_center_values[3]) / ffSI_scale_values[3]

ffSI_test$hc_x <- (ffSI_test$hc_x - ffSI_center_values[4]) / ffSI_scale_values[4]

ffSI_test$hc_y <- (ffSI_test$hc_y - ffSI_center_values[5]) / ffSI_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

ffSI_validate$hit_distance_sc <- (ffSI_validate$hit_distance_sc - ffSI_center_values[1]) / ffSI_scale_values[1]

ffSI_validate$hit_speed <- (ffSI_validate$hit_speed - ffSI_center_values[2]) / ffSI_scale_values[2]

ffSI_validate$hit_angle <- (ffSI_validate$hit_angle - ffSI_center_values[3]) / ffSI_scale_values[3]

ffSI_validate$hc_x <- (ffSI_validate$hc_x - ffSI_center_values[4]) / ffSI_scale_values[4]

ffSI_validate$hc_y <- (ffSI_validate$hc_y - ffSI_center_values[5]) / ffSI_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(ffSI_train)

set.seed(42)
ffSI_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(ffSI_train, -row), ntree = 501, importance = TRUE)

print(ffSI_rf.1)

plot(ffSI_rf.1)

varImpPlot(ffSI_rf.1)

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
ffSI_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(ffSI_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(ffSI_rf.5)

plot(ffSI_rf.5)

varImpPlot(ffSI_rf.5)

ffSI_predict_fit.rf.5 <- data.frame(fits = predict(ffSI_rf.5, ffSI_test, type = "prob")[,2], actuals = ffSI_test$hit)

ffSI_pred.rf.5 <- prediction(ffSI_predict_fit.rf.5$fits, ffSI_predict_fit.rf.5$actuals)

ffSI_roc.pred.rf.5 <- performance(ffSI_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(ffSI_roc.pred.rf.5)
abline(a = 0, b = 1)
ffSI_opt <- opt.cut(ffSI_roc.pred.rf.5, ffSI_pred.rf.5)
ffSI_opt
ffSI_opt <- ffSI_opt[3]
ffSI_predict_fit.rf.5$fits <- with(ffSI_predict_fit.rf.5, ifelse(fits > ffSI_opt, 1, 0)) 

str(ffSI_test)

#ffSI_test$fieldingTeam <- as.character(ffSI_test$fieldingTeam)
#ffSI_test$home_team <- as.character(ffSI_test$home_team)
#ffSI_test$hit <- as.logical(ffSI_test$hit)
#ffSI_test$stand <- as.logical(ffSI_test$stand)
str(ffSI_test)
ffSI_rf.5_confusion_test <- confusionMatrix(model = ffSI_rf.5, x = ffSI_test, y = ffSI_test$hit)
ffSI_rf.5_confusion_validate <- confusionMatrix(model = ffSI_rf.5, x = ffSI_validate, y = ffSI_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


ffSI_test_rows <- ffSI_test$row
ffSI_validate_rows <- ffSI_validate$row
ffSI_pred_data_emp_1 <- ungroup(ffSI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffSI_test_rows)

ffSI_pred_data_emp <- ungroup(ffSI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffSI_validate_rows) %>%
	rbind(ffSI_pred_data_emp_1)

ffSI_out_of_training_rows <- ffSI_pred_data_emp$row

ffSI_rf.5_full_out_of_sample_data <- ungroup(ffSI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffSI_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



ffSI_rf.5_full_out_of_sample_data_scaled <- ffSI_rf.5_full_out_of_sample_data

ffSI_rf.5_full_out_of_sample_data_scaled$hit_speed <- (ffSI_rf.5_full_out_of_sample_data_scaled$hit_speed - ffSI_center_values[2]) / ffSI_scale_values[2]

ffSI_rf.5_full_out_of_sample_data_scaled$hit_angle <- (ffSI_rf.5_full_out_of_sample_data_scaled$hit_angle - ffSI_center_values[3]) / ffSI_scale_values[3]

ffSI_rf.5.prob <- predict(ffSI_rf.5, ffSI_rf.5_full_out_of_sample_data_scaled, type = "response")

ffSI_rf.5_full_out_of_sample_data <- cbind(filter(ffSI_rf.5_full_out_of_sample_data, row %in% ffSI_out_of_training_rows), ffSI_rf.5.prob)

names(ffSI_rf.5_full_out_of_sample_data)[65] <- "fits"
names(ffSI_rf.5_full_out_of_sample_data)

ffSI_rf.5_full_out_of_sample_data_reduced <- ffSI_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

ffSI_rf.5_mean_table <- ffSI_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

ffSI_rf.5_full_out_of_sample_data$type <- with(ffSI_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

ffSI_rf.5_full_out_of_sample_data$hit_label <- with(ffSI_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

ffSI_rf.5_plot1 <- ggplot(ffSI_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

ffSI_rf.5_plot1

ffSI_rf.5_plot2 <- ggplot(ffSI_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

ffSI_rf.5_plot2

ffSI_rf.5_plot3 <- ggplot(ffSI_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

ffSI_rf.5_plot3

ffSI_grid_plot_rf.5 <- grid.arrange(ffSI_rf.5_plot1, ffSI_rf.5_plot2, ffSI_rf.5_plot3, ncol = 3)
ffSI_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

ffSI_test_rows <- ffSI_test$row
ffSI_validate_rows <- ffSI_validate$row
ffSI_pred_data_emp_1 <- ungroup(ffSI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffSI_test_rows)

ffSI_pred_data_emp <- ungroup(ffSI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffSI_validate_rows) %>%
	rbind(ffSI_pred_data_emp_1)

ffSI_out_of_training_rows <- ffSI_pred_data_emp$row

ffSI_pred_data_emp <- ungroup(ffSI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffSI_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

ffSI_pred_data_emp_scaled <- ffSI_pred_data_emp

ffSI_pred_data_emp_scaled$hit_speed <- (ffSI_pred_data_emp_scaled$hit_speed - ffSI_center_values[2]) / ffSI_scale_values[2]

ffSI_pred_data_emp_scaled$hit_angle <- (ffSI_pred_data_emp_scaled$hit_angle - ffSI_center_values[3]) / ffSI_scale_values[3]

ffSI_pred_prob_hit <- predict(ffSI_rf.5, ffSI_pred_data_emp_scaled, type = "prob")[,2]
ffSI_pred_prob_hit_fits <- cbind(ffSI_pred_data_emp, ffSI_pred_prob_hit)

ffSI_pred_prob_hit_fits[,c(1:2)] <- ffSI_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

ffSI_angle_speed_pred <- ffSI_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = ffSI_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nFF-SI Hit Probability\n") + theme_bp_grey()

ffSI_angle_speed_pred
