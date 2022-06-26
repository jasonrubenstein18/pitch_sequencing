#cuCH <- subset(statcast.2016, second_pitch == "CH" & first_pitch == "CU")
cuCH$hit_distance_sc <- as.numeric(cuCH$hit_distance_sc, na.rm = TRUE)
cuCH$hit_angle <- as.numeric(cuCH$hit_angle, na.rm = TRUE)
cuCH$hit_speed <- as.numeric(cuCH$hit_speed, na.rm = TRUE)

cuCH$hit <- with(cuCH, ifelse(grepl("Single", cuCH$events), 1,
															ifelse(grepl("Double", cuCH$events), 1,
																		 ifelse(grepl("Triple", cuCH$events), 1, 
																		 			 ifelse(grepl("Home Run", cuCH$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

cuCH$fieldingTeam <- with(cuCH, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[cuCH$hit_distance_sc == "null"] = NA
#cuCH$hit_speed[cuCH$hit_speed == "null"] = NA
#cuCH$hit_angle[cuCH$hit_angle == "null"] = NA

# include row names for unique record identification

cuCH$row <- row.names(cuCH) %>% as.numeric()

# recode stand and home_team as factors

cuCH$stand <- as.factor(cuCH$stand)
cuCH$home_team <- as.factor(cuCH$home_team)


cuCH$game_date <- as.Date(cuCH$game_date)


# subset 

cuCH_working_data <- ungroup(cuCH) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(cuCH_working_data)
str(ungroup(cuCH))
table(cuCH_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

cuCH_working_data <- filter(cuCH_working_data, hc_x != 1, hc_y != 1)
head(cuCH_working_data)

# create training and test sets
# scaled data

set.seed(42)
cuCH_train <- sample_frac(cuCH_working_data, .15, replace = FALSE)
cuCH_split <- setdiff(cuCH_working_data, cuCH_train)
cuCH_test <- sample_frac(cuCH_split, .50, replace = FALSE)
cuCH_validate <- setdiff(cuCH_split, cuCH_test)

nrow(cuCH_train) + nrow(cuCH_test) + nrow(cuCH_validate) == nrow(cuCH_working_data)

with(cuCH_train, table(hit)) %>% prop.table()
with(cuCH_test, table(hit)) %>% prop.table()
with(cuCH_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(cuCH_train)

##########################################################################################
str(cuCH)
##as.numeric(cuCH$hit_distance_sc, cuCH$hit_speed, cuCH$hit_angle)

#cuCH_train$hit_distance_sc <- as.numeric(cuCH_train$hit_distance_sc)
#cuCH_train$hit_speed <- as.numeric(cuCH_train$hit_speed)
#cuCH_train$hit_angle <- as.numeric(cuCH_train$hit_angle)
#View(cuCH_train)
cuCH_scaled_data <- scale(cuCH_train[,c(1:5)])
cuCH_scale_values <- attr(cuCH_scaled_data, 'scaled:scale')
cuCH_scale_values
##########################################################################################
cuCH_center_values <- attr(cuCH_scaled_data, 'scaled:center')
cuCH_center_values
##########################################################################################
cuCH_train <- cbind(cuCH_scaled_data, select(cuCH_train, hit:row))

# save levels for factor variables
cuCH_levels_home_team <- levels(cuCH_train$home_team)
cuCH_levels_stand <- levels(cuCH_train$stand)
cuCH_levels_fieldingTeam <- levels(cuCH_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(cuCH_test)

cuCH_test$hit_distance_sc <- (cuCH_test$hit_distance_sc - cuCH_center_values[1]) / cuCH_scale_values[1]

cuCH_test$hit_speed <- (cuCH_test$hit_speed - cuCH_center_values[2]) / cuCH_scale_values[2]

cuCH_test$hit_angle <- (cuCH_test$hit_angle - cuCH_center_values[3]) / cuCH_scale_values[3]

cuCH_test$hc_x <- (cuCH_test$hc_x - cuCH_center_values[4]) / cuCH_scale_values[4]

cuCH_test$hc_y <- (cuCH_test$hc_y - cuCH_center_values[5]) / cuCH_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

cuCH_validate$hit_distance_sc <- (cuCH_validate$hit_distance_sc - cuCH_center_values[1]) / cuCH_scale_values[1]

cuCH_validate$hit_speed <- (cuCH_validate$hit_speed - cuCH_center_values[2]) / cuCH_scale_values[2]

cuCH_validate$hit_angle <- (cuCH_validate$hit_angle - cuCH_center_values[3]) / cuCH_scale_values[3]

cuCH_validate$hc_x <- (cuCH_validate$hc_x - cuCH_center_values[4]) / cuCH_scale_values[4]

cuCH_validate$hc_y <- (cuCH_validate$hc_y - cuCH_center_values[5]) / cuCH_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(cuCH_train)

set.seed(42)
cuCH_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(cuCH_train, -row), ntree = 501, importance = TRUE)

print(cuCH_rf.1)

plot(cuCH_rf.1)

varImpPlot(cuCH_rf.1)

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
cuCH_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(cuCH_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(cuCH_rf.5)

plot(cuCH_rf.5)

varImpPlot(cuCH_rf.5)

cuCH_predict_fit.rf.5 <- data.frame(fits = predict(cuCH_rf.5, cuCH_test, type = "prob")[,2], actuals = cuCH_test$hit)

cuCH_pred.rf.5 <- prediction(cuCH_predict_fit.rf.5$fits, cuCH_predict_fit.rf.5$actuals)

cuCH_roc.pred.rf.5 <- performance(cuCH_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(cuCH_roc.pred.rf.5)
abline(a = 0, b = 1)
cuCH_opt <- opt.cut(cuCH_roc.pred.rf.5, cuCH_pred.rf.5)
cuCH_opt
cuCH_opt <- cuCH_opt[3]
cuCH_predict_fit.rf.5$fits <- with(cuCH_predict_fit.rf.5, ifelse(fits > cuCH_opt, 1, 0)) 

str(cuCH_test)

#cuCH_test$fieldingTeam <- as.character(cuCH_test$fieldingTeam)
#cuCH_test$home_team <- as.character(cuCH_test$home_team)
#cuCH_test$hit <- as.logical(cuCH_test$hit)
#cuCH_test$stand <- as.logical(cuCH_test$stand)
str(cuCH_test)
cuCH_rf.5_confusion_test <- confusionMatrix(model = cuCH_rf.5, x = cuCH_test, y = cuCH_test$hit)
cuCH_rf.5_confusion_validate <- confusionMatrix(model = cuCH_rf.5, x = cuCH_validate, y = cuCH_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


cuCH_test_rows <- cuCH_test$row
cuCH_validate_rows <- cuCH_validate$row
cuCH_pred_data_emp_1 <- ungroup(cuCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuCH_test_rows)

cuCH_pred_data_emp <- ungroup(cuCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuCH_validate_rows) %>%
	rbind(cuCH_pred_data_emp_1)

cuCH_out_of_training_rows <- cuCH_pred_data_emp$row

cuCH_rf.5_full_out_of_sample_data <- ungroup(cuCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuCH_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



cuCH_rf.5_full_out_of_sample_data_scaled <- cuCH_rf.5_full_out_of_sample_data

cuCH_rf.5_full_out_of_sample_data_scaled$hit_speed <- (cuCH_rf.5_full_out_of_sample_data_scaled$hit_speed - cuCH_center_values[2]) / cuCH_scale_values[2]

cuCH_rf.5_full_out_of_sample_data_scaled$hit_angle <- (cuCH_rf.5_full_out_of_sample_data_scaled$hit_angle - cuCH_center_values[3]) / cuCH_scale_values[3]

cuCH_rf.5.prob <- predict(cuCH_rf.5, cuCH_rf.5_full_out_of_sample_data_scaled, type = "response")

cuCH_rf.5_full_out_of_sample_data <- cbind(filter(cuCH_rf.5_full_out_of_sample_data, row %in% cuCH_out_of_training_rows), cuCH_rf.5.prob)

names(cuCH_rf.5_full_out_of_sample_data)[65] <- "fits"
names(cuCH_rf.5_full_out_of_sample_data)

cuCH_rf.5_full_out_of_sample_data_reduced <- cuCH_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

cuCH_rf.5_mean_table <- cuCH_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

cuCH_rf.5_full_out_of_sample_data$type <- with(cuCH_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

cuCH_rf.5_full_out_of_sample_data$hit_label <- with(cuCH_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

cuCH_rf.5_plot1 <- ggplot(cuCH_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

cuCH_rf.5_plot1

cuCH_rf.5_plot2 <- ggplot(cuCH_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

cuCH_rf.5_plot2

cuCH_rf.5_plot3 <- ggplot(cuCH_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

cuCH_rf.5_plot3

cuCH_grid_plot_rf.5 <- grid.arrange(cuCH_rf.5_plot1, cuCH_rf.5_plot2, cuCH_rf.5_plot3, ncol = 3)
cuCH_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

cuCH_test_rows <- cuCH_test$row
cuCH_validate_rows <- cuCH_validate$row
cuCH_pred_data_emp_1 <- ungroup(cuCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuCH_test_rows)

cuCH_pred_data_emp <- ungroup(cuCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuCH_validate_rows) %>%
	rbind(cuCH_pred_data_emp_1)

cuCH_out_of_training_rows <- cuCH_pred_data_emp$row

cuCH_pred_data_emp <- ungroup(cuCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuCH_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

cuCH_pred_data_emp_scaled <- cuCH_pred_data_emp

cuCH_pred_data_emp_scaled$hit_speed <- (cuCH_pred_data_emp_scaled$hit_speed - cuCH_center_values[2]) / cuCH_scale_values[2]

cuCH_pred_data_emp_scaled$hit_angle <- (cuCH_pred_data_emp_scaled$hit_angle - cuCH_center_values[3]) / cuCH_scale_values[3]

cuCH_pred_prob_hit <- predict(cuCH_rf.5, cuCH_pred_data_emp_scaled, type = "prob")[,2]
cuCH_pred_prob_hit_fits <- cbind(cuCH_pred_data_emp, cuCH_pred_prob_hit)

cuCH_pred_prob_hit_fits[,c(1:2)] <- cuCH_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

cuCH_angle_speed_pred <- cuCH_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = cuCH_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nCU-CH Hit Probability\n") + theme_bp_grey()

cuCH_angle_speed_pred

#cuSL <- subset(statcast.2016, second_pitch == "SL" & first_pitch == "CU")
cuSL$hit_distance_sc <- as.numeric(cuSL$hit_distance_sc, na.rm = TRUE)
cuSL$hit_angle <- as.numeric(cuSL$hit_angle, na.rm = TRUE)
cuSL$hit_speed <- as.numeric(cuSL$hit_speed, na.rm = TRUE)

cuSL$hit <- with(cuSL, ifelse(grepl("Single", cuSL$events), 1,
															ifelse(grepl("Double", cuSL$events), 1,
																		 ifelse(grepl("Triple", cuSL$events), 1, 
																		 			 ifelse(grepl("Home Run", cuSL$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

cuSL$fieldingTeam <- with(cuSL, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[cuSL$hit_distance_sc == "null"] = NA
#cuSL$hit_speed[cuSL$hit_speed == "null"] = NA
#cuSL$hit_angle[cuSL$hit_angle == "null"] = NA

# include row names for unique record identification

cuSL$row <- row.names(cuSL) %>% as.numeric()

# recode stand and home_team as factors

cuSL$stand <- as.factor(cuSL$stand)
cuSL$home_team <- as.factor(cuSL$home_team)


cuSL$game_date <- as.Date(cuSL$game_date)


# subset 

cuSL_working_data <- ungroup(cuSL) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(cuSL_working_data)
str(ungroup(cuSL))
table(cuSL_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

cuSL_working_data <- filter(cuSL_working_data, hc_x != 1, hc_y != 1)
head(cuSL_working_data)

# create training and test sets
# scaled data

set.seed(42)
cuSL_train <- sample_frac(cuSL_working_data, .15, replace = FALSE)
cuSL_split <- setdiff(cuSL_working_data, cuSL_train)
cuSL_test <- sample_frac(cuSL_split, .50, replace = FALSE)
cuSL_validate <- setdiff(cuSL_split, cuSL_test)

nrow(cuSL_train) + nrow(cuSL_test) + nrow(cuSL_validate) == nrow(cuSL_working_data)

with(cuSL_train, table(hit)) %>% prop.table()
with(cuSL_test, table(hit)) %>% prop.table()
with(cuSL_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(cuSL_train)

##########################################################################################
str(cuSL)
##as.numeric(cuSL$hit_distance_sc, cuSL$hit_speed, cuSL$hit_angle)

#cuSL_train$hit_distance_sc <- as.numeric(cuSL_train$hit_distance_sc)
#cuSL_train$hit_speed <- as.numeric(cuSL_train$hit_speed)
#cuSL_train$hit_angle <- as.numeric(cuSL_train$hit_angle)
#View(cuSL_train)
cuSL_scaled_data <- scale(cuSL_train[,c(1:5)])
cuSL_scale_values <- attr(cuSL_scaled_data, 'scaled:scale')
cuSL_scale_values
##########################################################################################
cuSL_center_values <- attr(cuSL_scaled_data, 'scaled:center')
cuSL_center_values
##########################################################################################
cuSL_train <- cbind(cuSL_scaled_data, select(cuSL_train, hit:row))

# save levels for factor variables
cuSL_levels_home_team <- levels(cuSL_train$home_team)
cuSL_levels_stand <- levels(cuSL_train$stand)
cuSL_levels_fieldingTeam <- levels(cuSL_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(cuSL_test)

cuSL_test$hit_distance_sc <- (cuSL_test$hit_distance_sc - cuSL_center_values[1]) / cuSL_scale_values[1]

cuSL_test$hit_speed <- (cuSL_test$hit_speed - cuSL_center_values[2]) / cuSL_scale_values[2]

cuSL_test$hit_angle <- (cuSL_test$hit_angle - cuSL_center_values[3]) / cuSL_scale_values[3]

cuSL_test$hc_x <- (cuSL_test$hc_x - cuSL_center_values[4]) / cuSL_scale_values[4]

cuSL_test$hc_y <- (cuSL_test$hc_y - cuSL_center_values[5]) / cuSL_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

cuSL_validate$hit_distance_sc <- (cuSL_validate$hit_distance_sc - cuSL_center_values[1]) / cuSL_scale_values[1]

cuSL_validate$hit_speed <- (cuSL_validate$hit_speed - cuSL_center_values[2]) / cuSL_scale_values[2]

cuSL_validate$hit_angle <- (cuSL_validate$hit_angle - cuSL_center_values[3]) / cuSL_scale_values[3]

cuSL_validate$hc_x <- (cuSL_validate$hc_x - cuSL_center_values[4]) / cuSL_scale_values[4]

cuSL_validate$hc_y <- (cuSL_validate$hc_y - cuSL_center_values[5]) / cuSL_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(cuSL_train)



set.seed(42)
cuSL_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(cuSL_train, -row), ntree = 501, importance = TRUE)

print(cuSL_rf.1)

plot(cuSL_rf.1)

varImpPlot(cuSL_rf.1)

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
cuSL_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(cuSL_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(cuSL_rf.5)

plot(cuSL_rf.5)

varImpPlot(cuSL_rf.5)

cuSL_predict_fit.rf.5 <- data.frame(fits = predict(cuSL_rf.5, cuSL_test, type = "prob")[,2], actuals = cuSL_test$hit)

cuSL_pred.rf.5 <- prediction(cuSL_predict_fit.rf.5$fits, cuSL_predict_fit.rf.5$actuals)

cuSL_roc.pred.rf.5 <- performance(cuSL_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(cuSL_roc.pred.rf.5)
abline(a = 0, b = 1)
cuSL_opt <- opt.cut(cuSL_roc.pred.rf.5, cuSL_pred.rf.5)
cuSL_opt
cuSL_opt <- cuSL_opt[3]
cuSL_predict_fit.rf.5$fits <- with(cuSL_predict_fit.rf.5, ifelse(fits > cuSL_opt, 1, 0)) 

str(cuSL_test)

#cuSL_test$fieldingTeam <- as.character(cuSL_test$fieldingTeam)
#cuSL_test$home_team <- as.character(cuSL_test$home_team)
#cuSL_test$hit <- as.logical(cuSL_test$hit)
#cuSL_test$stand <- as.logical(cuSL_test$stand)
str(cuSL_test)
cuSL_rf.5_confusion_test <- confusionMatrix(model = cuSL_rf.5, x = cuSL_test, y = cuSL_test$hit)
cuSL_rf.5_confusion_validate <- confusionMatrix(model = cuSL_rf.5, x = cuSL_validate, y = cuSL_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


cuSL_test_rows <- cuSL_test$row
cuSL_validate_rows <- cuSL_validate$row
cuSL_pred_data_emp_1 <- ungroup(cuSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuSL_test_rows)

cuSL_pred_data_emp <- ungroup(cuSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuSL_validate_rows) %>%
	rbind(cuSL_pred_data_emp_1)

cuSL_out_of_training_rows <- cuSL_pred_data_emp$row

cuSL_rf.5_full_out_of_sample_data <- ungroup(cuSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuSL_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



cuSL_rf.5_full_out_of_sample_data_scaled <- cuSL_rf.5_full_out_of_sample_data

cuSL_rf.5_full_out_of_sample_data_scaled$hit_speed <- (cuSL_rf.5_full_out_of_sample_data_scaled$hit_speed - cuSL_center_values[2]) / cuSL_scale_values[2]

cuSL_rf.5_full_out_of_sample_data_scaled$hit_angle <- (cuSL_rf.5_full_out_of_sample_data_scaled$hit_angle - cuSL_center_values[3]) / cuSL_scale_values[3]

cuSL_rf.5.prob <- predict(cuSL_rf.5, cuSL_rf.5_full_out_of_sample_data_scaled, type = "response")

cuSL_rf.5_full_out_of_sample_data <- cbind(filter(cuSL_rf.5_full_out_of_sample_data, row %in% cuSL_out_of_training_rows), cuSL_rf.5.prob)

names(cuSL_rf.5_full_out_of_sample_data)[65] <- "fits"
names(cuSL_rf.5_full_out_of_sample_data)

cuSL_rf.5_full_out_of_sample_data_reduced <- cuSL_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

cuSL_rf.5_mean_table <- cuSL_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

cuSL_rf.5_full_out_of_sample_data$type <- with(cuSL_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

cuSL_rf.5_full_out_of_sample_data$hit_label <- with(cuSL_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

cuSL_rf.5_plot1 <- ggplot(cuSL_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

cuSL_rf.5_plot1

cuSL_rf.5_plot2 <- ggplot(cuSL_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

cuSL_rf.5_plot2

cuSL_rf.5_plot3 <- ggplot(cuSL_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

cuSL_rf.5_plot3

cuSL_grid_plot_rf.5 <- grid.arrange(cuSL_rf.5_plot1, cuSL_rf.5_plot2, cuSL_rf.5_plot3, ncol = 3)
cuSL_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

cuSL_test_rows <- cuSL_test$row
cuSL_validate_rows <- cuSL_validate$row
cuSL_pred_data_emp_1 <- ungroup(cuSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuSL_test_rows)

cuSL_pred_data_emp <- ungroup(cuSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuSL_validate_rows) %>%
	rbind(cuSL_pred_data_emp_1)

cuSL_out_of_training_rows <- cuSL_pred_data_emp$row

cuSL_pred_data_emp <- ungroup(cuSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuSL_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

cuSL_pred_data_emp_scaled <- cuSL_pred_data_emp

cuSL_pred_data_emp_scaled$hit_speed <- (cuSL_pred_data_emp_scaled$hit_speed - cuSL_center_values[2]) / cuSL_scale_values[2]

cuSL_pred_data_emp_scaled$hit_angle <- (cuSL_pred_data_emp_scaled$hit_angle - cuSL_center_values[3]) / cuSL_scale_values[3]

cuSL_pred_prob_hit <- predict(cuSL_rf.5, cuSL_pred_data_emp_scaled, type = "prob")[,2]
cuSL_pred_prob_hit_fits <- cbind(cuSL_pred_data_emp, cuSL_pred_prob_hit)

cuSL_pred_prob_hit_fits[,c(1:2)] <- cuSL_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

cuSL_angle_speed_pred <- cuSL_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = cuSL_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nSL-CH Hit Probability\n") + theme_bp_grey()

cuSL_angle_speed_pred

############# cuCU <- subset(statcast.2016, second_pitch == "CU" & first_pitch == "CU")
cuCU$hit_distance_sc <- as.numeric(cuCU$hit_distance_sc, na.rm = TRUE)
cuCU$hit_angle <- as.numeric(cuCU$hit_angle, na.rm = TRUE)
cuCU$hit_speed <- as.numeric(cuCU$hit_speed, na.rm = TRUE)

cuCU$hit <- with(cuCU, ifelse(grepl("Single", cuCU$events), 1,
															ifelse(grepl("Double", cuCU$events), 1,
																		 ifelse(grepl("Triple", cuCU$events), 1, 
																		 			 ifelse(grepl("Home Run", cuCU$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

cuCU$fieldingTeam <- with(cuCU, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[cuCU$hit_distance_sc == "null"] = NA
#cuCU$hit_speed[cuCU$hit_speed == "null"] = NA
#cuCU$hit_angle[cuCU$hit_angle == "null"] = NA

# include row names for unique record identification

cuCU$row <- row.names(cuCU) %>% as.numeric()

# recode stand and home_team as factors

cuCU$stand <- as.factor(cuCU$stand)
cuCU$home_team <- as.factor(cuCU$home_team)


cuCU$game_date <- as.Date(cuCU$game_date)


# subset 

cuCU_working_data <- ungroup(cuCU) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(cuCU_working_data)
str(ungroup(cuCU))
table(cuCU_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

cuCU_working_data <- filter(cuCU_working_data, hc_x != 1, hc_y != 1)
head(cuCU_working_data)

# create training and test sets
# scaled data

set.seed(42)
cuCU_train <- sample_frac(cuCU_working_data, .15, replace = FALSE)
cuCU_split <- setdiff(cuCU_working_data, cuCU_train)
cuCU_test <- sample_frac(cuCU_split, .50, replace = FALSE)
cuCU_validate <- setdiff(cuCU_split, cuCU_test)

nrow(cuCU_train) + nrow(cuCU_test) + nrow(cuCU_validate) == nrow(cuCU_working_data)

with(cuCU_train, table(hit)) %>% prop.table()
with(cuCU_test, table(hit)) %>% prop.table()
with(cuCU_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(cuCU_train)

str(cuCU)
##as.numeric(cuCU$hit_distance_sc, cuCU$hit_speed, cuCU$hit_angle)

#cuCU_train$hit_distance_sc <- as.numeric(cuCU_train$hit_distance_sc)
#cuCU_train$hit_speed <- as.numeric(cuCU_train$hit_speed)
#cuCU_train$hit_angle <- as.numeric(cuCU_train$hit_angle)
#View(cuCU_train)
cuCU_scaled_data <- scale(cuCU_train[,c(1:5)])
cuCU_scale_values <- attr(cuCU_scaled_data, 'scaled:scale')
cuCU_scale_values
cuCU_center_values <- attr(cuCU_scaled_data, 'scaled:center')
cuCU_center_values
cuCU_train <- cbind(cuCU_scaled_data, select(cuCU_train, hit:row))

# save levels for factor variables
cuCU_levels_home_team <- levels(cuCU_train$home_team)
cuCU_levels_stand <- levels(cuCU_train$stand)
cuCU_levels_fieldingTeam <- levels(cuCU_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data

cuCU_test$hit_distance_sc <- (cuCU_test$hit_distance_sc - cuCU_center_values[1]) / cuCU_scale_values[1]

cuCU_test$hit_speed <- (cuCU_test$hit_speed - cuCU_center_values[2]) / cuCU_scale_values[2]

cuCU_test$hit_angle <- (cuCU_test$hit_angle - cuCU_center_values[3]) / cuCU_scale_values[3]

cuCU_test$hc_x <- (cuCU_test$hc_x - cuCU_center_values[4]) / cuCU_scale_values[4]

cuCU_test$hc_y <- (cuCU_test$hc_y - cuCU_center_values[5]) / cuCU_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

cuCU_validate$hit_distance_sc <- (cuCU_validate$hit_distance_sc - cuCU_center_values[1]) / cuCU_scale_values[1]

cuCU_validate$hit_speed <- (cuCU_validate$hit_speed - cuCU_center_values[2]) / cuCU_scale_values[2]

cuCU_validate$hit_angle <- (cuCU_validate$hit_angle - cuCU_center_values[3]) / cuCU_scale_values[3]

cuCU_validate$hc_x <- (cuCU_validate$hc_x - cuCU_center_values[4]) / cuCU_scale_values[4]

cuCU_validate$hc_y <- (cuCU_validate$hc_y - cuCU_center_values[5]) / cuCU_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(cuCU_train)

set.seed(42)
cuCU_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(cuCU_train, -row), ntree = 501, importance = TRUE)

print(cuCU_rf.1)

plot(cuCU_rf.1)

varImpPlot(cuCU_rf.1)

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
cuCU_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(cuCU_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(cuCU_rf.5)

plot(cuCU_rf.5)

varImpPlot(cuCU_rf.5)

cuCU_predict_fit.rf.5 <- data.frame(fits = predict(cuCU_rf.5, cuCU_test, type = "prob")[,2], actuals = cuCU_test$hit)

cuCU_pred.rf.5 <- prediction(cuCU_predict_fit.rf.5$fits, cuCU_predict_fit.rf.5$actuals)

cuCU_roc.pred.rf.5 <- performance(cuCU_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(cuCU_roc.pred.rf.5)
abline(a = 0, b = 1)
cuCU_opt <- opt.cut(cuCU_roc.pred.rf.5, cuCU_pred.rf.5)
cuCU_opt
cuCU_opt <- cuCU_opt[3]
cuCU_predict_fit.rf.5$fits <- with(cuCU_predict_fit.rf.5, ifelse(fits > cuCU_opt, 1, 0)) 

str(cuCU_test)

#cuCU_test$fieldingTeam <- as.character(cuCU_test$fieldingTeam)
#cuCU_test$home_team <- as.character(cuCU_test$home_team)
#cuCU_test$hit <- as.logical(cuCU_test$hit)
#cuCU_test$stand <- as.logical(cuCU_test$stand)
str(cuCU_test)
cuCU_rf.5_confusion_test <- confusionMatrix(model = cuCU_rf.5, x = cuCU_test, y = cuCU_test$hit)
cuCU_rf.5_confusion_validate <- confusionMatrix(model = cuCU_rf.5, x = cuCU_validate, y = cuCU_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


cuCU_test_rows <- cuCU_test$row
cuCU_validate_rows <- cuCU_validate$row
cuCU_pred_data_emp_1 <- ungroup(cuCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuCU_test_rows)

cuCU_pred_data_emp <- ungroup(cuCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuCU_validate_rows) %>%
	rbind(cuCU_pred_data_emp_1)

cuCU_out_of_training_rows <- cuCU_pred_data_emp$row

cuCU_rf.5_full_out_of_sample_data <- ungroup(cuCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuCU_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



cuCU_rf.5_full_out_of_sample_data_scaled <- cuCU_rf.5_full_out_of_sample_data

cuCU_rf.5_full_out_of_sample_data_scaled$hit_speed <- (cuCU_rf.5_full_out_of_sample_data_scaled$hit_speed - cuCU_center_values[2]) / cuCU_scale_values[2]

cuCU_rf.5_full_out_of_sample_data_scaled$hit_angle <- (cuCU_rf.5_full_out_of_sample_data_scaled$hit_angle - cuCU_center_values[3]) / cuCU_scale_values[3]

cuCU_rf.5.prob <- predict(cuCU_rf.5, cuCU_rf.5_full_out_of_sample_data_scaled, type = "response")

cuCU_rf.5_full_out_of_sample_data <- cbind(filter(cuCU_rf.5_full_out_of_sample_data, row %in% cuCU_out_of_training_rows), cuCU_rf.5.prob)

names(cuCU_rf.5_full_out_of_sample_data)[65] <- "fits"
names(cuCU_rf.5_full_out_of_sample_data)

cuCU_rf.5_full_out_of_sample_data_reduced <- cuCU_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

cuCU_rf.5_mean_table <- cuCU_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

cuCU_rf.5_full_out_of_sample_data$type <- with(cuCU_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

cuCU_rf.5_full_out_of_sample_data$hit_label <- with(cuCU_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

cuCU_rf.5_plot1 <- ggplot(cuCU_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

cuCU_rf.5_plot1

cuCU_rf.5_plot2 <- ggplot(cuCU_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

cuCU_rf.5_plot2

cuCU_rf.5_plot3 <- ggplot(cuCU_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

cuCU_rf.5_plot3

cuCU_grid_plot_rf.5 <- grid.arrange(cuCU_rf.5_plot1, cuCU_rf.5_plot2, cuCU_rf.5_plot3, ncol = 3)
cuCU_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

cuCU_test_rows <- cuCU_test$row
cuCU_validate_rows <- cuCU_validate$row
cuCU_pred_data_emp_1 <- ungroup(cuCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuCU_test_rows)

cuCU_pred_data_emp <- ungroup(cuCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuCU_validate_rows) %>%
	rbind(cuCU_pred_data_emp_1)

cuCU_out_of_training_rows <- cuCU_pred_data_emp$row

cuCU_pred_data_emp <- ungroup(cuCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuCU_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

cuCU_pred_data_emp_scaled <- cuCU_pred_data_emp

cuCU_pred_data_emp_scaled$hit_speed <- (cuCU_pred_data_emp_scaled$hit_speed - cuCU_center_values[2]) / cuCU_scale_values[2]

cuCU_pred_data_emp_scaled$hit_angle <- (cuCU_pred_data_emp_scaled$hit_angle - cuCU_center_values[3]) / cuCU_scale_values[3]

cuCU_pred_prob_hit <- predict(cuCU_rf.5, cuCU_pred_data_emp_scaled, type = "prob")[,2]
cuCU_pred_prob_hit_fits <- cbind(cuCU_pred_data_emp, cuCU_pred_prob_hit)

cuCU_pred_prob_hit_fits[,c(1:2)] <- cuCU_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

cuCU_angle_speed_pred <- cuCU_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = cuCU_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nCU-CU Hit Probability\n") + theme_bp_grey()

cuCU_angle_speed_pred

#slCH <- subset(statcast.2016, second_pitch == "CH" & first_pitch == "SL")
slCH$hit_distance_sc <- as.numeric(slCH$hit_distance_sc, na.rm = TRUE)
slCH$hit_angle <- as.numeric(slCH$hit_angle, na.rm = TRUE)
slCH$hit_speed <- as.numeric(slCH$hit_speed, na.rm = TRUE)

slCH$hit <- with(slCH, ifelse(grepl("Single", slCH$events), 1,
															ifelse(grepl("Double", slCH$events), 1,
																		 ifelse(grepl("Triple", slCH$events), 1, 
																		 			 ifelse(grepl("Home Run", slCH$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

slCH$fieldingTeam <- with(slCH, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[slCH$hit_distance_sc == "null"] = NA
#slCH$hit_speed[slCH$hit_speed == "null"] = NA
#slCH$hit_angle[slCH$hit_angle == "null"] = NA

# include row names for unique record identification

slCH$row <- row.names(slCH) %>% as.numeric()

# recode stand and home_team as factors

slCH$stand <- as.factor(slCH$stand)
slCH$home_team <- as.factor(slCH$home_team)


slCH$game_date <- as.Date(slCH$game_date)


# subset 

slCH_working_data <- ungroup(slCH) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(slCH_working_data)
str(ungroup(slCH))
table(slCH_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

slCH_working_data <- filter(slCH_working_data, hc_x != 1, hc_y != 1)
head(slCH_working_data)

# create training and test sets
# scaled data

set.seed(42)
slCH_train <- sample_frac(slCH_working_data, .15, replace = FALSE)
slCH_split <- setdiff(slCH_working_data, slCH_train)
slCH_test <- sample_frac(slCH_split, .50, replace = FALSE)
slCH_validate <- setdiff(slCH_split, slCH_test)

nrow(slCH_train) + nrow(slCH_test) + nrow(slCH_validate) == nrow(slCH_working_data)

with(slCH_train, table(hit)) %>% prop.table()
with(slCH_test, table(hit)) %>% prop.table()
with(slCH_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(slCH_train)

##########################################################################################
str(slCH)
##as.numeric(slCH$hit_distance_sc, slCH$hit_speed, slCH$hit_angle)

#slCH_train$hit_distance_sc <- as.numeric(slCH_train$hit_distance_sc)
#slCH_train$hit_speed <- as.numeric(slCH_train$hit_speed)
#slCH_train$hit_angle <- as.numeric(slCH_train$hit_angle)
#View(slCH_train)
slCH_scaled_data <- scale(slCH_train[,c(1:5)])
slCH_scale_values <- attr(slCH_scaled_data, 'scaled:scale')
slCH_scale_values
##########################################################################################
slCH_center_values <- attr(slCH_scaled_data, 'scaled:center')
slCH_center_values
##########################################################################################
slCH_train <- cbind(slCH_scaled_data, select(slCH_train, hit:row))

# save levels for factor variables
slCH_levels_home_team <- levels(slCH_train$home_team)
slCH_levels_stand <- levels(slCH_train$stand)
slCH_levels_fieldingTeam <- levels(slCH_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(slCH_test)

slCH_test$hit_distance_sc <- (slCH_test$hit_distance_sc - slCH_center_values[1]) / slCH_scale_values[1]

slCH_test$hit_speed <- (slCH_test$hit_speed - slCH_center_values[2]) / slCH_scale_values[2]

slCH_test$hit_angle <- (slCH_test$hit_angle - slCH_center_values[3]) / slCH_scale_values[3]

slCH_test$hc_x <- (slCH_test$hc_x - slCH_center_values[4]) / slCH_scale_values[4]

slCH_test$hc_y <- (slCH_test$hc_y - slCH_center_values[5]) / slCH_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

slCH_validate$hit_distance_sc <- (slCH_validate$hit_distance_sc - slCH_center_values[1]) / slCH_scale_values[1]

slCH_validate$hit_speed <- (slCH_validate$hit_speed - slCH_center_values[2]) / slCH_scale_values[2]

slCH_validate$hit_angle <- (slCH_validate$hit_angle - slCH_center_values[3]) / slCH_scale_values[3]

slCH_validate$hc_x <- (slCH_validate$hc_x - slCH_center_values[4]) / slCH_scale_values[4]

slCH_validate$hc_y <- (slCH_validate$hc_y - slCH_center_values[5]) / slCH_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(slCH_train)

str(slCH_train)
str(cuSL_train)
set.seed(42)
slCH_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(slCH_train, -row), ntree = 501, importance = TRUE)

print(slCH_rf.1)

plot(slCH_rf.1)

varImpPlot(slCH_rf.1)

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
slCH_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(slCH_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(slCH_rf.5)

plot(slCH_rf.5)

varImpPlot(slCH_rf.5)

slCH_predict_fit.rf.5 <- data.frame(fits = predict(slCH_rf.5, slCH_test, type = "prob")[,2], actuals = slCH_test$hit)

slCH_pred.rf.5 <- prediction(slCH_predict_fit.rf.5$fits, slCH_predict_fit.rf.5$actuals)

slCH_roc.pred.rf.5 <- performance(slCH_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(slCH_roc.pred.rf.5)
abline(a = 0, b = 1)
slCH_opt <- opt.cut(slCH_roc.pred.rf.5, slCH_pred.rf.5)
slCH_opt
slCH_opt <- slCH_opt[3]
slCH_predict_fit.rf.5$fits <- with(slCH_predict_fit.rf.5, ifelse(fits > slCH_opt, 1, 0)) 

str(slCH_test)

#slCH_test$fieldingTeam <- as.character(slCH_test$fieldingTeam)
#slCH_test$home_team <- as.character(slCH_test$home_team)
#slCH_test$hit <- as.logical(slCH_test$hit)
#slCH_test$stand <- as.logical(slCH_test$stand)
str(slCH_test)
slCH_rf.5_confusion_test <- confusionMatrix(model = slCH_rf.5, x = slCH_test, y = slCH_test$hit)
slCH_rf.5_confusion_validate <- confusionMatrix(model = slCH_rf.5, x = slCH_validate, y = slCH_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


slCH_test_rows <- slCH_test$row
slCH_validate_rows <- slCH_validate$row
slCH_pred_data_emp_1 <- ungroup(slCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slCH_test_rows)

slCH_pred_data_emp <- ungroup(slCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slCH_validate_rows) %>%
	rbind(slCH_pred_data_emp_1)

slCH_out_of_training_rows <- slCH_pred_data_emp$row

slCH_rf.5_full_out_of_sample_data <- ungroup(slCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slCH_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



slCH_rf.5_full_out_of_sample_data_scaled <- slCH_rf.5_full_out_of_sample_data

slCH_rf.5_full_out_of_sample_data_scaled$hit_speed <- (slCH_rf.5_full_out_of_sample_data_scaled$hit_speed - slCH_center_values[2]) / slCH_scale_values[2]

slCH_rf.5_full_out_of_sample_data_scaled$hit_angle <- (slCH_rf.5_full_out_of_sample_data_scaled$hit_angle - slCH_center_values[3]) / slCH_scale_values[3]

slCH_rf.5.prob <- predict(slCH_rf.5, slCH_rf.5_full_out_of_sample_data_scaled, type = "response")

slCH_rf.5_full_out_of_sample_data <- cbind(filter(slCH_rf.5_full_out_of_sample_data, row %in% slCH_out_of_training_rows), slCH_rf.5.prob)

names(slCH_rf.5_full_out_of_sample_data)[65] <- "fits"
names(slCH_rf.5_full_out_of_sample_data)

slCH_rf.5_full_out_of_sample_data_reduced <- slCH_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

slCH_rf.5_mean_table <- slCH_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

slCH_rf.5_full_out_of_sample_data$type <- with(slCH_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

slCH_rf.5_full_out_of_sample_data$hit_label <- with(slCH_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

slCH_rf.5_plot1 <- ggplot(slCH_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

slCH_rf.5_plot1

slCH_rf.5_plot2 <- ggplot(slCH_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

slCH_rf.5_plot2

slCH_rf.5_plot3 <- ggplot(slCH_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

slCH_rf.5_plot3

slCH_grid_plot_rf.5 <- grid.arrange(slCH_rf.5_plot1, slCH_rf.5_plot2, slCH_rf.5_plot3, ncol = 3)
slCH_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

slCH_test_rows <- slCH_test$row
slCH_validate_rows <- slCH_validate$row
slCH_pred_data_emp_1 <- ungroup(slCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slCH_test_rows)

slCH_pred_data_emp <- ungroup(slCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slCH_validate_rows) %>%
	rbind(slCH_pred_data_emp_1)

slCH_out_of_training_rows <- slCH_pred_data_emp$row

slCH_pred_data_emp <- ungroup(slCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slCH_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

slCH_pred_data_emp_scaled <- slCH_pred_data_emp

slCH_pred_data_emp_scaled$hit_speed <- (slCH_pred_data_emp_scaled$hit_speed - slCH_center_values[2]) / slCH_scale_values[2]

slCH_pred_data_emp_scaled$hit_angle <- (slCH_pred_data_emp_scaled$hit_angle - slCH_center_values[3]) / slCH_scale_values[3]

slCH_pred_prob_hit <- predict(slCH_rf.5, slCH_pred_data_emp_scaled, type = "prob")[,2]
slCH_pred_prob_hit_fits <- cbind(slCH_pred_data_emp, slCH_pred_prob_hit)

slCH_pred_prob_hit_fits[,c(1:2)] <- slCH_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

slCH_angle_speed_pred <- slCH_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = slCH_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nSL-CH Hit Probability\n") + theme_bp_grey()

slCH_angle_speed_pred

#chSL <- subset(statcast.2016, second_pitch == "SL" & first_pitch == "CH")
chSL$hit_distance_sc <- as.numeric(chSL$hit_distance_sc, na.rm = TRUE)
chSL$hit_angle <- as.numeric(chSL$hit_angle, na.rm = TRUE)
chSL$hit_speed <- as.numeric(chSL$hit_speed, na.rm = TRUE)

chSL$hit <- with(chSL, ifelse(grepl("Single", chSL$events), 1,
															ifelse(grepl("Double", chSL$events), 1,
																		 ifelse(grepl("Triple", chSL$events), 1, 
																		 			 ifelse(grepl("Home Run", chSL$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

chSL$fieldingTeam <- with(chSL, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[chSL$hit_distance_sc == "null"] = NA
#chSL$hit_speed[chSL$hit_speed == "null"] = NA
#chSL$hit_angle[chSL$hit_angle == "null"] = NA

# include row names for unique record identification

chSL$row <- row.names(chSL) %>% as.numeric()

# recode stand and home_team as factors

chSL$stand <- as.factor(chSL$stand)
chSL$home_team <- as.factor(chSL$home_team)


chSL$game_date <- as.Date(chSL$game_date)


# subset 

chSL_working_data <- ungroup(chSL) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(chSL_working_data)
str(ungroup(chSL))
table(chSL_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

chSL_working_data <- filter(chSL_working_data, hc_x != 1, hc_y != 1)
head(chSL_working_data)

# create training and test sets
# scaled data

set.seed(42)
chSL_train <- sample_frac(chSL_working_data, .15, replace = FALSE)
chSL_split <- setdiff(chSL_working_data, chSL_train)
chSL_test <- sample_frac(chSL_split, .50, replace = FALSE)
chSL_validate <- setdiff(chSL_split, chSL_test)

nrow(chSL_train) + nrow(chSL_test) + nrow(chSL_validate) == nrow(chSL_working_data)

with(chSL_train, table(hit)) %>% prop.table()
with(chSL_test, table(hit)) %>% prop.table()
with(chSL_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(chSL_train)

##########################################################################################
str(chSL)
##as.numeric(chSL$hit_distance_sc, chSL$hit_speed, chSL$hit_angle)

#chSL_train$hit_distance_sc <- as.numeric(chSL_train$hit_distance_sc)
#chSL_train$hit_speed <- as.numeric(chSL_train$hit_speed)
#chSL_train$hit_angle <- as.numeric(chSL_train$hit_angle)
#View(chSL_train)
chSL_scaled_data <- scale(chSL_train[,c(1:5)])
chSL_scale_values <- attr(chSL_scaled_data, 'scaled:scale')
chSL_scale_values
##########################################################################################
chSL_center_values <- attr(chSL_scaled_data, 'scaled:center')
chSL_center_values
##########################################################################################
chSL_train <- cbind(chSL_scaled_data, select(chSL_train, hit:row))

# save levels for factor variables
chSL_levels_home_team <- levels(chSL_train$home_team)
chSL_levels_stand <- levels(chSL_train$stand)
chSL_levels_fieldingTeam <- levels(chSL_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(chSL_test)

chSL_test$hit_distance_sc <- (chSL_test$hit_distance_sc - chSL_center_values[1]) / chSL_scale_values[1]

chSL_test$hit_speed <- (chSL_test$hit_speed - chSL_center_values[2]) / chSL_scale_values[2]

chSL_test$hit_angle <- (chSL_test$hit_angle - chSL_center_values[3]) / chSL_scale_values[3]

chSL_test$hc_x <- (chSL_test$hc_x - chSL_center_values[4]) / chSL_scale_values[4]

chSL_test$hc_y <- (chSL_test$hc_y - chSL_center_values[5]) / chSL_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

chSL_validate$hit_distance_sc <- (chSL_validate$hit_distance_sc - chSL_center_values[1]) / chSL_scale_values[1]

chSL_validate$hit_speed <- (chSL_validate$hit_speed - chSL_center_values[2]) / chSL_scale_values[2]

chSL_validate$hit_angle <- (chSL_validate$hit_angle - chSL_center_values[3]) / chSL_scale_values[3]

chSL_validate$hc_x <- (chSL_validate$hc_x - chSL_center_values[4]) / chSL_scale_values[4]

chSL_validate$hc_y <- (chSL_validate$hc_y - chSL_center_values[5]) / chSL_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(chSL_train)

set.seed(42)
chSL_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(chSL_train, -row), ntree = 501, importance = TRUE)

print(chSL_rf.1)

plot(chSL_rf.1)

varImpPlot(chSL_rf.1)

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
chSL_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(chSL_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(chSL_rf.5)

plot(chSL_rf.5)

varImpPlot(chSL_rf.5)

chSL_predict_fit.rf.5 <- data.frame(fits = predict(chSL_rf.5, chSL_test, type = "prob")[,2], actuals = chSL_test$hit)

chSL_pred.rf.5 <- prediction(chSL_predict_fit.rf.5$fits, chSL_predict_fit.rf.5$actuals)

chSL_roc.pred.rf.5 <- performance(chSL_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(chSL_roc.pred.rf.5)
abline(a = 0, b = 1)
chSL_opt <- opt.cut(chSL_roc.pred.rf.5, chSL_pred.rf.5)
chSL_opt
chSL_opt <- chSL_opt[3]
chSL_predict_fit.rf.5$fits <- with(chSL_predict_fit.rf.5, ifelse(fits > chSL_opt, 1, 0)) 

str(chSL_test)

#chSL_test$fieldingTeam <- as.character(chSL_test$fieldingTeam)
#chSL_test$home_team <- as.character(chSL_test$home_team)
#chSL_test$hit <- as.logical(chSL_test$hit)
#chSL_test$stand <- as.logical(chSL_test$stand)
str(chSL_test)
chSL_rf.5_confusion_test <- confusionMatrix(model = chSL_rf.5, x = chSL_test, y = chSL_test$hit)
chSL_rf.5_confusion_validate <- confusionMatrix(model = chSL_rf.5, x = chSL_validate, y = chSL_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


chSL_test_rows <- chSL_test$row
chSL_validate_rows <- chSL_validate$row
chSL_pred_data_emp_1 <- ungroup(chSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chSL_test_rows)

chSL_pred_data_emp <- ungroup(chSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chSL_validate_rows) %>%
	rbind(chSL_pred_data_emp_1)

chSL_out_of_training_rows <- chSL_pred_data_emp$row

chSL_rf.5_full_out_of_sample_data <- ungroup(chSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chSL_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



chSL_rf.5_full_out_of_sample_data_scaled <- chSL_rf.5_full_out_of_sample_data

chSL_rf.5_full_out_of_sample_data_scaled$hit_speed <- (chSL_rf.5_full_out_of_sample_data_scaled$hit_speed - chSL_center_values[2]) / chSL_scale_values[2]

chSL_rf.5_full_out_of_sample_data_scaled$hit_angle <- (chSL_rf.5_full_out_of_sample_data_scaled$hit_angle - chSL_center_values[3]) / chSL_scale_values[3]

chSL_rf.5.prob <- predict(chSL_rf.5, chSL_rf.5_full_out_of_sample_data_scaled, type = "response")

chSL_rf.5_full_out_of_sample_data <- cbind(filter(chSL_rf.5_full_out_of_sample_data, row %in% chSL_out_of_training_rows), chSL_rf.5.prob)

names(chSL_rf.5_full_out_of_sample_data)[65] <- "fits"
names(chSL_rf.5_full_out_of_sample_data)

chSL_rf.5_full_out_of_sample_data_reduced <- chSL_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

chSL_rf.5_mean_table <- chSL_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

chSL_rf.5_full_out_of_sample_data$type <- with(chSL_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

chSL_rf.5_full_out_of_sample_data$hit_label <- with(chSL_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

chSL_rf.5_plot1 <- ggplot(chSL_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

chSL_rf.5_plot1

chSL_rf.5_plot2 <- ggplot(chSL_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

chSL_rf.5_plot2

chSL_rf.5_plot3 <- ggplot(chSL_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

chSL_rf.5_plot3

chSL_grid_plot_rf.5 <- grid.arrange(chSL_rf.5_plot1, chSL_rf.5_plot2, chSL_rf.5_plot3, ncol = 3)
chSL_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

chSL_test_rows <- chSL_test$row
chSL_validate_rows <- chSL_validate$row
chSL_pred_data_emp_1 <- ungroup(chSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chSL_test_rows)

chSL_pred_data_emp <- ungroup(chSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chSL_validate_rows) %>%
	rbind(chSL_pred_data_emp_1)

chSL_out_of_training_rows <- chSL_pred_data_emp$row

chSL_pred_data_emp <- ungroup(chSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chSL_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

chSL_pred_data_emp_scaled <- chSL_pred_data_emp

chSL_pred_data_emp_scaled$hit_speed <- (chSL_pred_data_emp_scaled$hit_speed - chSL_center_values[2]) / chSL_scale_values[2]

chSL_pred_data_emp_scaled$hit_angle <- (chSL_pred_data_emp_scaled$hit_angle - chSL_center_values[3]) / chSL_scale_values[3]

chSL_pred_prob_hit <- predict(chSL_rf.5, chSL_pred_data_emp_scaled, type = "prob")[,2]
chSL_pred_prob_hit_fits <- cbind(chSL_pred_data_emp, chSL_pred_prob_hit)

chSL_pred_prob_hit_fits[,c(1:2)] <- chSL_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

chSL_angle_speed_pred <- chSL_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = chSL_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nCH-SL Hit Probability\n") + theme_bp_grey()

chSL_angle_speed_pred

#slCU <- subset(statcast.2016, second_pitch == "CU" & first_pitch == "SL")
slCU$hit_distance_sc <- as.numeric(slCU$hit_distance_sc, na.rm = TRUE)
slCU$hit_angle <- as.numeric(slCU$hit_angle, na.rm = TRUE)
slCU$hit_speed <- as.numeric(slCU$hit_speed, na.rm = TRUE)

slCU$hit <- with(slCU, ifelse(grepl("Single", slCU$events), 1,
															ifelse(grepl("Double", slCU$events), 1,
																		 ifelse(grepl("Triple", slCU$events), 1, 
																		 			 ifelse(grepl("Home Run", slCU$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

slCU$fieldingTeam <- with(slCU, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[slCU$hit_distance_sc == "null"] = NA
#slCU$hit_speed[slCU$hit_speed == "null"] = NA
#slCU$hit_angle[slCU$hit_angle == "null"] = NA

# include row names for unique record identification

slCU$row <- row.names(slCU) %>% as.numeric()

# recode stand and home_team as factors

slCU$stand <- as.factor(slCU$stand)
slCU$home_team <- as.factor(slCU$home_team)


slCU$game_date <- as.Date(slCU$game_date)


# subset 

slCU_working_data <- ungroup(slCU) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(slCU_working_data)
str(ungroup(slCU))
table(slCU_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

slCU_working_data <- filter(slCU_working_data, hc_x != 1, hc_y != 1)
head(slCU_working_data)

# create training and test sets
# scaled data

set.seed(42)
slCU_train <- sample_frac(slCU_working_data, .15, replace = FALSE)
slCU_split <- setdiff(slCU_working_data, slCU_train)
slCU_test <- sample_frac(slCU_split, .50, replace = FALSE)
slCU_validate <- setdiff(slCU_split, slCU_test)

nrow(slCU_train) + nrow(slCU_test) + nrow(slCU_validate) == nrow(slCU_working_data)

with(slCU_train, table(hit)) %>% prop.table()
with(slCU_test, table(hit)) %>% prop.table()
with(slCU_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(slCU_train)

##########################################################################################
str(slCU)
##as.numeric(slCU$hit_distance_sc, slCU$hit_speed, slCU$hit_angle)

#slCU_train$hit_distance_sc <- as.numeric(slCU_train$hit_distance_sc)
#slCU_train$hit_speed <- as.numeric(slCU_train$hit_speed)
#slCU_train$hit_angle <- as.numeric(slCU_train$hit_angle)
#View(slCU_train)
slCU_scaled_data <- scale(slCU_train[,c(1:5)])
slCU_scale_values <- attr(slCU_scaled_data, 'scaled:scale')
slCU_scale_values
##########################################################################################
slCU_center_values <- attr(slCU_scaled_data, 'scaled:center')
slCU_center_values
##########################################################################################
slCU_train <- cbind(slCU_scaled_data, select(slCU_train, hit:row))

# save levels for factor variables
slCU_levels_home_team <- levels(slCU_train$home_team)
slCU_levels_stand <- levels(slCU_train$stand)
slCU_levels_fieldingTeam <- levels(slCU_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(slCU_test)

slCU_test$hit_distance_sc <- (slCU_test$hit_distance_sc - slCU_center_values[1]) / slCU_scale_values[1]

slCU_test$hit_speed <- (slCU_test$hit_speed - slCU_center_values[2]) / slCU_scale_values[2]

slCU_test$hit_angle <- (slCU_test$hit_angle - slCU_center_values[3]) / slCU_scale_values[3]

slCU_test$hc_x <- (slCU_test$hc_x - slCU_center_values[4]) / slCU_scale_values[4]

slCU_test$hc_y <- (slCU_test$hc_y - slCU_center_values[5]) / slCU_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

slCU_validate$hit_distance_sc <- (slCU_validate$hit_distance_sc - slCU_center_values[1]) / slCU_scale_values[1]

slCU_validate$hit_speed <- (slCU_validate$hit_speed - slCU_center_values[2]) / slCU_scale_values[2]

slCU_validate$hit_angle <- (slCU_validate$hit_angle - slCU_center_values[3]) / slCU_scale_values[3]

slCU_validate$hc_x <- (slCU_validate$hc_x - slCU_center_values[4]) / slCU_scale_values[4]

slCU_validate$hc_y <- (slCU_validate$hc_y - slCU_center_values[5]) / slCU_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(slCU_train)

set.seed(42)
slCU_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(slCU_train, -row), ntree = 501, importance = TRUE)

print(slCU_rf.1)

plot(slCU_rf.1)

varImpPlot(slCU_rf.1)

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
slCU_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(slCU_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(slCU_rf.5)

plot(slCU_rf.5)

varImpPlot(slCU_rf.5)

slCU_predict_fit.rf.5 <- data.frame(fits = predict(slCU_rf.5, slCU_test, type = "prob")[,2], actuals = slCU_test$hit)

slCU_pred.rf.5 <- prediction(slCU_predict_fit.rf.5$fits, slCU_predict_fit.rf.5$actuals)

slCU_roc.pred.rf.5 <- performance(slCU_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(slCU_roc.pred.rf.5)
abline(a = 0, b = 1)
slCU_opt <- opt.cut(slCU_roc.pred.rf.5, slCU_pred.rf.5)
slCU_opt
slCU_opt <- slCU_opt[3]
slCU_predict_fit.rf.5$fits <- with(slCU_predict_fit.rf.5, ifelse(fits > slCU_opt, 1, 0)) 

str(slCU_test)

#slCU_test$fieldingTeam <- as.character(slCU_test$fieldingTeam)
#slCU_test$home_team <- as.character(slCU_test$home_team)
#slCU_test$hit <- as.logical(slCU_test$hit)
#slCU_test$stand <- as.logical(slCU_test$stand)
str(slCU_test)
slCU_rf.5_confusion_test <- confusionMatrix(model = slCU_rf.5, x = slCU_test, y = slCU_test$hit)
slCU_rf.5_confusion_validate <- confusionMatrix(model = slCU_rf.5, x = slCU_validate, y = slCU_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


slCU_test_rows <- slCU_test$row
slCU_validate_rows <- slCU_validate$row
slCU_pred_data_emp_1 <- ungroup(slCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slCU_test_rows)

slCU_pred_data_emp <- ungroup(slCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slCU_validate_rows) %>%
	rbind(slCU_pred_data_emp_1)

slCU_out_of_training_rows <- slCU_pred_data_emp$row

slCU_rf.5_full_out_of_sample_data <- ungroup(slCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slCU_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



slCU_rf.5_full_out_of_sample_data_scaled <- slCU_rf.5_full_out_of_sample_data

slCU_rf.5_full_out_of_sample_data_scaled$hit_speed <- (slCU_rf.5_full_out_of_sample_data_scaled$hit_speed - slCU_center_values[2]) / slCU_scale_values[2]

slCU_rf.5_full_out_of_sample_data_scaled$hit_angle <- (slCU_rf.5_full_out_of_sample_data_scaled$hit_angle - slCU_center_values[3]) / slCU_scale_values[3]

slCU_rf.5.prob <- predict(slCU_rf.5, slCU_rf.5_full_out_of_sample_data_scaled, type = "response")

slCU_rf.5_full_out_of_sample_data <- cbind(filter(slCU_rf.5_full_out_of_sample_data, row %in% slCU_out_of_training_rows), slCU_rf.5.prob)

names(slCU_rf.5_full_out_of_sample_data)[65] <- "fits"
names(slCU_rf.5_full_out_of_sample_data)

slCU_rf.5_full_out_of_sample_data_reduced <- slCU_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

slCU_rf.5_mean_table <- slCU_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

slCU_rf.5_full_out_of_sample_data$type <- with(slCU_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

slCU_rf.5_full_out_of_sample_data$hit_label <- with(slCU_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

slCU_rf.5_plot1 <- ggplot(slCU_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

slCU_rf.5_plot1

slCU_rf.5_plot2 <- ggplot(slCU_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

slCU_rf.5_plot2

slCU_rf.5_plot3 <- ggplot(slCU_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

slCU_rf.5_plot3

slCU_grid_plot_rf.5 <- grid.arrange(slCU_rf.5_plot1, slCU_rf.5_plot2, slCU_rf.5_plot3, ncol = 3)
slCU_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

slCU_test_rows <- slCU_test$row
slCU_validate_rows <- slCU_validate$row
slCU_pred_data_emp_1 <- ungroup(slCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slCU_test_rows)

slCU_pred_data_emp <- ungroup(slCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slCU_validate_rows) %>%
	rbind(slCU_pred_data_emp_1)

slCU_out_of_training_rows <- slCU_pred_data_emp$row

slCU_pred_data_emp <- ungroup(slCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slCU_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

slCU_pred_data_emp_scaled <- slCU_pred_data_emp

slCU_pred_data_emp_scaled$hit_speed <- (slCU_pred_data_emp_scaled$hit_speed - slCU_center_values[2]) / slCU_scale_values[2]

slCU_pred_data_emp_scaled$hit_angle <- (slCU_pred_data_emp_scaled$hit_angle - slCU_center_values[3]) / slCU_scale_values[3]

slCU_pred_prob_hit <- predict(slCU_rf.5, slCU_pred_data_emp_scaled, type = "prob")[,2]
slCU_pred_prob_hit_fits <- cbind(slCU_pred_data_emp, slCU_pred_prob_hit)

slCU_pred_prob_hit_fits[,c(1:2)] <- slCU_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

slCU_angle_speed_pred <- slCU_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = slCU_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nSL-CU Hit Probability\n") + theme_bp_grey()

slCU_angle_speed_pred

#chCU <- subset(statcast.2016, second_pitch == "CU" & first_pitch == "CH")
chCU$hit_distance_sc <- as.numeric(chCU$hit_distance_sc, na.rm = TRUE)
chCU$hit_angle <- as.numeric(chCU$hit_angle, na.rm = TRUE)
chCU$hit_speed <- as.numeric(chCU$hit_speed, na.rm = TRUE)

chCU$hit <- with(chCU, ifelse(grepl("Single", chCU$events), 1,
															ifelse(grepl("Double", chCU$events), 1,
																		 ifelse(grepl("Triple", chCU$events), 1, 
																		 			 ifelse(grepl("Home Run", chCU$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

chCU$fieldingTeam <- with(chCU, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[chCU$hit_distance_sc == "null"] = NA
#chCU$hit_speed[chCU$hit_speed == "null"] = NA
#chCU$hit_angle[chCU$hit_angle == "null"] = NA

# include row names for unique record identification

chCU$row <- row.names(chCU) %>% as.numeric()

# recode stand and home_team as factors

chCU$stand <- as.factor(chCU$stand)
chCU$home_team <- as.factor(chCU$home_team)


chCU$game_date <- as.Date(chCU$game_date)


# subset 

chCU_working_data <- ungroup(chCU) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(chCU_working_data)
str(ungroup(chCU))
table(chCU_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

chCU_working_data <- filter(chCU_working_data, hc_x != 1, hc_y != 1)
head(chCU_working_data)

# create training and test sets
# scaled data

set.seed(42)
chCU_train <- sample_frac(chCU_working_data, .15, replace = FALSE)
chCU_split <- setdiff(chCU_working_data, chCU_train)
chCU_test <- sample_frac(chCU_split, .50, replace = FALSE)
chCU_validate <- setdiff(chCU_split, chCU_test)

nrow(chCU_train) + nrow(chCU_test) + nrow(chCU_validate) == nrow(chCU_working_data)

with(chCU_train, table(hit)) %>% prop.table()
with(chCU_test, table(hit)) %>% prop.table()
with(chCU_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(chCU_train)

##########################################################################################
str(chCU)
##as.numeric(chCU$hit_distance_sc, chCU$hit_speed, chCU$hit_angle)

#chCU_train$hit_distance_sc <- as.numeric(chCU_train$hit_distance_sc)
#chCU_train$hit_speed <- as.numeric(chCU_train$hit_speed)
#chCU_train$hit_angle <- as.numeric(chCU_train$hit_angle)
#View(chCU_train)
chCU_scaled_data <- scale(chCU_train[,c(1:5)])
chCU_scale_values <- attr(chCU_scaled_data, 'scaled:scale')
chCU_scale_values
##########################################################################################
chCU_center_values <- attr(chCU_scaled_data, 'scaled:center')
chCU_center_values
##########################################################################################
chCU_train <- cbind(chCU_scaled_data, select(chCU_train, hit:row))

# save levels for factor variables
chCU_levels_home_team <- levels(chCU_train$home_team)
chCU_levels_stand <- levels(chCU_train$stand)
chCU_levels_fieldingTeam <- levels(chCU_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(chCU_test)

chCU_test$hit_distance_sc <- (chCU_test$hit_distance_sc - chCU_center_values[1]) / chCU_scale_values[1]

chCU_test$hit_speed <- (chCU_test$hit_speed - chCU_center_values[2]) / chCU_scale_values[2]

chCU_test$hit_angle <- (chCU_test$hit_angle - chCU_center_values[3]) / chCU_scale_values[3]

chCU_test$hc_x <- (chCU_test$hc_x - chCU_center_values[4]) / chCU_scale_values[4]

chCU_test$hc_y <- (chCU_test$hc_y - chCU_center_values[5]) / chCU_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

chCU_validate$hit_distance_sc <- (chCU_validate$hit_distance_sc - chCU_center_values[1]) / chCU_scale_values[1]

chCU_validate$hit_speed <- (chCU_validate$hit_speed - chCU_center_values[2]) / chCU_scale_values[2]

chCU_validate$hit_angle <- (chCU_validate$hit_angle - chCU_center_values[3]) / chCU_scale_values[3]

chCU_validate$hc_x <- (chCU_validate$hc_x - chCU_center_values[4]) / chCU_scale_values[4]

chCU_validate$hc_y <- (chCU_validate$hc_y - chCU_center_values[5]) / chCU_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(chCU_train)

set.seed(42)
chCU_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(chCU_train, -row), ntree = 501, importance = TRUE)

print(chCU_rf.1)

plot(chCU_rf.1)

varImpPlot(chCU_rf.1)

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
chCU_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(chCU_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(chCU_rf.5)

plot(chCU_rf.5)

varImpPlot(chCU_rf.5)

chCU_predict_fit.rf.5 <- data.frame(fits = predict(chCU_rf.5, chCU_test, type = "prob")[,2], actuals = chCU_test$hit)

chCU_pred.rf.5 <- prediction(chCU_predict_fit.rf.5$fits, chCU_predict_fit.rf.5$actuals)

chCU_roc.pred.rf.5 <- performance(chCU_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(chCU_roc.pred.rf.5)
abline(a = 0, b = 1)
chCU_opt <- opt.cut(chCU_roc.pred.rf.5, chCU_pred.rf.5)
chCU_opt
chCU_opt <- chCU_opt[3]
chCU_predict_fit.rf.5$fits <- with(chCU_predict_fit.rf.5, ifelse(fits > chCU_opt, 1, 0)) 

str(chCU_test)

#chCU_test$fieldingTeam <- as.character(chCU_test$fieldingTeam)
#chCU_test$home_team <- as.character(chCU_test$home_team)
#chCU_test$hit <- as.logical(chCU_test$hit)
#chCU_test$stand <- as.logical(chCU_test$stand)
str(chCU_test)
chCU_rf.5_confusion_test <- confusionMatrix(model = chCU_rf.5, x = chCU_test, y = chCU_test$hit)
chCU_rf.5_confusion_validate <- confusionMatrix(model = chCU_rf.5, x = chCU_validate, y = chCU_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


chCU_test_rows <- chCU_test$row
chCU_validate_rows <- chCU_validate$row
chCU_pred_data_emp_1 <- ungroup(chCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chCU_test_rows)

chCU_pred_data_emp <- ungroup(chCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chCU_validate_rows) %>%
	rbind(chCU_pred_data_emp_1)

chCU_out_of_training_rows <- chCU_pred_data_emp$row

chCU_rf.5_full_out_of_sample_data <- ungroup(chCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chCU_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



chCU_rf.5_full_out_of_sample_data_scaled <- chCU_rf.5_full_out_of_sample_data

chCU_rf.5_full_out_of_sample_data_scaled$hit_speed <- (chCU_rf.5_full_out_of_sample_data_scaled$hit_speed - chCU_center_values[2]) / chCU_scale_values[2]

chCU_rf.5_full_out_of_sample_data_scaled$hit_angle <- (chCU_rf.5_full_out_of_sample_data_scaled$hit_angle - chCU_center_values[3]) / chCU_scale_values[3]

chCU_rf.5.prob <- predict(chCU_rf.5, chCU_rf.5_full_out_of_sample_data_scaled, type = "response")

chCU_rf.5_full_out_of_sample_data <- cbind(filter(chCU_rf.5_full_out_of_sample_data, row %in% chCU_out_of_training_rows), chCU_rf.5.prob)

names(chCU_rf.5_full_out_of_sample_data)[65] <- "fits"
names(chCU_rf.5_full_out_of_sample_data)

chCU_rf.5_full_out_of_sample_data_reduced <- chCU_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

chCU_rf.5_mean_table <- chCU_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

chCU_rf.5_full_out_of_sample_data$type <- with(chCU_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

chCU_rf.5_full_out_of_sample_data$hit_label <- with(chCU_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

chCU_rf.5_plot1 <- ggplot(chCU_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

chCU_rf.5_plot1

chCU_rf.5_plot2 <- ggplot(chCU_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

chCU_rf.5_plot2

chCU_rf.5_plot3 <- ggplot(chCU_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

chCU_rf.5_plot3

chCU_grid_plot_rf.5 <- grid.arrange(chCU_rf.5_plot1, chCU_rf.5_plot2, chCU_rf.5_plot3, ncol = 3)
chCU_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

chCU_test_rows <- chCU_test$row
chCU_validate_rows <- chCU_validate$row
chCU_pred_data_emp_1 <- ungroup(chCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chCU_test_rows)

chCU_pred_data_emp <- ungroup(chCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chCU_validate_rows) %>%
	rbind(chCU_pred_data_emp_1)

chCU_out_of_training_rows <- chCU_pred_data_emp$row

chCU_pred_data_emp <- ungroup(chCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chCU_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

chCU_pred_data_emp_scaled <- chCU_pred_data_emp

chCU_pred_data_emp_scaled$hit_speed <- (chCU_pred_data_emp_scaled$hit_speed - chCU_center_values[2]) / chCU_scale_values[2]

chCU_pred_data_emp_scaled$hit_angle <- (chCU_pred_data_emp_scaled$hit_angle - chCU_center_values[3]) / chCU_scale_values[3]

chCU_pred_prob_hit <- predict(chCU_rf.5, chCU_pred_data_emp_scaled, type = "prob")[,2]
chCU_pred_prob_hit_fits <- cbind(chCU_pred_data_emp, chCU_pred_prob_hit)

chCU_pred_prob_hit_fits[,c(1:2)] <- chCU_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

chCU_angle_speed_pred <- chCU_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = chCU_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nCH-CU Hit Probability\n") + theme_bp_grey()

chCU_angle_speed_pred
