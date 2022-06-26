str(whiffs.data$whiffs)
# determine expected whiff rate based on:
# factor px (pitch horizontal) and pz (pitch vertical)
# pitch sequence
#	normalize sz_top and sz_bot
# Further determine parameters on indivudal pitches

whiffs.data$px <- as.numeric(whiffs.data$px, na.rm = TRUE)
whiffs.data$pz <- as.numeric(whiffs.data$pz, na.rm = TRUE)
whiffs.data$sz_bot <- as.numeric(whiffs.data$sz_bot)
whiffs.data$sz_top <- as.numeric(whiffs.data$sz_top)
#whiffs.data$hit_speed <- as.numeric(whiffs.data$hit_speed, na.rm = TRUE)
factor(whiffs.data$description)

head(whiffs.data)
swings.only <- subset(whiffs.data, description %in% c("Strike Swinging", "Foul", "In play, out(s)", "In play, no out", "In play, run(s)"))
swings.only$whiffs <- with(swings.only, ifelse(grepl("Strike Swinging", swings.only$description), 1,
																							ifelse(grepl("Foul", swings.only$description), 0, 
																								ifelse(grepl("In play, out(s)", swings.only$description), 0,
																									ifelse(grepl("In play, no out", swings.only$description), 0,
																										ifelse(grepl("In play, run(s)", swings.only$description), 0, 1)))))) %>% 
	
	as.factor()


head(swings.only$whiffs)
# create a variable for the fielding team

#swings.only$fieldingTeam <- with(swings.only, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
#	as.factor()

#F$hit_distance_sc[swings.only$hit_distance_sc == "null"] = NA
#swings.only$hit_speed[swings.only$hit_speed == "null"] = NA
#swings.only$hit_angle[swings.only$hit_angle == "null"] = NA

# include row names for unique record identification

swings.only$row <- row.names(swings.only) %>% as.numeric()

# recode stand and home_team as factors

swings.only$stand <- as.factor(swings.only$stand)
swings.only$home_team <- as.factor(swings.only$home_team)


swings.only$game_date <- as.Date(swings.only$game_date)
swings.only$whiffs <- as.factor(swings.only$whiffs)
str(swings.only)
# subset 
View(statcast.2016)

swings.only_working_data <- ungroup(swings.only) %>%
	filter(game_date <= "2016-05-28") %>%
	select(whiffs:sz_top, sz_bot, spin_rate, x0, z0, first_pitch) %>%
	filter(!is.na(sz_top)) %>%
	filter(!is.na(sz_bot)) %>% 
	filter(!is.na(spin_rate)) %>%
	filter(!is.na(x0)) %>%
	filter(!is.na(z0)) %>%
	arrange(desc(whiffs))
head(swings.only_working_data)
str(ungroup(swings.only))
table(swings.only_working_data$whiffs)

# remove apparently miscoded balls with x,y of 1,1

swings.only_working_data <- filter(swings.only_working_data, x0 != 1, z0 != 1)
head(swings.only_working_data)

# create training and test sets
# scaled data

set.seed(42)
swings.only_train <- sample_frac(swings.only_working_data, .15, replace = FALSE)
swings.only_split <- setdiff(swings.only_working_data, swings.only_train)
swings.only_test <- sample_frac(swings.only_split, .50, replace = FALSE)
swings.only_validate <- setdiff(swings.only_split, swings.only_test)

nrow(swings.only_train) + nrow(swings.only_test) + nrow(swings.only_validate) == nrow(swings.only_working_data)

with(swings.only_train, table(whiffs)) %>% prop.table()
with(swings.only_test, table(whiffs)) %>% prop.table()
with(swings.only_validate, table(whiffs)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
str(swings.only)
##as.numeric(swings.only$hit_distance_sc, swings.only$hit_speed, swings.only$hit_angle)

#swings.only_train$hit_distance_sc <- as.numeric(swings.only_train$hit_distance_sc)
#swings.only_train$hit_speed <- as.numeric(swings.only_train$hit_speed)
#swings.only_train$hit_angle <- as.numeric(swings.only_train$hit_angle)
#View(swings.only_train)
View(swings.only_train)
swings.only_scaled_data <- scale(swings.only_train[,c(9,10)])
swings.only_scale_values <- attr(swings.only_scaled_data, 'scaled:scale')
swings.only_scale_values
swings.only_center_values <- attr(swings.only_scaled_data, 'scaled:center')
swings.only_center_values
swings.only_train <- cbind(swings.only_scaled_data, select(swings.only_train, whiffs))

# save levels for factor variables
str(swings.only)
as.factor(swings.only$second_pitch)
as.factor(swings.only$first_pitch)

swings.only_levels_home_team <- levels(swings.only_train$home_team)
swings.only_levels_stand <- levels(swings.only_train$stand)
swings.only_levels_fieldingTeam <- levels(swings.only_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(swings.only_test)

WhiffReg <- 

swings.only_test$whiffs <- (swings.only_test$whiffs - swings.only_center_values[1]) / swings.only_scale_values[1]

swings.only_test$sz_top <- (swings.only_test$sz_top - swings.only_center_values[2]) / swings.only_scale_values[2]

swings.only_test$sz_bot <- (swings.only_test$sz_bot - swings.only_center_values[3]) / swings.only_scale_values[3]

swings.only_test$first_pitch <- (swings.only_test$first_pitch - swings.only_center_values[4]) / swings.only_scale_values[4]

swings.only_test$second_pitch <- (swings.only_test$second_pitch - swings.only_center_values[5]) / swings.only_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

swings.only_validate$whiffs <- (swings.only_validate$whiffs - swings.only_center_values[1]) / swings.only_scale_values[1]

swings.only_validate$sz_top <- (swings.only_validate$sz_top - swings.only_center_values[2]) / swings.only_scale_values[2]

swings.only_validate$sz_bot <- (swings.only_validate$sz_bot - swings.only_center_values[3]) / swings.only_scale_values[3]

#swings.only_validate$hc_x <- (swings.only_validate$hc_x - swings.only_center_values[4]) / swings.only_scale_values[4]

#swings.only_validate$hc_y <- (swings.only_validate$hc_y - swings.only_center_values[5]) / swings.only_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(swings.only_train)

set.seed(42)
swings.only_rf.1 <- randomForest(as.factor(whiffs) ~ ., data = select(swings.only_train, whiffs), ntree = 501, importance = TRUE)

print(swings.only_rf.1)

plot(swings.only_rf.1)

varImpPlot(swings.only_rf.1)

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
swings.only_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(swings.only_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(swings.only_rf.5)

plot(swings.only_rf.5)

varImpPlot(swings.only_rf.5)

swings.only_predict_fit.rf.5 <- data.frame(fits = predict(swings.only_rf.5, swings.only_test, type = "prob")[,2], actuals = swings.only_test$hit)

swings.only_pred.rf.5 <- prediction(swings.only_predict_fit.rf.5$fits, swings.only_predict_fit.rf.5$actuals)

swings.only_roc.pred.rf.5 <- performance(swings.only_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(swings.only_roc.pred.rf.5)
abline(a = 0, b = 1)
swings.only_opt <- opt.cut(swings.only_roc.pred.rf.5, swings.only_pred.rf.5)
swings.only_opt
swings.only_opt <- swings.only_opt[3]
swings.only_predict_fit.rf.5$fits <- with(swings.only_predict_fit.rf.5, ifelse(fits > swings.only_opt, 1, 0)) 

str(swings.only_test)

#swings.only_test$fieldingTeam <- as.character(swings.only_test$fieldingTeam)
#swings.only_test$home_team <- as.character(swings.only_test$home_team)
#swings.only_test$hit <- as.logical(swings.only_test$hit)
#swings.only_test$stand <- as.logical(swings.only_test$stand)
str(swings.only_test)
swings.only_rf.5_confusion_test <- confusionMatrix(model = swings.only_rf.5, x = swings.only_test, y = swings.only_test$hit)
swings.only_rf.5_confusion_validate <- confusionMatrix(model = swings.only_rf.5, x = swings.only_validate, y = swings.only_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


swings.only_test_rows <- swings.only_test$row
swings.only_validate_rows <- swings.only_validate$row
swings.only_pred_data_emp_1 <- ungroup(swings.only) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% swings.only_test_rows)

swings.only_pred_data_emp <- ungroup(swings.only) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% swings.only_validate_rows) %>%
	rbind(swings.only_pred_data_emp_1)

swings.only_out_of_training_rows <- swings.only_pred_data_emp$row

swings.only_rf.5_full_out_of_sample_data <- ungroup(swings.only) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% swings.only_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



swings.only_rf.5_full_out_of_sample_data_scaled <- swings.only_rf.5_full_out_of_sample_data

swings.only_rf.5_full_out_of_sample_data_scaled$hit_speed <- (swings.only_rf.5_full_out_of_sample_data_scaled$hit_speed - swings.only_center_values[2]) / swings.only_scale_values[2]

swings.only_rf.5_full_out_of_sample_data_scaled$hit_angle <- (swings.only_rf.5_full_out_of_sample_data_scaled$hit_angle - swings.only_center_values[3]) / swings.only_scale_values[3]

swings.only_rf.5.prob <- predict(swings.only_rf.5, swings.only_rf.5_full_out_of_sample_data_scaled, type = "response")

swings.only_rf.5_full_out_of_sample_data <- cbind(filter(swings.only_rf.5_full_out_of_sample_data, row %in% swings.only_out_of_training_rows), swings.only_rf.5.prob)

names(swings.only_rf.5_full_out_of_sample_data)[65] <- "fits"
names(swings.only_rf.5_full_out_of_sample_data)

swings.only_rf.5_full_out_of_sample_data_reduced <- swings.only_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

swings.only_rf.5_mean_table <- swings.only_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

swings.only_rf.5_full_out_of_sample_data$type <- with(swings.only_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

swings.only_rf.5_full_out_of_sample_data$hit_label <- with(swings.only_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

swings.only_rf.5_plot1 <- ggplot(swings.only_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

swings.only_rf.5_plot1

swings.only_rf.5_plot2 <- ggplot(swings.only_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

swings.only_rf.5_plot2

swings.only_rf.5_plot3 <- ggplot(swings.only_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

swings.only_rf.5_plot3

swings.only_grid_plot_rf.5 <- grid.arrange(swings.only_rf.5_plot1, swings.only_rf.5_plot2, swings.only_rf.5_plot3, ncol = 3)
swings.only_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

swings.only_test_rows <- swings.only_test$row
swings.only_validate_rows <- swings.only_validate$row
swings.only_pred_data_emp_1 <- ungroup(swings.only) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% swings.only_test_rows)

swings.only_pred_data_emp <- ungroup(swings.only) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% swings.only_validate_rows) %>%
	rbind(swings.only_pred_data_emp_1)

swings.only_out_of_training_rows <- swings.only_pred_data_emp$row

swings.only_pred_data_emp <- ungroup(swings.only) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% swings.only_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

swings.only_pred_data_emp_scaled <- swings.only_pred_data_emp

swings.only_pred_data_emp_scaled$hit_speed <- (swings.only_pred_data_emp_scaled$hit_speed - swings.only_center_values[2]) / swings.only_scale_values[2]

swings.only_pred_data_emp_scaled$hit_angle <- (swings.only_pred_data_emp_scaled$hit_angle - swings.only_center_values[3]) / swings.only_scale_values[3]

swings.only_pred_prob_hit <- predict(swings.only_rf.5, swings.only_pred_data_emp_scaled, type = "prob")[,2]
swings.only_pred_prob_hit_fits <- cbind(swings.only_pred_data_emp, swings.only_pred_prob_hit)

swings.only_pred_prob_hit_fits[,c(1:2)] <- swings.only_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

swings.only_angle_speed_pred <- swings.only_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = swings.only_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nFS-FF Hit Probability\n") + theme_bp_grey()

swings.only_angle_speed_pred	
