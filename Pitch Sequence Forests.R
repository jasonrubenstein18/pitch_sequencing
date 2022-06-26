FF <- subset(statcast.2016, second_pitch == "FF")
SL <- subset(statcast.2016, second_pitch == "SL")
CH <- subset(statcast.2016, second_pitch == "CH")
CU <- subset(statcast.2016, second_pitch == "CU")
##############

ffSL$hit_distance_sc <- as.numeric(ffSL$hit_distance_sc, na.rm = TRUE)
ffSL$hit_angle <- as.numeric(ffSL$hit_angle, na.rm = TRUE)
ffSL$hit_speed <- as.numeric(ffSL$hit_speed, na.rm = TRUE)

ffSL$hit <- with(ffSL, ifelse(grepl("Single", ffSL$events), 1,
													ifelse(grepl("Double", ffSL$events), 1,
																 ifelse(grepl("Triple", ffSL$events), 1, 
																 			 ifelse(grepl("Home Run", ffSL$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

ffSL$fieldingTeam <- with(ffSL, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[ffSL$hit_distance_sc == "null"] = NA
#ffSL$hit_speed[ffSL$hit_speed == "null"] = NA
#ffSL$hit_angle[ffSL$hit_angle == "null"] = NA

# include row names for unique record identification

ffSL$row <- row.names(ffSL) %>% as.numeric()

# recode stand and home_team as factors

ffSL$stand <- as.factor(ffSL$stand)
ffSL$home_team <- as.factor(ffSL$home_team)


ffSL$game_date <- as.Date(ffSL$game_date)


# subset 

ffSL_working_data <- ungroup(ffSL) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team, row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(ffSL_working_data)
str(ungroup(ffSL))
table(ffSL_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

ffSL_working_data <- filter(ffSL_working_data, hc_x != 1, hc_y != 1)
head(ffSL_working_data)

# create training and test sets
# scaled data

set.seed(42)
ffSL_train <- sample_frac(ffSL_working_data, .15, replace = FALSE)
ffSL_split <- setdiff(ffSL_working_data, ffSL_train)
ffSL_test <- sample_frac(ffSL_split, .50, replace = FALSE)
ffSL_validate <- setdiff(ffSL_split, ffSL_test)

nrow(ffSL_train) + nrow(ffSL_test) + nrow(ffSL_validate) == nrow(ffSL_working_data)

with(ffSL_train, table(hit)) %>% prop.table()
with(ffSL_test, table(hit)) %>% prop.table()
with(ffSL_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(ffSL_train)

##########################################################################################
str(ffSL)
##as.numeric(ffSL$hit_distance_sc, ffSL$hit_speed, ffSL$hit_angle)

#ffSL_train$hit_distance_sc <- as.numeric(ffSL_train$hit_distance_sc)
#ffSL_train$hit_speed <- as.numeric(ffSL_train$hit_speed)
#ffSL_train$hit_angle <- as.numeric(ffSL_train$hit_angle)
#View(ffSL_train)
ffSL_scaled_data <- scale(ffSL_train[,c(1:5)])
ffSL_scale_values <- attr(ffSL_scaled_data, 'scaled:scale')
ffSL_scale_values
##########################################################################################
ffSL_center_values <- attr(ffSL_scaled_data, 'scaled:center')
ffSL_center_values
##########################################################################################
ffSL_train <- cbind(ffSL_scaled_data, select(ffSL_train, hit:row))

# save levels for factor variables
ffSL_levels_home_team <- levels(ffSL_train$home_team)
ffSL_levels_stand <- levels(ffSL_train$stand)
ffSL_levels_fieldingTeam <- levels(ffSL_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(ffSL_test)

ffSL_test$hit_distance_sc <- (ffSL_test$hit_distance_sc - ffSL_center_values[1]) / ffSL_scale_values[1]

ffSL_test$hit_speed <- (ffSL_test$hit_speed - ffSL_center_values[2]) / ffSL_scale_values[2]

ffSL_test$hit_angle <- (ffSL_test$hit_angle - ffSL_center_values[3]) / ffSL_scale_values[3]

ffSL_test$hc_x <- (ffSL_test$hc_x - ffSL_center_values[4]) / ffSL_scale_values[4]

ffSL_test$hc_y <- (ffSL_test$hc_y - ffSL_center_values[5]) / ffSL_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

ffSL_validate$hit_distance_sc <- (ffSL_validate$hit_distance_sc - ffSL_center_values[1]) / ffSL_scale_values[1]

ffSL_validate$hit_speed <- (ffSL_validate$hit_speed - ffSL_center_values[2]) / ffSL_scale_values[2]

ffSL_validate$hit_angle <- (ffSL_validate$hit_angle - ffSL_center_values[3]) / ffSL_scale_values[3]

ffSL_validate$hc_x <- (ffSL_validate$hc_x - ffSL_center_values[4]) / ffSL_scale_values[4]

ffSL_validate$hc_y <- (ffSL_validate$hc_y - ffSL_center_values[5]) / ffSL_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(ffSL_train)

set.seed(42)
ffSL_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(ffSL_train, -row), ntree = 501, importance = TRUE)

print(ffSL_rf.1)

plot(ffSL_rf.1)

varImpPlot(ffSL_rf.1)

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
ffSL_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(ffSL_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(ffSL_rf.5)

plot(ffSL_rf.5)

varImpPlot(ffSL_rf.5)

ffSL_predict_fit.rf.5 <- data.frame(fits = predict(ffSL_rf.5, ffSL_test, type = "prob")[,2], actuals = ffSL_test$hit)

ffSL_pred.rf.5 <- prediction(ffSL_predict_fit.rf.5$fits, ffSL_predict_fit.rf.5$actuals)

ffSL_roc.pred.rf.5 <- performance(ffSL_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(ffSL_roc.pred.rf.5)
abline(a = 0, b = 1)
ffSL_opt <- opt.cut(ffSL_roc.pred.rf.5, ffSL_pred.rf.5)
ffSL_opt
ffSL_opt <- ffSL_opt[3]
ffSL_predict_fit.rf.5$fits <- with(ffSL_predict_fit.rf.5, ifelse(fits > ffSL_opt, 1, 0)) 

str(ffSL_test)

#ffSL_test$fieldingTeam <- as.character(ffSL_test$fieldingTeam)
#ffSL_test$home_team <- as.character(ffSL_test$home_team)
#ffSL_test$hit <- as.logical(ffSL_test$hit)
#ffSL_test$stand <- as.logical(ffSL_test$stand)
str(ffSL_test)
ffSL_rf.5_confusion_test <- confusionMatrix(model = ffSL_rf.5, x = ffSL_test, y = ffSL_test$hit)
ffSL_rf.5_confusion_validate <- confusionMatrix(model = ffSL_rf.5, x = ffSL_validate, y = ffSL_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


ffSL_test_rows <- ffSL_test$row
ffSL_validate_rows <- ffSL_validate$row
ffSL_pred_data_emp_1 <- ungroup(ffSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffSL_test_rows)

ffSL_pred_data_emp <- ungroup(ffSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffSL_validate_rows) %>%
	rbind(ffSL_pred_data_emp_1)

ffSL_out_of_training_rows <- ffSL_pred_data_emp$row

ffSL_rf.5_full_out_of_sample_data <- ungroup(ffSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffSL_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



ffSL_rf.5_full_out_of_sample_data_scaled <- ffSL_rf.5_full_out_of_sample_data

ffSL_rf.5_full_out_of_sample_data_scaled$hit_speed <- (ffSL_rf.5_full_out_of_sample_data_scaled$hit_speed - ffSL_center_values[2]) / ffSL_scale_values[2]

ffSL_rf.5_full_out_of_sample_data_scaled$hit_angle <- (ffSL_rf.5_full_out_of_sample_data_scaled$hit_angle - ffSL_center_values[3]) / ffSL_scale_values[3]

ffSL_rf.5.prob <- predict(ffSL_rf.5, ffSL_rf.5_full_out_of_sample_data_scaled, type = "response")

ffSL_rf.5_full_out_of_sample_data <- cbind(filter(ffSL_rf.5_full_out_of_sample_data, row %in% ffSL_out_of_training_rows), ffSL_rf.5.prob)

names(ffSL_rf.5_full_out_of_sample_data)[65] <- "fits"
names(ffSL_rf.5_full_out_of_sample_data)

ffSL_rf.5_full_out_of_sample_data_reduced <- ffSL_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

ffSL_rf.5_mean_table <- ffSL_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

ffSL_rf.5_full_out_of_sample_data$type <- with(ffSL_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

ffSL_rf.5_full_out_of_sample_data$hit_label <- with(ffSL_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

ffSL_rf.5_plot1 <- ggplot(ffSL_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

ffSL_rf.5_plot1

ffSL_rf.5_plot2 <- ggplot(ffSL_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

ffSL_rf.5_plot2

ffSL_rf.5_plot3 <- ggplot(ffSL_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

ffSL_rf.5_plot3

ffSL_grid_plot_rf.5 <- grid.arrange(ffSL_rf.5_plot1, ffSL_rf.5_plot2, ffSL_rf.5_plot3, ncol = 3)
ffSL_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

ffSL_test_rows <- ffSL_test$row
ffSL_validate_rows <- ffSL_validate$row
ffSL_pred_data_emp_1 <- ungroup(ffSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffSL_test_rows)

ffSL_pred_data_emp <- ungroup(ffSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffSL_validate_rows) %>%
	rbind(ffSL_pred_data_emp_1)

ffSL_out_of_training_rows <- ffSL_pred_data_emp$row

ffSL_pred_data_emp <- ungroup(ffSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffSL_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

ffSL_pred_data_emp_scaled <- ffSL_pred_data_emp

ffSL_pred_data_emp_scaled$hit_speed <- (ffSL_pred_data_emp_scaled$hit_speed - ffSL_center_values[2]) / ffSL_scale_values[2]

ffSL_pred_data_emp_scaled$hit_angle <- (ffSL_pred_data_emp_scaled$hit_angle - ffSL_center_values[3]) / ffSL_scale_values[3]

ffSL_pred_prob_hit <- predict(ffSL_rf.5, ffSL_pred_data_emp_scaled, type = "prob")[,2]
ffSL_pred_prob_hit_fits <- cbind(ffSL_pred_data_emp, ffSL_pred_prob_hit)

ffSL_pred_prob_hit_fits[,c(1:2)] <- ffSL_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

ffSL_angle_speed_pred <- ffSL_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = ffSL_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nFF-SL Hit Probability\n") + theme_bp_grey()

ffSL_angle_speed_pred

############################
#####	Slider - Fastball

slFF$hit_distance_sc <- as.numeric(slFF$hit_distance_sc, na.rm = TRUE)
slFF$hit_angle <- as.numeric(slFF$hit_angle, na.rm = TRUE)
slFF$hit_speed <- as.numeric(slFF$hit_speed, na.rm = TRUE)

slFF$hit <- with(slFF, ifelse(grepl("Single", slFF$events), 1,
															ifelse(grepl("Double", slFF$events), 1,
																		 ifelse(grepl("Triple", slFF$events), 1, 
																		 			 ifelse(grepl("Home Run", slFF$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

slFF$fieldingTeam <- with(slFF, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[slFF$hit_distance_sc == "null"] = NA
#slFF$hit_speed[slFF$hit_speed == "null"] = NA
#slFF$hit_angle[slFF$hit_angle == "null"] = NA

# include row names for unique record identification

slFF$row <- row.names(slFF) %>% as.numeric()

# recode stand and home_team as factors

slFF$stand <- as.factor(slFF$stand)
slFF$home_team <- as.factor(slFF$home_team)


slFF$game_date <- as.Date(slFF$game_date)


# subset 

slFF_working_data <- ungroup(slFF) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(slFF_working_data)
str(ungroup(slFF))
table(slFF_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

slFF_working_data <- filter(slFF_working_data, hc_x != 1, hc_y != 1)
head(slFF_working_data)

# create training and test sets
# scaled data

set.seed(42)
slFF_train <- sample_frac(slFF_working_data, .15, replace = FALSE)
slFF_split <- setdiff(slFF_working_data, slFF_train)
slFF_test <- sample_frac(slFF_split, .50, replace = FALSE)
slFF_validate <- setdiff(slFF_split, slFF_test)

nrow(slFF_train) + nrow(slFF_test) + nrow(slFF_validate) == nrow(slFF_working_data)

with(slFF_train, table(hit)) %>% prop.table()
with(slFF_test, table(hit)) %>% prop.table()
with(slFF_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(slFF_train)

##########################################################################################
str(slFF)
##as.numeric(slFF$hit_distance_sc, slFF$hit_speed, slFF$hit_angle)

#slFF_train$hit_distance_sc <- as.numeric(slFF_train$hit_distance_sc)
#slFF_train$hit_speed <- as.numeric(slFF_train$hit_speed)
#slFF_train$hit_angle <- as.numeric(slFF_train$hit_angle)
#View(slFF_train)
slFF_scaled_data <- scale(slFF_train[,c(1:5)])
slFF_scale_values <- attr(slFF_scaled_data, 'scaled:scale')
slFF_scale_values
##########################################################################################
slFF_center_values <- attr(slFF_scaled_data, 'scaled:center')
slFF_center_values
##########################################################################################
slFF_train <- cbind(slFF_scaled_data, select(slFF_train, hit:row))

# save levels for factor variables
slFF_levels_home_team <- levels(slFF_train$home_team)
slFF_levels_stand <- levels(slFF_train$stand)
slFF_levels_fieldingTeam <- levels(slFF_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(slFF_test)

slFF_test$hit_distance_sc <- (slFF_test$hit_distance_sc - slFF_center_values[1]) / slFF_scale_values[1]

slFF_test$hit_speed <- (slFF_test$hit_speed - slFF_center_values[2]) / slFF_scale_values[2]

slFF_test$hit_angle <- (slFF_test$hit_angle - slFF_center_values[3]) / slFF_scale_values[3]

slFF_test$hc_x <- (slFF_test$hc_x - slFF_center_values[4]) / slFF_scale_values[4]

slFF_test$hc_y <- (slFF_test$hc_y - slFF_center_values[5]) / slFF_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

slFF_validate$hit_distance_sc <- (slFF_validate$hit_distance_sc - slFF_center_values[1]) / slFF_scale_values[1]

slFF_validate$hit_speed <- (slFF_validate$hit_speed - slFF_center_values[2]) / slFF_scale_values[2]

slFF_validate$hit_angle <- (slFF_validate$hit_angle - slFF_center_values[3]) / slFF_scale_values[3]

slFF_validate$hc_x <- (slFF_validate$hc_x - slFF_center_values[4]) / slFF_scale_values[4]

slFF_validate$hc_y <- (slFF_validate$hc_y - slFF_center_values[5]) / slFF_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(slFF_train)

set.seed(42)
slFF_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(slFF_train, -row), ntree = 501, importance = TRUE)

print(slFF_rf.1)

plot(slFF_rf.1)

varImpPlot(slFF_rf.1)

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
slFF_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(slFF_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(slFF_rf.5)

plot(slFF_rf.5)

varImpPlot(slFF_rf.5)

slFF_predict_fit.rf.5 <- data.frame(fits = predict(slFF_rf.5, slFF_test, type = "prob")[,2], actuals = slFF_test$hit)

slFF_pred.rf.5 <- prediction(slFF_predict_fit.rf.5$fits, slFF_predict_fit.rf.5$actuals)

slFF_roc.pred.rf.5 <- performance(slFF_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(slFF_roc.pred.rf.5)
abline(a = 0, b = 1)
slFF_opt <- opt.cut(slFF_roc.pred.rf.5, slFF_pred.rf.5)
slFF_opt
slFF_opt <- slFF_opt[3]
slFF_predict_fit.rf.5$fits <- with(slFF_predict_fit.rf.5, ifelse(fits > slFF_opt, 1, 0)) 

str(slFF_test)

#slFF_test$fieldingTeam <- as.character(slFF_test$fieldingTeam)
#slFF_test$home_team <- as.character(slFF_test$home_team)
#slFF_test$hit <- as.logical(slFF_test$hit)
#slFF_test$stand <- as.logical(slFF_test$stand)
str(slFF_test)
slFF_rf.5_confusion_test <- confusionMatrix(model = slFF_rf.5, x = slFF_test, y = slFF_test$hit)
slFF_rf.5_confusion_validate <- confusionMatrix(model = slFF_rf.5, x = slFF_validate, y = slFF_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


slFF_test_rows <- slFF_test$row
slFF_validate_rows <- slFF_validate$row
slFF_pred_data_emp_1 <- ungroup(slFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slFF_test_rows)

slFF_pred_data_emp <- ungroup(slFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slFF_validate_rows) %>%
	rbind(slFF_pred_data_emp_1)

slFF_out_of_training_rows <- slFF_pred_data_emp$row

slFF_rf.5_full_out_of_sample_data <- ungroup(slFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slFF_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



slFF_rf.5_full_out_of_sample_data_scaled <- slFF_rf.5_full_out_of_sample_data

slFF_rf.5_full_out_of_sample_data_scaled$hit_speed <- (slFF_rf.5_full_out_of_sample_data_scaled$hit_speed - slFF_center_values[2]) / slFF_scale_values[2]

slFF_rf.5_full_out_of_sample_data_scaled$hit_angle <- (slFF_rf.5_full_out_of_sample_data_scaled$hit_angle - slFF_center_values[3]) / slFF_scale_values[3]

slFF_rf.5.prob <- predict(slFF_rf.5, slFF_rf.5_full_out_of_sample_data_scaled, type = "response")

slFF_rf.5_full_out_of_sample_data <- cbind(filter(slFF_rf.5_full_out_of_sample_data, row %in% slFF_out_of_training_rows), slFF_rf.5.prob)

names(slFF_rf.5_full_out_of_sample_data)[65] <- "fits"
names(slFF_rf.5_full_out_of_sample_data)

slFF_rf.5_full_out_of_sample_data_reduced <- slFF_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

slFF_rf.5_mean_table <- slFF_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

slFF_rf.5_full_out_of_sample_data$type <- with(slFF_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

slFF_rf.5_full_out_of_sample_data$hit_label <- with(slFF_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

slFF_rf.5_plot1 <- ggplot(slFF_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

slFF_rf.5_plot1

slFF_rf.5_plot2 <- ggplot(slFF_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

slFF_rf.5_plot2

slFF_rf.5_plot3 <- ggplot(slFF_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

slFF_rf.5_plot3

slFF_grid_plot_rf.5 <- grid.arrange(slFF_rf.5_plot1, slFF_rf.5_plot2, slFF_rf.5_plot3, ncol = 3)
slFF_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

slFF_test_rows <- slFF_test$row
slFF_validate_rows <- slFF_validate$row
slFF_pred_data_emp_1 <- ungroup(slFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slFF_test_rows)

slFF_pred_data_emp <- ungroup(slFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slFF_validate_rows) %>%
	rbind(slFF_pred_data_emp_1)

slFF_out_of_training_rows <- slFF_pred_data_emp$row

slFF_pred_data_emp <- ungroup(slFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slFF_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

slFF_pred_data_emp_scaled <- slFF_pred_data_emp

slFF_pred_data_emp_scaled$hit_speed <- (slFF_pred_data_emp_scaled$hit_speed - slFF_center_values[2]) / slFF_scale_values[2]

slFF_pred_data_emp_scaled$hit_angle <- (slFF_pred_data_emp_scaled$hit_angle - slFF_center_values[3]) / slFF_scale_values[3]

slFF_pred_prob_hit <- predict(slFF_rf.5, slFF_pred_data_emp_scaled, type = "prob")[,2]
slFF_pred_prob_hit_fits <- cbind(slFF_pred_data_emp, slFF_pred_prob_hit)

slFF_pred_prob_hit_fits[,c(1:2)] <- slFF_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

slFF_angle_speed_pred <- slFF_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = slFF_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nSL-FF Hit Probability\n") + theme_bp_grey()

slFF_angle_speed_pred

#################
#####	Fastball - ChangeUp
ffCH$hit_distance_sc <- as.numeric(ffCH$hit_distance_sc, na.rm = TRUE)
ffCH$hit_angle <- as.numeric(ffCH$hit_angle, na.rm = TRUE)
ffCH$hit_speed <- as.numeric(ffCH$hit_speed, na.rm = TRUE)

ffCH$hit <- with(ffCH, ifelse(grepl("Single", ffCH$events), 1,
															ifelse(grepl("Double", ffCH$events), 1,
																		 ifelse(grepl("Triple", ffCH$events), 1, 
																		 			 ifelse(grepl("Home Run", ffCH$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

ffCH$fieldingTeam <- with(ffCH, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[ffCH$hit_distance_sc == "null"] = NA
#ffCH$hit_speed[ffCH$hit_speed == "null"] = NA
#ffCH$hit_angle[ffCH$hit_angle == "null"] = NA

# include row names for unique record identification

ffCH$row <- row.names(ffCH) %>% as.numeric()

# recode stand and home_team as factors

ffCH$stand <- as.factor(ffCH$stand)
ffCH$home_team <- as.factor(ffCH$home_team)


ffCH$game_date <- as.Date(ffCH$game_date)


# subset 

ffCH_working_data <- ungroup(ffCH) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(ffCH_working_data)
str(ungroup(ffCH))
table(ffCH_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

ffCH_working_data <- filter(ffCH_working_data, hc_x != 1, hc_y != 1)
head(ffCH_working_data)

# create training and test sets
# scaled data

set.seed(42)
ffCH_train <- sample_frac(ffCH_working_data, .15, replace = FALSE)
ffCH_split <- setdiff(ffCH_working_data, ffCH_train)
ffCH_test <- sample_frac(ffCH_split, .50, replace = FALSE)
ffCH_validate <- setdiff(ffCH_split, ffCH_test)

nrow(ffCH_train) + nrow(ffCH_test) + nrow(ffCH_validate) == nrow(ffCH_working_data)

with(ffCH_train, table(hit)) %>% prop.table()
with(ffCH_test, table(hit)) %>% prop.table()
with(ffCH_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(ffCH_train)

##########################################################################################
str(ffCH)
##as.numeric(ffCH$hit_distance_sc, ffCH$hit_speed, ffCH$hit_angle)

#ffCH_train$hit_distance_sc <- as.numeric(ffCH_train$hit_distance_sc)
#ffCH_train$hit_speed <- as.numeric(ffCH_train$hit_speed)
#ffCH_train$hit_angle <- as.numeric(ffCH_train$hit_angle)
#View(ffCH_train)
ffCH_scaled_data <- scale(ffCH_train[,c(1:5)])
ffCH_scale_values <- attr(ffCH_scaled_data, 'scaled:scale')
ffCH_scale_values
##########################################################################################
ffCH_center_values <- attr(ffCH_scaled_data, 'scaled:center')
ffCH_center_values
##########################################################################################
ffCH_train <- cbind(ffCH_scaled_data, select(ffCH_train, hit:row))

# save levels for factor variables
ffCH_levels_home_team <- levels(ffCH_train$home_team)
ffCH_levels_stand <- levels(ffCH_train$stand)
ffCH_levels_fieldingTeam <- levels(ffCH_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(ffCH_test)

ffCH_test$hit_distance_sc <- (ffCH_test$hit_distance_sc - ffCH_center_values[1]) / ffCH_scale_values[1]

ffCH_test$hit_speed <- (ffCH_test$hit_speed - ffCH_center_values[2]) / ffCH_scale_values[2]

ffCH_test$hit_angle <- (ffCH_test$hit_angle - ffCH_center_values[3]) / ffCH_scale_values[3]

ffCH_test$hc_x <- (ffCH_test$hc_x - ffCH_center_values[4]) / ffCH_scale_values[4]

ffCH_test$hc_y <- (ffCH_test$hc_y - ffCH_center_values[5]) / ffCH_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

ffCH_validate$hit_distance_sc <- (ffCH_validate$hit_distance_sc - ffCH_center_values[1]) / ffCH_scale_values[1]

ffCH_validate$hit_speed <- (ffCH_validate$hit_speed - ffCH_center_values[2]) / ffCH_scale_values[2]

ffCH_validate$hit_angle <- (ffCH_validate$hit_angle - ffCH_center_values[3]) / ffCH_scale_values[3]

ffCH_validate$hc_x <- (ffCH_validate$hc_x - ffCH_center_values[4]) / ffCH_scale_values[4]

ffCH_validate$hc_y <- (ffCH_validate$hc_y - ffCH_center_values[5]) / ffCH_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(ffCH_train)

set.seed(42)
ffCH_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(ffCH_train, -row), ntree = 501, importance = TRUE)

print(ffCH_rf.1)

plot(ffCH_rf.1)

varImpPlot(ffCH_rf.1)

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
ffCH_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(ffCH_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(ffCH_rf.5)

plot(ffCH_rf.5)

varImpPlot(ffCH_rf.5)

ffCH_predict_fit.rf.5 <- data.frame(fits = predict(ffCH_rf.5, ffCH_test, type = "prob")[,2], actuals = ffCH_test$hit)

ffCH_pred.rf.5 <- prediction(ffCH_predict_fit.rf.5$fits, ffCH_predict_fit.rf.5$actuals)

ffCH_roc.pred.rf.5 <- performance(ffCH_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(ffCH_roc.pred.rf.5)
abline(a = 0, b = 1)
ffCH_opt <- opt.cut(ffCH_roc.pred.rf.5, ffCH_pred.rf.5)
ffCH_opt
ffCH_opt <- ffCH_opt[3]
ffCH_predict_fit.rf.5$fits <- with(ffCH_predict_fit.rf.5, ifelse(fits > ffCH_opt, 1, 0)) 

str(ffCH_test)

#ffCH_test$fieldingTeam <- as.character(ffCH_test$fieldingTeam)
#ffCH_test$home_team <- as.character(ffCH_test$home_team)
#ffCH_test$hit <- as.logical(ffCH_test$hit)
#ffCH_test$stand <- as.logical(ffCH_test$stand)
str(ffCH_test)
ffCH_rf.5_confusion_test <- confusionMatrix(model = ffCH_rf.5, x = ffCH_test, y = ffCH_test$hit)
ffCH_rf.5_confusion_validate <- confusionMatrix(model = ffCH_rf.5, x = ffCH_validate, y = ffCH_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


ffCH_test_rows <- ffCH_test$row
ffCH_validate_rows <- ffCH_validate$row
ffCH_pred_data_emp_1 <- ungroup(ffCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffCH_test_rows)

ffCH_pred_data_emp <- ungroup(ffCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffCH_validate_rows) %>%
	rbind(ffCH_pred_data_emp_1)

ffCH_out_of_training_rows <- ffCH_pred_data_emp$row

ffCH_rf.5_full_out_of_sample_data <- ungroup(ffCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffCH_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



ffCH_rf.5_full_out_of_sample_data_scaled <- ffCH_rf.5_full_out_of_sample_data

ffCH_rf.5_full_out_of_sample_data_scaled$hit_speed <- (ffCH_rf.5_full_out_of_sample_data_scaled$hit_speed - ffCH_center_values[2]) / ffCH_scale_values[2]

ffCH_rf.5_full_out_of_sample_data_scaled$hit_angle <- (ffCH_rf.5_full_out_of_sample_data_scaled$hit_angle - ffCH_center_values[3]) / ffCH_scale_values[3]

ffCH_rf.5.prob <- predict(ffCH_rf.5, ffCH_rf.5_full_out_of_sample_data_scaled, type = "response")

ffCH_rf.5_full_out_of_sample_data <- cbind(filter(ffCH_rf.5_full_out_of_sample_data, row %in% ffCH_out_of_training_rows), ffCH_rf.5.prob)

names(ffCH_rf.5_full_out_of_sample_data)[65] <- "fits"
names(ffCH_rf.5_full_out_of_sample_data)

ffCH_rf.5_full_out_of_sample_data_reduced <- ffCH_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

ffCH_rf.5_mean_table <- ffCH_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

ffCH_rf.5_full_out_of_sample_data$type <- with(ffCH_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

ffCH_rf.5_full_out_of_sample_data$hit_label <- with(ffCH_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

ffCH_rf.5_plot1 <- ggplot(ffCH_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

ffCH_rf.5_plot1

ffCH_rf.5_plot2 <- ggplot(ffCH_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

ffCH_rf.5_plot2

ffCH_rf.5_plot3 <- ggplot(ffCH_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

ffCH_rf.5_plot3

ffCH_grid_plot_rf.5 <- grid.arrange(ffCH_rf.5_plot1, ffCH_rf.5_plot2, ffCH_rf.5_plot3, ncol = 3)
ffCH_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

ffCH_test_rows <- ffCH_test$row
ffCH_validate_rows <- ffCH_validate$row
ffCH_pred_data_emp_1 <- ungroup(ffCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffCH_test_rows)

ffCH_pred_data_emp <- ungroup(ffCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffCH_validate_rows) %>%
	rbind(ffCH_pred_data_emp_1)

ffCH_out_of_training_rows <- ffCH_pred_data_emp$row

ffCH_pred_data_emp <- ungroup(ffCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffCH_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

ffCH_pred_data_emp_scaled <- ffCH_pred_data_emp

ffCH_pred_data_emp_scaled$hit_speed <- (ffCH_pred_data_emp_scaled$hit_speed - ffCH_center_values[2]) / ffCH_scale_values[2]

ffCH_pred_data_emp_scaled$hit_angle <- (ffCH_pred_data_emp_scaled$hit_angle - ffCH_center_values[3]) / ffCH_scale_values[3]

ffCH_pred_prob_hit <- predict(ffCH_rf.5, ffCH_pred_data_emp_scaled, type = "prob")[,2]
ffCH_pred_prob_hit_fits <- cbind(ffCH_pred_data_emp, ffCH_pred_prob_hit)

ffCH_pred_prob_hit_fits[,c(1:2)] <- ffCH_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

ffCH_angle_speed_pred <- ffCH_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = ffCH_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nFF-CH Hit Probability\n") + theme_bp_grey()

ffCH_angle_speed_pred


####################
###### ChangeUp - Fastball
chFF$hit_distance_sc <- as.numeric(chFF$hit_distance_sc, na.rm = TRUE)
chFF$hit_angle <- as.numeric(chFF$hit_angle, na.rm = TRUE)
chFF$hit_speed <- as.numeric(chFF$hit_speed, na.rm = TRUE)

chFF$hit <- with(chFF, ifelse(grepl("Single", chFF$events), 1,
															ifelse(grepl("Double", chFF$events), 1,
																		 ifelse(grepl("Triple", chFF$events), 1, 
																		 			 ifelse(grepl("Home Run", chFF$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

chFF$fieldingTeam <- with(chFF, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[chFF$hit_distance_sc == "null"] = NA
#chFF$hit_speed[chFF$hit_speed == "null"] = NA
#chFF$hit_angle[chFF$hit_angle == "null"] = NA

# include row names for unique record identification

chFF$row <- row.names(chFF) %>% as.numeric()

# recode stand and home_team as factors

chFF$stand <- as.factor(chFF$stand)
chFF$home_team <- as.factor(chFF$home_team)


chFF$game_date <- as.Date(chFF$game_date)


# subset 

chFF_working_data <- ungroup(chFF) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(chFF_working_data)
str(ungroup(chFF))
table(chFF_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

chFF_working_data <- filter(chFF_working_data, hc_x != 1, hc_y != 1)
head(chFF_working_data)

# create training and test sets
# scaled data

set.seed(42)
chFF_train <- sample_frac(chFF_working_data, .15, replace = FALSE)
chFF_split <- setdiff(chFF_working_data, chFF_train)
chFF_test <- sample_frac(chFF_split, .50, replace = FALSE)
chFF_validate <- setdiff(chFF_split, chFF_test)

nrow(chFF_train) + nrow(chFF_test) + nrow(chFF_validate) == nrow(chFF_working_data)

with(chFF_train, table(hit)) %>% prop.table()
with(chFF_test, table(hit)) %>% prop.table()
with(chFF_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(chFF_train)

##########################################################################################
str(chFF)
##as.numeric(chFF$hit_distance_sc, chFF$hit_speed, chFF$hit_angle)

#chFF_train$hit_distance_sc <- as.numeric(chFF_train$hit_distance_sc)
#chFF_train$hit_speed <- as.numeric(chFF_train$hit_speed)
#chFF_train$hit_angle <- as.numeric(chFF_train$hit_angle)
#View(chFF_train)
chFF_scaled_data <- scale(chFF_train[,c(1:5)])
chFF_scale_values <- attr(chFF_scaled_data, 'scaled:scale')
chFF_scale_values
##########################################################################################
chFF_center_values <- attr(chFF_scaled_data, 'scaled:center')
chFF_center_values
##########################################################################################
chFF_train <- cbind(chFF_scaled_data, select(chFF_train, hit:row))

# save levels for factor variables
chFF_levels_home_team <- levels(chFF_train$home_team)
chFF_levels_stand <- levels(chFF_train$stand)
chFF_levels_fieldingTeam <- levels(chFF_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(chFF_test)

chFF_test$hit_distance_sc <- (chFF_test$hit_distance_sc - chFF_center_values[1]) / chFF_scale_values[1]

chFF_test$hit_speed <- (chFF_test$hit_speed - chFF_center_values[2]) / chFF_scale_values[2]

chFF_test$hit_angle <- (chFF_test$hit_angle - chFF_center_values[3]) / chFF_scale_values[3]

chFF_test$hc_x <- (chFF_test$hc_x - chFF_center_values[4]) / chFF_scale_values[4]

chFF_test$hc_y <- (chFF_test$hc_y - chFF_center_values[5]) / chFF_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

chFF_validate$hit_distance_sc <- (chFF_validate$hit_distance_sc - chFF_center_values[1]) / chFF_scale_values[1]

chFF_validate$hit_speed <- (chFF_validate$hit_speed - chFF_center_values[2]) / chFF_scale_values[2]

chFF_validate$hit_angle <- (chFF_validate$hit_angle - chFF_center_values[3]) / chFF_scale_values[3]

chFF_validate$hc_x <- (chFF_validate$hc_x - chFF_center_values[4]) / chFF_scale_values[4]

chFF_validate$hc_y <- (chFF_validate$hc_y - chFF_center_values[5]) / chFF_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(chFF_train)

set.seed(42)
chFF_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(chFF_train, -row), ntree = 501, importance = TRUE)

print(chFF_rf.1)

plot(chFF_rf.1)

varImpPlot(chFF_rf.1)

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
chFF_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(chFF_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(chFF_rf.5)

plot(chFF_rf.5)

varImpPlot(chFF_rf.5)

chFF_predict_fit.rf.5 <- data.frame(fits = predict(chFF_rf.5, chFF_test, type = "prob")[,2], actuals = chFF_test$hit)

chFF_pred.rf.5 <- prediction(chFF_predict_fit.rf.5$fits, chFF_predict_fit.rf.5$actuals)

chFF_roc.pred.rf.5 <- performance(chFF_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(chFF_roc.pred.rf.5)
abline(a = 0, b = 1)
chFF_opt <- opt.cut(chFF_roc.pred.rf.5, chFF_pred.rf.5)
chFF_opt
chFF_opt <- chFF_opt[3]
chFF_predict_fit.rf.5$fits <- with(chFF_predict_fit.rf.5, ifelse(fits > chFF_opt, 1, 0)) 

str(chFF_test)

#chFF_test$fieldingTeam <- as.character(chFF_test$fieldingTeam)
#chFF_test$home_team <- as.character(chFF_test$home_team)
#chFF_test$hit <- as.logical(chFF_test$hit)
#chFF_test$stand <- as.logical(chFF_test$stand)
str(chFF_test)
chFF_rf.5_confusion_test <- confusionMatrix(model = chFF_rf.5, x = chFF_test, y = chFF_test$hit)
chFF_rf.5_confusion_validate <- confusionMatrix(model = chFF_rf.5, x = chFF_validate, y = chFF_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


chFF_test_rows <- chFF_test$row
chFF_validate_rows <- chFF_validate$row
chFF_pred_data_emp_1 <- ungroup(chFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chFF_test_rows)

chFF_pred_data_emp <- ungroup(chFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chFF_validate_rows) %>%
	rbind(chFF_pred_data_emp_1)

chFF_out_of_training_rows <- chFF_pred_data_emp$row

chFF_rf.5_full_out_of_sample_data <- ungroup(chFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chFF_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



chFF_rf.5_full_out_of_sample_data_scaled <- chFF_rf.5_full_out_of_sample_data

chFF_rf.5_full_out_of_sample_data_scaled$hit_speed <- (chFF_rf.5_full_out_of_sample_data_scaled$hit_speed - chFF_center_values[2]) / chFF_scale_values[2]

chFF_rf.5_full_out_of_sample_data_scaled$hit_angle <- (chFF_rf.5_full_out_of_sample_data_scaled$hit_angle - chFF_center_values[3]) / chFF_scale_values[3]

chFF_rf.5.prob <- predict(chFF_rf.5, chFF_rf.5_full_out_of_sample_data_scaled, type = "response")

chFF_rf.5_full_out_of_sample_data <- cbind(filter(chFF_rf.5_full_out_of_sample_data, row %in% chFF_out_of_training_rows), chFF_rf.5.prob)

names(chFF_rf.5_full_out_of_sample_data)[65] <- "fits"
names(chFF_rf.5_full_out_of_sample_data)

chFF_rf.5_full_out_of_sample_data_reduced <- chFF_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

chFF_rf.5_mean_table <- chFF_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

chFF_rf.5_full_out_of_sample_data$type <- with(chFF_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

chFF_rf.5_full_out_of_sample_data$hit_label <- with(chFF_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

chFF_rf.5_plot1 <- ggplot(chFF_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

chFF_rf.5_plot1

chFF_rf.5_plot2 <- ggplot(chFF_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

chFF_rf.5_plot2

chFF_rf.5_plot3 <- ggplot(chFF_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

chFF_rf.5_plot3

chFF_grid_plot_rf.5 <- grid.arrange(chFF_rf.5_plot1, chFF_rf.5_plot2, chFF_rf.5_plot3, ncol = 3)
chFF_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

chFF_test_rows <- chFF_test$row
chFF_validate_rows <- chFF_validate$row
chFF_pred_data_emp_1 <- ungroup(chFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chFF_test_rows)

chFF_pred_data_emp <- ungroup(chFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chFF_validate_rows) %>%
	rbind(chFF_pred_data_emp_1)

chFF_out_of_training_rows <- chFF_pred_data_emp$row

chFF_pred_data_emp <- ungroup(chFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chFF_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

chFF_pred_data_emp_scaled <- chFF_pred_data_emp

chFF_pred_data_emp_scaled$hit_speed <- (chFF_pred_data_emp_scaled$hit_speed - chFF_center_values[2]) / chFF_scale_values[2]

chFF_pred_data_emp_scaled$hit_angle <- (chFF_pred_data_emp_scaled$hit_angle - chFF_center_values[3]) / chFF_scale_values[3]

chFF_pred_prob_hit <- predict(chFF_rf.5, chFF_pred_data_emp_scaled, type = "prob")[,2]
chFF_pred_prob_hit_fits <- cbind(chFF_pred_data_emp, chFF_pred_prob_hit)

chFF_pred_prob_hit_fits[,c(1:2)] <- chFF_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

chFF_angle_speed_pred <- chFF_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = chFF_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nCH-FF Hit Probability\n") + theme_bp_grey()

chFF_angle_speed_pred


##################
####### Fastball - Curveball

ffCU$hit_distance_sc <- as.numeric(ffCU$hit_distance_sc, na.rm = TRUE)
ffCU$hit_angle <- as.numeric(ffCU$hit_angle, na.rm = TRUE)
ffCU$hit_speed <- as.numeric(ffCU$hit_speed, na.rm = TRUE)

ffCU$hit <- with(ffCU, ifelse(grepl("Single", ffCU$events), 1,
															ifelse(grepl("Double", ffCU$events), 1,
																		 ifelse(grepl("Triple", ffCU$events), 1, 
																		 			 ifelse(grepl("Home Run", ffCU$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

ffCU$fieldingTeam <- with(ffCU, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[ffCU$hit_distance_sc == "null"] = NA
#ffCU$hit_speed[ffCU$hit_speed == "null"] = NA
#ffCU$hit_angle[ffCU$hit_angle == "null"] = NA

# include row names for unique record identification

ffCU$row <- row.names(ffCU) %>% as.numeric()

# recode stand and home_team as factors

ffCU$stand <- as.factor(ffCU$stand)
ffCU$home_team <- as.factor(ffCU$home_team)


ffCU$game_date <- as.Date(ffCU$game_date)


# subset 

ffCU_working_data <- ungroup(ffCU) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(ffCU_working_data)
str(ungroup(ffCU))
table(ffCU_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

ffCU_working_data <- filter(ffCU_working_data, hc_x != 1, hc_y != 1)
head(ffCU_working_data)

# create training and test sets
# scaled data

set.seed(42)
ffCU_train <- sample_frac(ffCU_working_data, .15, replace = FALSE)
ffCU_split <- setdiff(ffCU_working_data, ffCU_train)
ffCU_test <- sample_frac(ffCU_split, .50, replace = FALSE)
ffCU_validate <- setdiff(ffCU_split, ffCU_test)

nrow(ffCU_train) + nrow(ffCU_test) + nrow(ffCU_validate) == nrow(ffCU_working_data)

with(ffCU_train, table(hit)) %>% prop.table()
with(ffCU_test, table(hit)) %>% prop.table()
with(ffCU_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(ffCU_train)

##########################################################################################
str(ffCU)
##as.numeric(ffCU$hit_distance_sc, ffCU$hit_speed, ffCU$hit_angle)

#ffCU_train$hit_distance_sc <- as.numeric(ffCU_train$hit_distance_sc)
#ffCU_train$hit_speed <- as.numeric(ffCU_train$hit_speed)
#ffCU_train$hit_angle <- as.numeric(ffCU_train$hit_angle)
#View(ffCU_train)
ffCU_scaled_data <- scale(ffCU_train[,c(1:5)])
ffCU_scale_values <- attr(ffCU_scaled_data, 'scaled:scale')
ffCU_scale_values
##########################################################################################
ffCU_center_values <- attr(ffCU_scaled_data, 'scaled:center')
ffCU_center_values
##########################################################################################
ffCU_train <- cbind(ffCU_scaled_data, select(ffCU_train, hit:row))

# save levels for factor variables
ffCU_levels_home_team <- levels(ffCU_train$home_team)
ffCU_levels_stand <- levels(ffCU_train$stand)
ffCU_levels_fieldingTeam <- levels(ffCU_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(ffCU_test)

ffCU_test$hit_distance_sc <- (ffCU_test$hit_distance_sc - ffCU_center_values[1]) / ffCU_scale_values[1]

ffCU_test$hit_speed <- (ffCU_test$hit_speed - ffCU_center_values[2]) / ffCU_scale_values[2]

ffCU_test$hit_angle <- (ffCU_test$hit_angle - ffCU_center_values[3]) / ffCU_scale_values[3]

ffCU_test$hc_x <- (ffCU_test$hc_x - ffCU_center_values[4]) / ffCU_scale_values[4]

ffCU_test$hc_y <- (ffCU_test$hc_y - ffCU_center_values[5]) / ffCU_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

ffCU_validate$hit_distance_sc <- (ffCU_validate$hit_distance_sc - ffCU_center_values[1]) / ffCU_scale_values[1]

ffCU_validate$hit_speed <- (ffCU_validate$hit_speed - ffCU_center_values[2]) / ffCU_scale_values[2]

ffCU_validate$hit_angle <- (ffCU_validate$hit_angle - ffCU_center_values[3]) / ffCU_scale_values[3]

ffCU_validate$hc_x <- (ffCU_validate$hc_x - ffCU_center_values[4]) / ffCU_scale_values[4]

ffCU_validate$hc_y <- (ffCU_validate$hc_y - ffCU_center_values[5]) / ffCU_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(ffCU_train)

set.seed(42)
ffCU_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(ffCU_train, -row), ntree = 501, importance = TRUE)

print(ffCU_rf.1)

plot(ffCU_rf.1)

varImpPlot(ffCU_rf.1)

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
ffCU_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(ffCU_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(ffCU_rf.5)

plot(ffCU_rf.5)

varImpPlot(ffCU_rf.5)

ffCU_predict_fit.rf.5 <- data.frame(fits = predict(ffCU_rf.5, ffCU_test, type = "prob")[,2], actuals = ffCU_test$hit)

ffCU_pred.rf.5 <- prediction(ffCU_predict_fit.rf.5$fits, ffCU_predict_fit.rf.5$actuals)

ffCU_roc.pred.rf.5 <- performance(ffCU_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(ffCU_roc.pred.rf.5)
abline(a = 0, b = 1)
ffCU_opt <- opt.cut(ffCU_roc.pred.rf.5, ffCU_pred.rf.5)
ffCU_opt
ffCU_opt <- ffCU_opt[3]
ffCU_predict_fit.rf.5$fits <- with(ffCU_predict_fit.rf.5, ifelse(fits > ffCU_opt, 1, 0)) 

str(ffCU_test)

#ffCU_test$fieldingTeam <- as.character(ffCU_test$fieldingTeam)
#ffCU_test$home_team <- as.character(ffCU_test$home_team)
#ffCU_test$hit <- as.logical(ffCU_test$hit)
#ffCU_test$stand <- as.logical(ffCU_test$stand)
str(ffCU_test)
ffCU_rf.5_confusion_test <- confusionMatrix(model = ffCU_rf.5, x = ffCU_test, y = ffCU_test$hit)
ffCU_rf.5_confusion_validate <- confusionMatrix(model = ffCU_rf.5, x = ffCU_validate, y = ffCU_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


ffCU_test_rows <- ffCU_test$row
ffCU_validate_rows <- ffCU_validate$row
ffCU_pred_data_emp_1 <- ungroup(ffCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffCU_test_rows)

ffCU_pred_data_emp <- ungroup(ffCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffCU_validate_rows) %>%
	rbind(ffCU_pred_data_emp_1)

ffCU_out_of_training_rows <- ffCU_pred_data_emp$row

ffCU_rf.5_full_out_of_sample_data <- ungroup(ffCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffCU_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



ffCU_rf.5_full_out_of_sample_data_scaled <- ffCU_rf.5_full_out_of_sample_data

ffCU_rf.5_full_out_of_sample_data_scaled$hit_speed <- (ffCU_rf.5_full_out_of_sample_data_scaled$hit_speed - ffCU_center_values[2]) / ffCU_scale_values[2]

ffCU_rf.5_full_out_of_sample_data_scaled$hit_angle <- (ffCU_rf.5_full_out_of_sample_data_scaled$hit_angle - ffCU_center_values[3]) / ffCU_scale_values[3]

ffCU_rf.5.prob <- predict(ffCU_rf.5, ffCU_rf.5_full_out_of_sample_data_scaled, type = "response")

ffCU_rf.5_full_out_of_sample_data <- cbind(filter(ffCU_rf.5_full_out_of_sample_data, row %in% ffCU_out_of_training_rows), ffCU_rf.5.prob)

names(ffCU_rf.5_full_out_of_sample_data)[65] <- "fits"
names(ffCU_rf.5_full_out_of_sample_data)

ffCU_rf.5_full_out_of_sample_data_reduced <- ffCU_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

ffCU_rf.5_mean_table <- ffCU_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

ffCU_rf.5_full_out_of_sample_data$type <- with(ffCU_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

ffCU_rf.5_full_out_of_sample_data$hit_label <- with(ffCU_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

ffCU_rf.5_plot1 <- ggplot(ffCU_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

ffCU_rf.5_plot1

ffCU_rf.5_plot2 <- ggplot(ffCU_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

ffCU_rf.5_plot2

ffCU_rf.5_plot3 <- ggplot(ffCU_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

ffCU_rf.5_plot3

ffCU_grid_plot_rf.5 <- grid.arrange(ffCU_rf.5_plot1, ffCU_rf.5_plot2, ffCU_rf.5_plot3, ncol = 3)
ffCU_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

ffCU_test_rows <- ffCU_test$row
ffCU_validate_rows <- ffCU_validate$row
ffCU_pred_data_emp_1 <- ungroup(ffCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffCU_test_rows)

ffCU_pred_data_emp <- ungroup(ffCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffCU_validate_rows) %>%
	rbind(ffCU_pred_data_emp_1)

ffCU_out_of_training_rows <- ffCU_pred_data_emp$row

ffCU_pred_data_emp <- ungroup(ffCU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffCU_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

ffCU_pred_data_emp_scaled <- ffCU_pred_data_emp

ffCU_pred_data_emp_scaled$hit_speed <- (ffCU_pred_data_emp_scaled$hit_speed - ffCU_center_values[2]) / ffCU_scale_values[2]

ffCU_pred_data_emp_scaled$hit_angle <- (ffCU_pred_data_emp_scaled$hit_angle - ffCU_center_values[3]) / ffCU_scale_values[3]

ffCU_pred_prob_hit <- predict(ffCU_rf.5, ffCU_pred_data_emp_scaled, type = "prob")[,2]
ffCU_pred_prob_hit_fits <- cbind(ffCU_pred_data_emp, ffCU_pred_prob_hit)

ffCU_pred_prob_hit_fits[,c(1:2)] <- ffCU_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

ffCU_angle_speed_pred <- ffCU_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = ffCU_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nFF-CU Hit Probability\n") + theme_bp_grey()

ffCU_angle_speed_pred

#################
###### Curveball - Fastball
cuFF$hit_distance_sc <- as.numeric(cuFF$hit_distance_sc, na.rm = TRUE)
cuFF$hit_angle <- as.numeric(cuFF$hit_angle, na.rm = TRUE)
cuFF$hit_speed <- as.numeric(cuFF$hit_speed, na.rm = TRUE)

cuFF$hit <- with(cuFF, ifelse(grepl("Single", cuFF$events), 1,
															ifelse(grepl("Double", cuFF$events), 1,
																		 ifelse(grepl("Triple", cuFF$events), 1, 
																		 			 ifelse(grepl("Home Run", cuFF$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

cuFF$fieldingTeam <- with(cuFF, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[cuFF$hit_distance_sc == "null"] = NA
#cuFF$hit_speed[cuFF$hit_speed == "null"] = NA
#cuFF$hit_angle[cuFF$hit_angle == "null"] = NA

# include row names for unique record identification

cuFF$row <- row.names(cuFF) %>% as.numeric()

# recode stand and home_team as factors

cuFF$stand <- as.factor(cuFF$stand)
cuFF$home_team <- as.factor(cuFF$home_team)


cuFF$game_date <- as.Date(cuFF$game_date)


# subset 

cuFF_working_data <- ungroup(cuFF) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(cuFF_working_data)
str(ungroup(cuFF))
table(cuFF_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

cuFF_working_data <- filter(cuFF_working_data, hc_x != 1, hc_y != 1)
head(cuFF_working_data)

# create training and test sets
# scaled data

set.seed(42)
cuFF_train <- sample_frac(cuFF_working_data, .15, replace = FALSE)
cuFF_split <- setdiff(cuFF_working_data, cuFF_train)
cuFF_test <- sample_frac(cuFF_split, .50, replace = FALSE)
cuFF_validate <- setdiff(cuFF_split, cuFF_test)

nrow(cuFF_train) + nrow(cuFF_test) + nrow(cuFF_validate) == nrow(cuFF_working_data)

with(cuFF_train, table(hit)) %>% prop.table()
with(cuFF_test, table(hit)) %>% prop.table()
with(cuFF_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(cuFF_train)

##########################################################################################
str(cuFF)
##as.numeric(cuFF$hit_distance_sc, cuFF$hit_speed, cuFF$hit_angle)

#cuFF_train$hit_distance_sc <- as.numeric(cuFF_train$hit_distance_sc)
#cuFF_train$hit_speed <- as.numeric(cuFF_train$hit_speed)
#cuFF_train$hit_angle <- as.numeric(cuFF_train$hit_angle)
#View(cuFF_train)
cuFF_scaled_data <- scale(cuFF_train[,c(1:5)])
cuFF_scale_values <- attr(cuFF_scaled_data, 'scaled:scale')
cuFF_scale_values
##########################################################################################
cuFF_center_values <- attr(cuFF_scaled_data, 'scaled:center')
cuFF_center_values
##########################################################################################
cuFF_train <- cbind(cuFF_scaled_data, select(cuFF_train, hit:row))

# save levels for factor variables
cuFF_levels_home_team <- levels(cuFF_train$home_team)
cuFF_levels_stand <- levels(cuFF_train$stand)
cuFF_levels_fieldingTeam <- levels(cuFF_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(cuFF_test)

cuFF_test$hit_distance_sc <- (cuFF_test$hit_distance_sc - cuFF_center_values[1]) / cuFF_scale_values[1]

cuFF_test$hit_speed <- (cuFF_test$hit_speed - cuFF_center_values[2]) / cuFF_scale_values[2]

cuFF_test$hit_angle <- (cuFF_test$hit_angle - cuFF_center_values[3]) / cuFF_scale_values[3]

cuFF_test$hc_x <- (cuFF_test$hc_x - cuFF_center_values[4]) / cuFF_scale_values[4]

cuFF_test$hc_y <- (cuFF_test$hc_y - cuFF_center_values[5]) / cuFF_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

cuFF_validate$hit_distance_sc <- (cuFF_validate$hit_distance_sc - cuFF_center_values[1]) / cuFF_scale_values[1]

cuFF_validate$hit_speed <- (cuFF_validate$hit_speed - cuFF_center_values[2]) / cuFF_scale_values[2]

cuFF_validate$hit_angle <- (cuFF_validate$hit_angle - cuFF_center_values[3]) / cuFF_scale_values[3]

cuFF_validate$hc_x <- (cuFF_validate$hc_x - cuFF_center_values[4]) / cuFF_scale_values[4]

cuFF_validate$hc_y <- (cuFF_validate$hc_y - cuFF_center_values[5]) / cuFF_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(cuFF_train)

set.seed(42)
cuFF_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(cuFF_train, -row), ntree = 501, importance = TRUE)

print(cuFF_rf.1)

plot(cuFF_rf.1)

varImpPlot(cuFF_rf.1)

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
cuFF_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(cuFF_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(cuFF_rf.5)

plot(cuFF_rf.5)

varImpPlot(cuFF_rf.5)

cuFF_predict_fit.rf.5 <- data.frame(fits = predict(cuFF_rf.5, cuFF_test, type = "prob")[,2], actuals = cuFF_test$hit)

cuFF_pred.rf.5 <- prediction(cuFF_predict_fit.rf.5$fits, cuFF_predict_fit.rf.5$actuals)

cuFF_roc.pred.rf.5 <- performance(cuFF_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(cuFF_roc.pred.rf.5)
abline(a = 0, b = 1)
cuFF_opt <- opt.cut(cuFF_roc.pred.rf.5, cuFF_pred.rf.5)
cuFF_opt
cuFF_opt <- cuFF_opt[3]
cuFF_predict_fit.rf.5$fits <- with(cuFF_predict_fit.rf.5, ifelse(fits > cuFF_opt, 1, 0)) 

str(cuFF_test)

#cuFF_test$fieldingTeam <- as.character(cuFF_test$fieldingTeam)
#cuFF_test$home_team <- as.character(cuFF_test$home_team)
#cuFF_test$hit <- as.logical(cuFF_test$hit)
#cuFF_test$stand <- as.logical(cuFF_test$stand)
str(cuFF_test)
cuFF_rf.5_confusion_test <- confusionMatrix(model = cuFF_rf.5, x = cuFF_test, y = cuFF_test$hit)
cuFF_rf.5_confusion_validate <- confusionMatrix(model = cuFF_rf.5, x = cuFF_validate, y = cuFF_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


cuFF_test_rows <- cuFF_test$row
cuFF_validate_rows <- cuFF_validate$row
cuFF_pred_data_emp_1 <- ungroup(cuFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuFF_test_rows)

cuFF_pred_data_emp <- ungroup(cuFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuFF_validate_rows) %>%
	rbind(cuFF_pred_data_emp_1)

cuFF_out_of_training_rows <- cuFF_pred_data_emp$row

cuFF_rf.5_full_out_of_sample_data <- ungroup(cuFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuFF_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



cuFF_rf.5_full_out_of_sample_data_scaled <- cuFF_rf.5_full_out_of_sample_data

cuFF_rf.5_full_out_of_sample_data_scaled$hit_speed <- (cuFF_rf.5_full_out_of_sample_data_scaled$hit_speed - cuFF_center_values[2]) / cuFF_scale_values[2]

cuFF_rf.5_full_out_of_sample_data_scaled$hit_angle <- (cuFF_rf.5_full_out_of_sample_data_scaled$hit_angle - cuFF_center_values[3]) / cuFF_scale_values[3]

cuFF_rf.5.prob <- predict(cuFF_rf.5, cuFF_rf.5_full_out_of_sample_data_scaled, type = "response")

cuFF_rf.5_full_out_of_sample_data <- cbind(filter(cuFF_rf.5_full_out_of_sample_data, row %in% cuFF_out_of_training_rows), cuFF_rf.5.prob)

names(cuFF_rf.5_full_out_of_sample_data)[65] <- "fits"
names(cuFF_rf.5_full_out_of_sample_data)

cuFF_rf.5_full_out_of_sample_data_reduced <- cuFF_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

cuFF_rf.5_mean_table <- cuFF_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

cuFF_rf.5_full_out_of_sample_data$type <- with(cuFF_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

cuFF_rf.5_full_out_of_sample_data$hit_label <- with(cuFF_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

cuFF_rf.5_plot1 <- ggplot(cuFF_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

cuFF_rf.5_plot1

cuFF_rf.5_plot2 <- ggplot(cuFF_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

cuFF_rf.5_plot2

cuFF_rf.5_plot3 <- ggplot(cuFF_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

cuFF_rf.5_plot3

cuFF_grid_plot_rf.5 <- grid.arrange(cuFF_rf.5_plot1, cuFF_rf.5_plot2, cuFF_rf.5_plot3, ncol = 3)
cuFF_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

cuFF_test_rows <- cuFF_test$row
cuFF_validate_rows <- cuFF_validate$row
cuFF_pred_data_emp_1 <- ungroup(cuFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuFF_test_rows)

cuFF_pred_data_emp <- ungroup(cuFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuFF_validate_rows) %>%
	rbind(cuFF_pred_data_emp_1)

cuFF_out_of_training_rows <- cuFF_pred_data_emp$row

cuFF_pred_data_emp <- ungroup(cuFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% cuFF_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

cuFF_pred_data_emp_scaled <- cuFF_pred_data_emp

cuFF_pred_data_emp_scaled$hit_speed <- (cuFF_pred_data_emp_scaled$hit_speed - cuFF_center_values[2]) / cuFF_scale_values[2]

cuFF_pred_data_emp_scaled$hit_angle <- (cuFF_pred_data_emp_scaled$hit_angle - cuFF_center_values[3]) / cuFF_scale_values[3]

cuFF_pred_prob_hit <- predict(cuFF_rf.5, cuFF_pred_data_emp_scaled, type = "prob")[,2]
cuFF_pred_prob_hit_fits <- cbind(cuFF_pred_data_emp, cuFF_pred_prob_hit)

cuFF_pred_prob_hit_fits[,c(1:2)] <- cuFF_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

cuFF_angle_speed_pred <- cuFF_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = cuFF_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nCU-FF Hit Probability\n") + theme_bp_grey()

cuFF_angle_speed_pred

###############
###### Fastball - Sinker

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
head(ffSI_train)

##########################################################################################
str(ffSI)
##as.numeric(ffSI$hit_distance_sc, ffSI$hit_speed, ffSI$hit_angle)

#ffSI_train$hit_distance_sc <- as.numeric(ffSI_train$hit_distance_sc)
#ffSI_train$hit_speed <- as.numeric(ffSI_train$hit_speed)
#ffSI_train$hit_angle <- as.numeric(ffSI_train$hit_angle)
#View(ffSI_train)
ffSI_scaled_data <- scale(ffSI_train[,c(1:5)])
ffSI_scale_values <- attr(ffSI_scaled_data, 'scaled:scale')
ffSI_scale_values
##########################################################################################
ffSI_center_values <- attr(ffSI_scaled_data, 'scaled:center')
ffSI_center_values
##########################################################################################
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


#######################
###### Slider - Sinker
slSI$hit_distance_sc <- as.numeric(slSI$hit_distance_sc, na.rm = TRUE)
slSI$hit_angle <- as.numeric(slSI$hit_angle, na.rm = TRUE)
slSI$hit_speed <- as.numeric(slSI$hit_speed, na.rm = TRUE)

slSI$hit <- with(slSI, ifelse(grepl("Single", slSI$events), 1,
															ifelse(grepl("Double", slSI$events), 1,
																		 ifelse(grepl("Triple", slSI$events), 1, 
																		 			 ifelse(grepl("Home Run", slSI$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

slSI$fieldingTeam <- with(slSI, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[slSI$hit_distance_sc == "null"] = NA
#slSI$hit_speed[slSI$hit_speed == "null"] = NA
#slSI$hit_angle[slSI$hit_angle == "null"] = NA

# include row names for unique record identification

slSI$row <- row.names(slSI) %>% as.numeric()

# recode stand and home_team as factors

slSI$stand <- as.factor(slSI$stand)
slSI$home_team <- as.factor(slSI$home_team)


slSI$game_date <- as.Date(slSI$game_date)


# subset 

slSI_working_data <- ungroup(slSI) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(slSI_working_data)
str(ungroup(slSI))
table(slSI_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

slSI_working_data <- filter(slSI_working_data, hc_x != 1, hc_y != 1)
head(slSI_working_data)

# create training and test sets
# scaled data

set.seed(42)
slSI_train <- sample_frac(slSI_working_data, .15, replace = FALSE)
slSI_split <- setdiff(slSI_working_data, slSI_train)
slSI_test <- sample_frac(slSI_split, .50, replace = FALSE)
slSI_validate <- setdiff(slSI_split, slSI_test)

nrow(slSI_train) + nrow(slSI_test) + nrow(slSI_validate) == nrow(slSI_working_data)

with(slSI_train, table(hit)) %>% prop.table()
with(slSI_test, table(hit)) %>% prop.table()
with(slSI_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(slSI_train)

##########################################################################################
str(slSI)
##as.numeric(slSI$hit_distance_sc, slSI$hit_speed, slSI$hit_angle)

#slSI_train$hit_distance_sc <- as.numeric(slSI_train$hit_distance_sc)
#slSI_train$hit_speed <- as.numeric(slSI_train$hit_speed)
#slSI_train$hit_angle <- as.numeric(slSI_train$hit_angle)
#View(slSI_train)
slSI_scaled_data <- scale(slSI_train[,c(1:5)])
slSI_scale_values <- attr(slSI_scaled_data, 'scaled:scale')
slSI_scale_values
##########################################################################################
slSI_center_values <- attr(slSI_scaled_data, 'scaled:center')
slSI_center_values
##########################################################################################
slSI_train <- cbind(slSI_scaled_data, select(slSI_train, hit:row))

# save levels for factor variables
slSI_levels_home_team <- levels(slSI_train$home_team)
slSI_levels_stand <- levels(slSI_train$stand)
slSI_levels_fieldingTeam <- levels(slSI_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(slSI_test)

slSI_test$hit_distance_sc <- (slSI_test$hit_distance_sc - slSI_center_values[1]) / slSI_scale_values[1]

slSI_test$hit_speed <- (slSI_test$hit_speed - slSI_center_values[2]) / slSI_scale_values[2]

slSI_test$hit_angle <- (slSI_test$hit_angle - slSI_center_values[3]) / slSI_scale_values[3]

slSI_test$hc_x <- (slSI_test$hc_x - slSI_center_values[4]) / slSI_scale_values[4]

slSI_test$hc_y <- (slSI_test$hc_y - slSI_center_values[5]) / slSI_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

slSI_validate$hit_distance_sc <- (slSI_validate$hit_distance_sc - slSI_center_values[1]) / slSI_scale_values[1]

slSI_validate$hit_speed <- (slSI_validate$hit_speed - slSI_center_values[2]) / slSI_scale_values[2]

slSI_validate$hit_angle <- (slSI_validate$hit_angle - slSI_center_values[3]) / slSI_scale_values[3]

slSI_validate$hc_x <- (slSI_validate$hc_x - slSI_center_values[4]) / slSI_scale_values[4]

slSI_validate$hc_y <- (slSI_validate$hc_y - slSI_center_values[5]) / slSI_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(slSI_train)

set.seed(42)
slSI_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(slSI_train, -row), ntree = 501, importance = TRUE)

print(slSI_rf.1)

plot(slSI_rf.1)

varImpPlot(slSI_rf.1)

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
slSI_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(slSI_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(slSI_rf.5)

plot(slSI_rf.5)

varImpPlot(slSI_rf.5)

slSI_predict_fit.rf.5 <- data.frame(fits = predict(slSI_rf.5, slSI_test, type = "prob")[,2], actuals = slSI_test$hit)

slSI_pred.rf.5 <- prediction(slSI_predict_fit.rf.5$fits, slSI_predict_fit.rf.5$actuals)

slSI_roc.pred.rf.5 <- performance(slSI_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(slSI_roc.pred.rf.5)
abline(a = 0, b = 1)
slSI_opt <- opt.cut(slSI_roc.pred.rf.5, slSI_pred.rf.5)
slSI_opt
slSI_opt <- slSI_opt[3]
slSI_predict_fit.rf.5$fits <- with(slSI_predict_fit.rf.5, ifelse(fits > slSI_opt, 1, 0)) 

str(slSI_test)

#slSI_test$fieldingTeam <- as.character(slSI_test$fieldingTeam)
#slSI_test$home_team <- as.character(slSI_test$home_team)
#slSI_test$hit <- as.logical(slSI_test$hit)
#slSI_test$stand <- as.logical(slSI_test$stand)
str(slSI_test)
slSI_rf.5_confusion_test <- confusionMatrix(model = slSI_rf.5, x = slSI_test, y = slSI_test$hit)
slSI_rf.5_confusion_validate <- confusionMatrix(model = slSI_rf.5, x = slSI_validate, y = slSI_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


slSI_test_rows <- slSI_test$row
slSI_validate_rows <- slSI_validate$row
slSI_pred_data_emp_1 <- ungroup(slSI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slSI_test_rows)

slSI_pred_data_emp <- ungroup(slSI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slSI_validate_rows) %>%
	rbind(slSI_pred_data_emp_1)

slSI_out_of_training_rows <- slSI_pred_data_emp$row

slSI_rf.5_full_out_of_sample_data <- ungroup(slSI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slSI_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



slSI_rf.5_full_out_of_sample_data_scaled <- slSI_rf.5_full_out_of_sample_data

slSI_rf.5_full_out_of_sample_data_scaled$hit_speed <- (slSI_rf.5_full_out_of_sample_data_scaled$hit_speed - slSI_center_values[2]) / slSI_scale_values[2]

slSI_rf.5_full_out_of_sample_data_scaled$hit_angle <- (slSI_rf.5_full_out_of_sample_data_scaled$hit_angle - slSI_center_values[3]) / slSI_scale_values[3]

slSI_rf.5.prob <- predict(slSI_rf.5, slSI_rf.5_full_out_of_sample_data_scaled, type = "response")

slSI_rf.5_full_out_of_sample_data <- cbind(filter(slSI_rf.5_full_out_of_sample_data, row %in% slSI_out_of_training_rows), slSI_rf.5.prob)

names(slSI_rf.5_full_out_of_sample_data)[65] <- "fits"
names(slSI_rf.5_full_out_of_sample_data)

slSI_rf.5_full_out_of_sample_data_reduced <- slSI_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

slSI_rf.5_mean_table <- slSI_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

slSI_rf.5_full_out_of_sample_data$type <- with(slSI_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

slSI_rf.5_full_out_of_sample_data$hit_label <- with(slSI_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

slSI_rf.5_plot1 <- ggplot(slSI_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

slSI_rf.5_plot1

slSI_rf.5_plot2 <- ggplot(slSI_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

slSI_rf.5_plot2

slSI_rf.5_plot3 <- ggplot(slSI_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

slSI_rf.5_plot3

slSI_grid_plot_rf.5 <- grid.arrange(slSI_rf.5_plot1, slSI_rf.5_plot2, slSI_rf.5_plot3, ncol = 3)
slSI_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

slSI_test_rows <- slSI_test$row
slSI_validate_rows <- slSI_validate$row
slSI_pred_data_emp_1 <- ungroup(slSI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slSI_test_rows)

slSI_pred_data_emp <- ungroup(slSI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slSI_validate_rows) %>%
	rbind(slSI_pred_data_emp_1)

slSI_out_of_training_rows <- slSI_pred_data_emp$row

slSI_pred_data_emp <- ungroup(slSI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slSI_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

slSI_pred_data_emp_scaled <- slSI_pred_data_emp

slSI_pred_data_emp_scaled$hit_speed <- (slSI_pred_data_emp_scaled$hit_speed - slSI_center_values[2]) / slSI_scale_values[2]

slSI_pred_data_emp_scaled$hit_angle <- (slSI_pred_data_emp_scaled$hit_angle - slSI_center_values[3]) / slSI_scale_values[3]

slSI_pred_prob_hit <- predict(slSI_rf.5, slSI_pred_data_emp_scaled, type = "prob")[,2]
slSI_pred_prob_hit_fits <- cbind(slSI_pred_data_emp, slSI_pred_prob_hit)

slSI_pred_prob_hit_fits[,c(1:2)] <- slSI_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

slSI_angle_speed_pred <- slSI_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = slSI_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nSL-SI Hit Probability\n") + theme_bp_grey()

slSI_angle_speed_pred



#	Individual Pitches Start
#	4-Seam Fastball (FF)
FF$hit_distance_sc <- as.numeric(FF$hit_distance_sc, na.rm = TRUE)
FF$hit_angle <- as.numeric(FF$hit_angle, na.rm = TRUE)
FF$hit_speed <- as.numeric(FF$hit_speed, na.rm = TRUE)

FF$hit <- with(FF, ifelse(grepl("Single", FF$events), 1,
															ifelse(grepl("Double", FF$events), 1,
																		 ifelse(grepl("Triple", FF$events), 1, 
																		 			 ifelse(grepl("Home Run", FF$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

FF$fieldingTeam <- with(FF, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[FF$hit_distance_sc == "null"] = NA
#FF$hit_speed[FF$hit_speed == "null"] = NA
#FF$hit_angle[FF$hit_angle == "null"] = NA

# include row names for unique record identification

FF$row <- row.names(FF) %>% as.numeric()

# recode stand and home_team as factors

FF$stand <- as.factor(FF$stand)
FF$home_team <- as.factor(FF$home_team)


FF$game_date <- as.Date(FF$game_date)


# subset 

FF_working_data <- ungroup(FF) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(FF_working_data)
str(ungroup(FF))
table(FF_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

FF_working_data <- filter(FF_working_data, hc_x != 1, hc_y != 1)
head(FF_working_data)

# create training and test sets
# scaled data

set.seed(42)
FF_train <- sample_frac(FF_working_data, .15, replace = FALSE)
FF_split <- setdiff(FF_working_data, FF_train)
FF_test <- sample_frac(FF_split, .50, replace = FALSE)
FF_validate <- setdiff(FF_split, FF_test)

nrow(FF_train) + nrow(FF_test) + nrow(FF_validate) == nrow(FF_working_data)

with(FF_train, table(hit)) %>% prop.table()
with(FF_test, table(hit)) %>% prop.table()
with(FF_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(FF_train)

##########################################################################################
str(FF)
##as.numeric(FF$hit_distance_sc, FF$hit_speed, FF$hit_angle)

#FF_train$hit_distance_sc <- as.numeric(FF_train$hit_distance_sc)
#FF_train$hit_speed <- as.numeric(FF_train$hit_speed)
#FF_train$hit_angle <- as.numeric(FF_train$hit_angle)
#View(FF_train)
FF_scaled_data <- scale(FF_train[,c(1:5)])
FF_scale_values <- attr(FF_scaled_data, 'scaled:scale')
FF_scale_values
##########################################################################################
FF_center_values <- attr(FF_scaled_data, 'scaled:center')
FF_center_values
##########################################################################################

FF_train <- cbind(FF_scaled_data, select(FF_train, hit:row))

# save levels for factor variables
FF_levels_home_team <- levels(FF_train$home_team)
FF_levels_stand <- levels(FF_train$stand)
FF_levels_fieldingTeam <- levels(FF_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(FF_test)

FF_test$hit_distance_sc <- (FF_test$hit_distance_sc - FF_center_values[1]) / FF_scale_values[1]

FF_test$hit_speed <- (FF_test$hit_speed - FF_center_values[2]) / FF_scale_values[2]

FF_test$hit_angle <- (FF_test$hit_angle - FF_center_values[3]) / FF_scale_values[3]

FF_test$hc_x <- (FF_test$hc_x - FF_center_values[4]) / FF_scale_values[4]

FF_test$hc_y <- (FF_test$hc_y - FF_center_values[5]) / FF_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

FF_validate$hit_distance_sc <- (FF_validate$hit_distance_sc - FF_center_values[1]) / FF_scale_values[1]

FF_validate$hit_speed <- (FF_validate$hit_speed - FF_center_values[2]) / FF_scale_values[2]

FF_validate$hit_angle <- (FF_validate$hit_angle - FF_center_values[3]) / FF_scale_values[3]

FF_validate$hc_x <- (FF_validate$hc_x - FF_center_values[4]) / FF_scale_values[4]

FF_validate$hc_y <- (FF_validate$hc_y - FF_center_values[5]) / FF_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(FF_train)

set.seed(42)
FF_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(FF_train, -row), ntree = 501, importance = TRUE)

print(FF_rf.1)

plot(FF_rf.1)

varImpPlot(FF_rf.1)

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
FF_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(FF_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(FF_rf.5)

plot(FF_rf.5)

varImpPlot(FF_rf.5)

FF_predict_fit.rf.5 <- data.frame(fits = predict(FF_rf.5, FF_test, type = "prob")[,2], actuals = FF_test$hit)

FF_pred.rf.5 <- prediction(FF_predict_fit.rf.5$fits, FF_predict_fit.rf.5$actuals)

FF_roc.pred.rf.5 <- performance(FF_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(FF_roc.pred.rf.5)
abline(a = 0, b = 1)
FF_opt <- opt.cut(FF_roc.pred.rf.5, FF_pred.rf.5)
FF_opt
FF_opt <- FF_opt[3]
FF_predict_fit.rf.5$fits <- with(FF_predict_fit.rf.5, ifelse(fits > FF_opt, 1, 0)) 

str(FF_test)

#FF_test$fieldingTeam <- as.character(FF_test$fieldingTeam)
#FF_test$home_team <- as.character(FF_test$home_team)
#FF_test$hit <- as.logical(FF_test$hit)
#FF_test$stand <- as.logical(FF_test$stand)
str(FF_test)
FF_rf.5_confusion_test <- confusionMatrix(model = FF_rf.5, x = FF_test, y = FF_test$hit)
FF_rf.5_confusion_validate <- confusionMatrix(model = FF_rf.5, x = FF_validate, y = FF_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


FF_test_rows <- FF_test$row
FF_validate_rows <- FF_validate$row
FF_pred_data_emp_1 <- ungroup(FF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% FF_test_rows)

FF_pred_data_emp <- ungroup(FF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% FF_validate_rows) %>%
	rbind(FF_pred_data_emp_1)

FF_out_of_training_rows <- FF_pred_data_emp$row

FF_rf.5_full_out_of_sample_data <- ungroup(FF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% FF_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



FF_rf.5_full_out_of_sample_data_scaled <- FF_rf.5_full_out_of_sample_data

FF_rf.5_full_out_of_sample_data_scaled$hit_speed <- (FF_rf.5_full_out_of_sample_data_scaled$hit_speed - FF_center_values[2]) / FF_scale_values[2]

FF_rf.5_full_out_of_sample_data_scaled$hit_angle <- (FF_rf.5_full_out_of_sample_data_scaled$hit_angle - FF_center_values[3]) / FF_scale_values[3]

FF_rf.5.prob <- predict(FF_rf.5, FF_rf.5_full_out_of_sample_data_scaled, type = "response")

FF_rf.5_full_out_of_sample_data <- cbind(filter(FF_rf.5_full_out_of_sample_data, row %in% FF_out_of_training_rows), FF_rf.5.prob)

names(FF_rf.5_full_out_of_sample_data)[65] <- "fits"
names(FF_rf.5_full_out_of_sample_data)

FF_rf.5_full_out_of_sample_data_reduced <- FF_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

FF_rf.5_mean_table <- FF_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

FF_rf.5_full_out_of_sample_data$type <- with(FF_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

FF_rf.5_full_out_of_sample_data$hit_label <- with(FF_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

FF_rf.5_plot1 <- ggplot(FF_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

FF_rf.5_plot1

FF_rf.5_plot2 <- ggplot(FF_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

FF_rf.5_plot2

FF_rf.5_plot3 <- ggplot(FF_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

FF_rf.5_plot3

FF_grid_plot_rf.5 <- grid.arrange(FF_rf.5_plot1, FF_rf.5_plot2, FF_rf.5_plot3, ncol = 3)
FF_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

FF_test_rows <- FF_test$row
FF_validate_rows <- FF_validate$row
FF_pred_data_emp_1 <- ungroup(FF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% FF_test_rows)

FF_pred_data_emp <- ungroup(FF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% FF_validate_rows) %>%
	rbind(FF_pred_data_emp_1)

FF_out_of_training_rows <- FF_pred_data_emp$row

FF_pred_data_emp <- ungroup(FF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% FF_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

FF_pred_data_emp_scaled <- FF_pred_data_emp

FF_pred_data_emp_scaled$hit_speed <- (FF_pred_data_emp_scaled$hit_speed - FF_center_values[2]) / FF_scale_values[2]

FF_pred_data_emp_scaled$hit_angle <- (FF_pred_data_emp_scaled$hit_angle - FF_center_values[3]) / FF_scale_values[3]

FF_pred_prob_hit <- predict(FF_rf.5, FF_pred_data_emp_scaled, type = "prob")[,2]
FF_pred_prob_hit_fits <- cbind(FF_pred_data_emp, FF_pred_prob_hit)

FF_pred_prob_hit_fits[,c(1:2)] <- FF_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

FF_angle_speed_pred <- FF_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = FF_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nFF Hit Probability\n") + theme_bp_grey()

FF_angle_speed_pred

#	Curveball (CU)
CU$hit_distance_sc <- as.numeric(CU$hit_distance_sc, na.rm = TRUE)
CU$hit_angle <- as.numeric(CU$hit_angle, na.rm = TRUE)
CU$hit_speed <- as.numeric(CU$hit_speed, na.rm = TRUE)

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

##########################################################################################
str(CU)
##as.numeric(CU$hit_distance_sc, CU$hit_speed, CU$hit_angle)

#CU_train$hit_distance_sc <- as.numeric(CU_train$hit_distance_sc)
#CU_train$hit_speed <- as.numeric(CU_train$hit_speed)
#CU_train$hit_angle <- as.numeric(CU_train$hit_angle)
#View(CU_train)
CU_scaled_data <- scale(CU_train[,c(1:5)])
CU_scale_values <- attr(CU_scaled_data, 'scaled:scale')
CU_scale_values
##########################################################################################
CU_center_values <- attr(CU_scaled_data, 'scaled:center')
CU_center_values
##########################################################################################
CU_train <- cbind(CU_scaled_data, select(CU_train, hit:row))

# save levels for factor variables
CU_levels_home_team <- levels(CU_train$home_team)
CU_levels_stand <- levels(CU_train$stand)
CU_levels_fieldingTeam <- levels(CU_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(CU_test)

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

str(CU_test)

#CU_test$fieldingTeam <- as.character(CU_test$fieldingTeam)
#CU_test$home_team <- as.character(CU_test$home_team)
#CU_test$hit <- as.logical(CU_test$hit)
#CU_test$stand <- as.logical(CU_test$stand)
str(CU_test)
CU_rf.5_confusion_test <- confusionMatrix(model = CU_rf.5, x = CU_test, y = CU_test$hit)
CU_rf.5_confusion_validate <- confusionMatrix(model = CU_rf.5, x = CU_validate, y = CU_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


CU_test_rows <- CU_test$row
CU_validate_rows <- CU_validate$row
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
	filter(row %in% CU_test_rows)

CU_pred_data_emp <- ungroup(CU) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% CU_validate_rows) %>%
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

CU_angle_speed_pred <- CU_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = CU_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nCU Hit Probability\n") + theme_bp_grey()

CU_angle_speed_pred


#	Slider (SL)

SL$hit_distance_sc <- as.numeric(SL$hit_distance_sc, na.rm = TRUE)
SL$hit_angle <- as.numeric(SL$hit_angle, na.rm = TRUE)
SL$hit_speed <- as.numeric(SL$hit_speed, na.rm = TRUE)

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

##########################################################################################
str(SL)
##as.numeric(SL$hit_distance_sc, SL$hit_speed, SL$hit_angle)

#SL_train$hit_distance_sc <- as.numeric(SL_train$hit_distance_sc)
#SL_train$hit_speed <- as.numeric(SL_train$hit_speed)
#SL_train$hit_angle <- as.numeric(SL_train$hit_angle)
#View(SL_train)
SL_scaled_data <- scale(SL_train[,c(1:5)])
SL_scale_values <- attr(SL_scaled_data, 'scaled:scale')
SL_scale_values
##########################################################################################
SL_center_values <- attr(SL_scaled_data, 'scaled:center')
SL_center_values
##########################################################################################
SL_train <- cbind(SL_scaled_data, select(SL_train, hit:row))

# save levels for factor variables
SL_levels_home_team <- levels(SL_train$home_team)
SL_levels_stand <- levels(SL_train$stand)
SL_levels_fieldingTeam <- levels(SL_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(SL_test)

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

str(SL_test)

#SL_test$fieldingTeam <- as.character(SL_test$fieldingTeam)
#SL_test$home_team <- as.character(SL_test$home_team)
#SL_test$hit <- as.logical(SL_test$hit)
#SL_test$stand <- as.logical(SL_test$stand)
str(SL_test)
SL_rf.5_confusion_test <- confusionMatrix(model = SL_rf.5, x = SL_test, y = SL_test$hit)
SL_rf.5_confusion_validate <- confusionMatrix(model = SL_rf.5, x = SL_validate, y = SL_validate$hit)

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
	filter(row %in% SL_test_rows)

SL_pred_data_emp <- ungroup(SL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% SL_validate_rows) %>%
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

SL_angle_speed_pred <- SL_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = SL_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nSL Hit Probability\n") + theme_bp_grey()

SL_angle_speed_pred


#	Two-Seam Fastball (FT)
FT$hit_distance_sc <- as.numeric(FT$hit_distance_sc, na.rm = TRUE)
FT$hit_angle <- as.numeric(FT$hit_angle, na.rm = TRUE)
FT$hit_speed <- as.numeric(FT$hit_speed, na.rm = TRUE)

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

##########################################################################################
str(FT)
##as.numeric(FT$hit_distance_sc, FT$hit_speed, FT$hit_angle)

#FT_train$hit_distance_sc <- as.numeric(FT_train$hit_distance_sc)
#FT_train$hit_speed <- as.numeric(FT_train$hit_speed)
#FT_train$hit_angle <- as.numeric(FT_train$hit_angle)
#View(FT_train)
FT_scaled_data <- scale(FT_train[,c(1:5)])
FT_scale_values <- attr(FT_scaled_data, 'scaled:scale')
FT_scale_values
##########################################################################################
FT_center_values <- attr(FT_scaled_data, 'scaled:center')
FT_center_values
##########################################################################################
FT_train <- cbind(FT_scaled_data, select(FT_train, hit:row))

# save levels for factor variables
FT_levels_home_team <- levels(FT_train$home_team)
FT_levels_stand <- levels(FT_train$stand)
FT_levels_fieldingTeam <- levels(FT_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(FT_test)

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

str(FT_test)

#FT_test$fieldingTeam <- as.character(FT_test$fieldingTeam)
#FT_test$home_team <- as.character(FT_test$home_team)
#FT_test$hit <- as.logical(FT_test$hit)
#FT_test$stand <- as.logical(FT_test$stand)
str(FT_test)
FT_rf.5_confusion_test <- confusionMatrix(model = FT_rf.5, x = FT_test, y = FT_test$hit)
FT_rf.5_confusion_validate <- confusionMatrix(model = FT_rf.5, x = FT_validate, y = FT_validate$hit)

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
	filter(row %in% FT_test_rows)

FT_pred_data_emp <- ungroup(FT) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% FT_validate_rows) %>%
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

FT_angle_speed_pred <- FT_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = FT_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nFT Hit Probability\n") + theme_bp_grey()

FT_angle_speed_pred


#	Changeup (CH)
CH$hit_distance_sc <- as.numeric(CH$hit_distance_sc, na.rm = TRUE)
CH$hit_angle <- as.numeric(CH$hit_angle, na.rm = TRUE)
CH$hit_speed <- as.numeric(CH$hit_speed, na.rm = TRUE)

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

##########################################################################################
str(CH)
##as.numeric(CH$hit_distance_sc, CH$hit_speed, CH$hit_angle)

#CH_train$hit_distance_sc <- as.numeric(CH_train$hit_distance_sc)
#CH_train$hit_speed <- as.numeric(CH_train$hit_speed)
#CH_train$hit_angle <- as.numeric(CH_train$hit_angle)
#View(CH_train)
CH_scaled_data <- scale(CH_train[,c(1:5)])
CH_scale_values <- attr(CH_scaled_data, 'scaled:scale')
CH_scale_values
##########################################################################################
CH_center_values <- attr(CH_scaled_data, 'scaled:center')
CH_center_values
##########################################################################################
CH_train <- cbind(CH_scaled_data, select(CH_train, hit:row))

# save levels for factor variables
CH_levels_home_team <- levels(CH_train$home_team)
CH_levels_stand <- levels(CH_train$stand)
CH_levels_fieldingTeam <- levels(CH_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(CH_test)

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

str(CH_test)

#CH_test$fieldingTeam <- as.character(CH_test$fieldingTeam)
#CH_test$home_team <- as.character(CH_test$home_team)
#CH_test$hit <- as.logical(CH_test$hit)
#CH_test$stand <- as.logical(CH_test$stand)
str(CH_test)
CH_rf.5_confusion_test <- confusionMatrix(model = CH_rf.5, x = CH_test, y = CH_test$hit)
CH_rf.5_confusion_validate <- confusionMatrix(model = CH_rf.5, x = CH_validate, y = CH_validate$hit)

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
	filter(row %in% CH_test_rows)

CH_pred_data_emp <- ungroup(CH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% CH_validate_rows) %>%
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

CH_angle_speed_pred <- CH_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = CH_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nCH Hit Probability\n") + theme_bp_grey()

CH_angle_speed_pred


#	Cutter (FC)
FC$hit_distance_sc <- as.numeric(FC$hit_distance_sc, na.rm = TRUE)
FC$hit_angle <- as.numeric(FC$hit_angle, na.rm = TRUE)
FC$hit_speed <- as.numeric(FC$hit_speed, na.rm = TRUE)

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

##########################################################################################
str(FC)
##as.numeric(FC$hit_distance_sc, FC$hit_speed, FC$hit_angle)

#FC_train$hit_distance_sc <- as.numeric(FC_train$hit_distance_sc)
#FC_train$hit_speed <- as.numeric(FC_train$hit_speed)
#FC_train$hit_angle <- as.numeric(FC_train$hit_angle)
#View(FC_train)
FC_scaled_data <- scale(FC_train[,c(1:5)])
FC_scale_values <- attr(FC_scaled_data, 'scaled:scale')
FC_scale_values
##########################################################################################
FC_center_values <- attr(FC_scaled_data, 'scaled:center')
FC_center_values
##########################################################################################
FC_train <- cbind(FC_scaled_data, select(FC_train, hit:row))

# save levels for factor variables
FC_levels_home_team <- levels(FC_train$home_team)
FC_levels_stand <- levels(FC_train$stand)
FC_levels_fieldingTeam <- levels(FC_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(FC_test)

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

str(FC_test)

#FC_test$fieldingTeam <- as.character(FC_test$fieldingTeam)
#FC_test$home_team <- as.character(FC_test$home_team)
#FC_test$hit <- as.logical(FC_test$hit)
#FC_test$stand <- as.logical(FC_test$stand)
str(FC_test)
FC_rf.5_confusion_test <- confusionMatrix(model = FC_rf.5, x = FC_test, y = FC_test$hit)
FC_rf.5_confusion_validate <- confusionMatrix(model = FC_rf.5, x = FC_validate, y = FC_validate$hit)

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
	filter(row %in% FC_test_rows)

FC_pred_data_emp <- ungroup(FC) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% FC_validate_rows) %>%
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

FC_angle_speed_pred <- FC_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = FC_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nFC Hit Probability\n") + theme_bp_grey()

FC_angle_speed_pred


#	Splitter (FS)
FS$hit_distance_sc <- as.numeric(FS$hit_distance_sc, na.rm = TRUE)
FS$hit_angle <- as.numeric(FS$hit_angle, na.rm = TRUE)
FS$hit_speed <- as.numeric(FS$hit_speed, na.rm = TRUE)

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

##########################################################################################
str(FS)
##as.numeric(FS$hit_distance_sc, FS$hit_speed, FS$hit_angle)

#FS_train$hit_distance_sc <- as.numeric(FS_train$hit_distance_sc)
#FS_train$hit_speed <- as.numeric(FS_train$hit_speed)
#FS_train$hit_angle <- as.numeric(FS_train$hit_angle)
#View(FS_train)
FS_scaled_data <- scale(FS_train[,c(1:5)])
FS_scale_values <- attr(FS_scaled_data, 'scaled:scale')
FS_scale_values
##########################################################################################
FS_center_values <- attr(FS_scaled_data, 'scaled:center')
FS_center_values
##########################################################################################
FS_train <- cbind(FS_scaled_data, select(FS_train, hit:row))

# save levels for factor variables
FS_levels_home_team <- levels(FS_train$home_team)
FS_levels_stand <- levels(FS_train$stand)
FS_levels_fieldingTeam <- levels(FS_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(FS_test)

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

str(FS_test)

#FS_test$fieldingTeam <- as.character(FS_test$fieldingTeam)
#FS_test$home_team <- as.character(FS_test$home_team)
#FS_test$hit <- as.logical(FS_test$hit)
#FS_test$stand <- as.logical(FS_test$stand)
str(FS_test)
FS_rf.5_confusion_test <- confusionMatrix(model = FS_rf.5, x = FS_test, y = FS_test$hit)
FS_rf.5_confusion_validate <- confusionMatrix(model = FS_rf.5, x = FS_validate, y = FS_validate$hit)

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
	filter(row %in% FS_test_rows)

FS_pred_data_emp <- ungroup(FS) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% FS_validate_rows) %>%
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

FS_angle_speed_pred <- FS_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = FS_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nFS Hit Probability\n") + theme_bp_grey()

FS_angle_speed_pred


#	Knuckle Curve (KC)
KC$hit_distance_sc <- as.numeric(KC$hit_distance_sc, na.rm = TRUE)
KC$hit_angle <- as.numeric(KC$hit_angle, na.rm = TRUE)
KC$hit_speed <- as.numeric(KC$hit_speed, na.rm = TRUE)

KC$hit <- with(KC, ifelse(grepl("Single", KC$events), 1,
															ifelse(grepl("Double", KC$events), 1,
																		 ifelse(grepl("Triple", KC$events), 1, 
																		 			 ifelse(grepl("Home Run", KC$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

KC$fieldingTeam <- with(KC, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[KC$hit_distance_sc == "null"] = NA
#KC$hit_speed[KC$hit_speed == "null"] = NA
#KC$hit_angle[KC$hit_angle == "null"] = NA

# include row names for unique record identification

KC$row <- row.names(KC) %>% as.numeric()

# recode stand and home_team as factors

KC$stand <- as.factor(KC$stand)
KC$home_team <- as.factor(KC$home_team)


KC$game_date <- as.Date(KC$game_date)


# subset 

KC_working_data <- ungroup(KC) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(KC_working_data)
str(ungroup(KC))
table(KC_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

KC_working_data <- filter(KC_working_data, hc_x != 1, hc_y != 1)
head(KC_working_data)

# create training and test sets
# scaled data

set.seed(42)
KC_train <- sample_frac(KC_working_data, .15, replace = FALSE)
KC_split <- setdiff(KC_working_data, KC_train)
KC_test <- sample_frac(KC_split, .50, replace = FALSE)
KC_validate <- setdiff(KC_split, KC_test)

nrow(KC_train) + nrow(KC_test) + nrow(KC_validate) == nrow(KC_working_data)

with(KC_train, table(hit)) %>% prop.table()
with(KC_test, table(hit)) %>% prop.table()
with(KC_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(KC_train)

##########################################################################################
str(KC)
##as.numeric(KC$hit_distance_sc, KC$hit_speed, KC$hit_angle)

#KC_train$hit_distance_sc <- as.numeric(KC_train$hit_distance_sc)
#KC_train$hit_speed <- as.numeric(KC_train$hit_speed)
#KC_train$hit_angle <- as.numeric(KC_train$hit_angle)
#View(KC_train)
KC_scaled_data <- scale(KC_train[,c(1:5)])
KC_scale_values <- attr(KC_scaled_data, 'scaled:scale')
KC_scale_values
##########################################################################################
KC_center_values <- attr(KC_scaled_data, 'scaled:center')
KC_center_values
##########################################################################################
KC_train <- cbind(KC_scaled_data, select(KC_train, hit:row))

# save levels for factor variables
KC_levels_home_team <- levels(KC_train$home_team)
KC_levels_stand <- levels(KC_train$stand)
KC_levels_fieldingTeam <- levels(KC_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(KC_test)

KC_test$hit_distance_sc <- (KC_test$hit_distance_sc - KC_center_values[1]) / KC_scale_values[1]

KC_test$hit_speed <- (KC_test$hit_speed - KC_center_values[2]) / KC_scale_values[2]

KC_test$hit_angle <- (KC_test$hit_angle - KC_center_values[3]) / KC_scale_values[3]

KC_test$hc_x <- (KC_test$hc_x - KC_center_values[4]) / KC_scale_values[4]

KC_test$hc_y <- (KC_test$hc_y - KC_center_values[5]) / KC_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

KC_validate$hit_distance_sc <- (KC_validate$hit_distance_sc - KC_center_values[1]) / KC_scale_values[1]

KC_validate$hit_speed <- (KC_validate$hit_speed - KC_center_values[2]) / KC_scale_values[2]

KC_validate$hit_angle <- (KC_validate$hit_angle - KC_center_values[3]) / KC_scale_values[3]

KC_validate$hc_x <- (KC_validate$hc_x - KC_center_values[4]) / KC_scale_values[4]

KC_validate$hc_y <- (KC_validate$hc_y - KC_center_values[5]) / KC_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(KC_train)

set.seed(42)
KC_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(KC_train, -row), ntree = 501, importance = TRUE)

print(KC_rf.1)

plot(KC_rf.1)

varImpPlot(KC_rf.1)

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
KC_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(KC_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(KC_rf.5)

plot(KC_rf.5)

varImpPlot(KC_rf.5)

KC_predict_fit.rf.5 <- data.frame(fits = predict(KC_rf.5, KC_test, type = "prob")[,2], actuals = KC_test$hit)

KC_pred.rf.5 <- prediction(KC_predict_fit.rf.5$fits, KC_predict_fit.rf.5$actuals)

KC_roc.pred.rf.5 <- performance(KC_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(KC_roc.pred.rf.5)
abline(a = 0, b = 1)
KC_opt <- opt.cut(KC_roc.pred.rf.5, KC_pred.rf.5)
KC_opt
KC_opt <- KC_opt[3]
KC_predict_fit.rf.5$fits <- with(KC_predict_fit.rf.5, ifelse(fits > KC_opt, 1, 0)) 

str(KC_test)

#KC_test$fieldingTeam <- as.character(KC_test$fieldingTeam)
#KC_test$home_team <- as.character(KC_test$home_team)
#KC_test$hit <- as.logical(KC_test$hit)
#KC_test$stand <- as.logical(KC_test$stand)
str(KC_test)
KC_rf.5_confusion_test <- confusionMatrix(model = KC_rf.5, x = KC_test, y = KC_test$hit)
KC_rf.5_confusion_validate <- confusionMatrix(model = KC_rf.5, x = KC_validate, y = KC_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


KC_test_rows <- KC_test$row
KC_validate_rows <- KC_validate$row
KC_pred_data_emp_1 <- ungroup(KC) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% KC_test_rows)

KC_pred_data_emp <- ungroup(KC) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% KC_validate_rows) %>%
	rbind(KC_pred_data_emp_1)

KC_out_of_training_rows <- KC_pred_data_emp$row

KC_rf.5_full_out_of_sample_data <- ungroup(KC) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% KC_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



KC_rf.5_full_out_of_sample_data_scaled <- KC_rf.5_full_out_of_sample_data

KC_rf.5_full_out_of_sample_data_scaled$hit_speed <- (KC_rf.5_full_out_of_sample_data_scaled$hit_speed - KC_center_values[2]) / KC_scale_values[2]

KC_rf.5_full_out_of_sample_data_scaled$hit_angle <- (KC_rf.5_full_out_of_sample_data_scaled$hit_angle - KC_center_values[3]) / KC_scale_values[3]

KC_rf.5.prob <- predict(KC_rf.5, KC_rf.5_full_out_of_sample_data_scaled, type = "response")

KC_rf.5_full_out_of_sample_data <- cbind(filter(KC_rf.5_full_out_of_sample_data, row %in% KC_out_of_training_rows), KC_rf.5.prob)

names(KC_rf.5_full_out_of_sample_data)[65] <- "fits"
names(KC_rf.5_full_out_of_sample_data)

KC_rf.5_full_out_of_sample_data_reduced <- KC_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

KC_rf.5_mean_table <- KC_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

KC_rf.5_full_out_of_sample_data$type <- with(KC_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

KC_rf.5_full_out_of_sample_data$hit_label <- with(KC_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

KC_rf.5_plot1 <- ggplot(KC_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

KC_rf.5_plot1

KC_rf.5_plot2 <- ggplot(KC_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

KC_rf.5_plot2

KC_rf.5_plot3 <- ggplot(KC_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

KC_rf.5_plot3

KC_grid_plot_rf.5 <- grid.arrange(KC_rf.5_plot1, KC_rf.5_plot2, KC_rf.5_plot3, ncol = 3)
KC_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

KC_test_rows <- KC_test$row
KC_validate_rows <- KC_validate$row
KC_pred_data_emp_1 <- ungroup(KC) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% KC_test_rows)

KC_pred_data_emp <- ungroup(KC) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% KC_validate_rows) %>%
	rbind(KC_pred_data_emp_1)

KC_out_of_training_rows <- KC_pred_data_emp$row

KC_pred_data_emp <- ungroup(KC) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% KC_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

KC_pred_data_emp_scaled <- KC_pred_data_emp

KC_pred_data_emp_scaled$hit_speed <- (KC_pred_data_emp_scaled$hit_speed - KC_center_values[2]) / KC_scale_values[2]

KC_pred_data_emp_scaled$hit_angle <- (KC_pred_data_emp_scaled$hit_angle - KC_center_values[3]) / KC_scale_values[3]

KC_pred_prob_hit <- predict(KC_rf.5, KC_pred_data_emp_scaled, type = "prob")[,2]
KC_pred_prob_hit_fits <- cbind(KC_pred_data_emp, KC_pred_prob_hit)

KC_pred_prob_hit_fits[,c(1:2)] <- KC_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

KC_angle_speed_pred <- KC_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = KC_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nKC Hit Probability\n") + theme_bp_grey()

KC_angle_speed_pred


# Knuckleball (KN)
KN$hit_distance_sc <- as.numeric(KN$hit_distance_sc, na.rm = TRUE)
KN$hit_angle <- as.numeric(KN$hit_angle, na.rm = TRUE)
KN$hit_speed <- as.numeric(KN$hit_speed, na.rm = TRUE)

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

##########################################################################################
str(KN)
##as.numeric(KN$hit_distance_sc, KN$hit_speed, KN$hit_angle)

#KN_train$hit_distance_sc <- as.numeric(KN_train$hit_distance_sc)
#KN_train$hit_speed <- as.numeric(KN_train$hit_speed)
#KN_train$hit_angle <- as.numeric(KN_train$hit_angle)
#View(KN_train)
KN_scaled_data <- scale(KN_train[,c(1:5)])
KN_scale_values <- attr(KN_scaled_data, 'scaled:scale')
KN_scale_values
##########################################################################################
KN_center_values <- attr(KN_scaled_data, 'scaled:center')
KN_center_values
##########################################################################################
KN_train <- cbind(KN_scaled_data, select(KN_train, hit:row))

# save levels for factor variables
KN_levels_home_team <- levels(KN_train$home_team)
KN_levels_stand <- levels(KN_train$stand)
KN_levels_fieldingTeam <- levels(KN_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(KN_test)

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

str(KN_test)

#KN_test$fieldingTeam <- as.character(KN_test$fieldingTeam)
#KN_test$home_team <- as.character(KN_test$home_team)
#KN_test$hit <- as.logical(KN_test$hit)
#KN_test$stand <- as.logical(KN_test$stand)
str(KN_test)
KN_rf.5_confusion_test <- confusionMatrix(model = KN_rf.5, x = KN_test, y = KN_test$hit)
KN_rf.5_confusion_validate <- confusionMatrix(model = KN_rf.5, x = KN_validate, y = KN_validate$hit)

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
	filter(row %in% KN_test_rows)

KN_pred_data_emp <- ungroup(KN) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% KN_validate_rows) %>%
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

KN_angle_speed_pred <- KN_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = KN_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nKN Hit Probability\n") + theme_bp_grey()

KN_angle_speed_pred


# Sinker (SI)

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
head(SI_train)

##########################################################################################
str(SI)
##as.numeric(SI$hit_distance_sc, SI$hit_speed, SI$hit_angle)

#SI_train$hit_distance_sc <- as.numeric(SI_train$hit_distance_sc)
#SI_train$hit_speed <- as.numeric(SI_train$hit_speed)
#SI_train$hit_angle <- as.numeric(SI_train$hit_angle)
#View(SI_train)
SI_scaled_data <- scale(SI_train[,c(1:5)])
SI_scale_values <- attr(SI_scaled_data, 'scaled:scale')
SI_scale_values
##########################################################################################
SI_center_values <- attr(SI_scaled_data, 'scaled:center')
SI_center_values
##########################################################################################
SI_train <- cbind(SI_scaled_data, select(SI_train, hit:row))

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



#	Fastball - Fastball

ffFF$hit_distance_sc <- as.numeric(ffFF$hit_distance_sc, na.rm = TRUE)
ffFF$hit_angle <- as.numeric(ffFF$hit_angle, na.rm = TRUE)
ffFF$hit_speed <- as.numeric(ffFF$hit_speed, na.rm = TRUE)

ffFF$hit <- with(ffFF, ifelse(grepl("Single", ffFF$events), 1,
															ifelse(grepl("Double", ffFF$events), 1,
																		 ifelse(grepl("Triple", ffFF$events), 1, 
																		 			 ifelse(grepl("Home Run", ffFF$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

ffFF$fieldingTeam <- with(ffFF, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[ffFF$hit_distance_sc == "null"] = NA
#ffFF$hit_speed[ffFF$hit_speed == "null"] = NA
#ffFF$hit_angle[ffFF$hit_angle == "null"] = NA

# include row names for unique record identification

ffFF$row <- row.names(ffFF) %>% as.numeric()

# recode stand and home_team as factors

ffFF$stand <- as.factor(ffFF$stand)
ffFF$home_team <- as.factor(ffFF$home_team)


ffFF$game_date <- as.Date(ffFF$game_date)


# subset 

ffFF_working_data <- ungroup(ffFF) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(ffFF_working_data)
str(ungroup(ffFF))
table(ffFF_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

ffFF_working_data <- filter(ffFF_working_data, hc_x != 1, hc_y != 1)
head(ffFF_working_data)

# create training and test sets
# scaled data

set.seed(42)
ffFF_train <- sample_frac(ffFF_working_data, .15, replace = FALSE)
ffFF_split <- setdiff(ffFF_working_data, ffFF_train)
ffFF_test <- sample_frac(ffFF_split, .50, replace = FALSE)
ffFF_validate <- setdiff(ffFF_split, ffFF_test)

nrow(ffFF_train) + nrow(ffFF_test) + nrow(ffFF_validate) == nrow(ffFF_working_data)

with(ffFF_train, table(hit)) %>% prop.table()
with(ffFF_test, table(hit)) %>% prop.table()
with(ffFF_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(ffFF_train)

##########################################################################################
str(ffFF)
##as.numeric(ffFF$hit_distance_sc, ffFF$hit_speed, ffFF$hit_angle)

#ffFF_train$hit_distance_sc <- as.numeric(ffFF_train$hit_distance_sc)
#ffFF_train$hit_speed <- as.numeric(ffFF_train$hit_speed)
#ffFF_train$hit_angle <- as.numeric(ffFF_train$hit_angle)
#View(ffFF_train)
ffFF_scaled_data <- scale(ffFF_train[,c(1:5)])
ffFF_scale_values <- attr(ffFF_scaled_data, 'scaled:scale')
ffFF_scale_values
##########################################################################################
ffFF_center_values <- attr(ffFF_scaled_data, 'scaled:center')
ffFF_center_values
##########################################################################################
ffFF_train <- cbind(ffFF_scaled_data, select(ffFF_train, hit:row))

# save levels for factor variables
ffFF_levels_home_team <- levels(ffFF_train$home_team)
ffFF_levels_stand <- levels(ffFF_train$stand)
ffFF_levels_fieldingTeam <- levels(ffFF_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(ffFF_test)

ffFF_test$hit_distance_sc <- (ffFF_test$hit_distance_sc - ffFF_center_values[1]) / ffFF_scale_values[1]

ffFF_test$hit_speed <- (ffFF_test$hit_speed - ffFF_center_values[2]) / ffFF_scale_values[2]

ffFF_test$hit_angle <- (ffFF_test$hit_angle - ffFF_center_values[3]) / ffFF_scale_values[3]

ffFF_test$hc_x <- (ffFF_test$hc_x - ffFF_center_values[4]) / ffFF_scale_values[4]

ffFF_test$hc_y <- (ffFF_test$hc_y - ffFF_center_values[5]) / ffFF_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

ffFF_validate$hit_distance_sc <- (ffFF_validate$hit_distance_sc - ffFF_center_values[1]) / ffFF_scale_values[1]

ffFF_validate$hit_speed <- (ffFF_validate$hit_speed - ffFF_center_values[2]) / ffFF_scale_values[2]

ffFF_validate$hit_angle <- (ffFF_validate$hit_angle - ffFF_center_values[3]) / ffFF_scale_values[3]

ffFF_validate$hc_x <- (ffFF_validate$hc_x - ffFF_center_values[4]) / ffFF_scale_values[4]

ffFF_validate$hc_y <- (ffFF_validate$hc_y - ffFF_center_values[5]) / ffFF_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(ffFF_train)

set.seed(42)
ffFF_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(ffFF_train, -row), ntree = 501, importance = TRUE)

print(ffFF_rf.1)

plot(ffFF_rf.1)

varImpPlot(ffFF_rf.1)

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
ffFF_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(ffFF_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(ffFF_rf.5)

plot(ffFF_rf.5)

varImpPlot(ffFF_rf.5)

ffFF_predict_fit.rf.5 <- data.frame(fits = predict(ffFF_rf.5, ffFF_test, type = "prob")[,2], actuals = ffFF_test$hit)

ffFF_pred.rf.5 <- prediction(ffFF_predict_fit.rf.5$fits, ffFF_predict_fit.rf.5$actuals)

ffFF_roc.pred.rf.5 <- performance(ffFF_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(ffFF_roc.pred.rf.5)
abline(a = 0, b = 1)
ffFF_opt <- opt.cut(ffFF_roc.pred.rf.5, ffFF_pred.rf.5)
ffFF_opt
ffFF_opt <- ffFF_opt[3]
ffFF_predict_fit.rf.5$fits <- with(ffFF_predict_fit.rf.5, ifelse(fits > ffFF_opt, 1, 0)) 

str(ffFF_test)

#ffFF_test$fieldingTeam <- as.character(ffFF_test$fieldingTeam)
#ffFF_test$home_team <- as.character(ffFF_test$home_team)
#ffFF_test$hit <- as.logical(ffFF_test$hit)
#ffFF_test$stand <- as.logical(ffFF_test$stand)
str(ffFF_test)
ffFF_rf.5_confusion_test <- confusionMatrix(model = ffFF_rf.5, x = ffFF_test, y = ffFF_test$hit)
ffFF_rf.5_confusion_validate <- confusionMatrix(model = ffFF_rf.5, x = ffFF_validate, y = ffFF_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


ffFF_test_rows <- ffFF_test$row
ffFF_validate_rows <- ffFF_validate$row
ffFF_pred_data_emp_1 <- ungroup(ffFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffFF_test_rows)

ffFF_pred_data_emp <- ungroup(ffFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffFF_validate_rows) %>%
	rbind(ffFF_pred_data_emp_1)

ffFF_out_of_training_rows <- ffFF_pred_data_emp$row

ffFF_rf.5_full_out_of_sample_data <- ungroup(ffFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffFF_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



ffFF_rf.5_full_out_of_sample_data_scaled <- ffFF_rf.5_full_out_of_sample_data

ffFF_rf.5_full_out_of_sample_data_scaled$hit_speed <- (ffFF_rf.5_full_out_of_sample_data_scaled$hit_speed - ffFF_center_values[2]) / ffFF_scale_values[2]

ffFF_rf.5_full_out_of_sample_data_scaled$hit_angle <- (ffFF_rf.5_full_out_of_sample_data_scaled$hit_angle - ffFF_center_values[3]) / ffFF_scale_values[3]

ffFF_rf.5.prob <- predict(ffFF_rf.5, ffFF_rf.5_full_out_of_sample_data_scaled, type = "response")

ffFF_rf.5_full_out_of_sample_data <- cbind(filter(ffFF_rf.5_full_out_of_sample_data, row %in% ffFF_out_of_training_rows), ffFF_rf.5.prob)

names(ffFF_rf.5_full_out_of_sample_data)[65] <- "fits"
names(ffFF_rf.5_full_out_of_sample_data)

ffFF_rf.5_full_out_of_sample_data_reduced <- ffFF_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

ffFF_rf.5_mean_table <- ffFF_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

ffFF_rf.5_full_out_of_sample_data$type <- with(ffFF_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

ffFF_rf.5_full_out_of_sample_data$hit_label <- with(ffFF_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

ffFF_rf.5_plot1 <- ggplot(ffFF_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

ffFF_rf.5_plot1

ffFF_rf.5_plot2 <- ggplot(ffFF_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

ffFF_rf.5_plot2

ffFF_rf.5_plot3 <- ggplot(ffFF_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

ffFF_rf.5_plot3

ffFF_grid_plot_rf.5 <- grid.arrange(ffFF_rf.5_plot1, ffFF_rf.5_plot2, ffFF_rf.5_plot3, ncol = 3)
ffFF_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

ffFF_test_rows <- ffFF_test$row
ffFF_validate_rows <- ffFF_validate$row
ffFF_pred_data_emp_1 <- ungroup(ffFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffFF_test_rows)

ffFF_pred_data_emp <- ungroup(ffFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffFF_validate_rows) %>%
	rbind(ffFF_pred_data_emp_1)

ffFF_out_of_training_rows <- ffFF_pred_data_emp$row

ffFF_pred_data_emp <- ungroup(ffFF) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% ffFF_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

ffFF_pred_data_emp_scaled <- ffFF_pred_data_emp

ffFF_pred_data_emp_scaled$hit_speed <- (ffFF_pred_data_emp_scaled$hit_speed - ffFF_center_values[2]) / ffFF_scale_values[2]

ffFF_pred_data_emp_scaled$hit_angle <- (ffFF_pred_data_emp_scaled$hit_angle - ffFF_center_values[3]) / ffFF_scale_values[3]

ffFF_pred_prob_hit <- predict(ffFF_rf.5, ffFF_pred_data_emp_scaled, type = "prob")[,2]
ffFF_pred_prob_hit_fits <- cbind(ffFF_pred_data_emp, ffFF_pred_prob_hit)

ffFF_pred_prob_hit_fits[,c(1:2)] <- ffFF_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

ffFF_angle_speed_pred <- ffFF_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = ffFF_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nFF-FF Hit Probability\n") + theme_bp_grey()

ffFF_angle_speed_pred

# Sinker - Sinker 
siSI$hit_distance_sc <- as.numeric(siSI$hit_distance_sc, na.rm = TRUE)
siSI$hit_angle <- as.numeric(siSI$hit_angle, na.rm = TRUE)
siSI$hit_speed <- as.numeric(siSI$hit_speed, na.rm = TRUE)

siSI$hit <- with(siSI, ifelse(grepl("Single", siSI$events), 1,
															ifelse(grepl("Double", siSI$events), 1,
																		 ifelse(grepl("Triple", siSI$events), 1, 
																		 			 ifelse(grepl("Home Run", siSI$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

siSI$fieldingTeam <- with(siSI, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[siSI$hit_distance_sc == "null"] = NA
#siSI$hit_speed[siSI$hit_speed == "null"] = NA
#siSI$hit_angle[siSI$hit_angle == "null"] = NA

# include row names for unique record identification

siSI$row <- row.names(siSI) %>% as.numeric()

# recode stand and home_team as factors

siSI$stand <- as.factor(siSI$stand)
siSI$home_team <- as.factor(siSI$home_team)


siSI$game_date <- as.Date(siSI$game_date)


# subset 

siSI_working_data <- ungroup(siSI) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(siSI_working_data)
str(ungroup(siSI))
table(siSI_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

siSI_working_data <- filter(siSI_working_data, hc_x != 1, hc_y != 1)
head(siSI_working_data)

# create training and test sets
# scaled data

set.seed(42)
siSI_train <- sample_frac(siSI_working_data, .15, replace = FALSE)
siSI_split <- setdiff(siSI_working_data, siSI_train)
siSI_test <- sample_frac(siSI_split, .50, replace = FALSE)
siSI_validate <- setdiff(siSI_split, siSI_test)

nrow(siSI_train) + nrow(siSI_test) + nrow(siSI_validate) == nrow(siSI_working_data)

with(siSI_train, table(hit)) %>% prop.table()
with(siSI_test, table(hit)) %>% prop.table()
with(siSI_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(siSI_train)

##########################################################################################
str(siSI)
##as.numeric(siSI$hit_distance_sc, siSI$hit_speed, siSI$hit_angle)

#siSI_train$hit_distance_sc <- as.numeric(siSI_train$hit_distance_sc)
#siSI_train$hit_speed <- as.numeric(siSI_train$hit_speed)
#siSI_train$hit_angle <- as.numeric(siSI_train$hit_angle)
#View(siSI_train)
siSI_scaled_data <- scale(siSI_train[,c(1:5)])
siSI_scale_values <- attr(siSI_scaled_data, 'scaled:scale')
siSI_scale_values
##########################################################################################
siSI_center_values <- attr(siSI_scaled_data, 'scaled:center')
siSI_center_values
##########################################################################################
siSI_train <- cbind(siSI_scaled_data, select(siSI_train, hit:row))

# save levels for factor variables
siSI_levels_home_team <- levels(siSI_train$home_team)
siSI_levels_stand <- levels(siSI_train$stand)
siSI_levels_fieldingTeam <- levels(siSI_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(siSI_test)

siSI_test$hit_distance_sc <- (siSI_test$hit_distance_sc - siSI_center_values[1]) / siSI_scale_values[1]

siSI_test$hit_speed <- (siSI_test$hit_speed - siSI_center_values[2]) / siSI_scale_values[2]

siSI_test$hit_angle <- (siSI_test$hit_angle - siSI_center_values[3]) / siSI_scale_values[3]

siSI_test$hc_x <- (siSI_test$hc_x - siSI_center_values[4]) / siSI_scale_values[4]

siSI_test$hc_y <- (siSI_test$hc_y - siSI_center_values[5]) / siSI_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

siSI_validate$hit_distance_sc <- (siSI_validate$hit_distance_sc - siSI_center_values[1]) / siSI_scale_values[1]

siSI_validate$hit_speed <- (siSI_validate$hit_speed - siSI_center_values[2]) / siSI_scale_values[2]

siSI_validate$hit_angle <- (siSI_validate$hit_angle - siSI_center_values[3]) / siSI_scale_values[3]

siSI_validate$hc_x <- (siSI_validate$hc_x - siSI_center_values[4]) / siSI_scale_values[4]

siSI_validate$hc_y <- (siSI_validate$hc_y - siSI_center_values[5]) / siSI_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(siSI_train)

set.seed(42)
siSI_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(siSI_train, -row), ntree = 501, importance = TRUE)

print(siSI_rf.1)

plot(siSI_rf.1)

varImpPlot(siSI_rf.1)

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
siSI_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(siSI_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(siSI_rf.5)

plot(siSI_rf.5)

varImpPlot(siSI_rf.5)

siSI_predict_fit.rf.5 <- data.frame(fits = predict(siSI_rf.5, siSI_test, type = "prob")[,2], actuals = siSI_test$hit)

siSI_pred.rf.5 <- prediction(siSI_predict_fit.rf.5$fits, siSI_predict_fit.rf.5$actuals)

siSI_roc.pred.rf.5 <- performance(siSI_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(siSI_roc.pred.rf.5)
abline(a = 0, b = 1)
siSI_opt <- opt.cut(siSI_roc.pred.rf.5, siSI_pred.rf.5)
siSI_opt
siSI_opt <- siSI_opt[3]
siSI_predict_fit.rf.5$fits <- with(siSI_predict_fit.rf.5, ifelse(fits > siSI_opt, 1, 0)) 

str(siSI_test)

#siSI_test$fieldingTeam <- as.character(siSI_test$fieldingTeam)
#siSI_test$home_team <- as.character(siSI_test$home_team)
#siSI_test$hit <- as.logical(siSI_test$hit)
#siSI_test$stand <- as.logical(siSI_test$stand)
str(siSI_test)
siSI_rf.5_confusion_test <- confusionMatrix(model = siSI_rf.5, x = siSI_test, y = siSI_test$hit)
siSI_rf.5_confusion_validate <- confusionMatrix(model = siSI_rf.5, x = siSI_validate, y = siSI_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


siSI_test_rows <- siSI_test$row
siSI_validate_rows <- siSI_validate$row
siSI_pred_data_emp_1 <- ungroup(siSI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% siSI_test_rows)

siSI_pred_data_emp <- ungroup(siSI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% siSI_validate_rows) %>%
	rbind(siSI_pred_data_emp_1)

siSI_out_of_training_rows <- siSI_pred_data_emp$row

siSI_rf.5_full_out_of_sample_data <- ungroup(siSI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% siSI_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



siSI_rf.5_full_out_of_sample_data_scaled <- siSI_rf.5_full_out_of_sample_data

siSI_rf.5_full_out_of_sample_data_scaled$hit_speed <- (siSI_rf.5_full_out_of_sample_data_scaled$hit_speed - siSI_center_values[2]) / siSI_scale_values[2]

siSI_rf.5_full_out_of_sample_data_scaled$hit_angle <- (siSI_rf.5_full_out_of_sample_data_scaled$hit_angle - siSI_center_values[3]) / siSI_scale_values[3]

siSI_rf.5.prob <- predict(siSI_rf.5, siSI_rf.5_full_out_of_sample_data_scaled, type = "response")

siSI_rf.5_full_out_of_sample_data <- cbind(filter(siSI_rf.5_full_out_of_sample_data, row %in% siSI_out_of_training_rows), siSI_rf.5.prob)

names(siSI_rf.5_full_out_of_sample_data)[65] <- "fits"
names(siSI_rf.5_full_out_of_sample_data)

siSI_rf.5_full_out_of_sample_data_reduced <- siSI_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

siSI_rf.5_mean_table <- siSI_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

siSI_rf.5_full_out_of_sample_data$type <- with(siSI_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

siSI_rf.5_full_out_of_sample_data$hit_label <- with(siSI_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

siSI_rf.5_plot1 <- ggplot(siSI_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

siSI_rf.5_plot1

siSI_rf.5_plot2 <- ggplot(siSI_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

siSI_rf.5_plot2

siSI_rf.5_plot3 <- ggplot(siSI_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

siSI_rf.5_plot3

siSI_grid_plot_rf.5 <- grid.arrange(siSI_rf.5_plot1, siSI_rf.5_plot2, siSI_rf.5_plot3, ncol = 3)
siSI_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

siSI_test_rows <- siSI_test$row
siSI_validate_rows <- siSI_validate$row
siSI_pred_data_emp_1 <- ungroup(siSI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% siSI_test_rows)

siSI_pred_data_emp <- ungroup(siSI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% siSI_validate_rows) %>%
	rbind(siSI_pred_data_emp_1)

siSI_out_of_training_rows <- siSI_pred_data_emp$row

siSI_pred_data_emp <- ungroup(siSI) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% siSI_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

siSI_pred_data_emp_scaled <- siSI_pred_data_emp

siSI_pred_data_emp_scaled$hit_speed <- (siSI_pred_data_emp_scaled$hit_speed - siSI_center_values[2]) / siSI_scale_values[2]

siSI_pred_data_emp_scaled$hit_angle <- (siSI_pred_data_emp_scaled$hit_angle - siSI_center_values[3]) / siSI_scale_values[3]

siSI_pred_prob_hit <- predict(siSI_rf.5, siSI_pred_data_emp_scaled, type = "prob")[,2]
siSI_pred_prob_hit_fits <- cbind(siSI_pred_data_emp, siSI_pred_prob_hit)

siSI_pred_prob_hit_fits[,c(1:2)] <- siSI_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

siSI_angle_speed_pred <- siSI_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = siSI_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nSI-SI Hit Probability\n") + theme_bp_grey()

siSI_angle_speed_pred

#	Slider - Slider

slSL$hit_distance_sc <- as.numeric(slSL$hit_distance_sc, na.rm = TRUE)
slSL$hit_angle <- as.numeric(slSL$hit_angle, na.rm = TRUE)
slSL$hit_speed <- as.numeric(slSL$hit_speed, na.rm = TRUE)

slSL$hit <- with(slSL, ifelse(grepl("Single", slSL$events), 1,
															ifelse(grepl("Double", slSL$events), 1,
																		 ifelse(grepl("Triple", slSL$events), 1, 
																		 			 ifelse(grepl("Home Run", slSL$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

slSL$fieldingTeam <- with(slSL, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[slSL$hit_distance_sc == "null"] = NA
#slSL$hit_speed[slSL$hit_speed == "null"] = NA
#slSL$hit_angle[slSL$hit_angle == "null"] = NA

# include row names for unique record identification

slSL$row <- row.names(slSL) %>% as.numeric()

# recode stand and home_team as factors

slSL$stand <- as.factor(slSL$stand)
slSL$home_team <- as.factor(slSL$home_team)


slSL$game_date <- as.Date(slSL$game_date)


# subset 

slSL_working_data <- ungroup(slSL) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(slSL_working_data)
str(ungroup(slSL))
table(slSL_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

slSL_working_data <- filter(slSL_working_data, hc_x != 1, hc_y != 1)
head(slSL_working_data)

# create training and test sets
# scaled data

set.seed(42)
slSL_train <- sample_frac(slSL_working_data, .15, replace = FALSE)
slSL_split <- setdiff(slSL_working_data, slSL_train)
slSL_test <- sample_frac(slSL_split, .50, replace = FALSE)
slSL_validate <- setdiff(slSL_split, slSL_test)

nrow(slSL_train) + nrow(slSL_test) + nrow(slSL_validate) == nrow(slSL_working_data)

with(slSL_train, table(hit)) %>% prop.table()
with(slSL_test, table(hit)) %>% prop.table()
with(slSL_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(slSL_train)

##########################################################################################
str(slSL)
##as.numeric(slSL$hit_distance_sc, slSL$hit_speed, slSL$hit_angle)

#slSL_train$hit_distance_sc <- as.numeric(slSL_train$hit_distance_sc)
#slSL_train$hit_speed <- as.numeric(slSL_train$hit_speed)
#slSL_train$hit_angle <- as.numeric(slSL_train$hit_angle)
#View(slSL_train)
slSL_scaled_data <- scale(slSL_train[,c(1:5)])
slSL_scale_values <- attr(slSL_scaled_data, 'scaled:scale')
slSL_scale_values
##########################################################################################
slSL_center_values <- attr(slSL_scaled_data, 'scaled:center')
slSL_center_values
##########################################################################################
slSL_train <- cbind(slSL_scaled_data, select(slSL_train, hit:row))

# save levels for factor variables
slSL_levels_home_team <- levels(slSL_train$home_team)
slSL_levels_stand <- levels(slSL_train$stand)
slSL_levels_fieldingTeam <- levels(slSL_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(slSL_test)

slSL_test$hit_distance_sc <- (slSL_test$hit_distance_sc - slSL_center_values[1]) / slSL_scale_values[1]

slSL_test$hit_speed <- (slSL_test$hit_speed - slSL_center_values[2]) / slSL_scale_values[2]

slSL_test$hit_angle <- (slSL_test$hit_angle - slSL_center_values[3]) / slSL_scale_values[3]

slSL_test$hc_x <- (slSL_test$hc_x - slSL_center_values[4]) / slSL_scale_values[4]

slSL_test$hc_y <- (slSL_test$hc_y - slSL_center_values[5]) / slSL_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

slSL_validate$hit_distance_sc <- (slSL_validate$hit_distance_sc - slSL_center_values[1]) / slSL_scale_values[1]

slSL_validate$hit_speed <- (slSL_validate$hit_speed - slSL_center_values[2]) / slSL_scale_values[2]

slSL_validate$hit_angle <- (slSL_validate$hit_angle - slSL_center_values[3]) / slSL_scale_values[3]

slSL_validate$hc_x <- (slSL_validate$hc_x - slSL_center_values[4]) / slSL_scale_values[4]

slSL_validate$hc_y <- (slSL_validate$hc_y - slSL_center_values[5]) / slSL_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(slSL_train)

set.seed(42)
slSL_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(slSL_train, -row), ntree = 501, importance = TRUE)

print(slSL_rf.1)

plot(slSL_rf.1)

varImpPlot(slSL_rf.1)

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
slSL_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(slSL_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(slSL_rf.5)

plot(slSL_rf.5)

varImpPlot(slSL_rf.5)

slSL_predict_fit.rf.5 <- data.frame(fits = predict(slSL_rf.5, slSL_test, type = "prob")[,2], actuals = slSL_test$hit)

slSL_pred.rf.5 <- prediction(slSL_predict_fit.rf.5$fits, slSL_predict_fit.rf.5$actuals)

slSL_roc.pred.rf.5 <- performance(slSL_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(slSL_roc.pred.rf.5)
abline(a = 0, b = 1)
slSL_opt <- opt.cut(slSL_roc.pred.rf.5, slSL_pred.rf.5)
slSL_opt
slSL_opt <- slSL_opt[3]
slSL_predict_fit.rf.5$fits <- with(slSL_predict_fit.rf.5, ifelse(fits > slSL_opt, 1, 0)) 

str(slSL_test)

#slSL_test$fieldingTeam <- as.character(slSL_test$fieldingTeam)
#slSL_test$home_team <- as.character(slSL_test$home_team)
#slSL_test$hit <- as.logical(slSL_test$hit)
#slSL_test$stand <- as.logical(slSL_test$stand)
str(slSL_test)
slSL_rf.5_confusion_test <- confusionMatrix(model = slSL_rf.5, x = slSL_test, y = slSL_test$hit)
slSL_rf.5_confusion_validate <- confusionMatrix(model = slSL_rf.5, x = slSL_validate, y = slSL_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


slSL_test_rows <- slSL_test$row
slSL_validate_rows <- slSL_validate$row
slSL_pred_data_emp_1 <- ungroup(slSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slSL_test_rows)

slSL_pred_data_emp <- ungroup(slSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slSL_validate_rows) %>%
	rbind(slSL_pred_data_emp_1)

slSL_out_of_training_rows <- slSL_pred_data_emp$row

slSL_rf.5_full_out_of_sample_data <- ungroup(slSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slSL_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



slSL_rf.5_full_out_of_sample_data_scaled <- slSL_rf.5_full_out_of_sample_data

slSL_rf.5_full_out_of_sample_data_scaled$hit_speed <- (slSL_rf.5_full_out_of_sample_data_scaled$hit_speed - slSL_center_values[2]) / slSL_scale_values[2]

slSL_rf.5_full_out_of_sample_data_scaled$hit_angle <- (slSL_rf.5_full_out_of_sample_data_scaled$hit_angle - slSL_center_values[3]) / slSL_scale_values[3]

slSL_rf.5.prob <- predict(slSL_rf.5, slSL_rf.5_full_out_of_sample_data_scaled, type = "response")

slSL_rf.5_full_out_of_sample_data <- cbind(filter(slSL_rf.5_full_out_of_sample_data, row %in% slSL_out_of_training_rows), slSL_rf.5.prob)

names(slSL_rf.5_full_out_of_sample_data)[65] <- "fits"
names(slSL_rf.5_full_out_of_sample_data)

slSL_rf.5_full_out_of_sample_data_reduced <- slSL_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

slSL_rf.5_mean_table <- slSL_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

slSL_rf.5_full_out_of_sample_data$type <- with(slSL_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

slSL_rf.5_full_out_of_sample_data$hit_label <- with(slSL_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

slSL_rf.5_plot1 <- ggplot(slSL_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

slSL_rf.5_plot1

slSL_rf.5_plot2 <- ggplot(slSL_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

slSL_rf.5_plot2

slSL_rf.5_plot3 <- ggplot(slSL_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

slSL_rf.5_plot3

slSL_grid_plot_rf.5 <- grid.arrange(slSL_rf.5_plot1, slSL_rf.5_plot2, slSL_rf.5_plot3, ncol = 3)
slSL_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

slSL_test_rows <- slSL_test$row
slSL_validate_rows <- slSL_validate$row
slSL_pred_data_emp_1 <- ungroup(slSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slSL_test_rows)

slSL_pred_data_emp <- ungroup(slSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slSL_validate_rows) %>%
	rbind(slSL_pred_data_emp_1)

slSL_out_of_training_rows <- slSL_pred_data_emp$row

slSL_pred_data_emp <- ungroup(slSL) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% slSL_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

slSL_pred_data_emp_scaled <- slSL_pred_data_emp

slSL_pred_data_emp_scaled$hit_speed <- (slSL_pred_data_emp_scaled$hit_speed - slSL_center_values[2]) / slSL_scale_values[2]

slSL_pred_data_emp_scaled$hit_angle <- (slSL_pred_data_emp_scaled$hit_angle - slSL_center_values[3]) / slSL_scale_values[3]

slSL_pred_prob_hit <- predict(slSL_rf.5, slSL_pred_data_emp_scaled, type = "prob")[,2]
slSL_pred_prob_hit_fits <- cbind(slSL_pred_data_emp, slSL_pred_prob_hit)

slSL_pred_prob_hit_fits[,c(1:2)] <- slSL_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

slSL_angle_speed_pred <- slSL_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = slSL_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nSL-SL Hit Probability\n") + theme_bp_grey()

slSL_angle_speed_pred

# Changeup - Changeup

chCH$hit_distance_sc <- as.numeric(chCH$hit_distance_sc, na.rm = TRUE)
chCH$hit_angle <- as.numeric(chCH$hit_angle, na.rm = TRUE)
chCH$hit_speed <- as.numeric(chCH$hit_speed, na.rm = TRUE)

chCH$hit <- with(chCH, ifelse(grepl("Single", chCH$events), 1,
															ifelse(grepl("Double", chCH$events), 1,
																		 ifelse(grepl("Triple", chCH$events), 1, 
																		 			 ifelse(grepl("Home Run", chCH$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

chCH$fieldingTeam <- with(chCH, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[chCH$hit_distance_sc == "null"] = NA
#chCH$hit_speed[chCH$hit_speed == "null"] = NA
#chCH$hit_angle[chCH$hit_angle == "null"] = NA

# include row names for unique record identification

chCH$row <- row.names(chCH) %>% as.numeric()

# recode stand and home_team as factors

chCH$stand <- as.factor(chCH$stand)
chCH$home_team <- as.factor(chCH$home_team)


chCH$game_date <- as.Date(chCH$game_date)


# subset 

chCH_working_data <- ungroup(chCH) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(chCH_working_data)
str(ungroup(chCH))
table(chCH_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

chCH_working_data <- filter(chCH_working_data, hc_x != 1, hc_y != 1)
head(chCH_working_data)

# create training and test sets
# scaled data

set.seed(42)
chCH_train <- sample_frac(chCH_working_data, .15, replace = FALSE)
chCH_split <- setdiff(chCH_working_data, chCH_train)
chCH_test <- sample_frac(chCH_split, .50, replace = FALSE)
chCH_validate <- setdiff(chCH_split, chCH_test)

nrow(chCH_train) + nrow(chCH_test) + nrow(chCH_validate) == nrow(chCH_working_data)

with(chCH_train, table(hit)) %>% prop.table()
with(chCH_test, table(hit)) %>% prop.table()
with(chCH_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
head(chCH_train)

##########################################################################################
str(chCH)
##as.numeric(chCH$hit_distance_sc, chCH$hit_speed, chCH$hit_angle)

#chCH_train$hit_distance_sc <- as.numeric(chCH_train$hit_distance_sc)
#chCH_train$hit_speed <- as.numeric(chCH_train$hit_speed)
#chCH_train$hit_angle <- as.numeric(chCH_train$hit_angle)
#View(chCH_train)
chCH_scaled_data <- scale(chCH_train[,c(1:5)])
chCH_scale_values <- attr(chCH_scaled_data, 'scaled:scale')
chCH_scale_values
##########################################################################################
chCH_center_values <- attr(chCH_scaled_data, 'scaled:center')
chCH_center_values
##########################################################################################
chCH_train <- cbind(chCH_scaled_data, select(chCH_train, hit:row))

# save levels for factor variables
chCH_levels_home_team <- levels(chCH_train$home_team)
chCH_levels_stand <- levels(chCH_train$stand)
chCH_levels_fieldingTeam <- levels(chCH_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(chCH_test)

chCH_test$hit_distance_sc <- (chCH_test$hit_distance_sc - chCH_center_values[1]) / chCH_scale_values[1]

chCH_test$hit_speed <- (chCH_test$hit_speed - chCH_center_values[2]) / chCH_scale_values[2]

chCH_test$hit_angle <- (chCH_test$hit_angle - chCH_center_values[3]) / chCH_scale_values[3]

chCH_test$hc_x <- (chCH_test$hc_x - chCH_center_values[4]) / chCH_scale_values[4]

chCH_test$hc_y <- (chCH_test$hc_y - chCH_center_values[5]) / chCH_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

chCH_validate$hit_distance_sc <- (chCH_validate$hit_distance_sc - chCH_center_values[1]) / chCH_scale_values[1]

chCH_validate$hit_speed <- (chCH_validate$hit_speed - chCH_center_values[2]) / chCH_scale_values[2]

chCH_validate$hit_angle <- (chCH_validate$hit_angle - chCH_center_values[3]) / chCH_scale_values[3]

chCH_validate$hc_x <- (chCH_validate$hc_x - chCH_center_values[4]) / chCH_scale_values[4]

chCH_validate$hc_y <- (chCH_validate$hc_y - chCH_center_values[5]) / chCH_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(chCH_train)

set.seed(42)
chCH_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(chCH_train, -row), ntree = 501, importance = TRUE)

print(chCH_rf.1)

plot(chCH_rf.1)

varImpPlot(chCH_rf.1)

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
chCH_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(chCH_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(chCH_rf.5)

plot(chCH_rf.5)

varImpPlot(chCH_rf.5)

chCH_predict_fit.rf.5 <- data.frame(fits = predict(chCH_rf.5, chCH_test, type = "prob")[,2], actuals = chCH_test$hit)

chCH_pred.rf.5 <- prediction(chCH_predict_fit.rf.5$fits, chCH_predict_fit.rf.5$actuals)

chCH_roc.pred.rf.5 <- performance(chCH_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(chCH_roc.pred.rf.5)
abline(a = 0, b = 1)
chCH_opt <- opt.cut(chCH_roc.pred.rf.5, chCH_pred.rf.5)
chCH_opt
chCH_opt <- chCH_opt[3]
chCH_predict_fit.rf.5$fits <- with(chCH_predict_fit.rf.5, ifelse(fits > chCH_opt, 1, 0)) 

str(chCH_test)

#chCH_test$fieldingTeam <- as.character(chCH_test$fieldingTeam)
#chCH_test$home_team <- as.character(chCH_test$home_team)
#chCH_test$hit <- as.logical(chCH_test$hit)
#chCH_test$stand <- as.logical(chCH_test$stand)
str(chCH_test)
chCH_rf.5_confusion_test <- confusionMatrix(model = chCH_rf.5, x = chCH_test, y = chCH_test$hit)
chCH_rf.5_confusion_validate <- confusionMatrix(model = chCH_rf.5, x = chCH_validate, y = chCH_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


chCH_test_rows <- chCH_test$row
chCH_validate_rows <- chCH_validate$row
chCH_pred_data_emp_1 <- ungroup(chCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chCH_test_rows)

chCH_pred_data_emp <- ungroup(chCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chCH_validate_rows) %>%
	rbind(chCH_pred_data_emp_1)

chCH_out_of_training_rows <- chCH_pred_data_emp$row

chCH_rf.5_full_out_of_sample_data <- ungroup(chCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chCH_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



chCH_rf.5_full_out_of_sample_data_scaled <- chCH_rf.5_full_out_of_sample_data

chCH_rf.5_full_out_of_sample_data_scaled$hit_speed <- (chCH_rf.5_full_out_of_sample_data_scaled$hit_speed - chCH_center_values[2]) / chCH_scale_values[2]

chCH_rf.5_full_out_of_sample_data_scaled$hit_angle <- (chCH_rf.5_full_out_of_sample_data_scaled$hit_angle - chCH_center_values[3]) / chCH_scale_values[3]

chCH_rf.5.prob <- predict(chCH_rf.5, chCH_rf.5_full_out_of_sample_data_scaled, type = "response")

chCH_rf.5_full_out_of_sample_data <- cbind(filter(chCH_rf.5_full_out_of_sample_data, row %in% chCH_out_of_training_rows), chCH_rf.5.prob)

names(chCH_rf.5_full_out_of_sample_data)[65] <- "fits"
names(chCH_rf.5_full_out_of_sample_data)

chCH_rf.5_full_out_of_sample_data_reduced <- chCH_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

chCH_rf.5_mean_table <- chCH_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

chCH_rf.5_full_out_of_sample_data$type <- with(chCH_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

chCH_rf.5_full_out_of_sample_data$hit_label <- with(chCH_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

chCH_rf.5_plot1 <- ggplot(chCH_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

chCH_rf.5_plot1

chCH_rf.5_plot2 <- ggplot(chCH_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

chCH_rf.5_plot2

chCH_rf.5_plot3 <- ggplot(chCH_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

chCH_rf.5_plot3

chCH_grid_plot_rf.5 <- grid.arrange(chCH_rf.5_plot1, chCH_rf.5_plot2, chCH_rf.5_plot3, ncol = 3)
chCH_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

chCH_test_rows <- chCH_test$row
chCH_validate_rows <- chCH_validate$row
chCH_pred_data_emp_1 <- ungroup(chCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chCH_test_rows)

chCH_pred_data_emp <- ungroup(chCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chCH_validate_rows) %>%
	rbind(chCH_pred_data_emp_1)

chCH_out_of_training_rows <- chCH_pred_data_emp$row

chCH_pred_data_emp <- ungroup(chCH) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% chCH_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

chCH_pred_data_emp_scaled <- chCH_pred_data_emp

chCH_pred_data_emp_scaled$hit_speed <- (chCH_pred_data_emp_scaled$hit_speed - chCH_center_values[2]) / chCH_scale_values[2]

chCH_pred_data_emp_scaled$hit_angle <- (chCH_pred_data_emp_scaled$hit_angle - chCH_center_values[3]) / chCH_scale_values[3]

chCH_pred_prob_hit <- predict(chCH_rf.5, chCH_pred_data_emp_scaled, type = "prob")[,2]
chCH_pred_prob_hit_fits <- cbind(chCH_pred_data_emp, chCH_pred_prob_hit)

chCH_pred_prob_hit_fits[,c(1:2)] <- chCH_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

chCH_angle_speed_pred <- chCH_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = chCH_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nCH-CH Hit Probability\n") + theme_bp_grey()

chCH_angle_speed_pred
