#	4-Seam Fastballs
siFF_angle_speed_pred
knFF_angle_speed_pred
kcFF_angle_speed_pred
fsFF_angle_speed_pred
fcFF_angle_speed_pred
chFF_angle_speed_pred
ftFF_angle_speed_pred
slFF_angle_speed_pred
cuFF_angle_speed_pred

#	2-Seam Fastballs
#knFT_angle_speed_pred
siFT_angle_speed_pred
kcFT_angle_speed_pred
fsFT_angle_speed_pred
fcFT_angle_speed_pred
chFT_angle_speed_pred
slFT_angle_speed_pred
cuFT_angle_speed_pred
ffFT_angle_speed_pred

#	Sliders
siSL_angle_speed_pred
#knSL_angle_speed_pred
kcSL_angle_speed_pred
fsSL_angle_speed_pred
fcSL_angle_speed_pred
chSL_angle_speed_pred
ftSL_angle_speed_pred
cuSL_angle_speed_pred
ffSL_angle_speed_pred

#	Curveballs
siCU_angle_speed_pred
#knCU_angle_speed_pred
kcCU_angle_speed_pred
fsCU_angle_speed_pred
fcCU_angle_speed_pred
chCU_angle_speed_pred
ftCU_angle_speed_pred
slCU_angle_speed_pred
ffCU_angle_speed_pred

#	Changeups
siCH_angle_speed_pred
knCH_angle_speed_pred
kcCH_angle_speed_pred
fsCH_angle_speed_pred
fcCH_angle_speed_pred
ftCH_angle_speed_pred
cuCH_angle_speed_pred
slCH_angle_speed_pred
ffCH_angle_speed_pred

#	Cutters
siFC_angle_speed_pred
#knFC_angle_speed_pred
kcFC_angle_speed_pred
fsFC_angle_speed_pred
chFC_angle_speed_pred
ftFC_angle_speed_pred
slFC_angle_speed_pred
cuFC_angle_speed_pred
ffFC_angle_speed_pred

#	Splitters
siFS_angle_speed_pred
#knFS_angle_speed_pred
#kcFS_angle_speed_pred
fcFS_angle_speed_pred
chFS_angle_speed_pred
ftFS_angle_speed_pred
slFS_angle_speed_pred
#cuFS_angle_speed_pred
ffFS_angle_speed_pred

#	Knuckle Curves
siKC_angle_speed_pred
#knKC_angle_speed_pred
#fsKC_angle_speed_pred
chKC_angle_speed_pred
ftKC_angle_speed_pred
slKC_angle_speed_pred
fcKC_angle_speed_pred
#cuKC_angle_speed_pred
ffKC_angle_speed_pred

#	Knuckleballs
#siKN_angle_speed_pred
#kcKN_angle_speed_pred
#fsKN_angle_speed_pred
#fcKN_angle_speed_pred
#chKN_angle_speed_pred
ftKN_angle_speed_pred
slKN_angle_speed_pred
#cuKN_angle_speed_pred
ffKN_angle_speed_pred

#	Sinkers
#knSI_angle_speed_pred
kcSI_angle_speed_pred
fsSI_angle_speed_pred
fcSI_angle_speed_pred
chSI_angle_speed_pred
ftSI_angle_speed_pred
slSI_angle_speed_pred
cuSI_angle_speed_pred
ffSI_angle_speed_pred



statcast.2016$hit_distance_sc <- as.numeric(statcast.2016$hit_distance_sc, na.rm = TRUE)
statcast.2016$hit_angle <- as.numeric(statcast.2016$hit_angle, na.rm = TRUE)
statcast.2016$hit_speed <- as.numeric(statcast.2016$hit_speed, na.rm = TRUE)

statcast.2016$hit <- with(statcast.2016, ifelse(grepl("Single", statcast.2016$events), 1,
															ifelse(grepl("Double", statcast.2016$events), 1,
																		 ifelse(grepl("Triple", statcast.2016$events), 1, 
																		 			 ifelse(grepl("Home Run", statcast.2016$events), 1, 0))))) %>% 
	as.factor()

# create a variable for the fielding team

statcast.2016$fieldingTeam <- with(statcast.2016, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
	as.factor()

#F$hit_distance_sc[statcast.2016$hit_distance_sc == "null"] = NA
#statcast.2016$hit_speed[statcast.2016$hit_speed == "null"] = NA
#statcast.2016$hit_angle[statcast.2016$hit_angle == "null"] = NA

# include row names for unique record identification

statcast.2016$row <- row.names(statcast.2016) %>% as.numeric()

# recode stand and home_team as factors

statcast.2016$stand <- as.factor(statcast.2016$stand)
statcast.2016$home_team <- as.factor(statcast.2016$home_team)


statcast.2016$game_date <- as.Date(statcast.2016$game_date)


# subset 

statcast.2016_working_data <- ungroup(statcast.2016) %>%
	filter(game_date <= "2016-05-28") %>%
	select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team,  row) %>%
	filter(!is.na(hit_distance_sc)) %>%
	filter(!is.na(hit_angle)) %>% 
	filter(!is.na(hit_speed)) %>%
	arrange(desc(hit))
head(statcast.2016_working_data)
str(ungroup(statcast.2016))
table(statcast.2016_working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

statcast.2016_working_data <- filter(statcast.2016_working_data, hc_x != 1, hc_y != 1)
head(statcast.2016_working_data)

# create training and test sets
# scaled data

set.seed(42)
statcast.2016_train <- sample_frac(statcast.2016_working_data, .15, replace = FALSE)
statcast.2016_split <- setdiff(statcast.2016_working_data, statcast.2016_train)
statcast.2016_test <- sample_frac(statcast.2016_split, .50, replace = FALSE)
statcast.2016_validate <- setdiff(statcast.2016_split, statcast.2016_test)

nrow(statcast.2016_train) + nrow(statcast.2016_test) + nrow(statcast.2016_validate) == nrow(statcast.2016_working_data)

with(statcast.2016_train, table(hit)) %>% prop.table()
with(statcast.2016_test, table(hit)) %>% prop.table()
with(statcast.2016_validate, table(hit)) %>% prop.table()


# normalize exit velocity, launch angle and distance
# scaled features
str(statcast.2016)
##as.numeric(statcast.2016$hit_distance_sc, statcast.2016$hit_speed, statcast.2016$hit_angle)

#statcast.2016_train$hit_distance_sc <- as.numeric(statcast.2016_train$hit_distance_sc)
#statcast.2016_train$hit_speed <- as.numeric(statcast.2016_train$hit_speed)
#statcast.2016_train$hit_angle <- as.numeric(statcast.2016_train$hit_angle)
#View(statcast.2016_train)
statcast.2016_scaled_data <- scale(statcast.2016_train[,c(1:5)])
statcast.2016_scale_values <- attr(statcast.2016_scaled_data, 'scaled:scale')
statcast.2016_scale_values
statcast.2016_center_values <- attr(statcast.2016_scaled_data, 'scaled:center')
statcast.2016_center_values
statcast.2016_train <- cbind(statcast.2016_scaled_data, select(statcast.2016_train, hit:row))

# save levels for factor variables
statcast.2016_levels_home_team <- levels(statcast.2016_train$home_team)
statcast.2016_levels_stand <- levels(statcast.2016_train$stand)
statcast.2016_levels_fieldingTeam <- levels(statcast.2016_train$fieldingTeam)
#levels_first_pitch <- levels(train$first_pitch)
#levels_second_pitch <- levels(train$levels_second_pitch)

# apply scaling to test data
#View(statcast.2016_test)

statcast.2016_test$hit_distance_sc <- (statcast.2016_test$hit_distance_sc - statcast.2016_center_values[1]) / statcast.2016_scale_values[1]

statcast.2016_test$hit_speed <- (statcast.2016_test$hit_speed - statcast.2016_center_values[2]) / statcast.2016_scale_values[2]

statcast.2016_test$hit_angle <- (statcast.2016_test$hit_angle - statcast.2016_center_values[3]) / statcast.2016_scale_values[3]

statcast.2016_test$hc_x <- (statcast.2016_test$hc_x - statcast.2016_center_values[4]) / statcast.2016_scale_values[4]

statcast.2016_test$hc_y <- (statcast.2016_test$hc_y - statcast.2016_center_values[5]) / statcast.2016_scale_values[5]

#test$px <- (test$px - center_values[6])/scale_values[6]

#test$pz <- (test$pz - center_values[7])/scale_values[7]

#test$break_length <- (test$break_length - center_values[8])/scale_values[8]

# apply scaling to validation data

statcast.2016_validate$hit_distance_sc <- (statcast.2016_validate$hit_distance_sc - statcast.2016_center_values[1]) / statcast.2016_scale_values[1]

statcast.2016_validate$hit_speed <- (statcast.2016_validate$hit_speed - statcast.2016_center_values[2]) / statcast.2016_scale_values[2]

statcast.2016_validate$hit_angle <- (statcast.2016_validate$hit_angle - statcast.2016_center_values[3]) / statcast.2016_scale_values[3]

statcast.2016_validate$hc_x <- (statcast.2016_validate$hc_x - statcast.2016_center_values[4]) / statcast.2016_scale_values[4]

statcast.2016_validate$hc_y <- (statcast.2016_validate$hc_y - statcast.2016_center_values[5]) / statcast.2016_scale_values[5]

#validate$px <- (validate$px - center_values[6]) / scale_values[6]

#validate$pz <- (validate$pz - center_values[7])/scale_values[7]

#validate$break_length <- (validate$break_length - center_values[8])/scale_values[8]

# build, test, validate models



# model hit/no-hit random forest
# with all variables
head(statcast.2016_train)

set.seed(42)
statcast.2016_rf.1 <- randomForest(as.factor(hit) ~ ., data = select(statcast.2016_train, -row), ntree = 501, importance = TRUE)

print(statcast.2016_rf.1)

plot(statcast.2016_rf.1)

varImpPlot(statcast.2016_rf.1)

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
statcast.2016_rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(statcast.2016_train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(statcast.2016_rf.5)

plot(statcast.2016_rf.5)

varImpPlot(statcast.2016_rf.5)

statcast.2016_predict_fit.rf.5 <- data.frame(fits = predict(statcast.2016_rf.5, statcast.2016_test, type = "prob")[,2], actuals = statcast.2016_test$hit)

statcast.2016_pred.rf.5 <- prediction(statcast.2016_predict_fit.rf.5$fits, statcast.2016_predict_fit.rf.5$actuals)

statcast.2016_roc.pred.rf.5 <- performance(statcast.2016_pred.rf.5, measure = "tpr", x.measure = "fpr")

plot(statcast.2016_roc.pred.rf.5)
abline(a = 0, b = 1)
statcast.2016_opt <- opt.cut(statcast.2016_roc.pred.rf.5, statcast.2016_pred.rf.5)
statcast.2016_opt
statcast.2016_opt <- statcast.2016_opt[3]
statcast.2016_predict_fit.rf.5$fits <- with(statcast.2016_predict_fit.rf.5, ifelse(fits > statcast.2016_opt, 1, 0)) 

str(statcast.2016_test)

#statcast.2016_test$fieldingTeam <- as.character(statcast.2016_test$fieldingTeam)
#statcast.2016_test$home_team <- as.character(statcast.2016_test$home_team)
#statcast.2016_test$hit <- as.logical(statcast.2016_test$hit)
#statcast.2016_test$stand <- as.logical(statcast.2016_test$stand)
str(statcast.2016_test)
statcast.2016_rf.5_confusion_test <- confusionMatrix(model = statcast.2016_rf.5, x = statcast.2016_test, y = statcast.2016_test$hit)
statcast.2016_rf.5_confusion_validate <- confusionMatrix(model = statcast.2016_rf.5, x = statcast.2016_validate, y = statcast.2016_validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?


statcast.2016_test_rows <- statcast.2016_test$row
statcast.2016_validate_rows <- statcast.2016_validate$row
statcast.2016_pred_data_emp_1 <- ungroup(statcast.2016) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% statcast.2016_test_rows)

statcast.2016_pred_data_emp <- ungroup(statcast.2016) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% statcast.2016_validate_rows) %>%
	rbind(statcast.2016_pred_data_emp_1)

statcast.2016_out_of_training_rows <- statcast.2016_pred_data_emp$row

statcast.2016_rf.5_full_out_of_sample_data <- ungroup(statcast.2016) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% statcast.2016_out_of_training_rows) %>%
	filter(!is.na(hit_speed)) %>%
	filter(!is.na(hit_angle))



statcast.2016_rf.5_full_out_of_sample_data_scaled <- statcast.2016_rf.5_full_out_of_sample_data

statcast.2016_rf.5_full_out_of_sample_data_scaled$hit_speed <- (statcast.2016_rf.5_full_out_of_sample_data_scaled$hit_speed - statcast.2016_center_values[2]) / statcast.2016_scale_values[2]

statcast.2016_rf.5_full_out_of_sample_data_scaled$hit_angle <- (statcast.2016_rf.5_full_out_of_sample_data_scaled$hit_angle - statcast.2016_center_values[3]) / statcast.2016_scale_values[3]

statcast.2016_rf.5.prob <- predict(statcast.2016_rf.5, statcast.2016_rf.5_full_out_of_sample_data_scaled, type = "response")

statcast.2016_rf.5_full_out_of_sample_data <- cbind(filter(statcast.2016_rf.5_full_out_of_sample_data, row %in% statcast.2016_out_of_training_rows), statcast.2016_rf.5.prob)

names(statcast.2016_rf.5_full_out_of_sample_data)[65] <- "fits"
names(statcast.2016_rf.5_full_out_of_sample_data)

statcast.2016_rf.5_full_out_of_sample_data_reduced <- statcast.2016_rf.5_full_out_of_sample_data %>%
	select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

statcast.2016_rf.5_mean_table <- statcast.2016_rf.5_full_out_of_sample_data_reduced %>% 
	group_by(hit, fits) %>%
	summarise_each(funs(mean(., na.rm = TRUE),n()))

statcast.2016_rf.5_full_out_of_sample_data$type <- with(statcast.2016_rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

statcast.2016_rf.5_full_out_of_sample_data$hit_label <- with(statcast.2016_rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

tab_condensed <- c("#006BA4", "#C85200")

statcast.2016_rf.5_plot1 <- ggplot(statcast.2016_rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

statcast.2016_rf.5_plot1

statcast.2016_rf.5_plot2 <- ggplot(statcast.2016_rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nSpeed off the Bat") +
	ylab("Batted Ball Distance (ft.)\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

statcast.2016_rf.5_plot2

statcast.2016_rf.5_plot3 <- ggplot(statcast.2016_rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
	geom_point(aes(color = as.factor(type)), alpha = .2) + 
	xlab("\nLaunch Angle") +
	ylab("Speed off the Bat\n") +
	facet_wrap(~hit_label) + 
	theme_bp_grey() + 
	theme(strip.text.x = element_text(face = "bold", size = 14)) +
	scale_color_manual(values = tab_condensed, guide = FALSE)

statcast.2016_rf.5_plot3

statcast.2016_grid_plot_rf.5 <- grid.arrange(statcast.2016_rf.5_plot1, statcast.2016_rf.5_plot2, statcast.2016_rf.5_plot3, ncol = 3)
statcast.2016_grid_plot_rf.5


#ggsave("grid_plot_rf.5.png", grid_plot_rf.5rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

statcast.2016_test_rows <- statcast.2016_test$row
statcast.2016_validate_rows <- statcast.2016_validate$row
statcast.2016_pred_data_emp_1 <- ungroup(statcast.2016) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% statcast.2016_test_rows)

statcast.2016_pred_data_emp <- ungroup(statcast.2016) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% statcast.2016_validate_rows) %>%
	rbind(statcast.2016_pred_data_emp_1)

statcast.2016_out_of_training_rows <- statcast.2016_pred_data_emp$row

statcast.2016_pred_data_emp <- ungroup(statcast.2016) %>% 
	filter(hc_x != 1, hc_y != 1) %>% 
	filter(row %in% statcast.2016_out_of_training_rows) %>%
	select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
	filter(complete.cases(.))

# empirical data

statcast.2016_pred_data_emp_scaled <- statcast.2016_pred_data_emp

statcast.2016_pred_data_emp_scaled$hit_speed <- (statcast.2016_pred_data_emp_scaled$hit_speed - statcast.2016_center_values[2]) / statcast.2016_scale_values[2]

statcast.2016_pred_data_emp_scaled$hit_angle <- (statcast.2016_pred_data_emp_scaled$hit_angle - statcast.2016_center_values[3]) / statcast.2016_scale_values[3]

statcast.2016_pred_prob_hit <- predict(statcast.2016_rf.5, statcast.2016_pred_data_emp_scaled, type = "prob")[,2]
statcast.2016_pred_prob_hit_fits <- cbind(statcast.2016_pred_data_emp, statcast.2016_pred_prob_hit)

statcast.2016_pred_prob_hit_fits[,c(1:2)] <- statcast.2016_pred_prob_hit_fits[,c(1:2)] %>%
	round(.)

statcast.2016_angle_speed_pred <- statcast.2016_pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = statcast.2016_pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(limit = c(0,1), low = "red", high = "blue", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nFull Hit Probability\n") + theme_bp_grey()

statcast.2016_angle_speed_pred	

