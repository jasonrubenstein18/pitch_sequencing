#	Install Baseball Prospectus Data
library(readr)
bpstats_03_01_2017_1_ <- read_csv("~/Downloads/bpstats_03-01-2017 (1).csv")
View(bpstats_03_01_2017_1_)
TunnelData <- data.frame(bpstats_03_01_2017_1_)

#	Install Player ID List
playerid_list <- read_csv("~/Downloads/playerid_list.csv")
names(playerid_list)

install.packages("dplyr")
library(dplyr)


library(plyr)
TunnelData <- rename(TunnelData, c("LVL"="League", "X1st.Pitch.Type"="First_Pitch", "X2nd.Pitch.Type"="pitch_type", "Break.Tunnel"="Break Tunnel", "Flight.Time.Diff"="Flight Time Differential", "NAME"="Pitcher Name", "PITCHER"="Pitcher ID", "PITCHES"="Pitcher Pitch Total", "PITCH_SEQUENCE"="Pitch Sequence", "Plate.Diff"="Plate Differential", "Release.Diff"="Release Differential", "Post.tunnel.Break"="Post Tunnel Break", "Release.Tunnel"="Release Tunnel", "TEAM"="Team", "Tunnel.Differential"="Tunnel Differential", "YEAR"="Year"))
TunnelData <- subset(TunnelData, TunnelData$`Release Differential` > 0 & TunnelData$`Pitcher Pitch Total` > 15)
names(TunnelData)
View(TunnelData)

rm(bpstats_03_01_2017_1_)

install.packages("pitchRx")
library(ggplot2)
library(pitchRx)

#	Scrape PitchFX Data
PitchFX <- scrape(start = "2016-4-01", end = "2016-04-01")
head(PitchFX)
View(PitchFX)
install.packages("dplyr")
PitchFXatbat <- data.frame(PitchFX$atbat)
names(PitchFXatbat)
PitchFXatbat <- rename(PitchFXatbat, c("pitcher" = "MLBCODE"))
names(PitchFXatbat)											
 
#	Separate PitchFX Data Tables
PitchFXaction <- data.frame(PitchFX$action)
PitchFXpitch <- data.frame(PitchFX$pitch)
PitchFXpo <- data.frame(PitchFX$po)
PitchFXrun <- data.frame(PitchFX$runner)

#	Rename Tunnel Data to match ID List
library(plyr)  
TunnelData <- rename(TunnelData, c("Pitcher ID" = "PLAYERID"))
names(TunnelData)

#	Merge Tunnel and ID List
TunnelandIDList <- merge(TunnelData, playerid_list, by="PLAYERID")

head(PitchFXatbat)
head(playerid_list)
View(PitchFXatbat)

#	Merge PitchFXatbat with PitchFXpitch
PitchFXmerged <- merge(PitchFXatbat, PitchFXpitch, by = "url")
head(PitchFXmerged,11)

#	Create new column First_Pitch
PitchFXmerged["First_Pitch"] <- NA
head(PitchFXmerged)

# Merge PitchFXmergeID and TunnelDataID
install.packages("data.table")
library(data.table)
PitchFXmerged <- merge(PitchFXmerged, playerid_list, by = "MLBCODE")

#	Form pitch sequence to match Tunnel Data
PFXm <- mutate(PitchFXmerged, First_Pitch = lag(pitch_type, shift = 1))
head(PFXm,11)
View(PFXm)
#	Rename to match Pitch FX names for pitch sequence
TunnelandIDList <- plyr::rename(TunnelandIDList, c("1st Pitch Type" = "First_Pitch", "2nd Pitch Type" = "pitch_type"))

FullStack <-ldply(list(PFXm, TunnelandIDList), data.frame)
head(FullStack,1100)

FullStack <- rbind.fill(PFXm, TunnelandIDList)
head(FullStack)

#SABRMerge <- mutate(PitchFXmerged, First_Pitch = lag(pitch_type, shift = 2))
#head(SABRMerge)
#SABRMerge["New_At_Bat"] <- NA

#NewMerge <- mutate(SABRMerge, New_At_Bat = lag(type))
#head(NewMerge, 50)
#View(NewMerge)

#AtBatType <- NewMerge$New_At_Bat

#Coffee <- function(fac){
#	if (lag(fac) == "X")
#		fac = 5
#	else 
#		fac = 6
#}

#Markers <- ifelse(Coffee1 == 0, NA, lag(SABRMerge$pitch_type))
#Markers

#Coffee1 <- ifelse(lag(AtBatType) == "X", 0, 1)
#Coffee1
#head(NewMerge,11)

#Coffee <- lapply(NewMerge[85], Coffee1)

#SABRMerge <- merge(SABRMerge, TunnelandIDList, by = "First_Pitch" & "pitch_type" & "MLBCODE")


