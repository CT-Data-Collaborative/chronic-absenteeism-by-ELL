library(dplyr)
library(devtools)
library(datapkg)

##################################################################
#
# Processing Script for Chronic-Absenteeism-by-ELL
# Created by Jenna Daly
# On 03/16/2017
#
##################################################################

#Setup environment
path_to_top_level <- (paste0(getwd(), "/"))
path_to_raw_data <- (paste0(getwd(), "/", "raw"))
all_csvs <- dir(path_to_raw_data, recursive=T, pattern = ".csv") 
all_state_csvs <- dir(path_to_raw_data, recursive=T, pattern = "ct.csv") 
all_dist_csvs <- all_csvs[!all_csvs %in% all_state_csvs]

chronic_absent_dist <- data.frame(stringsAsFactors = F)
chronic_absent_dist_noTrend <- grep("trend", all_dist_csvs, value=T, invert=T)
for (i in 1:length(chronic_absent_dist_noTrend)) {
  current_file <- read.csv(paste0(path_to_raw_data, "/", chronic_absent_dist_noTrend[i]), stringsAsFactors=F, header=F )
  current_file <- current_file[-c(1:2),]
  colnames(current_file) = current_file[which( current_file$V1 %in% c('District', 'Organization')), ]
  current_file = current_file[-1, ]          # removing the first row.
  current_file <- current_file[, !(names(current_file) %in% c("District Code", "Student Count"))]
  get_year <- as.numeric(substr(unique(unlist(gsub("[^0-9]", "", unlist(chronic_absent_dist_noTrend[i])), "")), 1, 4))
  get_year <- paste0(get_year, "-", get_year + 1) 
  current_file$Year <- get_year
  
  # Added by Ilya on 23 March 2021: recent format has both N and %. Correct col name for compatibility:
  names(current_file)[names(current_file) == '%'] <- '% Chronically Absent'
  current_file <- current_file[ current_file$`% Chronically Absent` != '%', ] # drop first row
  
  chronic_absent_dist <- rbind(chronic_absent_dist, current_file)
}

#Rename statewide data...
chronic_absent_dist[["District"]][chronic_absent_dist$"District" %in% c("State Level", 'NOTSET')]<- "Connecticut"

#Relabel ELL column
chronic_absent_dist$`English Learner Status`[chronic_absent_dist$`English Learner Status` == "ELL"] <- "English Language Learner"
chronic_absent_dist$`English Learner Status`[chronic_absent_dist$`English Learner Status` == "Non-ELL"] <- "Non-English Language Learner"

#backfill Districts
district_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-school-district-list/master/datapackage.json'
district_dp <- datapkg_read(path = district_dp_URL)
districts <- (district_dp$data[[1]])

chronic_absent_fips <- merge(chronic_absent_dist, districts, by.x = "District", by.y = "District", all=T)

chronic_absent_fips$District <- NULL

chronic_absent_fips<-chronic_absent_fips[!duplicated(chronic_absent_fips), ]####

#backfill year
years <- c("2011-2012",
           "2012-2013",
           "2013-2014",
           "2014-2015",
           "2015-2016", 
           "2016-2017",
           "2017-2018",
           "2018-2019",
           "2019-2020")

backfill_years <- expand.grid(
  `FixedDistrict` = unique(districts$`FixedDistrict`),
  `English Learner Status` = unique(chronic_absent_dist$`English Learner Status`),
  `Year` = years 
)

backfill_years$FixedDistrict <- as.character(backfill_years$FixedDistrict)
backfill_years$Year <- as.character(backfill_years$Year)

backfill_years <- arrange(backfill_years, FixedDistrict)

complete_chronic_absent <- merge(chronic_absent_fips, backfill_years, by = c("FixedDistrict", "English Learner Status", "Year"), all=T)

complete_chronic_absent <- complete_chronic_absent[order(complete_chronic_absent$FixedDistrict, complete_chronic_absent$Year, complete_chronic_absent$`English Learner Status`),]

#remove duplicated Year rows
complete_chronic_absent <- complete_chronic_absent[!with(complete_chronic_absent, is.na(complete_chronic_absent$Year)),]

#recode missing data with -6666
complete_chronic_absent[["% Chronically Absent"]][is.na(complete_chronic_absent[["% Chronically Absent"]])] <- -6666

#recode suppressed data with -9999
complete_chronic_absent[["% Chronically Absent"]][complete_chronic_absent$"% Chronically Absent" == "*"]<- -9999

#return blank in FIPS if not reported
complete_chronic_absent$FIPS <- as.character(complete_chronic_absent$FIPS)
complete_chronic_absent[["FIPS"]][is.na(complete_chronic_absent[["FIPS"]])] <- ""

#reshape from wide to long format
cols_to_stack <- c("% Chronically Absent")

long_row_count = nrow(complete_chronic_absent) * length(cols_to_stack)

complete_chronic_absent_long <- reshape(complete_chronic_absent,
                                            varying = cols_to_stack,
                                            v.names = "Value",
                                            timevar = "Variable",
                                            times = cols_to_stack,
                                            new.row.names = 1:long_row_count,
                                            direction = "long"
)

#Rename FixedDistrict to District
names(complete_chronic_absent_long)[names(complete_chronic_absent_long) == 'FixedDistrict'] <- 'District'

#reorder columns and remove ID column
complete_chronic_absent_long <- complete_chronic_absent_long[order(complete_chronic_absent_long$District, complete_chronic_absent_long$Year),]
complete_chronic_absent_long$id <- NULL

#Add Measure Type
complete_chronic_absent_long$`Measure Type` <- "Percent"

#Rename Variable columns
complete_chronic_absent_long$`Variable` <- "Chronically Absent Students"

#Order columns
complete_chronic_absent_long <- complete_chronic_absent_long %>% 
  select(`District`, `FIPS`, `Year`, `English Learner Status`, `Variable`, `Measure Type`, `Value`)

#Use this to find if there are any duplicate entires for a given district
test <- complete_chronic_absent_long[,c("District", "Year", "English Learner Status")]
test2<-test[duplicated(test), ]

#Write CSV
write.table(
  complete_chronic_absent_long,
  file.path(path_to_top_level, "data", "chronic_absenteeism_by_ell_2012-2020.csv"),
  sep = ",",
  row.names = F
)