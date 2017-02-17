# # ------------- Downloading and Cleaning ATUS Data from the BLS ----------------
# library(data.table)
# library(utils)
# library(bit64)
# 
# # Downloading data
# temp <- tempfile()
# download.file("https://www.bls.gov/tus/special.requests/atusact_0315.zip", temp)
# timeUse <- fread(unzip(temp, files = "atusact_0315.dat"))
# download.file("https://www.bls.gov/tus/special.requests/atussum_0315.zip", temp)
# demographics <- fread(unzip(temp, files = "atussum_0315.dat"))
# rm(temp)
# 
# # Dropping activity columns from demographics data table (already contained in
# # timeUse data table)
# activityCols <- grep("t", names(demographics))
# demographics[, (activityCols) := NULL]
# 
# # Merging data
# setkey(timeUse, "TUCASEID")
# setkey(demographics, "TUCASEID")
# mergedData <- timeUse[demographics]
# timeUse <- mergedData
# rm(mergedData, demographics, activityCols)
# 
# # -------------------- Dropping Respondents ------------------------------------
# # If you want to clean the data differently, this is where you make your choices
# 
# # Dropping all individuals younger than 18.
# timeUse <- timeUse[TEAGE >= 18]
# 
# # ------------------- Adding variables and labels ------------------------------
# # Generating years, home dummy, and age group (see below for labels)
# timeUse[, `:=`(TUYEAR = as.integer(substr(TUCASEID, 1, 4)),
#                home = ifelse(TEWHERE == -1, -1,
#                              ifelse(TEWHERE == -2, -2,
#                                     ifelse(TEWHERE == -3, -3,
#                                            ifelse(TEWHERE == 1, 1, 2)))),
#                ageGroup = ifelse(TEAGE >= 18 & TEAGE <= 24, 1,
#                                  ifelse(TEAGE >= 25 & TEAGE <= 54, 2,
#                                         ifelse(TEAGE >= 55 & TEAGE <= 64, 3,
#                                                ifelse(TEAGE >= 65, 4, NA)))))]
# 
# # Labeling gender
# timeUse$TESEX <- factor(timeUse$TESEX, levels = c(1, 2),
#                         labels = c("Male", "Female"))
# 
# # Labeling location
# timeUse$home <- factor(timeUse$home, levels = c(-1, -2, -3, 1, 2),
#                        labels = c("Blank", "Don't Know", "Refused", "Home", "Away"))
# 
# # Labeling age groups
# timeUse$ageGroup <- factor(timeUse$ageGroup, levels = c(1, 2, 3, 4),
#                            labels = c("18-24", "25-54", "55-64", "65+"))
# 
# # Labeling metro areas
# timeUse$GTMETSTA <- factor(timeUse$GTMETSTA, levels = c(-1, 1, 2, 3),
#                            labels = c("Blank", "Metro",
#                                       "Non-Metro", "Not Identified"))
# 
# # Labeling employment status
# timeUse$TELFS <- factor(timeUse$TELFS, levels = c(1, 2, 3, 4, 5),
#                         labels = c("Employed - at work", "Employed - absent", 
#                                    "Unemployed - on layoff", "Unemployed - looking",
#                                    "Not in labor force"))
# 
# # Labeling 2 digit activity categories. Note that converting to factors
# # changes the levels to from 1 to 18 instead of from 1 to 16, 18, and 50.
# timeUse$TRTIER1P <- factor(timeUse$TRTIER1P, levels = c(1:16, 18, 50),
#                            labels = activities <- c("Personal Care",
#                                                     "Household Activities",
#                                                     "Caring for and Helping HH Members",
#                                                     "Caring for and Helping non-HH Members",
#                                                     "Work and Related Activities",
#                                                     "Education",
#                                                     "Consumer Purchases",
#                                                     "Professional and Personal Care Services",
#                                                     "Household Services",
#                                                     "Government Services and Civics",
#                                                     "Eating and Drinking",
#                                                     "Socializing, Relaxing, and Leisure",
#                                                     "Sports, Exercise, and Recreation",
#                                                     "Religion",
#                                                     "Volunteering",
#                                                     "Telephone Calls",
#                                                     "Traveling",
#                                                     "Unknown"))
# 
# # Creating variable for weekly hours
# timeUse <- timeUse[, .(weeklyHours = sum(TUACTDUR24) / 60 * 7),
#                    by = .(TUCASEID, TRTIER1P, TRTIER2P, TRCODEP, TEWHERE, GEMETSTA,
#                           GTMETSTA, PEEDUCA, PEHSPNON, PTDTRACE, TEAGE, TELFS,
#                           TESCHENR, TESCHLVL, TESEX, TESPEMPNOT, TRCHILDNUM,
#                           TRDPFTPT, TRERNWA, TRYHHCHILD, TUDIARYDAY, TUFNWGTP,
#                           TUYEAR, home, ageGroup)]
# 
# #-------------- Saving Data ----------------------------------------------------
# # This is file that is saved and loaded to generate the charts.
# #save(timeUse, file = "./timeUseActivity.rda", compress = TRUE)
# save(timeUse, file = "Dropbox/UPenn/Research/TimeUse/time-use/time-use-app/timeUseActivity.rda", 
#      compress = TRUE)
