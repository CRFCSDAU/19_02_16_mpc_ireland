
  library(haven)
  library(tidyverse)
  library(rms)
  library(Hmisc)

# 1. Data -----------------------------------------------------------------------

  data <- read_spss("data/DataFile_Updated_Feb21ST2019-no_password.sav")

# names(data)
# lapply(data, class)
# lapply(data, table)
# lapply(data, function(x) table(is.na(x)))

  names(data) <- make.names(tolower(names(data)))


# 2. Clean/correct/comment data -------------------------------------------------

  data <- rename(data, sex = gender)
  data$sex <- factor(data$sex)

  data$enrolledonscheme <- factor(
    data$enrolledonscheme,
    levels = 0:1,
    labels = c("Yes", "No")
  )

  labs <- c(
    "Graduates of Irish medical schools",
    "Medical Practitioners who graduated in a medical school in the EU and are EU Nationals",
    "Graduated in a medical school in the EU (and they are not an EU National)",
    "International graduates from a medical school outside the EU and Ireland"
  )

  data$category[data$category == 0] <- NA # QUERY 2 values = 0
  data$category <- factor(data$category, levels = 1:4, labels = labs)
  data$category2 <- factor(
    data$category,
    labels = c("Irish", "EU/EU", "EU/Non-EU", "International")
  )

  data$status <- factor(data$status)
# data$intention QUERY Needs lots of work
# data$countryofbmq QUERY Needs cleaning

  data$ireland <- factor(ifelse(data$countryofbmq == "Ireland", "Yes", "No"))

  data$intention <- tolower(data$intention)
  data$intention[nchar(data$intention) > 30] <- NA # QUERY 2 long free text answers
  data$intention[data$intention == "na"] <- NA # QUERY 3 "na"s

  data <- mutate(
    data,
    intends = case_when(
      intention %in% c("intend to comply", "all o.k.") ~ "Intends",
      intention %in% c("probably will comply") ~ "Probably",
      intention %in% c("probably won't comply") ~ "Probably not",
      intention %in% c("intend not to comply") ~ "Intends not",
      intention %in% c("unsure about my intentions") ~ "Unsure"
    )
  )

  data$intends <- ordered(
    data$intends, levels = levels(factor(data$intends))[c(2, 4, 5, 3, 1)]
    )

  data$intends_yn <- factor(
    ifelse(data$intends == "Intends", "Intends", "Other")
    )

  data <- filter(data, !is.na(intends))

# data$aop # QUERY Probably needs some subclassifications to be useful

  data$country_groups <- factor(
    data$country_groups,
    levels = 1:3,
    labels = c("Ireland", "EU", "Non EU")
  )

  data$years_groups <- factor(
    data$years_groups,
    levels = 1:4,
    labels = c("0 – 4", "5 – 14", "15 – 24", "25+")
  )

  data$age_groups <- factor(
    data$age_groups,
    levels = 1:4,
    labels = c("34 and under", "35 – 44", "45 – 54", "55+")
  )

  data$division <- factor(
    gsub(" Registration", "", data$division)
  )

  data$binary_locumexpense[is.na(data$binary_locumexpense)] <- "NA"

# data <- data[complete.cases(data), ]

  data$binary_locumexpense[data$binary_locumexpense == "NA"] <- NA

# 2.1. Additional variables ----------------------------------------------------
# New fields added April 29 2018

  new_data <- read_spss(
    "data/SHARED_with_UCC_April_2019_with_added_ARAF_variables-no_password.sav"
  )
  names(new_data) <- make.names(tolower(names(new_data)))

  new_data <- select(
    new_data, id, contains("qualitifcation"), country:passport_country
  )

  data <- left_join(data, new_data, by = "id")

# New variables
  data$country[nchar(data$country) < 10] <- NA
  data$country <- factor(data$country)

# County is a mess. There are 25 options (once 999 is removed) and 26 labels
# 999 matches rows with other missing data QUERY

# x <- levels(as_factor(data$county))[c(1:27)]
# table(zap_label(data$county))
  data$county[data$county == 999] <- NA
  data$county <- as.numeric(data$county)

  data$county_dublin[!is.na(data$county)] <- "Other"
  data$county_dublin[data$county == 6] <- "Dublin"
  data$county_dublin <- factor(data$county_dublin)

  data$hours[data$hours == 999] <- NA
  data$hours <- droplevels(as_factor(data$hours))

  data$role[data$role == 999] <- NA
  data$role <- droplevels(as_factor(data$role))

  data$servicemodel[nchar(data$servicemodel) < 10] <- NA
  data$servicemodel <- factor(data$servicemodel)

  data$passport_country[nchar(data$passport_country) < 3] <- NA
  data$passport_country <- factor(data$passport_country)
  data$passport_ireland <- factor(ifelse(
      data$passport_country == "Ireland", "Ireland", "Other"
      ))


# 2.1 Clean up likert items ----------------------------------------------------

# 2.1.1 Correct errors
  data$locumexpenseabarrier[data$locumexpenseabarrier == "Not applicable"] <- NA
  #QUERY 1150 "Not applicable"
  data$locumexpenseabarrier <- as.numeric(data$locumexpenseabarrier)

# 2.1.2 Convert to factors with correct labels
# A. Labels
  likert_labs <- c("Strongly disagree",	"Disagree",
                   "Neither agree nor disagree", "Agree",	"Strongly agree")

# B. Select all numeric vars with max value = 4 (the likert items)
  tar <- sapply(
    data[sapply(data, is.numeric)],
    function(x) max(x, na.rm = TRUE) == 4
    )

# C. Convert to factors
  data[sapply(data, is.numeric)][tar] <- map_df(
    data[sapply(data, is.numeric)][tar],
    ordered,
    levels = 0:4,
    labels = likert_labs
  )


# 2.1.3 Add stubs to item names

  names(data)[10:38] <- paste0(
    c(paste0(rep("mpc_", 3), paste0(0, 1:3, "_")),
      paste0(rep("part_", 4), paste0(0, 4:7, "_")),
      paste0(rep("barrier_", 8), c("08_", "09_", paste0(10:15, "_"))),
      paste0(rep("engage_", 4), paste0(17:20, "_")),
      paste0(rep("agree_", 10), paste0(21:30, "_"))),
    names(data)[10:38]
  )

# 2.2 Clean up binary items ----------------------------------------------------
  tar <- grepl("binary", names(data))
  data[tar] <- map_df(data[tar], factor, labels = c("No", "Yes"))

# 2.3 Labels -------------------------------------------------------------------

  label(data$age) <- "Age (years)"

# 4. Save data, clear environment ----------------------------------------------
  # save(data, file = "data.RData")
  # rm(list = ls())
  # load("data.RData")

  save(data, file = "data_full.RData")
  rm(list = ls())
  load("data_full.RData")

# 5. Inspect data --------------------------------------------------------------

# summarytools::view(dfSummary(data))

# 5. Heat map ------------------------------------------------------------------

# heatmap(as.matrix(select(data, starts_with("binary_"))))

# as.matrix(select(data, starts_with("binary_"))) %>% dim()

# df <- data_frame()
# for(i in names(select(data, starts_with("binary_")))){
#   for(j in names(select(data, starts_with("binary_")))){
#
#    x <- table(data[[i]], data[[j]])[4] / sum(table(data[[i]], data[[j]]))
#    df <- bind_rows(
#      df,
#      data_frame(var1 = i, var2 = j, value = x)
#      )
#   }
# }


