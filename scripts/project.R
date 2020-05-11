# Overview ---------------------------------------------------------------------

# The questionnaire used a 5 point Likert scale for responses indicating
# disagreement/ agreement with a series of statements. We have subsequently
# recoded these into binary responses, grouping the middle group with the
# non-desired response. So that, for example, if the statement was that
# Maintenance of Professional Competence reassures patients and the public,
# then those who strongly disagreed, disagreed or neither agreed nor disagreed
# are grouped together as 0, and those who agreed and strongly agreed are
# grouped together as 1. We were thinking that we would use the 5 point responses
# for description but use the binary responses for comparing demographic
# groups etc.
#
# We have also created some groups using demographic characteristics, and
# these are at the end of the data set.


# 1. To describe doctors attitudes, beliefs and experiences of the regulation
# of professional competence and their intention to comply with requirements
# in future
# 2. To relate the findings to demographic characteristics
# 3. To relate these demographic characteristics, attitudes, beliefs and
# experiences to intention to comply with requirements in future
# 4. Possibly to validate the survey using factor analysis (depending on advice)

# pw APA17811???

# Packages----------------------------------------------------------------------

  pkgs <- c("tidyverse", "openxlsx")
  install.packages(pkgs[!pkgs %in% installed.packages()])

# Sysinfo ----------------------------------------------------------------------

# Scripts/Objectives -----------------------------------------------------------

# Data -------------------------------------------------------------------------

