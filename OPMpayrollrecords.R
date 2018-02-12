#Jason Goldrosen
#2-9-18
library(readr)
library(data.table)
library(ggplot2)
library(RCurl)
#Documenation for data available: https://ia800608.us.archive.org/16/items/opm-federal-employment-data/docs/2015-02-11-opm-foia-response.pdf

#Clear working environment
rm(list = ls())


#STATUS Files: Quarterly employee-level snapshots of the Federal Workforce 
opmheaders1973_2014status <- c("pseudoid", "name", "date", "agency", "station", "age",
                               "educ","payplan","grade","loslevel", "occ", "occcategory","adjbasicpay","supervisorystat","typeofappt","worksched","NSFTP") # No years since degree
opmwidth1973_2014status <-  c(9, 23, 8, 4, 9, 6, 2, 2, 2, 6, 4, 1, 6, 1, 2, 1, 1)  
subagencies <- fread("/Volumes/Samsung USB/Federal_Agencies.txt", sep=",", header = TRUE)
agencies <- subagencies[, .SD[1], by = AGY]


years <- c(seq(1975,1995, by=5))
for (yyyy in years) {
path_nondod  <- paste0("/Volumes/Samsung USB/Status_Non_DoD_",yyyy,"_12",".txt")
path_dod <- paste0("/Volumes/Samsung USB/Status_DoD_",yyyy,"_12",".txt")

nondod <- read_fwf(
  file=path_nondod,
  #n_max = 10000,
  fwf_widths(opmwidth1973_2014status, col_names = opmheaders1973_2014status))

dod <- read_fwf(
  file=path_dod,
  #n_max = 10000,
  fwf_widths(opmwidth1973_2014status, col_names = opmheaders1973_2014status))

employees <- rbind(data.table(dod), data.table(nondod))

loslevel <- employees[ , .(.N, year = yyyy), loslevel]
age <- employees[ , .(.N, year = yyyy), age]
educ <- employees[ , .(.N, year = yyyy), educ]
agency <- employees[ , .(.N, year = yyyy), substr(agency,1,2)]
subagency <- employees[ , .(.N, year = yyyy), agency]


if (yyyy == years[1]) {
  loslevel_panel <-  loslevel
  age_panel <-  age
  educ_panel <-  educ
  agency_panel <-  agency
  subagency_panel <-  subagency
} else {
  loslevel_panel <- rbind( loslevel_panel, loslevel)
  age_panel <- rbind(age_panel,age)
  educ_panel <- rbind(educ_panel,educ)
  agency_panel <- rbind(agency_panel,agency)
  subagency_panel <- rbind(subagency_panel,subagency)
}

}



