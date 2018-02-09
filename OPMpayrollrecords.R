######################R portion of the program######################
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

years <- c(seq(1975,2010,by=5), 2013)
for (yyyy in years) {
path <- paste0("/Volumes/Samsung USB/Status_Non_DoD_",yyyy,"_12",".txt")

x <- read_fwf(
file=path,
#n_max = 10000,
fwf_widths(opmwidth1973_2014status, col_names = opmheaders1973_2014status))

dt = data.table(x)
dt$N <- 1
age_structure_yyyy <- dt[age!="UNSP",.(employee_count = sum(N), year = yyyy), by=age]
age_structure_yyyy$employee_tot <- sum(age_structure_yyyy[, .(employee_count)])
age_agency_structure_yyyy <- dt[age!="UNSP",.(employee_count = sum(N), year = yyyy), by=.(age, agency)]

if (yyyy == 1975) {
age_structure <- age_structure_yyyy
age_agency_structure <- age_agency_structure_yyyy

} else {
  age_structure <- rbind(age_structure, age_structure_yyyy)
  age_agency_structure <- rbind(age_agency_structure, age_agency_structure_yyyy)
}
}

write.csv(age_structure, "/Volumes/Samsung USB/age_structure.csv", row.names = FALSE)
write.csv(age_agency_structure, "/Volumes/Samsung USB/age_agency_structure.csv", row.names = FALSE)

######################End of Program######################


age_structure$age_share <- age_structure$employee_count/age_structure$employee_tot
age_structure_st_end <- age_structure[(year == 1975 | year == 1985 | year == 1995 | year == 2005 | year==2013)]
g <- ggplot(age_structure_st_end, aes(x=age, y=age_share))
g + geom_bar(aes(fill = factor(year)), position = "dodge", stat="identity")

test <- dt[, .(employee_count = sum(N)), by=.(agency , age)]
test <- dt[, .(employee_count = sum(N)), by=agency]
write.csv(test, "/Volumes/Samsung USB/Age_Distribution_by_Agency.csv", row.names = FALSE)

test2<- data.table(read.csv(
  file="/Volumes/Samsung USB/Federal_Agencies.csv",
  header=TRUE, sep=","))


test3 <- merge(x=test, y=test2, by.x=as.factor(agency), by.y=AGYSUB, all.x = TRUE) 




install
totemployees <- age_structure[, .(max(employee_tot)), by=year]




pathUSBdynamic <- "/Volumes/Samsung USB/SEP2013.NONDOD.FO05M3.TXT"
#Dynamic Analysis -- Changes in entries and exits
opmheaders1973_2014dynamic <- c("pseudoid", "name", "agency", "acc_sep", "effdate", "age", "payplan","grade","loslevel", "station", "occ", "occcategory","adjbasicpay", "typeofappt", "worksched")
opmwidth1973_2014dynamic <-  c(9, 23, 4, 2, 8, 6, 2, 2, 6, 9, 4, 1, 6, 2, 1)
                               




y <- read_fwf(
  file=pathUSBdynamic,
  #n_max = 10000,
  fwf_widths(opmwidth1973_2014dynamic, col_names = opmheaders1973_2014dynamic))


ydt = data.table(y)
ydt$N <- 1
ydt[,.(count = sum(N)),by=acc_sep]



age_structure_st_end
cast(age_structure, age~year, mean) 




# Start the clock!
ptm <- proc.time()

xURL <- read_fwf(
  file="https://ia800608.us.archive.org/16/items/opm-federal-employment-data/data/1973-09-to-2014-06/non-dod/status/Status_Non_DoD_1973_09.txt",
  n_max = 10000,
  fwf_widths(opmwidth1973_2014status, col_names = opmheaders1973_2014status))

# Stop the clock
proc.time() - ptm

dt = data.table(x)
dt$N <- 1
dt[,.(agency_count = sum(N)),by=agency]





x2013 <- read_fwf(
  file="/Volumes/Samsung USB/Status_Non_DoD_2013_12.txt",
  #n_max = 10000,
  fwf_widths(opmwidth1973_2014status, col_names = opmheaders1973_2014status))

x1975 <- read_fwf(
  file="/Volumes/Samsung USB/Status_Non_DoD_1975_12.txt",
 # n_max = 10000,
  fwf_widths(opmwidth1973_2014status, col_names = opmheaders1973_2014status))

x1980 <- read_fwf(
  file="/Volumes/Samsung USB/Status_Non_DoD_1980_12.txt",
  #n_max = 10000,
  fwf_widths(opmwidth1973_2014status, col_names = opmheaders1973_2014status))

x1985 <- read_fwf(
  file="/Volumes/Samsung USB/Status_Non_DoD_1985_12.txt",
  # n_max = 10000,
  fwf_widths(opmwidth1973_2014status, col_names = opmheaders1973_2014status))

x1990 <- read_fwf(
  file="/Volumes/Samsung USB/Status_Non_DoD_1990_12.txt",
  # n_max = 10000,
  fwf_widths(opmwidth1973_2014status, col_names = opmheaders1973_2014status))

x1995 <- read_fwf(
  file="/Volumes/Samsung USB/Status_Non_DoD_1995_12.txt",
  # n_max = 10000,
  fwf_widths(opmwidth1973_2014status, col_names = opmheaders1973_2014status))

x2000 <- read_fwf(
  file="/Volumes/Samsung USB/Status_Non_DoD_2000_12.txt",
  # n_max = 10000,
  fwf_widths(opmwidth1973_2014status, col_names = opmheaders1973_2014status))

x2005 <- read_fwf(
  file="/Volumes/Samsung USB/Status_Non_DoD_2005_12.txt",
  # n_max = 10000,
  fwf_widths(opmwidth1973_2014status, col_names = opmheaders1973_2014status))

x2010 <- read_fwf(
  file="/Volumes/Samsung USB/Status_Non_DoD_2010_12.txt",
  # n_max = 10000,
  fwf_widths(opmwidth1973_2014status, col_names = opmheaders1973_2014status))



xx <- data.table(x1980)
xx <- data.table(rbind(x1975,x1980))

xx <- data.table(c(x1975$pseudoid,x1980$pseudoid, x1985$pseudoid, x1990$pseudoid, x1995$pseudoid, x2000$pseudoid, x2005$pseudoid, x2010$pseudoid, x2013$pseudoid))

xx


x2013 <- read_fwf(
  file="/Volumes/Samsung USB/Status_Non_DoD_2013_12.txt",
  #n_max = 10000,
  fwf_widths(opmwidth1973_2014status, col_names = opmheaders1973_2014status))
x2013_2 <- read_fwf(
  file="/Volumes/Samsung USB/Status_DoD_2013_12.txt",
  #n_max = 10000,
  fwf_widths(opmwidth1973_2014status, col_names = opmheaders1973_2014status))


ggplot(data=xx, aes(as.numeric(adjbasicpay))) + geom_histogram()

xx <-  xx[order(pseudoid),]

sorts
# sample <- read.table("https://ia800608.us.archive.org/16/items/opm-federal-employment-data/data/2016-12-to-2017-03/non-dod/status/Non-DoD%20FOIA%202017-04762%20201703.txt")
# 
# sample <- read.table("https://ia800608.us.archive.org/16/items/opm-federal-employment-data/data/2014-09-to-2016-09/non-dod/status/Non_DoD_201609.txt", header = FALSE, sep = "|", nrows = 100)
# 
# 
# x <- read.fwf(
#   file=url("http://www.cpc.ncep.noaa.gov/data/indices/wksst8110.for"),
#   skip=4,
#   widths=c(12, 7, 4, 9, 4, 9, 4, 9, 4))
