#Jason Goldrosen
#2-13-18
library(readr)
library(data.table)
library(dplyr)
library(ggplot2)
library(RCurl)
#Documenation for data available: https://ia800608.us.archive.org/16/items/opm-federal-employment-data/docs/2015-02-11-opm-foia-response.pdf
#Clear working environment
rm(list = ls())

raw.data.path <- "/Volumes/Samsung USB"

#STATUS Files: Quarterly employee-level snapshots of the Federal Workforce 
opmheaders1973_2014status <- c("pseudoid", "name", "date", "agency", "station", "age",
                               "educ","payplan","grade","loslevel", "occ", "occcategory",
                               "adjbasicpay","supervisorystat","typeofappt","worksched","NSFTP") # Note: years since degree
opmwidth1973_2014status <-  c(9, 23, 8, 4, 9, 6, 2, 2, 2, 6, 4, 1, 6, 1, 2, 1, 1)  

#Vectir of years to pull
years <- c(seq(1975,2010, by=5),2013)

#Separate dataset listing names of each federal agency
subagencies <- fread("/Volumes/Samsung USB/Federal_Agencies.txt", sep=",", header = TRUE)
agencies <- subagencies[, .SD[1], by = AGY]

#yyyy <-1975
for (yyyy in years) {
path.nondod  <- paste0(raw.data.path,"/Status_Non_DoD_",yyyy,"_12",".txt")
path.dod <- paste0(raw.data.path,"/Status_DoD_",yyyy,"_12",".txt")

nondod <- read_fwf(
  file=path.nondod,
  #n_max = 10000,
  fwf_widths(opmwidth1973_2014status, col_names = opmheaders1973_2014status))

dod <- read_fwf(
  file=path.dod,
  #n_max = 10000,
  fwf_widths(opmwidth1973_2014status, col_names = opmheaders1973_2014status))

employees <- rbind(data.table(dod), data.table(nondod))

loslevel <- employees[ , .(.N, year = yyyy), loslevel]
age <- employees[ , .(.N, year = yyyy), age]
educ <- employees[ , .(.N, year = yyyy), educ]
agency <- employees[ , .(.N, year = yyyy), substr(agency,1,2)]
subagency <- employees[ , .(.N, year = yyyy), agency]


if (yyyy == years[1]) {
  loslevel.panel <-  loslevel
  age.panel <-  age
  educ.panel <-  educ
  agency.panel <-  agency
  subagency.panel <-  subagency
} else {
  loslevel.panel <- rbind(loslevel.panel, loslevel)
  age.panel <- rbind(age.panel,age)
  educ.panel <- rbind(educ.panel,educ)
  agency.panel <- rbind(agency.panel,agency)
  subagency.panel <- rbind(subagency.panel,subagency)
}
}

#Extend age categories such that they span 10 years rather than 5 (e.g. "25-29" and "30-34" --> "25-34")
age.panel.10years <- data.table(mutate(mutate(age.panel, age.numeric = as.numeric(substr(age,1,2))), age.category = ifelse(age.numeric <= 24, "<25",
                                                                                                         ifelse(age.numeric <=34, "25-34",       
                                                                                                         ifelse(age.numeric <=44, "35-44",        
                                                                                                         ifelse(age.numeric <=54, "45-54",
                                                                                                         ifelse(age.numeric <=64, "55-64", "65+")))))))[ , .(N = sum(N), age.numeric = min(age.numeric)), by = .(year, age.category)][order(year, age.numeric)]
age.panel.10years <-  merge(x=age.panel.10years, y=age.panel.10years[, .(NTOT = sum(N)), by=year], by = "year")[ , share := N/NTOT]





#Age Plot
plot.title <- "Age Distribution of the Federal Workforce"
x.title <- "Age"
y.title <- "Share of Federal Workforce"
footnote <-"Source: OPM"
legend.title <- "Year"
  
age_plot <- ggplot(data=age.panel.10years, aes(x=age.category, y=share, fill=as.factor(year))) 
age_plot + geom_bar(stat="identity", position=position_dodge()) + 
          xlab(x.title) + 
          ylab(y.title) + 
          labs(title = plot.title, caption=footnote) + 
          scale_fill_discrete(guide = guide_legend(title = legend.title))


#Extend age categories such that they span 10 years rather than 5 (e.g. "25-29" and "30-34" --> "25-34")
loslevel.panel.5years <- data.table(mutate(mutate(loslevel.panel, loslevel.numeric= as.numeric(gsub("-","",substr(gsub("< ", "", loslevel),1,2)))), loslevel.category = ifelse(loslevel.numeric <= 4, "0-4",
                                                                                                                                                        ifelse(loslevel.numeric <= 9, "5-9",
                                                                                                                                                        ifelse(loslevel.numeric <= 14, "10-14",
                                                                                                                                                        ifelse(loslevel.numeric <= 19, "15-19",
                                                                                                                                                        ifelse(loslevel.numeric <= 24, "20-24",
                                                                                                                                                        "25+")))))))

loslevel.panel.5years[ , .(N = sum(N), loslevel.numeric = min(loslevel.numeric)), by = .(year, loslevel.category)][order(year, loslevel.numeric)]



mutate(mutate(loslevel.panel, loslevel.numeric= as.numeric(gsub("-","",substr(gsub("< ", "", loslevel),1,2)))), loslevel.category = ifelse(loslevel.numeric <= 4, "0-4","25+"))




(mutate(mutate(loslevel.panel.5years, age.numeric = as.numeric(substr(age,1,2))), age.category = 








loslevel.panel.5years <- data.table(mutate(mutate(age.panel, age.numeric = as.numeric(substr(age,1,2))), age.category = ifelse(age.numeric <= 24, "<25",
                                                                                                                           ifelse(age.numeric <=34, "25-34",       
                                                                                                                                  ifelse(age.numeric <=44, "35-44",        
                                                                                                                                         ifelse(age.numeric <=54, "45-54",
                                                                                                                                                ifelse(age.numeric <=64, "55-64", "65+")))))))[ , .(N = sum(N), age.numeric = min(age.numeric)), by = .(year, age.category)][order(year, age.numeric)]
age.panel.10years <-  merge(x=age.panel.10years, y=age.panel.10years[, .(NTOT = sum(N)), by=year], by = "year")[ , share := N/NTOT]



loslevel.panel <-  merge(x=loslevel.panel, y=loslevel.panel[, .(NTOT = sum(N)), by=year], by = "year")[ , share := N/NTOT]
loslevel.panel[order(year,loslevel), ]

loslevel.plot <- ggplot(data=loslevel.panel, aes(x=loslevel, y=share, fill=as.factor(year))) 
loslevel.plot + geom_bar(stat="identity", position=position_dodge()) 
  

as.numeric(substr(gsub("< ", "", loslevel.panel$loslevel),1,2))


mutate(loslevel.panel, loslevel.numeric = as.numeric(gsub("-","",substr(gsub("< ", "", loslevel),1,2))))

loslevel.panel.test <- loslevel.panel[, loslevel.numeric := as.numeric(gsub("-","",substr(gsub("< ", "", loslevel),1,2)))]

strings <- c(
  "apple", 
  "219 733 8965", 
  "329-293-8753", 
  "Work: 579-499-7527; Home: 543.355.3679"
)
phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"


#Shift with by to show how data have changed

data.table(cbind(c("<1", "1-2", "")))

head(mutate(mtcars, disp_l = disp / 61.0237))
x <- 10
ifelse(x > 9, "x is greater than 9", "x is not greater than 9")

test <- data.frame(id= c(1, 2, 3), age = c("15-19", "35-39", "50-54", "70-74", "25-29", "UNSP"))
test <- mutate(test, age.numeric = as.numeric(substr(age,1,2)))
test <- mutate(test, age.category = ifelse(age.numeric <= 24, "<25", 
                                    ifelse(age.numeric <=34, "25-34", 
                                    ifelse(age.numeric <=44, "35-44", 
                                    ifelse(age.numeric <=54, "45-54",
                                    ifelse(age.numeric <=64, "55-64", "65+"))))))
                                         