#Jason Goldrosen
#2-13-18
#The follwing program analyzes the demographic trends of the federal workforce over the last 40 years.
#Documenation for data available: https://ia800608.us.archive.org/16/items/opm-federal-employment-data/docs/2015-02-11-opm-foia-response.pdf

pacman::p_load(readr, data.table, dplyr, ggplot2) 

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
    fwf_widths(opmwidth1973_2014status, col_names = opmheaders1973_2014status)) #All Non-Department of Defense Agencies
  
  dod <- read_fwf(
    file=path.dod,
    #n_max = 10000,
    fwf_widths(opmwidth1973_2014status, col_names = opmheaders1973_2014status)) #All Department of Defense Agencies
  
  employees <- rbind(data.table(dod), data.table(nondod)) 
  
  #Summarize total employment by group
  loslevel <- employees[ , .(.N, year = yyyy), loslevel] #Employment by Length of Service
  age <- employees[ , .(.N, year = yyyy), age] #Employment by Age Range
  educ <- employees[ , .(.N, year = yyyy), educ] # " " Educational Attainment
  agency <- employees[ , .(.N, year = yyyy), by = .(AGY = substr(agency,1,2))] # " " Agency (.e.g., Department of Justice)
  subagency <- employees[ , .(.N, year = yyyy), agency] # " " subagency (e.g., Drug Enforcement Agency within Department of Justice)
  
  #Concatenate the collapsed data sets
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


#####Distribution of Federal Workers by Age#####
#Extend age categories such that they span 10 years rather than 5 (e.g. "25-29" and "30-34" are combined into "25-34")
age.panel.10years <- data.table(mutate(mutate(age.panel, age.numeric = as.numeric(substr(age,1,2))), age.category = ifelse(age.numeric <= 24, "<25",
                                                                                                                    ifelse(age.numeric <=34, "25-34",       
                                                                                                                    ifelse(age.numeric <=44, "35-44",        
                                                                                                                    ifelse(age.numeric <=54, "45-54",
                                                                                                                    ifelse(age.numeric <=64, "55-64", 
                                                                                                                    "65+")))))))[ , .(N = sum(N), age.numeric = min(age.numeric)), by = .(year, age.category)][order(year, age.numeric)]

age.panel.10years <-  merge(x=age.panel.10years, y=age.panel.10years[, .(NTOT = sum(N)), by=year], by = "year")[ , share := N/NTOT]


#Age Plot
plot.title <- "Age Distribution of the Federal Workforce"
x.title <- "Age"
y.title <- "Share of Federal Workforce"
footnote <-"Source: OPM"
legend.title <- "Year"
positions <- c("<25", "25-34", "35-44", "45-54", "55-64", "65+") #Positions along Horizontal Axis

age_plot <- ggplot(data=age.panel.10years, aes(x=age.category, y=share, fill=as.factor(year))) 
age_plot + geom_bar(stat="identity", position=position_dodge()) + 
          xlab(x.title) + 
          ylab(y.title) + 
          labs(title = plot.title, caption=footnote) + 
          scale_fill_discrete(guide = guide_legend(title = legend.title)) +
          scale_x_discrete(limits = positions)


#####Distribution of Federal Workers by Length of Service#####
#Extend length of service categories such that each is 5 years long (e.g. 0-4,5-9, ..., 25+)
loslevel.panel.5years <- data.table(mutate(mutate(loslevel.panel, loslevel.numeric= as.numeric(gsub("-","",substr(gsub("< ", "", loslevel),1,2)))), loslevel.category = ifelse(loslevel.numeric <= 4, "0-4",
                                                                                                                                                                        ifelse(loslevel.numeric <= 9, "5-9",
                                                                                                                                                                        ifelse(loslevel.numeric <= 14, "10-14",
                                                                                                                                                                        ifelse(loslevel.numeric <= 19, "15-19",
                                                                                                                                                                        ifelse(loslevel.numeric <= 24, "20-24",
                                                                                                                                                                        "25+")))))))[ , .(N = sum(N), loslevel.numeric = min(loslevel.numeric)), by = .(year, loslevel.category)][order(year, loslevel.numeric)]

loslevel.panel.5years <-  merge(x=loslevel.panel.5years, y=loslevel.panel.5years[, .(NTOT = sum(N)), by=year], by = "year")[ , share := N/NTOT]

#Length of Service Plot
plot.title <- "Length of Service Distribution of the Federal Workforce"
x.title <- "Years of Service"
y.title <- "Share of Federal Workforce"
footnote <-"Source: OPM"
legend.title <- "Year"
positions <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25+") #Positions along Horizontal Axis

loslevel_plot <- ggplot(data=loslevel.panel.5years, aes(x=loslevel.category, y=share, fill=as.factor(year))) 
loslevel_plot + geom_bar(stat="identity", position=position_dodge()) + 
  xlab(x.title) + 
  ylab(y.title) + 
  labs(title = plot.title, caption=footnote) + 
  scale_fill_discrete(guide = guide_legend(title = legend.title)) + 
  scale_x_discrete(limits = positions)



#####Distribution of Federal Workers by Education#####
#Partion education codes into four categories: HS or Less; Some College, Bachelor's, Graduate Degree
educ.panel.categories <- data.table(mutate(mutate(educ.panel, educ.numeric = as.numeric(educ)), educ.category = ifelse(educ.numeric <=4, "HS or Less", 
                                                                                                                ifelse(educ.numeric <=12, "Some College",
                                                                                                                ifelse(educ.numeric <=14, "Bachelor's",
                                                                                                                ifelse(educ.numeric <=22, "Graduate", NA))))))[ , .(N = sum(N), educ.numeric = min(educ.numeric)), by = .(year, educ.category)][order(year, educ.numeric)] 
                                                                                                                  
educ.panel.categories <-  merge(x=educ.panel.categories[is.na(educ.category) == FALSE], y=educ.panel.categories[is.na(educ.category) == FALSE, .(NTOT = sum(N)), by=year], by = "year")[ , share := N/NTOT]

#Education Plot
plot.title <- "Educational Distribution of the Federal Workforce"
x.title <- "Highest Degree Attained"
y.title <- "Share of Federal Workforce"
footnote <-"Source: OPM"
legend.title <- "Year"
positions <- c("HS or Less", "Some College", "Bachelor's", "Graduate") #Positions along Horizontal Axis

educ.plot <- ggplot(data=educ.panel.categories, aes(x=educ.category, y=share, fill=as.factor(year))) 
educ.plot + geom_bar(stat="identity", position=position_dodge()) + 
  xlab(x.title) + 
  ylab(y.title) + 
  labs(title = plot.title, caption=footnote) + 
  scale_fill_discrete(guide = guide_legend(title = legend.title)) + 
  scale_x_discrete(limits = positions)

  
#####Distribution of Federal Workers by Agency#####
agency.panel.names <- merge(x=agency.panel, y=agencies[ ,.(AGY, AGYTYP, AGYT)], by="AGY", all.x = TRUE, all.y = TRUE)[AGYTYP == 1]

plot.title <- "Federal Employment by Cabinent Agency"
x.title <- "Year"
y.title <- "Total Workforce"
footnote <-"Source: OPM"
legend.title <- "Agency"

agency.plot <-ggplot(agency.panel.names, aes(x = year, y = N, fill = AGYT)) 
agency.plot + geom_area(position = 'stack')
  xlab(x.title) + 
  ylab(y.title) + 
  labs(title = plot.title, caption=footnote) + 
  scale_fill_discrete(guide = guide_legend(title = legend.title))  
