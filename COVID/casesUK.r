# remotes::install_github("publichealthengland/coronavirus-dashboard-api-R-sdk")
library(ukcovid19)
library(dplyr) # for %>% and mutate
library(ggplot2)
library(readxl)
# also needs zoo for rollmean, curl for curl_download

if (!file.exists("ukmidyearestimates20192020ladcodes.xls")) {
  # temp <- tempfile()
  source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2020localauthoritydistrictcodes/ukmidyearestimates20192020ladcodes.xls"
  curl::curl_download(url=source, destfile="ukmidyearestimates20192020ladcodes.xls", quiet=FALSE, mode="wb")
}

pop <- read_excel("ukmidyearestimates20192020ladcodes.xls", sheet="MYE2 - Persons", range="A6:D431", col_names = c("code","name","geography","pop"))

if (!file.exists("casesByLA.csv"))
{
  # LTLA
  casesLower <- get_data(filters="areaType=ltla", structure=list(date="date",name="areaName",code="areaCode",cases="newCasesBySpecimenDate"))
  casesLower <- casesLower %>% mutate(date=as.Date(date)) %>% filter(date>as.Date("2020-03-19"))
  
  # UTLA : not needed because unitaries etc all appear at LTLA level as well
  # cases <- get_data(filters="areaType=utla", structure=list(date="date",name="areaName",code="areaCode",cases="newCasesBySpecimenDate"))
  # cases <- cases %>% mutate(date=as.Date(date)) %>% filter(date>as.Date("2020-03-19"))
  # casesAll <- rbind(cases, casesLower)
  
  casesByLA <- casesLower %>% filter (
                                  startsWith(code, "S12") | # Scottish council area
                                  startsWith(code, "N09") | # NI local government district
                                  startsWith(code, "E09") | # London borough
                                  startsWith(code, "E08") | # Metropolitan district
                                  startsWith(code, "E07") | # Non-metropolitan district
                                  startsWith(code, "E06") | # Unitary authority
                                  startsWith(code, "W06")   # Welsh principal area
                                    )
  
  bucksCodes <- c("E07000005", "E07000006", "E07000007", "E07000004")
  
  # Buckinghamshire went unitary in 2020 but still reports as districts as of 02/11/2020 : consolidate them
  casesByLA <- casesByLA %>% mutate(date=date,
                                     name=case_when(code %in% bucksCodes ~ "Buckinghamshire", TRUE ~ name),
                                     code=case_when(code %in% bucksCodes ~ "E06000060", TRUE ~ code),
                                     cases=cases) %>% group_by(date, code, name) %>% summarise(cases=sum(cases)) %>% ungroup()
  
  
  
  #Merge into case data
  casesByLA <- casesByLA %>% left_join(pop[, c("code","pop")], by="code") %>% arrange(date) 
  #y = DF2[ , c("Client", "LO")],
  
  
  
  # compute seven day rolling mean
  casesByLARM <- casesByLA %>%
                  group_by(code) %>%
                  mutate(date=as.Date(date) + 3, name=name, code=code, cases=zoo::rollsum(cases, k=7, fill=NA)) %>%
                  mutate(caserate=cases*100000/pop) %>%
                  ungroup() %>%
                  filter(date<=max(casesByLA$date))
  
  write.csv(casesByLARM,"casesByLA.csv")
  
} else {
  casesByLARM <- read.csv("casesByLA.csv")
}

casesByLARM$date <- as.Date(casesByLARM$date)
casesByLARM$code <- as.character(casesByLARM$code)

cropLagDays <- 6
latestDate = max(casesByLARM$date) - cropLagDays;
maxCaseRate = max(casesByLARM$caserate, na.rm=TRUE);

windows()

ggplot()+
    geom_line(data=casesByLARM[casesByLARM$date<=(latestDate), ], aes(x=date, y=caserate, group=name), colour="Grey80") +
    geom_line(data=casesByLARM[casesByLARM$date<=(latestDate) & startsWith(casesByLARM$code, "S"), ], aes(x=date,y=caserate,group=name),colour="blue") + 
    geom_line(data=casesByLARM[casesByLARM$date<=(latestDate) & startsWith(casesByLARM$code, "W"), ], aes(x=date,y=caserate,group=name),colour="darkgreen")

# what's the direction of travel over the last week?
caseDelta <- casesByLARM %>% subset(date==latestDate) %>% left_join(casesByLARM[casesByLARM$date == latestDate - 7, c("code", "cases", "caserate")], by="code")

filter_wales = startsWith(caseDelta$code,"W")
filter_scot = startsWith(caseDelta$code,"S")
ratio_wales = 100 * (sum(caseDelta$cases.x[filter_wales]) / sum(caseDelta$cases.y[filter_wales]) - 1)
ratio_scot = 100 * (sum(caseDelta$cases.x[filter_scot]) / sum(caseDelta$cases.y[filter_scot]) - 1)

yeqx <- data.frame(x=c(10, maxCaseRate), y=c(10,maxCaseRate))

windows()

ggplot() +
    scale_x_log10(name=paste("Cases per 100K population in week preceding", format(latestDate - 7, "%d %B"))) + 
    scale_y_log10(name=paste("Cases per 100K population in week preceding", format(latestDate, "%d %B"))) + 
    geom_line(data = yeqx, aes(x=x, y=y)) + 
    geom_point(data=caseDelta, aes(x=caserate.y, y=caserate.x), colour="Grey80") + 
    geom_point(data=caseDelta[startsWith(caseDelta$code, "S"), ], aes(x=caserate.y, y=caserate.x), colour="blue") + 
    geom_point(data=caseDelta[startsWith(caseDelta$code, "W"), ], aes(x=caserate.y, y=caserate.x), colour="green3")+
    ggtitle("National lockdown vs. regional approach:\ncases in Wales (green)\nare still growing faster than Scotland (blue)")
  


