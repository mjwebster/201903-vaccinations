#March 2019
#data from Minnesota Department of Health
#We put in a request for school-level vaccination data that includes the codes that the
#department of education uses for schools 
#MDH publishes school-level vaccination data on its website, but it didn't have the codes
#Ben Christensen, an epidemologist who works with this program, added the codes himself

#there are 5 vaccination files:
#kindergarten 2012-13
#kindergarten 2017-18
#7th grade 2012-13
#7th grade 2017-18
#child care centers 2017-18 (from MDH website)

#We paired the child care center data with the Dept of Human Services data on licensed child care facilities
#Paired the school data with Dept of Education data on enrollment by race, pct of free or reduced lunch
# and address files for public and private schools
#These pieces allowed us to identify schools by type (private, charter or other public),
 #by what county they are located in,
 #by diversity levels and income levels (public schools only)

#We identified a school has being a pocket lacking full immunity (mmr_pocket and var_pocket) 
#if less than 90% of the kids were fully vaccinated (herd immunity for measles is considered 95%)

#For chicken pox, we added together the kids fully vaccinated with those counted as "disease history"
#(meaning they already had the chicken pox) in order to calculate the percentage of kids in a school
#that have immunity (and only schools with less than 90% on this metric are identified as a pocket)



library(readr) #importing csv files
library(dplyr) #general analysis 
library(ggplot2) #making charts
library(lubridate) #date functions
library(reshape2) #use this for melt function to create one record for each team
library(tidyr)
library(janitor) #use this for doing crosstabs
library(scales) #needed for stacked bar chart axis labels
library(knitr) #needed for making tables in markdown page
library(htmltools)#this is needed for Rstudio to display kable and other html code
library(rmarkdown)
library(kableExtra)
library(ggthemes)
library(RMySQL)
library(readxl) #for importing Excel files
library(DT) #needed for making  searchable sortable data tble
library(jsonlite) #for exporting JSON

library(stringr)#for working with strings; trim, substring, etc

options(scipen=999) #this prevents results from displaying as scientific notation

# IMPORT DATA -------------------------------------------------------------



# import from mysql -------------------------------------------------------
con <- dbConnect(RMySQL::MySQL(), host = Sys.getenv("host"), dbname="Schools",user= Sys.getenv("userid"), password=Sys.getenv("pwd"))


#private school list
data1 <- dbSendQuery(con, "select * from nonpublic_addresses_March2019")
#assign it to a new data frame
private_addresses <- fetch(data1, n=-1) %>% clean_names()
dbClearResult(data1)


#public school list
data2 <- dbSendQuery(con, "select * from addresses_Feb2019")
#assign it to a new data frame
public_addresses <- fetch(data2, n=-1) %>% clean_names()
dbClearResult(data2)


#enroll special
data3 <- dbSendQuery(con, "select datayear, schoolid, k12enr, freek12, redk12, pctpoverty 
                            from enroll_special 
                            where grade='All grades' and 
                     (DataYear='17-18' or DataYear='12-13')")
#assign it to a new data frame
enroll_special <- fetch(data3, n=-1) %>% clean_names()
dbClearResult(data3)


#enroll_race
data4 <- dbSendQuery(con, "select datayear, schoolid, SchoolType, SchoolLocationCountyName, TotalStudents, TotalMinority, pctminority from enroll_race where  
                     (DataYear='17-18' or DataYear='12-13')")
#assign it to a new data frame
enroll_race <- fetch(data4, n=-1) %>% clean_names()
dbClearResult(data4)


#SchoolList table
data5 <- dbSendQuery(con, "select * from SchoolList")
#assign it to a new data frame
master_school_list <- fetch(data5, n=-1) %>% clean_names()
dbClearResult(data5)


#districtlist table
data6 <- dbSendQuery(con, "select * from DistrictList")
#assign it to a new data frame
district_list <- fetch(data6, n=-1) %>% clean_names() %>% rename(countyname=county)
dbClearResult(data6)


#disconnect connection
dbDisconnect(con)

rm(data1)
rm(data2)
rm(data3)
rm(data4)
rm(data5)
rm(data6)


#Assign buckets to enroll_race and enroll_special
enroll_race <-  enroll_race %>% mutate(diversity_bucket = case_when(pctminority>=.80~4,
                                                                    pctminority>=.50 & pctminority<.80~3,
                                                                    pctminority>=.25 & pctminority<.5~2,
                                                                    pctminority<.25~1))

enroll_special <-  enroll_special %>% mutate(income_bucket = case_when(pctpoverty>=.80~4,
                                                                       pctpoverty>=.50 & pctpoverty<.80~3,
                                                                       pctpoverty>=.25 & pctpoverty<.5~2,
                                                                       pctpoverty<.25~1))


#winnow down the public and private address lists and merge them into one file
private2 <- private_addresses %>%  distinct(school_id, school_name, physical_city, county, grades, school_classification) %>% mutate(schooltype='private')

#Need to use distinct on this because some schools have more than one record in the table
public2 <-  public_addresses %>%
  filter(school_number!=0) %>% 
  distinct(idnumber, organization, physical_city, county, grades, school_classification) %>% 
  mutate(schooltype='public') %>% 
  rename(school_id=idnumber,school_name=organization)

school_list <-  rbind(private2, public2)



# VACCINATION DATA --------------------------------------------------------


#2012-13 kindergarten
#import file, clean headers, create numeric enrollment field
#create new schoolid field with hyphens to match my school data

kind_old <-  read_excel("./data/Kindergarten_1213.xlsx", 
                        sheet="FILTER_FOR_KINDERGARTEN_121_000", range="A1:AS1238") %>% 
  clean_names() %>% 
  mutate(enroll_new = case_when(enroll=='Enrollment < 5'~0, TRUE~as.double(enroll)),
         schoolID = paste(str_sub(orgidmde,3,6),str_sub(orgidmde, 1,2), str_sub(orgidmde,7,9),sep="-"),
         grade='Kindergarten',
         yr = 2013,
         schoolyr='2012-13',
         districtid =paste(str_sub(orgidmde,3,6),str_sub(orgidmde, 1,2), '000',sep="-"),
         districttype= paste(str_sub(orgidmde,1,2)))

#Fix Andersen Community (only problematic in the 2013 data)
kind_old$districtid[kind_old$orgidmde=='30001190000'] <-  '0001-03-000'
kind_old$districttype[kind_old$orgidmde=='30001190000'] <- '03'
kind_old$orgidmde[kind_old$orgidmde=='30001190000'] <- '03000119000'




#join to the address files
kind_old <- left_join(kind_old, school_list, by=c("schoolID"="school_id"))


#identify whether public or private
kind_old$schooltype[kind_old$districttype=='31'] <-  'private'
kind_old$schooltype[kind_old$districttype=='32'] <-  'private'
kind_old$schooltype[kind_old$districttype=='33'] <-  'private'
kind_old$schooltype[kind_old$districttype=='34'] <-  'private'
kind_old$schooltype[kind_old$districttype=='01'] <-  'public'
kind_old$schooltype[kind_old$districttype=='03'] <-  'public'


#join to the master school list to try to catch any that weren't in the address files
kind_old <- left_join(kind_old, master_school_list %>% 
                        select(school_id, city, school_location_county_name), by = c("schoolID" = "school_id") ) %>%
  mutate(
    physical_city = if_else(is.na(physical_city), city, physical_city),
    county = if_else(is.na(county), str_to_title(school_location_county_name), county)
  ) %>% 
  select(-city, -school_location_county_name)

#join to district list 
kind_old <- left_join(kind_old, district_list %>% 
                        select(id_number, countyname), by = c("districtid" = "id_number") )%>%
  mutate(county = if_else(is.na(county), str_to_title(countyname), county)) %>% 
  select(-countyname)

#this creates a list of counties lined up with district names
temp <-  kind_old %>% filter(county!='NA') %>%  distinct(disname, str_to_title(county)) %>% summarise(count=n()) %>% rename(countyname=county)

#then pulls the county back for any remaining blanks (most appear to be private schools and the district name tells you which district they are located in)
kind_old <- left_join(kind_old, temp %>% 
                        select(disname, countyname), by = c("disname" = "disname") )%>%
  mutate(county = if_else(is.na(county), str_to_title(countyname), county)) %>% 
  select(-countyname)


#get rid of duplicates
kind_old <- kind_old %>% distinct()


# MATCH WITH ENROLL SPECIAL -----------------------------------------------

kind_old <- left_join(kind_old, enroll_special %>% filter(datayear=='12-13'), by=c("schoolID"="schoolid"))

#match with enroll_race
kind_old <- left_join(kind_old, enroll_race %>% filter(datayear=='12-13') %>% 
                        select(schoolid,total_students,total_minority, pctminority), by=c("schoolID"="schoolid"))


#assign income buckets, diversity bucket and flag schools with MMR vaccination rate below 90%,
#and set schools to type2 bucket (charter, private, traditional)
kind_old <-  kind_old %>% mutate(income_bucket = case_when(pctpoverty>=.80~4,
                                                           pctpoverty>=.50 & pctpoverty<.80~3,
                                                           pctpoverty>=.25 & pctpoverty<.5~2,
                                                           pctpoverty<.25~1),
                                 diversity_bucket = case_when(pctminority>=.80~4,
                                                              pctminority>=.50 & pctminority<.80~3,
                                                              pctminority>=.25 & pctminority<.5~2,
                                                              pctminority<.25~1),
                                 mmr_pocket=case_when(complete_pert_mmr>=.895~'no',
                                                      complete_pert_mmr>=0 & complete_pert_mmr<.895~'yes',
                                                      is.na(complete_pert_mmr)~'not reported',
                                                      TRUE~'unk'),
                                 type2 = case_when(districttype=='07' & schooltype=='public'~'charter',
                                                   districttype!='07'& schooltype=='public'~'traditional',
                                                   districttype=='07'& is.na(schooltype)~'charter',
                                                   schooltype=='private'~'private',
                                                   TRUE~'unknown'),
                                 grades=str_trim(grades))


kind_old$complete_mmr[is.na(kind_old$complete_mmr)] <- 0
kind_old$inprogress_mmr[is.na(kind_old$inprogress_mmr)] <- 0
kind_old$co_mmr[is.na(kind_old$co_mmr)] <- 0
kind_old$me_mmr[is.na(kind_old$me_mmr)] <- 0





# KINDERGARTEN 2017-18 ----------------------------------------------------

#2017-18 schools that didn't respond
noresponse_new <-  read_excel("./data/Nonresponders_1718.xlsx", 
                              sheet="QUERY_FOR_FILTER_FOR_NONRESPOND", range="A1:d128") %>% 
  clean_names()  %>% 
  mutate(schoolID = paste(str_sub(orgidmde,3,6),str_sub(orgidmde, 1,2), str_sub(orgidmde,7,9),sep="-"),
                           districtid=paste(str_sub(orgidmde,3,6),str_sub(orgidmde, 1,2), '000',sep="-"),
         districttype= paste(str_sub(orgidmde,1,2)))

noresponse_new <- left_join(noresponse_new, school_list, by=c("schoolID"="school_id")) %>% 
  mutate(grades=str_trim(grades))

#join to a lookup table that translates the grades field to whether or not it has kindergarten
grades_list <-  read_csv("grades.csv") %>% mutate(grades=str_trim(grades))

#identify which ones have a kindergarten and which don't
noresponse_new <-  left_join(noresponse_new, grades_list %>% select(grades, kindergarten), by=c("grades"="grades"))



#2017-18 kindergarten
kind_new <-  read_excel("./data/KINDERGARTEN_1718.xlsx", 
                        sheet="KINDERGARTEN_1718_FINAL", range="A1:au1195") %>% 
  clean_names() %>% 
  mutate(enroll_new = case_when(enroll=='Enrollment < 5'~0, TRUE~as.double(enroll)),
         schoolID = paste(str_sub(orgidmde,3,6),str_sub(orgidmde, 1,2), str_sub(orgidmde,7,9),sep="-"),
         grade='Kindergarten',
         yr = 2018,
         schoolyr='2017-18',
         districtid =paste(str_sub(orgidmde,3,6),str_sub(orgidmde, 1,2), '000',sep="-"),
         districttype= paste(str_sub(orgidmde,1,2)))

#join to the address file
kind_new <- left_join(kind_new, school_list, by=c("schoolID"="school_id"))

#append the no response records to the kindergarten_new table
noresponse_kindergarten <-  noresponse_new %>% filter(kindergarten=='yes') %>% mutate(grade='Kindergarten', yr=2018, schoolyr='2017-18')
kind_new <- bind_rows(kind_new, noresponse_kindergarten)



kind_new$schooltype[kind_new$districttype=='31'] <-  'private'
kind_new$schooltype[kind_new$districttype=='32'] <-  'private'
kind_new$schooltype[kind_new$districttype=='33'] <-  'private'
kind_new$schooltype[kind_new$districttype=='34'] <-  'private'
kind_new$schooltype[kind_new$districttype=='01'] <-  'public'
kind_new$schooltype[kind_new$districttype=='03'] <-  'public'


#join to the master school list to try to catch any that weren't in the address files
kind_new <- left_join(kind_new, master_school_list %>% 
                        select(school_id, city, school_location_county_name), by = c("schoolID" = "school_id") ) %>%
  mutate(
    physical_city = if_else(is.na(physical_city), city, physical_city),
    county = if_else(is.na(county), str_to_title(school_location_county_name), county)
  )%>% 
  select(-city, -school_location_county_name)



#join to the district file
kind_new <- left_join(kind_new, district_list %>% 
                        select(id_number, countyname), by = c("districtid" = "id_number") )%>%
  mutate(county = if_else(is.na(county), str_to_title(countyname), county)) %>% 
  select(-countyname)


#this creates a list of counties lined up with district names
temp_new <-  kind_new %>% filter(county!='NA') %>%  group_by(disname, county) %>% summarise(count=n()) %>% rename(countyname=county)

#then pulls the county back for any remaining blanks (most appear to be private schools and the district name tells you which district they are located in)
kind_new <- left_join(kind_new, temp_new %>% 
                        select(disname, countyname), by = c("disname" = "disname") )%>%
  mutate(county = if_else(is.na(county), str_to_title(countyname), county)) %>% 
  select(-countyname)


#get rid of duplicates
kind_new <- kind_new %>% distinct()


#match with enroll special to get poverty (only public schools)
kind_new <- left_join(kind_new, enroll_special %>% filter(datayear=='17-18'), by=c("schoolID"="schoolid"))

#match with enroll race to get diversity (only public schools)
kind_new <- left_join(kind_new, enroll_race %>% filter(datayear=='17-18') %>% 
                        select(schoolid,total_students,total_minority, pctminority), by=c("schoolID"="schoolid"))


#assign income buckets, diversity bucket and flag schools with MMR vaccination rate below 90%
kind_new <-  kind_new %>% mutate(income_bucket = case_when(pctpoverty>=.80~4,
                                                           pctpoverty>=.50 & pctpoverty<.80~3,
                                                           pctpoverty>=.25 & pctpoverty<.5~2,
                                                           pctpoverty<.25~1),
                                 diversity_bucket = case_when(pctminority>=.80~4,
                                                              pctminority>=.50 & pctminority<.80~3,
                                                              pctminority>=.25 & pctminority<.5~2,
                                                              pctminority<.25~1),
                                 mmr_pocket=case_when(complete_pert_mmr>=.895~'no',
                                                      complete_pert_mmr>=0 & complete_pert_mmr<.895~'yes',
                                                      is.na(complete_pert_mmr)~'not reported',
                                                      TRUE~'unk'),
                                 type2 = case_when(districttype=='07' & schooltype=='public'~'charter',
                                                   districttype!='07'& schooltype=='public'~'traditional',
                                                   districttype=='07'& is.na(schooltype)~'charter',
                                                   schooltype=='private'~'private',
                                                   TRUE~'unknown'))

kind_new$complete_mmr[is.na(kind_new$complete_mmr)] <- 0
kind_new$inprogress_mmr[is.na(kind_new$inprogress_mmr)] <- 0
kind_new$co_mmr[is.na(kind_new$co_mmr)] <- 0
kind_new$me_mmr[is.na(kind_new$me_mmr)] <- 0




# 7th grade ---------------------------------------------------------------



#2012-13 7th grade (has a different format)
seventh_old <-  read_excel("./data/SEVENTH_1213_newformat.xlsx", 
                           sheet="FILTER_FOR_SEVENTH_1213_FINAL2", range="A1:BD853") %>% 
  clean_names() %>% 
  rename(district_type_code=school_type)



#add new columns
seventh_old <-  seventh_old %>% 
  mutate(enroll_new = case_when(enroll=='Enrollment < 5'~0, TRUE~as.double(enroll)),
         schoolID = paste(district_num,district_type_code, school_num,sep="-"),
         grade='Seventh',
         yr = 2013,
         schoolyr='2012-13',
         districtid =paste(district_num,district_type_code, '000',sep="-"),
         districttype= district_type_code)


#join to the address file
seventh_old <- left_join(seventh_old, school_list, by=c("schoolID"="school_id"))

seventh_old$schooltype[seventh_old$districttype=='31'] <-  'private'
seventh_old$schooltype[seventh_old$districttype=='32'] <-  'private'
seventh_old$schooltype[seventh_old$districttype=='33'] <-  'private'
seventh_old$schooltype[seventh_old$districttype=='34'] <-  'private'
seventh_old$schooltype[seventh_old$districttype=='01'] <-  'public'
seventh_old$schooltype[seventh_old$districttype=='03'] <-  'public'


#join to the master school list to try to catch any that weren't in the address files
seventh_old <- left_join(seventh_old, master_school_list %>% 
                           select(school_id, city, school_location_county_name), by = c("schoolID" = "school_id") ) %>%
  mutate(
    physical_city = if_else(is.na(physical_city), city, physical_city),
    county = if_else(is.na(county), str_to_title(school_location_county_name), county)
  )%>% 
  select(-city, -school_location_county_name)



#join to the district file
seventh_old <- left_join(seventh_old, district_list %>% 
                           select(id_number, countyname), by = c("districtid" = "id_number") )%>%
  mutate(county = if_else(is.na(county), str_to_title(countyname), county)) %>% 
  select(-countyname)


#this creates a list of counties lined up with district names
temp_old_seventh <-  seventh_old %>% filter(county!='NA') %>%  group_by(disname, county) %>% summarise(count=n()) %>% rename(countyname=county)

#then pulls the county back for any remaining blanks (most appear to be private schools and the district name tells you which district they are located in)
seventh_old <- left_join(seventh_old, temp_old_seventh %>% 
                           select(disname, countyname), by = c("disname" = "disname") )%>%
  mutate(county = if_else(is.na(county), str_to_title(countyname), county)) %>% 
  select(-countyname)


#get rid of duplicates
seventh_old <- seventh_old %>% distinct()


#match with enroll special to get poverty (only public schools)
seventh_old <- left_join(seventh_old, enroll_special %>% filter(datayear=='17-18'), by=c("schoolID"="schoolid"))

#match with enroll race to get diversity (only public schools)
seventh_old <- left_join(seventh_old, enroll_race %>% filter(datayear=='17-18') %>% 
                           select(schoolid,total_students,total_minority, pctminority), by=c("schoolID"="schoolid"))


#assign income buckets, diversity bucket and flag schools with MMR vaccination rate below 90%
seventh_old <-  seventh_old %>% mutate(income_bucket = case_when(pctpoverty>=.80~4,
                                                                 pctpoverty>=.50 & pctpoverty<.80~3,
                                                                 pctpoverty>=.25 & pctpoverty<.5~2,
                                                                 pctpoverty<.25~1),
                                       diversity_bucket = case_when(pctminority>=.80~4,
                                                                    pctminority>=.50 & pctminority<.80~3,
                                                                    pctminority>=.25 & pctminority<.5~2,
                                                                    pctminority<.25~1),
                                       mmr_pocket=case_when(complete_pert_mmr>=.895~'no',
                                                            complete_pert_mmr>=0 & complete_pert_mmr<.895~'yes',
                                                            is.na(complete_pert_mmr)~'not reported',
                                                            TRUE~'unk'),
                                       type2 = case_when(districttype=='07' & schooltype=='public'~'charter',
                                                         districttype!='07'& schooltype=='public'~'traditional',
                                                         districttype=='07'& is.na(schooltype)~'charter',
                                                         schooltype=='private'~'private',
                                                         TRUE~'unknown'),
                                       grades=str_trim(grades))



seventh_old$complete_mmr[is.na(seventh_old$complete_mmr)] <- 0
seventh_old$inprogress_mmr[is.na(seventh_old$inprogress_mmr)] <- 0
seventh_old$co_mmr[is.na(seventh_old$co_mmr)] <- 0
seventh_old$me_mmr[is.na(seventh_old$me_mmr)] <- 0









#2017-18 7th grade
seventh_new <-  read_excel("./data/SEVENTH_1718.xlsx", 
                           sheet="SEVENTH_1718", range="A1:BK837") %>% 
  clean_names() %>% 
  mutate(enroll_new = case_when(enroll=='Enrollment < 5'~0, TRUE~as.double(enroll)),
         schoolID = paste(str_sub(orgidmde,3,6),str_sub(orgidmde, 1,2), str_sub(orgidmde,7,9),sep="-"),
         grade='Seventh',
         yr = 2018,
         schoolyr='2017-18',
         districtid =paste(str_sub(orgidmde,3,6),str_sub(orgidmde, 1,2), '000',sep="-"),
         districttype= paste(str_sub(orgidmde,1,2)))


#join to the address file
seventh_new <- left_join(seventh_new, school_list, by=c("schoolID"="school_id")) %>% mutate(grades=str_trim(grades))


#append the no response records to the seventh_new table

noresponse_seventh <-  inner_join(noresponse_new, grades_list %>% filter(seventh=='yes') %>% select(grades, seventh),
                                  by=c("grades"="grades"))%>% 
  mutate(grade='Seventh', yr=2018, schoolyr='2017-18')

seventh_new <- bind_rows(seventh_new, noresponse_seventh)

#identify schools as either public or private
seventh_new$schooltype[seventh_new$districttype=='31'] <-  'private'
seventh_new$schooltype[seventh_new$districttype=='32'] <-  'private'
seventh_new$schooltype[seventh_new$districttype=='33'] <-  'private'
seventh_new$schooltype[seventh_new$districttype=='34'] <-  'private'
seventh_new$schooltype[seventh_new$districttype=='01'] <-  'public'
seventh_new$schooltype[seventh_new$districttype=='03'] <-  'public'


#join to the master school list to try to catch any that weren't in the address files
seventh_new <- left_join(seventh_new, master_school_list %>% 
                        select(school_id, city, school_location_county_name), by = c("schoolID" = "school_id") ) %>%
  mutate(
    physical_city = if_else(is.na(physical_city), city, physical_city),
    county = if_else(is.na(county), str_to_title(school_location_county_name), county)
  )%>% 
  select(-city, -school_location_county_name)



#join to the district file
seventh_new <- left_join(seventh_new, district_list %>% 
                        select(id_number, countyname), by = c("districtid" = "id_number") )%>%
  mutate(county = if_else(is.na(county), str_to_title(countyname), county)) %>% 
  select(-countyname)


#this creates a list of counties lined up with district names
temp_new_seventh <-  seventh_new %>% filter(county!='NA') %>%  group_by(disname, county) %>% summarise(count=n()) %>% rename(countyname=county)

#then pulls the county back for any remaining blanks (most appear to be private schools and the district name tells you which district they are located in)
seventh_new <- left_join(seventh_new, temp_new_seventh %>% 
                        select(disname, countyname), by = c("disname" = "disname") )%>%
  mutate(county = if_else(is.na(county), str_to_title(countyname), county)) %>% 
  select(-countyname)


#get rid of duplicates
seventh_new <- seventh_new %>% distinct()


#match with enroll special to get poverty (only public schools)
seventh_new <- left_join(seventh_new, enroll_special %>% filter(datayear=='17-18'), by=c("schoolID"="schoolid"))

#match with enroll race to get diversity (only public schools)
seventh_new <- left_join(seventh_new, enroll_race %>% filter(datayear=='17-18') %>% 
                        select(schoolid,total_students,total_minority, pctminority), by=c("schoolID"="schoolid"))


#assign income buckets, diversity bucket, whether it's a charter school, and flag schools with MMR vaccination rate below 90%
seventh_new <-  seventh_new %>% mutate(income_bucket = case_when(pctpoverty>=.80~4,
                                                           pctpoverty>=.50 & pctpoverty<.80~3,
                                                           pctpoverty>=.25 & pctpoverty<.5~2,
                                                           pctpoverty<.25~1),
                                 diversity_bucket = case_when(pctminority>=.80~4,
                                                              pctminority>=.50 & pctminority<.80~3,
                                                              pctminority>=.25 & pctminority<.5~2,
                                                              pctminority<.25~1),
                                 mmr_pocket=case_when(complete_pert_mmr>=.895~'no',
                                                      complete_pert_mmr>=0 & complete_pert_mmr<.895~'yes',
                                                      is.na(complete_pert_mmr)~'not reported',
                                                      TRUE~'unk'),
                                 type2 = case_when(districttype=='07' & schooltype=='public'~'charter',
                                                   districttype!='07'& schooltype=='public'~'traditional',
                                                   districttype=='07'& is.na(schooltype)~'charter',
                                                   schooltype=='private'~'private',
                                                   TRUE~'unknown'))


seventh_new$complete_mmr[is.na(seventh_new$complete_mmr)] <- 0
seventh_new$inprogress_mmr[is.na(seventh_new$inprogress_mmr)] <- 0
seventh_new$co_mmr[is.na(seventh_new$co_mmr)] <- 0
seventh_new$me_mmr[is.na(seventh_new$me_mmr)] <- 0



# MERGE DATA FOR MMR ONLY -------------------------------------------------

mmr_kind_1718 <-  kind_new %>% select(schoolID, disname, schname, enroll_new, 
                                 complete_mmr, inprogress_mmr, co_mmr, me_mmr,
                                 complete_pert_mmr, inprogress_pert_mmr, co_pert_mmr, me_pert_mmr, 
                                 pctpoverty, pctminority, income_bucket, 
                                 diversity_bucket, mmr_pocket, 
                                 county, type2, yr, schoolyr, k12enr, grade, schooltype)

mmr_kind_1213 <-  kind_old  %>% select(schoolID, disname, schname, enroll_new, 
                                  complete_mmr, inprogress_mmr, co_mmr, me_mmr,
                                  complete_pert_mmr, inprogress_pert_mmr, co_pert_mmr, me_pert_mmr, 
                                  pctpoverty, pctminority, income_bucket, 
                                  diversity_bucket, mmr_pocket, 
                                  county, type2, yr, schoolyr, k12enr, grade, schooltype)

mmr_7_1718 <-  seventh_new %>% select(schoolID, disname, schname, enroll_new, 
                                      complete_mmr, inprogress_mmr, co_mmr, me_mmr,
                                      complete_pert_mmr, inprogress_pert_mmr, co_pert_mmr, me_pert_mmr, 
                                      pctpoverty, pctminority, income_bucket, 
                                      diversity_bucket, mmr_pocket, 
                                      county, type2, yr, schoolyr, k12enr, grade, schooltype)

mmr_7_1213 <-  seventh_old  %>% select(schoolID, disname, schname, enroll_new, 
                                       complete_mmr, inprogress_mmr, co_mmr, me_mmr,
                                       complete_pert_mmr, inprogress_pert_mmr, co_pert_mmr, me_pert_mmr, 
                                       pctpoverty, pctminority, income_bucket, 
                                       diversity_bucket, mmr_pocket, 
                                       county, type2, yr, schoolyr, k12enr, grade, schooltype)



mmr <-  bind_rows(mmr_kind_1213, mmr_kind_1718, mmr_7_1718, mmr_7_1213)

mmr$co_pert_mmr[is.na(mmr$co_pert_mmr)] <- 0


#assign the primary "reason" schools have a low MMR rate
#looks at whether school has higher rate of incomplete or higher rate of waivers
mmr <-  mmr %>% mutate(pocket_reason = case_when(mmr_pocket=='no'~'na',
                                                 co_pert_mmr>inprogress_pert_mmr~'non-medical waiver',
                                                 inprogress_pert_mmr>co_pert_mmr~'in progress',
                                                 co_pert_mmr==inprogress_pert_mmr~'equal',
                                                 mmr_pocket=='not reported'~'not reported',
                                                 TRUE~'not clear'))


kind_new$complete_mmr[is.na(kind_new$complete_mmr)] <- 0
kind_new$inprogress_mmr[is.na(kind_new$inprogress_mmr)] <- 0
kind_new$co_mmr[is.na(kind_new$co_mmr)] <- 0
kind_new$me_mmr[is.na(kind_new$me_mmr)] <- 0





mmr$enroll_new[is.na(mmr$enroll_new)] <- 0

countylevel <-   mmr%>% filter(yr==2018, mmr_pocket!='not reported') %>%  group_by(county, mmr_pocket) %>% summarise(count=n(), enroll_county=sum(enroll_new)) %>% mutate(pctenroll=enroll_county/sum(enroll_county), totalschools=sum(count), pctschools=count/totalschools)%>% 
  filter(mmr_pocket=='yes') %>% arrange(desc(pctenroll))

notreported_county <-  mmr %>% filter(yr==2018, mmr_pocket=='not reported') %>% group_by(county) %>% summarise(schools_not_reporting=n())

countylevel <-  left_join(countylevel, notreported_county, by=c("county"="county"))

#write.csv(countylevel, 'countylevel_mmr.csv', row.names=FALSE)




# CHILD CARE CENTERS ------------------------------------------------------


childcare_vax <-  read_csv('./data/childcarecenter1718.csv', col_types=cols(.default=col_double(),license_num=col_character(), center_name=col_character(),
                                                                     City=col_character(),
                                          enrollment_2yrsup=col_integer(),
                                          notes=col_character()))



childcare_lic <-  read_csv('./data/Licensing_Lookup_Results_ Feb.08.2019.csv', 
                           col_types=cols(`License Number`=col_character())) %>% clean_names()


match <-  left_join(childcare_vax, childcare_lic, by=c("license_num"="license_number")) %>% 
  filter(license_status=='Active')

match %>% filter(is.na(name_of_program), enrollment_2yrsup!=0) %>% arrange((enrollment_2yrsup)) %>% 
  select(center_name, City,enrollment_2yrsup )



childcare_vax_foronline <-  match %>% select(license_num, center_name, enrollment_2yrsup,
                                             MMR_pct_vac, MMR_pct_nodoses, MMR_pct_nonmedical,
                                             MMR_pct_medical, address_line1, address_line2, city,
                                             state, zip, county, capacity) 


# done with child care ----------------------------------------------------



#compare kindergarten and 7th grade for schools that have both
mmr_k <-  mmr %>% filter(grade=='Kindergarten', yr==2018)
mmr_7 <-  mmr %>% filter(grade=='Seventh', yr==2018)

mmr_compare <-  inner_join(mmr_k, mmr_7, by=c("schoolID"="schoolID"))

mmr_compare_export <-  mmr_compare %>% filter(schname.x!="NA", schname.y!="NA", mmr_pocket.x!='not reported', mmr_pocket.y!='not reported', enroll_new.x>10, enroll_new.y >10) %>% 
  select(schoolID, disname.x, schname.y, mmr_pocket.x, mmr_pocket.y, k_mmr_complete=complete_pert_mmr.x , Seven_mmr_complete=complete_pert_mmr.y) %>% 
  mutate(diff=k_mmr_complete-Seven_mmr_complete) %>% 
  arrange(desc(diff))

#write.csv(mmr_compare_export, "mmr_k_7.csv", row.names=FALSE)


#see how many schools we have by size grouping
mmr <-  mmr %>% 
  mutate(schoolsize = case_when(
    enroll_new==0~0,
    enroll_new<10~1,
    enroll_new>=10 & enroll_new<20~2,
    enroll_new>=20 & enroll_new<50~3,
    
    enroll_new>=50 & enroll_new<70~5,
    enroll_new>= 70 & enroll_new<90~6,
    enroll_new>=90 & enroll_new<150~7,
    enroll_new>=150~8,
    TRUE~999))

schoolsizes_k <-    mmr %>%filter (yr==2018, grade=='Kindergarten') %>%  group_by( mmr_pocket,schoolsize) %>% summarise(count=n())


#write.csv(schoolsizes_k, 'schoolsizes.csv', row.names=FALSE)




# EXPORT TO JSON ----------------------------------------------------------

schools_vax_export <-  bind_rows(kind_new, seventh_new, kind_old, seventh_old) %>% select(-kindergarten, -seventh)
write.csv(schools_vax_export, 'schools_vax_export_test.csv', row.names =FALSE)

#childcare_vax_json<-  toJSON(childcare_vax_foronline, pretty=TRUE)
#write(childcare_vax_json, "childcare_vax.json")

schools_vax_json<-  toJSON(schools_vax_export, pretty=TRUE)
write(schools_vax_json, "schools_vax.json")


