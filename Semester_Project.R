install.packages("readr")
install.packages("plyr")
install.packages("dplyr")
install.packages("stringi")
install.packages("gapminder")
install.packages("ggplot2")
install.packages("zoo")
install.packages("ggfortify")

library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)

# library(plyr)
# library(rvest)
# library(lubridate)
# library(stringi)
# library(tidyr)
# library(tibble)
# library(gapminder)
# library(zoo)
# library(ggfortify)

setwd("~/Desktop/Data Science for Developers/Data Science Project Report")

##################################### READ THE DATA ###########################################################################################################################

data_before = read_csv("/Users/apple/Desktop/Data Science for Developers/Data Science Project Report/PCD_OA_LSOA_MSOA_LAD_AUG21_UK_LU 5.csv")

#################################### SURREY GEO INFO AND POPULATION ##############################################################################################################################################


districts_Surrey = c("Elmbridge", "Epsom and Ewell",  "Guildford", "Mole Valley", "Reigate and Banstead", "Runnymede", "Spelthorne", "Surrey Heath", "Tandridge", "Waverley", "Woking")

Surrey.GEO.PPL = filter(data_before, ladnm %in% districts_Surrey) %>% #Finds the specific rows from columns
  dplyr::rename("District_Name" = "ladnm", #Rename the columns' names
         "MSOA_Name" = "msoa11nm",
         "LSOA_Name" = "lsoa11nm",
         "District_Code" = "ladcd",
         "MSOA_Code" = "msoa11cd",
         "LSOA_Code" = "lsoa11cd",
         "OutputArea_Code" = "oa11cd",
         "Postcode" = "pcds") %>%
  select(-c("usertype", "doterm", "dointr", "pcd8", "pcd7", "ladnmw")) %>% #deletes the unwished columns
  mutate(County_Name = "Surrey") %>% #add new column
  select("County_Name", "District_Name" , "District_Code", "MSOA_Name", "MSOA_Code", "LSOA_Name", "LSOA_Code", "Postcode", "OutputArea_Code") 

PPL.Surrey = read_csv("/Users/apple/Desktop/Data Science for Developers/Data Science Project Report/All areas 2012-2018.csv") %>%
  select(c("Code", "Area", "Total Population")) 

Surrey.GEO.PPL = right_join(PPL.Surrey, Surrey.GEO.PPL, by = c("Area" = "LSOA_Name")) %>%
  dplyr::rename("LSOA_Name" = "Area",
                "LSOA_Population" = "Total Population") %>%
  select(-c("Code"))

Surrey.GEO.PPL = right_join(PPL.Surrey, Surrey.GEO.PPL, by = c("Area" = "District_Name")) %>%
  dplyr::rename("District_Name" = "Area",
                "District_Population" = "Total Population") %>%
  select(-c("Code")) %>%
  mutate(County_Code = "E10000030",
         County_Population = "1189934") %>%
  select("County_Name", "County_Code", "County_Population", "District_Name", "District_Code", "District_Population", "MSOA_Name", "MSOA_Code", "LSOA_Name", "LSOA_Code", "LSOA_Population", "Postcode", "OutputArea_Code") 


# Ward.Surrey = read_csv("/Users/apple/Desktop/Data Science for Developers/Data Science Project Report/Active Lives PA Levels in Surrey MSOAs & Wards, June 2019.csv") %>%
#   select(-c("Inactive (%)", "Active (%)", "Twice a month (%)")) %>%
#   dplyr::rename("MSOA_Code" = "MSOA11",
#          "MSOA_Name" = "Local Authority",
#          "Ward_Name" = "Ward Name") %>%
#   select(-c("MSOA_Code"))
# 
# Surrey.GEO.PPL = left_join(Ward.Surrey, Surrey.GEO.PPL, by = "MSOA_Name") 


surrey.ppl.districts = Surrey.GEO.PPL %>%
  select(c("District_Name", "District_Population")) %>%
  distinct(District_Name, .keep_all = TRUE)

View(Surrey.GEO.PPL)




#################################### WEST SUSSEX GEO INFO AND POPULATION ##########################################################################################################################


districts_West.Sussex = c("Adur", "Arun", "Chichester", "Horsham", "Mid Sussex", "Crawley", "Worthing")

WestSussex.GEO.PPL = filter(data_before, ladnm %in% districts_West.Sussex) %>%
  dplyr::rename("District_Name" = "ladnm", #Rename the columns' names
         "MSOA_Name" = "msoa11nm",
         "LSOA_Name" = "lsoa11nm",
         "District_Code" = "ladcd",
         "MSOA_Code" = "msoa11cd",
         "LSOA_Code" = "lsoa11cd",
         "OutputArea_Code" = "oa11cd",
         "Postcode" = "pcds") %>%
  select(-c("usertype", "doterm", "dointr", "pcd8", "pcd7", "ladnmw")) %>% #deletes the unwished columns
  mutate(County_Name = "West Sussex") %>% #add new column
  select("County_Name", "District_Name" , "District_Code", "MSOA_Name", "MSOA_Code", "LSOA_Name", "LSOA_Code", "Postcode", "OutputArea_Code") #rearrange the dataset

PPL.WestSussex = read_csv("/Users/apple/Desktop/Data Science for Developers/Data Science Project Report/2011_census_usual_resident_population_by_resident_type_sex_and_5_year_age_groups_at_lsoa_level.csv") %>%
  select(c("Local authority code", "Local authority name", "LSOA Code", "LSOA Name", "All ages")) %>%
  slice(-c(1)) %>%
  dplyr::rename("District_Name" = "Local authority name",
         "District_Code" = "Local authority code",
         "LSOA_Name" = "LSOA Name",
         "LSOA_Code" = "LSOA Code",
         "LSOA_Population" = "All ages") %>%
  select(c("LSOA_Code", "LSOA_Population", "District_Name"))

WestSussex.GEO.PPL = left_join(PPL.WestSussex, WestSussex.GEO.PPL, by = "LSOA_Code") %>%
  mutate(County_Population = "885852",
         County_Code = "E10000032") %>%
  dplyr::rename("District_Name" = "District_Name.y") %>%
  select(-c("District_Name.x")) %>%
  select("County_Name", "County_Code", "County_Population", "District_Name", "District_Code", "MSOA_Name", "MSOA_Code", "LSOA_Name", "LSOA_Code", "LSOA_Population", "Postcode", "OutputArea_Code") 

ws.ppl.districts = read_csv("/Users/apple/Desktop/Data Science for Developers/Data Science Project Report/census_2001_2011_population_west_sussex_LAs.csv") %>%
  select(c(1,5)) %>%
  drop_na() %>%
  dplyr::rename("District_Name" = "Total Populations",
                "District_Population" = "2011") %>%
  slice(-c(8))

WestSussex.GEO.PPL = left_join(ws.ppl.districts, WestSussex.GEO.PPL, by = "District_Name") %>%
  select("County_Name", "County_Population", "County_Code", "District_Name", "District_Population", "District_Code", "MSOA_Name", "MSOA_Code", "LSOA_Name", "LSOA_Code", "LSOA_Population", "Postcode", "OutputArea_Code") 

View(WestSussex.GEO.PPL)


############################################ GEO INFO AND POPULATION OF BOTH ####################################################################################################################

#data_GIS = full_join(data_Surrey, data_West.Sussex)  #combines 2 dataset

# write_excel_csv2(data_Final, "Geogrpahic Information Data of Surrey and West Sussex.csv") #Creates new .csv file and exports data to Excel

################################################## SURREY HOUSE PRICE ##############################################################################################################


data_HP1 = read_csv("/Users/apple/Desktop/Data Science for Developers/Data Science Project Report/1ukhpi-comparison-all-avg-elmbridge-from-2019-01-01-to-2019-12-01.csv")
data_HP2 = read_csv("/Users/apple/Desktop/Data Science for Developers/Data Science Project Report/2ukhpi-comparison-all-avg-runnymede-from-2019-01-01-to-2019-12-01.csv")
data_HP3 = read_csv("/Users/apple/Desktop/Data Science for Developers/Data Science Project Report/3ukhpi-comparison-all-avg-woking-from-2019-01-01-to-2019-12-01.csv")

HP.Surrey = full_join(data_HP1, data_HP2) %>% full_join(., data_HP3) %>%
  select(-c("URI", "Reporting period", "Pivotable date", "Sales volume")) %>%
  dplyr::rename("District_Name" = "Name",
                "District_Code" = "Region GSS code",
                "Date" = "Period",
                "Average_Price" = "Average price All property types") %>%
  dplyr::mutate(County_Name = "Surrey", .before = District_Name)

districts_Surrey = c("Elmbridge", "Epsom and Ewell",  "Guildford", "Mole Valley", "Reigate and Banstead", "Runnymede", "Spelthorne", "Surrey Heath", "Tandridge", "Waverley", "Woking")

Average.Surrey.HP = HP.Surrey %>%
  group_by(District_Name) %>%
  dplyr::summarise(Average.House.Price = median(Average_Price)) %>%
  mutate_if(is.numeric, round) %>%
  dplyr::mutate(County_Name = "Surrey", .before = District_Name)

View(Average.Surrey.HP)

# View(HP.Surrey)

###### VISUALIZATION || SURREY HOUSE PRICES  ##############################################

############## Median Price of Houses of Districts of Surrey by Month #############

Visualization.1.Surrey.HP = HP.Surrey %>% 
  group_by(District_Name, Date) %>%
  dplyr::summarise(Average.House.Price = median(Average_Price)) %>%
  mutate_if(is.numeric, round) %>%
  dplyr::mutate(County_Name = "Surrey", .before = District_Name)

ggplot(Visualization.1.Surrey.HP, aes(Date, Average.House.Price, colour = District_Name, group = District_Name)) +
  geom_point() + geom_line()  +
  labs(
    title = "Median House Prices of Districts of Surrey by Month",
    caption = "Author: Nurlan Guliyev") +
  theme(
    plot.title = element_text(size = 19, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 12, face = "italic"),
    axis.text=element_text(size=10),
    axis.title=element_text(size=12,face="bold")) +
  xlab("Date") + 
  ylab("Median House Prices") +
  labs(color = "District") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))

############### Median House Price of Surrey by District ###################


ggplot(Average.Surrey.HP, aes(District_Name, Average.House.Price, fill = District_Name)) +
  geom_bar(stat = "identity") +
  labs(
    fill = "District",
    title = "Median House Prices of Surrey by District",
    subtitle = "Time Period: 01/2019 - 12/2019",
    caption = "Author: Nurlan Guliyev") +
  theme(
    plot.title = element_text(size = 19, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "blue", size = 14 ),
    plot.caption = element_text(size = 13, face = "italic")) +
  xlab("Districts of Surrey") + 
  ylab("Median House Prices of Districts") +
  scale_y_continuous(limits = c(0, 600000), breaks = seq(0, 600000, by = 50000)) +
  coord_flip() 



#################################### WEST SUSSEX HOUSE PRICES ##############################################################################################################################################


data_HP4 = read_csv("/Users/apple/Desktop/Data Science for Developers/Data Science Project Report/4ukhpi-comparison-all-avg-adur-from-2019-01-01-to-2019-12-01.csv")
data_HP5 = read_csv("/Users/apple/Desktop/Data Science for Developers/Data Science Project Report/5ukhpi-comparison-all-avg-crawley-from-2019-01-01-to-2019-12-01.csv")

HP.West_Sussex = full_join(data_HP4, data_HP5) %>%
  select(-c("URI", "Reporting period", "Pivotable date", "Sales volume")) %>%
  dplyr::rename("District_Name" = "Name",
         "District_Code" = "Region GSS code",
         "Average_Price" = "Average price All property types",
         "Date" = "Period") %>%
  dplyr::mutate(County_name = "West Sussex", .before = District_Name)


Average.West_Sussex.HP = HP.West_Sussex %>%
  group_by(District_Name) %>%
  dplyr::summarise(Average.House.Price = median(Average_Price)) %>%
  mutate_if(is.numeric, round) %>%
  dplyr::mutate(County_Name = "West Sussex", .before = District_Name)


View(Average.West_Sussex.HP)

# View(HP.West_Sussex)

###### VISUALIZATION || WEST SUSSEX HOUSE PRICES  ##############################################

# Median Price of Houses of Districts of West Sussex by Month

Visualization.1.West_Sussex.HP = HP.West_Sussex %>% 
  group_by(District_Name, Date) %>%
  dplyr::summarise(Average.House.Price = median(Average_Price)) %>%
  mutate_if(is.numeric, round) %>%
  dplyr::mutate(County_Name = "West Sussex", .before = District_Name)

ggplot(Visualization.1.West_Sussex.HP, aes(Date, Average.House.Price, colour = District_Name, group = District_Name)) +
  geom_point() + geom_line()  +
  labs(
    title = "Median House Prices of Districts of West Sussex by Month",
    caption = "Author: Nurlan Guliyev") +
  theme(
    plot.title = element_text(size = 19, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 12, face = "italic"),
    axis.text=element_text(size=10),
    axis.title=element_text(size=12,face="bold")) +
  xlab("Date") + 
  ylab("Median Price of Houses") +
  scale_y_continuous(limits = c(270000, 400000), breaks = seq(270000, 400000, by = 20000)) +
  labs(color = "District") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))


############### Median House Price of West Sussex by District ###################


ggplot(Average.West_Sussex.HP, aes(District_Name, Average.House.Price, fill = District_Name)) +
  geom_bar(stat = "identity") +
  labs(
    fill = "Districts",
    title = "Median House Prices of West Sussex by District",
    subtitle = "Time Period: 01/2019 - 12/2019",
    caption = "Author: Nurlan Guliyev") +
  theme(
    plot.title = element_text(size = 19, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "blue", size = 14 ),
    plot.caption = element_text(size = 13, face = "italic")) +
  xlab("Districts of Surrey") + 
  ylab("Median House Prices of Districts") +
  scale_y_continuous(limits = c(0, 600000), breaks = seq(0, 600000, by = 50000)) +
  coord_flip() 



#################################### HOUSE PRICES OF BOTH ##############################################################################################################################################

#data_HP.Final = full_join(data_HP.Surrey, data_HP.West_Sussex)

# write_excel_csv2(data_HP.Final, "House Prices of Surrey and West Sussex.csv")

################################################################################################################################################################


#################################### SURREY CRIME DATA ##############################################################################################################################################

setwd("/Users/apple/Desktop/Data Science for Developers/Data Science Project Report/surrey police 01.2019 - 12.2019")

Police.Surrey = list.files(path = "/Users/apple/Desktop/Data Science for Developers/Data Science Project Report/surrey police 01.2019 - 12.2019", pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  select(-c("Falls within", "Longitude", "Latitude", "Location", "Last outcome category", "Context", "Reported by")) %>%
  dplyr::rename("LSOA_Name" = "LSOA name",
                "LSOA_Code" = "LSOA code",
                "Date" = "Month",
                "Crime_ID" = "Crime ID") %>%
  mutate(County_Name = "Surrey") %>%
  select("Crime_ID", "County_Name", "Date", "LSOA_Name", "LSOA_Code", "Crime type") %>%
  drop_na()


support.data.surrey.police = Surrey.GEO.PPL %>%
  select(c("LSOA_Name", "District_Name", "District_Population")) %>%
  distinct(LSOA_Name, .keep_all = TRUE)

Police.Surrey = right_join(support.data.surrey.police, Police.Surrey, by = "LSOA_Name") %>%
  distinct(Crime_ID, .keep_all = TRUE) %>%
  drop_na() %>%
  dplyr::rename("Crime_Type" = "Crime type") %>%
  select(c("County_Name", "Date", "District_Name", "District_Population", "LSOA_Name", "Crime_Type"))


Average.Police.Surrey = Police.Surrey %>%
  group_by(District_Name) %>%
  dplyr::summarise(Crime_Type = n()) %>%
  dplyr::rename("Crime_Number" = "Crime_Type") %>%
  left_join(surrey.ppl.districts, by = "District_Name") %>%
  mutate(Crime_Rate = Crime_Number/District_Population*1000) %>%
  mutate_if(is.numeric, round)

View(Average.Police.Surrey)

# View(Police.Surrey)

###### VISUALIZATION || Surrey CRIME RATE  ##############################################


ggplot(Average.Police.Surrey, aes(District_Name, Crime_Rate, fill = District_Name)) +
  geom_bar(stat = "identity") +
  labs(
    fill = "District",
    title = "Average Crime Rate of Surrey by Districts",
    subtitle = "Time Period: 01/2019 - 12/2019",
    caption = "Author: Nurlan Guliyev") +
  theme(
    plot.title = element_text(size = 19, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "blue", size = 14 ),
    plot.caption = element_text(size = 13, face = "italic")) +
  xlab("Districts of West Sussex") + 
  ylab("Crime Rates of Districts") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))




#################################### WEST SUSSEX CRIME DATA ##############################################################################################################################################


setwd("/Users/apple/Desktop/Data Science for Developers/Data Science Project Report/sussex police 01.2019 - 12.2019")

Police.Sussex = list.files(path = "/Users/apple/Desktop/Data Science for Developers/Data Science Project Report/sussex police 01.2019 - 12.2019", pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  select(-c( "Falls within", "Longitude", "Latitude", "Location", "Last outcome category", "Context", "Reported by")) %>%
  dplyr::rename("LSOA_Name" = "LSOA name",
                "LSOA_Code" = "LSOA code",
                "Date" = "Month",
                "Crime.ID" = "Crime ID",
                "Crime_Type" = "Crime type") %>% 
  mutate(County_Name = "West Sussex")

support.data.WS.police = WestSussex.GEO.PPL %>%
  select(c("LSOA_Name", "District_Name", "District_Population")) %>%
  distinct(LSOA_Name, .keep_all = TRUE)

Police.West.Sussex = right_join(support.data.WS.police, Police.Sussex, by = "LSOA_Name", keep = FALSE) %>%
  distinct(Crime.ID, .keep_all = TRUE) %>%
  drop_na() %>%
  select(c("County_Name", "Date", "District_Name", "District_Population", "LSOA_Name", "Crime_Type"))

Average.Police.West.Sussex = Police.West.Sussex %>%
  group_by(District_Name) %>%
  dplyr::summarise(Crime_Type = n()) %>%
  dplyr::rename("Crime_Number" = "Crime_Type") %>%
  left_join(ws.ppl.districts, by = "District_Name") %>%
  mutate(Crime_Rate = Crime_Number/District_Population*1000) %>%
  mutate_if(is.numeric, round)
  
View(Average.Police.West.Sussex)

###### VISUALIZATION || WEST SUSSEX CRIME RATE  ##############################################


ggplot(Average.Police.West.Sussex, aes(District_Name, Crime_Rate, fill = District_Name)) +
  geom_bar(stat = "identity") +
  labs(
    fill = "District",
    title = "Average Crime Rate of West Sussex by District",
    subtitle = "Time Period: 01/2019 - 12/2019",
    caption = "Author: Nurlan Guliyev") +
  theme(
    plot.title = element_text(size = 19, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "blue", size = 14 ),
    plot.caption = element_text(size = 13, face = "italic")) +
  xlab("Districts of West Sussex") + 
  ylab("Crime Rates of Districts") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))

# View(Police.West.Sussex)

#################################### CRIME DATA OF BOTH ##############################################################################################################################################

#data_Police.Both = full_join(data_West.Sussex.Police, data_Surrey.Police)

################################################################################################################################################################

######################################### SURREY SCHOOL QUALITY #######################################################################################################################

setwd("/Users/apple/Desktop/Data Science for Developers/Data Science Project Report/2018-2019 Surrey Sch ks2 ks4 ks5")

ks5_Surrey_Sch = read_csv("/Users/apple/Desktop/Data Science for Developers/Data Science Project Report/2018-2019 Surrey Sch ks2 ks4 ks5/936_ks5final.csv") %>%
  select(c("SCHNAME", "PCODE", "TALLPPE_ALEV_1618")) %>%
  dplyr::rename("KS5_School_Name" = "SCHNAME",
                "KS5_Postcode" = "PCODE",
                "KS5_Average_Point_A.level" = "TALLPPE_ALEV_1618")

ks5_Surrey_Sch = right_join(Surrey.GEO.PPL, ks5_Surrey_Sch, by = c("Postcode" = "KS5_Postcode"), keep = TRUE) %>%
  select(c("KS5_Postcode","LSOA_Name", "District_Name", "KS5_School_Name", "KS5_Average_Point_A.level")) %>%
  slice(-c(69:71)) %>% 
  dplyr::mutate(County_Name = "Surrey", .after = KS5_Postcode) %>%
  filter(KS5_Average_Point_A.level != c("NE")) %>%
  filter(KS5_Average_Point_A.level != c("SUPP"))

ks5_Surrey_Sch$KS5_Average_Point_A.level = as.numeric(ks5_Surrey_Sch$KS5_Average_Point_A.level)



ks4_Surrey_Sch = read_csv("/Users/apple/Desktop/Data Science for Developers/Data Science Project Report/2018-2019 Surrey Sch ks2 ks4 ks5/936_ks4final.csv") %>%
  select(c("SCHNAME", "PCODE", "KS2APS")) %>%
  dplyr::rename("KS4_School_Name" = "SCHNAME",
                "KS4_Postcode" = "PCODE",
                "KS4_Average.Cohort.Score" = "KS2APS")

ks4_Surrey_Sch = right_join(Surrey.GEO.PPL, ks4_Surrey_Sch, by = c("Postcode" = "KS4_Postcode"), keep = TRUE) %>%
  select(c("KS4_Postcode", "LSOA_Name", "District_Name", "KS4_School_Name", "KS4_Average.Cohort.Score")) %>%
  slice(-c(131:133)) %>% 
  dplyr::mutate(County_Name = "Surrey", .after = KS4_Postcode) %>%
  filter(KS4_Average.Cohort.Score != c("NP")) %>%
  filter(KS4_Average.Cohort.Score != c("SUPP")) %>%
  filter(KS4_Average.Cohort.Score != c(""))

ks4_Surrey_Sch$KS4_Average.Cohort.Score = as.numeric(ks4_Surrey_Sch$KS4_Average.Cohort.Score)



ks2_Surrey_Sch = read_csv("/Users/apple/Desktop/Data Science for Developers/Data Science Project Report/2018-2019 Surrey Sch ks2 ks4 ks5/936_ks2final.csv") %>%
  select(c("SCHNAME", "PCODE", "TKS1AVERAGE")) %>%
  dplyr::rename("KS2_School_Name" = "SCHNAME",
                "KS2_Postcode" = "PCODE",
                "KS2_Average.Cohort.Score" = "TKS1AVERAGE")

ks2_Surrey_Sch = right_join(Surrey.GEO.PPL, ks2_Surrey_Sch, by = c("Postcode" = "KS2_Postcode"), keep = TRUE) %>%
  select(c("KS2_Postcode", "LSOA_Name", "District_Name", "KS2_School_Name", "KS2_Average.Cohort.Score")) %>%
  slice(-c(239:242)) %>% 
  dplyr::mutate(County_Name = "Surrey", .after = KS2_Postcode)  %>%
  filter(KS2_Average.Cohort.Score != c("SUPP")) %>%
  filter(KS2_Average.Cohort.Score != c(""))

ks2_Surrey_Sch$KS2_Average.Cohort.Score = as.numeric(ks2_Surrey_Sch$KS2_Average.Cohort.Score)



Average.KS5.Surrey = ks5_Surrey_Sch %>%
  group_by(District_Name) %>%
  dplyr::summarise(KS5.Average.A_Level.Score = mean(KS5_Average_Point_A.level)) %>%
  mutate_if(is.numeric, round) %>%
  dplyr::mutate(County_Name = "Surrey", .before = District_Name)


Average.KS4.Surrey = ks4_Surrey_Sch %>%
  group_by(District_Name) %>%
  dplyr::summarise(KS4_Average.Cohort.Score = mean(KS4_Average.Cohort.Score)) %>%
  mutate_if(is.numeric, round) %>%
  dplyr::mutate(County_Name = "Surrey", .before = District_Name)


Average.KS2.Surrey = ks2_Surrey_Sch %>%
  group_by(District_Name) %>%
  dplyr::summarise(KS2_Average.Cohort.Score = mean(KS2_Average.Cohort.Score)) %>%
  mutate_if(is.numeric, round) %>%
  dplyr::mutate(County_Name = "Surrey", .before = District_Name)

Average.Surrey.School.Quality = full_join(Average.KS2.Surrey, Average.KS4.Surrey) %>% full_join(., Average.KS5.Surrey) %>%
  group_by(District_Name) %>%
  dplyr::mutate(Total.Average.School.Quality = mean(c(KS2_Average.Cohort.Score, KS4_Average.Cohort.Score, KS5.Average.A_Level.Score)), .after = District_Name) %>%
  mutate_if(is.numeric, round)

View(Average.Surrey.School.Quality)


#View(ks5_Surrey_Sch)
#View(ks4_Surrey_Sch)
#View(ks2_Surrey_Sch)

###### VISUALIZATION || SURREY SCHOOL QUALITY ##############################################

Visualization.1.Surrey.Sch.Quality = Average.Surrey.School.Quality %>% gather(key = Season, value = Value, Total.Average.School.Quality:KS5.Average.A_Level.Score)

ggplot(Visualization.1.Surrey.Sch.Quality, aes(District_Name, Value, fill = Season)) + 
  geom_col(position = "dodge")  +
  labs(
    fill = "Average Scores by Key Stage Levels\nand Total Average of them",
    title = "Average School Scores of Surrey by District and Key Stage Levels",
    subtitle = "Study Period: 2019/2020",
    caption = "Author: Nurlan Guliyev") +
  theme(
    plot.title = element_text(size = 19, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "blue", size = 14 ),
    plot.caption = element_text(size = 13, face = "italic")) +
  scale_fill_discrete(
    labels = c("Key Stage 2, Average Cohort Score", "Key Stage 4, Average Cohort Score",
               "Key Stage 5, Average A Level Score", "Total Average of all Key Stages")) +
  xlab("Districts of Surrey") + 
  ylab("Average Cohort and A Level Scores") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))


############################################ WEST SUSSEX SCHOOL QUALITY ####################################################################################################################


setwd("/Users/apple/Desktop/Data Science for Developers/Data Science Project Report/2018-2019 West Sussex Sch ks2 k4 ks5")

ks5_West.Sussex_Sch = read_csv("/Users/apple/Desktop/Data Science for Developers/Data Science Project Report/2018-2019 West Sussex Sch ks2 k4 ks5/938_ks5final.csv") %>%
  select(c("SCHNAME", "PCODE", "TALLPPE_ALEV_1618")) %>%
  dplyr::rename("KS5_School_Name" = "SCHNAME",
                "KS5_Postcode" = "PCODE",
                "KS5_Average_Point_A.level" = "TALLPPE_ALEV_1618")

ks5_West.Sussex_Sch = right_join(WestSussex.GEO.PPL, ks5_West.Sussex_Sch, by = c("Postcode" = "KS5_Postcode"), keep = TRUE) %>%
  select(c("KS5_Postcode","LSOA_Name", "District_Name", "KS5_School_Name", "KS5_Average_Point_A.level")) %>%
  slice(-c(35:37)) %>% 
  dplyr::mutate(County_Name = "West Sussex", .after = KS5_Postcode) %>%
  filter(KS5_Average_Point_A.level != c("SUPP"))

ks5_West.Sussex_Sch$KS5_Average_Point_A.level = as.numeric(ks5_West.Sussex_Sch$KS5_Average_Point_A.level)




ks4_West.Sussex_Sch = read_csv("/Users/apple/Desktop/Data Science for Developers/Data Science Project Report/2018-2019 West Sussex Sch ks2 k4 ks5/938_ks4final.csv") %>%
  select(c("SCHNAME", "PCODE", "KS2APS")) %>%
  dplyr::rename("KS4_School_Name" = "SCHNAME",
                "KS4_Postcode" = "PCODE",
                "KS4_Average.Cohort.Score" = "KS2APS")

ks4_West.Sussex_Sch = right_join(WestSussex.GEO.PPL, ks4_West.Sussex_Sch, by = c("Postcode" = "KS4_Postcode"), keep = TRUE) %>%
  select(c("KS4_Postcode", "LSOA_Name", "District_Name", "KS4_School_Name", "KS4_Average.Cohort.Score")) %>%
  slice(-c(74:76)) %>% 
  dplyr::mutate(County_Name = "West Sussex", .after = KS4_Postcode) %>%
  filter(KS4_Average.Cohort.Score != c("NP")) %>%
  filter(KS4_Average.Cohort.Score != c("SUPP")) %>%
  filter(KS4_Average.Cohort.Score != c(""))

ks4_West.Sussex_Sch$KS4_Average.Cohort.Score = as.numeric(ks4_West.Sussex_Sch$KS4_Average.Cohort.Score)



ks2_West.Sussex_Sch = read_csv("/Users/apple/Desktop/Data Science for Developers/Data Science Project Report/2018-2019 West Sussex Sch ks2 k4 ks5/938_ks2final.csv") %>%
  select(c("SCHNAME", "PCODE", "TKS1AVERAGE")) %>%
  dplyr::rename("KS2_School_Name" = "SCHNAME",
                "KS2_Postcode" = "PCODE",
                "KS2_Average.Cohort.Score" = "TKS1AVERAGE")

ks2_West.Sussex_Sch = right_join(WestSussex.GEO.PPL, ks2_West.Sussex_Sch, by = c("Postcode" = "KS2_Postcode"), keep = TRUE) %>%
  select(c("KS2_Postcode", "LSOA_Name", "District_Name", "KS2_School_Name", "KS2_Average.Cohort.Score")) %>%
  slice(-c(222:224)) %>% 
  dplyr::mutate(County_Name = "West Sussex", .after = KS2_Postcode) %>%
  filter(KS2_Average.Cohort.Score != c("SUPP")) %>%
  filter(KS2_Average.Cohort.Score != c(""))

ks2_West.Sussex_Sch$KS2_Average.Cohort.Score = as.numeric(ks2_West.Sussex_Sch$KS2_Average.Cohort.Score)



Average.KS5.West.Sussex = ks5_West.Sussex_Sch %>%
  group_by(District_Name) %>%
  dplyr::summarise(KS5.Average.A_Level.Score = mean(KS5_Average_Point_A.level)) %>%
  mutate_if(is.numeric, round) %>%
  dplyr::mutate(County_Name = "West Sussex", .before = District_Name)



Average.KS4.West.Sussex = ks4_West.Sussex_Sch %>%
  group_by(District_Name) %>%
  dplyr::summarise(KS4_Average.Cohort.Score = mean(KS4_Average.Cohort.Score)) %>%
  mutate_if(is.numeric, round) %>%
  dplyr::mutate(County_Name = "West Sussex", .before = District_Name)


Average.KS2.West.Sussex = ks2_West.Sussex_Sch %>%
  group_by(District_Name) %>%
  dplyr::summarise(KS2_Average.Cohort.Score = mean(KS2_Average.Cohort.Score)) %>%
  mutate_if(is.numeric, round) %>%
  dplyr::mutate(County_Name = "West Sussex", .before = District_Name)

Average.West.Sussex.School.Quality = full_join(Average.KS2.West.Sussex, Average.KS4.West.Sussex) %>% full_join(., Average.KS5.West.Sussex) %>%
  group_by(District_Name) %>%
  dplyr::mutate(Total.Average.School.Quality = mean(c(KS2_Average.Cohort.Score, KS4_Average.Cohort.Score, KS5.Average.A_Level.Score)), .after = District_Name) %>%
  mutate_if(is.numeric, round)


View(Average.West.Sussex.School.Quality)

#View(ks5_West.Sussex_Sch)
#View(ks4_West.Sussex_Sch)
#View(ks2_West.Sussex_Sch)


###### VISUALIZATION || WEST SUSSEX SCHOOL QUALITY ##############################################

Visualization.1.West_Sussex.Sch.Quality = Average.West.Sussex.School.Quality %>% gather(key = Season, value = Value, Total.Average.School.Quality:KS5.Average.A_Level.Score)

ggplot(Visualization.1.West_Sussex.Sch.Quality, aes(District_Name, Value, fill = Season)) + 
  geom_col(position = "dodge")  +
  labs(
    fill = "Average Scores by Key Stage Levels\nand Total Average of them",
    title = "Average School Scores of West Sussex by District and Key Stage Levels",
    subtitle = "Study Period: 2019/2020",
    caption = "Author: Nurlan Guliyev") +
  theme(
    plot.title = element_text(size = 19, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "blue", size = 14 ),
    plot.caption = element_text(size = 13, face = "italic")) +
  scale_fill_discrete(
    labels = c("Key Stage 2, Average Cohort Score", "Key Stage 4, Average Cohort Score",
               "Key Stage 5, Average A Level Score", "Total Average of all Key Stages")) +
  xlab("Districts of West Sussex") + 
  ylab("Average Cohort and A Level Scores") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))



#######################################################


Result.Surrey = full_join(Average.Surrey.HP, Average.Police.Surrey) %>% full_join(., Average.Surrey.School.Quality) %>%
  select(c("District_Name", "Average.House.Price", "Crime_Rate", "Total.Average.School.Quality"))

View(Result.Surrey)

# Result.Surrey %>%
#   select(-c("Average.House.Price")) %>%
#   ggplot(aes(Crime_Rate, Total.Average.School.Quality)) +
#   geom_point(aes(colour = District_Name), size = 6) +
#   geom_smooth(method = "lm", se = FALSE) 
#   # geom_segment(aes(xend=Crime_Rate,yend=Total.Average.School.Quality), colour="red", size=8)
# 
# 
# model = lm(Total.Average.School.Quality~Crime_Rate,data=Result.Surrey)
# autoplot(model)

Result.West.Sussex = full_join(Average.West_Sussex.HP, Average.Police.West.Sussex) %>% full_join(., Average.West.Sussex.School.Quality) %>%
  select(c("District_Name", "Average.House.Price", "Crime_Rate", "Total.Average.School.Quality"))

View(Result.West.Sussex)

# ggplot(Result.West.Sussex,aes(Crime_Rate,Average.House.Price)) +
#   geom_point() +
#   geom_smooth(method=lm, se=FALSE) + 
#   geom_text(label=rownames(Result.West.Sussex))


setwd("~/Desktop/Data Science for Developers/Data Science Project Report")








