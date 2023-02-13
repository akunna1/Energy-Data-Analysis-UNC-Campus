# Set working directory to Source File Location
# The CSV files from my desktop

# Load libraries
library(tidyverse)  # dealing with tabular data
library(lubridate) # handling dates
library(dplyr) # data manipulation
library(tidyr) # tidy data
library(data.table) # table formatted result
library(ggplot2) # visualization
library(scales)# graphical scaling

# Importing my .csv data files to the Global Environment
eui_data_2019 = read_csv("eui2019_ake.csv")
br_data_2019 = read_csv("buildings_and_renos_2021_ake.csv")

eui_data_joined = left_join(eui_data_2019, br_data_2019, by="Building_number")
view(eui_data_joined) # contains 252 entries

#_____________________________________________________________________________________________________________________________________________________________________________________________________________________________________
#Adding two new columns to joined dataframe: steam/area and chilled/area
eui_data_joined_1 <- eui_data_joined # Replicating data
eui_data_joined_1$Steam_area <- eui_data_joined_1$Steam_in_klb / eui_data_joined_1$Square_feet # Creating new column and adding it to dataframe
view(eui_data_joined_1) # Printing new data- contains steam/area

eui_data_joined_2 <- eui_data_joined_1 # Replicating data
eui_data_joined_2$Chilled_area <- eui_data_joined_2$Chilled_water_ton_hr / eui_data_joined_2$Square_feet # Creating new column and adding it to dataframe
view(eui_data_joined_2) # Printing new data- contains Steam/area and Chilled/area

write.csv(eui_data_joined_2,file='/Users/akunna1/Desktop/joined_file_edited.csv')

#_____________________________________________________________________________________________________________________________________________________________________________________________________________________________________
#Filtering to remove NAs
#removing NAs in Steam_in_klb, Chilled_water_ton_hr and EUI_in_mmbtu_per_sqft columns
eui_data_sa <- filter(eui_data_joined_2, !is.na(eui_data_joined_2$Steam_area)) #Removes NAs for Steam_area
eui_data_ca <- filter(eui_data_joined_2, !is.na(eui_data_joined_2$Chilled_area))#Removes NAs for Chilled_area
eui_data_ea <-filter(eui_data_joined_2, !is.na(eui_data_joined_2$EUI_in_mmbtu_per_sqft))#Removes NAs for EUI

view(eui_data_sa) #gives 147 entries
view(eui_data_ca)#gives 147 entries
view(eui_data_ea) #gives 252 entries

#_____________________________________________________________________________________________________________________________________________________________________________________________________________________________________
#Grouping and Counting
#Grouping the joined data with new columns by property type and doing a count
property_type_count <- eui_data_joined_2%>%
  group_by(Property_type) %>%
  summarise(Total = n()) # grouping the joined data into property type by count
view(property_type_count)

#Grouping Steam_area data by property type and doing a count
property_type_count_steam <- eui_data_sa%>%
  group_by(Property_type) %>%
  summarise(Total = n()) # grouping the steam_area data into property type by count
view(property_type_count_steam)

#Grouping chilled_area data by property type and doing a count
property_type_count_chilled <- eui_data_ca%>%
  group_by(Property_type) %>%
  summarise(Total = n()) # grouping the chilled_area data into property type by count
view(property_type_count_chilled)

#Grouping eui data by property type and doing a count
property_type_count_eui <- eui_data_ea%>%
  group_by(Property_type) %>%
  summarise(Total = n()) # grouping the eui data into property type by count
view(property_type_count_eui)

#_____________________________________________________________________________________________________________________________________________________________________________________________________________________________________
#Boxplots
#boxplot1: Steam_area
qplot(x=eui_data_sa$Property_type, y=eui_data_sa$Steam_area, geom="boxplot", main="Boxplot of Steam per Area (klb/Sqft) by Property type", ylab="Steam per Area(klb/Sqft)", xlab="Property Type", col=I('black'), fill=eui_data_sa$Property_type) + 
  coord_flip()+
  theme(legend.position="none") #to remove legend

#boxplot2: Chilled_area
qplot(x=eui_data_ca$Property_type, y=eui_data_ca$Chilled_area, geom="boxplot", main="Boxplot of Chilled Water per Area (ton-hr/Sqft) by Property type", ylab="Chilled Water per Area (ton-hr/Sqft)", xlab="Property Type", col=I('black'), fill=eui_data_ca$Property_type) + 
  coord_flip()+
  theme(legend.position="none") #to remove legend

#boxplot3: EUI
qplot(x=eui_data_ea$Property_type, y=eui_data_ea$EUI_in_mmbtu_per_sqft, geom="boxplot", main="Boxplot of EUI (MMBTU/Sqft) by Property type", ylab="EUI (MMBTU/Sqft)", xlab="Property Type", col=I('black'), fill=eui_data_ea$Property_type) + 
  coord_flip()+
  theme(legend.position="none") #to remove legend
#_____________________________________________________________________________________________________________________________________________________________________________________________________________________________________
#For Diana:
#Filtering data to remove all the NA values of Steam_area, chilled_area, eui
#NAs for steam have already been removed (eui_data_sa)
eui_data_sca_x <- filter(eui_data_sa, !is.na(eui_data_sa$Chilled_area)) #removes NAs for chilled water
view(eui_data_sca_x) #gives 126 entries
eui_data_scea_x <- filter(eui_data_sca_x, !is.na(eui_data_sca_x$EUI_in_mmbtu_per_sqft))
view(eui_data_scea_x) #gives 126 entries

write.csv(eui_data_scea_x,file='/Users/akunna1/Desktop/edited_file.csv')
#_____________________________________________________________________________________________________________________________________________________________________________________________________________________________________
#Line Graph for Year Occupied Vs. Year of Latest Renovation
#Filtering to remove rows with NAs for columns Year_occupied and Latest_reno
eui_data_n1 <- filter(eui_data_joined_2, !is.na(eui_data_joined_2$Year_occupied))
view(eui_data_n1) #gives 236 entries

eui_data_n2 <- filter(eui_data_n1, !is.na(eui_data_n1$Latest_reno))
view(eui_data_n2) #gives 117 entries

#Plotting
ggplot()+
  geom_point(data = eui_data_n2, se=FALSE, color='black', mapping = aes(x=Year_occupied, y=Latest_reno))+
  labs(x='Year Occupied', y='Year of Latest Renovation', title='Latest Renovation Year Vs. Occupancy Year')
#_____________________________________________________________________________________________________________________________________________________________________________________________________________________________________

#Summaries of steam_area, chilled_area, EUI
summary(eui_data_sa$Steam_area) #Steam_area (steam/area)
summary(eui_data_ca$Chilled_area) #Chilled_area (chilled water/area)
summary(eui_data_ea$EUI_in_mmbtu_per_sqft) #EUI

#Creating Histograms for steam_area, chilled_area, EUI
hist(eui_data_sa$Steam_area, main="Histogram of Steam per Area", xlab="Steam per Area (klb/Sqft)", col="pink")
hist(eui_data_ca$Chilled_area, main="Histogram of Chilled Water per Area", xlab="Chilled Water per Area (ton*hr/Sqft)", col="brown")
hist(eui_data_ea$EUI_in_mmbtu_per_sqft, main="Histogram of EUI", xlab="EUI (MMBtu/sqft)", col="purple")

#Plotting Densities for steam_area, chilled_area, EUI
plot(density(eui_data_sa$Steam_area), main="Steam per Area Density")
plot(density(eui_data_ca$Chilled_area), main="Chilled Water per Area Density")
plot(density(eui_data_ea$EUI_in_mmbtu_per_sqft), main="EUI Density")

#_____________________________________________________________________________________________________________________________________________________________________________________________________________________________________
#Extracting the Outliers for steam_area, chilled_area, EUI by property type
#Steam_area: extracting outliers
#Outliers are present in the property types of research laboratory (1),lab/class (1), classrooms(1), atheltic (1), administrative (3)
eui_data_sa%>%
  filter(Property_type=="RESEARCH LABORATORY")%>%
  View() #check out the steam_area max in the table (1 max)

eui_data_sa%>%
  filter(Property_type=="LAB/CLASS")%>%
  View() #check out the steam_area max in the table (1 max)

eui_data_sa%>%
  filter(Property_type=="CLASSROOMS")%>%
  View() #check out the steam_area max in the table (1 max)

eui_data_sa%>%
  filter(Property_type=="ATHLETIC")%>%
  View() #check out the steam_area max in the table (1 max)

eui_data_sa%>%
  filter(Property_type=="ADMINISTRATIVE")%>%
  View() #check out the steam_area max in the table (2 maxes)
#____________________________________
#Chilled Water: extracting outliers
eui_data_ca%>%
  filter(Property_type=="RESEARCH LABORATORY")%>%
  View() #check out the chilled_area max in the table (1 max)

eui_data_ca%>%
  filter(Property_type=="PHYSICAL ED")%>%
  View() #check out the chilled_area max in the table (1 max)

eui_data_ca%>%
  filter(Property_type=="LAB/CLASS")%>%
  View() #check out the chilled_area max and min in the table (1 max, 1 min)

eui_data_ca%>%
  filter(Property_type=="CONFERENCE CTR")%>%
  View() #check out the chilled_area min in the table (1 min)

eui_data_ca%>%
  filter(Property_type=="CLINICAL")%>%
  View() #check out the chilled_area min in the table (1 min)

eui_data_ca%>%
  filter(Property_type=="CLASSROOMS")%>%
  View() #check out the chilled_area max in the table (1 max)

eui_data_ca%>%
  filter(Property_type=="ATHLETIC")%>%
  View() #check out the chilled_area min in the table (1 min)

eui_data_ca%>%
  filter(Property_type=="ADMINISTRATIVE")%>%
  View() #check out the chilled_area maxes in the table (5 maxes)

#____________________________________
#EUI: extracting outliers
eui_data_ea%>%
  filter(Property_type=="SHOP SPACE")%>%
  View() #check out the EUI max in the table (1 max)

eui_data_ea%>%
  filter(Property_type=="RESIDENTIAL")%>%
  View() #check out the EUI max in the table (1 max)

eui_data_ea%>%
  filter(Property_type=="RESEARCH LABORATORY")%>%
  View() #check out the EUI max in the table (1 max)

eui_data_ea%>%
  filter(Property_type=="LAB/CLASS")%>%
  View() #check out the EUI min and max in the table (1 max and 1 min)

eui_data_ea%>%
  filter(Property_type=="HOTEL/DINING")%>%
  View() #check out the EUI max in the table (1 max)

eui_data_ea%>%
  filter(Property_type=="ADMINISTRATIVE")%>%
  View() #check out the EUI maxes in the table (5 maxes)
