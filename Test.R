# Load Packages / Libraries
library(dplyr)
library(plotly)
library(purrr)
library(corrly)
# Load Data
Tdata<-read.csv("./Ref_data.csv")
head(Tdata)
paste("Rows", nrow(Tdata))
paste("Columns", ncol(Tdata))

# Remove Strings from Supplier.Name Column
Tdata %>% mutate_all(~gsub("THE CROWN GROUP |THE CROWN GROUP CO |THE CROWN GROUP CO.", "", .))
Tdata %>% filter(Supplier.Name == "THE CROWN GROUP CO.")

# Check Strings Counts
RelParts<- Tdata %>% filter(Rep.Machine.Class== "OFP" | Rep.Machine.Class== "Related Parts")
head(RelParts)
count(Tdata, Rep.Machine.Class)
count(Tdata, Unit.Name)
count(Tdata, Supplier.Name)
# plot_ly(Tdata, x= ~Supplier.Name, type="heatmap", color = ~Supplier.Name)
count(Tdata, MGC)
summary(Tdata)

# DM Cost > than 1$
Tdata %>% filter(DM.Cost>1)
TTen<- Tdata %>% filter(DM.Cost>65)
count(TTen, Rep.Machine.Class)

####### Analysis for Mannheim¶

ManData<- Tdata %>% filter(Unit.Name == "Mannheim")
MGcheck<- Tdata %>% filter(MGC == c("D03030200","D03030110","D03050400","D14010110","D07010301","D07050200"))
head(MGcheck)
summary(MGcheck)

count(ManData, duplicated(ManData))

# Rows and ColumnS
paste(nrow(ManData), ncol(ManData))

# Check NA's
count(ManData, is.na("ManData"))
sum(is.na(ManData))
map(ManData, ~sum(is.na(.))) # Check NA's Columnwise


#=====================================================================================================================
#################### Analysis for MGC "D03030200","D03030110","D03050400","D14010110","D07010301","D07050200" ########

selclm<- Tdata %>% select(weight, DM.Cost)
selist<- c(selclm$weight, selclm$DM.Cost)

spearman<- corr_coef_spearman(variable1= Tdata$weight, variable2=Tdata$DM.Cost, decimal = 2)
corr_scatterly(data=Tdata,x=Tdata$weight,y=Tdata$DM.Cost,corr_coef=spearman,xname="Weight",yname="DM Cost")
