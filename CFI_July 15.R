install.packages("dplyr")
install.packages("stringr")
install.packages("lubridate")
install.packages("formattable")
install.packages("tidyr")
install.packages("ggplot2")
library("dplyr")
library("stringr")
library("lubridate")
library("formattable")
library("tidyr")
library("ggplot2")

#Load dataset
CFI_Dataset <- read.csv("C:/Users/charlotte.mihailov/Downloads/CFI_List of Awards_Clean Data.csv", header = TRUE, stringsAsFactors =  FALSE)

#Change column titles
names(CFI_Dataset) <- c("Province", "Inst_Name", "Inst_Type", "Inst_Funding", "Inst_CRCs", "Award_Name", "PI_Name", "PI_Sex", "Award", "Date", "Keywords", "Sector_RD", "RD", "Sector_Application", "Application")

#View summary and structure of dataset
summary(CFI_Dataset)
str(CFI_Dataset)

#Change class types
CFI_Dataset$Inst_Funding <- currency(as.numeric(gsub('[$,]', '', CFI_Dataset$Inst_Funding), rm.na = T), digits = 0L)
CFI_Dataset$Award <- currency(as.numeric(gsub('[$,]', '', CFI_Dataset$Award), rm.na = T))
CFI_Dataset$Date <- as.Date(CFI_Dataset$Date, "%d-%b-%y")
CFI_Dataset$PI_Sex <- as.factor(CFI_Dataset$PI_Sex)

#Cleaning keywords
CFI_Dataset$Keywords <- tolower(CFI_Dataset$Keywords)
CFI_Dataset$Keywords <- gsub("[^ -~]", "", CFI_Dataset$Keywords)
CFI_Dataset$Keywords <- gsub("[.]", "", CFI_Dataset$Keywords)
CFI_Dataset$Keywords <- gsub(",", "", CFI_Dataset$Keywords)
CFI_Dataset$Keywords <- gsub(";", "", CFI_Dataset$Keywords)
CFI_Dataset$Keywords <- strsplit(CFI_Dataset$Keywords, " ")

#Number of institution types by province
Table_1 <- CFI_Dataset %>%
  select(Province, Inst_Type) %>%
  group_by(Province, Inst_Type) %>%
  summarise(n())

#Institutional funding by province
Table_2 <- CFI_Dataset %>%
    select(Province, Inst_Funding) %>%
    distinct(Inst_Funding, .keep_all = T) %>%
    summarise(AB = sum(Inst_Funding[Province == "AB"]),
              BC = sum(Inst_Funding[Province == "BC"]),
              MB = sum(Inst_Funding[Province == "MB"]),
              NB = sum(Inst_Funding[Province == "NB"]),
              NL = sum(Inst_Funding[Province == "NL"]),
              NS = sum(Inst_Funding[Province == "NS"]),
              ON = sum(Inst_Funding[Province == "ON"]),
              PE = sum(Inst_Funding[Province == "PE"]),
              QC = sum(Inst_Funding[Province == "QC"]),
              SK = sum(Inst_Funding[Province == "SK"]))
Table_2 <- gather(Table_2, "Province", "Inst_Funding")
Table_2_Num <- Table_2
Table_2_Num$Inst_Funding <- as.numeric(Table_2$Inst_Funding)
barplot(Table_2_Num$Inst_Funding, names.arg = Table_2_Num$Province, yaxt = "n")
axis(2, at=axTicks(2), labels=sprintf("$%s", axTicks(2)), las=1)

#Number of CRCs by province
Table_3 <- CFI_Dataset %>%
  na.omit() %>%
  select(Province, Inst_Name, Inst_CRCs) %>%
  distinct(Inst_Name, .keep_all = T) %>%
  summarise(AB = sum(Inst_CRCs[Province == "AB"]),
            BC = sum(Inst_CRCs[Province == "BC"]),
            MB = sum(Inst_CRCs[Province == "MB"]),
            NB = sum(Inst_CRCs[Province == "NB"]),
            NL = sum(Inst_CRCs[Province == "NL"]),
            NS = sum(Inst_CRCs[Province == "NS"]),
            ON = sum(Inst_CRCs[Province == "ON"]),
            PE = sum(Inst_CRCs[Province == "PE"]),
            QC = sum(Inst_CRCs[Province == "QC"]),
            SK = sum(Inst_CRCs[Province == "SK"]))
Table_3 <- gather(Table_3, "Province", "Inst_CRCs")
barplot(Table_3$Inst_CRCs, names.arg = Table_2_Num$Province)

#Number of CFI Awards by province
Table_4 <- CFI_Dataset %>%
  select(Province, Award) %>%
  group_by(Province) %>%
  summarise(n())

#Awarded CFI funding by province
Table_5 <- CFI_Dataset %>%
  select(Province, Award) %>%
  group_by(Province) %>%
  summarise(sum(Award))

#Female vs Male ratio by province
Table_6 <- CFI_Dataset %>%
  select(Province, PI_Sex) %>%
  group_by(Province) %>%
  count(PI_Sex)
Table_6 <- spread(Table_6, PI_Sex, n)
Table_6 <- Table_6[,-4]
names(Table_6) <- c("Province", "Females", "Males")
Table_6$Ratio <- Table_6$Females / Table_6$Males

#Top 30 - Number of CFI awards by institution
Table_7 <- CFI_Dataset %>%
  select(Inst_Name, Award) %>%
  group_by(Inst_Name) %>%
  summarise(Awards = n()) %>%
  arrange(desc(Awards))
summary(Table_7)
barplot(Table_7$Awards)
  
#Top 30 - CFI awarded funding by institution
Table_8 <- CFI_Dataset %>%
  select(Inst_Name, Award) %>%
  group_by(Inst_Name) %>%
  summarise(Awards = sum(Award)) %>%
  arrange(desc(Awards))
summary(Table_8)

#Female vs Male ratio by institution
Table_9 <- CFI_Dataset %>%
  select(Inst_Name, PI_Sex) %>%
  group_by(Inst_Name) %>%
  count(PI_Sex)
Table_9 <- spread(Table_9, PI_Sex, n)
Table_9 <- Table_9[,-4]
names(Table_9) <- c("Inst_Name", "Females", "Males")
Table_9$Ratio <- Table_9$Females / Table_9$Males
summary(Table_9$Ratio)
Table_9_Least_Equal <- head(Table_9[order(Table_9$Ratio),], n = 20L)
Table_9_Most_Equal <- head(Table_9[order(Table_9$Ratio, decreasing = TRUE),], n = 20L)

#PIs who received awards more than once
Table_10 <- CFI_Dataset %>%
  select(PI_Name, Award) %>%
  group_by(PI_Name) %>%
  summarise(Awards = n()) %>%
  filter(Awards > 1) %>%
  arrange(desc(Awards))

Table_10_Inst <- CFI_Dataset %>%
  select(Inst_Name, PI_Name, Award) %>%
  group_by(Inst_Name, PI_Name) %>%
  summarise(Awards = n()) %>%
  filter(Awards > 1) %>%
  arrange(desc(Awards))

#Number of CFI awards by institution type
Table_11 <- CFI_Dataset %>%
  select(Inst_Type, Award) %>%
  group_by(Inst_Type) %>%
  summarise(Awards = n()) %>%
  arrange(desc(Awards))

#Number of CFI awarded funding by institution type
Table_12 <- CFI_Dataset %>%
  select(Inst_Type, Award) %>%
  group_by(Inst_Type) %>%
  summarise(Awards = sum(Award)) %>%
  arrange(desc(Awards))

#Correlation: Number of CRCs vs institutional funding (Universities)
cor(CFI_Dataset$Inst_CRCs, CFI_Dataset$Inst_Funding, use = "complete.obs")

#Correlation: CFI Awarded Funding vs institutional funding
Table_13 <- CFI_Dataset %>%
  select(Inst_Funding, Inst_Name) %>%
  distinct(Inst_Name, Inst_Funding) %>%
  na.omit()
Table_14 <- left_join(Table_13, Table_8, by="Inst_Name")
cor(Table_14$Inst_Funding, Table_14$Awards, use = "complete.obs")
plot(Table_14$Inst_Funding, Table_14$Awards)

#Correlation: Number of CFI Awards vs institutional funding
Table_15 <- left_join(Table_13, Table_7, by="Inst_Name")
cor(Table_15$Inst_Funding, Table_15$Awards, use = "complete.obs")
plot(Table_15$Inst_Funding, Table_15$Awards)

#Variable Review: Institutional Funding
summary(CFI_Dataset$Award)
Distinct_Inst_Funding <- as.numeric(unique(CFI_Dataset$Inst_Funding))
summary(Distinct_Inst_Funding)
Distinct_Inst_Funding_Log <- log(Distinct_Inst_Funding)
boxplot(Distinct_Inst_Funding_Log)
hist(Distinct_Inst_Funding_Log)

#Variable Review: CFI Awarded Funding
Award_Log <- log(CFI_Dataset$Award)
boxplot(Award_Log)
hist(Award_Log)

#Female vs Male: CFI Awarded Funding
Table_16 <- CFI_Dataset %>%
  select(PI_Sex, Award) %>%
  group_by(PI_Sex) %>%
  summarise(Award = sum(Award))

#Funding Awarded by Year
Table_17 <- aggregate(CFI_Dataset$Award, by=list(CFI_Dataset$Date), sum)
Table_18 <- CFI_Dataset %>%
  select(Date, Award) %>%
  group_by(year = floor_date(Date, "year")) %>%
  summarise(Award = sum(Award))

plot(Table_18)

Table_19 <- CFI_Dataset %>%
  select(Date, Award) %>%
  group_by(year = floor_date(Date, "year")) %>%
  summarise(Awards = n())

plot(Table_19)

#Funding Awarded by Quarter and Fiscal Year (April 1 - March 31)
Table_20 <- CFI_Dataset %>%
  select(Date, Award) %>%
  group_by(Quarter = quarter(Date, with_year = T, fiscal_start = 4)) %>%
  summarise(Award = sum(Award))

plot(Table_20)

Table_21 <- CFI_Dataset %>%
  select(Date, Award) %>%
  group_by(Quarter = quarter(Date, with_year = T, fiscal_start = 4)) %>%
  summarise(Awards = n())

plot(Table_21)

#Sector Research Discipline
Table_22 <- CFI_Dataset %>%
  select(Award, Sector_RD) %>%
  group_by(Sector_RD) %>%
  summarise(Awards = n())

Table_23 <- CFI_Dataset %>%
  select(Award, Sector_RD) %>%
  group_by(Sector_RD) %>%
  summarise(Awards = sum(Award))

#Sector Area of Application
Table_24 <- CFI_Dataset %>%
  select(Award, Sector_Application) %>%
  group_by(Sector_Application) %>%
  summarise(Awards = n())

Table_25 <- CFI_Dataset %>%
  select(Award, Sector_Application) %>%
  group_by(Sector_Application) %>%
  summarise(Awards = sum(Award))

#Sector Research Discipline over the years
Table_26 <- CFI_Dataset %>%
  select(Award, Sector_RD, Date) %>%
  group_by(year = floor_date(Date, "year"), Sector_RD) %>%
  summarise(Awards = n())

ggplot(Table_26, aes(fill=Sector_RD, y=Awards, x=year)) + 
  geom_bar( stat="identity")

Table_27 <- CFI_Dataset %>%
  select(Award, Sector_RD, Date) %>%
  group_by(year = floor_date(Date, "year"), Sector_RD) %>%
  summarise(Awards = sum(Award))

ggplot(Table_27, aes(fill=Sector_RD, y=Awards, x=year)) + 
  geom_bar( stat="identity")

#Sector Area of Application over the years
Table_28 <- CFI_Dataset %>%
  select(Award, Sector_Application, Date) %>%
  group_by(year = floor_date(Date, "year"), Sector_Application) %>%
  summarise(Awards = n())

ggplot(Table_28, aes(fill=Sector_Application, y=Awards, x=year)) + 
  geom_bar( stat="identity")

Table_29 <- CFI_Dataset %>%
  select(Award, Sector_Application, Date) %>%
  group_by(year = floor_date(Date, "year"), Sector_Application) %>%
  summarise(Awards = sum(Award))

ggplot(Table_29, aes(fill=Sector_Application, y=Awards, x=year)) + 
  geom_bar( stat="identity")

