#Clear workspace.
rm(list = ls())

#Load all libraries.
library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(choroplethr)
library(choroplethrMaps)

#Import primary data sources.
df_GDP <- read_csv('ClassData/group13/CCI & GDP/YGDP 2014-2021.csv')
df_CCI <- read_csv('ClassData/group13/CCI & GDP/CCI 2014-2023.csv')

#Cleaning data.

#Subset for same time period.
df_CCI <- subset(df_CCI, TIME < '2022-01')

#Remove unnecessary columns.
df_GDP$SUBJECT <- NULL
df_GDP$`Flag Codes`<- NULL
df_GDP$MEASURE <- NULL
df_CCI$SUBJECT <- NULL
df_CCI$`Flag Codes`<- NULL
df_CCI$MEASURE <- NULL

#Change column names.
names(df_GDP)[names(df_GDP) == "LOCATION"] <- "country"
names(df_GDP)[names(df_GDP) == "TIME"] <- "year"
names(df_GDP) <- tolower(names(df_GDP))
names(df_CCI)[names(df_CCI) == "LOCATION"] <- "country"
names(df_CCI)[names(df_CCI) == "TIME"] <- "year_month"
names(df_CCI)[names(df_CCI) == "Value"] <- "monthly_cci"
names(df_CCI) <- tolower(names(df_CCI))

#Change column types.
df_GDP$year <- factor(df_GDP$year)
df_GDP$country <- factor(df_GDP$country)
df_CCI$country <- factor(df_CCI$country)

#Summary of data.
missing_GDP <- sum(is.na(df_GDP))
rows_GDP <- nrow(df_GDP)
cols_GDP <- ncol(df_GDP)
tot_avg_GDP <- mean(df_GDP$value, na.rm = TRUE)
print(summary(df_GDP$value))

missing_CCI <- sum(is.na(df_CCI))
rows_CCI <- nrow(df_CCI)
cols_CCI <- ncol(df_CCI)
tot_avg_CCI <- mean(df_CCI$monthly_cci, na.rm = TRUE)
print(summary(df_CCI$monthly_cci))

#Remove rows with missing values.
df_GDP <- df_GDP[complete.cases(df_GDP), ]
df_CCI <- df_CCI[complete.cases(df_CCI), ]


#Analysis

#Correlation of average GDP and CCI for all countries using time series.
#Create new df with only year and total avg. for each year.
df_tsGDP <- df_GDP %>% 
  select(year, value)
df_tsGDP <- df_tsGDP %>% 
  group_by(year) %>% 
  mutate(annual_GDP = mean(value))
df_tsGDP$value <- NULL #Drop.
df_tsGDP <- distinct(df_tsGDP) #Remove duplicates.

#Create new df with only year and total avg. for each year.
df_CCI$year <- df_CCI$year_month
df_CCI$year <- factor(df_CCI$year)
#For loop to change year and month to only year.
months <- c('-01', '-02', '-03', '-04', '-05', '-06', '-07', '-08', '-09', '-10', '-11', '-12')
for (i in 2014:2023){
  for (j in months){
    levels(df_CCI$year)[levels(df_CCI$year)== paste0(i,j)] <- i
  }
}  
df_tsCCI <- df_CCI %>% 
  select(year, monthly_cci)
df_tsCCI <- df_tsCCI %>% 
  group_by(year) %>% 
  mutate(annual_CCI = mean(monthly_cci))
df_tsCCI$monthly_cci <- NULL #Drop.
df_tsCCI <- distinct(df_tsCCI) #Remove duplicates.

#GDP time series.
p1 <- ggplot(data = df_tsGDP, aes(x = year, y = annual_GDP, group=1))
p1 <- p1 + geom_line()
p1 <- p1 + ggtitle ('Average Annual GDP 2014-2021')
p1 <- p1 + theme(plot.title = element_text(size = 20, face = "bold", hjust=0.5))
p1 <- p1 + theme(panel.grid.minor = element_blank())
p1 <- p1 + scale_y_continuous(labels = dollar)
print(p1)

#CCI time series.
p2 <- ggplot(data = df_tsCCI, aes(x = year, y = annual_CCI, group=1))
p2 <- p2 + geom_line()
p2 <- p2 + ggtitle ('Average Annual CCI 2014-2021')
p2 <- p2 + theme(plot.title = element_text(size = 20, face = "bold", hjust=0.5))
p2 <- p2 + theme(panel.grid.minor = element_blank())
print(p2)


#Scatter plot of GDP and CCI by year and country.
#Calculate avg. GDP by year and for each country in new df.
df1_GDP <- df_GDP %>% 
  group_by(country, year) %>% 
  mutate(value = mean(value))
#Remove duplicates.
df1_GDP <- df1_GDP %>% 
  select(country,year,value) %>%  
  distinct()

#Calculate avg. CCI by year and for each country in new df.
df1_CCI <- df_CCI %>% 
  group_by(country, year) %>% 
  mutate(value = mean(monthly_cci))
#Remove duplicates.
df1_CCI <- df1_CCI %>% 
  select(country,year,value) %>%  
  distinct()

#GDP scatter plot.
p3 <- ggplot(data = df1_GDP, aes(x = year, y = value, color = country))
p3 <- p3 + geom_point()
p3 <- p3 + ggtitle ('Annual GDP By Country 2014-2021')
p3 <- p3 + theme(plot.title = element_text(size = 20, face = "bold", hjust=0.1))
p3 <- p3 + theme(panel.grid.minor = element_blank())
p3 <- p3 + scale_y_continuous(labels = dollar,
                              name = 'Annual GDP')
print(p3)

#CCI scatter plot.
p4 <- ggplot(data = df1_CCI, aes(x = year, y = value, color = country))
p4 <- p4 + geom_point()
p4 <- p4 + ggtitle ('Annual Average CCI By Country 2014-2021')
p4 <- p4 + theme(plot.title = element_text(size = 20, face = "bold", hjust=0.1))
p4 <- p4 + theme(panel.grid.minor = element_blank())
p4 <- p4 + scale_y_continuous(name = 'Avg. Annual CCI')
print(p4)


#Choropeth maps of country avg. GDP and CCI from 2014-2021. 
#Calculate avg. GDP for each country in new df.
df2_GDP <- df_GDP %>% 
  group_by(country) %>% 
  mutate(value = mean(value))
#Remove duplicates.
df2_GDP <- df2_GDP %>% 
  select(country,value) %>%  
  distinct()
#Calculate avg. CCI for each country in new df.
df2_CCI <- df_CCI %>% 
  group_by(country) %>% 
  mutate(value = mean(monthly_cci))
#Remove duplicates.
df2_CCI <- df2_CCI %>% 
  select(country,value) %>%  
  distinct()
#Remove year column.
df2_GDP$year <- NULL
df2_CCI$year <- NULL

#Merge to create df with CCI and GDP.
df_both <- merge(df2_CCI, df2_GDP, by.x = 'country', by.y = 'country')
#Import file with ISO codes and full country names.
cnames <- read_csv('ClassData/group13/CCI & GDP/iso_3digit_alpha_country_codes.csv')
names(cnames)[names(cnames) == "...2"] <- "country.name"
names(cnames)[names(cnames) == "ISO 3-Digit Alpha Country Code"] <- "abbrev"
cnames <- cnames[complete.cases(cnames), ]

#GDP
#Create df with only GDP and merge with ISO.
summ1 <- df_both
summ1$value.x <- NULL
names(summ1)[names(summ1) == "value.y"] <- "value"
summ1 <- merge(summ1, cnames, by.x = 'country', by.y = 'abbrev')
summ1$country <- NULL
summ1$country.name <- tolower(summ1$country.name)
#Change country names to match country.regions df.
summ1[41, 2] = 'united states of america'
summ1[11, 2] = 'czech republic'
summ1[27, 2] = 'south korea'
summ1[36, 2] = 'russia'
names(summ1)[names(summ1) == "country.name"] <- "region"
#Choropleth of GDP.
p5 <- country_choropleth(summ1) +
  scale_fill_brewer(name="Avg. Annual GDP",palette = "Greens")
p5 <- p5 + ggtitle('2014-2021 Average GDP World Map')
p5 <- p5 + theme(plot.title = element_text(size = 20, face = "bold", hjust=0.1))
print(p5)


#CCI
#Create df with only CCI and merge with ISO.
summ2 <- df_both
summ2$value.y <- NULL
names(summ2)[names(summ2) == "value.x"] <- "value"
summ2 <- merge(summ2, cnames, by.x = 'country', by.y = 'abbrev')
summ2$country <- NULL
summ2$country.name <- tolower(summ2$country.name)
#Change country names to match country.regions df.
summ2[41, 2] = 'united states of america'
summ2[11, 2] = 'czech republic'
summ2[27, 2] = 'south korea'
summ2[36, 2] = 'russia'
names(summ2)[names(summ2) == "country.name"] <- "region"
#Choropleth of CCI.
p6 <- country_choropleth(summ2) +
  scale_fill_brewer(name="Avg. CCI",palette = "Blues")
p6 <- p6 + ggtitle('2014-2021 Average CCI World Map')
p6 <- p6 + theme(plot.title = element_text(size = 20, face = "bold", hjust=0.1))
print(p6)


#Correlation between GDP and CCI for high and low GDP countries.
#Find top 5 countries with highest avg. GDP 2014-2021.
df_both <- group_by(df_both, country)
summ3 <- summarize(df_both, avg_tot_GDP = mean(value.y))
summ3 <- summ3[order(summ3$avg_tot_GDP, decreasing = TRUE), ]
summ3 <- summ3[1:5, ]
print(summ3)
df_both <- ungroup(df_both)

#Find bottom 5 countries with lowest avg. GDP 2014-2021.
df_both <- group_by(df_both, country)
summ4 <- summarize(df_both, avg_tot_GDP = mean(value.y))
summ4 <- summ4[order(summ4$avg_tot_GDP, decreasing = FALSE), ]
summ4 <- summ4[1:5, ]
print(summ4)
df_both <- ungroup(df_both)

#Bar graph for top 5 and bottom 5 countries.
#Top 5
df_top5GDP <- subset(df2_GDP, country =='LUX'| country == 'IRL' | country == 'CHE'|
                       country == 'USA'| country == 'NLD')
df_CCIfortop5GDP <- subset(df2_CCI, country =='LUX'| country == 'IRL' | country == 'CHE'|
                 country == 'USA'| country == 'NLD')
#GDP
p7 <- ggplot(data = df_top5GDP, aes(x = country, y = value, fill = country))
p7 <- p7 + geom_bar(stat = 'identity')
p7 <- p7 + ggtitle ('Top 5 Average Total GDP 2014-2021')
p7 <- p7 + theme(plot.title = element_text(size = 20, face = "bold", hjust=0.5))
p7 <- p7 + theme(panel.grid.minor = element_blank())
p7 <- p7 + scale_y_continuous(labels = dollar,
                              name = 'Avg. GDP')
print(p7)

#CCI
p8 <- ggplot(data = df_CCIfortop5GDP, aes(x = country, y = value, fill = country))
p8 <- p8 + geom_bar(stat = 'identity')
p8 <- p8 + ggtitle ('Average CCI for Top 5 GDP Countries 2014-2021')
p8 <- p8 + theme(plot.title = element_text(size = 16, face = "bold", hjust=0.2))
p8 <- p8 + theme(panel.grid.minor = element_blank())
p8 <- p8 + scale_y_continuous(name = 'Avg. CCI',
                              limits = c(99,102),
                              oob = rescale_none)
print(p8)

#Bottom 5
df_bott5GDP <- subset(df2_GDP, country =='IND'| country == 'IDN' | country == 'ZAF'|
                       country == 'BRA'| country == 'CHN')
df_CCIforbott5GDP <- subset(df2_CCI, country =='IND'| country == 'IDN' | country == 'ZAF'|
                        country == 'BRA'| country == 'CHN')
#GDP
p9 <- ggplot(data = df_bott5GDP, aes(x = country, y = value, fill = country))
p9 <- p9 + geom_bar(stat = 'identity')
p9 <- p9 + ggtitle ('Bottom 5 Average Total GDP 2014-2021')
p9 <- p9 + theme(plot.title = element_text(size = 20, face = "bold", hjust=0.5))
p9 <- p9 + theme(panel.grid.minor = element_blank())
p9 <- p9 + scale_y_continuous(labels = dollar,
                              name = 'Avg. GDP')
print(p9)

#CCI
p10 <- ggplot(data = df_CCIforbott5GDP, aes(x = country, y = value, fill = country))
p10 <- p10 + geom_bar(stat = 'identity')
p10 <- p10 + ggtitle ('Average CCI for Bottom 5 GDP Countries 2014-2021')
p10 <- p10 + theme(plot.title = element_text(size = 16, face = "bold", hjust=0.2))
p10 <- p10 + theme(panel.grid.minor = element_blank())
p10 <- p10 + scale_y_continuous(name = 'Avg. CCI',
                              limits = c(99,102),
                              oob = rescale_none)
print(p10)

#Correlation coefficients.
cor_top5 <- cor(df_top5GDP$value, df_CCIfortop5GDP$value,
                method="spearman")
cor_bott5 <- cor(df_bott5GDP$value, df_CCIforbott5GDP$value,
                 method="spearman")

