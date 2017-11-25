# Install packages and libraries

#install.packages("dyplr")
#install.packages("rvest")
#install.packages("xml2")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("curl")

#Get libraries
library("dplyr")
library("rvest")
library("tidyr")
library("ggplot2")

# Session Info
sessionInfo()


# 2A: Read in CSV
# How many rows and columns (4264 rows and 61 columns)
procrast <- read.csv("C:\\SMU_INTRO_TO_DS\\CASESTUDY2\\Data\\Procrastination.csv")

dim(procrast)

# 2B: Update column names
colnames(procrast) <- c("Age","Gender","Kids","Edu","WorkStatus","AnnualIncome","Occupation","YrsHeld","MthsHeld","CommSize",
												"Country","MaritalStat","Sons","Daughters","DP1","DP2","DP3","DP4","DP5","AIP1","AIP2","AIP3","AIP4",
												"AIP5","AIP6","AIP7","AIP8","AIP9","AIP10","AIP11","AIP12","AIP13","AIP14","AIP15","GP1","GP2","GP3",
												"GP4","GP5","GP6","GP7","GP8","GP9","GP10","GP11","GP12","GP13","GP14","GP15","GP16",
												"GP17","GP18","GP19","GP20","SWLS1","SWLS2","SWLS3","SWLS4","SWLS5","SelfView","OthersView")

# 2C: Cleanup the data

# 2Ci:
# Position Yrs Held Cleanup
# Clean YrsHeld Column (column referring to job years) 
procrast$YrsHeld <- sub("e\\S+","",procrast$YrsHeld,perl=T)

# Change YrsHeld to numeric and round
procrast$YrsHeld <- round(as.numeric(procrast$YrsHeld), digits = 0)

unique(procrast$YrsHeld)

# If YrsHeld is greater than 70 years then set the value to NA. Although 70 years is still a very long time, just want 
# to be sure to include and company 'lifers' in case they exist.  
procrast$YrsHeld <- ifelse(procrast$YrsHeld > 70, NA, procrast$YrsHeld)

# 2cii:
# Convert to character so can replace Male and Female anomolies
procrast$Sons <- as.character(procrast$Sons)

unique(procrast$Sons)

# If female should be 2, if male should be 1
procrast$Sons <- ifelse(procrast$Sons == "Female", 2, procrast$Sons)
procrast$Sons <- ifelse(procrast$Sons == "Male", 1, procrast$Sons)

# Convert sons to integer
procrast$Sons <- strtoi(procrast$Sons)


# 2ciii:
# Country of Residence Cleanup
# Change Country to Character
procrast$Country <- as.character(procrast$Country)

# Review unique values
unique(procrast$Country)

# Update empty string and 0 values to NA
procrast$Country <- ifelse(procrast$Country == "0", "NA", procrast$Country)
procrast$Country <- ifelse(procrast$Country == "", "NA", procrast$Country)


# 2civ:
# Occupation cleanup
# Find unique occupations
unique(procrast$Occupation)

# Update to character string
procrast$Occupation <- as.character(procrast$Occupation)

# Update occupations for 0, empty and please specify to NA
procrast$Occupation <- ifelse(procrast$Occupation == "0", "NA", procrast$Occupation)
procrast$Occupation <- ifelse(procrast$Occupation == "please specify", "NA", procrast$Occupation)
procrast$Occupation <- ifelse(procrast$Occupation == "", "NA", procrast$Occupation)

# Update anything like teacher
procrast$Occupation[grepl("teacher", procrast$Occupation, ignore.case = TRUE)] <- "Teacher"

# Update anything like director
procrast$Occupation[grepl("director", procrast$Occupation, ignore.case = TRUE)] <- "Director"

# Update anything like analyst
procrast$Occupation[grepl("analyst", procrast$Occupation, ignore.case = TRUE)] <- "Analyst"

# Update anything like manager
procrast$Occupation[grepl("manager", procrast$Occupation, ignore.case = TRUE)] <- "Manager"

# 2D:

# Update column data types
procrast$Gender <- as.character(procrast$Gender)
procrast$Kids <- as.character(procrast$Kids)
procrast$Edu <- as.character(procrast$Edu)
procrast$WorkStatus <- as.character(procrast$WorkStatus)
procrast$AnnualIncome <- as.numeric(procrast$AnnualIncome)
procrast$MthsHeld <- as.numeric(procrast$MthsHeld)
procrast$CommSize <- as.character(procrast$CommSize)
procrast$Country <- as.character(procrast$Country)
procrast$MaritalStat <- as.character(procrast$MaritalStat)
procrast$SelfView <- as.character(procrast$SelfView)
procrast$OthersView <- as.character(procrast$OthersView)

# 2E:

# Create new columns
procrast$DPMean <- "NA"
procrast$AIPMean <- "NA"
procrast$GPMean <- "NA"
procrast$SWLSMean <- "NA"

procrast$DPMean <- rowMeans(procrast[,15:19])
procrast$AIPMean <- rowMeans(procrast[,20:34])
procrast$GPMean <- rowMeans(procrast[,35:54])
procrast$SWLSMean <- rowMeans(procrast[,55:59])


# 3:
# Scrape the website


# 3A:
url <- "https://en.wikipedia.org/wiki/List_of_countries_by_Human_Development_Index#Complete_list_of_countries"
webpage <- read_html(url)

###Scrape WIKI and assign the 8 tables
tbls <- html_nodes(webpage, "table")
tbl1 <- html_table(tbls[4], fill = TRUE)
tbl2 <- html_table(tbls[5], fill = TRUE)
tbl3 <- html_table(tbls[7], fill = TRUE)
tbl4 <- html_table(tbls[8], fill = TRUE)
tbl5 <- html_table(tbls[10], fill = TRUE)
tbl6 <- html_table(tbls[11], fill = TRUE)
tbl7 <- html_table(tbls[13], fill = TRUE)
tbl8 <- html_table(tbls[14], fill = TRUE)

#Create dataframes for each
df1 <- data.frame(tbl1)
df2 <- data.frame(tbl2)
df3 <- data.frame(tbl3)
df4 <- data.frame(tbl4)
df5 <- data.frame(tbl5)
df6 <- data.frame(tbl6)
df7 <- data.frame(tbl7)
df8 <- data.frame(tbl8)


# 3B:
# Remove unnecessary columns, first row, rename columns and add HDI Category column
#DF1
df1 <- df1[,-c(1, 2, 5)]
df1 <- df1[-c(1),]
colnames(df1) <- c("Country", "HDI")
df1$Category <- "Very High Human Development"

#DF2
df2 <- df2[,-c(1, 2, 5)]
df2 <- df2[-c(1),]
colnames(df2) <- c("Country", "HDI")
df2$Category <- "Very High Human Development"

#DF3
df3 <- df3[,-c(1, 2, 5)]
df3 <- df3[-c(1),]
colnames(df3) <- c("Country", "HDI")
df3$Category <- "High Human Development"

#DF4
df4 <- df4[,-c(1, 2, 5)]
df4 <- df4[-c(1),]
colnames(df4) <- c("Country", "HDI")
df4$Category <- "High Human Development"

#DF5
df5 <- df5[,-c(1, 2, 5)]
df5 <- df5[-c(1),]
colnames(df5) <- c("Country", "HDI")
df5$Category <- "Medium Human Development"

#DF6
df6 <- df6[,-c(1, 2, 5)]
df6 <- df6[-c(1),]
colnames(df6) <- c("Country", "HDI")
df6$Category <- "Medium Human Development"


#DF7
df7 <- df7[,-c(1, 2, 5)]
df7 <- df7[-c(1),]
colnames(df7) <- c("Country", "HDI")
df7$Category <- "Low Human Development"


#DF6
df8 <- df8[,-c(1, 2, 5)]
df8 <- df8[-c(1),]
colnames(df8) <- c("Country", "HDI")
df8$Category <- "Low Human Development"

# Bind them all together into 1 df
dfHDI <- rbind(df1, df2, df3, df4, df5, df6, df7, df8)

dim(dfHDI)

#Output new table to csv file

getwd()
setwd("C:\\SMU_INTRO_TO_DS\\CASESTUDY2")

#write.csv(dfHDI, "dfHDI.csv")

# 3C:
# Merge data into 1 dataset
mergedData <- merge(procrast, dfHDI, by.x=c("Country"),
										by.y=c("Country"), all= TRUE)


# 236 countries in procrast dataset have NA as the country.  MergedData has 3996 observations
# and procrast dataset has 4264 observations.  4264-236 = 4028 so still 29 other rows that did not merge.

# 4:

# 4A:
# Remove records from mergedData where age < 18 
mdSub1 <- subset(mergedData, mergedData$Age > 18)

#Check ages to make sure no crazy outliers and that min age is > 18
summary(mdSub1$Age)


# 4B:

#install.packages("stargazer")
library("stargazer")

mDF <- data.frame(mdSub1)

mDF$HDI <- as.numeric(mDF$HDI, digits = 4)

cols <- c('Age', 'AnnualIncome', 'HDI', 'DPMean', 'AIPMean', 'GPMean', 'SWLSMean')
stargazer(
	mDF[, cols], type = "text", 
	summary.stat = c("min", "p25", "median", "p75", "max", "mean", "sd")
)

# Histogram of Age
hist(mDF$Age, 
		 main="Histogram of Age", 
		 xlab="Age", 
		 border="blue", 
		 col="green",
		 las=1)


# Histogram of Annual Income
hist(mDF$AnnualIncome, 
		 main="Histogram of Annual Income", 
		 xlab="Annual Income", 
		 border="blue", 
		 col="red",
		 las=1)

# Neither the Age nor Annual Income histograms are particularly normally distributed.  The Age histogram is a bit of a bimodal
# distribution with peaks around the ages of 25, 45 and 55. THe Annual Income is more right-skewed.

# 4C:

# Frequencies per column 
# Gender
apply(mDF["Gender"], 2, table)

# WorkStatus
apply(mDF["WorkStatus"], 2, table)

# Occupation
apply(mDF["Occupation"], 2, table)


# 4D:

# Counts of participants per country in descending order

countryCt <- count(mDF, mDF$Country)
colnames(countryCt) <- c("Country", "Count")

sortedCC <- countryCt[order(countryCt$Count, decreasing = TRUE),]


# 4E:

# Find the number of people whose self view matched others view
viewMatch <- count(mDF[which(mDF$SelfView == mDF$OthersView),])
print(viewMatch)

# 5:

# 5B:

DPMeanSub <- mDF[,c("Country","DPMean", "Category")]
DPMeanAvg <- aggregate(DPMean~Country, DPMeanSub, mean)
DPMeanUnique <- aggregate(Category~Country, DPMeanSub, unique)

DPMeanAvgHDI <- merge(DPMeanAvg, DPMeanUnique , by.x=c("Country"),
											by.y=c("Country"))


SortedDP <- DPMeanAvgHDI[order(DPMeanAvgHDI$DPMean, decreasing = TRUE),]
Top15DP <- SortedDP[1:15,]


##Set chunk size for output
ggplot(Top15DP, aes(reorder(Country, -DPMean), DPMean))+
	geom_bar(aes(fill=Category), stat="identity")+
	ggtitle("Countries with the top 15 DPMean") +
	theme(plot.title = element_text(hjust = 0.5)) +
	xlab("Country Name")+
	ylab("DP Mean Value")


# 5C:
AIPMeanSub <- mDF[,c("Country","AIPMean", "Category")]
AIPMeanAvg <- aggregate(AIPMean~Country, AIPMeanSub, mean)
AIPMeanUnique <- aggregate(Category~Country, AIPMeanSub, unique)

AIPMeanAvgHDI <- merge(AIPMeanAvg, AIPMeanUnique , by.x=c("Country"),
											by.y=c("Country"))


SortedAIP <- AIPMeanAvgHDI[order(AIPMeanAvgHDI$AIPMean, decreasing = TRUE),]
Top15AIP <- SortedAIP[1:15,]


##Set chunk size for output
ggplot(Top15AIP, aes(reorder(Country, -AIPMean), AIPMean))+
	geom_bar(aes(fill=Category), stat="identity")+
	ggtitle("Countries with the top 15 AIPMean") +
	theme(plot.title = element_text(hjust = 0.5)) +
	xlab("Country Name")+
	ylab("AIP Mean Value")

#Find countries that exist in both the top 15 AIP and DP Means
merge(Top15DP, Top15AIP, by.x=c("Country"), by.y=c("Country"))

# There are 6 countries that exist in both DP and API top 15 Means.  They are:
# Dominican Republic, Ecuador, Panama, Qatar, Sri Lanka, Uraguay


# 5D:

# Subset merged data frame to only include those rows where Gender is not empty and Annual Income is not empty
mDFMF <- subset(mDF[c("Country","Age","AnnualIncome","Gender")], Gender != "" & AnnualIncome != "")

# Get the average income per age
mDFMFAvg <- aggregate(AnnualIncome~Age + Gender, mDFMF, mean)


par(mar=c(4,6,2,2))
plot(mDFMFAvg$AnnualIncome ~ mDFMFAvg$Age, xaxt="n", yaxt="n", pch = 16, cex = 1.3, col = ifelse(mDFMFAvg$Gender == "Female", "hot pink", "blue"), xlab="", ylab="")
axis(1,at=pretty(mDFMFAvg$Age),labels=pretty(mDFMFAvg$Age),las=1)
axis(2,at=pretty(mDFMFAvg$AnnualIncome),labels=format(pretty(mDFMFAvg$AnnualIncome),big.mark=",", scientific=FALSE),las=1)
mtext(text="Age", side=1, line=2)
mtext(text="Annual Income", side=2, line=5)
title("Mean Income by Age and Gender")
abline(lm(mDFMFAvg$AnnualIncome ~ mDFMFAvg$Age), col = "green")

# There does seem to be a relationship between age and annual income, with annual income increasing with age.  However,
# there also seems to be a large disparity between males and females and their annual income starting at around age 40 and 
# growing from there.

# 5E: 

# Subset merged data frame to only include those rows where Gender is not empty
mDFHDI <- subset(mDF[c("Country","SWLSMean", "HDI", "Category")])

mDFHDIAvg <- aggregate(SWLSMean~HDI + Category, mDFHDI, mean)


par(mar=c(4,6,2,2))
plot(mDFHDIAvg$SWLSMean ~ mDFHDIAvg$HDI, xaxt="n", yaxt="n", pch = 16, cex = 1.3, col = "blue", xlab="", ylab="")
axis(1,at=pretty(mDFHDIAvg$HDI),labels=pretty(mDFHDIAvg$HDI),las=1)
axis(2,at=pretty(mDFHDIAvg$SWLSMean),labels=format(pretty(mDFHDIAvg$SWLSMean),big.mark=",", scientific=FALSE),las=1)
mtext(text="HDI", side=1, line=2)
mtext(text="Mean Life Satisfaction", side=2, line=5)
title("Life Satisfaction Mean by HDI")
abline(lm(mDFHDIAvg$SWLSMean ~ mDFHDIAvg$HDI), col = "green")


# Barchart between HDI Category and Mean Life Satisfaction

ggplot(mDFHDIAvg, aes(fill=Category, y=SWLSMean, x=Category)) + 
	scale_x_discrete(labels=c("High Human Development" = "HHD", "Very High Human Development" = "VHHD",
																"Medium Human Development" = "MHD", "Low Human Development" = "LHD")) +
	geom_bar(position="dodge", stat="identity") +
	ggtitle("Mean Life Satisfaction per HDI Category Test") +
	theme(plot.title = element_text(hjust = 0.5)) +
	xlab("Human Development Category") +
	ylab("Life Satisfaction Mean")


# 6

# A-C

sink('CaseStudy2_OutputQuestion6.csv')

# Write the first dataframe, with a title and final line separator 
cat('Finalized HDI table (from 3A and 3B)')
cat('\n')
write.csv(dfHDI)
cat('____________________________')

cat('\n')
cat('\n')

# Write the 2nd dataframe to the same sink
cat('Merged Dataframe between Procrastination and HDI Data from Wikipedia (from 3C)')
cat('\n')
write.csv(mergedData)
cat('____________________________')


cat('\n')
cat('\n')

# Write the 3rd dataframe to the same sink
cat('Top 15 DP Sorted Countries and their HDI')
cat('\n')
write.csv(Top15DP)
cat('____________________________')

cat('\n')
cat('\n')

# Write the 3rd dataframe to the same sink
cat('Top 15 API Sorted Countries and their HDI')
cat('\n')
write.csv(Top15AIP)
cat('____________________________')

# Close the sink
sink()