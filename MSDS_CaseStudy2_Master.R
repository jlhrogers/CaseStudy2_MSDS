# Install packages and libraries

#install.packages("dyplr")
#install.packages("rvest")
#install.packages("xml2")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("curl")
#install.packages("stargazer")
#install.packages("stringi")
#install.packages("scales")


#Get libraries
#library("dplyr")
#library("rvest")
#library("tidyr")
#library("ggplot2")
#library("stargazer")
#library("ggpubr")
#library("reshape2")
#library("stringr")
#library("scales")

#Include custom function library
source('MSDS6306_Final_Case_Study_Functions.R')

# 2: Read in and clean raw data

# 2A: Read in CSV
filepath = 'C:\\Users\\Jose\\Downloads\\Case_Study_2\\CaseStudy2_MSDS'
dataset_raw <- read.csv(paste(filepath,'Procrastination.csv',sep='\\'))

dim(dataset_raw)
# There are 4264 rows and 61 columns

# 2B: Partition Procrastination into 2 tables (Attributes and Responses/Answers) and update column names
ID = 1:dim(dataset_raw)[1]
resp_attr = cbind(ID,dataset_raw[,1:14])
resp_answers = cbind(ID,dataset_raw[,15:length(dataset_raw)])

resp_attr_names = c('ID','Age','Gender','Kids','Edu','Work_Status',
										'Annual_Inc','Current_Occ','Employ_Yrs','Employ_Mths',
										'Comm_Size','Country_Res','Married','Num_sons',
										'Num_daught')

resp_answer_names = c('ID',paste('DP_',1:5,sep=''),paste('AIP_',1:15,sep=''),
											paste('GP_',1:20,sep=''),paste('SWLS_',1:5,sep=''),'Self_Score','Others_Score')

colnames(resp_attr) = resp_attr_names
colnames(resp_answers) = resp_answer_names


# 2C: Cleanup the data

# 2Ci through 2civ:

# Begin by converting all blank or empty strings to NA in the entire data set
resp_attr = gsubkeep(pattern='^$|^ $',replacement=NA,x=resp_attr)


# Now go through individual attributes list and replace 'bad' values with NA
cleanup_list = data.frame(rbind(c(' Kids$','',4), #remove 'Kids'
																c('0', NA, 6), #Work status
																c('0|please specify',NA,8), #Occupation 
																c('999',NA,9), #Employment Years
																c('[08]',NA,11), #Community Size
																c('0',NA,12), #Country of Residence
																c('0',NA,13), #Married
																c('Male','1',14), #Number of sons
																c('Female','2',14))) #Number of sons

# Employ_Yrs_num - replace small numbers with 0.  Round to nearest integer.
resp_attr[which(resp_attr[,9] < .5),9] = 0
resp_attr[,9] = as.integer(round(resp_attr[,9],digits=0))

# Num_sons_fac - Convert to numeric
resp_attr[,14] = as.integer(resp_attr[,14])

# Num_daughters_num - no change

# 2d:
# Set columns to the proper data types
for (i in 1:dim(cleanup_list)[1]) {
	pattern = as.character(cleanup_list[i,1])
	replacement = as.character(cleanup_list[i,2])
	column = as.integer(as.character(cleanup_list[i,3]))
	resp_attr[,column] = gsubkeep(pattern=pattern,replacement=replacement,x=resp_attr[,column])
}


# 2e:
# Assign mean columns for the 4 response categories
resp_answers$DP_Avg = round(apply(X=resp_answers[,2:6],MARGIN=1,FUN=mean),5)
resp_answers$AIP_Avg = round(apply(X=resp_answers[,7:21],MARGIN=1,FUN=mean),5)
resp_answers$GP_Avg = round(apply(X=resp_answers[,22:41],MARGIN=1,FUN=mean),5)
resp_answers$SWLS_Avg = round(apply(X=resp_answers[,42:46],MARGIN=1,FUN=mean),5)


# 3: HDI table scrape from Wikipedia

# Scrape the Wikipedia website for the 8 HDI data tables

URL = 'https://en.wikipedia.org/wiki/List_of_countries_by_Human_Development_Index#Complete_list_of_countries'

html_resp = read_html(URL)

#HDI_data is a single data frame country and HDI
HDI_data = 
	html_nodes(html_resp,css='table.wikitable') %>%
	html_table(fill=TRUE) %>%
	list_rowbind(elements=c(1:8),cols=c(3,4),colname=c('Country','HDI'))

#Cleaning up HDI data and adding categories
HDI_data = subset(HDI_data,HDI_data[,1] != 'Change in rank from previous year[1]')
rownames(HDI_data) = 1:dim(HDI_data)[1]
HDI_data$HDI_Cat = c(rep('Very High',51),rep('High',56),rep('Medium',41),rep('Low',41))
HDI_data$HDI = as.numeric(HDI_data$HDI)

#Attach HDI data to attribute table, and maintain original row ordering
resp_attrwHDI = merge(x=resp_attr,y=HDI_data,by.x='Country_Res',by.y='Country',all.x=TRUE,incomparables=NA)
resp_attrwHDI = resp_attrwHDI[order(resp_attrwHDI$ID),]


# 4: Preliminary Analysis

# 4A:
# Remove respondents under 18 from resp_attr and resp_answers
row_keep = which(resp_attrwHDI$Age>=18)
resp_attrwHDI = resp_attrwHDI[row_keep,]
resp_answers = resp_answers[row_keep,]


#Merge attributes with question answers for analysis
resp_full = merge(x=resp_answers,y=resp_attrwHDI,by.x='ID',by.y='ID',all.y=TRUE)


# 4B:
#statistics table
stargazer(resp_full[,c('Age','Annual_Inc','HDI','DP_Avg','AIP_Avg','GP_Avg','SWLS_Avg')],type='text',
					summary.stat=c('n','min','p25','median','p75','max','mean','sd'))


#Create 4 plots for sample figure:

h1 = gghistogram(resp_full,x='Age',y='..count..',
								 bins=10,
								 fill='blue',
								 size=2,
								 title='Histogram of Age',
								 color='white',
								 ylab='Count'
)

h1 = h1 + font('xy.text',size=12) + theme(plot.title=element_text(hjust=0.5))


h2 = gghistogram(resp_full,x='Annual_Inc',y='..count..',
								 fill='blueviolet',
								 size=2,
								 bins=25,
								 title='Histogram of Annual Income',
								 color='white',
								 ylab='Count',
								 xlab='Annual Income'
)

Annual_Inc_Clean = resp_full[which(!is.na(resp_full$Annual_Inc)),'Annual_Inc']

h2 = h2 + font('xy.text',size=12) + 
  theme(plot.title=element_text(hjust=0.5)) +
  rotate_x_text(30) +
  scale_x_continuous(label=dollar_format(),breaks = round(seq(min(Annual_Inc_Clean), max(Annual_Inc_Clean), by = 50000),1))

h3 = gghistogram(resp_full,x='HDI',y='..count..',
								 bins=35,
								 fill='cadetblue3',
								 size=2,
								 title='Histogram of HDI',
								 color='white',
								 ylab='Count'
)

h3 = h3 + font('xy.text',size=12) + theme(plot.title=element_text(hjust=0.5))

DPMetric <- resp_full["DP_Avg"]
DPMetric$Variable <- 'DP_Avg'
colnames(DPMetric) <- c("Value","Variable")

AIPMetric <- resp_full["AIP_Avg"]
AIPMetric$Variable <- 'AIP_Avg'
colnames(AIPMetric) <- c("Value","Variable")

GPMetric <- resp_full["GP_Avg"]
GPMetric$Variable <- 'GP_Avg'
colnames(GPMetric) <- c("Value","Variable")

SWLSMetric <- resp_full["SWLS_Avg"]
SWLSMetric$Variable <- 'SWLS_Avg'
colnames(SWLSMetric) <- c("Value","Variable")

metrics <- rbind(DPMetric, AIPMetric, GPMetric, SWLSMetric)

h4 = ggboxplot(metrics,x='Variable',
							 y='Value',
							 color='Variable',
							 palette='jco',
							 size=1,
							 xlab='')


h4 = h4 + font('xy.text',size=12) + theme(plot.title=element_text(hjust=0.5)) +
  rotate_x_text(30)

ggarrange(h1,h3,
					h2,h4,
					nrow=2,
					ncol=2)

# Neither the Age nor Annual Income histograms are particularly normally distributed.  The Age histogram is a bit of a bimodal
# distribution with peaks around the ages of 25, 45 and 55. THe Annual Income is more right-skewed.

# 4C:


#Categorize occupations for table

jobs = cbind(as.character(resp_full$Current_Occ), NA)

jobs_matrix = unlist(apply(jobs,MARGIN=2,FUN=function(x){strsplit(x,' ')})) %>%
	gsub(pattern='[[:punct:]]',replacement='') %>%
	gsub(pattern='^$|^ $',replacement=NA) 

jobs_matrix = as.factor(tolower(jobs_matrix))

jobs_count = as.data.frame(table(jobs_matrix))
colnames(jobs_count)=c('title','count')
jobs_count = jobs_count[order(jobs_count$count,jobs_count$title,decreasing=TRUE),]

#Get top 10 job keywords
keywords = jobs_count[1:10,]

keywords_del = c('and','of')
top10Jobs = subset(keywords,!keywords$title %in% keywords_del)

#For the codebook only
#These are the job assignments we subjectively made to get the top10 job counts shown:
top_jobs = match_list(resp_full$Current_Occ,findtext=keywords$title)

for (i in 1:length(top10Jobs$title)) {
print(paste(tolower(as.character(top10Jobs$title[i])),'<-', unique(unique(top_jobs[[i]][,2]))))
}

# Frequencies per column 
# Unique distributions
# Gender
apply(resp_full["Gender"], 2, table)

# WorkStatus
apply(resp_full["Work_Status"], 2, table)


#Distributions across Gender and Work Status
with(resp_full, table(Gender, Work_Status))

top10Jobs

# 4D:

# Counts of participants per country in descending order

countryCt <- count(resp_full, resp_full$Country_Res)
colnames(countryCt) <- c("Country", "Count")

sortedCC <- countryCt[order(countryCt$Count, decreasing = TRUE),]

print(sortedCC)

# 4E:

# Find the number of people whose self view matched others view

resp_full$Others_Score <- as.character(resp_full$Others_Score)
resp_full$Self_Score <- as.character(resp_full$Self_Score)

viewMatch <- count(resp_full[which(resp_full$Self_Score == resp_full$Others_Score),])
print(viewMatch)

viewMatch <- count(resp_full[which(resp_full$Self_Score == 'no' & resp_full$Others_Score == 'no'),])
print(viewMatch)

viewMatch <- count(resp_full[which(resp_full$Self_Score == 'yes' & resp_full$Others_Score == 'yes'),])
print(viewMatch)

# 5: Visualization and Deeper Analysis

# 5B:

DPMeanSub <- resp_full[,c("Country_Res","DP_Avg", "HDI_Cat")]
DPMeanAvg <- aggregate(DP_Avg~Country_Res, DPMeanSub, mean)
DPMeanUnique <- aggregate(HDI_Cat~Country_Res, DPMeanSub, unique)

DPMeanAvgHDI <- merge(DPMeanAvg, DPMeanUnique , by.x=c("Country_Res"),
											by.y=c("Country_Res"))


SortedDP <- DPMeanAvgHDI[order(DPMeanAvgHDI$DP_Avg, decreasing = TRUE),]
Top15DP <- SortedDP[1:15,]


##Set chunk size for output
ggplot(Top15DP, aes(reorder(Country_Res, -DP_Avg), DP_Avg))+
	geom_bar(aes(fill=HDI_Cat), stat="identity")+
	ggtitle("Countries with the top 15 DPMean") +
	theme(plot.title = element_text(hjust = 0.5)) +
	xlab("Country Name")+
	ylab("DP Mean Value")

####START HERE:

# 5C:
AIPMeanSub <- resp_full[,c("Country_Res","AIP_Avg", "HDI_Cat")]
AIPMeanAvg <- aggregate(AIP_Avg~Country_Res, AIPMeanSub, mean)
AIPMeanUnique <- aggregate(HDI_Cat~Country_Res, AIPMeanSub, unique)

AIPMeanAvgHDI <- merge(AIPMeanAvg, AIPMeanUnique , by.x=c("Country_Res"),
											 by.y=c("Country_Res"))


SortedAIP <- AIPMeanAvgHDI[order(AIPMeanAvgHDI$AIP_Avg, decreasing = TRUE),]
Top15AIP <- SortedAIP[1:15,]


##Set chunk size for output
ggplot(Top15AIP, aes(reorder(Country_Res, -AIP_Avg), AIP_Avg))+
	geom_bar(aes(fill=HDI_Cat), stat="identity")+
	ggtitle("Countries with the top 15 AIPMean") +
	theme(plot.title = element_text(hjust = 0.5)) +
	xlab("Country Name")+
	ylab("AIP Mean Value")

#Find countries that exist in both the top 15 AIP and DP Means
merge(Top15DP, Top15AIP, by.x=c("Country_Res"), by.y=c("Country_Res"))

# There are 6 countries that exist in both DP and API top 15 Means.  They are:
# Dominican Republic, Ecuador, Panama, Qatar, Sri Lanka, Uraguay


# 5D:

# Subset merged data frame to only include those rows where Gender is not empty and Annual Income is not empty
mDFMF <- subset(resp_full[c("Country_Res","Age","Annual_Inc","Gender")], Gender != "" & Annual_Inc != "")

# Get the average income per age
mDFMFAvg <- aggregate(Annual_Inc~Age + Gender, mDFMF, mean)


par(mar=c(4,6,2,2))
plot(mDFMFAvg$Annual_Inc ~ mDFMFAvg$Age, xaxt="n", yaxt="n", pch = 16, cex = 1.3, col = ifelse(mDFMFAvg$Gender == "Female", "hot pink", "blue"), xlab="", ylab="")
axis(1,at=pretty(mDFMFAvg$Age),labels=pretty(mDFMFAvg$Age),las=1)
axis(2,at=pretty(mDFMFAvg$Annual_Inc),labels=format(pretty(mDFMFAvg$Annual_Inc),big.mark=",", scientific=FALSE),las=1)
mtext(text="Age", side=1, line=2)
mtext(text="Annual Income", side=2, line=5)
title("Mean Income by Age and Gender")
abline(lm(mDFMFAvg$Annual_Inc ~ mDFMFAvg$Age), col = "green")

# There does seem to be a relationship between age and annual income, with annual income increasing with age.  However,
# there also seems to be a large disparity between males and females and their annual income starting at around age 40 and 
# growing from there.

# 5E: 

# Subset merged data frame to only include those rows where Gender is not empty
mDFHDI <- subset(resp_full[c("Country_Res","SWLS_Avg", "HDI", "HDI_Cat")])

mDFHDIAvg <- aggregate(SWLS_Avg~HDI + HDI_Cat, mDFHDI, mean)


par(mar=c(4,6,2,2))
plot(mDFHDIAvg$SWLS_Avg ~ mDFHDIAvg$HDI, xaxt="n", yaxt="n", pch = 16, cex = 1.3, col = "blue", xlab="", ylab="")
axis(1,at=pretty(mDFHDIAvg$HDI),labels=pretty(mDFHDIAvg$HDI),las=1)
axis(2,at=pretty(mDFHDIAvg$SWLS_Avg),labels=format(pretty(mDFHDIAvg$SWLS_Avg),big.mark=",", scientific=FALSE),las=1)
mtext(text="HDI", side=1, line=2)
mtext(text="Mean Life Satisfaction", side=2, line=5)
title("Life Satisfaction Mean by HDI")
abline(lm(mDFHDIAvg$SWLS_Avg ~ mDFHDIAvg$HDI), col = "green")


# Barchart between HDI Category and Mean Life Satisfaction

ggplot(mDFHDIAvg, aes(fill=HDI_Cat, y=SWLS_Avg, x=HDI_Cat)) + 
	scale_x_discrete(labels=c("High Human Development" = "HHD", "Very High Human Development" = "VHHD",
														"Medium Human Development" = "MHD", "Low Human Development" = "LHD")) +
	geom_bar(position="dodge", stat="identity") +
	ggtitle("Mean Life Satisfaction per HDI Category Test") +
	theme(plot.title = element_text(hjust = 0.5)) +
	xlab("Human Development Category") +
	ylab("Life Satisfaction Mean")





