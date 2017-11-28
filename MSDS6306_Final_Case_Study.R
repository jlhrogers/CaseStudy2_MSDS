data_dir = 'C:/Users/Jose/Downloads/Case_Study_2/'
data_file = 'Procrastination.csv'
setwd(data_dir)
dataset_raw = read.csv(data_file)

#Include custom function library
source('MSDS6306_Final_Case_Study_Functions.R')

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

resp_answers$DP_Avg = round(apply(X=resp_answers[,2:6],MARGIN=1,FUN=mean),5)
resp_answers$AIP_Avg = round(apply(X=resp_answers[,7:21],MARGIN=1,FUN=mean),5)
resp_answers$GP_Avg = round(apply(X=resp_answers[,22:41],MARGIN=1,FUN=mean),5)
resp_answers$SWLS_Avg = round(apply(X=resp_answers[,42:46],MARGIN=1,FUN=mean),5)


#Convert blank or space to NA in the entire data set
resp_attr = gsubkeep(pattern='^$|^ $',replacement=NA,x=resp_attr)


#Attributes cleanup
#Replace 1st argument with the 2nd argument in column = 3rd argument
cleanup_list = data.frame(rbind(c(' Kids$','',4), #remove 'Kids'
                          c('0', NA, 6), #Work status
                          c('0|please specify',NA,8), #Occupation 
                          c('999',NA,9), #Employment Years
                          c('[08]',NA,11), #Community Size
                          c('0',NA,12), #Country of Residence
                          c('0',NA,13), #Married
                          c('Male','1',14), #Number of sons
                          c('Female','2',14))) #Number of sons

#Continuing attributes cleanup...
for (i in 1:dim(cleanup_list)[1]) {
  pattern = as.character(cleanup_list[i,1])
  replacement = as.character(cleanup_list[i,2])
  column = as.integer(as.character(cleanup_list[i,3]))
  resp_attr[,column] = gsubkeep(pattern=pattern,replacement=replacement,x=resp_attr[,column])
}


#$ Employ_Yrs_num - replace small numbers with 0.  Round to nearest integer.
resp_attr[which(resp_attr[,9] < .5),9] = 0
resp_attr[,9] = as.integer(round(resp_attr[,9],digits=0))

#$ Num_sons_fac - Convert to numeric
resp_attr[,14] = as.integer(resp_attr[,14])

#$ Num_daughters_num - no change


###Scraping tasks###
# ? - Get a single table that has columns Country, 2016 est for 2015, and development category
#       using the complete list of countries at the top
# ? - Output a cSV of this table to the repo
# ? - Merge the HDI table with the resp_attr table

library(rvest)
library(dplyr)
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


###Prelim Analysis Tasks###
# ? - Remove respondents under 18 from resp_attr and resp_answers
row_keep = which(resp_attrwHDI$Age>=18)
resp_attrwHDI = resp_attrwHDI[row_keep,]
resp_answers = resp_answers[row_keep,]

#Other outliers to remove?

# ? - Tabulated descriptive statistics on Age, Income, HDI, and four mean columns (DP, etc.)
# ? - Histogram for only 2 of the seven variables; comment on the shape
# ? - Give frequencies for Gender, Work Status, and occupation in a separate table
# ? - Counts of participants per country, in desc order
# ? - Contingency table of procrastination indicators

library(stargazer)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(stringr)

#Merge attributes with question answers for analysis
resp_full = merge(x=resp_answers,y=resp_attrwHDI,by.x='ID',by.y='ID',all.y=TRUE)

#statistics table
stargazer(resp_full[,c('Age','Annual_Inc','HDI','DP_Avg','AIP_Avg','GP_Avg','SWLS_Avg')],type='text',
          summary.stat=c('n','min','p25','median','p75','max','mean','sd'))

#Create 4 plots for sample figure:

h1 = gghistogram(resp_full,x='Age',y='..count..',
                 bins=10,
                 fill='blue',
                 size=2,
                 title='Age',
                 color='white'
                 )

h1 + font('x.text',size=8)

h2 = gghistogram(resp_full,x='Annual_Inc',y='..count..',
                 bins=10,
                 fill='blueviolet',
                 size=2,
                 title='Ann Inc',
                 color='white'
                 )

h2 + font('x.text',size=10)

h3 = gghistogram(resp_full,x='HDI',y='..count..',
                 bins=5,
                 fill='cadetblue3',
                 size=2,
                 title='HDI',
                 color='white'
)

h4 = ggboxplot(metrics,x='variable',
               y='value',
               color='variable',
               palette='jco',
               size=1.5
)

h4 + font('x.text',size=10)

ggarrange(ggarrange(h1,h2,h3,ncol=3,labels=c('1','2','3')),
          h4,
          nrow=2,
          labels='4')

#Categorize occupations for table

jobs = cbind(as.character(resp_full$Current_Occ),NA)
jobs_matrix = unlist(apply(jobs,MARGIN=2,FUN=function(x){strsplit(x,' ')})) %>%
              gsub(pattern='[[:punct:]]',replacement='') %>%
              gsub(pattern='^$|^ $',replacement=NA) %>%
              subset(!is.na(jobs_matrix))
jobs_matrix = as.factor(tolower(jobs_matrix))
jobs_count = as.data.frame(table(jobs_matrix))
colnames(jobs_count)=c('title','count')
jobs_count = jobs_count[order(jobs_count$count,jobs_count$title,decreasing=TRUE),]

#Get top 10 job keywords
keywords = jobs_count[1:10,]

keywords

keywords_del = c('and','of')
keywords = subset(keywords,!keywords$title %in% keywords_del)

#Get a list of all occupations that match the keywords job list
top_jobs = match_list(resp_full$Current_Occ,findtext=keywords$title)
unique(top_jobs[[1]][,2])


#Skipping count/frequency tasks because still researching how to make them pretty lol

#Sunday
###Deeper Analysis and Visualization###
# ? - Bar chart that displays nations with top 15 average procrastination scores, using -either- DP, AIP, or GP
#       desc order on bars, color bars by HDI category using non-default color palette
# ? - Bar chart that displays nations with top 15 average procrastination scores, using a score -not- used in prior question
# ? - Which countries show up in both bar charts
# ? - Create scatterplot Income vs Age, color points based on gender.  Optional to use lm()
# ? - Create scatterplot Life Satisfaction vs HDI
# ? - Create bar chart of Life Satisfaction vs HDI category

###Final output###
#1. Finalized HDI table
#2. Finalized data set, including merged HDI table
#3. Data set with top 15 nations from 5B/5C, including their scores 


#Packages
#stargazer - statistical summary
#scatterplot