
# Codebook for Procrastination Tendencies of Company Corp. Customers

<br>

### Introduction

The following datasets represent a data sample collected via survey (Procrastination dataset) and a dataset scraped from Wikipedia (HDI Data) for the purposes of understanding and correlating individuals procrastination choices and views by their country of residence, with the Human Development Index factor for that country. 

This data folder contains the following files:

* Procrastination.csv - a data frame containing several attributes from 4,264 individuals surveyed from from various countries, 
	and includes individual responses to difference scales of procrastination

* HDI.csv - data scraped from Wikipedia that lists, by country, the Human Development Index 2016 estimates for 2015. 	  	
  (https://en.wikipedia.org/wiki/List_of_countries_by_Human_Development_Index#Complete_list_of_countries)  
	 

<br>    

#### Procrastination.csv:

Type: Comma-separated value file  
Dimensions: 4264 rows, 61 columns  
Unit Used: MSDS Case Study 2  
Public: Yes  
Variable Information: Individual and response variables  


Summary: A data collection survey of 4264 individuals including various personal qualities and individual responses to scales of procrastination:
(AIP: Adult Inventory of Procrastination (McCown & Johnson, 1989), DP: Decisional Procrastination (Mann, 1982), 
SWLS: Satisfaction with Life Scale (Diener et al., 1985), GP: General Procrastination (Lay, 1986)). 
Details of the scales are found below.  

<br>

#### Procrastination scales background:  


_**AIP (Adult Inventory of Procrastination):**_

"The AIP scale measures the chronic tendency to postpone tasks in various situations (see Ferrari et al., 1995, for the complete list of items). It examines procrastination motivated by fears (e.g., success or failure), avoidance of disclosure of skill inabilities, and performance insecurity (Ferrari, 1991). The AIP assesses avoidance procrastination; that is, putting off tasks to protect one's self-esteem from possible failure. The AIP is composed of 15 Likert-scale items such that respondents express an opinion on a 5-point scale (1 = strongly disagree; 5 = strongly agree) to statements such as "I am not very good at meeting deadlines" and "I don't get things done on time." For seven items, scores are reversed so that high ratings indicate procrastination."

*(reference: Adult Inventory of Procrastination Scale (AIP): A comparison of models with an Italian sample. Available from:*
https://www.researchgate.net/publication/279532373_Adult_Inventory_of_Procrastination_Scale_AIP_A_comparison_of_models_with_an_Italian_sample)

* 1 = strongly disagree to 5 = strongly agree


_**DP (Decisional Procrastination):**_

Developed by Mann to give a reliable and valid measure of indecision.

Represented by a 5-point scale with the following scoring: 

* 1 = never to 5 = always


_**SWLS (Satisfaction with Life Scale):**_

"A five item scale designed to measure global cognitive judgments of one's life satisfaction
(not a measure of either positive or negative affect). Participants indicate how much they agree or disagree with each of the 5 items using a 7-point scale that ranges from 7 strongly agree to 1 strongly disagree.""

*(reference:*
https://www.researchgate.net/file.PostFileLoader.html?id=5754d4d393553b616d2244e3&assetKey=AS%3A369794550386690%401465177299568)

* 7 - Strongly agree 
* 6 - Agree
* 5 - Slightly agree
* 4 - Neither agree nor disagree
* 3 - Slightly disagree
* 2 - Disagree
* 1 - Strongly disagree

_For this analysis, a scale of 1 through 5 was taken instead of 1 through 7._

_**GP (General Procrastination):**_

"The GPS (General Procrastination Scale) was a 20-item scale consisting of a reliability of .82 according to Cronbach's alpha coefficient." 

*(reference: https://www.researchgate.net/profile/Shannon_Scielzo/publication/273259879_Finally_The_Development_and_Validation_of_the_Academic_Procrastination_Scale/links/54fcfb3d0cf20700c5e9c735/Finally-The-Development-and-Validation-of-the-Academic-Procrastination-Scale.pdf)*

* 1 point = extremely uncharacteristic.
* 2 points = moderately uncharacteristic.
* 3 points = neutral.
* 4 points = moderately characteristic.
* 5 points = extremely characteristic.


#### Procrastination.csv variables:

* Age: The participant's age in years
* Gender: The gender the participant identifes as (Male or Female)
* Kids: Binary, whether or not they have kids
* Edu: Education Level
* Work Status: What kind of job are they working
* Annual Income: All converted to dollars
* Current Occupation: A write-in for occupation
* How long have you held this position: Years: Number of years in this job
* How long have you held this position: Months: Number of months in this job
* Community: Size of community
* Country of Residence: The country where the person holds citizenship
* Marital Status: Single, Married, Divorced, Separated, etc.
* Number of sons/Number of daughters: integer number of children
* DP variables: (All numeric ratings between 1 and 5)
	+ (DP 1) I waste a lot of time on trivial matters before getting to the final decisions
	+ (DP 2) Even after I make a decision I delay acting upon it
	+ (DP 3) I don't make decisions unless I really have to
	+ (DP 4) I delay making decisions until it's too late
	+ (DP 5) I put off making decisions until it's too late
* AIP variables: (All numeric ratings between 1 and 5)	
	+ (AIP 1) I pay my bills on time
	+ (AIP 2) I am prompt and on time for most appointments.	+ (AIP 3) I lay out my clothes the night before I have an important 			appointment, so I won't be late
	+ (AIP 4) I find myself running later than I would like to be
	+ (AIP 5) I don't get things done on time
	+ (AIP 6) If someone were teaching a course on how to get things done on time, I would attend
	+ (AIP 7) My friends and family think I wait until the last minute.
	+ (AIP 8) I get important things done with time to spare
	+ (AIP 9) I am not very good at meeting deadlines
	+ (AIP 10) I find myself running out of time.
	+ (AIP 11) I schedule doctor's appointments when I am supposed to without delay
	+ (AIP 12) I am more punctual than most people I know
	+ (AIP 13) I do routine maintenance (e.g., changing the car oil) on things I own as often as I should
	+ (AIP 14) When I have to be somewhere at a certain time my friends expect me to run a bit late
	+ (AIP 15) Putting things off till the last minute has cost me money in the past
* GP Variables: (All numeric ratings between 1 and 5)
	+ (GP 1) I often find myself performing tasks that I had intended to do days before
	+ (GP 2) I often miss concerts, sporting events, or the like because I don't get around to buying tickets on time
	+ (GP 3) When planning a party, I make the necessary arrangements well in advance
	+ (GP 4) When it is time to get up in the morning, I most often get right out of bed
	+ (GP 5) A letter may sit for days after I write it before mailing it possible
	+ (GP 6) I generally return phone calls promptly
	+ (GP 7) Even jobs that require little else except sitting down and doing them, I find that they seldom get done for days
	+ (GP 8) I usually make decisions as soon as possible
	+ (GP 9) I generally delay before starting on work I have to do
	+ (GP 10) When traveling, I usually have to rush in preparing to arrive at the airport or station at the appropriate time
	+ (GP 11) When preparing to go out, I am seldom caught having to do something at the last minute
	+ (GP 12) In preparation for some deadlines, I often waste time by doing other things
	+ (GP 13) If a bill for a small amount comes, I pay it right away
	+ (GP 14) I usually return a "RSVP" request very shortly after receiving it
	+ (GP 15) I often have a task finished sooner than necessary
	+ (GP 16) I always seem to end up shopping for birthday gifts at the last minute
	+ (GP 17) I usually buy even an essential item at the last minute
	+ (GP 18) I usually accomplish all the things I plan to do in a day
	+ (GP 19) I am continually saying "I'll do it tomorrow"
	+ (GP 20) I usually take care of all the tasks I have to do before I settle down and relax for the evening
* SWLS Variables: (All numeric ratings between 0 and 5)	
	+ (SWLS 1) In most ways my life is close to my ideal
	+ (SWLS 2)The conditions of my life are excellent
	+ (SWLS 3) I am satisfied with my life
	+ (SWLS 4) So far I have gotten the important things I want in life
	+ (SWLS 5) If I could live my life over, I would change almost nothing
* Do you consider yourself a procrastinator: a binary response
* Do others consider you a procrastinator: a binary response

<br>

#### HDI.csv: 

Type: Comma-separated value file  
Dimensions: 189 rows, 2 columns  
Unit Used: MSDS Case Study 2  
Public: Yes  
Variable Information: Country and 2016 HDI estimates for 2015  

Summary: HDI Dataset: A list of all countries by the HDI (Human Development Index) as included in the United Nations Development Programme's Human Development Report. The data from this site was scraped and pushed into a CSV file, to later merge with the Procrastination dataset listed above, by country.
(ref: Wikipedia)

<br>

#### HDI.csv variables:

1. Country: Name of the country from the UN's Development Programme's Human Development Report
2. HDI: 2016 Human Development Index score estimate for 2015


<br>


	
