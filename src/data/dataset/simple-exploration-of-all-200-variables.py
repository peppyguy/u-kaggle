---
title: "Home Credit Default Risk"
author: "Ganesh N"
output:
  html_document:
    number_sections: true
    toc: true
    fig_width: 10
    code_folding: hide
    fig_height: 4.5
    theme: cosmo
    highlight: tango
---

<center><img src="https://upload.wikimedia.org/wikipedia/commons/7/7c/Home_credit_logo_640px.jpg"></center>     


#Introduction

Many people struggle to get loans due to insufficient or non-existent credit histories. And, unfortunately, this population is often taken advantage of by 
untrustworthy lenders. Home Credit strives to broaden financial inclusion for the unbanked population by providing a positive and safe borrowing experience.
In order to make sure thisunderserved population has a positive loan experience, Home Credit makes use of a variety of alternative data-
-including telco and transactional information--to predict their clients' repayment abilities.

While Home Credit is currently using various statistical and machine learning methods to make these predictions, 
they're challenging Kagglers to help them unlock the full potential of their data. Doing so will ensure that clients capable of 
repayment are not rejected and that loans are given with a principal, maturity, and repayment calendar that will empower their clients to be successful.


#Loading Libraries and Importing Data{.tabset .tabset-fade .tabset-pills}

##Loading Libraries

```{r,message=FALSE,warning=FALSE}

library(tidyverse)


library(plotly)
library(knitr)
library(ggthemes)
library(highcharter)
library(igraph)
library(ggraph)
library(qgraph)
library(visNetwork)

```

##Importing data

```{r,message=FALSE,warning=FALSE,echo=FALSE,results=FALSE}

rm(list=ls())

fillColor = "#FFA07A"
fillColor2 = "#F1C40F"
fillColorLightCoral = "#F08080"

application_train <- read_csv("../input/application_train.csv")
bureau <- read_csv("../input/bureau.csv")
bureau_balance <- read_csv("../input/bureau_balance.csv")
credit_card_balance <- read_csv("../input/credit_card_balance.csv")

```

#Exploring Data : Identifying Variables{.tabset .tabset-fade .tabset-pills}


##Bureau

```{r,message=FALSE,warning=FALSE}

glimpse(bureau)

```
##ApplicationTrain

```{r,message=FALSE,warning=FALSE}

glimpse(application_train)

```

##Bureau Balance

```{r,message=FALSE,warning=FALSE}

glimpse(bureau_balance)

```


##Credit Card Balance

```{r,message=FALSE,warning=FALSE}

glimpse(credit_card_balance)

```

#Understanding Missing Data Values


##Number of missing values in application_train and Missing values percentage of applicationtrain variables

```{r,message=FALSE,warning=FALSE}

sum(is.na(application_train))

missing_data <- as.data.frame(sort(sapply(application_train, function(x) sum(is.na(x))),decreasing = T))

missing_data <- (missing_data/nrow(application_train))*100

colnames(missing_data)[1] <- "missingvaluesPercentage"
missing_data$features <- rownames(missing_data)
ggplot(missing_data[missing_data$missingvaluesPercentage >40,],aes(reorder(features,-missingvaluesPercentage),missingvaluesPercentage,fill= features)) +
  geom_bar(stat="identity") +theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") + ylab("Percentage of missingvalues") +
  xlab("Feature") + ggtitle("Understanding Missing Data")

```

##Number of missing values and  Missing values percentage of Bureau variables

```{r,message=FALSE,warning=FALSE}

   sum(is.na(bureau))


 missing_data <- as.data.frame(sort(sapply(bureau, function(x) sum(is.na(x))),decreasing = T))

missing_data <- (missing_data/nrow(bureau))*100

colnames(missing_data)[1] <- "missingvaluesPercentage"
missing_data$features <- rownames(missing_data)
ggplot(missing_data[missing_data$missingvaluesPercentage > 2,],aes(reorder(features,-missingvaluesPercentage),missingvaluesPercentage,fill= features)) +
  geom_bar(stat="identity") +theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") + ylab("Percentage of missingvalues") +
  xlab("Feature") + ggtitle("Understanding Missing Data")
```

##Number of missing values in Bureau balance

```{r,message=FALSE,warning=FALSE}

  sum(is.na(bureau_balance))
  
```
There are No missing Values in Bureau balance


##Number of missing values and  Missing values percentage of creditcard_balance variables

```{r,message=FALSE,warning=FALSE}

missing_data <- as.data.frame(sort(sapply(credit_card_balance, function(x) sum(is.na(x))),decreasing = T))

missing_data <- (missing_data/nrow(credit_card_balance))*100

colnames(missing_data)[1] <- "missingvaluesPercentage"
missing_data$features <- rownames(missing_data)
ggplot(missing_data[missing_data$missingvaluesPercentage > 2,],aes(reorder(features,-missingvaluesPercentage),missingvaluesPercentage,fill= features)) +
  geom_bar(stat="identity") +theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") + ylab("Percentage of missingvalues") +
  xlab("Feature") + ggtitle("Understanding Missing Data")
 ```
 
  
  There are many Missing Values and we need to take care of these NA's
 
 
 As there are many variables ,Lets  visualize all the quantitative variables at once to get a glance of all variables.
 
 
#Exploring Distribution of All  Quantitatve variables 

##Exploring Distribution of All application_train Quantitatve variables 

```{r,message=FALSE,warning=FALSE}
  options(scipen = 99)
  application_train[1:18] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "green")
  ```
  
  AMT_CREDIT      -       Credit amount of the loan
  
AMT_ANNUITY	      -      Loan annuity

AMT_GOODS_PRICE   - 	For consumer loans it is the price of the goods for which the loan is given

AMT_INCOME_TOTAL  - Income of the client

CNT_CHILDREN	         -            Number of children the client has

REGION_POPULATION_RELATIVE  - 	Normalized population of region where client lives
                                (higher number means the client lives in more populated region)
                                
DAYS_BIRTH                -	Client's age in days at the time of application

SK_ID_CURR                - ID of loan in our sample

TARGET            - Target variable 1 - client with payment difficulties: 
                       (he/she had late payment more than X days on at least one of the first Y installments of the loan in our sample)
                        , 0 - all other cases)


```{r,message=FALSE,warning=FALSE}
    options(scipen = 99)
  application_train[16:32] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "green")
```

CNT_FAM_MEMBERS - How many family members does client have

DAYS_ID_PUBLISH - How many days before the application did client change
                   the identity document with which he applied for the loan.
                   
DAYS_REGISTRATION	How many days before the application did client change his registration

FLAG_MOBIL	Did client provide mobile phone (1=YES, 0=NO)

FLAG_EMP_PHONE	Did client provide work phone (1=YES, 0=NO)

FLAG_WORK_PHONE	Did client provide home phone (1=YES, 0=NO)

FLAG_CONT_MOBILE	Was mobile phone reachable (1=YES, 0=NO)

FLAG_PHONE	Did client provide home phone (1=YES, 0=NO)

FLAG_EMAIL	Did client provide email (1=YES, 0=NO)

OWN_CAR_AGE	Age of client's car

REGION_RATING_CLIENT	 rating of the region where client lives (1,2,3)

REGION_RATING_CLIENT_W_CITY	   rating of the region where client lives with taking city into account (1,2,3)


```{r,message=FALSE,warning=FALSE}
   options(scipen = 99)
  application_train[32:42] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "green")
```
  
  EXT_SOURCE_1	Normalized score from external data source

  HOUR_APPR_PROCESS_START	Approximately at what hour did the client apply for the loan
  
REG_REGION_NOT_LIVE_REGION	Flag if client's permanent address does not match contact address (1=different, 0=same, at region level)

REG_REGION_NOT_WORK_REGION	Flag if client's permanent address does not match work address (1=different, 0=same, at region level)

LIVE_REGION_NOT_WORK_REGION	Flag if client's contact address does not match work address (1=different, 0=same, at region level)

REG_CITY_NOT_LIVE_CITY	Flag if client's permanent address does not match contact address (1=different, 0=same, at city level)

REG_CITY_NOT_WORK_CITY	Flag if client's permanent address does not match work address (1=different, 0=same, at city level)

LIVE_CITY_NOT_WORK_CITY	Flag if client's contact address does not match work address (1=different, 0=same, at city level)


 ```{r,message=FALSE,warning=FALSE} 
     options(scipen = 99)
  application_train[43:51] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "green")
```

EXT_SOURCE_3	Normalized score from external data source

Normalized information about building where the client lives, What is average (_AVG suffix),
modus (_MODE suffix), median (_MEDI suffix) apartment size, common area, 
living area, age of building, number of elevators, number of entrances, state of the building, number of floor



```{r,message=FALSE,warning=FALSE} 
  options(scipen = 99)
  application_train[52:60] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "red")  
```


Normalized information about building where the client lives, What is average (_AVG suffix), modus (_MODE suffix), 
median (_MEDI suffix) apartment size, common area, living area, age of building, number of elevators, number of entrances, 
state of the building, number of floor

```{r,message=FALSE,warning=FALSE} 
  application_train[61:70] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "blue") 
 ```
Normalized information about building where the client lives, What is average (_AVG suffix), modus (_MODE suffix), 
median (_MEDI suffix) apartment size, common area, living area, age of building, number of elevators, number of entrances, 
state of the building, number of floor 

```{r,message=FALSE,warning=FALSE} 
 application_train[71:79] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "blue")  
```
Normalized information about building where the client lives, What is average (_AVG suffix), modus (_MODE suffix), 
median (_MEDI suffix) apartment size, common area, living area, age of building, number of elevators, number of entrances, 
state of the building, number of floor 


```{r,message=FALSE,warning=FALSE}  
   application_train[80:88] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "blue")
```

Normalized information about building where the client lives, What is average (_AVG suffix), modus (_MODE suffix), 
median (_MEDI suffix) apartment size, common area, living area, age of building, number of elevators, number of entrances, 
state of the building, number of floor 


```{r,message=FALSE,warning=FALSE}  
   application_train[89:97] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "blue")
```

OBS_30_CNT_SOCIAL_CIRCLE	How many observation of client's social surroundings with observable 30 DPD (days past due) default

DEF_30_CNT_SOCIAL_CIRCLE	How many observation of client's social surroundings defaulted on 30 DPD (days past due) 

OBS_60_CNT_SOCIAL_CIRCLE	How many observation of client's social surroundings with observable 60 DPD (days past due) default

DEF_60_CNT_SOCIAL_CIRCLE	How many observation of client's social surroundings defaulted on 60 (days past due) DPD

DAYS_LAST_PHONE_CHANGE	How many days before application did client change phone


```{r,message=FALSE,warning=FALSE}    
   application_train[98:107] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "blue")
```
FLAG_DOCUMENT_2	Did client provide document 2



```{r,message=FALSE,warning=FALSE}  
   application_train[108:116] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "blue")
```


FLAG_DOCUMENT_12	Did client provide document 12


```{r,message=FALSE,warning=FALSE}    
   application_train[117:122] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "blue")
```

AMT_REQ_CREDIT_BUREAU_HOUR	Number of enquiries to Credit Bureau about the client one hour before application

AMT_REQ_CREDIT_BUREAU_DAY	Number of enquiries to Credit Bureau about the client one day before application (excluding one hour before application)

AMT_REQ_CREDIT_BUREAU_WEEK	Number of enquiries to Credit Bureau about the client one week before application (excluding one day before application)

AMT_REQ_CREDIT_BUREAU_MON	Number of enquiries to Credit Bureau about the client one month before application (excluding one week before application)

AMT_REQ_CREDIT_BUREAU_QRT	Number of enquiries to Credit Bureau about the client 3 month before application (excluding one month before application)

AMT_REQ_CREDIT_BUREAU_YEAR	Number of enquiries to Credit Bureau about the client one day year (excluding last 3 months before application)


##Exploring Distribution of All bureau Quantitatve variables 


```{r,message=FALSE,warning=FALSE}
bureau[1:17] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "green")
```

SK_BUREAU_ID	Recoded ID of previous Credit Bureau credit related to our loan (unique coding for each loan application)

CREDIT_ACTIVE	Status of the Credit Bureau (CB) reported credits

CREDIT_CURRENCY	Recoded currency of the Credit Bureau credit

DAYS_CREDIT	How many days before current application did client apply for Credit Bureau 

CREDIT_DAY_OVERDUE	Number of days past due on CB credit at the time of application for related loan in our sample

DAYS_CREDIT_ENDDATE	Remaining duration of CB credit (in days) at the time of application in Home Credit

DAYS_ENDDATE_FACT	Days since CB credit ended at the time of application in Home Credit (only for closed credit)

AMT_CREDIT_MAX_OVERDUE	Maximal amount overdue on the Credit Bureau credit so far (at application date of loan in our sample)

CNT_CREDIT_PROLONG	How many times was the Credit Bureau credit prolonged

AMT_CREDIT_SUM	Current credit amount for the Credit Bureau credit

AMT_CREDIT_SUM_DEBT	Current debt on Credit Bureau credit

AMT_CREDIT_SUM_LIMIT	Current credit limit of credit card reported in Credit Bureau

AMT_CREDIT_SUM_OVERDUE	Current amount overdue on Credit Bureau credit

CREDIT_TYPE	Type of Credit Bureau credit (Car, cash,...)

DAYS_CREDIT_UPDATE	How many days before loan application did last information about the Credit Bureau credit come



##Exploring Distribution of All bureau_balance Quantitatve variables 

```{r,message=FALSE,warning=FALSE}
bureau_balance %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "green")
```


SK_BUREAU_ID	Recoded ID of Credit Bureau credit (unique coding for each application) - use this to join to CREDIT_BUREAU table 

MONTHS_BALANCE	Month of balance relative to application date (-1 means the freshest balance date)

STATUS	Status of Credit Bureau loan during the month (active, closed, DPD0-30,… 
                 [C means closed,
                 X means status unknown,
                 0 means no DPD, 
                 1 means maximal did during month between 1-30,
                 2 means DPD 31-60,…
                 5 means DPD 120+ or sold or written off ] )

##Exploring Distribution of All credit_cardbalance Quantitatve variables

```{r,message=FALSE,warning=FALSE}
credit_card_balance[1:12] %>%
 keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "green")
```

SK_ID_PREV 	ID of previous credit in Home credit related to loan in our sample. (One loan in our sample can have 0,1,2 or more previous loans in Home Credit)

SK_ID_CURR	ID of loan in our sample

MONTHS_BALANCE	Month of balance relative to application date (-1 means the freshest balance date)

AMT_BALANCE	Balance during the month of previous credit

AMT_CREDIT_LIMIT_ACTUAL	Credit card limit during the month of the previous credit

AMT_DRAWINGS_ATM_CURRENT	Amount drawing at ATM during the month of the previous credit

AMT_DRAWINGS_CURRENT	Amount drawing during the month of the previous credit

AMT_DRAWINGS_OTHER_CURRENT	Amount of other drawings during the month of the previous credit

AMT_DRAWINGS_POS_CURRENT	Amount drawing or buying goods during the month of the previous credit

AMT_INST_MIN_REGULARITY	Minimal installment for this month of the previous credit

AMT_PAYMENT_CURRENT	How much did the client pay during the month on the previous credit

```{r,message=FALSE,warning=FALSE}
credit_card_balance[12:23] %>%
 keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "green")  
```

MONTHS_BALANCE	Month of balance relative to application date (-1 means the information to the freshest monthly snapshot, 
                    0 means the information at application - often it will be the same as -1 as many banks are 
                     not updating the information to Credit Bureau regularly )
                     

AMT_PAYMENT_TOTAL_CURRENT	How much did the client pay during the month in total on the previous credit

AMT_RECEIVABLE_PRINCIPAL	Amount receivable for principal on the previous credit

AMT_RECIVABLE	Amount receivable on the previous credit

AMT_TOTAL_RECEIVABLE	Total amount receivable on the previous credit

CNT_DRAWINGS_ATM_CURRENT	Number of drawings at ATM during this month on the previous credit

CNT_DRAWINGS_CURRENT	Number of drawings during this month on the previous credit

CNT_DRAWINGS_OTHER_CURRENT	Number of other drawings during this month on the previous credit

CNT_DRAWINGS_POS_CURRENT	Number of drawings for goods during this month on the previous credit

CNT_INSTALMENT_MATURE_CUM	Number of paid installments on the previous credit

NAME_CONTRACT_STATUS	Contract status (active signed,...) on the previous credit

SK_DPD	DPD (Days past due) during the month on the previous credit


Lets Analyse the Important features in detail....


#Univariate Analysis - Exploring  each Important  Individual features one by one.
 
##Target - Exploring Dependent variable

```{r,message=FALSE,warning=FALSE}
options(scipen = 99)
application_train %>%
  count(TARGET) %>%
  arrange(desc(n)) %>%
           ggplot(aes(reorder(TARGET, -n, FUN = min), n, fill = TARGET)) +
           geom_col() + 
           theme(legend.position = "none", axis.text.x  = element_text(angle = 90,hjust=1, vjust=0.9)) + 
           labs(x = 'TARGET',y = 'Count') 
```


##Amount Credit Distribution

```{r,message=FALSE,warning=FALSE}
application_train %>% group_by(AMT_CREDIT) %>% 
             summarise(Total = n()) %>% 
             hchart("line", hcaes(x = AMT_CREDIT, y = Total)) %>%
             hc_title(text = "Amount Credited Distribution") %>%
            hc_add_theme(hc_theme_ffx())
 ```
   
##Gender - No of loans as per Gender 
 
```{r,message=FALSE,warning=FALSE}
  
  application_train %>%  
  count(CODE_GENDER) %>%
  arrange(desc(n)) %>%
           ggplot(aes(reorder(CODE_GENDER, -n, FUN = min), n, fill = CODE_GENDER)) +
           geom_col() + 
           theme(legend.position = "none", axis.text.x  = element_text(angle = 90,hjust=1, vjust=0.9)) + 
           labs(x = 'CODE_GENDER',y = 'Count') %>%
           ggtitle("No of people applied for loan Per Each Gender")
```
           
##Contract Type - No of people applied for loan under  each contract type


```{r,message=FALSE,warning=FALSE}
application_train %>%  
  count(NAME_CONTRACT_TYPE) %>%
  arrange(desc(n)) %>%
           ggplot(aes(reorder(NAME_CONTRACT_TYPE, -n, FUN = min), n, fill = NAME_CONTRACT_TYPE)) +
           geom_col() + 
           theme(legend.position = "none", axis.text.x  = element_text(angle = 90,hjust=1, vjust=0.9)) + 
           labs(x = 'NAME_CONTRACT_TYPE',y = 'Count') %>%
           ggtitle("No of people applied for loan under  each contract type")
  ```  
  
##Owns Car -No of people applied for loan Owns Car?

```{r,message=FALSE,warning=FALSE}
   application_train %>%  
  count(FLAG_OWN_CAR) %>%
  arrange(desc(n)) %>%
           ggplot(aes(reorder(FLAG_OWN_CAR, -n, FUN = min), n, fill = FLAG_OWN_CAR)) +
           geom_col() + 
           theme(legend.position = "none", axis.text.x  = element_text(angle = 90,hjust=1, vjust=0.9)) + 
           labs(x = 'Own Car',y = 'Count') %>%
           ggtitle("No of people applied for loan Owns Car?")
  ```

 MORE TO COME.............STAY TUNED.............
 