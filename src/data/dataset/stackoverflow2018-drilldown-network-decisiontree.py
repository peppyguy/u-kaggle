---
title: "Detailed Exploratory Data Analysis : Stack Overflow Survey 2018"
author: "Ankur Singh & Ankush Jain"
date: "May 31, 2018"
output: 
  html_document:
    code_folding: hide
    toc: TRUE
---

## Motivation For this Notebook

+ Recently, we decided to try some new and different kind of plots among the vast number of libraries that are available online for data visualization. So here we are presenting this Notebook with some interesting plots - DrillDown Charts, Network Plots & Motion Plots.
+ The notebook has been divided into 5 major sections (A-E) that deal with different aspects of the dataset.
+ **Insights** provide the technical information derived from plots.
+ **Inference** highlight the MOST IMPORTANT FINDINGS from the data and gives better understanding of the insights by putting light on the potential meaning of the insights.
+ Constructive criticism will be appreciated. Please upvote our work! Your support will motivate us to try more cool stuff and bring it to the community.

<center><img src="https://cdn-images-1.medium.com/max/1000/1*tBGHrk548cAdfrpk2hhXhA.png"></center>

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE,warning=FALSE,message=FALSE)
```

```{r,echo=FALSE,results='hide'}
# Loading Packages
library(dplyr)
library(highcharter)
library(tidyr)
library(viridisLite)
library(countrycode)
library(ggplot2)
library(gdata)
library(purrr)
library(rworldmap)
library (igraph)
library(visNetwork)
library(randomcoloR)
library(readr)
library(stringr)
library(scales)
library(reshape2)
library(rpart)
```

```{r,echo=FALSE,results='hide'}
#Loading Datasets
survey_results_public <- read.csv("../input/survey_results_public.csv",stringsAsFactors =FALSE)
```
<hr>
# A. Developer Profile Analysis
<hr>
This section deals with the analysis of the most important aspect of Stack Overflow - its community. It shows a detailed analysis of the various metrics that were explored during the EDA.
We will take a detailed look at the Country, Education, Non-Degree Education Sources, Occupations, Job Experience, Demoplotics (such as Gender, Age) and the overall satisfaction of the users with Stack Overflow and its community. 

## 1. What is their Country?

### 1.1. All Respondents by Country

```{r}
#Load worldgeojson for word map plot
data(worldgeojson, package = "highcharter")

#Grouping country by name and then counting number of respondent
by_country <- survey_results_public %>% select(Country) %>% filter(!is.na(Country)) %>%group_by(Country) %>% summarise(n1=n())
code <- countrycode(by_country$Country, 'country.name', 'iso3c')
by_country$iso3 <- code

#Making highchart plot of world map
p_by_country <- highchart() %>% 
                  hc_add_series_map(worldgeojson, by_country, value = "n1", joinBy = "iso3") %>% 
                  hc_colorAxis(stops = color_stops()) %>% 
                  hc_legend(enabled = TRUE) %>%  
                  hc_mapNavigation(enabled = TRUE) %>%
                  hc_title(text = "Respondent by Country")  %>%
                  hc_tooltip(useHTML = TRUE, headerFormat = "",
                            pointFormat = "Country: {point.Country} Total Respondent: {point.n1}") %>%  hc_add_theme(hc_theme_google())

#For professionals, I am filting the student="No"
professionals_result <- survey_results_public %>% filter(Student=="No")

#Grouping country by name and then counting number of respondent
professionals_by_country <- professionals_result %>% select(Country) %>% filter(!is.na(Country)) %>%group_by(Country) %>% summarise(n2=n())
code <- countrycode(professionals_by_country$Country, 'country.name', 'iso3c')
professionals_by_country$iso3 <- code


#For getting country code, joining by iso3
combined_result <- by_country %>% left_join(professionals_by_country, by="iso3") %>% select(iso3, Country.x, n1, n2)
names(combined_result) <- c("iso3", "Country", "n1", "n2")


#Making highchart plot of world map by professionals
p_professionals_by_country <- highchart() %>% 
                              hc_add_series_map(worldgeojson, combined_result, value = "n2", joinBy = "iso3") %>% 
                              hc_colorAxis(stops = color_stops()) %>% 
                              hc_legend(enabled = TRUE) %>% 
                              #hc_add_theme(hc_theme_google()) %>% 
                              hc_mapNavigation(enabled = TRUE) %>%
                              hc_title(text = "Professionals Respondent by Countries")  %>%
                              hc_tooltip(useHTML = TRUE, headerFormat = "",
                                        pointFormat = "Country: {point.Country} Professionals: {point.n2} Total Respondent: {point.n1}") %>% hc_add_theme(hc_theme_google())

lst <- list(
  p_by_country,
  p_professionals_by_country
)

#For view
hw_grid(lst, rowheight = 350)

```

#### **Insight**

+ The first plot gives the number of respondents that belong to each country. It can be observed that the top 3 highest number of respondents from various countries are:
    1. United States : 20309
    2. India : 13721
    3. Germany : 6459
+ The second plot gives the number of professional respondents that belong to each country. It can be observed that the top 3 highest number of respondents who are 'Professional' from various countries are:
    1. United States : 17220
    2. India : 8228
    3. United Kingdom : 5354

#### **Inference**

+ Major user base of Stack Overflow is from United States and India.

## 2. What is their Education?

### 2.1. Are they Student?

```{r}

by_Student <- survey_results_public %>%
                        filter(!is.na(Student)) %>%
                        group_by(Student) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(Student = reorder(Student,Total)) %>%
                        mutate(Percent = round(Total/sum(Total)*100)) %>%
                        head(10)

highchart() %>%
  hc_xAxis(categories = by_Student$Student) %>% 
  hc_add_series(name = "Percent %", data = by_Student$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Are Respondent Student")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ It can be observed that 74% are not students while the remaining 26% are students.

#### **Inference**

+ Major user base of Stack Overflow are Non Students.

### 2.2. Formal Education

```{r}

by_FormalEducation <- survey_results_public %>%
                        filter(!is.na(FormalEducation)) %>%
                        group_by(FormalEducation) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(FormalEducation = reorder(FormalEducation,Total)) %>%
                        mutate(Percent = round(Total/sum(Total)*100)) %>%
                        head(10)

p_by_FormalEducation <- highchart() %>%
                          hc_xAxis(categories = by_FormalEducation$FormalEducation) %>% 
                          hc_add_series(name = "Percent %", data = by_FormalEducation$Percent, colorByPoint =  1) %>% 
                          hc_title(text = "Formal Education of Respondent")  %>%
                          hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())


by_UndergradMajor <- survey_results_public %>%
                        filter(!is.na(UndergradMajor)) %>%
                        group_by(UndergradMajor) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(UndergradMajor = reorder(UndergradMajor,Total)) %>%
                        mutate(Percent = round(Total/sum(Total)*100)) %>%
                        head(10)

p_by_UndergradMajor <-  highchart() %>%
                          hc_xAxis(categories = by_UndergradMajor$UndergradMajor) %>% 
                          hc_add_series(name = "Percent %", data = by_UndergradMajor$Percent, colorByPoint =  1) %>% 
                          hc_title(text = "Main Field of Study of Respondent")  %>%
                          hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())


lst <- list(
  p_by_FormalEducation,
  p_by_UndergradMajor
)

hw_grid(lst, rowheight = 400)

```

#### **Insight**

+ In first plot, it illustrates that majority of respondents are Bachelor's Degree holders, followed by Master's Degree holders & other college graduates.
+ In second plot, it illustrates that Computer Science, Computer Engineering or Software Engineering is the main field of study of respondents at 64%.

### 2.3. Non-Degree Education

```{r}

by_EducationTypes <-    survey_results_public %>% 
                        select(Respondent,EducationTypes) %>% 
                        mutate(EducationTypes = strsplit(as.character(EducationTypes), ";")) %>% 
                        unnest(EducationTypes) %>%
                        filter(!is.na(EducationTypes)) %>%
                        group_by(EducationTypes) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(EducationTypes = reorder(EducationTypes,Total)) %>%
                        mutate(Percent = round(Total/nrow(survey_results_public)*100)) %>%
                        head(10)

p_by_EducationTypes <- highchart() %>%
                          hc_xAxis(categories = by_EducationTypes$EducationTypes) %>% 
                          hc_add_series(name = "Percent %", data = by_EducationTypes$Percent, colorByPoint =  1) %>% 
                          hc_title(text = "Non-Degree Education of Respondent")  %>%
                          hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

by_SelfTaughtTypes <-   survey_results_public %>% 
                        select(Respondent,SelfTaughtTypes) %>% 
                        mutate(SelfTaughtTypes = strsplit(as.character(SelfTaughtTypes), ";")) %>% 
                        unnest(SelfTaughtTypes) %>%
                        filter(!is.na(SelfTaughtTypes)) %>%
                        group_by(SelfTaughtTypes) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(SelfTaughtTypes = reorder(SelfTaughtTypes,Total)) %>%
                        mutate(Percent = round(Total/nrow(survey_results_public)*100)) %>%
                        head(10)

p_by_SelfTaughtTypes <- highchart() %>%
                          hc_xAxis(categories = by_SelfTaughtTypes$SelfTaughtTypes) %>% 
                          hc_add_series(name = "Percent %", data = by_SelfTaughtTypes$Percent, colorByPoint =  1) %>% 
                          hc_title(text = "Self Taught Types Education of Respondent")  %>%
                          hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

lst <- list(
  p_by_EducationTypes,
  p_by_SelfTaughtTypes
)

hw_grid(lst, rowheight = 400)

```

#### **Insight**

+ In first plot, Non-Degree Educated respondents have educated themselves in the various ways as illustrated. Most common sources of gaining knowledge are Self Learning at 60%, followed by Online Courses at 33% and Open Source Contribution at 28%.
+ In second plot, it further elaborate the Self Taught Respondents. Most common self teaching methodology includes reading Official Documentation at 48%, Q&A at SO at 48% and Books & E-Books at 29%.

### 2.4. Types of Non-Degree Education Vs Time to get a Full-time Job as a Developer

```{r}

survey_results_public2 <-  survey_results_public %>%    mutate(EducationTypes = strsplit(as.character(EducationTypes), ";"))  %>%
                                                        unnest(EducationTypes)

df1 <- survey_results_public2 %>%
       filter(!is.na(EducationTypes)) %>%
       group_by(name = EducationTypes, drilldown = tolower(EducationTypes)) %>% 
       summarise(y = n()) %>% arrange(desc(y))

df2 <-survey_results_public2 %>% filter(!is.na(EducationTypes)) %>% filter(!is.na(TimeAfterBootcamp)) %>% group_by(EducationTypes,TimeAfterBootcamp) %>% dplyr::mutate(y = n()) %>%arrange(desc(y))%>%
  group_by(name = EducationTypes, id = tolower(EducationTypes)) %>% 
  do(data = list_parse(
                  mutate(.,name = TimeAfterBootcamp, drilldown = tolower(paste(EducationTypes,TimeAfterBootcamp,sep=": "))) %>% 
                      group_by(name,drilldown) %>% 
                        summarise(y=n())%>% dplyr::select(name, y, drilldown)   %>%
                            arrange(desc(y))) 
    )
    
highchart() %>% 
  hc_chart(type = "column") %>%
  hc_title(text = 'Types of Non-Degree Education Vs Time to get a Full-time Job as a Developer ') %>%
  hc_add_series(data = df1, name = "Types of Non-Degree Education",colorByPoint =  1) %>% 
  hc_legend(enabled = FALSE) %>%
  hc_xAxis(type = "category") %>% 
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series =list_parse(df2)
  ) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ Note : It's a Drill down plot.
+ This plot illustrates the amount of time taken by Respondents in getting a Full Time Job as a Developer after gaining knowledge from Non Degree Education sources.
+ On upper level it illustrates the number of repondents for each source. But after drill down it shows the number of respondents in different time intervals.
+ It has been observed that most repondents were 'Already Full Time Developers' in each of the knowledge source.

### 2.5. Network Analysis : Education Types

```{r}

df <- survey_results_public %>% select(Respondent,EducationTypes)
df2 <- df %>% 
         mutate(EducationTypes = strsplit(as.character(EducationTypes), ";")) %>% 
         unnest(EducationTypes)
         
df2_edges <- df2 %>% group_by(Respondent) %>%
             filter(n()>=2) %>%
             do(data.frame(t(combn((.)[["EducationTypes"]], 2)), stringsAsFactors=FALSE)) %>% ungroup() %>%
             rename(source = X1, target = X2) %>%
             select(-Respondent)

df2_edges <- df2_edges %>% group_by(source,target) %>% summarise(weight=n())

names(df2_edges) <- c("from","to","weight")
df2_edges$weight <- df2_edges$weight/1500

df2_edges$width <- 1+df2_edges$weight # line width
#df2_edges$color <- "gray"    # line color  
#df2_edges$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
df2_edges$smooth <- FALSE    # should the edges be curved?
df2_edges$shadow <- FALSE    # edge shadow

df2_nodes <- df2 %>% filter(!is.na(EducationTypes)) %>% group_by(EducationTypes) %>% summarise(n = n()/1000) %>% arrange(desc(n))
names(df2_nodes) <- c("id","size")

n <- nrow(df2_nodes)
palette <- distinctColorPalette(n)

df2_nodes$shape <- "dot"  
df2_nodes$shadow <- TRUE # Nodes will drop shadow
df2_nodes$title <- df2_nodes$id # Text on click
df2_nodes$label <- df2_nodes$id # Node label
df2_nodes$size <- df2_nodes$size # Node size
df2_nodes$borderWidth <- 2 # Node border width

df2_nodes$color.background <- palette[as.numeric(as.factor(df2_nodes$id))]
df2_nodes$color.border <- "black"
df2_nodes$color.highlight.background <- "orange"
df2_nodes$color.highlight.border <- "darkred"

df2_nodes <- df2 %>% filter(!is.na(EducationTypes)) %>% group_by(EducationTypes) %>% summarise(n = n()/1000) %>% arrange(desc(n))
names(df2_nodes) <- c("id","size")

n <- nrow(df2_nodes)
palette <- distinctColorPalette(n)

df2_nodes$shape <- "dot"  
df2_nodes$shadow <- TRUE # Nodes will drop shadow
df2_nodes$title <- df2_nodes$id # Text on click
df2_nodes$label <- df2_nodes$id # Node label
df2_nodes$size <- df2_nodes$size # Node size
df2_nodes$borderWidth <- 2 # Node border width

df2_nodes$color.background <- palette[as.numeric(as.factor(df2_nodes$id))]
df2_nodes$color.border <- "black"
df2_nodes$color.highlight.background <- "orange"
df2_nodes$color.highlight.border <- "darkred"

visNetwork(df2_nodes, df2_edges, height = "500px", width = "100%") %>% visIgraphLayout(layout = "layout_with_lgl") %>% 
  visEdges(shadow = TRUE,
           color = list(color = "gray", highlight = "orange"))
```

#### **Insight**

+ This is a Network plot for the different Non Degree Education sources.
+ Each node denotes the different education source and the size of node denotes the number of respondents that used the particular source.
+ Each connecting edge between any two nodes denotes that the respondents chose both the education sources. And, the width of the edge denotes the number of users that chose both the education sources. 
+ It can be observed that the top 5 highest correlation is between:
    1. 'Taught yourself a new language...' --- 'Taken an online course...'
    2. 'Taught yourself a new language...' --- 'Contributed to open source software'
    3. 'Taught yourself a new language...' --- 'Received on the job training in software development'
    4. 'Taught yourself a new language...' --- 'Participated in a hackathon'
    5. 'Taken an online course...' --- 'Contributed to open source software'
+ This means that all the people who chose the first option among the above also chose the second option.

#### **Inference**

+ Self Learning is the most common mode of Non-Degree Education.

### 2.6. Hackathon Reasons

```{r}

by_HackathonReasons <-   survey_results_public %>% 
                        select(Respondent,HackathonReasons) %>% 
                        mutate(HackathonReasons = strsplit(as.character(HackathonReasons), ";")) %>% 
                        unnest(HackathonReasons) %>%
                        filter(!is.na(HackathonReasons)) %>%
                        group_by(HackathonReasons) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(HackathonReasons = reorder(HackathonReasons,Total)) %>%
                        mutate(Percent = round(Total/nrow(survey_results_public)*100)) %>%
                        head(10)

highchart() %>%
  hc_xAxis(categories = by_HackathonReasons$HackathonReasons) %>% 
  hc_add_series(name = "Percent %", data = by_HackathonReasons$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Reasons For Participating in Hackathon")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ This is a bar plot illustrating the various reassons given by the respondents for participating in the hackathons.
+ The top 3 most common reasons are:
    1. 'Beacause I find it enjoyable' : 20%
    2. 'To improve my general technical skills or programming ability' : 17%
    3. 'To improve my knowledge of a specific programming language...' : 13%

## 3. What is their Occupation?

### 3.1. DevType : Describe you

```{r}

by_DevType <-   survey_results_public %>% 
                        select(Respondent,DevType) %>% 
                        mutate(DevType = strsplit(as.character(DevType), ";")) %>% 
                        unnest(DevType) %>%
                        filter(!is.na(DevType)) %>%
                        group_by(DevType) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(DevType = reorder(DevType,Total)) %>%
                        mutate(Percent = round(Total/nrow(survey_results_public)*100)) %>%
                        head(10)

highchart() %>%
  hc_xAxis(categories = by_DevType$DevType) %>% 
  hc_add_series(name = "Percent %", data = by_DevType$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Developer Type")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ This bar plot illustrates the different kind of developers.
+ There are 10 type of developers.
+ The top 3 most commonn types are:
    1. Back-end developer : 54%
    2. Full-stack developer : 45%
    3. Front-end developer : 35%
    4. Mobile developer : 19%
    5. Desktop or enterprise application developer : 16%

#### **Inference**

+ Back-end, Front-end & Full Stack Developers are the most integral part of Stack Overflow.

### 3.2. Network Analysis : DevType

```{r}

df <- survey_results_public %>% select(Respondent,DevType)
df2 <- df %>% 
         mutate(DevType = strsplit(as.character(DevType), ";")) %>% 
         unnest(DevType)
         
df2_edges <- df2 %>% group_by(Respondent) %>%
             filter(n()>=2) %>%
             do(data.frame(t(combn((.)[["DevType"]], 2)), stringsAsFactors=FALSE)) %>% ungroup() %>%
             rename(source = X1, target = X2) %>%
             select(-Respondent)

df2_edges <- df2_edges %>% group_by(source,target) %>% summarise(weight=n())


names(df2_edges) <- c("from","to","weight")
df2_edges$weight <- df2_edges$weight/1500

df2_edges$width <- 1+df2_edges$weight # line width
#df2_edges$color <- "gray"    # line color  
#df2_edges$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
df2_edges$smooth <- FALSE    # should the edges be curved?
df2_edges$shadow <- FALSE    # edge shadow

df2_nodes <- df2 %>% filter(!is.na(DevType)) %>% group_by(DevType) %>% summarise(n = n()/700) %>% arrange(desc(n))
names(df2_nodes) <- c("id","size")

n <- nrow(df2_nodes)
palette <- distinctColorPalette(n)

df2_nodes$shape <- "dot"  
df2_nodes$shadow <- TRUE # Nodes will drop shadow
df2_nodes$title <- df2_nodes$id # Text on click
df2_nodes$label <- df2_nodes$id # Node label
df2_nodes$size <- df2_nodes$size # Node size
df2_nodes$borderWidth <- 2 # Node border width

df2_nodes$color.background <- palette[as.numeric(as.factor(df2_nodes$id))]
df2_nodes$color.border <- "black"
df2_nodes$color.highlight.background <- "orange"
df2_nodes$color.highlight.border <- "darkred"

visNetwork(df2_nodes, df2_edges, height = "500px", width = "100%") %>% visIgraphLayout(layout = "layout_with_lgl") %>% 
  visEdges(shadow = TRUE,
           color = list(color = "gray", highlight = "orange"))
```

#### **Insight**

+ This is a network plot for the different types of developers.
+ Each node represents the type of developer and the size of node denotes the number of developers belonging to each type.
+ Each connecting edge between any two nodes denotes that the respondents chose both the developer types. The width of each edge denotes the nmber of respondents that chose both the developer types.
+ It can be observed that the top 5 highest correlation is between:
    1. Back end developer --- Full stack developer
    2. Front end developer --- Full stack developer
    3. Back end developer --- Front end developer
    4. Full stack developer --- Database administrator
    5. Desktop or enterprise application developer --- Back end developer

#### **Inference**

+ Back-end, Front-end and Full Stack Developers have high degree of correlation. Thus, if a developer is one of the three kinds (say Back-end) then she is probably a developer of the remaining two kinds (Front-end or Full-stack or both) as well.

### 3.3. Who are the Open Source Project Contributors?

```{r}

survey_results_public2 <-  survey_results_public %>%    mutate(DevType = strsplit(as.character(DevType), ";"))  %>%
                                                        unnest(DevType) %>% 
                                                        filter(!is.na(OpenSource)) %>% filter(!is.na(DevType)) %>%
                                                        select(DevType, OpenSource)

survey_results_public2 <- survey_results_public2 %>% filter(!is.na(DevType)) %>% group_by(DevType) %>% summarise(count=n()) %>% right_join(survey_results_public2,by="DevType")


df1 <- survey_results_public2 %>%
       filter(!is.na(OpenSource)) %>%
       group_by(name = OpenSource, drilldown = tolower(OpenSource)) %>% 
       summarise(Total = n()) %>% ungroup() %>% mutate(y = (Total/sum(Total))*100) %>% arrange(desc(y))

df2 <-survey_results_public2 %>% filter(!is.na(OpenSource)) %>% filter(!is.na(DevType)) %>% group_by(OpenSource,DevType) %>% dplyr::mutate(y = n()) %>%arrange(desc(y))%>%
  group_by(name = OpenSource, id = tolower(OpenSource)) %>% 
  do(data = list_parse(
                  mutate(.,name = DevType, count=as.numeric(count) , drilldown = tolower(paste(OpenSource,DevType,sep=": "))) %>% 
                      group_by(name,drilldown,count) %>% 
                       summarise(y=n()) %>% ungroup() %>% filter(y>=100) %>% mutate(y = (y/count)*100)%>% dplyr::select(name, y, drilldown)   %>%
                            arrange(desc(y))) %>% head(10) 
    )
    
highchart() %>% 
  hc_chart(type = "bar") %>%
  hc_title(text = 'Who Contributed to Open Source Projects') %>%
  hc_add_series(data = df1, name = "Is Contributed to opensource",colorByPoint =  1) %>% 
  hc_legend(enabled = FALSE) %>%
  hc_xAxis(type = "category") %>% 
  hc_yAxis(title = list(text = "Percentage %")) %>% 
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series =list_parse(df2)
  ) %>% 
hc_add_theme(hc_theme_google())

```

#### **Insight**

+ Note : It's a Drill Down plot.
+ This plot illustrates the detailed analysis of Open Source contributors.
+ At upper level the plot illustrates whether the respondent answered Yes or No for Open Source project contribution.
+ At deeper level for each answer choice, the plot illustrates the % of respondents belonging to each developer type.
+ Yes : 135508
+ No : 147354
+ Top 3 developer types with highest respondents choosing "Yes" are:
    1. **61%** of `C-suite executive (CEO, CTO, etc.)` are contributes in Open Source Projects.
    2. **60.5%** of `DevOps specialist` are contributes in Open Source Projects.   
    3. **60%** of `Educator or Academic` Researcher are contributes in Open Source Projects.
+ Top 3 developer types with highest respondents choosing "No" are:
    1. **58%** of `Students` are not contributes in Open Source Projects.
    2. **57%** of `Data or Business Analyst` are not contributes in Open Source Projects.
    3. **57%** of `Desktop or enterprise application developer` are not contributes in Open Source Projects.

#### **Inference**

+ About 47.8% community has participated in Open Source Projects.
+ `C-suite executive (CEO, CTO, etc.)`, `DevOps specialist` and `Educator or Academic` have participated the most(in %) in Open Source Projects.

### 3.4. Who have Coding as Hobby?

```{r}

survey_results_public2 <-  survey_results_public %>%    mutate(DevType = strsplit(as.character(DevType), ";"))  %>%
                                                        unnest(DevType) %>% 
                                                        filter(!is.na(Hobby)) %>% filter(!is.na(DevType)) %>%
                                                        select(DevType, Hobby)

survey_results_public2 <- survey_results_public2 %>% filter(!is.na(DevType)) %>% group_by(DevType) %>% summarise(count=n()) %>% right_join(survey_results_public2,by="DevType")


df1 <- survey_results_public2 %>%
       filter(!is.na(Hobby)) %>%
       group_by(name = Hobby, drilldown = tolower(Hobby)) %>% 
       summarise(Total = n()) %>% ungroup() %>% mutate(y = (Total/sum(Total))*100) %>% arrange(desc(y))

df2 <-survey_results_public2 %>% filter(!is.na(Hobby)) %>% filter(!is.na(DevType)) %>% group_by(Hobby,DevType) %>% dplyr::mutate(y = n()) %>%arrange(desc(y))%>%
  group_by(name = Hobby, id = tolower(Hobby)) %>% 
  do(data = list_parse(
                  mutate(.,name = DevType, count=as.numeric(count) , drilldown = tolower(paste(Hobby,DevType,sep=": "))) %>% 
                      group_by(name,drilldown,count) %>% 
                       summarise(y=n()) %>% ungroup() %>% filter(y>=100) %>% mutate(y = (y/count)*100)%>% dplyr::select(name, y, drilldown)   %>%
                            arrange(desc(y))) %>% head(10) 
    )
    
highchart() %>% 
  hc_chart(type = "bar") %>%
  hc_title(text = 'Who have code as Hobby') %>%
  hc_add_series(data = df1, name = "Is Coding Hobby",colorByPoint =  1) %>% 
  hc_legend(enabled = FALSE) %>%
  hc_xAxis(type = "category") %>% 
  hc_yAxis(title = list(text = "Percentage %")) %>% 
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series =list_parse(df2)
  ) %>% 
hc_add_theme(hc_theme_google())

```

#### **Insight**

+ Note : It's a Drill down plot.
+ This plot illustrates the detailed analysis of 'Is Coding a Hobby' respondents.
+ At upper level the plot illustrates whether the respondent answered Yes or No for 'Is Coding a Hobby'.
+ At deeper level for each answer choice, the plot illustrates the % of respondents belonging to each developer type.
+ Yes : 235293
+ No : 46569
+ Top 3 developer types with highest respondents choosing "Yes" are:
    1. **90%** of `Game or graphics developer` have coding as Hobby.
    2. **87%** of `Student` have coding as Hobby 
    3. **86%** of `System administrator` have coding as Hobby 
+ Top 3 developer types with highest respondents choosing "No" are:
    1. **20%** of `Marketing or sales Professional` have not coding as Hobby 
    2. **20%** of `Data or Business Analyst` have not coding as Hobby 
    3. **19%** of `Desktop or enterprise application developer` have not coding as Hobby. 

#### **Inference**

+ 83.2% respondents on Stack Overflow have coding as Hobby.
+ 20% `Data or Business Analyst` have not coding as Hobby.

## 4. What is their Experience?

### 4.1. No. of Years Coded Vs No. of Years Coded Professionally


```{r}

df1 <- survey_results_public %>% filter(!is.na(YearsCoding)) %>%
  group_by(name = YearsCoding, drilldown = tolower(YearsCoding)) %>% 
  summarise(y = n()) %>% arrange(desc(y)) %>% head(10)


df2 <-survey_results_public %>% filter(!is.na(YearsCoding)) %>% filter(!is.na(YearsCodingProf)) %>%  group_by(YearsCoding,YearsCodingProf) %>% dplyr::mutate(y = n()) %>%arrange(desc(y))%>%
  group_by(name = YearsCoding, id = tolower(YearsCoding)) %>% 
  do(data = list_parse(
                  mutate(.,name = YearsCodingProf, drilldown = tolower(paste(YearsCoding,YearsCodingProf,sep=": "))) %>% 
                      group_by(name,drilldown) %>% 
                        summarise(y=n())%>% dplyr::select(name, y, drilldown)   %>%
                            arrange(desc(y)))
    )

highchart() %>% 
  hc_chart(type = "column") %>%
  hc_title(text = 'No. of Years Coded Vs No. of Years Coded Professionally') %>%
  hc_add_series(data = df1, name = "No. of Years Coded",colorByPoint =  1) %>% 
  hc_legend(enabled = FALSE) %>%
  hc_xAxis(type = "category") %>% 
  hc_yAxis(title = list(text = "Total Response"))  %>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list_parse(df2)
  ) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ Note : It's a Drill down plot.
+ This plot illustrates the detailed analysis of 'No. of years Coding' and 'No. of years Coding Professionally' respondents.
+ At upper level the plot illustrates the number of respondents for each interval of 'No. of years Coding'.
+ At deeper level for each interval of 'No. of years Coding', the plot illustrates the 'No. of years Coding professionally' for the repondents.
+ For example some X people have coded for 6-8 years, then after drilling down, we will find out that out of 6-8 years, for how many years they have coded professionally.
+ Top 3 categories with highest number of respondents are:
    1. 3-5 years : 23313
        Professional Coding Years
            1. 0-2 years : 10104
            2. 3-5 years : 8126
            
    2. 6-8 years : 19338 : 
        Professional Coding Years
            1. 3-5 years : 7678
            2. 0-2 years : 4753
            3. 6-8 years : 4064
            
    3. 9-11 years : 12169
        Professional Coding Years
            1. 6-8 years : 3616
            2. 3-5 years : 3356
            3. 9-11 years : 2408
            4. 0-2 years : 1215

#### **Inference**

+ **IMPORTANT** : It can be observed that within each time interval, there is a very small number of respondents who have answered for a higher interval of 'Professional Coding Years' even when they belong to the lower interval of 'Coding Years'. These are the outliers in the data as its logically incorrect. For instance, someone with total coding experience of 5 years cannot have professional coding experience of 20 years.

## 5. What are the Demographics?


### 5.1. Gender Distribution

```{r}

by_Gender <-   survey_results_public %>% 
                mutate(Gender = strsplit(as.character(Gender), ";"))  %>%
                unnest(Gender) %>%
                filter(!is.na(Gender)) %>%
                group_by(Gender) %>%
                summarise(n = n()) %>%
                mutate(percentage = round((n / sum(n))*100))

highchart() %>% 
 hc_chart(type = "column") %>% 
 hc_title(text = "Gender Distribution") %>%
 hc_add_series_labels_values(labels = by_Gender$Gender, values = by_Gender$percentage,colorByPoint =  1) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ This plot illustrates the Gender Distribution of Respondents.
    1. Male : 92%
    2. Female : 7%
    3. Non Binary : 1%
    4. Transgender : 1%

#### **Inference**

+ The major developers on Stack Overflow are Male.

### 5.2. Gender By Sexual Orientation

```{r}

survey_results_public2 <- survey_results_public %>% filter(!is.na(Gender)) %>%
                          mutate(Gender = strsplit(as.character(Gender), ";"))  %>%
                          unnest(Gender) %>%
                          filter(!is.na(SexualOrientation)) %>%
                          mutate(SexualOrientation = strsplit(as.character(SexualOrientation), ";"))  %>%
                          unnest(SexualOrientation)


df1 <- survey_results_public2 %>% filter(!is.na(Gender)) %>%
  group_by(name = Gender, drilldown = tolower(Gender)) %>% 
  summarise(y = n()) %>% arrange(desc(y)) %>% head(10)


df2 <-survey_results_public2 %>% filter(!is.na(Gender)) %>% filter(!is.na(SexualOrientation)) %>%  group_by(Gender,SexualOrientation) %>% dplyr::mutate(y = n()) %>%arrange(desc(y))%>%
  group_by(name = Gender, id = tolower(Gender)) %>% 
  do(data = list_parse(
                  mutate(.,name = SexualOrientation, drilldown = tolower(paste(Gender,SexualOrientation,sep=": "))) %>% 
                      group_by(name,drilldown) %>% 
                        summarise(y=n())%>% dplyr::select(name, y, drilldown)   %>%
                            arrange(desc(y))) %>% head(10)
    )

highchart() %>% 
  hc_chart(type = "column") %>%
  hc_title(text = 'Gender By Sexual Orientation') %>%
  hc_add_series(data = df1, name = "Gender",colorByPoint =  1) %>% 
  hc_legend(enabled = FALSE) %>%
  hc_xAxis(type = "category") %>% 
  hc_yAxis(type = 'logarithmic',title = list(text = "Total Response"))  %>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list_parse(df2)
  ) %>% hc_add_theme(hc_theme_google())

```

#### **Insights**

+ Note : This is a drill down plot.
+ At upper level, the gender distribution is plotted.
+ At deeper level, the sexual orientations of each gender are plotted.

### 5.3. Years of Experience by Gender

```{r}

by_yearOfExp_gender <- survey_results_public %>% select(YearsCoding, Gender) %>%
                              filter(!is.na(YearsCoding)) %>%
                              mutate(YearsCodingNum = parse_number(YearsCoding),
                                     Gender = str_split(Gender, pattern = ";")) %>%
                              unnest(Gender) %>%
                              mutate(Gender = case_when(str_detect(Gender, "Non-binary") ~ "Non-binary",
                                                        TRUE ~ Gender)) %>%
                              group_by(YearsCodingNum, Gender) %>%
                              summarise(n = n()) %>% 
                              filter(Gender %in% c("Male", "Female", "Non-binary"))

hchart(by_yearOfExp_gender, "line", hcaes(x = YearsCodingNum, y = n, group = Gender))  %>%
hc_title(text = 'Gender Vs Years of Experience') %>%
hc_xAxis(title = list(text = "Years of Experience")) %>% 
hc_yAxis(title = list(text = "No Of Male/Female")) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ This plot illustrates the number of years of experience vs the number of respondents belonging to a gender.
+ Years of Experience with highest number of respondents : 3 Years
    1. Male 13101
    2. Female 1349
    3. Non-binary 136

#### **Inference**

+ The number of respondents on Stack Overflow is decreasing after 3 years of experience among all the genders except for repondents with more than 30 years of experience.

### 5.4. Gender % by Years of Experience

```{r}

by_yearOfExp_gender$Percent <- (by_yearOfExp_gender$n/sum(by_yearOfExp_gender$n)*100)

aqw <- dcast(by_yearOfExp_gender, YearsCodingNum ~ Gender)
aqw$sum <- aqw$Female + aqw$Male + aqw$`Non-binary`

highchart() %>% 
  hc_title(text = 'Gender % Vs Years of Experience') %>%
  hc_chart(type = "column") %>% 
  hc_xAxis(categories = aqw$YearsCodingNum,title = list(text = "Years of Experience")) %>% 
  hc_add_series(data = aqw$sum) %>%
  hc_add_series(name = "Male",type = "line", data = aqw$Male) %>%
  hc_add_series(name = "Female",type = "line", data = aqw$Female) %>%
  hc_add_series(name = "Non-binary",type = "line", data = aqw$`Non-binary`) %>%
  hc_yAxis(title = list(text = "% Of Male/Female/Non-binary"))  %>% hc_add_theme(hc_theme_google())


```

#### **Insight**

+ The line plot in the given plot is same as above but the bar plot is cumulative % of all the repondents irrespective of their gender.
+ Years of Experience with highest number of respondents : 
    + Interval : 3 Years
    + % of Respondents : 22.52%

#### **Inference**

+ It can be observed that overall as well, the number of respondents with higher experience generally declines after 3 years among all the respondents except for repondents with more than 30 years of experience.

## 6. How is their connection with the other Developers?

### 6.1. Kinship Vs Competition Vs Negative Self-Evaluation

```{r}

x <-data.frame(table(survey_results_public$AgreeDisagree1))
y <- data.frame(table(survey_results_public$AgreeDisagree2))
z <-data.frame(table(survey_results_public$AgreeDisagree3))

highchart() %>% 
  hc_title(text = 'Kinship vs Competition vs Negative Self-Evaluation') %>%
  hc_chart(type = "column") %>% 
  hc_xAxis(categories = x$Var1,title = list(text = "Agreement Level Scale")) %>% 
  hc_add_series(name = "I feel a sense of kinship or connection to other developers",type = "line", data = x$Freq) %>%
  hc_add_series(name = "I think of myself as competing with my peers",type = "line", data = y$Freq) %>%
  hc_add_series(name = "I'm not as good at programming as most of my peers",type = "line", data = z$Freq) %>%
  hc_yAxis(title = list(text = "Total"))  %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ This plot gives number of respondents who answered the three questions - 'Sense of kinship', 'Competing with peers' and 'Not a good programmer' according to their responses.
+ Highest number of responses for each question :
    1. 'Kinship' - 36777 Agree
    2. 'Competition' - 18673 Agree
    3. 'Negative Self-Evaluation' - 23341 Disagree
+ Lowest number of reponses for each question : 
    1. 'Kinship' - 1491 Strongly Disagree
    2. 'Competition' - 5329 Strongly Agree
    3. 'Negative Self-Evaluation' - 2652 Strongly Agree

#### **Inference**

+ High agreement level for kinship and competition and disagreement on negative self evaluation indicate that most people on Stack Overflow enjoy being the part of the community and the platform provides healthy competition for developers to learn.

### 6.2. Kinship, Competition & Negative Self-Evaluation by Years of Experience

```{r}

by_agreeDisagree1_yearOfExp <- survey_results_public %>%
                       filter(!is.na(AgreeDisagree1)) %>%
                       mutate(YearsCodingNum = parse_number(YearsCoding)) %>%
                       group_by(AgreeDisagree1,YearsCodingNum) %>%
                       summarise(n = n()) %>% 
                       select(AgreeDisagree1, YearsCodingNum, n)

by_agreeDisagree1_yearOfExp$Percent <- (by_agreeDisagree1_yearOfExp$n/sum(by_agreeDisagree1_yearOfExp$n)*100)

p_by_agreeDisagree1_yearOfExp <- hchart(by_agreeDisagree1_yearOfExp, "line", hcaes(x = YearsCodingNum, y = Percent, group = AgreeDisagree1))  %>%
                                    hc_title(text = 'Kinship by Years of Experience') %>%
                                    hc_xAxis(title = list(text = "Years of Experience")) %>% 
                                    hc_yAxis(title = list(text = "Percentage of Agreement Level"))  %>% hc_add_theme(hc_theme_google())

by_agreeDisagree2_yearOfExp <- survey_results_public %>%
                       filter(!is.na(AgreeDisagree2)) %>%
                       mutate(YearsCodingNum = parse_number(YearsCoding)) %>%
                       group_by(AgreeDisagree2,YearsCodingNum) %>%
                       summarise(n = n()) %>% 
                       select(AgreeDisagree2, YearsCodingNum, n)

by_agreeDisagree2_yearOfExp$Percent <- (by_agreeDisagree2_yearOfExp$n/sum(by_agreeDisagree2_yearOfExp$n)*100)

p_by_agreeDisagree2_yearOfExp <- hchart(by_agreeDisagree2_yearOfExp, "line", hcaes(x = YearsCodingNum, y = Percent, group = AgreeDisagree2))  %>%
                                    hc_title(text = 'Competition by Years of Experience') %>%
                                    hc_xAxis(title = list(text = "Years of Experience")) %>% 
                                    hc_yAxis(title = list(text = "Percentage of Agreement Level"))  %>% hc_add_theme(hc_theme_google())


by_agreeDisagree3_yearOfExp <- survey_results_public %>%
                       filter(!is.na(AgreeDisagree3)) %>%
                       mutate(YearsCodingNum = parse_number(YearsCoding)) %>%
                       group_by(AgreeDisagree3,YearsCodingNum) %>%
                       summarise(n = n()) %>% 
                       select(AgreeDisagree3, YearsCodingNum, n)

by_agreeDisagree3_yearOfExp$Percent <- (by_agreeDisagree3_yearOfExp$n/sum(by_agreeDisagree3_yearOfExp$n)*100)

p_by_agreeDisagree3_yearOfExp <- hchart(by_agreeDisagree3_yearOfExp, "line", hcaes(x = YearsCodingNum, y = Percent, group = AgreeDisagree3))  %>%
                                    hc_title(text = 'Self-Evaluation by Years of Experience') %>%
                                    hc_xAxis(title = list(text = "Years of Experience")) %>% 
                                    hc_yAxis(title = list(text = "Percentage of Agreement Level"))  %>% hc_add_theme(hc_theme_google())
                                    
lst <- list(
  p_by_agreeDisagree1_yearOfExp,
  p_by_agreeDisagree2_yearOfExp,
  p_by_agreeDisagree3_yearOfExp
)

hw_grid(lst, rowheight = 400)

```

#### **Insight**

+ This plot gives the number of repondents who answered the three questions in the above section on the basis of their Years of Experience.
+ It can be observed that the highest number of respondents for each question are having 3 years of experience.
+ 'Kinship' -  has clear 'Agree' responses from all the categories of Years of Experience.
+ 'Competition' - has high 'Agree' reponses till 9 Years of Experience. After that, it shows higher number of 'Disagree' responses.
+ 'Negative Self-Evaluation' - has high 'Disagree' reponses till 27 Years of Experience. After that, it shows higher number of 'Strongly Disagree' response.

#### **Inference**

+ Competition : Most people with 9 years or less experience agree that they are competing with their peers whereas most people with more than 9 years of experience disagree. 
+ Negative Self Evaluation : 
    + People with zero years of experience have a neutral stance on Self Evaluation. 
    + Till 24 years of experience people disagree on on Negative Self Evaluation but the trend is degrading i.e. Number of Agree & Disagree are coming closer as the years increase.
    + After 24 years of experience people agree that they are not as good at programming as their peers. The reason for such reponse is the fact that over the past 30 years the industry landscape has changed drastically and it requires consistent learning to keep up with the latest trends.


<hr>
# B. Technology Analysis
<hr>
This section deals with the analysis of the primary purpose of Stack Overflow - the technology and discussions. During the survey, the respondents were asked various questions regarding the technologies that they work on. 
This section delivers all the related insights gathered during the EDA which includes Programming Languages, Databases, Software Dev Platforms, Frameworks and IDEs. 

## 1. What are the Most Popular Programming Languages?

### 1.1. Most & Least Popular Programming Languages

```{r}

by_LanguageWorkedWith <-   survey_results_public %>% 
                        select(Respondent,LanguageWorkedWith) %>% 
                        mutate(LanguageWorkedWith = strsplit(as.character(LanguageWorkedWith), ";")) %>% 
                        unnest(LanguageWorkedWith) %>%
                        filter(!is.na(LanguageWorkedWith)) %>%
                        group_by(LanguageWorkedWith) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(LanguageWorkedWith = reorder(LanguageWorkedWith,Total)) %>%
                        mutate(Percent=round(Total/nrow(survey_results_public)*100)) %>%
                        head(10)

p_by_LanguageWorkedWith <- highchart() %>%
                              hc_xAxis(categories = by_LanguageWorkedWith$LanguageWorkedWith) %>% 
                              hc_add_series(name = "Percent %", data = by_LanguageWorkedWith$Percent, colorByPoint =  1) %>% 
                              hc_title(text = "Most Popular Programming Language")  %>%
                              hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1))  %>% hc_add_theme(hc_theme_google())

by_LanguageWorkedWith <-   survey_results_public %>% 
                        select(Respondent,LanguageWorkedWith) %>% 
                        mutate(LanguageWorkedWith = strsplit(as.character(LanguageWorkedWith), ";")) %>% 
                        unnest(LanguageWorkedWith) %>%
                        filter(!is.na(LanguageWorkedWith)) %>%
                        group_by(LanguageWorkedWith) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(LanguageWorkedWith = reorder(LanguageWorkedWith,Total)) %>%
                        mutate(Percent=(Total/nrow(survey_results_public)*100)) %>%
                        tail(10)

p2_by_LanguageWorkedWith <- highchart() %>%
                              hc_xAxis(categories = by_LanguageWorkedWith$LanguageWorkedWith) %>% 
                              hc_add_series(name = "Percent %", data = by_LanguageWorkedWith$Percent, colorByPoint =  1) %>% 
                              hc_title(text = "Less Popular Programming Language")  %>%
                              hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1))  %>% hc_add_theme(hc_theme_google())

lst <- list(
  p_by_LanguageWorkedWith,
  p2_by_LanguageWorkedWith
)

hw_grid(lst, rowheight = 400)

```

#### **Insight**

+ Top 5 Most popular programming languages are:
    1. JavaScript 55%
    2. HTML 54%
    3. CSS 52%
    4. SQL 45%
    5. Java 36%
+ Top 5 Least popular programming languages are:
    1. Hack 0.25%
    2. Julia 0.43%
    3. Ocaml 0.47%
    4. Cobol 0.59%
    5. Erlang 0.89%

#### **Inference**

+ Web Development languages are most commonly discussed on Stack Overflow. 

### 1.2. Top Programming Languages Desired for Next Year

```{r}

by_LanguageDesireNextYear <-   survey_results_public %>% 
                        select(Respondent,LanguageDesireNextYear) %>% 
                        mutate(LanguageDesireNextYear = strsplit(as.character(LanguageDesireNextYear), ";")) %>% 
                        unnest(LanguageDesireNextYear) %>%
                        filter(!is.na(LanguageDesireNextYear)) %>%
                        group_by(LanguageDesireNextYear) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(LanguageDesireNextYear = reorder(LanguageDesireNextYear,Total)) %>%
                        mutate(Percent=round(Total/nrow(survey_results_public)*100)) %>%
                        head(10)

highchart() %>%
  hc_xAxis(categories = by_LanguageDesireNextYear$LanguageDesireNextYear) %>% 
  hc_add_series(name = "Percent %", data = by_LanguageDesireNextYear$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Most Popular Language Desire for Next Year")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ This bar plot illustrates the programming languages that people wish to learn for next year.
+ Top 5 popular languages are:
    1. JavaScript 39%
    2. Python 33%
    3. HTML 32%
    4. CSS 31%
    5. SQL 28%

#### **Inference**

+ Since Web Development Languages are discussed a lot on Stack Overflow, JavaScript being a language choice for next year is understandable since a lot of JS frameworks for frontend and backend are popular these days (Angular, Node, React, Express).
+ Python is another trending programming language.

### 1.3. Same Programming Languages Continued for Next Year


```{r}

LanguageWorkedWith_LanguageDesireNextYear_df <- survey_results_public %>% select(Respondent,LanguageWorkedWith,LanguageDesireNextYear) %>% 
                                                 mutate(LanguageWorkedWith = strsplit(as.character(LanguageWorkedWith), ";")) %>% 
                                                 unnest(LanguageWorkedWith)  %>%
                                                 filter(!is.na(LanguageWorkedWith)) %>%
                                                 mutate(LanguageDesireNextYear = strsplit(as.character(LanguageDesireNextYear), ";")) %>% 
                                                 unnest(LanguageDesireNextYear) %>%
                                                 filter(!is.na(LanguageDesireNextYear))

df2_edges <- LanguageWorkedWith_LanguageDesireNextYear_df %>%
            group_by(LanguageWorkedWith,LanguageDesireNextYear) %>%
            summarise(n=n()) %>% arrange(desc(n))
df2_edges <- as.data.frame(df2_edges)
names(df2_edges) <- c("from","to","weight")

x <- df2_edges %>% filter_(~from==to) %>% head(10)
x$percent <- round(x$weight/nrow(survey_results_public)*100)
p_LanguageWorkedWith_LanguageDesireNextYear_df <- highchart() %>%
  hc_xAxis(categories = x$from) %>% 
  hc_add_series(name = "Percent %", data = x$percent, colorByPoint =  1) %>% 
  hc_title(text = "Same Programming Language Desire for Next Year")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

lst <- list(
  p_LanguageWorkedWith_LanguageDesireNextYear_df,
  p_by_LanguageWorkedWith
)

hw_grid(lst, rowheight = 400)

```

#### **Insight**

+ This bar plot illustrates the percentage of people who want to learn the same language as current one for next year.
    1. JavaScript 34%
    2. HTML 30%
    3. CSS 28%
    4. SQL 26%
    5. Python 21%


## 2. What are the Most Popular Databases?

### 2.1. Most & Least Popular Databases

```{r}

by_DatabaseWorkedWith <-   survey_results_public %>% 
                        select(Respondent,DatabaseWorkedWith) %>% 
                        mutate(DatabaseWorkedWith = strsplit(as.character(DatabaseWorkedWith), ";")) %>% 
                        unnest(DatabaseWorkedWith) %>%
                        filter(!is.na(DatabaseWorkedWith)) %>%
                        group_by(DatabaseWorkedWith) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(DatabaseWorkedWith = reorder(DatabaseWorkedWith,Total)) %>%
                        mutate(Percent=round(Total/nrow(survey_results_public)*100)) %>%
                        head(10)

p_by_DatabaseWorkedWith <- highchart() %>%
                          hc_xAxis(categories = by_DatabaseWorkedWith$DatabaseWorkedWith) %>% 
                          hc_add_series(name = "Percent %", data = by_DatabaseWorkedWith$Percent, colorByPoint =  1) %>% 
                          hc_title(text = "Most Popular Database")  %>%
                          hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

by_DatabaseWorkedWith <-   survey_results_public %>% 
                        select(Respondent,DatabaseWorkedWith) %>% 
                        mutate(DatabaseWorkedWith = strsplit(as.character(DatabaseWorkedWith), ";")) %>% 
                        unnest(DatabaseWorkedWith) %>%
                        filter(!is.na(DatabaseWorkedWith)) %>%
                        group_by(DatabaseWorkedWith) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(DatabaseWorkedWith = reorder(DatabaseWorkedWith,Total)) %>%
                        mutate(Percent=(Total/nrow(survey_results_public)*100)) %>%
                        tail(10)

p2_by_DatabaseWorkedWith <- highchart() %>%
                              hc_xAxis(categories = by_DatabaseWorkedWith$DatabaseWorkedWith) %>% 
                              hc_add_series(name = "Percent %", data = by_DatabaseWorkedWith$Percent, colorByPoint =  1) %>% 
                              hc_title(text = "Less Popular Database")  %>%
                              hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

lst <- list(
  p_by_DatabaseWorkedWith,
  p2_by_DatabaseWorkedWith
)

hw_grid(lst, rowheight = 400)

```

#### **Insight**

+ The first bar plot illustrates the most popular databases that are discussed on Stack Overflow.
    1. MySQL 39%
    2. SQL Server 28%
    3. PostgreSQL 22%
    4. MongoDB 17%
    5. SQLite 13%
+ The second bar plot illustrates the least popular databases that are discussed on Stack Overflow.
    1. Apache HBase 1.11%
    2. Google BigQuery 1.40%
    3. Apache Hive 1.45%
    4. Amazon Redshift 1.48%
    5. Neo4j 1.59%


### 2.2. Top Databases Desired for Next Year

```{r}

by_DatabaseDesireNextYear <-   survey_results_public %>% 
                        select(Respondent,DatabaseDesireNextYear) %>% 
                        mutate(DatabaseDesireNextYear = strsplit(as.character(DatabaseDesireNextYear), ";")) %>% 
                        unnest(DatabaseDesireNextYear) %>%
                        filter(!is.na(DatabaseDesireNextYear)) %>%
                        group_by(DatabaseDesireNextYear) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(DatabaseDesireNextYear = reorder(DatabaseDesireNextYear,Total)) %>%
                        mutate(Percent=round(Total/nrow(survey_results_public)*100)) %>%
                        head(10)

highchart() %>%
  hc_xAxis(categories = by_DatabaseDesireNextYear$DatabaseDesireNextYear) %>% 
  hc_add_series(name = "Percent %", data = by_DatabaseDesireNextYear$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Most Popular Database Desire for Next Year")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ This bar plot illustrates the most popular database that people wish to learn for next year.
    1. MySQL 22%
    2. MongoDB 21%
    3. PostgreSQL 20%
    4. SQL Server 16%
    5. Redis 14%

#### **Inference**

+ RDBMS (SQL, MySQL) systems are always desirable.
+ Its interesting to see that In-memory database Redis and NoSQL database MongoDB are also being desired to learn. Both are being extensively used in the current tech scenario.

### 2.3. Same Databases Continued for Next Year


```{r}

DatabaseWorkedWith_DatabaseDesireNextYear_df <- survey_results_public %>% select(Respondent,DatabaseWorkedWith,DatabaseDesireNextYear) %>% 
                                                 mutate(DatabaseWorkedWith = strsplit(as.character(DatabaseWorkedWith), ";")) %>% 
                                                 unnest(DatabaseWorkedWith)  %>%
                                                 filter(!is.na(DatabaseWorkedWith)) %>%
                                                 mutate(DatabaseDesireNextYear = strsplit(as.character(DatabaseDesireNextYear), ";")) %>% 
                                                 unnest(DatabaseDesireNextYear) %>%
                                                 filter(!is.na(DatabaseDesireNextYear))

df2_edges <- DatabaseWorkedWith_DatabaseDesireNextYear_df %>%
            group_by(DatabaseWorkedWith,DatabaseDesireNextYear) %>%
            summarise(n=n()) %>% arrange(desc(n))
df2_edges <- as.data.frame(df2_edges)
names(df2_edges) <- c("from","to","weight")

x <- df2_edges %>% filter_(~from==to) %>% head(10)
x$percent <- round(x$weight/nrow(survey_results_public)*100)
p_DatabaseWorkedWith_DatabaseDesireNextYear_df <- highchart() %>%
                      hc_xAxis(categories = x$from) %>% 
                      hc_add_series(name = "Percent %", data = x$percent, colorByPoint =  1) %>% 
                      hc_title(text = "Same Database Desire for Next Year")  %>%
                      hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

lst <- list(
  p_DatabaseWorkedWith_DatabaseDesireNextYear_df,
  p_by_DatabaseWorkedWith
)

hw_grid(lst, rowheight = 400)

```

#### **Insight**

+ This bar plot illustrates the percentage of people who want to learn the same database as current one for next year.
    1. MySQL 19%
    2. SQL Server 14%
    3. PostgreSQL 14%
    4. MongoDB 10%
    5. Redis 8%

#### **Inference**

+ RDBMS (SQL, MySQL) systems are always desirable.
+ Its interesting to see that In-memory database Redis and NoSQL database MongoDB are also being desired to learn. Both are being extensively used in the current tech scenario.

## 3. What are the Most Popular Platforms?

### 3.1. Most & Least Popular Platforms

```{r}

by_PlatformWorkedWith <-   survey_results_public %>% 
                        select(Respondent,PlatformWorkedWith) %>% 
                        mutate(PlatformWorkedWith = strsplit(as.character(PlatformWorkedWith), ";")) %>% 
                        unnest(PlatformWorkedWith) %>%
                        filter(!is.na(PlatformWorkedWith)) %>%
                        group_by(PlatformWorkedWith) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(PlatformWorkedWith = reorder(PlatformWorkedWith,Total)) %>%
                        mutate(Percent=round(Total/nrow(survey_results_public)*100)) %>%
                        head(10)

p_by_PlatformWorkedWith <- highchart() %>%
                  hc_xAxis(categories = by_PlatformWorkedWith$PlatformWorkedWith) %>% 
                  hc_add_series(name = "Percent %", data = by_PlatformWorkedWith$Percent, colorByPoint =  1) %>% 
                  hc_title(text = "Most Popular Platform")  %>%
                  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1))  %>% hc_add_theme(hc_theme_google())

by_PlatformWorkedWith <-   survey_results_public %>% 
                        select(Respondent,PlatformWorkedWith) %>% 
                        mutate(PlatformWorkedWith = strsplit(as.character(PlatformWorkedWith), ";")) %>% 
                        unnest(PlatformWorkedWith) %>%
                        filter(!is.na(PlatformWorkedWith)) %>%
                        group_by(PlatformWorkedWith) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(PlatformWorkedWith = reorder(PlatformWorkedWith,Total)) %>%
                        mutate(Percent=(Total/nrow(survey_results_public)*100)) %>%
                        tail(10)

p2_by_PlatformWorkedWith<- highchart() %>%
                  hc_xAxis(categories = by_PlatformWorkedWith$PlatformWorkedWith) %>% 
                  hc_add_series(name = "Percent %", data = by_PlatformWorkedWith$Percent, colorByPoint =  1) %>% 
                  hc_title(text = "Least Popular Platform")  %>%
                  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

lst <- list(
  p_by_PlatformWorkedWith,
  p2_by_PlatformWorkedWith
)

hw_grid(lst, rowheight = 400)

```

#### **Insight**

+ The first bar plot illustrates the most popular platforms that are discussed on Stack Overflow.
    1. Linux 32%
    2. Windows Desktop or Server 24%
    3. Android 19%
    4. AWS 16%
    5. Mac OS 12%
+ The second bar plot illustrates the least popular platforms that are discussed on Stack Overflow.
    1. Predix 0.11%
    2. Mainframe 0.53%
    3. Gaming Console 0.85%
    4. Google Home 0.90%
    5. IBM Cloud or Google Watson 0.96%

### 3.2. Top Platforms Desired for Next Year

```{r}

by_PlatformDesireNextYear <-   survey_results_public %>% 
                        select(Respondent,PlatformDesireNextYear) %>% 
                        mutate(PlatformDesireNextYear = strsplit(as.character(PlatformDesireNextYear), ";")) %>% 
                        unnest(PlatformDesireNextYear) %>%
                        filter(!is.na(PlatformDesireNextYear)) %>%
                        group_by(PlatformDesireNextYear) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(PlatformDesireNextYear = reorder(PlatformDesireNextYear,Total)) %>%
                        mutate(Percent=round(Total/nrow(survey_results_public)*100)) %>%
                        head(10)

highchart() %>%
  hc_xAxis(categories = by_PlatformDesireNextYear$PlatformDesireNextYear) %>% 
  hc_add_series(name = "Percent %", data = by_PlatformDesireNextYear$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Most Popular Platform Desire for Next Year")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ This bar plot illustrates the most popular platform that people wish to learn for next year.
    1. Linux 30%
    2. Android 22%
    3. AWS 19%
    4. Raspberry Pi 16%
    5. Windows Desktop or Server 16%

#### **Inference**

+ Linux & Android are the most desired platform for learning.

### 3.3. Same Platforms Continued for Next Year


```{r}

PlatformWorkedWith_PlatformDesireNextYear_df <- survey_results_public %>% select(Respondent,PlatformWorkedWith,PlatformDesireNextYear) %>% 
                                                 mutate(PlatformWorkedWith = strsplit(as.character(PlatformWorkedWith), ";")) %>% 
                                                 unnest(PlatformWorkedWith)  %>%
                                                 filter(!is.na(PlatformWorkedWith)) %>%
                                                 mutate(PlatformDesireNextYear = strsplit(as.character(PlatformDesireNextYear), ";")) %>% 
                                                 unnest(PlatformDesireNextYear) %>%
                                                 filter(!is.na(PlatformDesireNextYear))

df2_edges <- PlatformWorkedWith_PlatformDesireNextYear_df %>%
            group_by(PlatformWorkedWith,PlatformDesireNextYear) %>%
            summarise(n=n()) %>% arrange(desc(n))
df2_edges <- as.data.frame(df2_edges)
names(df2_edges) <- c("from","to","weight")

x <- df2_edges %>% filter_(~from==to) %>% head(10)
x$percent <- round(x$weight/nrow(survey_results_public)*100)
p_PlatformWorkedWith_PlatformDesireNextYear_df<-highchart() %>%
                  hc_xAxis(categories = x$from) %>% 
                  hc_add_series(name = "Percent %", data = x$percent, colorByPoint =  1) %>% 
                  hc_title(text = "Same Platform Desire for Next Year")  %>%
                  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())
                  
  
lst <- list(
  p_PlatformWorkedWith_PlatformDesireNextYear_df,
  p_by_PlatformWorkedWith
)

hw_grid(lst, rowheight = 400)

```

#### **Insight**

+ This bar plot illustrates the percentage of people who want to learn the same platform as current one for next year.
    1. Linux 25%
    2. Windows Desktop or Server 14%
    3. Android 12%
    4. AWS 11%
    5. Mac OS 8% 

## 4. What are the Most Popular Frameworks?

### 4.1. Most & Least Popular Frameworks

```{r}

by_FrameworkWorkedWith <-   survey_results_public %>% 
                        select(Respondent,FrameworkWorkedWith) %>% 
                        mutate(FrameworkWorkedWith = strsplit(as.character(FrameworkWorkedWith), ";")) %>% 
                        unnest(FrameworkWorkedWith) %>%
                        filter(!is.na(FrameworkWorkedWith)) %>%
                        group_by(FrameworkWorkedWith) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(FrameworkWorkedWith = reorder(FrameworkWorkedWith,Total)) %>%
                        mutate(Percent=round(Total/nrow(survey_results_public)*100)) %>%
                        head(10)
p_by_FrameworkWorkedWith <- highchart() %>%
                              hc_xAxis(categories = by_FrameworkWorkedWith$FrameworkWorkedWith) %>% 
                              hc_add_series(name = "Percent %", data = by_FrameworkWorkedWith$Percent, colorByPoint =  1) %>% 
                              hc_title(text = "Most Popular Framework")  %>%
                              hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

by_FrameworkWorkedWith <-   survey_results_public %>% 
                        select(Respondent,FrameworkWorkedWith) %>% 
                        mutate(FrameworkWorkedWith = strsplit(as.character(FrameworkWorkedWith), ";")) %>% 
                        unnest(FrameworkWorkedWith) %>%
                        filter(!is.na(FrameworkWorkedWith)) %>%
                        group_by(FrameworkWorkedWith) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(FrameworkWorkedWith = reorder(FrameworkWorkedWith,Total)) %>%
                        mutate(Percent=(Total/nrow(survey_results_public)*100)) %>%
                        tail(10)

p2_by_FrameworkWorkedWith <- highchart() %>%
                          hc_xAxis(categories = by_FrameworkWorkedWith$FrameworkWorkedWith) %>% 
                          hc_add_series(name = "Percent %", data = by_FrameworkWorkedWith$Percent, colorByPoint =  1) %>% 
                          hc_title(text = "Least Popular Framework")  %>%
                          hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

lst <- list(
  p_by_FrameworkWorkedWith,
  p2_by_FrameworkWorkedWith
)

hw_grid(lst, rowheight = 400)

```

#### **Insight**

+ The first bar plot illustrates the most popular frameworks that are discussed on Stack Overflow.
    1. Node.js 26%
    2. Angular 19%
    3. React 15%
    4. .NET Core 14%
    5. Spring 9%
+ The second bar plot illustrates the least popular frameworks that are discussed on Stack Overflow.
    1. Torch/Py Torch 0.87%
    2. Hadoop 2.47%
    3. Spark 2.50%
    4. Xamarin 3.83%
    5. Tensor Flow 4.07%

#### **Inference**

+ JavaScript frameworks (Node, Angular, React) are very popular currently.
+ New & Complex framworks like Tensor FLow, Spark, Hadoop etc. are less popular.

### 4.2. Top Frameworks Desired for Next Year

```{r}

by_FrameworkDesireNextYear <-   survey_results_public %>% 
                        select(Respondent,FrameworkDesireNextYear) %>% 
                        mutate(FrameworkDesireNextYear = strsplit(as.character(FrameworkDesireNextYear), ";")) %>% 
                        unnest(FrameworkDesireNextYear) %>%
                        filter(!is.na(FrameworkDesireNextYear)) %>%
                        group_by(FrameworkDesireNextYear) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(FrameworkDesireNextYear = reorder(FrameworkDesireNextYear,Total)) %>%
                        mutate(Percent=round(Total/nrow(survey_results_public)*100)) %>%
                        head(10)

highchart() %>%
  hc_xAxis(categories = by_FrameworkDesireNextYear$FrameworkDesireNextYear) %>% 
  hc_add_series(name = "Percent %", data = by_FrameworkDesireNextYear$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Most Popular Framework Desire for Next Year")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1))  %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ This bar plot illustrates the most popular frameworks that people wish to learn for next year.
    1. Node.js 28%
    2. React 24%
    3. Angular 19%
    4. .NET Core 15%
    5. TensorFlow 15%


#### **Inference**

+ JavaScript frameworks are post popular for learning next year. Machine Learning and AI specialists are aiming to learn TensorFlow. 

### 4.3. Same Frameworks Continued for Next Year


```{r}

FrameworkWorkedWith_FrameworkDesireNextYear_df <- survey_results_public %>% select(Respondent,FrameworkWorkedWith,FrameworkDesireNextYear) %>% 
                                                 mutate(FrameworkWorkedWith = strsplit(as.character(FrameworkWorkedWith), ";")) %>% 
                                                 unnest(FrameworkWorkedWith)  %>%
                                                 filter(!is.na(FrameworkWorkedWith)) %>%
                                                 mutate(FrameworkDesireNextYear = strsplit(as.character(FrameworkDesireNextYear), ";")) %>% 
                                                 unnest(FrameworkDesireNextYear) %>%
                                                 filter(!is.na(FrameworkDesireNextYear))

df2_edges <- FrameworkWorkedWith_FrameworkDesireNextYear_df %>%
            group_by(FrameworkWorkedWith,FrameworkDesireNextYear) %>%
            summarise(n=n()) %>% arrange(desc(n))
df2_edges <- as.data.frame(df2_edges)
names(df2_edges) <- c("from","to","weight")

x <- df2_edges %>% filter_(~from==to) %>% head(10)
x$percent <- round(x$weight/nrow(survey_results_public)*100)
p_FrameworkWorkedWith_FrameworkDesireNextYear_df <- highchart() %>%
  hc_xAxis(categories = x$from) %>% 
  hc_add_series(name = "Percent %", data = x$percent, colorByPoint =  1) %>% 
  hc_title(text = "Same Framework Desire for Next Year")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())
  
lst <- list(
  p_FrameworkWorkedWith_FrameworkDesireNextYear_df,
  p_by_FrameworkWorkedWith
)

hw_grid(lst, rowheight = 400)

```

#### **Insight**

+ This bar plot illustrates the percentage of people who want to learn the same framework as current one for next year.
    1. Node.js 17%
    2. Angular 11%
    3. React 10%
    4. .NET Core 9%
    5. Spring 6%

## 5. What are the Most Popular Development Environments?

### 5.1. Most & Least Popular IDEs

```{r}

by_IDE <-   survey_results_public %>% 
                        select(Respondent,IDE) %>% 
                        mutate(IDE = strsplit(as.character(IDE), ";")) %>% 
                        unnest(IDE) %>%
                        filter(!is.na(IDE)) %>%
                        group_by(IDE) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(IDE = reorder(IDE,Total)) %>%
                        mutate(Percent=round(Total/nrow(survey_results_public)*100)) %>%
                        head(10)
p_by_IDE <- highchart() %>%
              hc_xAxis(categories = by_IDE$IDE) %>% 
              hc_add_series(name = "Percent %", data = by_IDE$Percent, colorByPoint =  1) %>% 
              hc_title(text = "Most Popular IDE")  %>%
              hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

by_IDE <-   survey_results_public %>% 
                        select(Respondent,IDE) %>% 
                        mutate(IDE = strsplit(as.character(IDE), ";")) %>% 
                        unnest(IDE) %>%
                        filter(!is.na(IDE)) %>%
                        group_by(IDE) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(IDE = reorder(IDE,Total)) %>%
                        mutate(Percent=(Total/nrow(survey_results_public)*100)) %>%
                        tail(10)

p2_by_IDE <- highchart() %>%
              hc_xAxis(categories = by_IDE$IDE) %>% 
              hc_add_series(name = "Percent %", data = by_IDE$Percent, colorByPoint =  1) %>% 
              hc_title(text = "Least Popular IDE")  %>%
              hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())
  
lst <- list(
  p_by_IDE,
  p2_by_IDE
)

hw_grid(lst, rowheight = 400)

```

#### **Insight**

+ The first bar plot illustrates the most popular IDE that are discussed on Stack Overflow.
    1. Visual Studio Code 27%
    2. Visual Studio 26%
    3. Notepad ++ 26%
    4. Sublime Text 22%
    5. Vim 20%
+ The second bar plot illustrates the least popular IDE that are discussed on Stack Overflow.
    1. Light Table 0.14%
    2. Zend 0.30%
    3. Komodo 0.43%
    4. Coda 0.49%
    5. TextMate 0.81%


### 5.2. Network Analysis : IDEs

```{r}

df <- survey_results_public %>% select(Respondent,IDE)
df2 <- df %>% 
         mutate(IDE = strsplit(as.character(IDE), ";")) %>% 
         unnest(IDE)
         
df2_edges <- df2 %>% group_by(Respondent) %>%
             filter(n()>=2) %>%
             do(data.frame(t(combn((.)[["IDE"]], 2)), stringsAsFactors=FALSE)) %>% ungroup() %>%
             rename(source = X1, target = X2) %>%
             select(-Respondent)

df2_edges <- df2_edges %>% group_by(source,target) %>% summarise(weight=n())


names(df2_edges) <- c("from","to","weight")
df2_edges$weight <- df2_edges$weight/800

df2_edges$width <- 1+df2_edges$weight # line width
#df2_edges$color <- "gray"    # line color  
#df2_edges$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
df2_edges$smooth <- FALSE    # should the edges be curved?
df2_edges$shadow <- FALSE    # edge shadow


df2_nodes <- df2 %>% filter(!is.na(IDE)) %>% group_by(IDE) %>% summarise(n = n()/500) %>% arrange(desc(n))
names(df2_nodes) <- c("id","size")

n <- nrow(df2_nodes)
palette <- distinctColorPalette(n)

df2_nodes$shape <- "dot"  
df2_nodes$shadow <- TRUE # Nodes will drop shadow
df2_nodes$title <- df2_nodes$id # Text on click
df2_nodes$label <- df2_nodes$id # Node label
df2_nodes$size <- df2_nodes$size # Node size
df2_nodes$borderWidth <- 2 # Node border width

df2_nodes$color.background <- palette[]
df2_nodes$color.border <- "black"
df2_nodes$color.highlight.background <- "orange"
df2_nodes$color.highlight.border <- "darkred"

df2_nodes$font.size <- 40

visNetwork(df2_nodes, df2_edges, height = "500px", width = "100%") %>% visIgraphLayout(layout = "layout_with_lgl") %>% 
  visEdges(shadow = TRUE,
           color = list(color = "gray", highlight = "orange"))

```

#### **Insight**

+ This is a Network plot for the different IDEs.
+ Each node denotes the different IDEs and the size of node denotes the number of respondents that use the particular IDE.
+ Each connecting edge between any two nodes denotes that the respondents chose both the IDEs. And, the width of the edge denotes the number of users that chose both the IDEs.
+ It can be observed that the top 3 highest correlation is between:
    1. Notepad++ --- Visual Studio
    2. Visual Studio --- Visual Studio Code
    3. Visual Studio Code --- Notepad++
    4. Eclipse --- Notepad++
    5. Android Studio --- Notepad++

## 6. Other Technology Related Statistics

### 6.1. Most Popular Operating Systems

```{r}

by_OperatingSystem <- survey_results_public %>%
                        filter(!is.na(OperatingSystem)) %>%
                        group_by(OperatingSystem) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(Student = reorder(OperatingSystem,Total)) %>%
                        mutate(Percent = (Total/nrow(survey_results_public)*100)) %>%
                        head(5)

highchart() %>%
  hc_xAxis(categories = by_OperatingSystem$OperatingSystem) %>% 
  hc_add_series(name = "Percent %", data = by_OperatingSystem$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Primary Operating System")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ This bar plot illustrates the most popular Operating System used by the respondents.
    1. Windows 38.46%
    2. MacOS 20.56%
    3. Linux-based 17.88%
    4. BSD/Unix 0.14%

### 6.2. Number of Monitors used at Workstation

```{r}

by_NumberMonitors <- survey_results_public %>%
                        filter(!is.na(NumberMonitors)) %>%
                        group_by(NumberMonitors) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(Student = reorder(NumberMonitors,Total)) %>%
                        mutate(Percent = (Total/sum(Total)*100)) %>%
                        head(10)

highchart() %>%
  hc_xAxis(categories = by_NumberMonitors$NumberMonitors) %>% 
  hc_add_series(name = "Percent %", data = by_NumberMonitors$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Number of Monitors at workstation")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ This bar plot illustrates the number of monitors at workstation used by the respondents.
    1. 2 Monitors 51.13%
    2. 1 Monitors 31.88%
    3. 3 Monitors 14.37%
    4. More than 4 Monitors 1.42%
    5. 4 Monitors 1.18%

### 6.3. Most Popular Version Controls

```{r}

by_VersionControl <-   survey_results_public %>% 
                        select(Respondent,VersionControl) %>% 
                        mutate(VersionControl = strsplit(as.character(VersionControl), ";")) %>% 
                        unnest(VersionControl) %>%
                        filter(!is.na(VersionControl)) %>%
                        group_by(VersionControl) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(VersionControl = reorder(VersionControl,Total)) %>%
                        mutate(Percent=round(Total/nrow(survey_results_public)*100)) %>%
                        head(10)

highchart() %>%
  hc_xAxis(categories = by_VersionControl$VersionControl) %>% 
  hc_add_series(name = "Percent %", data = by_VersionControl$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Popular Version Controls")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ This bar plot illustrates the most popular Version Control Software used by the respondents.
    1. Git 66%
    2. Subversion 12%
    3. Team Foundation Version Control 8%
    4. Zip File Backups 6%
    5. Copy/Paste Network Sharing 6%

### 6.4. Version Control By CheckInCode

```{r}

survey_results_public2 <- survey_results_public %>% filter(!is.na(VersionControl)) %>%
                          mutate(VersionControl = strsplit(as.character(VersionControl), ";"))  %>%
                          unnest(VersionControl)


df1 <- survey_results_public2 %>% filter(!is.na(VersionControl)) %>%
  group_by(name = VersionControl, drilldown = tolower(VersionControl)) %>% 
  summarise(y = n()) %>% arrange(desc(y)) %>% head(10)


df2 <-survey_results_public2 %>% filter(!is.na(VersionControl)) %>% filter(!is.na(CheckInCode)) %>%  group_by(VersionControl,CheckInCode) %>% dplyr::mutate(y = n()) %>%arrange(desc(y))%>%
  group_by(name = VersionControl, id = tolower(VersionControl)) %>% 
  do(data = list_parse(
                  mutate(.,name = CheckInCode, drilldown = tolower(paste(VersionControl,CheckInCode,sep=": "))) %>% 
                      group_by(name,drilldown) %>% 
                        summarise(y=n())%>% dplyr::select(name, y, drilldown)   %>%
                            arrange(desc(y))) %>% head(10)
    )

highchart() %>% 
  hc_chart(type = "column") %>%
  hc_title(text = 'Version Control By CheckInCode') %>%
  hc_add_series(data = df1, name = "VersionControl",colorByPoint =  1) %>% 
  hc_legend(enabled = FALSE) %>%
  hc_xAxis(type = "category") %>% 
  hc_yAxis(type = 'logarithmic',title = list(text = "Total Response"))  %>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list_parse(df2)
  ) %>% hc_add_theme(hc_theme_google())

```

### 6.5. Most Popular Communication Tools

```{r}
by_CommunicationTools <-   survey_results_public %>% 
                        select(Respondent,CommunicationTools) %>% 
                        mutate(CommunicationTools = strsplit(as.character(CommunicationTools), ";")) %>% 
                        unnest(CommunicationTools) %>%
                        filter(!is.na(CommunicationTools)) %>%
                        group_by(CommunicationTools) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(CommunicationTools = reorder(CommunicationTools,Total)) %>%
                        mutate(Percent=round(Total/nrow(survey_results_public)*100)) %>%
                        head(15)

highchart() %>%
  hc_xAxis(categories = by_CommunicationTools$CommunicationTools) %>% 
  hc_add_series(name = "Percent %", data = by_CommunicationTools$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Popular Communication Tools")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ This bar plot illustrates the most popular Version Control Software used by the respondents.
    1. Slack 30%
    2. Jira 24%
    3. Office Suites 23%
    4. Other Wiki Tools 18%
    5. Confluence 17%


### 6.6. Most Popular Software Development Methodology

```{r}

by_Methodology <-   survey_results_public %>% 
                        select(Respondent,Methodology) %>% 
                        mutate(Methodology = strsplit(as.character(Methodology), ";")) %>% 
                        unnest(Methodology) %>%
                        filter(!is.na(Methodology)) %>%
                        group_by(Methodology) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(Methodology = reorder(Methodology,Total)) %>%
                        mutate(Percent=round(Total/nrow(survey_results_public)*100)) %>%
                        head(15)

highchart() %>%
  hc_xAxis(categories = by_Methodology$Methodology) %>% 
  hc_add_series(name = "Percent %", data = by_Methodology$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Popular Methodologies")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())


```

#### **Insight**

+ This bar plot illustrates the most popular Software Development Methodology used by the respondents.
    1. Agile 51%
    2. Scrum 37%
    3. Kanban 21%
    4. Pair Programming 17%
    5. Extreme Programming 9%

#### **Inference**

+ Agile methodology and its related frameworks are the most popular software development methodologies currently.

## 7. What do Developers think about Advertisements & AdBlockers?

### 7.1. Do you have Adblocker?

```{r}

by_AdBlocker <- survey_results_public %>%
                        filter(!is.na(AdBlocker)) %>%
                        group_by(AdBlocker) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(AdBlocker = reorder(AdBlocker,Total)) %>%
                        mutate(Percent = round(Total/sum(Total)*100)) %>%
                        head(10)

highchart() %>%
  hc_xAxis(categories = by_AdBlocker$AdBlocker) %>% 
  hc_add_series(name = "Percent %", data = by_AdBlocker$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Do you have Adblocker?")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ Having a tech savvy user base, atleast 72% respondents use AdBlocker.

#### 7.1.1. AdBlocker usage by Country

```{r}

survey_results_public2 <- survey_results_public %>% filter(!is.na(Country)) %>% group_by(Country) %>% summarise(count=n()) %>% right_join(survey_results_public,by="Country")


df1 <- survey_results_public2 %>% filter(!is.na(AdBlocker)) %>%
  group_by(name = AdBlocker, drilldown = tolower(AdBlocker)) %>% 
  summarise(Total = n()) %>% ungroup() %>% mutate(y = (Total/sum(Total))*100) %>% arrange(desc(y))


df2 <-survey_results_public2 %>% filter(!is.na(AdBlocker)) %>% filter(!is.na(Country)) %>%  group_by(AdBlocker,Country) %>% dplyr::mutate(y = n()) %>%arrange(desc(y))%>%
  group_by(name = AdBlocker, id = tolower(AdBlocker)) %>% 
  do(data = list_parse(
                  mutate(.,name = Country, count=as.numeric(count) ,drilldown = tolower(paste(AdBlocker,Country,sep=": "))) %>% 
                      group_by(name,drilldown,count) %>% 
                        summarise(y=n()) %>% ungroup() %>% filter(y>=100) %>% mutate(y = (y/count)*100)%>% dplyr::select(name, y, drilldown)   %>%
                            arrange(desc(y))) %>% head(10) 
    )

highchart() %>% 
  hc_chart(type = "bar") %>%
  hc_title(text = 'AdBlocker By Country') %>%
  hc_add_series(data = df1, name = "have AdBlocker",colorByPoint =  1) %>% 
  hc_legend(enabled = FALSE) %>%
  hc_xAxis(type = "category") %>% 
  hc_yAxis(title = list(text = "Percentage %"))  %>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list_parse(df2)
  ) %>% hc_add_theme(hc_theme_google())
```

#### **Insight**
+ Note : This is a drill down chart.
+ Highest AdBlocker using Countries:
    + **67%** of `France respondents` have AdBlocker.
    + **65%** of `Belgium respondents` have AdBlocker.
    + **65%** of `Germany respondents` have AdBlocker.

+ Lowest AbBlocker using Countries:
    + **33%** of `Nigeria respondents` not have AdBlocker.
    + **25%** of `Iran respondents` not have AdBlocker.
    + **25%** of `Pakistan respondents` not have AdBlocker.


#### 7.1.2. AdBlocker usage by Gender 

```{r}

survey_results_public %>%  
  filter(!is.na(AdBlocker)) %>% 
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  group_by(AdBlocker,Gender) %>% 
  count() %>% 
  hchart('column',hcaes('AdBlocker','n',group = 'Gender')) %>% 
  hc_colors(c("red", "blue")) %>% 
  hc_title(text = 'AdBlocker by Gender ') %>% 
  hc_yAxis(type = 'logarithmic', title = list(text = "Count") ) %>%
  hc_xAxis(title = list(text = "has Adblocker") )

```

#### **Insight**

+ 13128 Male respondents do not uses AdBlocker.
+ 43789 Male respondents uses AdBlocker.
+ 996 Female respondents do not uses AdBlocker.
+ 2654 Female respondents uses AdBlocker.

### 7.2. Do you disable AdBlocker?

```{r}

by_AdBlockerDisable <- survey_results_public %>%
                        filter(!is.na(AdBlockerDisable)) %>%
                        group_by(AdBlockerDisable) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(AdBlockerDisable = reorder(AdBlockerDisable,Total)) %>%
                        mutate(Percent = round(Total/sum(Total)*100)) %>%
                        head(10)

highchart() %>%
  hc_xAxis(categories = by_AdBlockerDisable$AdBlockerDisable) %>% 
  hc_add_series(name = "Percent %", data = by_AdBlockerDisable$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Do you disable Adblocker")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ 71% users have to disable AdBlocker.

### 7.3. What is the reason for disabling AdBlocker?

```{r}

by_AdBlockerReasons <- survey_results_public %>%
                        filter(!is.na(AdBlockerReasons)) %>%
                        mutate(AdBlockerReasons = str_split(AdBlockerReasons, pattern = ";")) %>%
                        unnest(AdBlockerReasons) %>%
                        group_by(AdBlockerReasons) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(AdBlockerReasons = reorder(AdBlockerReasons,Total)) %>%
                        mutate(Percent = round(Total/nrow(survey_results_public)*100)) %>%
                        head(10)

highchart() %>%
  hc_xAxis(categories = by_AdBlockerReasons$AdBlockerReasons) %>% 
  hc_add_series(name = "Percent %", data = by_AdBlockerReasons$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Reasons of Disable Adblocker")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ The most common reasons for disabling AdBlocker:
    1. Website forced to disable Adblocker - 22%
    2. Wanted to support website by viewing ads - 17%
    3. Display issues caused by Adblocker - 15%

### 7.4. Do you agree that online advertising can be valuable when it is relevant to you?

```{r}

by_AdsAgreeDisagree1 <- survey_results_public %>%
                        filter(!is.na(AdsAgreeDisagree1)) %>%
                        group_by(AdsAgreeDisagree1) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(AdsAgreeDisagree1 = reorder(AdsAgreeDisagree1,Total)) %>%
                        mutate(Percent = round(Total/sum(Total)*100)) %>%
                        head(10)

highchart() %>%
  hc_xAxis(categories = by_AdsAgreeDisagree1$AdsAgreeDisagree1) %>% 
  hc_add_series(name = "Percent %", data = by_AdsAgreeDisagree1$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Do you Agree? - Online advertising can be valuable when it is relevant to me ")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ A total of 56% people agree that online ads are valued only when they are relevant.
+ A total of 26% people disagree that online ads are valued only when they are relevant.
+ 18% people hold a neutral stance on online ads.

### 7.5. Do you agree that you enjoy seeing online updates from companies that you like?

```{r}

by_AdsAgreeDisagree2 <- survey_results_public %>%
                        filter(!is.na(AdsAgreeDisagree2)) %>%
                        group_by(AdsAgreeDisagree2) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(AdsAgreeDisagree2 = reorder(AdsAgreeDisagree2,Total)) %>%
                        mutate(Percent = round(Total/sum(Total)*100)) %>%
                        head(10)

highchart() %>%
  hc_xAxis(categories = by_AdsAgreeDisagree2$AdsAgreeDisagree2) %>% 
  hc_add_series(name = "Percent %", data = by_AdsAgreeDisagree2$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Do you Agree? - I enjoy seeing online updates from companies that I like")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ A total of 53% people agree that seeing online ads from companies that they like is enjoyable.
+ A total of 23% people disagree that seeing online ads from companies that they like is enjoyable.
+ 24% people hold a neutral stance on online ads.

#### **Inference**

+ From previous two sections it can be determined that people dont mind viewing relevant ads from the companies that they like.

### 7.6. Do you agree that you fundamentally dislike the concept of advertising?

```{r}

by_AdsAgreeDisagree3 <- survey_results_public %>%
                        filter(!is.na(AdsAgreeDisagree3)) %>%
                        group_by(AdsAgreeDisagree3) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(AdsAgreeDisagree3 = reorder(AdsAgreeDisagree3,Total)) %>%
                        mutate(Percent = round(Total/sum(Total)*100)) %>%
                        head(10)

highchart() %>%
  hc_xAxis(categories = by_AdsAgreeDisagree3$AdsAgreeDisagree3) %>% 
  hc_add_series(name = "Percent %", data = by_AdsAgreeDisagree3$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Do you Agree? - I fundamentally dislike the concept of advertising")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ A total of 41% people agree that they fundamentally dislike the concept of advertising.
+ A total of 29% people disagree that they fundamentally dislike the concept of advertising.
+ 30% people hold a neutral stance on online ads.

#### **Inference**

+ While roughly 1/3 people hold a neutral stance on the concept of ads, 41% people dislike the concept of ads and 29% like the concept of ads.
+ Ads tend to affect the aesthetics and the user experience of websites. As long as that doesn't happen, people won't mind ads.

## 8. What do Developers think about AI Technology?

### 8.1. Most Dangerous Aspect of Increasingly Advanced AI Technology

```{r}

by_AIDangerous <- survey_results_public %>%
                        filter(!is.na(AIDangerous)) %>%
                        group_by(AIDangerous) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(AIDangerous = reorder(AIDangerous,Total)) %>%
                        mutate(Percent = round(Total/sum(Total)*100))

          highchart() %>%
                      hc_xAxis(categories = by_AIDangerous$AIDangerous) %>% 
                      hc_add_series(name = "Percent %", data = by_AIDangerous$Percent, colorByPoint =  1) %>% 
                      hc_title(text = "Most Dangerous Aspect of Increasingly Advanced AI Technology")  %>%
                      hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**
+ Most developers find following two aspects of AI dangerous:
    1. "Algorithm making important decisions" - 29%
    2. "Artificial Intelligence surpassing huma intelligence (the singularity)" - 28%


### 8.2. Most Dangerous Aspect by Developer Type

```{r}

survey_results_public2 <-  survey_results_public %>%    mutate(DevType = strsplit(as.character(DevType), ";"))  %>%
                                                        unnest(DevType) %>% 
                                                        filter(!is.na(AIDangerous)) %>% filter(!is.na(DevType)) %>%
                                                        select(DevType, AIDangerous)

survey_results_public2 <- survey_results_public2 %>% filter(!is.na(DevType)) %>% group_by(DevType) %>% summarise(count=n()) %>% right_join(survey_results_public2,by="DevType")


df1 <- survey_results_public2 %>%
       filter(!is.na(AIDangerous)) %>%
       group_by(name = AIDangerous, drilldown = tolower(AIDangerous)) %>% 
       summarise(Total = n()) %>% ungroup() %>% mutate(y = (Total/sum(Total))*100) %>% arrange(desc(y))

df2 <-survey_results_public2 %>% filter(!is.na(AIDangerous)) %>% filter(!is.na(DevType)) %>% group_by(AIDangerous,DevType) %>% dplyr::mutate(y = n()) %>%arrange(desc(y))%>%
  group_by(name = AIDangerous, id = tolower(AIDangerous)) %>% 
  do(data = list_parse(
                  mutate(.,name = DevType, count=as.numeric(count) , drilldown = tolower(paste(AIDangerous,DevType,sep=": "))) %>% 
                      group_by(name,drilldown,count) %>% 
                       summarise(y=n()) %>% ungroup() %>% filter(y>=100) %>% mutate(y = (y/count)*100)%>% dplyr::select(name, y, drilldown)   %>%
                            arrange(desc(y))) %>% head(10) 
    )
    
highchart() %>% 
  hc_chart(type = "bar") %>%
  hc_title(text = 'Developer wise Opinion on Dangerous Aspect of AI') %>%
  hc_add_series(data = df1, name = "Opinion on Dangerous Aspect",colorByPoint =  1) %>% 
  hc_legend(enabled = FALSE) %>%
  hc_xAxis(type = "category") %>% 
  hc_yAxis(title = list(text = "Percentage %")) %>% 
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series =list_parse(df2)
  ) %>% 
#hc_tooltip(useHTML = TRUE, headerFormat = "",
#                            pointFormat = "Total: {point.y} Percentage: {point.Percentage}") %>%  
hc_add_theme(hc_theme_google())

```

#### **Insight**

+ Note : This is a drill down plot.
+ At upper level the plot represents the number of respondents of each Developer Type who answered the question.
+ After drill down, we see the % of responses from respondents of each developer type.

#### **Inference**

+ Data Scientists & Machine Learning Specialists feel that "Evolving definition of fairness in algorithmic versus human decisions" is the real threat. Since this particular developer type deals with AI and has more knowledge about the field, their concern is valid. They are thinking at the root level of the problem.


### 8.3. Most Exciting Aspect of Increasingly Advanced AI Technology

```{r}

by_AIInteresting <- survey_results_public %>%
                        filter(!is.na(AIInteresting)) %>%
                        group_by(AIInteresting) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(AIInteresting = reorder(AIInteresting,Total)) %>%
                        mutate(Percent = round(Total/sum(Total)*100))

highchart() %>%
  hc_xAxis(categories = by_AIInteresting$AIInteresting) %>% 
  hc_add_series(name = "Percent %", data = by_AIDangerous$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Most Exciting Aspect of Increasingly Advanced AI Technology")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ Most developers find following two aspects of AI most exciting:
    1. "Increasing automation of jobs" - 29%
    2. "Algorithm making important decisions" - 28%


### 8.4. Most Exciting Aspect by Developer Type

```{r}

survey_results_public2 <-  survey_results_public %>%    mutate(DevType = strsplit(as.character(DevType), ";"))  %>%
                                                        unnest(DevType) %>% 
                                                        filter(!is.na(AIInteresting)) %>% filter(!is.na(DevType)) %>%
                                                        select(DevType, AIInteresting)

survey_results_public2 <- survey_results_public2 %>% filter(!is.na(DevType)) %>% group_by(DevType) %>% summarise(count=n()) %>% right_join(survey_results_public2,by="DevType")


df1 <- survey_results_public2 %>%
       filter(!is.na(AIInteresting)) %>%
       group_by(name = AIInteresting, drilldown = tolower(AIInteresting)) %>% 
       summarise(Total = n()) %>% ungroup() %>% mutate(y = (Total/sum(Total))*100) %>% arrange(desc(y))

df2 <-survey_results_public2 %>% filter(!is.na(AIInteresting)) %>% filter(!is.na(DevType)) %>% group_by(AIInteresting,DevType) %>% dplyr::mutate(y = n()) %>%arrange(desc(y))%>%
  group_by(name = AIInteresting, id = tolower(AIInteresting)) %>% 
  do(data = list_parse(
                  mutate(.,name = DevType, count=as.numeric(count) , drilldown = tolower(paste(AIInteresting,DevType,sep=": "))) %>% 
                      group_by(name,drilldown,count) %>% 
                       summarise(y=n()) %>% ungroup() %>% filter(y>=100) %>% mutate(y = (y/count)*100)%>% dplyr::select(name, y, drilldown)   %>%
                            arrange(desc(y))) %>% head(10) 
    )
    
highchart() %>% 
  hc_chart(type = "bar") %>%
  hc_title(text = 'Developer wise Opinion on Exciting Aspect of Increasingly Advanced AI Technology') %>%
  hc_add_series(data = df1, name = "Exciting Aspect",colorByPoint =  1) %>% 
  hc_legend(enabled = FALSE) %>%
  hc_xAxis(type = "category") %>% 
  hc_yAxis(title = list(text = "Percentage %")) %>% 
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series =list_parse(df2)
  ) %>% 
#hc_tooltip(useHTML = TRUE, headerFormat = "",
#                            pointFormat = "Total: {point.y} Percentage: {point.Percentage}") %>%  
hc_add_theme(hc_theme_google())

```

#### **Insight**

+ Note : This is a drill down plot.
+ At upper level the plot represents the number of respondents of each Developer Type who answered the question.
+ After drill down, we see the % of responses from respondents of each developer type.
+ All developer types unanimously agree that "Increasing automation of jobs" is the most exciting feature.

#### **Inference**

+ After analyzing data from this section and previous section, following remarks can be made.
    1. Increasing job automation is safely the most exciting feature of AI and people don't fear it.
    2. While people fear "AI decision making fairness" and "AI surpassing human intelligence", it can be inferred that while these two things are potentially dangerous but people are excited to experience such advances in AI. 

### 8.5. Responsiblity for ramifications of increasing advances in AI

```{r}

by_AIResponsible <- survey_results_public %>%
                        filter(!is.na(AIResponsible)) %>%
                        group_by(AIResponsible) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(AIResponsible = reorder(AIResponsible,Total)) %>%
                        mutate(Percent = round(Total/sum(Total)*100))

highchart() %>%
  hc_xAxis(categories = by_AIResponsible$AIResponsible) %>% 
  hc_add_series(name = "Percent %", data = by_AIResponsible$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Ramifications of Increasingly Advanced AI Technology")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ 48% people agree that the "Developer or the people creating the AI" are responsible for any ramifications regarding advancement in AI technology.
+ 28% people feel that "Government Agencies" should be responsible for AI.

### 8.6. Responsiblity for AI by Developer Type

```{r}

survey_results_public2 <-  survey_results_public %>%    mutate(DevType = strsplit(as.character(DevType), ";"))  %>%
                                                        unnest(DevType) %>% 
                                                        filter(!is.na(AIResponsible)) %>% filter(!is.na(DevType)) %>%
                                                        select(DevType, AIResponsible)

survey_results_public2 <- survey_results_public2 %>% filter(!is.na(DevType)) %>% group_by(DevType) %>% summarise(count=n()) %>% right_join(survey_results_public2,by="DevType")


df1 <- survey_results_public2 %>%
       filter(!is.na(AIResponsible)) %>%
       group_by(name = AIResponsible, drilldown = tolower(AIResponsible)) %>% 
       summarise(Total = n()) %>% ungroup() %>% mutate(y = (Total/sum(Total))*100) %>% arrange(desc(y))

df2 <-survey_results_public2 %>% filter(!is.na(AIResponsible)) %>% filter(!is.na(DevType)) %>% group_by(AIResponsible,DevType) %>% dplyr::mutate(y = n()) %>%arrange(desc(y))%>%
  group_by(name = AIResponsible, id = tolower(AIResponsible)) %>% 
  do(data = list_parse(
                  mutate(.,name = DevType, count=as.numeric(count) , drilldown = tolower(paste(AIResponsible,DevType,sep=": "))) %>% 
                      group_by(name,drilldown,count) %>% 
                       summarise(y=n()) %>% ungroup() %>% filter(y>=100) %>% mutate(y = (y/count)*100)%>% dplyr::select(name, y, drilldown)   %>%
                            arrange(desc(y))) %>% head(10) 
    )
    
highchart() %>% 
  hc_chart(type = "bar") %>%
  hc_title(text = 'Developer wise Opinion on Responsibility for AI Dev') %>%
  hc_add_series(data = df1, name = "Responsibility for AI Dev",colorByPoint =  1) %>% 
  hc_legend(enabled = FALSE) %>%
  hc_xAxis(type = "category") %>% 
  hc_yAxis(title = list(text = "Percentage %")) %>% 
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series =list_parse(df2)
  ) %>% 
#hc_tooltip(useHTML = TRUE, headerFormat = "",
#                            pointFormat = "Total: {point.y} Percentage: {point.Percentage}") %>%  
hc_add_theme(hc_theme_google())

```

+ Note : This is a drill down plot.
+ At upper level the plot represents the % of respondents of each Developer Type who answered the question.
+ After drill down, we see the number of responses from respondents of each developer type.

#### **Inference**

+ It's good that majority of the developers feel that they should be responsible for the ramifications caused by their AI. Such awareness will result in better and responsible AI development.
+ Around 50% Data Scientists & Machine Learning Experts also feel that they are responsible for developing AIs responsibly.

### 8.7. Views on the Future of Artificial Intelligence

```{r}

by_AIFuture <- survey_results_public %>%
                        filter(!is.na(AIFuture)) %>%
                        group_by(AIFuture) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(AIFuture = reorder(AIFuture,Total)) %>%
                        mutate(Percent = round(Total/sum(Total)*100))

highchart() %>%
  hc_xAxis(categories = by_AIFuture$AIFuture) %>% 
  hc_add_series(name = "Percent %", data = by_AIFuture$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Views on the Future of Artificial Intelligence")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())


```

#### **Insight**

+ 73% of all respondents are "Excited about the possibilities rather than being worried about its dangers".


<hr>
# C. Employment & Career Analysis
<hr>

This section deals with the analysis of the Employment Status of the survey respondents.
It focuses on the insights gathered during the EDA for employment with respect to different employement statuses, countries & technologies. 

## 1. What is the Employment status?

### 1.1. Employment Status

```{r}

by_Employment <- survey_results_public %>%
                        filter(!is.na(Employment)) %>%
                        group_by(Employment) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(Employment = reorder(Employment,Total)) %>%
                        mutate(Percent = (Total/sum(Total)*100)) %>%
                        head(10)

highchart() %>%
  hc_xAxis(categories = by_Employment$Employment) %>% 
  hc_add_series(name = "Percent %", data = by_Employment$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Employment Status")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insight**
+ Around 89% of all repondents are employed in some way or another (Fulltime, Parttime, Freelance, Self Employed)
+ Around 11% of all repondents are not employed or retired.


### 1.2. Employment Status by Country


```{r}

survey_results_public2 <- survey_results_public %>% filter(!is.na(Country)) %>% group_by(Country) %>% summarise(count=n()) %>% right_join(survey_results_public,by="Country")


df1 <- survey_results_public2 %>% filter(!is.na(Employment)) %>%
  group_by(name = Employment, drilldown = tolower(Employment)) %>% 
  summarise(Total = n()) %>% ungroup() %>% mutate(y = (Total/sum(Total))*100) %>% arrange(desc(y))


df2 <-survey_results_public2 %>% filter(!is.na(Employment)) %>% filter(!is.na(Country)) %>%  group_by(Employment,Country) %>% dplyr::mutate(y = n()) %>%arrange(desc(y))%>%
  group_by(name = Employment, id = tolower(Employment)) %>% 
  do(data = list_parse(
                  mutate(.,name = Country, count=as.numeric(count) ,drilldown = tolower(paste(Employment,Country,sep=": "))) %>% 
                      group_by(name,drilldown,count) %>% 
                        summarise(y=n()) %>% ungroup() %>% filter(y>=100) %>% mutate(y = (y/count)*100)%>% dplyr::select(name, y, drilldown)   %>%
                            arrange(desc(y))) %>% head(10) 
    )

highchart() %>% 
  hc_chart(type = "bar") %>%
  hc_title(text = 'Employment Status By Country') %>%
  hc_add_series(data = df1, name = "Employment Status",colorByPoint =  1) %>% 
  hc_legend(enabled = FALSE) %>%
  hc_xAxis(type = "category") %>% 
  hc_yAxis(title = list(text = "Percentage %"))  %>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list_parse(df2)
  ) %>% hc_add_theme(hc_theme_google())
  
```

#### **Insight**

+ Countries with Highest Employment %: 
    + Full Time
        1. United Arab Emirates 81%
        2. Hong Kong 81%
        3. Latvia 79%
    + Part Time
        1. Switzerland 16%
        2. Austria 16%
        3. Czech Republic 14%
+ Countries with Highest Unmployment: 
    + Unemployed but looking for Job
        1. Pakistan 14%
        2. Ukraine 11%
        3. India 9%

### 1.3. Employment Status by Gender

```{r}

df1 <- survey_results_public %>% filter(!is.na(Employment)) %>%
  group_by(name = Employment, drilldown = tolower(Employment)) %>% 
  summarise(y = n()) %>% arrange(desc(y)) %>% head(10)
df2 <-survey_results_public %>% filter(!is.na(Employment)) %>% mutate(Gender = strsplit(as.character(Gender), ";"))  %>%
  filter(!is.na(Gender)) %>% unnest(Gender) %>%  group_by(Employment,Gender) %>% dplyr::mutate(y = n()) %>%arrange(desc(y))%>%
  group_by(name = Employment, id = tolower(Employment)) %>% 
  do(data = list_parse(
                  mutate(.,name = Gender, drilldown = tolower(paste(Employment,Gender,sep=": "))) %>% 
                      group_by(name,drilldown) %>% 
                        summarise(y=n())%>% dplyr::select(name, y, drilldown)   %>%
                            arrange(desc(y))) %>% head(10)
    )
highchart() %>% 
  hc_chart(type = "column") %>%
  hc_title(text = 'Employment Status By Gender') %>%
  hc_add_series(data = df1, name = "Employment Status",colorByPoint =  1) %>% 
  hc_legend(enabled = FALSE) %>%
  hc_xAxis(type = "category") %>% 
  hc_yAxis(title = list(text = "Total Response"))  %>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list_parse(df2)
  ) %>% hc_add_theme(hc_theme_google())
  
```

#### **Insight**
+ Genders are employed (fulltime, parttime, self) :
    + Males 53761
    + Females 2843
+ Genders unemployed (unemployed but looking, unemployed and not looking for job):
    + Males 5327
    + Females 492

#### **Inference**
+ 90% Males are employed.
+ 85% females are employed.

## 2. What is the Salary paid for different Technologies?

### 2.1. Developer Salary by Country

```{r}

by_country_salary <- survey_results_public %>% select(Country, Salary) %>% mutate(Salary=as.numeric(Salary))  %>% filter(!is.na(Country)) %>% filter(!is.na(Salary)) %>%group_by(Country) %>% summarize(AvgSalary = median(Salary, na.rm=TRUE))

data(worldgeojson, package = "highcharter")
code <- countrycode(by_country_salary$Country, 'country.name', 'iso3c')
by_country_salary$iso3 <- code
by_country_salary$AvgSalary <- round(by_country_salary$AvgSalary)

highchart() %>% 
  hc_add_series_map(worldgeojson, by_country_salary, value = "AvgSalary", joinBy = "iso3",colorByPoint =  1) %>% 
  hc_colorAxis(stops = color_stops()) %>% 
  hc_legend(enabled = TRUE) %>%  
  hc_mapNavigation(enabled = TRUE) %>%
  hc_title(text = "Avg Salary by Country")  %>%
  hc_tooltip(useHTML = TRUE, headerFormat = "",
            pointFormat = "Country: {point.Country} Median Salary: ${point.AvgSalary}") %>% hc_add_theme(hc_theme_google())
            

```

#### **Insight**

+ This plot illustrates the median salary distribution for the different countries.
+ Top 5 Highest Median Salaries:
    1. South Korea $22000000
    2. Iran $15000000
    3. Vietnam $5500000
    4. Indonesia $4500000
    5. Colombia $1661000

### 2.2. Motion Plot : Developer Type Vs Salary Vs Years of Experience

```{r}

motion_df <- survey_results_public %>% select(DevType,YearsCodingProf,Salary) %>%
            mutate(YearsCodingNum = parse_number(YearsCodingProf),
                                    DevType = str_split(DevType, pattern = ";"),
                                    Salary = as.numeric(Salary)) %>%
                                    unnest(DevType) %>% 
                                    filter(!is.na(DevType)) %>% filter(!is.na(YearsCodingNum))

motion_df2 <- motion_df %>% filter(!is.na(Salary)) %>%
            select(DevType, YearsCodingNum, Salary) %>% 
            filter(!is.na(DevType)) %>% group_by(DevType,YearsCodingNum) %>% summarize(AvgSalary = median(Salary, na.rm=TRUE))


#motion_df$z <- motion_df$AvgSalary

data_strt2 <- motion_df2  %>% 
  mutate(x = YearsCodingNum, y = AvgSalary, z = 100)

data_strt2$color = distinctColorPalette(length(unique(motion_df2$DevType)))[as.numeric(as.factor(motion_df2$DevType))]

data_seqc2 <- motion_df %>% 
  arrange(DevType, YearsCodingNum) %>% 
  group_by(DevType) %>% 
  summarise(n=n()) %>%
  right_join(motion_df2, by="DevType") %>%
  group_by(DevType) %>%
  do(sequence = list_parse(select(., x = YearsCodingNum, y = AvgSalary, z = n)))

data2 <- left_join(data_strt2, data_seqc2)  

highchart() %>% 
  hc_add_series(data = data2, type = "bubble",
                minSize = 0, maxSize = 30, dataLabels = list(enabled = TRUE, format = "{point.DevType}")) %>% 
  hc_motion(enabled = TRUE, series = 0, labels = unique(motion_df2$YearsCodingNum),
            loop = TRUE, 
            updateInterval = 1000, magnet = list(step =  1)) %>% 
  hc_plotOptions(series = list(showInLegend = FALSE)) %>% 
  hc_xAxis(min = 0, max = 30, title = list(text = "Year Of Exp")) %>% 
  hc_yAxis(min = 500, max = 200000, title = list(text = "Median Salary (USD)")) %>% 
  hc_title(text = "Motion Plot of Devtype vs Salary vs Year of Exp")  %>%
  hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = "{point.DevType} Year Of Exp: {point.x}y Median Salary: ${point.y} No Of Response : {point.z}") %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ This is a motion plot of Devtype vs Salary vs Years of Experience.
+ Full Stack developers, Back End developers and Front End developers have the highest number of respondents for most Year of Experience intervals.
+ Engineering Managers have highest salary for most Years of Experience intervals, except for people with 27.5 years of experience.

### 2.3. Developer Salary : Global Vs India Vs USA

```{r}


global_salary <-  survey_results_public %>% select(DevType,Salary) %>%
            mutate(DevType = str_split(DevType, pattern = ";"),
                   Salary = as.numeric(Salary)) %>%
            unnest(DevType) %>% filter(!is.na(Salary)) %>%
            select(DevType,Salary) %>% 
            filter(!is.na(DevType)) %>% group_by(DevType) %>% summarize(AvgSalary = median(Salary, na.rm=TRUE)) %>% arrange(desc(AvgSalary)) %>% head(10)
 
india_salary <-  survey_results_public %>% select(DevType,Salary,Country) %>%
            mutate(DevType = str_split(DevType, pattern = ";"),
                   Salary = as.numeric(Salary)) %>%
            unnest(DevType) %>% filter(!is.na(Salary)) %>%
            filter(Country %in% c("India")) %>% 
            select(DevType,Salary) %>% 
            filter(!is.na(DevType)) %>% group_by(DevType) %>% summarize(AvgSalary = median(Salary, na.rm=TRUE)) %>% arrange(desc(AvgSalary)) %>% head(10)
 
usa_salary <-  survey_results_public %>% select(DevType,Salary,Country) %>%
            mutate(DevType = str_split(DevType, pattern = ";"),
                   Salary = as.numeric(Salary)) %>%
            unnest(DevType) %>% filter(!is.na(Salary)) %>%
            filter(Country %in% c("United States")) %>% 
            select(DevType,Salary) %>% 
            filter(!is.na(DevType)) %>% group_by(DevType) %>% summarize(AvgSalary = median(Salary, na.rm=TRUE)) %>% arrange(desc(AvgSalary)) %>% head(10)
 
p1 <- highchart() %>%
          hc_xAxis(categories = global_salary$DevType) %>% 
          hc_add_series(name = "Median Salary $", data = global_salary$AvgSalary, colorByPoint =  1) %>% 
          hc_title(text = "Global Salary by Developer Type")  %>%
          hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

p2 <- highchart() %>%
          hc_xAxis(categories = india_salary$DevType) %>% 
          hc_add_series(name = "Median Salary $", data = india_salary$AvgSalary, colorByPoint =  1) %>% 
          hc_title(text = "India Salary by Developer Type")  %>%
          hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

p3 <- highchart() %>%
          hc_xAxis(categories = usa_salary$DevType) %>% 
          hc_add_series(name = "Median Salary $", data = usa_salary$AvgSalary, colorByPoint =  1) %>% 
          hc_title(text = "USA Salary by Developer Type")  %>%
          hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())
          
lst <- list(
  p1,
  p2,
  p3
)

hw_grid(lst, rowheight = 400)
         

```

#### **Insight**

+ Engineering Managers have the highest salaries among all the cases taken in account, closely followed by DevOps specialists & C-suite executives.
+ Engineering Manager :
    1. Global Median : $90000
    2. India Median : $600000
    3. USA Median : $134000


### 2.4. Motion Plot : Developer Salary & Experience by Language

```{r}

motion_df <- survey_results_public %>% select(LanguageWorkedWith,YearsCodingProf,Salary) %>%
            mutate(YearsCodingNum = parse_number(YearsCodingProf),
                                    LanguageWorkedWith = str_split(LanguageWorkedWith, pattern = ";"),
                                    Salary = as.numeric(Salary)) %>%
                                    unnest(LanguageWorkedWith) %>% 
                                    filter(!is.na(LanguageWorkedWith)) %>% filter(!is.na(YearsCodingNum))

motion_df2 <- motion_df %>% filter(!is.na(Salary)) %>%
            select(LanguageWorkedWith, YearsCodingNum, Salary) %>% 
            filter(!is.na(LanguageWorkedWith)) %>% group_by(LanguageWorkedWith,YearsCodingNum) %>% summarize(AvgSalary = median(Salary, na.rm=TRUE))


#motion_df$z <- motion_df$AvgSalary

data_strt2 <- motion_df2  %>% 
  mutate(x = YearsCodingNum, y = AvgSalary, z = 100)

data_strt2$color = distinctColorPalette(length(unique(motion_df2$LanguageWorkedWith)))[as.numeric(as.factor(motion_df2$LanguageWorkedWith))]

data_seqc2 <- motion_df %>% 
  arrange(LanguageWorkedWith, YearsCodingNum) %>% 
  group_by(LanguageWorkedWith) %>% 
  summarise(n=n()) %>%
  right_join(motion_df2, by="LanguageWorkedWith") %>%
  group_by(LanguageWorkedWith) %>%
  do(sequence = list_parse(select(., x = YearsCodingNum, y = AvgSalary, z = n)))

data2 <- left_join(data_strt2, data_seqc2)  

highchart() %>% 
  hc_add_series(data = data2, type = "bubble",
                minSize = 0, maxSize = 30, dataLabels = list(enabled = TRUE, format = "{point.LanguageWorkedWith}")) %>% 
  hc_motion(enabled = TRUE, series = 0, labels = unique(motion_df2$YearsCodingNum),
            loop = TRUE, 
            updateInterval = 1000, magnet = list(step =  1)) %>% 
  hc_plotOptions(series = list(showInLegend = FALSE)) %>% 
  hc_xAxis(min = 0, max = 30, title = list(text = "Year Of Exp")) %>% 
  hc_yAxis(min = 500, max = 200000, title = list(text = "Median Salary (USD)")) %>% 
  hc_title(text = "Motion Plot of Programming Language vs Salary vs Year of Exp")  %>%
  hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = "{point.LanguageWorkedWith} Year Of Exp: {point.x}y Median Salary: ${point.y} No Of Response : {point.z}") %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ It is observed that Less Popular programming languages like Ocaml, Hack, Haskel, Julia etc tend to have higher Median Salaries with increasing work experience.
+ The more common programming languages like SQL, C#, JavaScript etc. have moderate levels of salaries with discrepancies ranging from $30000 to $70000.  


### 2.5. Developer Salary by Gender

```{r}

by_salary_gender <- survey_results_public %>% select(Gender,Salary,YearsCodingProf) %>%
                        mutate(YearsCodingNum = parse_number(YearsCodingProf),
                                    Gender = str_split(Gender, pattern = ";"),
                                    Salary = as.numeric(Salary)) %>%
                                    unnest(Gender) %>% 
                                    filter(!is.na(Salary)) %>%
                                    select(Gender, YearsCodingNum, Salary) %>% 
                                    filter(!is.na(Gender)) %>% group_by(Gender,YearsCodingNum) %>% 
                                    summarize(AvgSalary = median(Salary, na.rm=TRUE))
                                    
hchart(by_salary_gender, "line", hcaes(x = YearsCodingNum, y = AvgSalary, group = Gender)) %>%
hc_xAxis(min = 0, max = 30, title = list(text = "Year Of Exp")) %>% 
  hc_yAxis(min = 500, max = 200000, title = list(text = "Median Salary (USD)"))  %>% hc_add_theme(hc_theme_google())

```

#### **Insight**

+ For 0-6 years, Females have higher median salaries. However the number of females is lower.
+ For 9-30 years, Transgenders & Non Binary genders have higher salaries with a single instance of Males having high salary (27 years).
+ A general trend of increase in salary is seen for all the genders with increase in experience but the number of respondents earning high salaries is lower.
+ Also, since the number of Females, Transgenders & Non-Binary gender respondents is low (accounts for 10% of total gender distribution) so the median values are not a reliable source as they are skewed. A much more stable and gradually increasing plot can be seen for Males due to abundance of data.



### 2.6. Median Salary Vs Years of Experience by Developer Type

```{r}

by_salary_devtype <- survey_results_public %>% select(DevType,Salary,YearsCodingProf) %>%
                        mutate(YearsCodingNum = parse_number(YearsCodingProf),
                                    DevType = str_split(DevType, pattern = ";"),
                                    Salary = as.numeric(Salary)) %>%
                                    unnest(DevType) %>% 
                                    filter(!is.na(Salary)) %>%
                                    select(DevType, YearsCodingNum, Salary) %>% 
                                    filter(!is.na(DevType)) %>% group_by(DevType,YearsCodingNum) %>% 
                                    summarize(AvgSalary = median(Salary, na.rm=TRUE))
                                    
hchart(by_salary_devtype, "spline", hcaes(x = YearsCodingNum, y = AvgSalary, group = DevType)) %>%
hc_xAxis(min = 0, max = 30, title = list(text = "Year Of Exp")) %>% 
hc_yAxis(min = 500, max = 180000, title = list(text = "Median Salary (USD)"))  %>%
hc_legend(align = "left", layout = "vertical", verticalAlign = "top") %>% 
hc_tooltip(sort = TRUE, table = TRUE)  %>%
hc_title(text = "Developer Type vs Median Salary by Year of Exp")  %>% 
hc_add_theme(hc_theme_google())

```

#### **Insight**

+ Note : Click on the labels (legends) on left hand side to toggle (on/off) the plot for the particular category. It's interactive.
+ Developer Types that have high salaries across all Years of Experience are:
    1. DevOps Specialist
    2. Engineering Manager
    3. Data or Business Analyst
    4. Data Scientists
    5. Product Managers
+ Developer Types that have low salaries across all Years of Experience are:
    1. Students 
    2. Educator or Academic Research
    3. System Administrators
    4. Game or plotics Developer
    5. Designers

### 2.7.  Median Salary Vs Years of Experience by Programming Language Type

```{r}

by_salary_LanguageWorkedWith <- survey_results_public %>% select(LanguageWorkedWith,Salary,YearsCodingProf) %>%
                        mutate(YearsCodingNum = parse_number(YearsCodingProf),
                                    LanguageWorkedWith = str_split(LanguageWorkedWith, pattern = ";"),
                                    Salary = as.numeric(Salary)) %>%
                                    unnest(LanguageWorkedWith) %>% 
                                    filter(!is.na(Salary)) %>%
                                    select(LanguageWorkedWith, YearsCodingNum, Salary) %>% 
                                    filter(!is.na(LanguageWorkedWith)) %>% group_by(LanguageWorkedWith,YearsCodingNum) %>% 
                                    summarize(AvgSalary = median(Salary, na.rm=TRUE))
                                    
hchart(by_salary_LanguageWorkedWith, "spline", hcaes(x = YearsCodingNum, y = AvgSalary, group = LanguageWorkedWith)) %>%
hc_xAxis(min = 0, max = 30, title = list(text = "Year Of Exp")) %>% 
hc_yAxis(min = 500, max = 180000, title = list(text = "Median Salary (USD)"))  %>%
hc_legend(align = "left", layout = "vertical", verticalAlign = "top") %>% 
hc_tooltip(sort = TRUE, table = TRUE)  %>%
hc_title(text = "Programming Language vs Median Salary by Year of Exp")  %>% 
hc_add_theme(hc_theme_google())

```

#### **Insight**

+ Note : Click on the labels (legends) on left hand side to toggle (on/off) the plot for the particular category. It's interactive.
+ The Less Popular Programming Languages offer high salaries across all Years of Experience. Some high paying languages are:
    1. Clojure
    2. Hack
    3. Ocaml
    4. Go
    5. Erlang
+ The Less Popular Programming Languages offer high salaries across all Years of Experience. Some high paying languages are:
    1. Delphi/Object Pascal
    2. Cobol
    3. Visual Basic 6
    4. VBA
    5. Assembly
+ Most consistently increasing salary paying programming languages:
    1. JavaScript
    2. CSS
    3. SQL
    4. HTML
    5. Python
    6. C++
    7. Java

## 3. What Do Developers Hope To Be Doing in Five Years?

### 3.1. Hope to be Doing in Five Years

```{r}
by_HopeFiveYears <- survey_results_public %>%
                        filter(!is.na(HopeFiveYears)) %>%
                        group_by(HopeFiveYears) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(Employment = reorder(HopeFiveYears,Total)) %>%
                        mutate(Percent = (Total/sum(Total)*100)) %>%
                        head(10)
highchart() %>%
  hc_xAxis(categories = by_HopeFiveYears$HopeFiveYears) %>% 
  hc_add_series(name = "Percent %", data = by_HopeFiveYears$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Hope to be Doing in Five Years")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())
```

#### **Insights**

+ Most popular aspirations for next five years are:
    1. Working in a different or more specialized technical role
    2. Working as founder or co-founder of their own company
    3. Doing the same work

#### **Inference**

+ About 95.5% people have progressive goals.
+ About 1.67% people are retiring. This can be linked to the people with more than 30 years of experience on Stack Overflow.
+ About 2.8% see themselves doing something else apart from Software Development in next 5 years.

### 3.2. Hope to be Doing in Five Years By Country

```{r}
survey_results_public2 <- survey_results_public %>% filter(!is.na(Country)) %>% group_by(Country) %>% summarise(count=n()) %>% right_join(survey_results_public,by="Country")


df1 <- survey_results_public2 %>% filter(!is.na(HopeFiveYears)) %>%
  group_by(name = HopeFiveYears, drilldown = tolower(HopeFiveYears)) %>% 
  summarise(Total = n()) %>% ungroup() %>% mutate(y = (Total/sum(Total))*100) %>% arrange(desc(y))


df2 <-survey_results_public2 %>% filter(!is.na(HopeFiveYears)) %>% filter(!is.na(Country)) %>%  group_by(HopeFiveYears,Country) %>% dplyr::mutate(y = n()) %>%arrange(desc(y))%>%
  group_by(name = HopeFiveYears, id = tolower(HopeFiveYears)) %>% 
  do(data = list_parse(
                  mutate(.,name = Country, count=as.numeric(count) ,drilldown = tolower(paste(HopeFiveYears,Country,sep=": "))) %>% 
                      group_by(name,drilldown,count) %>% 
                        summarise(y=n()) %>% ungroup() %>% filter(y>=100) %>% mutate(y = (y/count)*100)%>% dplyr::select(name, y, drilldown)   %>%
                            arrange(desc(y))) %>% head(10) 
    )

highchart() %>% 
  hc_chart(type = "bar") %>%
  hc_title(text = 'Hope to be Doing in Five Years By Country') %>%
  hc_add_series(data = df1, name = "Employment Status",colorByPoint =  1) %>% 
  hc_legend(enabled = FALSE) %>%
  hc_xAxis(type = "category") %>% 
  hc_yAxis(title = list(text = "Percentage %"))  %>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list_parse(df2)
  ) %>% hc_add_theme(hc_theme_google())
```

#### **Insights**

+ Note : This is a drill down plot.
+ At top level we see the numver of respondents for each choice. 
+ At lower level we see the % of respondents from each country who chose the particular answer.
+ Most popular aspirations for next five years are:
    + Top 3 for 'Working in a different or more specialized technical role'
        1. Finland 33%
        2. Sweden 31%
        3. Poland 31%
    + Top 3 for 'Working as founder or co-founder of their own company'
        1. Nigeria 38%
        2. Colombia 33%
        3. Serbia 31%
    + Top 3 for 'Doing the same work'
        1. Norway 25%
        2. Czech Republic 22%
        3. Denmark 21%

#### **Inference**

+ Product Manager/Project Manager : It is interesting to see that a large number of developer from India (1119) see themselves as "Product Managers or Project Managers". Unlike other hopes for next 5 years, in this category, India surpasses United States.
+ Retirement : It is interesting to see that among the top 10 countries displayed for this category, India is the only developing country and it has substantially lower number of retirement seekers. this can mean two things.
    1. There are less number of experienced Indian developers on Stack Overflow.
    2. The majority of Indian developers are young, which is true. 

### 3.3. Hope to be Doing in Five Years By Gender

```{r}
survey_results_public2 <- survey_results_public %>% filter(!is.na(Gender)) %>% group_by(Gender) %>% summarise(count=n()) %>% right_join(survey_results_public,by="Gender")


df1 <- survey_results_public2 %>% filter(!is.na(HopeFiveYears)) %>%
  group_by(name = HopeFiveYears, drilldown = tolower(HopeFiveYears)) %>% 
  summarise(Total = n()) %>% ungroup() %>% mutate(y = (Total/sum(Total))*100) %>% arrange(desc(y))


df2 <-survey_results_public2 %>% filter(!is.na(HopeFiveYears)) %>% filter(!is.na(Gender)) %>%  group_by(HopeFiveYears,Gender) %>% dplyr::mutate(y = n()) %>%arrange(desc(y))%>%
  group_by(name = HopeFiveYears, id = tolower(HopeFiveYears)) %>% 
  do(data = list_parse(
                  mutate(.,name = Gender, count=as.numeric(count) ,drilldown = tolower(paste(HopeFiveYears,Gender,sep=": "))) %>% 
                      group_by(name,drilldown,count) %>% 
                        summarise(y=n()) %>% ungroup() %>% filter(y>=100) %>% mutate(y = (y/count)*100)%>% dplyr::select(name, y, drilldown)   %>%
                            arrange(desc(y))) %>% head(10) 
    )

highchart() %>% 
  hc_chart(type = "bar") %>%
  hc_title(text = 'Hope to be Doing in Five Years By Gender') %>%
  hc_add_series(data = df1, name = "Hope in Five Years",colorByPoint =  1) %>% 
  hc_legend(enabled = FALSE) %>%
  hc_xAxis(type = "category") %>% 
  hc_yAxis(title = list(text = "Percentage %"))  %>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list_parse(df2)
  ) %>% hc_add_theme(hc_theme_google())
  
```

#### **Insight**

+ All answer choices scale according to the Gender ratios.

#### **Inference**

+ However, we see a lower ratio of women who aim to be 'Working as founder or co-founder of their own company' compared to all other choices. This is due to the unfortunate dynamics of the world where men tend to get more opportunities.

## 4. How do Developers feel about their Careers and Jobs?

### 4.1. Satisfied with Current Job and Career

```{r}
by_JobSatisfaction <- survey_results_public %>%
                        filter(!is.na(JobSatisfaction)) %>%
                        group_by(JobSatisfaction) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(JobSatisfaction = reorder(JobSatisfaction,Total)) %>%
                        mutate(Percent = (Total/sum(Total)*100)) %>%
                        head(10)
p1 <- highchart() %>%
  hc_xAxis(categories = by_JobSatisfaction$JobSatisfaction) %>% 
  hc_add_series(name = "Percent %", data = by_JobSatisfaction$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Satisfied with Your Current Job")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())
  
  
  by_CareerSatisfaction <- survey_results_public %>%
                        filter(!is.na(CareerSatisfaction)) %>%
                        group_by(CareerSatisfaction) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(CareerSatisfaction = reorder(CareerSatisfaction,Total)) %>%
                        mutate(Percent = (Total/sum(Total)*100)) %>%
                        head(10)
p2 <- highchart() %>%
  hc_xAxis(categories = by_CareerSatisfaction$CareerSatisfaction) %>% 
  hc_add_series(name = "Percent %", data = by_CareerSatisfaction$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Satisfied with Your Career")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())
  
lst <- list(
  p1,
  p2
)
hw_grid(lst, rowheight = 400)
```

#### **Inference**

+ In the first plot we observe the satisfaction levels of people with their Current Job.
    1. Satisfied 70%
    2. Unsatisfied 23%
    3. Neutral 7%
+ In the second plot we observe the satisfaction levels of people with their Career.
    1. Satisfied 72.8%
    2. Unsatisfied 18.95%
    3. Neutral 8.25%

### 4.2. Job Satisfaction - By Country

```{r}

# JobSatisfaction, Gender Country
survey_results_public2 <- survey_results_public %>% filter(!is.na(Country)) %>% group_by(Country) %>% summarise(count=n()) %>% right_join(survey_results_public,by="Country")


df1 <- survey_results_public2 %>% filter(!is.na(JobSatisfaction)) %>%
  group_by(name = JobSatisfaction, drilldown = tolower(JobSatisfaction)) %>% 
  summarise(Total = n()) %>% ungroup() %>% mutate(y = (Total/sum(Total))*100) %>% arrange(desc(y))


df2 <-survey_results_public2 %>% filter(!is.na(JobSatisfaction)) %>% filter(!is.na(Country)) %>%  group_by(JobSatisfaction,Country) %>% dplyr::mutate(y = n()) %>%arrange(desc(y))%>%
  group_by(name = JobSatisfaction, id = tolower(JobSatisfaction)) %>% 
  do(data = list_parse(
                  mutate(.,name = Country, count=as.numeric(count) ,drilldown = tolower(paste(JobSatisfaction,Country,sep=": "))) %>% 
                      group_by(name,drilldown,count) %>% 
                        summarise(y=n()) %>% ungroup() %>% filter(y>=100) %>% mutate(y = (y/count)*100)%>% dplyr::select(name, y, drilldown)   %>%
                            arrange(desc(y))) %>% head(10) 
    )

highchart() %>% 
  hc_chart(type = "bar") %>%
  hc_title(text = 'JobSatisfaction by Country') %>%
  hc_add_series(data = df1, name = "Job Satisfaction",colorByPoint =  1) %>% 
  hc_legend(enabled = FALSE) %>%
  hc_xAxis(type = "category") %>% 
  hc_yAxis(title = list(text = "Percentage %"))  %>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list_parse(df2)
  ) %>% hc_add_theme(hc_theme_google())
```

### 4.3. Job Satisfaction By Gender

```{r}

survey_results_public2 <- survey_results_public %>% filter(!is.na(Gender)) %>% group_by(Gender) %>% summarise(count=n()) %>% right_join(survey_results_public,by="Gender")


df1 <- survey_results_public2 %>% filter(!is.na(JobSatisfaction)) %>%
  group_by(name = JobSatisfaction, drilldown = tolower(JobSatisfaction)) %>% 
  summarise(Total = n()) %>% ungroup() %>% mutate(y = (Total/sum(Total))*100) %>% arrange(desc(y))


df2 <-survey_results_public2 %>% filter(!is.na(JobSatisfaction)) %>% filter(!is.na(Gender)) %>%  group_by(JobSatisfaction,Gender) %>% dplyr::mutate(y = n()) %>%arrange(desc(y))%>%
  group_by(name = JobSatisfaction, id = tolower(JobSatisfaction)) %>% 
  do(data = list_parse(
                  mutate(.,name = Gender, count=as.numeric(count) ,drilldown = tolower(paste(JobSatisfaction,Gender,sep=": "))) %>% 
                      group_by(name,drilldown,count) %>% 
                        summarise(y=n()) %>% ungroup() %>% filter(y>=100) %>% mutate(y = (y/count)*100)%>% dplyr::select(name, y, drilldown)   %>%
                            arrange(desc(y))) %>% head(10) 
    )

highchart() %>% 
  hc_chart(type = "bar") %>%
  hc_title(text = 'JobSatisfaction By Gender') %>%
  hc_add_series(data = df1, name = "Job Satisfaction",colorByPoint =  1) %>% 
  hc_legend(enabled = FALSE) %>%
  hc_xAxis(type = "category") %>% 
  hc_yAxis(title = list(text = "Percentage %"))  %>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list_parse(df2)
  ) %>% hc_add_theme(hc_theme_google())
```

<hr>
# D. Stack Overflow Usage Analysis
<hr>

## 1. How do you Recommend StackOverflow?

```{r}

by_StackOverflowRecommend <- survey_results_public %>%
                        filter(!is.na(StackOverflowRecommend)) %>%
                        group_by(StackOverflowRecommend) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(StackOverflowRecommend = reorder(StackOverflowRecommend,Total)) %>%
                        mutate(Percent = (Total/sum(Total)*100))

highchart() %>%
  hc_xAxis(categories = by_StackOverflowRecommend$StackOverflowRecommend) %>% 
  hc_add_series(name = "Percent %", data = by_StackOverflowRecommend$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Recommend StackOverflow")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insights**

+ Score 10 : 69% are fully satisfied with Stack Overflow and will recommend it.
+ Score 7-10 : 95% respondents
+ Score 1-5 : 5% respondents

## 2. How frequently do you visit Stackoverflow?

```{r}

by_StackOverflowVisit <- survey_results_public %>%
                        filter(!is.na(StackOverflowVisit)) %>%
                        group_by(StackOverflowVisit) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(StackOverflowVisit = reorder(StackOverflowVisit,Total)) %>%
                        mutate(Percent = (Total/sum(Total)*100))

highchart() %>%
  hc_xAxis(categories = by_StackOverflowVisit$StackOverflowVisit) %>% 
  hc_add_series(name = "Percent %", data = by_StackOverflowVisit$Percent, colorByPoint =  1) %>% 
  hc_title(text = " How frequently do you visit Stackoverflow")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insights**

+ Roughly 1/3 people visit SO daily.
+ Roughly another 1/3 people visit SO multiple times a day.

#### **Inference**
+ Stack Overflow is an integral part of the daily routine of 63% of the developers who took the survey.

### 2.1. StackOverflow Visit by Country

```{r}


survey_results_public2 <- survey_results_public %>% filter(!is.na(Country)) %>% group_by(Country) %>% summarise(count=n()) %>% right_join(survey_results_public,by="Country")


df1 <- survey_results_public2 %>% filter(!is.na(StackOverflowVisit)) %>%
  group_by(name = StackOverflowVisit, drilldown = tolower(StackOverflowVisit)) %>% 
  summarise(Total = n()) %>% ungroup() %>% mutate(y = (Total/sum(Total))*100) %>% arrange(desc(y))


df2 <-survey_results_public2 %>% filter(!is.na(StackOverflowVisit)) %>% filter(!is.na(Country)) %>%  group_by(StackOverflowVisit,Country) %>% dplyr::mutate(y = n()) %>%arrange(desc(y))%>%
  group_by(name = StackOverflowVisit, id = tolower(StackOverflowVisit)) %>% 
  do(data = list_parse(
                  mutate(.,name = Country, count=as.numeric(count) ,drilldown = tolower(paste(StackOverflowVisit,Country,sep=": "))) %>% 
                      group_by(name,drilldown,count) %>% 
                        summarise(y=n()) %>% ungroup() %>% filter(y>=100) %>% mutate(y = (y/count)*100)%>% dplyr::select(name, y, drilldown)   %>%
                            arrange(desc(y))) %>% head(10) 
    )

highchart() %>% 
  hc_chart(type = "bar") %>%
  hc_title(text = 'StackOverflowVisit by Country') %>%
  hc_add_series(data = df1, name = "StackOverflow Visit",colorByPoint =  1) %>% 
  hc_legend(enabled = FALSE) %>%
  hc_xAxis(type = "category") %>% 
  hc_yAxis(title = list(text = "Percentage %"))  %>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list_parse(df2)
  ) %>% hc_add_theme(hc_theme_google())


```

#### **Insights**

+ Note : This is a drill down plot.
+ At top level we observe the answer choices with total number of Respondents for that choice.
+ At lower level we observe the % of respondents from each country who chose the particular answer choice.

### 2.2. StackOverflow Visit by Gender by Country

```{r}

# StackOverflowVisit, Gender Country
df1 <- survey_results_public %>% 
          filter(!is.na(StackOverflowVisit)) %>%
          group_by(name = StackOverflowVisit, drilldown = tolower(StackOverflowVisit)) %>% 
          summarise(y = n()) %>% arrange(desc(y))


df2 <-survey_results_public %>% filter(!is.na(StackOverflowVisit)) %>% 
                                filter(!is.na(Gender)) %>%
                                mutate(Gender = strsplit(as.character(Gender), ";"))  %>%
                                filter(!is.na(Gender)) %>% unnest(Gender) %>%
                                group_by(StackOverflowVisit,Gender) %>% 
                                dplyr::mutate(y = n()) %>%
                                arrange(desc(y))%>%
                                group_by(name = StackOverflowVisit, id = tolower(StackOverflowVisit)) %>% 
                                  do(data = list_parse(
                                                  mutate(.,name = Gender, drilldown = tolower(paste(StackOverflowVisit,Gender,sep=": "))) %>% 
                                                      group_by(name,drilldown) %>% 
                                                        summarise(y=n())%>% dplyr::select(name, y, drilldown)   %>%
                                                            arrange(desc(y)))
                                    )

df3 <- survey_results_public %>% select(StackOverflowVisit,Gender,Country) %>%
                                  filter(!is.na(StackOverflowVisit)) %>% 
                                  filter(!is.na(Gender)) %>%
                                  mutate(Gender = strsplit(as.character(Gender), ";"))  %>%
                                  filter(!is.na(Gender)) %>% unnest(Gender) %>%
                                  group_by(StackOverflowVisit,Gender,Country) %>%
                                  summarise(count = n()) %>%
                                  mutate(name = Gender, id = tolower(paste(StackOverflowVisit,Gender,sep=": "))) %>% 
                                  arrange(desc(count)) %>% 
                                  group_by(name,id) %>% 
                                  do(data = list_parse2(select(., name = Country, y = count)) %>% head(10))

highchart() %>% 
  hc_chart(type = "column") %>%
  hc_title(text = "StackOverflow Visit by Gender by Country")  %>%
  hc_add_series(data = df1, name = "StackOverflow Visit",colorByPoint =  1) %>% 
  hc_legend(enabled = FALSE) %>%
  hc_xAxis(type = "category") %>% 
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = append(list_parse(df2),list_parse(df3))
  ) %>% hc_add_theme(hc_theme_google())

```

#### **Insights**

+ Note : This is a **3 level drill down** plot.
+ At top level (first) we observe the answer choices with total number of Respondents for that choice.
+ At middle level (second) we observe the number of respondents from each gender who chose the particular answer choice.
+ At lower level (third) we observe the number of respondents from each country, belonging to the particular gender who chose the particular answer choice.

### 2.3. StackOverflow Visit by DevType

```{r}

survey_results_public2 <-  survey_results_public %>%    mutate(DevType = strsplit(as.character(DevType), ";"))  %>%
                                                        unnest(DevType)

df1 <- survey_results_public2 %>%
       filter(!is.na(DevType)) %>%
       group_by(name = DevType, drilldown = tolower(DevType)) %>% 
       summarise(y = n()) %>% arrange(desc(y))

df2 <-survey_results_public2 %>% filter(!is.na(DevType)) %>% filter(!is.na(StackOverflowVisit)) %>% group_by(DevType,StackOverflowVisit) %>% dplyr::mutate(y = n()) %>%arrange(desc(y))%>%
  group_by(name = DevType, id = tolower(DevType)) %>% 
  do(data = list_parse(
                  mutate(.,name = StackOverflowVisit, drilldown = tolower(paste(DevType,StackOverflowVisit,sep=": "))) %>% 
                      group_by(name,drilldown) %>% 
                        summarise(y=n())%>% dplyr::select(name, y, drilldown)   %>%
                            arrange(desc(y))) 
    )
    
highchart() %>% 
  hc_chart(type = "column") %>%
  hc_title(text = 'Developer wise StackOverflow Site Visit') %>%
  hc_add_series(data = df1, name = "Developer Type",colorByPoint =  1) %>% 
  hc_legend(enabled = FALSE) %>%
  hc_xAxis(type = "category") %>% 
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series =list_parse(df2)
  ) %>% hc_add_theme(hc_theme_google())


survey_results_public2 <-  survey_results_public %>%    mutate(DevType = strsplit(as.character(DevType), ";"))  %>%
                                                        unnest(DevType)

df1 <- survey_results_public2 %>%
       filter(!is.na(StackOverflowVisit)) %>%
       group_by(name = StackOverflowVisit, drilldown = tolower(StackOverflowVisit)) %>% 
       summarise(y = n()) %>% arrange(desc(y))

df2 <-survey_results_public2 %>% filter(!is.na(StackOverflowVisit)) %>% filter(!is.na(DevType)) %>% group_by(StackOverflowVisit,DevType) %>% dplyr::mutate(y = n()) %>%arrange(desc(y))%>%
  group_by(name = StackOverflowVisit, id = tolower(StackOverflowVisit)) %>% 
  do(data = list_parse(
                  mutate(.,name = DevType, drilldown = tolower(paste(StackOverflowVisit,DevType,sep=": "))) %>% 
                      group_by(name,drilldown) %>% 
                        summarise(y=n())%>% dplyr::select(name, y, drilldown)   %>%
                            arrange(desc(y))) 
    )
    
highchart() %>% 
  hc_chart(type = "column") %>%
  hc_title(text = 'StackOverflow Site Visit by DevType') %>%
  hc_add_series(data = df1, name = "StackOverflowVisit",colorByPoint =  1) %>% 
  hc_legend(enabled = FALSE) %>%
  hc_xAxis(type = "category") %>% 
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series =list_parse(df2)
  ) %>% hc_add_theme(hc_theme_google())

```

#### **Insights**

+ Note : These are drill down plots.
+ For first plot : 
    + At top level we observe the developer types with total number of Respondents for each type.
    + At lower level we observe the different answer choices that were answered by the particular developer type.
+ For second plot : 
    + At top level we observe the answer choices with total number of Respondents for that choice.
    + At lower level we observe the developers types who answered the particular choice.

## 3. Do you have StackOverflow Account?

```{r}

by_StackOverflowHasAccount <- survey_results_public %>%
                        filter(!is.na(StackOverflowHasAccount)) %>%
                        group_by(StackOverflowHasAccount) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(StackOverflowHasAccount = reorder(StackOverflowHasAccount,Total)) %>%
                        mutate(Percent = (Total/sum(Total)*100))

highchart() %>%
  hc_xAxis(categories = by_StackOverflowHasAccount$StackOverflowHasAccount) %>% 
  hc_add_series(name = "Percent %", data = by_StackOverflowHasAccount$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Has StackOverflow Account")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insights**

+ 87.5% had Stack Overflow accounts.

## 4. Do you participate on StackOverflow?

```{r}

visit_by_participate <- survey_results_public %>% filter(!is.na(StackOverflowVisit)) %>% filter(!is.na(StackOverflowParticipate)) %>% group_by(StackOverflowVisit, StackOverflowParticipate) %>% summarise(n=n())

hchart(visit_by_participate, "heatmap", hcaes(x = StackOverflowVisit, y = StackOverflowParticipate, value = n)) %>% 
hc_title(text = "StackOverflow Visit vs StackOverflow Participate")  %>%
hc_tooltip(useHTML = TRUE, headerFormat = "",
            pointFormat = "Total : {point.n}") %>%
hc_xAxis(title = list(text = "StackOverflow Site Visit")) %>% 
hc_yAxis(title = list(text = "StackOverflow Participate")) %>% 
hc_add_theme(hc_theme_google())

```

#### **Insights**

+ This is a heatmap between Stack Overflow Visit and Stack Overflow Participation.
+ Top 5 highest type of people :
    1. Visit Daily/Almost Daily --- Participation Less than one per month or monthly : 8135
    2. Visit a few times per week --- Participation Less than one per month or monthly : 6628
    3. Visit multiple times per day --- Participation Less than one per month or monthly : 6411
    4. Visit Daily/Almost Daily --- A few times per month or weekly : 5591
    5. Visit multiple times per day --- A few times per month or weekly : 4351

### 4.1. StackOverflowParticipate by Country

```{r}

survey_results_public2 <- survey_results_public %>% filter(!is.na(Country)) %>% group_by(Country) %>% summarise(count=n()) %>% right_join(survey_results_public,by="Country")


df1 <- survey_results_public2 %>% filter(!is.na(StackOverflowParticipate)) %>%
  group_by(name = StackOverflowParticipate, drilldown = tolower(StackOverflowParticipate)) %>% 
  summarise(Total = n()) %>% ungroup() %>% mutate(y = (Total/sum(Total))*100) %>% arrange(desc(y))


df2 <-survey_results_public2 %>% filter(!is.na(StackOverflowParticipate)) %>% filter(!is.na(Country)) %>%  group_by(StackOverflowParticipate,Country) %>% dplyr::mutate(y = n()) %>%arrange(desc(y))%>%
  group_by(name = StackOverflowParticipate, id = tolower(StackOverflowParticipate)) %>% 
  do(data = list_parse(
                  mutate(.,name = Country, count=as.numeric(count) ,drilldown = tolower(paste(StackOverflowParticipate,Country,sep=": "))) %>% 
                      group_by(name,drilldown,count) %>% 
                        summarise(y=n()) %>% ungroup() %>% filter(y>=100) %>% mutate(y = (y/count)*100)%>% dplyr::select(name, y, drilldown)   %>%
                            arrange(desc(y))) %>% head(10) 
    )

highchart() %>% 
  hc_chart(type = "bar") %>%
  hc_title(text = 'StackOverflowParticipate by Country') %>%
  hc_add_series(data = df1, name = "StackOverflow Participate",colorByPoint =  1) %>% 
  hc_legend(enabled = FALSE) %>%
  hc_xAxis(type = "category") %>% 
  hc_yAxis(title = list(text = "Percentage %"))  %>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list_parse(df2)
  ) %>% hc_add_theme(hc_theme_google())

```

#### **Insights**

+ Note : This is a drill down plot.
+ At top level we observe the various choices for SO participation.
+ At lower level we observe the % of respondents from each country for the particular participation choice.

### 4.2. StackOverflow Site Participate by DevType

```{r}

survey_results_public2 <-  survey_results_public %>%    mutate(DevType = strsplit(as.character(DevType), ";"))  %>%
                                                        unnest(DevType)

df1 <- survey_results_public2 %>%
       filter(!is.na(StackOverflowParticipate)) %>%
       group_by(name = StackOverflowParticipate, drilldown = tolower(StackOverflowParticipate)) %>% 
       summarise(y = n()) %>% arrange(desc(y))

df2 <-survey_results_public2 %>% filter(!is.na(StackOverflowParticipate)) %>% filter(!is.na(DevType)) %>% group_by(StackOverflowParticipate,DevType) %>% dplyr::mutate(y = n()) %>%arrange(desc(y))%>%
  group_by(name = StackOverflowParticipate, id = tolower(StackOverflowParticipate)) %>% 
  do(data = list_parse(
                  mutate(.,name = DevType, drilldown = tolower(paste(StackOverflowParticipate,DevType,sep=": "))) %>% 
                      group_by(name,drilldown) %>% 
                        summarise(y=n())%>% dplyr::select(name, y, drilldown)   %>%
                            arrange(desc(y))) 
    )
    
highchart() %>% 
  hc_chart(type = "column") %>%
  hc_title(text = 'StackOverflow Participate by DevType') %>%
  hc_add_series(data = df1, name = "StackOverflow Participate",colorByPoint =  1) %>% 
  hc_legend(enabled = FALSE) %>%
  hc_xAxis(type = "category") %>% 
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series =list_parse(df2)
  ) %>% hc_add_theme(hc_theme_google())

```

#### **Insights**

+ Note : This is a drill down plot.
+ At top level we observe the various choices for SO participation.
+ At lower level we observe the number of respondents from each developer type for the particular participation choice.

#### **Inference**
+ Most participation types scale according to the total number of developer types.
+ However for 'I have never participated in Q&A on SO', the 'Student' developer type is 4th highest after the consistent top 3 dev types (backend, frontend and full stack). That's probably because most students deal with entry level problems or easy problems which are easily available in the abundant resource of Q&A on SO.

## 5. Do you know about StackOverflowDevStory?

### 5.1. StackOverflow Visit vs StackOverflow DevStory

```{r}

visit_by_participate <- survey_results_public %>% filter(!is.na(StackOverflowVisit)) %>% filter(!is.na(StackOverflowDevStory)) %>% group_by(StackOverflowVisit, StackOverflowDevStory) %>% summarise(n=n())

hchart(visit_by_participate, "heatmap", hcaes(x = StackOverflowVisit, y = StackOverflowDevStory, value = n)) %>% 
hc_title(text = "StackOverflow Visit vs StackOverflow DevStory")  %>%
hc_tooltip(useHTML = TRUE, headerFormat = "",
            pointFormat = "Total : {point.n}") %>%
hc_xAxis(title = list(text = "StackOverflow Site Visit")) %>% 
hc_yAxis(title = list(text = "StackOverflow DevStory")) %>% 
hc_add_theme(hc_theme_google())

```

#### **Insights**

+ This is a heatmap between frequency of SO Visit and awareness about SO DevStory.

#### **Inference**

+ DevStory by Stack Overflow is not very popular in the community. Only 10930 people use it actively out of which 8325 are daily user.
+ 17349 daily users are not even aware of DevStory.

## 6. How do you Recommend StackOverflow Jobs?

### 6.1. StackOverflow Visit vs StackOverflow Jobs Recommend

```{r}

visit_by_participate <- survey_results_public %>% filter(!is.na(StackOverflowVisit)) %>% filter(!is.na(StackOverflowJobsRecommend)) %>% group_by(StackOverflowVisit, StackOverflowJobsRecommend) %>% summarise(n=n())

hchart(visit_by_participate, "heatmap", hcaes(x = StackOverflowVisit, y = StackOverflowJobsRecommend, value = n)) %>% 
hc_title(text = "StackOverflow Visit vs StackOverflow Jobs Recommend")  %>%
hc_tooltip(useHTML = TRUE, headerFormat = "",
            pointFormat = "Total : {point.n}") %>%
hc_xAxis(title = list(text = "StackOverflow Site Visit")) %>% 
hc_yAxis(title = list(text = "StackOverflow Jobs Recommend")) %>% 
hc_add_theme(hc_theme_google())

```

#### **Insights**

+ This is a heatmap between frequency of SO Visit and SO jobs recommendation.

#### **Inference**

+ Daily users are most likely to recommend Stack Overflow Jobs. Score 10 : 7908.
+ Heat map suggest that majority of people have voted 10 and most common range of score is from 5-10, irrespective of usage frequency.
+ 'Less than once a month' users have minimal effect on the data.

## 7. Do You Consider Yourself as StackOverflow Member?

### 7.1. StackOverflow Visit vs StackOverflow Consider Member


```{r}

#table(survey_results_public$StackOverflowConsiderMember)
visit_by_participate <- survey_results_public %>% filter(!is.na(StackOverflowVisit)) %>% filter(!is.na(StackOverflowConsiderMember)) %>% group_by(StackOverflowVisit, StackOverflowConsiderMember) %>% summarise(n=n()) %>% arrange(desc(n))

hchart(visit_by_participate, "heatmap", hcaes(x =StackOverflowVisit , y = StackOverflowConsiderMember, value = n)) %>% 
hc_title(text = "StackOverflow Visit vs Consider StackOverflow Member")  %>%
hc_tooltip(useHTML = TRUE, headerFormat = "",
            pointFormat = "Total : {point.n}") %>%
hc_xAxis(title = list(text = "StackOverflow Site Visit")) %>% 
hc_yAxis(title = list(text = "Consider StackOverflow Member")) %>% 
hc_add_theme(hc_theme_google())

```

#### **Insights**

+ This is a heatmap between frequency of SO Visit and SO Membership.

#### **Inference**

+ Most daily visitors and weekly visitors are members.
+ Similar trend in visit frequency is also seen among the non members but at a lower magnitude

<hr>
# E. Ethics Analysis
<hr>


## 1. Do you Write Code for an Unethical Purpose?

```{r}

by_EthicsChoice <- survey_results_public %>%
                        filter(!is.na(EthicsChoice)) %>%
                        group_by(EthicsChoice) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(EthicsChoice = reorder(EthicsChoice,Total)) %>%
                        mutate(Percent = round(Total/sum(Total)*100)) %>%
                        head(10)

highchart() %>%
  hc_xAxis(categories = by_EthicsChoice$EthicsChoice) %>% 
  hc_add_series(name = "Percent %", data = by_EthicsChoice$Percent, colorByPoint =  1) %>% 
  hc_title(text = " Do you Write Code for an Unethical Purpose")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insights**

+ 5% respondents code for unethical purpose.
+ 37% are not sure.

### 1.1. Ethics Choice by Gender

```{r}

survey_results_public %>%  
  filter(!is.na(EthicsChoice)) %>% 
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  group_by(EthicsChoice,Gender) %>% 
  count() %>% 
  hchart('column',hcaes('EthicsChoice','n',group = 'Gender')) %>% 
  hc_colors(c("red", "blue")) %>% 
  hc_title(text = 'Ethics Choice by Gender ') %>% 
  hc_yAxis(type = 'logarithmic', title = list(text = "Count") ) %>%
  hc_xAxis(title = list(text = "Ethics Choice") ) %>% hc_add_theme(hc_theme_google())

```

#### **Insights**

+ Most developers who code for unethical purpose are Male (2710 or 95% of all unethical coder)

### 1.2. Ethics Choice vs StackOverflow Consider Member

```{r}

visit_by_participate <- survey_results_public %>% filter(!is.na(EthicsChoice)) %>% filter(!is.na(StackOverflowConsiderMember)) %>% group_by(EthicsChoice, StackOverflowConsiderMember) %>% summarise(n=n()) %>% arrange(desc(n))

hchart(visit_by_participate, "heatmap", hcaes(x =EthicsChoice , y = StackOverflowConsiderMember, value = n)) %>% 
hc_title(text = "Ethics Choice vs Consider StackOverflow Member")  %>%
hc_tooltip(useHTML = TRUE, headerFormat = "",
            pointFormat = "Total : {point.n}") %>%
hc_xAxis(title = list(text = "Ethics Choice")) %>% 
hc_yAxis(title = list(text = "Consider StackOverflow Member")) %>% 
hc_add_theme(hc_theme_google())

```

## 2. Would you Report Ethical Problems with Code?

```{r}

by_EthicsReport <- survey_results_public %>%
                        filter(!is.na(EthicsReport)) %>%
                        group_by(EthicsReport) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(EthicsReport = reorder(EthicsReport,Total)) %>%
                        mutate(Percent = round(Total/sum(Total)*100)) %>%
                        head(10)

highchart() %>%
  hc_xAxis(categories = by_EthicsReport$EthicsReport) %>% 
  hc_add_series(name = "Percent %", data = by_EthicsReport$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Would you Report Ethical Problems with Code")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

### 2.1. Ethics Report by Gender

```{r}

survey_results_public %>%  
  filter(!is.na(EthicsReport)) %>% 
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  group_by(EthicsReport,Gender) %>% 
  count() %>% 
  hchart('column',hcaes('EthicsReport','n',group = 'Gender')) %>% 
  hc_colors(c("red", "blue")) %>% 
  hc_title(text = 'Ethics Report by Gender ') %>% 
  hc_yAxis(type = 'logarithmic', title = list(text = "Count") ) %>%
  hc_xAxis(title = list(text = "Ethics Report") ) %>% hc_add_theme(hc_theme_google())

```

### 2.2. Ethics Report vs StackOverflow Consider Member

```{r}

visit_by_participate <- survey_results_public %>% filter(!is.na(EthicsReport)) %>% filter(!is.na(StackOverflowConsiderMember)) %>% group_by(EthicsReport, StackOverflowConsiderMember) %>% summarise(n=n()) %>% arrange(desc(n))

hchart(visit_by_participate, "heatmap", hcaes(x =EthicsReport , y = StackOverflowConsiderMember, value = n)) %>% 
hc_title(text = "Ethics Report vs Consider StackOverflow Member")  %>%
hc_tooltip(useHTML = TRUE, headerFormat = "",
            pointFormat = "Total : {point.n}") %>%
hc_xAxis(title = list(text = "Ethics Report")) %>% 
hc_yAxis(title = list(text = "Consider StackOverflow Member")) %>% 
hc_add_theme(hc_theme_google())

```

## 3. Who Is Most Responsible for Code That Accomplishes Something Unethical?

```{r}

by_EthicsResponsible <- survey_results_public %>%
                        filter(!is.na(EthicsResponsible)) %>%
                        group_by(EthicsResponsible) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(EthicsResponsible = reorder(EthicsResponsible,Total)) %>%
                        mutate(Percent = round(Total/sum(Total)*100)) %>%
                        head(10)

highchart() %>%
  hc_xAxis(categories = by_EthicsResponsible$EthicsResponsible) %>% 
  hc_add_series(name = "Percent %", data = by_EthicsResponsible$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Most Responsible for Code That Accomplishes Something Unethical")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

## 4. Do you Have an Obligation to Consider the Ethical Implications of Their Code?

```{r}

by_EthicalImplications <- survey_results_public %>%
                        filter(!is.na(EthicalImplications)) %>%
                        group_by(EthicalImplications) %>%
                        summarise(Total = n())  %>%
                        arrange(desc(Total)) %>%
                        ungroup() %>%
                        mutate(EthicalImplications = reorder(EthicalImplications,Total)) %>%
                        mutate(Percent = round(Total/sum(Total)*100)) %>%
                        head(10)

highchart() %>%
  hc_xAxis(categories = by_EthicalImplications$EthicalImplications) %>% 
  hc_add_series(name = "Percent %", data = by_EthicalImplications$Percent, colorByPoint =  1) %>% 
  hc_title(text = "Have Obligation to Consider the Ethical Implications of Their Code")  %>%
  hc_chart(type = "bar", options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% hc_add_theme(hc_theme_google())

```

#### **Insights**

+ We have an ethically strong community at Stack Overflow as 80% people feel that they are obligated about the ethical implications of the code.


```{r}
developer <- survey_results_public %>%
mutate(DevType = strsplit(as.character(DevType), ";"))%>%
unnest(DevType) %>%
mutate(EducationTypes = strsplit(as.character(EducationTypes), ";"))%>%
unnest(EducationTypes) %>%
mutate(YearsCoding = parse_number(YearsCoding),
      YearsCodingProf = parse_number(YearsCodingProf),
      CompanySize = parse_number(CompanySize))

```

# Predict

## Who all contributed to open source ?

```{r}

developer_type1 <- developer %>% select(Hobby,OpenSource,Student,Employment,FormalEducation,UndergradMajor,CompanySize,YearsCoding,YearsCodingProf,JobSatisfaction,ConvertedSalary,DevType,EducationTypes)
developer_type1 <- as.data.frame(unclass(developer_type1))
rpart_type1 <- rpart(OpenSource~.,data = developer_type1,method = "class")

visTree(rpart_type1)

```

#### **Insights**

+ **22%** Respondent have taken `multiple non-degree educations`, has `not code as a hobby` and more than 7.5y of Coding Exp are **contributed to Open Source**.
+ **11%** Respondent have taken `open source software non-degree education` are **contributed to Open Source**.
+ **9%** Respondent have taken `multiple non-degree educations` but has `not code as a hobby` are **not contributed to Open Source**.
+ **6%** Respondent have taken `certifications, participated in hackathons, participated in online coding competitions as non degree educations and has less than 7.5y of Coding Exp` are **contributed to Open Source**.


## What is Hope in Next Five Year?


```{r}

developer_type2 <- developer %>% select(Hobby,OpenSource,Student,Employment,FormalEducation,UndergradMajor,CompanySize,YearsCoding,YearsCodingProf,CareerSatisfaction,JobSatisfaction,ConvertedSalary,DevType,EducationTypes,HopeFiveYears)
developer_type2 <- as.data.frame(unclass(developer_type2))
rpart_type2 <- rpart(HopeFiveYears~.,data = developer_type2,method = "class")

visTree(rpart_type2)

```

#### **Insights**

+ **26%** Respondent are `dissatisfied with job`, So they want to **work in different or more specialized technical role**.
+ **20%** Respondent are `dissatisfied with job`, So they has hope of **Working as a founder or co-founder of my own company in Next Five years**.
+ **6%** `Independent contractor, freelancer, or self-employed` has hope of **Working as a founder or co-founder of my own company in Next Five years**.
+ **5%** Respondent are `Extremely satisfied with job`, So they want to **do same work in Next Five Years**.

## Who know about StackOverflow DevStory?

```{r}

temp_df <- survey_results_public %>% select(StackOverflowRecommend, StackOverflowVisit, StackOverflowHasAccount, StackOverflowParticipate, StackOverflowJobs, StackOverflowDevStory, StackOverflowJobsRecommend, StackOverflowConsiderMember)
   
temp_df <- as.data.frame(unclass(temp_df))

rpart_type3 <- rpart(StackOverflowDevStory~.,data = temp_df,method = "class")

visTree(rpart_type3)

```

### **Insights**

+ **24%** Respondent `who don't know about that Stack Overflow had a jobs board` OR `knew that Stack Overflow had a jobs board but have never used or visited` **don't know what that is stackoverflow Dev Story**.
+ **5%** Respondent `who know about stackoverflow job` AND `never participated in Q&A on Stack Overflow` OR `Less than once per month or monthly` AND `don't Consider as Stackoverflow Member` **don't know what that is stackoverflow Dev Story**.
+ **5%** Respondent `who know about stackoverflow job` AND `never participated in Q&A on Stack Overflow` OR `Less than once per month or monthly` AND `Consider as Stackoverflow Member` **have stackoverflow Dev Story but it's out of date**.
+ **4%** Respondent `who know about stackoverflow job` AND `participated in Q&A on Stack Overflow few times per month or weekly` OR `Daily or almost daily` OR `Multiple times per day` AND `Consider as Stackoverflow Member` **have stackoverflow Dev Story**.

## Why do developer want to change their job?

```{r}

temp_df <- survey_results_public %>% select(Hobby, OpenSource, Employment, FormalEducation, UndergradMajor, CompanySize, YearsCodingProf, JobSatisfaction,CareerSatisfaction ,HopeFiveYears,JobSearchStatus)
   
temp_df <- as.data.frame(unclass(temp_df))

rpart_type4 <- rpart(JobSearchStatus~.,data = temp_df,method = "class")

visTree(rpart_type4)

```

### **Insights**

+ **9%** Respondent `who Extremely satisfied with job` is **Not  interested in new Job**.
+ **3%** Respondent `who dissatisfied with job` AND `Not employed` is **actively looking for a Job**.
+ **51%**Respondent `who dissatisfied with job` AND `Independent contractor, freelancer, or self-employed` is **not actively looking for a Job but open for new opportunities**.


# Summary
+ Most professional developers are relatively `new to coding`. 55% have been coding for `less than 8 years`, and a 1/3 have been coding for `less than 5 years`.
+ Most developers have `less than 5 years` of professional experience coding.
+ `Nearly half` of all professional developers also contribute to `open source`.
+ `Almost all` professional developers enjoy coding as a `hobby` as well.
+ `A quarter` of all professional developers don’t have a `bachelor’s degree`.
+ And of the professional developers who have a bachelor’s degree, `1 in 3` have a major unrelated to `computer science or software engineering`.
+ `Virtually` all professional developers learn `new skills` informally most commonly through online courses and teaching themselves using the documentation.
+ `About 1/4` of developers participate in `hackathons` mainly because they’re fun.
+ `Many developers` attend `coding bootcamps` AFTER they already have a `full time job`, for the purpose of expanding their skills.
+ `Most developers` work `full-time` for somebody else, and about `10% of them freelance`. Only `5% of developers` who want to work are `currently unemployed` much better employment stats than pretty much any field.
+ They work in a wide range of industries many of them outside of what we traditionally think of as `tech`.
+ Most of them at `small-to-medium-sized companies`. The more experience a developer has, the more likely they are to work at a large company.
+ They’re an ambitious lot. `Only 1 in 5 developers` wants to be working in the `same capacity 5 years from now`. A quarter of developers aspire to start their own company.
+ `73% of developers` are satisfied with their `choice of careers`.
+ `JavaScript` is the most widely-used technology for the `6th year in a row`.
+ And `JavaScript frameworks` and libraries remain key tools for `most developers`.
+ `SQL databases` are still the most commonly used. Document store database MongoDB and key-value store database Redis both surged in popularity this year.
+ `Software development` is still overwhelmingly dominated by young.
+ They do spend `more than half` of their waking life on a computer, though
+ This said, most developers make time for `regular exercise`.
+ Developers are overwhelmingly optimistic about the future of `artificial intelligence`.
+ But they acknowledge that it is ultimately developers who are responsible for `AI safety`.
+ Most developers said they would `refuse` to write code that they perceive to serve an `unethical purpose`.
+ Still, they say the blame for `unethical code` rests on `management`.
+ Developers overwhelmingly believe they are `obligated to consider the ethical implications` of their code.

# *More Insights Incoming. Stay Tuned & Upvote!*
Constructive criticism is welcome. If there are any suggestions or changes you would like to see in the Kernel please let us know.