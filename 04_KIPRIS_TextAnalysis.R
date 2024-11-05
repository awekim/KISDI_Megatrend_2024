#######################################################################
##  Made by: Dr. Keungoui Kim
##  Title: KISDI Megatrend Project - Data Prep 04. Text Analysis
##  goal : Megatrend Tech Analysis
##  Data set: KIPRIS
##  Time Span: 
##  Variables
##      Input: 
##      Output:  
##  Methodology: 
##  Time-stamp: :  
##  Notice :
#######################################################################

library(dplyr)

### Load Data
load(file="R file/cpc.trend.all.RData")

country.code <- 'KR'

### Abstract
load(file=paste0("R file/",country.code,"/Abstract.RData"))

### Priority
load(file=paste0("R file/",country.code,"/Priority.RData"))
Priority$Year <- substr(Priority$우선권주장출원일자,1,4)

# remove NA columns
Abstract <- Abstract[,is.na(names(Abstract))==FALSE]
nrow(Abstract) # 4,903,079
Abstract %>% head(1)

# Remove missing abstracts
Abstract<- Abstract[!grepl('내용 없음',Abstract$초록),]
Abstract<- Abstract[!grepl('내용없음',Abstract$초록),]
Abstract<- Abstract[!grepl('요약 없음',Abstract$초록),]
Abstract<- Abstract[!grepl('요약없음',Abstract$초록),]
nrow(Abstract) # 4,647,489 -> 4,640,916 -> 4,640,602 - > 4,640,154

# Pre-processing
Abstract$초록 <- 
  gsub("<IDXWORD>[^<]*</IDXWORD>", "", Abstract$초록)
Abstract$초록 <- 
  gsub("<Keyword>[^<]*</Keyword>", "", Abstract$초록)
Abstract$초록 <- 
  gsub("<[^>]*>", "", Abstract$초록)
Abstract$초록 <- 
  gsub("\\[[^]]*\\]", "", Abstract$초록)
Abstract$초록 <- 
  gsub("\\([^)]*\\)", "", Abstract$초록)
Abstract$초록 <- 
  gsub("\\s+", " ", Abstract$초록)

Abstract.trend <- cpc.trend.all %>% select(출원번호,type) %>% unique %>% 
  left_join(Abstract) %>%
  left_join(Priority %>% select(출원번호,Year) %>% unique)
rm(Priority)

Abstract.trend$Year %>% table
length(unique(Abstract.trend$출원번호)) # 13,064

rm(Abstract)
save(Abstract.trend, file="R file/Abstract.trend.RData")
load(file="R file/Abstract.trend.RData")
write.csv(Abstract.trend, file="Abstract_trend.csv", row.names=FALSE)

cpc.trend.all$출원번호 %>% unique %>% length # 52672
Abstract.trend$출원번호 %>% unique %>% length # 52672

rm(cpc.trend.all, Abstract.trend)
