#####################################################################################################################################################
######### AIM: estimate transition rates between health states, from SILC 2015-2019, health state on GALI
########  transition rates estimated with IMACH
###########################################################################################################################
####### data set from longitudinal files of SILC
dev.off()
rm(list=ls())

library(tidyr)
library(dplyr)
library(purrr)


setwd("K:\\data\\EUROSTAT\\VID_EUSILC\\Long\\Long")
alldirs <- list.dirs("K:\\data\\EUROSTAT\\VID_EUSILC\\Long\\Long")

myfold <- sort(unique(substr(alldirs,39,40)))[-c(1,8,18,33)]

dataprep <- function(year,i){
  mypath <- paste("K:\\data\\EUROSTAT\\VID_EUSILC\\Long\\Long\\",myfold[i], "\\", year,sep="")
  setwd(mypath)
  read.table(file=paste("UDB_l",myfold[i],substr(year,3,4),"R.csv", sep=""), sep=",", header=TRUE) %>% #design weights
    mutate(country=RB020, iid=RB030) %>%
    select(country,iid) %>% 
    left_join(read.table(file=paste("UDB_l",myfold[i],substr(year,3,4),"R.csv", sep=""), sep=",", header=TRUE) %>% #deaths (not possible to identify when the whole houeshold died)
                mutate(iid=RB030, died=RB110, quarterdied=RB140, yeardied=RB150) %>%
                select(iid,died,quarterdied,yeardied) %>%
                filter(died==6), by="iid") %>%
    left_join(read.table(file=paste("UDB_l",myfold[i],substr(year,3,4),"P.csv", sep=""), sep=",", header=TRUE) %>% #other variables
                mutate(yearint=PB010, iid = PB030, quartint= PB100, sex = PB150,
                       yearbirth=PB140, quartbirth= PB130, GALI = PH030, year=year, weighti=PB050) %>% 
                select(yearint,iid, quartint,sex,yearbirth,quartbirth,GALI,weighti), by="iid") %>%
    filter(weighti!=0)} %>% #remove duplicate info of individuals who left one year to live in their own household, or after death
  mutate(died=ifelse(died==6,1,0),
         yearint=ifelse((died==1 & yearint==yeardied & quarterdied<=quartint & !is.na(quarterdied)),NA,yearint))
#if died before interview, remove interview, probably proxy



#years studied: 2015-2019 (4 datasets 2016-2019), samples first drawn in 2012-2018

i=16 #italy
data19 <- dataprep(year=2019,i) 
data1819 <- data19%>% 
  add_row(dataprep(year=2018,i)%>% filter(!(iid %in%data19$iid))) 
data1719 <- data1819 %>%
  add_row(dataprep(year=2017,i)%>% filter(!(iid %in%data1819$iid))) 
data1519 <- data1719 %>%
  add_row(dataprep(year=2016,i)%>% filter(!(iid %in%data1719$iid))) 

setwd("K:\\data\\EUROSTAT\\VID_EUSILC\\magda")
write.table(data1519, file="basic1519.csv", sep=",",row.names=FALSE)

################################################################################
##### prepare one file with longitudinal data for 2015-2019
dev.off()
rm(list=ls())

library(tidyr)
library(dplyr)
library(purrr)

setwd("K:\\data\\EUROSTAT\\VID_EUSILC\\magda")
data1519 <- read.table(file="basic1519.csv", sep=",",header=TRUE)

long1518 <- data1519 %>%
  filter(yearint==2015) %>%
  rename(quartint15=quartint,yearint15=yearint, GALI15=GALI, weight15=weighti) %>%
  left_join(data1519 %>%
              filter(yearint==2016) %>%
              rename(quartint16=quartint,yearint16=yearint, GALI16=GALI, weight16=weighti) %>%
              select(iid,quartint16,yearint16,GALI16,weight16)) %>% 
  left_join(data1519 %>%
              filter(yearint==2017) %>%
              rename(quartint17=quartint,yearint17=yearint, GALI17=GALI, weight17=weighti) %>%
              select(iid,quartint17,yearint17,GALI17,weight17)) %>%
  left_join(data1519 %>%
              filter(yearint==2018) %>%
              rename(quartint18=quartint,yearint18=yearint, GALI18=GALI, weight18=weighti) %>%
              select(iid,quartint18,yearint18,GALI18,weight18)) %>%
  distinct() %>%
  filter(!(is.na(yearint16)& is.na(yearint17)&is.na(yearint18) & is.na(yeardied))) #remove if no other observation
  
  
long1619 <- data1519 %>% 
  filter(yearint==2016,!(iid %in%long1518$iid)) %>%
  rename(quartint16=quartint,yearint16=yearint, GALI16=GALI, weight16=weighti) %>%
  left_join(data1519 %>%
              filter(yearint==2017) %>%
              rename(quartint17=quartint,yearint17=yearint, GALI17=GALI, weight17=weighti) %>%
              select(iid,quartint17,yearint17,GALI17,weight17)) %>% 
  left_join(data1519 %>%
              filter(yearint==2018) %>%
              rename(quartint18=quartint,yearint18=yearint, GALI18=GALI, weight18=weighti) %>%
              select(iid,quartint18,yearint18,GALI18,weight18)) %>%
  left_join(data1519 %>%
              filter(yearint==2019) %>%
              rename(quartint19=quartint,yearint19=yearint, GALI19=GALI, weight19=weighti) %>%
              select(iid,quartint19,yearint19,GALI19,weight19)) %>%
  distinct()
  
  
long1719<- data1519 %>%
              filter(yearint==2017, !(iid %in%long1619$iid),!(iid %in%long1518$iid)) %>%
              rename(quartint17=quartint,yearint17=yearint, GALI17=GALI, weight17=weighti) %>%
  left_join(data1519 %>%
              filter(yearint==2018) %>%
              rename(quartint18=quartint,yearint18=yearint, GALI18=GALI, weight18=weighti) %>%
              select(iid,quartint18,yearint18,GALI18,weight18)) %>%
  left_join(data1519 %>%
              filter(yearint==2019) %>%
              rename(quartint19=quartint,yearint19=yearint, GALI19=GALI, weight19=weighti) %>%
              select(iid,quartint19,yearint19,GALI19,weight19)) %>%
  distinct()

long1819<- data1519 %>%
  filter(yearint==2018, !(iid %in%long1719$iid),!(iid %in%long1619$iid),!(iid %in%long1518$iid)) %>%
  rename(quartint18=quartint,yearint18=yearint, GALI18=GALI, weight18=weighti) %>%
  left_join(data1519 %>%
              filter(yearint==2019) %>%
              rename(quartint19=quartint,yearint19=yearint, GALI19=GALI, weight19=weighti) %>%
              select(iid,quartint19,yearint19,GALI19,weight19)) %>%
  distinct()


longall <- long1518 %>%
  mutate(quartint19=NA, yearint19=NA, GALI19=NA, weight19=NA) %>%
  add_row(long1619 %>%
            mutate(quartint15=NA, yearint15=NA, GALI15=NA, weight15=NA)) %>%
  add_row(long1719 %>%
            mutate(quartint15=NA, yearint15=NA, GALI15=NA, weight15=NA, 
                   quartint16=NA, yearint16=NA, GALI16=NA, weight16=NA)) %>%
  add_row(long1819 %>%
            mutate(quartint15=NA, yearint15=NA, GALI15=NA, weight15=NA, 
                   quartint16=NA, yearint16=NA, GALI16=NA, weight16=NA,
                   quartint17=NA, yearint17=NA, GALI17=NA, weight17=NA))

setwd("K:\\data\\EUROSTAT\\VID_EUSILC\\magda")
write.table(longall, file="longall1519.csv", sep=",",row.names=FALSE)


####################################################################################################################################################
######### prepare the weights
dev.off()
rm(list=ls())

library(tidyr)
library(dplyr)
library(msm)
library(elect)
library(HMDHFDplus)
library(weights)
library(anesrake)

setwd("K:\\data\\EUROSTAT\\VID_EUSILC\\magda")
mydata <-  read.table(file="longall1519.csv", sep=",",header=TRUE)


######## we split the data into observation years, as the base weights are originally estimated like this in SILC
##weights readjustments:
#select individuals by their longitudinal paths, in each year-groups-file and for each sex summarize to 100
### this implies that we estimate transitions between health states on average in each single year between 2015-2019

###sorry, it will be a function once I figure out how to paste variable names to insert into functions
indivall <- mydata %>%
  filter(!is.na(yearint15)) %>%
  left_join(mydata %>%
              filter(!is.na(yearint15)) %>%
              group_by(sex) %>%
              dplyr::summarize(sumweights=sum(weight15))) %>%
  mutate(weightnew=100*weight15/sumweights) %>%
  mutate(yearint1=yearint15, quartint1=quartint15, GALI1=GALI15, yearint2=yearint16, quartint2=quartint16, GALI2=GALI16,
         quartint2=ifelse(is.na(quartint2),2,quartint2),
         quarterdied=ifelse(is.na(quarterdied),2,quarterdied),
         quartint1=ifelse(is.na(quartint1),2,quartint1),
         ageint1 = floor(((yearint1-yearbirth)*4+(quartint1-quartbirth))/4),
         ageint2 = (yearint2-yearbirth)*4+(quartint2-quartbirth),
         agedied = (yeardied-yearbirth)*4+(quarterdied-quartbirth),
         ageint2=ifelse(is.na(ageint2),999,ageint2))%>%
  mutate(GALI2=ifelse((ageint2>=agedied & !is.na(agedied)),0,GALI2),
         yearint2=ifelse(GALI2==0,yeardied,yearint2),
         quartint2=ifelse(GALI2==0,quarterdied,quartint2)) %>% 
  select(c(country:yeardied,sex:quartbirth, weightnew:ageint1)) %>%
  add_row(mydata %>%
            filter(!is.na(yearint16)) %>%
            left_join(mydata %>%
                        filter(!is.na(yearint16)) %>%
                        group_by(sex) %>%
                        dplyr::summarize(sumweights=sum(weight16))) %>%
            mutate(weightnew=100*weight16/sumweights) %>%
            mutate(yearint1=yearint16, quartint1=quartint16, GALI1=GALI16, yearint2=yearint17, quartint2=quartint17, GALI2=GALI17,
                   quartint2=ifelse(is.na(quartint2),2,quartint2),
                   quarterdied=ifelse(is.na(quarterdied),2,quarterdied),
                   quartint1=ifelse(is.na(quartint1),2,quartint1),
                   ageint1 = floor(((yearint1-yearbirth)*4+(quartint1-quartbirth))/4),
                   ageint2 = (yearint2-yearbirth)*4+(quartint2-quartbirth),
                   agedied = (yeardied-yearbirth)*4+(quarterdied-quartbirth),
                   ageint2=ifelse(is.na(ageint2),999,ageint2))%>%
            mutate(GALI2=ifelse((ageint2>=agedied & !is.na(agedied)),0,GALI2),
                   yearint2=ifelse(GALI2==0,yeardied,yearint2),
                   quartint2=ifelse(GALI2==0,quarterdied,quartint2)) %>% 
            select(c(country:yeardied, sex:quartbirth, weightnew:ageint1))) %>%
  add_row(mydata %>%
            filter(!is.na(yearint17)) %>%
            left_join(mydata %>%
                        filter(!is.na(yearint17)) %>%
                        group_by(sex) %>%
                        dplyr::summarize(sumweights=sum(weight17))) %>%
            mutate(weightnew=100*weight17/sumweights) %>%
            mutate(yearint1=yearint17, quartint1=quartint17, GALI1=GALI17, yearint2=yearint18, quartint2=quartint18, GALI2=GALI18,
                   quartint2=ifelse(is.na(quartint2),2,quartint2),
                   quarterdied=ifelse(is.na(quarterdied),2,quarterdied),
                   quartint1=ifelse(is.na(quartint1),2,quartint1),
                   ageint1 = floor(((yearint1-yearbirth)*4+(quartint1-quartbirth))/4),
                   ageint2 = (yearint2-yearbirth)*4+(quartint2-quartbirth),
                   agedied = (yeardied-yearbirth)*4+(quarterdied-quartbirth),
                   ageint2=ifelse(is.na(ageint2),999,ageint2))%>%
            mutate(GALI2=ifelse((ageint2>=agedied & !is.na(agedied)),0,GALI2),
                   yearint2=ifelse(GALI2==0,yeardied,yearint2),
                   quartint2=ifelse(GALI2==0,quarterdied,quartint2)) %>% 
            select(c(country:yeardied, sex:quartbirth, weightnew:ageint1))) %>%
  add_row(mydata %>%
            filter(!is.na(yearint18)) %>%
            left_join(mydata %>%
                        filter(!is.na(yearint18)) %>%
                        group_by(sex) %>%
                        dplyr::summarize(sumweights=sum(weight18))) %>%
            mutate(weightnew=100*weight18/sumweights) %>%
            mutate(yearint1=yearint18, quartint1=quartint18, GALI1=GALI18, yearint2=yearint19, quartint2=quartint19, GALI2=GALI19,
                   quartint2=ifelse(is.na(quartint2),2,quartint2),
                   quarterdied=ifelse(is.na(quarterdied),2,quarterdied),
                   quartint1=ifelse(is.na(quartint1),2,quartint1),
                   ageint1 = floor(((yearint1-yearbirth)*4+(quartint1-quartbirth))/4),
                   ageint2 = (yearint2-yearbirth)*4+(quartint2-quartbirth),
                   agedied = (yeardied-yearbirth)*4+(quarterdied-quartbirth),
                   ageint2=ifelse(is.na(ageint2),999,ageint2))%>%
            mutate(GALI2=ifelse((ageint2>=agedied & !is.na(agedied)),0,GALI2),
                   yearint2=ifelse(GALI2==0,yeardied,yearint2),
                   quartint2=ifelse(GALI2==0,quarterdied,quartint2)) %>% 
            select(c(country:yeardied, sex:quartbirth, weightnew:ageint1))) %>%  
  distinct() %>%
  mutate(agedied=floor(((yeardied-yearbirth)*4+(quarterdied-quartbirth))/4), #age at interview is age in completed years, 
    agedied=ifelse(GALI2==0,agedied,NA),
    GALI1=recode(GALI1,'2'='2','1'='2','3'='1','0'='3'),
         GALI2=recode(GALI2,'2'='2','1'='2','3'='1','0'='3'),
         GALI1=as.numeric(GALI1), GALI2=as.numeric(GALI2))%>%
  filter(ageint1>=50 & ageint1<80) %>% #only those aged 50-79 at 1st interview
  mutate(ageint5=5*floor(ageint1/5),
         agedied=ifelse(agedied>79,NA,agedied),
         survived=ifelse(!is.na(agedied),0,1)) %>%
  filter(!is.na(yearint2)) #remove those who attrited between the interviews


#####adjust to external information on mortality from HMD

males <- indivall %>%
  filter(survived==1,sex==1) %>% 
  group_by(ageint5) %>%
  summarise(survived=sum(weightnew)) %>%
  left_join(indivall %>% 
              group_by(ageint5) %>%
              summarise(pop=sum(weightnew))) %>%
  mutate(probd=1-survived/pop) 


qx <- c(0.01499, 0.02465,  0.03992, 0.06313, 0.10189, 0.16677) #from HMD, Italian males, 2015-2019, 5-year age groups
calibrmarg <- cbind(males,qx) %>% 
  mutate(wmulti=qx/probd)

#prepare data for imach
forimach <- indivall %>%
 filter(sex==1) %>%
 left_join(calibrmarg %>% 
             select(ageint5,wmulti), by="ageint5") %>%
  mutate(weightnew=ifelse(GALI2==3,weightnew*wmulti/4.75,weightnew)) %>% #the observed exposure is in quarters and the models are estimated in months, hence we loose for sure the first 1.5 and last 1.5 months of time in the models
  dplyr::mutate_at(vars(starts_with("quart")), ~recode(.,'1'='2','2'='5','3'='8','4'='11','99'='99')) %>% #recode from quarters to months
  dplyr::mutate(across(starts_with("yearint"), ~replace_na(.,9999))) %>% 
  dplyr::mutate(across(starts_with("GALI"), ~replace_na(.,-1))) %>% 
  mutate(birth=paste(quartbirth,yearbirth,sep="/"),
         int1=paste(quartint1,yearint1,sep="/"),
         int2=paste(quartint2,yearint2,sep="/"),
         death="99/9999",
         death=ifelse(GALI2==3,int2,death)) %>%
select(weightnew,birth,death,int1,GALI1,int2,GALI2)


rownames(forimach) <- 1:nrow(forimach)
write.table(forimach, file="forimach.txt", col.names=FALSE,quote=FALSE)

###run imach
system2("C:\\Program Files\\IMaCh-0.99r19\\bin\\IMaCh.exe", "C:\\Users\\Magdalena\\demography\\withTim\\stateduration\\data\\panel1519\\italianmales.txt",wait=FALSE)
