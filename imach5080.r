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



#mutate(ageint=(yearint-yearbirth)*4+(quartint-quartbirh)-200) %>% #age since 50th birthday in quarters
#         ageint=ifelse(ageint>=120,120,ageint)) %>%
#  filter(ageint>=0)  #there will be still cases when 2018 interview below 50, it will be deleted in the next step


#########################weights readjustments
#select individuals by their longitudinal paths, in each year-groups-file and for each sex summarize to 100
indiv1516 <- mydata %>%
  filter(!is.na(yearint15) & (!is.na(yearint16)|yeardied==2015|(is.na(yearint16) & yeardied==2016))) %>%
  left_join(mydata %>%
              filter(!is.na(yearint15) & (!is.na(yearint16)|yeardied==2015|(is.na(yearint16) & yeardied==2016))) %>%
              group_by(sex) %>%
              dplyr::summarize(sumweights=sum(weight15))) %>%
  mutate(weightnew=100*weight15/sumweights)


indiv1617 <- mydata %>%
                 filter(!is.na(yearint16) & (!is.na(yearint17)|yeardied==2016|(is.na(yearint17) & yeardied==2017)))%>%
  left_join(mydata %>%
              filter(!is.na(yearint16) & (!is.na(yearint17)|yeardied==2016|(is.na(yearint17) & yeardied==2017))) %>%
                       group_by(sex) %>%
                       dplyr::summarize(sumweights=sum(weight16))) %>%
              mutate(weightnew=100*weight16/sumweights)


indiv1718 <- mydata %>%
                 filter(!is.na(yearint17) & (!is.na(yearint18)|yeardied==2017|(is.na(yearint18) & yeardied==2018)))%>%
  left_join(mydata %>%
              filter(!is.na(yearint17) & (!is.na(yearint18)|yeardied==2017|(is.na(yearint18) & yeardied==2018))) %>%
              group_by(sex) %>%
              dplyr::summarize(sumweights=sum(weight17))) %>%
  mutate(weightnew=100*weight17/sumweights)


indiv1819 <- mydata %>%
                  filter(!is.na(yearint18) & (!is.na(yearint19)|yeardied==2018|(is.na(yearint19) & yeardied==2019)))%>%
  left_join(mydata %>% filter(!is.na(yearint18) & (!is.na(yearint19)|yeardied==2018|(is.na(yearint19) & yeardied==2019)))%>%
              group_by(sex) %>%
              dplyr::summarize(sumweights=sum(weight18))) %>%
  mutate(weightnew=100*weight18/sumweights)

indivwall <- indiv1516 %>%   ###the paired records back together
  add_row(indiv1617 %>%
            filter(!(indiv1617$iid %in%indiv1516$iid))) %>%
  add_row(indiv1718 %>%
            filter(!(indiv1718$iid %in%indiv1516$iid|indiv1718$iid %in%indiv1617$iid))) %>%
  add_row(indiv1819 %>%
            filter(!(indiv1819$iid %in%indiv1617$iid|indiv1819$iid %in%indiv1718$iid)))

individ1519 <- indivwall %>%
  left_join(indivwall %>%
              group_by(sex) %>%
              dplyr::summarize(sumweightsall=sum(weightnew)), by=c("sex")) %>% 
  ###readjust weights for each sex together, to sum to 100 000, data is still wide here
  mutate(weightnew=10000*weightnew/sumweightsall) %>%
  select(-c(yearint15,weight15,yearint16,weight16,yearint17,weight17,yearint18,weight18,yearint19,weight19,sumweights,sumweightsall))


## finally make the records long and then again wide, since they start at different year
final1519 <- individ1519 %>%
  filter(!is.na(quartint15)) %>%
  mutate(GALI=GALI15,
         quartint=quartint15,
         yearint=2015,
         ageint=(quartint15-quartbirth)+(2015-yearbirth)*4) %>%
  select(iid,ageint,GALI, quartint,yearint, sex,quartbirth, yearbirth,weightnew) %>%
  add_row(individ1519 %>%
            filter(!is.na(quartint16)) %>%
            mutate(GALI=GALI16,
                   quartint=quartint16,
                   yearint=2016,
                   ageint=(quartint16-quartbirth)+(2016-yearbirth)*4) %>%
            select(iid,ageint,GALI, quartint,yearint, sex,quartbirth, yearbirth,weightnew)) %>%
  add_row(individ1519 %>%
            filter(!is.na(quartint17)) %>%
            mutate(GALI=GALI17,
                   quartint=quartint17,
                   yearint=2017,
                   ageint=(quartint17-quartbirth)+(2017-yearbirth)*4) %>%
            select(iid,ageint,GALI, quartint,yearint, sex,quartbirth, yearbirth,weightnew)) %>%
  add_row(individ1519 %>%
            filter(!is.na(quartint18)) %>%
            mutate(GALI=GALI18,
                   quartint=quartint18,
                   yearint=2018,
                   ageint=(quartint18-quartbirth)+(2018-yearbirth)*4) %>%
            select(iid,ageint,GALI, quartint,yearint, sex,quartbirth, yearbirth,weightnew)) %>%
  add_row(individ1519 %>%
            filter(!is.na(quartint19)) %>%
            mutate(GALI=GALI19,
                   quartint=quartint19,
                   yearint=2019,
                   ageint=(quartint19-quartbirth)+(2019-yearbirth)*4) %>%
            select(iid,ageint,GALI, quartint,yearint, sex,quartbirth, yearbirth,weightnew)) %>%
  add_row(individ1519 %>%
            filter(!is.na(quarterdied)) %>%
            mutate(GALI=0,
                   quartint=quarterdied,
                   yearint=yeardied,
                   ageint=(quarterdied-quartbirth)+(yeardied-yearbirth)*4) %>%
            select(iid,ageint,GALI, quartint,yearint, sex,quartbirth, yearbirth,weightnew)) %>%
  distinct() %>%
  mutate(GALI=recode(GALI,'2'='2','1'='2','3'='1','0'='3'),
         GALI=as.numeric(GALI))%>%
  filter(ageint>=200 & ageint<320) %>%  #only those aged 50-79.75
  group_by(iid) %>% 
  arrange(iid, ageint) %>% 
  mutate(rank = dense_rank(ageint)) %>% #add the rank of the observation to be able to pivot wider then
  pivot_wider(names_from = rank, values_from = ageint:yearint)
  
write.table(final1519, file="final1519.csv", sep=",",row.names=FALSE)


######################################################################
dev.off()
rm(list=ls())

library(tidyr)
library(dplyr)

setwd("K:\\data\\EUROSTAT\\VID_EUSILC\\magda")
###check deaths by age
males <-  read.table(file="final1519.csv", sep=",",header=TRUE) %>%
  filter(sex==1) %>%
  mutate(agedeath2=ifelse(GALI_2==3, 5*floor(ageint_2/20),0),
         agedeath3=ifelse(GALI_3==3,5*floor(ageint_3/20),0),
         agedeath4=ifelse(GALI_4==3,5*floor(ageint_4/20),0)) %>%
  dplyr::mutate(across(starts_with("agedeath"), ~replace_na(.,0))) %>%
  mutate(agedeath=as.numeric(agedeath2)+as.numeric(agedeath3)+as.numeric(agedeath4)) %>%
  mutate(age1= 5*floor(ageint_1/20),
         cases=1,
         cases=ifelse(is.na(ageint_2),0,1)) 

deathprob <- males %>%
  mutate(death=ifelse(agedeath>0,1,0)) %>%
  group_by(age1) %>%
    summarise(deaths=sum(death),startpop=sum(cases))

qx <- c(0.01499, 0.02465,0.03992, 0.06313, 0.10189, 0.16677) #from HMD, Italian males, 2015-2019, 5-year age groups
expected <- round(9/24*qx*deathprob$startpop)

brutald <- cbind(deathprob,expected) %>%
  mutate(wmulti=expected/deaths)

forimach <- read.table(file="final1519.csv", sep=",",header=TRUE) %>%
 mutate(age1=5*floor(ageint_1/20)) %>%
 left_join(brutald, by="age1") %>%
 mutate(weightnew2=ifelse(GALI_2==3,weightnew*wmulti,NA),
         weightnew3=ifelse(GALI_3==3,weightnew*wmulti,NA),
         weightnew4=ifelse(GALI_4==3,weightnew*wmulti,NA)) %>%
  mutate(weightnew=ifelse(!is.na(weightnew2),weightnew2,weightnew),
         weightnew=ifelse(!is.na(weightnew3),weightnew3,weightnew),
         weightnew=ifelse(!is.na(weightnew4),weightnew4,weightnew)) %>%
  dplyr:: mutate_at(vars(starts_with("quart")), ~replace_na(.,99)) %>%
  dplyr::mutate_at(vars(starts_with("quart")), ~recode(.,'1'='2','2'='5','3'='8','4'='11','99'='99')) %>% #recode from quarters to months
  dplyr::mutate(across(starts_with("yearint"), ~replace_na(.,9999))) %>% 
  dplyr::mutate(across(starts_with("GALI"), ~replace_na(.,-1))) %>% 
  filter(GALI_1!=3 & GALI_1!=-1) %>% #remove the case where the first observation is dead or NA
  mutate(birth=paste(quartbirth,yearbirth,sep="/"),
         int1=paste(quartint_1,yearint_1,sep="/"),
         int2=paste(quartint_2,yearint_2,sep="/"),
         int3=paste(quartint_3,yearint_3,sep="/"),
         int4=paste(quartint_4,yearint_4,sep="/"),
         death="99/9999",
         death=ifelse(GALI_1==3,int1,death),
         death=ifelse(GALI_2==3,int2,death),
         death=ifelse(GALI_3==3,int3,death),
         death=ifelse(GALI_4==3,int4,death)) %>%
  # mutate(int5="1/2020",
  #        GALI_5=-2,
  #   #     GALI_5=ifelse(death!="99/9999",3,-2),
  #        GALI_4=ifelse(int4=="99/9999",GALI_5,GALI_4),
  #        int4=ifelse(int4=="99/9999",int5,int4))%>%
#  dplyr::mutate_at(vars(starts_with("GALI")), ~ifelse(.==3,-1,.)) %>%
 select(sex,weightnew,birth,death,int1,GALI_1,int2,GALI_2,int3,GALI_3,int4,GALI_4)


ourdata <- forimach %>%
  filter(sex==1)
rownames(ourdata) <- 1:nrow(ourdata)
write.table(ourdata, file="forimach.txt", col.names=FALSE,quote=FALSE)



system2("C:\\Program Files\\IMaCh-0.99r19\\bin\\IMaCh.exe", "C:\\Users\\Magdalena\\demography\\withTim\\stateduration\\data\\panel1519\\italianmales.txt",wait=FALSE)
  
  
  
  
