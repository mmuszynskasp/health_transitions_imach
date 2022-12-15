#####################################################################################################################################################
##################### AIM: estimate transition rates between health states, from SILC 2015-2019, health state on GALI
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
######### transition rates with msm
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
  mutate(weightnew=100000*weightnew/sumweightsall) %>%
  select(-c(yearint15,weight15,yearint16,weight16,yearint17,weight17,yearint18,weight18,yearint19,weight19,sumweights,sumweightsall)) %>%
  uncount(round(as.numeric(weightnew)),.id="wid") %>% #round the weights and create new people with extra id
  mutate(newid=paste(iid,wid,sep="")) 

## finally make the records long
long1519 <- individ1519 %>%
  filter(!is.na(quartint15)) %>%
  mutate(GALI=GALI15,
         ageint=(quartint15-quartbirth)+(2015-yearbirth)*4) %>%
  select(newid,ageint,GALI,sex,yearbirth) %>%
  add_row(individ1519 %>%
            filter(!is.na(quartint16)) %>%
            mutate(GALI=GALI16,
                   ageint=(quartint16-quartbirth)+(2016-yearbirth)*4) %>%
            select(newid,ageint,GALI,sex,yearbirth))%>%
  add_row(individ1519 %>%
            filter(!is.na(quartint17)) %>%
            mutate(GALI=GALI17,
                   ageint=(quartint17-quartbirth)+(2017-yearbirth)*4) %>%
            select(newid,ageint,GALI,sex,yearbirth))%>%
  add_row(individ1519 %>%
            filter(!is.na(quartint18)) %>%
            mutate(GALI=GALI18,
                   ageint=(quartint18-quartbirth)+(2018-yearbirth)*4) %>%
            select(newid,ageint,GALI,sex,yearbirth)) %>%
  add_row(individ1519 %>%
            filter(!is.na(quartint19)) %>%
            mutate(GALI=GALI19,
                   ageint=(quartint19-quartbirth)+(2019-yearbirth)*4) %>%
            select(newid,ageint,GALI,sex,yearbirth))%>%
  add_row(individ1519 %>%
            filter(!is.na(quarterdied)) %>%
            mutate(GALI=0,
                   ageint=(quarterdied-quartbirth)+(yeardied-yearbirth)*4) %>%
            select(newid,ageint,GALI,sex,yearbirth)) %>%
  distinct()


write.table(long1519, file="final1519.csv", sep=",",row.names=FALSE)

forelect <-long1519 %>%
  filter(sex=="1", !is.na(GALI)) %>%
  mutate(GALI=recode(GALI,'2'='2','1'='2','3'='1','0'='3'),
         GALI=as.numeric(GALI),
         age=floor(ageint/4),
         time=ageint/4-60,
         state=GALI)%>%
  filter(age>=0 & age<=21) %>% #only those aged <50-80>
  #  filter(!is.na(GALI))%>%
  arrange(newid,age)#sort observations by id, required by msm package


##model in msm
Q <- rbind(c(-0.23,0.23,0.004),c(0.48,-0.5,0.029), c(0,0,0))


#testmodel <- msm(GALI~ageint, subject=newid, data=msmtest, center=FALSE,
#    qmatrix=Q, death=TRUE, control=list(reltol=1e-32, maxit=100000, fnscale=100000), gen.inits=TRUE)


modelelect <- msm(state~time, subject=newid, data=forelect, qmatrix=Q, deathexact = 3, 
                  control=list(reltol=1e-60, maxit=10000000, fnscale=10000000), 
                  gen.inits=FALSE, center=FALSE, covariates=~age)

trprob <- cbind(rownames(pmatrix.msm(modelelect,covariates = list(age=90))),pmatrix.msm(modelelect,covariates = list(age=90)),50)[-3,]
setwd("C:\\Users\\Magdalena\\demography\\withTim\\stateduration\\data\\panel1519")
colnames(trprob) <- c("start_state", "end_st_1","end_st_2","dead","age")
write.table(trprob, file="trprob.csv",sep=",", row.names=FALSE)

for (agei in 1:60){
  trprob <- cbind(rownames(pmatrix.msm(modelelect,covariates = list(age1=agei))),pmatrix.msm(modelelect,covariates = list(age1=agei)),50+agei)[-3,]
  setwd("C:\\Users\\Magdalena\\demography\\withTim\\stateduration\\data\\panel1519")
  write.table(trprob, file="trprob.csv",sep=",", row.names=FALSE,col.names=FALSE, append=TRUE)
}
transprob <- read.table(file="trprob.csv",sep=",",header=TRUE)


#probabilities
transprob <- read.table(file="trprob.csv",sep=",",header=TRUE)

#############################################################################################################
######## playing with elect
LEs    <- elect(x = modelelect, b.covariates = list(age1=0),
                statedistdata = forelect, h = 0.5, age.max = 70, S = 25)





# 
# 
#
# #####################################################################################################################################################
# ####################### raking to pop survival margins from Eurostat 2018 life tables, with base weights
# dev.off()
# rm(list=ls())
# 
# library(tidyr)
# library(dplyr)
# #library(HMDHFDplus)
# library(weights)
# library(anesrake)
# library(eurostat)
# 
# setwd("C:\\Users\\Magdalena\\demography\\withTim\\stateduration\\data\\panel1819")
# mydata <-  read.table(file="basic191819.csv", sep=",",header=TRUE)
# panel1819 <- mydata %>%
#     filter(yearint==2018) %>%#easier than pivot_wider
#     left_join(mydata %>% 
#               filter(yearint==2019) %>%
#               select(iid,quartint,yearint,sr,chronic,GALI), by="iid") %>%  
#   mutate(ageint18=10*floor(((2018-yearbirth)*12+(quartint.x-quartbirh)*4)/120)) %>%
#   filter(ageint18>=50)  #already here we limit to 50
#   
# 
# #HMDcountries <- c("AUT","BEL","BGR","CHE","CZE","DNK","EST", "GRC","ESP", "FIN", "FRACNP", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", "LVA")
# #SILCcountries <- c("AT","BE","BG","CH","CZ","DK","EE","EL","ES","FI","FR","HR","HU","IE","IT","LT","LU","LV")
# #countries <- cbind(HMDcountries,SILCcountries)
# 
# ltdat <- get_eurostat("demo_mlifetable", time_format = "num") %>% # life tables from Eurostat
#   filter(indic_de=="SURVIVORS",time==2018)%>%
#   mutate(age=substr(age,2,4),
#          age=ifelse(age=="_LT",0,age),
#          age=ifelse(age=="_GE",85,age),
#          age=as.numeric(as.character(age)))%>%
#   filter(geo %in% unique(panel1819$country), #only countries in SILC 2018
#          age %in% unique(panel1819$ageint18)) %>%#only age groups selected
#   group_by(sex,geo) %>%
#   mutate(value_next=lead(values),
#          survprob=value_next/values) %>%
#   filter(age<80)
# ######################################
# ###### from this point we start to prepare everything only for GALI, if needed just change in the code to self-rated health or 
# gali1819 <- panel1819 %>%
#   mutate(died=ifelse((died==6 & is.na(GALI.y)),1,0)) %>% #died between the interviews
#   mutate(GALI.y=ifelse(died==1,0,GALI.y)) %>% #deaths =0  in GALI.2019 var
#   filter(!is.na(GALI.x),!is.na(GALI.y),#remove missings of GALI in any interview, to be handled by some attrition model later
#          ageint18>=50)  %>% #only those 50+
#   mutate(weighti=weighti/10000) #smaller weights as R might have problems with large once
#    
# surv19 <- gali1819 %>%
#   group_by(country,sex,ageint18,died) %>%
#   summarise(forprob=sum(weighti)) %>% #sum dead and alive, age as age at interview, not perfect but good enough, including weights here
#   pivot_wider(names_from=died, values_from = forprob, names_prefix = "out" ) %>%
#   mutate(dead=out1, 
#          all=out0+out1,
#          probsurv=(all-dead)/all,
#          probsurv=ifelse(is.na(probsurv),1,probsurv))
# 
