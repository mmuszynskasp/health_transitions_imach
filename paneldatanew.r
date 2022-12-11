#####################################################################################################################################################
##################### AIM: estimate transition rates between health states, from SILC 2018-2019, health state on GALI
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
                       yearbirth=PB140, quartbirh= PB130, GALI = PH030, year=year, weighti=PB050) %>% 
                select(yearint,iid, quartint,sex,yearbirth,quartbirh,GALI,weighti), by="iid") %>%
    filter(weighti!=0)} #remove duplicate info of individuals who left one year to live in their own household, or after death



#years studied: 2015-2019, samples first drawn in 2012-2018

i=16 #italy
data19 <- dataprep(year=2019,i) 
data1819 <- data19%>% 
  add_row(dataprep(year=2018,i)%>% filter(!(iid %in%data19$iid))) 
data1719 <- data1819 %>%
  add_row(dataprep(year=2017,i)%>% filter(!(iid %in%data1819$iid))) 
data1619 <- data1719 %>%
  add_row(dataprep(year=2016,i)%>% filter(!(iid %in%data1719$iid))) 
data1519 <- data1619 %>%
  add_row(dataprep(year=2016,i)%>% filter(!(iid %in%data1619$iid))) %>%
  distinct()

setwd("K:\\data\\EUROSTAT\\VID_EUSILC\\magda")
write.table(data1519, file="basic1519.csv", sep=",",row.names=FALSE)

####################################################################################################################################################
######### transition rates with msm
dev.off()
rm(list=ls())

library(tidyr)
library(dplyr)
library(msm)
library(elect)

setwd("K:\\data\\EUROSTAT\\VID_EUSILC\\magda")
mydata <-  read.table(file="basic1519.csv", sep=",",header=TRUE)

panel1519 <- mydata %>%
  add_row(mydata %>% #add rows for those who died in the last year 
            filter(!is.na(yeardied))%>%
            mutate(yearint=yeardied, quartint=quarterdied, GALI=0,weighti=1,  #weight=1 to avoid many repeated spells with distinct weights only
                   quartint=ifelse((quartint==quarterdied & yearint==yeardied),quartint+1,quartint)) %>% #cases when died in the same quarter aas last obs, add quarter to death date
            distinct() %>%
            filter(!is.na(yearint))) %>%
  mutate(ageint=(yearint-yearbirth)*4+(quartint-quartbirh)-200) %>% #age since 50th birthday in quarters
  #         ageint=ifelse(ageint>=120,120,ageint)) %>%
  filter(ageint>=0)  #there will be still cases when 2018 interview below 50, it will be deleted in the next step


#########################weights readjustments
syweights <- function(year){## for each country, 2-year combo, sex: 1.weights sum to 100. sorry, I do not accommodate returns to panel yet
  indiv <- panel1519 %>%
    filter(yearint==year & GALI!=0) %>%
    left_join(panel1519 %>%  #make the data wide, 2 time points only=our interest
                filter(yearint==(year+1)|yeardied==year) %>%
                mutate(ageint2=ageint,yearint2=yearint,quartint2=quartint,GALI2=GALI) %>%
                select(iid,yearint2, ageint2,quartint2,GALI2), by="iid") %>%
    filter(ageint2>ageint) %>%
    filter((!is.na(yearint2)|GALI==0)) %>% #only those re-interviewed!!!!!!!!!!!!
    distinct()
  grouped <- indiv %>%
    group_by(country,sex) %>%
    summarize(sumweights=sum(weighti))
  indiv2 <- indiv %>%
    left_join(grouped) %>%
    mutate(weightnew=100*weighti/sumweights)
  return(indiv2)
}

sample1519 <- ungroup(syweights(year=2015))%>%
  add_row(ungroup(syweights(year=2016))) %>%
  add_row(ungroup(syweights(year=2017))) %>%  
  add_row(ungroup(syweights(year=2018))) %>%
  filter(iid!=c("1314560002","1231410001","1216090003")) #a stupid mistake in Italy when died before interview

###this part removes double records of death, it should be inside the function syweights, but I do not know how and the error is small since there are few deaths
doubled <- sample1519 %>%   #remove the first epizode if two for death in a single year of first interview, applied in the next 
  filter(GALI2==0 & !is.na(GALI2)) %>%
  group_by(iid) %>%
  summarise(maxint1=max(ageint))


all1519 <- sample1519 %>% 
  left_join(doubled) %>%
  filter(!(GALI2==0 & ageint<maxint1)) %>%
  ###readjust weights in each country and sex, to sum to 100 000, data is still wide here
  left_join(ungroup(sample1519 %>% 
                      group_by(country,sex) %>%
                      summarize(sumweights=sum(weightnew))), by=c("sex")) %>% #add country here later
  mutate(weightnew2=100000*weightnew/sumweights.y, yearint=yearint) %>% ## for each coutry,sex: 1.weights sum to 10000
  select(iid,weightnew2,yearint) %>%
  left_join(sample1519) %>%
  filter(!is.na(weightnew2)) %>% ##cases when 2018 interview below 50 deleted
  select(-c(died:yeardied,weighti,sumweights,weightnew)) %>%
  distinct()

epiz1519 <- all1519 %>%
  filter(yearint==2015)%>%
  mutate(iid=paste(iid,1,sep="")) %>%
  add_row(all1519 %>%
            filter(yearint==2016)%>%
            mutate(iid=paste(iid,2,sep="")))%>%
  add_row(all1519 %>%
            filter(yearint==2017)%>%
            mutate(iid=paste(iid,3,sep=""))) %>%
  add_row(all1519 %>%
            filter(yearint==2018)%>%
            mutate(iid=paste(iid,4,sep=""))) %>%
  select(iid,weightnew2,country,sex,GALI,ageint,ageint2,GALI2) %>%
  ###aim:blow up the sample to include the weights
  uncount(round(as.numeric(weightnew2)),.id="wid") %>% #round the weights and create new people with extra id
  mutate(newid=paste(iid,wid,sep="")) %>%
  filter(ageint<120) %>%    #1st interview before 80
  distinct() %>%
  na.omit()

msmtest <- epiz1519 %>%
  mutate(age=50+floor(ageint/4)) %>%
  select(-c(ageint2,GALI2)) %>%
  add_row(epiz1519 %>%
            mutate(age=50+floor(ageint/4)) %>%
            select(-c(ageint,GALI)) %>%
            rename(ageint=ageint2,GALI=GALI2)) %>%
  filter(sex=="1") %>%
  select(country,newid,sex,GALI,ageint,age) %>%
  distinct() %>%
  mutate(GALI=recode(GALI,'2'='2','1'='2','3'='1','0'='3'),
         GALI=as.numeric(GALI))%>% 
  #  filter(!is.na(GALI))%>%
  arrange(newid,ageint)#sort observations by id, required by msm package


##model in msm
Q <- rbind(c(-0.06,0.6,0.01),c(0.11,-0.1,0.001), c(0,0,0))


testmodel <- msm(GALI~ageint, subject=newid, data=msmtest, center=FALSE,
                 qmatrix=Q, death=TRUE, covariates=~age, control=list(reltol=1e-32, maxit=100000, fnscale=100000), gen.inits=TRUE)

#probabilities

#probabilities
trprob <- cbind(rownames(pmatrix.msm(testmodel,covariates = list(age=50))),pmatrix.msm(testmodel,covariates = list(age=50)),50)[-3,]
setwd("K:\\data\\EUROSTAT\\VID_EUSILC\\magda")
colnames(trprob) <- c("start_state", "end_st_1","end_st_2","dead","age")
write.table(trprob, file="trprob.csv",sep=",", row.names=FALSE)

for (agei in 51:119){
  trprob <- cbind(rownames(pmatrix.msm(testmodel,covariates = list(age=agei))),pmatrix.msm(testmodel,covariates = list(age=agei)),agei)[-3,]
  setwd("K:\\data\\EUROSTAT\\VID_EUSILC\\magda")
  write.table(trprob, file="trprob.csv",sep=",", row.names=FALSE,col.names=FALSE, append=TRUE)
}


transprob <- read.table(file="trprob.csv",sep=",",header=TRUE)


# 
# 
# ##old plan, does not work because of small data samples
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
