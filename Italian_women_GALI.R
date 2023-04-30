rm(list=ls())
###########################################################################################################################
###AIM: estimate transition rates between health states, women, Spain, 2015-2017, wave 6-7 SHARE, health state according to GALI
## state 1= not limited
## state 2= somehow limited/severe limitations
########  transition rates estimated with IMACH
##########################################################################################################################
library(foreign)
library(geepack)
library(utils)
library(HMDHFDplus)
library(xtable)
library(tidyr)
library(dplyr)
library(purrr)
library(nnet)


data67 <- read.dta(file="C:\\Users\\Magdalena\\demography\\attrition\\data\\SHARE\\SHARE_6\\sharew6_rel7-1-0_gv_health.dta", convert.factors = FALSE) %>%
  select(mergeid,gali) %>%
  rename(gali6=gali) %>%
  left_join(read.dta(file="C:\\Users\\Magdalena\\demography\\attrition\\data\\SHARE\\SHARE_6\\sharew6_rel7-1-0_gv_weights.dta", convert.factors = FALSE) %>%
              select(mergeid,cciw_w6)) %>%
  rename("weight6"="cciw_w6") %>%
  left_join(read.dta(file="C:\\Users\\Magdalena\\demography\\attrition\\data\\SHARE\\SHARE_7\\sharew7_rel7-1-0_gv_health.dta", convert.factors = FALSE) %>%
              select(mergeid,gali) %>%
              rename(gali7=gali)) %>%
  left_join(read.dta(file="C:\\Users\\Magdalena\\demography\\attrition\\data\\SHARE\\ALL_Coverscreen\\sharewX_rel7-1-0_gv_allwaves_cv_r.dta",convert.factors = FALSE) %>%
              select(mergeid, country, gender, yrbirth, mobirth, int_year_w6, int_year_w7, int_month_w6, int_month_w7,
                     deceased_year, deceased_month) %>%
              rename("sex"="gender")) %>%
  mutate(monthbirth= as.numeric(mobirth),
         agew6= (int_year_w6-yrbirth)*12 + as.numeric(int_month_w6)-13-monthbirth, agew6=replace(agew6,agew6<595,-1),
         agew7= (int_year_w7-yrbirth)*12 + as.numeric(int_month_w7)-13-monthbirth, agew6=replace(agew6,agew6<595,-1),
         monthdeath= as.numeric(deceased_month)-4, 
         monthdeath=replace(monthdeath,0,1), monthdeath=replace(monthdeath,-1,6),
         agedeath= (deceased_year-yrbirth)*12 + monthdeath-monthbirth,
         agedeath=replace(agedeath,NA,-99),
         agedeath=replace(agedeath,agedeath< -99,-99)) %>%
  filter(agedeath>600|agedeath<0, agew6>599) %>% 
  droplevels() %>%
  mutate(int_year_w6=replace(int_year_w6,(agew6>=agedeath & agedeath>0),-91),
         int_year_w7=replace(int_year_w7,(agew7>=agedeath & agedeath>0),-91),
         weight6=replace(weight6,agew6==-91,NA),
         agew6=replace(agew6,is.na(weight6),-1),
         cname=substr(mergeid,1,2),
         cname=recode(cname,"Eg"="ES","F1"="FR","Cf"="CH","Cg"="CH","Ci"="CH","Bf"="BE","Bn"="BE")) %>%
  filter(country!=25, cname!="PT") %>% #remove portugal very small sample
  mutate(deceased_year=ifelse((deceased_year>2017),-1,deceased_year)) #remove deaths in 2018 and 2019 with the exception of Portugal
 
forimach <- data67 %>%   #prepare the set up for IMACH
  mutate(weight6 = weight6/100000, #IMACH does not manage large weights
         across(starts_with("gali"), ~ replace_na(., -1))) %>%
  mutate(across(-cname, ~ifelse(.<=-2, -1, .))) %>%
  mutate(across(starts_with("gali"), ~ifelse(.==1, 2, .))) %>%
  mutate(across(starts_with("gali"), ~ifelse(.==0, 1, .))) %>%
  replace(is.na(.), -1) %>%
  mutate(int_month_w7=ifelse(int_month_w7==-1,99,int_month_w7),
         deceased_month=ifelse(deceased_month==-1,99,deceased_month),
         int_year_w7=ifelse(int_year_w7==-1,9999,int_year_w7),
         deceased_year=ifelse(deceased_year==-1,9999,deceased_year),
         birth = paste(monthbirth, yrbirth, sep = "/"),
         int1 = paste(int_month_w6, int_year_w6, sep = "/"),
         int2 = paste(int_month_w7, int_year_w7, sep = "/"),
         death = paste(deceased_month,deceased_year,sep="/")) %>%
  mutate(death=ifelse(death=="99/2019","99/9999",death),
         death=ifelse(death=="99/2015","6/2015",death),
         death=ifelse(death=="99/2016","6/2016",death),
         death=ifelse(death=="99/2017","6/2017",death),
         death=ifelse(death=="99/2018","6/2018",death),
         int2=ifelse(death!="99/9999","99/9999",int2),
         gali7=ifelse(death!="99/9999",3, gali7)) %>%
  filter(weight6>0) %>%
  select(cname,sex,weight6, birth, death, int1, gali6, int2, gali7)


######run the models in IMACH
countries <- sort(unique(forimach$cname)) #in case other countries or sex of interest
sex <- c("male","female")

i=12
j=2

forimach2 <- forimach %>%
    filter(cname==countries[i],sex==j) %>%
    select(weight6, birth, death, int1, gali6, int2, gali7)
    rownames(forimach2) <- 1:nrow(forimach2)
    setwd("C:\\Users\\Magdalena\\demography\\healthmicrosim\\data\\GALI")
    write.table(forimach2, file=paste("GALI",countries[i],sex[j],"data.txt",sep=""), col.names=FALSE,quote=FALSE)
    system2("C:\\Program Files\\IMaCh-0.99r42\\bin\\IMaCh.exe", paste("C:\\Users\\Magdalena\\demography\\healthmicrosim\\data\\GALI\\GALI",countries[i],sex[j],".txt",sep=""),wait=FALSE)
 

################################################### transition probabilities between health states
pij <- read.table(file=paste("C:\\Users\\Magdalena\\demography\\healthmicrosim\\data\\GALI\\GALI",countries[i],sex[j],"\\PROB_r",countries[i],sex[j],".txt",sep=""),sep=" ",skip = 6) %>%
      select(V1,V3,V7,V11,V15,V19,V23)
    colnames(pij) <- c("Age1","p11", "p12", "p13", "p21", "p22", "p23")
    
setwd("C:\\Users\\Magdalena\\demography\\healthmicrosim\\data\\GALI\\outprob")
write.table(pij, file=paste(countries[i],sex[j],".txt",sep=""),sep=",",row.names=FALSE)
