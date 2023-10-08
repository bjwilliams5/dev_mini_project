##############################################
# Author: Brandon Williams
# Date: 9/29/2023
# Description: 
#   clean data from Peruvian INEI ENCUESTA DEMOGRÁFICA Y DE SALUD FAMILIAR
#   (ENDES) 
#
##############################################

# Load data

df <- read.dbf(here("data/raw/INEI/2015_64_RECH0.dbf"))

df <- df %>% 
  select(HHID,HV001,HV007,HV009,HV010,HV011,HV014,HV015,HV024,HV025,HV026,HV035,HV040) %>% 
  rename(
    cluster = HV001,
    year = HV007,
    hh.mmbrs = HV009,
    hh.women = HV010,
    hh.men = HV011,
    hh.chld.under5 = HV014,
    interview.result = HV015,
    region = HV024,
    rural = HV025,
    residence = HV026,
    eligible.chld = HV035,
    altitude = HV040
  )


df2 <- read.dbf(here("data/raw/INEI/2015_64_RECH1.dbf")) %>% 
  select(HHID,HVIDX,HV101,HV104,HV105,HV109,HV115,HV117) %>% 
  rename(
    rel.to.head = HV101,
    index = HVIDX,
    sex = HV104,
    age = HV105,
    educ = HV109,
    marital.status = HV115,
    women.study = HV117
  )
  

df_merge <- merge(x = df2, y = df, by = "HHID", all = FALSE) 

df3 <- read.dbf(here("data/raw/INEI/2015_64_RECH4.dbf")) %>% 
  select(HHID,IDXH4,SH11Z,SH13) %>% 
  rename(
    index = IDXH4,
    no.insurance = SH11Z,
    work.activity = SH13
  ) %>% 
  mutate_all(~replace_na(.,99))

df_merge <- merge(x = df_merge, y = df3, by = c("HHID","index"), all = FALSE) 


df4 <- read.dbf(here("data/raw/INEI/2015_65_RECH23.dbf")) %>% 
  select(HHID,HV201,HV205,HV206,HV213,HV214,HV215) %>% 
  rename(
    drnk.wtr.src = HV201,
    toilet = HV205,
    electricity = HV206,
    floor = HV213,
    wall = HV214,
    roof = HV215
  )

df_merge <- merge(x = df_merge, y = df4, by = c("HHID"), all = FALSE)

df5 <- read.dbf(here("data/raw/INEI/2015_programas sociales x Hogar.dbf")) %>%
  select(HHID,QH95,QH99) %>%
  rename(
    juntos = QH95,
    pension65 = QH99
  ) %>%
  mutate_all(~replace_na(.,99))

df_merge <- merge(x = df_merge, y = df5, by = c("HHID"), all = FALSE)


df6 <- read.dbf(here("data/raw/INEI/2015_73_REC84DV.dbf"))

df6[ , 176:177] <- str_split_fixed(df6$CASEID, ' ', 2)

df6 <- df6 %>% 
  select(V176,V177,D103A,D103B,D105A,D105B,D105C,D105D,D105E,D105F,D105G,D105I,D106,
         D107,D108,D119XB,D128) %>% 
  rename(
    HHID = V176,
    index = V177,
    humiliated = D103A,
    threaten = D103B,
    push = D105A,
    slap = D105B,
    punch = D105C,
    kick = D105D,
    strangle = D105E,
    knife.threat = D105F,
    knife.attack = D105G,
    force.sex = D105I,
    violence = D106,
    severe.violence = D107,
    sex.violence = D108,
    sought.pro.help = D119XB,
    told.anyone=D128
  )%>% 
  mutate_all(~replace_na(.,99))


df6$index <- as.numeric(df6$index)
df6$HHID <- as.factor(df6$HHID)




df_merge <- merge(x = df_merge, y = df6, by = c("HHID","index"), all =T)

df_merge %>% 
  filter(women.study>0) %>% 
  count(women.study)

df7 <- read.dbf(here("data/raw/INEI/2015_414_CSALUD01.dbf"))

df7$QHCLUSTE <- str_pad(df7$QHCLUSTE, 4, pad = "0")
df7$QHNUMBER <- str_pad(df7$QHNUMBER, 3, pad = "0")
df7$QHHOME <- str_pad(df7$QHHOME, 2, pad = "0")
df7 <- unite(df7,HHID,c(QHCLUSTE,QHNUMBER,QHHOME),sep = "")

df7 <- df7 %>% 
  select(HHID,QSNUMERO,QS100,QS107,QS202,QS209,QS311,QS409,QS704B,QS704C,QS707) %>% 
  rename(
    index = QSNUMERO,
    measure.bp = QS100,
    measure.bsugar = QS107,
    smoke = QS202,
    drink = QS209,
    dentist = QS311,
    mammogram = QS409,
    depressed = QS704B,
    no.sleep = QS704C,
    receive.treat = QS707,
  )%>% 
  mutate_all(~replace_na(.,99))

df_merge <- merge(x = df_merge, y = df7, by = c("HHID","index"), all =T)

write.csv(df_merge, here("data/clean/2015_clean.csv"), row.names=FALSE) 


