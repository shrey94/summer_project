#abalysis of breath test patterns using race

race_present_dataset = master[!is.na(master$RACE2),]

race_df = subset(master, select = c(RACE2,groups))
race_df$RACE2 = as.factor(race_df$RACE2)
which(is.na(race_df$RACE2))
class(race_df$RACE2)

race_df= race_df[!is.na(race_df$RACE2),]

table(race_df$RACE2)
race_df = race_df[(race_df$RACE2 != "NA"),]
levels(race_df$RACE2)

race_df$RACE2 = factor(race_df$RACE2)

race_df$groups = factor(race_df$groups)
levels(race_df$RACE2)
race_df = race_df[(race_df$RACE2 != "Patient Refused"),]
race_df$RACE2 = factor(race_df$RACE2)


for(unique_group in unique(race_df$groups)){
  
  race_df[paste("group", unique_group, sep = ".")] <- ifelse(race_df$groups == unique_group, 1, 0)
}



#ASIAN +AFRICAN AMERICAN MODEL
asians_and_afric_americans = race_df %>% filter(RACE2 =="Asian" | RACE2== "Black or African American")

NOt_asian_or_afric_amer = race_df %>% filter(!(RACE2 =="Asian" | RACE2== "Black or African American"))

asians_and_afric_americans$RACE2 = 1
NOt_asian_or_afric_amer$RACE2 = 0
combined_model_asian_afric_amer = rbind(asians_and_afric_americans,NOt_asian_or_afric_amer)


posit_h2_african = filter(asians_and_afric_americans,asians_and_afric_americans$`group.Postive H2`==1)

attach(combined_model_asian_afric_amer)
AAFA_model = glm(RACE2~groups -1 ,family = "binomial", data = combined_model_asian_afric_amer)
summary(AAFA_model)
combined_model_asian_afric_amer$groups = relevel(combined_model_asian_afric_amer$groups,ref = "Normal BT")
# AAFA_reference_level = glm(RACE2~groups ,family = "binomial", data = combined_model_asian_afric_amer)
# summary(AAFA_reference_level)
exp(cbind("Odds ratio" = coef(AAFA_model), confint.default(AAFA_model, level = 0.95)))

ga = glm(RACE2~`group.Positive CH4`+group.Equivocal+group.Flatliner+`group.Positive h2 & Ch4`+`group.Normal BT`+`group.Postive H2`,family = "binomial",data = combined_model_asian_afric_amer)
summary(ga)
#OTHERS model

others = race_df %>% filter(RACE2 =="Other")
others$RACE2 =1
not_others = race_df %>% filter(!(RACE2 == "Other"))
not_others$RACE2 =0
combined_others = rbind(others, not_others)
othersmodel = glm(RACE2~groups-1,family = "binomial", data = combined_others)
summary(othersmodel)
exp(cbind("Odds ratio" = coef(othersmodel), confint.default(othersmodel, level = 0.95)))

oma = glm(RACE2~group.Equivocal+group.Flatliner+`group.Normal BT`+`group.Positive CH4`+`group.Positive h2 & Ch4`+`group.Postive H2`,family = "binomial", data = combined_others)
summary(oma)

#whites

w = race_df %>% filter(RACE2 =="White")
w$RACE2 =1
nw = race_df %>% filter(!(RACE2 == "White"))
nw$RACE2 =0
combined_w = rbind(w, nw)
whitesmodel = glm(RACE2~groups-1,family = "binomial", data = combined_w)
summary(whitesmodel)
exp(cbind("Odds ratio" = coef(whitesmodel), confint.default(whitesmodel, level = 0.95)))



nwa = glm(RACE2~group.Equivocal+group.Flatliner+`group.Normal BT`+`group.Positive CH4`+`group.Positive h2 & Ch4`+`group.Postive H2`-1,family = "binomial", data = combined_w)
summary(nwa)
exp(cbind("Odds ratio" = coef(nwa), confint.default(nwa, level = 0.95)))




analysis_chi = matrix(c(30,225,2287,811,207,2728,3,15,167,51,22,293),nrow = 6)

analysis_chi
model = chisq.test(analysis_chi)
summary(model)


#asians only
asians = race_df %>% filter(RACE2 =="Asian")

NOt_asian = race_df %>% filter(!(RACE2 =="Asian" ))

asians$RACE2 = 1
NOt_asian$RACE2 = 0
combined_model_asians = rbind(asians,NOt_asian)


# posit_h2_african = filter(asians_and_afric_americans,asians_and_afric_americans$`group.Postive H2`==1)

attach(combined_model_asians)
Asians_model = glm(RACE2~groups -1 ,family = "binomial", data = combined_model_asians)
summary(Asians_model)
exp(cbind("Odds ratio" = coef(Asians_model), confint.default(Asians_model, level = 0.95)))





#AA only

afican_americans = race_df %>% filter(RACE2 =="Black or African American")

NOt_AAs = race_df %>% filter(!(RACE2 =="Black or African American" ))

afican_americans$RACE2 = 1
NOt_AAs$RACE2 = 0
combined_model_AAs = rbind(afican_americans,NOt_AAs)


# posit_h2_african = filter(asians_and_afric_americans,asians_and_afric_americans$`group.Postive H2`==1)

attach(combined_model_AAs)
AAs_model = glm(RACE2~groups -1 ,family = "binomial", data = combined_model_AAs)
summary(AAs_model)
exp(cbind("Odds ratio" = coef(AAs_model), confint.default(AAs_model, level = 0.95)))

