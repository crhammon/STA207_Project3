library(tidyverse)
library(ggplot2)
library(lme4)

covid = read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")
gsodi = read_csv('gsodi_pv_4.csv')
gsodi = gsodi[gsodi[,'ID_year']==2019,]
countries_covid = unique(covid$Country)
countries_gsodi = sort(unique(gsodi$ID_country_name))

# check for matches between gsodi and covid
Chk_cntry = gsodi$ID_country_name %in% countries_covid
gsodi_sub = gsodi[Chk_cntry,]
countries_gsodi_sub = sort(unique(gsodi_sub$ID_country_name))

# then check for matches using the subset of gsodi
Chk_cntry2 = covid$Country %in% countries_gsodi_sub
covid_sub = covid[Chk_cntry2,]
countries_covid_sub = unique(covid_sub$Country)


for (country in countries_covid_sub){
  covid_sub[covid_sub[,'Country']==country,'C_A1'] = gsodi[gsodi[,'ID_country_name']==country,'C_A1']
  covid_sub[covid_sub[,'Country']==country,'C_A2'] = gsodi[gsodi[,'ID_country_name']==country,'C_A2']
  covid_sub[covid_sub[,'Country']==country,'C_A3'] = gsodi[gsodi[,'ID_country_name']==country,'C_A3']
  covid_sub[covid_sub[,'Country']==country,'C_A4'] = gsodi[gsodi[,'ID_country_name']==country,'C_A4']
}

# summary(lmer(New_deaths ~ (1|C_A1),data=covid_sub))

# model = lm(New_deaths ~ C_A1+C_A2+C_A3+C_A4,data=covid_sub)
# summary(model)
# plot(model)
ggplot(data = gsodi_sub) +
  geom_histogram(mapping = aes(x=C_A4))
# covidagg = aggregate(x=covid[c('Cumulative_cases','Cumulative_deaths')],by=list(Group.date = covid$Date_reported),FUN='sum')
# 
# ggplot(data=covidagg)+
#   geom_line(mapping=aes(x=Group.date,y=Cumulative_cases,color='Cumulative_cases'))+
#   geom_line(mapping=aes(x=Group.date,y=Cumulative_deaths,color='Cumulative_deaths'))+
#   ylab(' ') + xlab('Date')
# 
# 
# 
# covidagg$PercDeath = covidagg$Cumulative_deaths/covidagg$Cumulative_cases*100
# 
# ggplot(data=covidagg)+
#   geom_line(mapping=aes(x=Group.date,y=PercDeath,color='Perc_death'))+
#   ylab(' ') + xlab('Date')

