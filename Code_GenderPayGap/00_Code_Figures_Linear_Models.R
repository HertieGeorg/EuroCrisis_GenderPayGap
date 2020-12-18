# Analysis of Government Spending

# Data Wranglin - Firs Regressions


# Load libraries 

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(stargazer)
library(ggstance)
library(readr)


# Loading data set

dataset <- read_csv("F&F_Tabelle_GovSpending_ALL_VALUES_GPG_Including_Gov_Sector.csv")

summary(dataset$GPG_Including_Gov)



# Alpha: Visualization Correlation Government Spending and GPG over Time 
# Sorted by budget size for a better visualization 

target_group <- c("Germany", "Slovakia", "France", "Spain", "Finland", "Netherlands",  "Ireland", "Slovenia", "Latvia", "Lithuania", "Cyprus", "Estonia")


#FFF_Alpha_1
dataset %>%  filter(Country %in% target_group) %>%  filter(All_Government_Total_Spending_Mill_Euro >= 1000000.0) %>% 
  ggplot(aes(x=GPG_Including_Gov, y = All_Government_Total_Spending_Mill_Euro, color = Country)) +
  geom_point()+
  geom_line()+
  theme_bw() +
  xlab("Gender Pay Gap (%)") +  
  ylab("Total Government Spending (€ Million)") +
  ggtitle("Figure 3. Correlation GPG and Government Spending (3)")

#FFF_Alpha_2
dataset %>%  filter(Country %in% target_group) %>% filter(All_Government_Total_Spending_Mill_Euro >= 70000.0 & All_Government_Total_Spending_Mill_Euro <= 1000000.0) %>% 
  ggplot(aes(x=GPG_Including_Gov, y = All_Government_Total_Spending_Mill_Euro, color = Country)) +
  geom_point()+
  geom_line()+
  theme_bw() +
  xlab("Gender Pay Gap (%)") +  
  ylab("Total Government Spending (€ Million)") +
  ggtitle("Figure 2. Correlation GPG and Government Spending (2)")

#FFF_Alpha_3
dataset %>% filter(Country %in% target_group) %>%  filter(All_Government_Total_Spending_Mill_Euro <= 70000) %>% 
  ggplot(aes(x=GPG_Including_Gov, y = All_Government_Total_Spending_Mill_Euro, color = Country)) +
  geom_point()+
  geom_line() +
  theme_bw() +
  xlab("Gender Pay Gap (%)") +  
  ylab("Total Government Spending (€ Million)") +
  ggtitle("Figure 1. Correlation GPG and Government Spending (1)")

#All
dataset %>% filter(Country %in% target_group) %>% 
  ggplot(aes(x= GPG_Including_Gov, y = All_Government_Total_Spending_Mill_Euro, color = Country)) +
  geom_point()+
  geom_line()+
  theme_bw() +
  xlab("Gender Pay Gap (%)") +  
  ylab("Total Government Spending (€ Million)") +
  ggtitle("Correlation Gender Pay Gap and Government Spending")




#--------------------------------------Ceta First Model: Controls (Year, GDP)  ----------------------------------------------------------


G <- dataset %>% filter(Country == "Germany") 
lm_G3 <- lm(GPG_Including_Gov  ~ All_Government_Total_Spending_Mill_Euro   + GDP_Total+  Year, data = G)

Ire <- dataset %>% filter(Country == "Ireland") 
lm_Ire3 <- lm(GPG_Including_Gov  ~ All_Government_Total_Spending_Mill_Euro   + GDP_Total+  Year, data = Ire)

Fr <- dataset %>% filter(Country == "France") 
lm_Fr3 <- lm(GPG_Including_Gov  ~ All_Government_Total_Spending_Mill_Euro   + GDP_Total+  Year, data = Fr)

Fi <- dataset %>% filter(Country == "Finland") 
lm_Fi3 <- lm(GPG_Including_Gov  ~ All_Government_Total_Spending_Mill_Euro   + GDP_Total+  Year, data = Fi)

Sp <- dataset %>% filter(Country == "Spain") 
lm_Sp3 <- lm(GPG_Including_Gov  ~ All_Government_Total_Spending_Mill_Euro   + GDP_Total+  Year, data = Sp)

C <- dataset %>% filter(Country == "Cyprus") 
lm_C3 <- lm(GPG_Including_Gov  ~ All_Government_Total_Spending_Mill_Euro   + GDP_Total+  Year, data = C)

Ne <- dataset %>% filter(Country == "Netherlands") 
lm_Ne3 <- lm(GPG_Including_Gov  ~ All_Government_Total_Spending_Mill_Euro   + GDP_Total+  Year, data = Ne)

Lat <- dataset %>% filter(Country == "Latvia") 
lm_Lat3 <- lm(GPG_Including_Gov  ~ All_Government_Total_Spending_Mill_Euro   + GDP_Total+  Year, data = Lat)

Lit <- dataset %>% filter(Country == "Lithuania") 
lm_Lit3 <- lm(GPG_Including_Gov  ~ All_Government_Total_Spending_Mill_Euro   + GDP_Total+  Year, data = Lit)

Slove <- dataset %>% filter(Country == "Slovenia") 
lm_Slove3 <- lm(GPG_Including_Gov  ~ All_Government_Total_Spending_Mill_Euro   + GDP_Total+  Year, data = Slove)

Slova <- dataset %>% filter(Country == "Slovakia") 
lm_Slova3 <- lm(GPG_Including_Gov  ~ All_Government_Total_Spending_Mill_Euro   + GDP_Total+  Year, data = Slova)

Esto <- dataset %>% filter(Country == "Estonia") 
lm_Esto3 <- lm(GPG_Including_Gov  ~ All_Government_Total_Spending_Mill_Euro   + GDP_Total+  Year, data = Esto)


own_labels3 =  c("Spending: Total", "GDP","Year")

# All countries
stargazer(lm_G3, lm_Slova3, lm_Fr3, lm_Sp3, lm_Fi3,  lm_Ne3, lm_Lit3,  lm_C3,lm_Ire3, lm_Slove3,  lm_Lat3, lm_Esto3, 
          title="Table 1. Influence of Government Expenditures on Gender Pay Gap", 
          covariate.labels = own_labels3, 
          type = "text",
          column.labels = c("Germany", "Slovakia", "France", "Spain", "Finland", "Netherlands", "Lithuania",  "Cyprus", "Ireland", "Slovenia", "Latvia", "Estonia"), 
          omit.stat=c("f", "ser"),
          style = "default",
          dep.var.caption  = "Gender Pay Gap",
          dep.var.labels   = " ",
          out="0_FFF_Ceta.html")


#--------------------------------- Visualization of Debt Spending versus Overall Spending --------------------------------------------------------------

#Approach: Ration Debt/Overall Spending 

Government_Debt_Spending_2 <- read_csv("Government_Debt_Spending2.csv")

Government_Debt_Spending_2 <- Government_Debt_Spending_2 %>% mutate(Ratio_Debt_Overall = Public_Debt_Transactions/ Total_Spending)



##FFF_Deta_1
# Countries with high values
Government_Debt_Spending_2  %>% filter(Country == "Ireland" |  Country == "Spain" | Country == "Cyprus") %>% 
  ggplot(aes(x=Year, y = Ratio_Debt_Overall, color = Country))+
  geom_line()+
  xlab(" ") +  
  ylab("Debt/Total Ratio") +
  theme_bw() + 
  #or theme_ipsum()
  ggtitle("Figure 4. Ratio Debt Payments to Total Spending (1)")


##FFF_Deta_2
# Countries with mediocre values
Government_Debt_Spending_2  %>% filter(Country == "Germany"  | Country == "France" |Country == "Slovakia" |  Country == "Slovenia") %>% 
  ggplot(aes(x=Year, y = Ratio_Debt_Overall, color = Country))+
  geom_line()+
  xlab(" ") +  
  ylab("Debt/Total Ratio") +
  theme_bw() + 
  #or theme_ipsum()
  ggtitle("Figure 5. Ratio Debt Payments to Total Spending (2)") 
 

##FFF_Deta_3
# Countries with low values
Government_Debt_Spending_2  %>% filter(Country == "Estonia"  |  Country == "Latvia" | Country == "Netherlands" | Country == "Finland" | Country == "Lithuania" ) %>% 
  ggplot(aes(x=Year, y = Ratio_Debt_Overall, color = Country))+
  geom_line()+
  xlab(" ") +  
  ylab("Debt/Total Ratio") +
  theme_bw() + 
  #or theme_ipsum()
  ggtitle("Figure 6. Ratio Debt Payments to Total Spending (3)")




#---------------------------------Eta Second Model: Government Spending on sector-level --------------------------------------------------------------


G <- dataset %>% filter(Country == "Germany") 
lm_G4 <- lm(GPG_Including_Gov  ~  Spending_Education + GDP_Total+  Year, data = G)

Ire <- dataset %>% filter(Country == "Ireland") 
lm_Ire4 <- lm(GPG_Including_Gov  ~ Spending_Education  + GDP_Total+  Year, data = Ire)

Fr <- dataset %>% filter(Country == "France") 
lm_Fr4 <- lm(GPG_Including_Gov  ~   Spending_Education + GDP_Total+  Year, data = Fr)

Fi <- dataset %>% filter(Country == "Finland") 
lm_Fi4 <- lm(GPG_Including_Gov  ~  Spending_Education  + GDP_Total+  Year, data = Fi)

Sp <- dataset %>% filter(Country == "Spain") 
lm_Sp4 <- lm(GPG_Including_Gov  ~ Spending_Education   + GDP_Total+  Year, data = Sp)

C <- dataset %>% filter(Country == "Cyprus") 
lm_C4 <- lm(GPG_Including_Gov  ~  Spending_Education + GDP_Total+  Year, data = C)

Ne <- dataset %>% filter(Country == "Netherlands") 
lm_Ne4 <- lm(GPG_Including_Gov  ~ Spending_Education  + GDP_Total+  Year, data = Ne)

Lat <- dataset %>% filter(Country == "Latvia") 
lm_Lat4 <- lm(GPG_Including_Gov  ~ Spending_Education  + GDP_Total +  Year, data = Lat)

Lit <- dataset %>% filter(Country == "Lithuania") 
lm_Lit4 <- lm(GPG_Including_Gov  ~ Spending_Education +   GDP_Total +  Year, data = Lit)

Slove <- dataset %>% filter(Country == "Slovenia") 
lm_Slove4 <- lm(GPG_Including_Gov  ~ Spending_Education+ GDP_Total  +  Year, data = Slove)

Slova <- dataset %>% filter(Country == "Slovakia") 
lm_Slova4 <- lm(GPG_Including_Gov  ~ Spending_Education + GDP_Total + Year, data = Slova)

Esto <- dataset %>% filter(Country == "Estonia") 
lm_Esto4 <- lm(GPG_Including_Gov  ~ Spending_Education + GDP_Total + Year, data = Esto)


#---------------------------------------------------------------
  
G <- dataset %>% filter(Country == "Germany") 
lm_G5 <- lm(GPG_Including_Gov  ~    Spending_Health   + GDP_Total+  Year, data = G)

Ire <- dataset %>% filter(Country == "Ireland") 
lm_Ire5 <- lm(GPG_Including_Gov  ~ Spending_Health   + GDP_Total+  Year, data = Ire)

Fr <- dataset %>% filter(Country == "France") 
lm_Fr5 <- lm(GPG_Including_Gov  ~   Spending_Health  + GDP_Total+  Year, data = Fr)

Fi <- dataset %>% filter(Country == "Finland") 
lm_Fi5 <- lm(GPG_Including_Gov  ~    Spending_Health    + GDP_Total+  Year, data = Fi)

Sp <- dataset %>% filter(Country == "Spain") 
lm_Sp5 <- lm(GPG_Including_Gov  ~  Spending_Health   + GDP_Total+  Year, data = Sp)

C <- dataset %>% filter(Country == "Cyprus") 
lm_C5 <- lm(GPG_Including_Gov  ~   Spending_Health  +GDP_Total+  Year, data = C)

Ne <- dataset %>% filter(Country == "Netherlands") 
lm_Ne5 <- lm(GPG_Including_Gov  ~  Spending_Health    + GDP_Total+  Year, data = Ne)

Lat <- dataset %>% filter(Country == "Latvia") 
lm_Lat5 <- lm(GPG_Including_Gov  ~  Spending_Health    + GDP_Total+  Year, data = Lat)

Lit <- dataset %>% filter(Country == "Lithuania") 
lm_Lit5 <- lm(GPG_Including_Gov  ~    Spending_Health + GDP_Total+  Year, data = Lit)

Slove <- dataset %>% filter(Country == "Slovenia") 
lm_Slove5 <- lm(GPG_Including_Gov  ~     Spending_Health  + GDP_Total+  Year, data = Slove)

Slova <- dataset %>% filter(Country == "Slovakia") 
lm_Slova5 <- lm(GPG_Including_Gov  ~     Spending_Health   + GDP_Total+ Year, data = Slova)

Esto <- dataset %>% filter(Country == "Estonia") 
lm_Esto5 <- lm(GPG_Including_Gov  ~     Spending_Health   + GDP_Total+ Year, data = Esto)


#---------------------------------------------------------------

G <- dataset %>% filter(Country == "Germany") 
lm_G6 <- lm(GPG_Including_Gov  ~      Spending_Social_Protection   + GDP_Total+  Year, data = G)

Ire <- dataset %>% filter(Country == "Ireland") 
lm_Ire6 <- lm(GPG_Including_Gov  ~      Spending_Social_Protection + GDP_Total+  Year, data = Ire)

Fr <- dataset %>% filter(Country == "France") 
lm_Fr6 <- lm(GPG_Including_Gov  ~     Spending_Social_Protection +  GDP_Total+  Year, data = Fr)

Fi <- dataset %>% filter(Country == "Finland") 
lm_Fi6 <- lm(GPG_Including_Gov  ~      Spending_Social_Protection   + GDP_Total+  Year, data = Fi)

Sp <- dataset %>% filter(Country == "Spain") 
lm_Sp6 <- lm(GPG_Including_Gov  ~    Spending_Social_Protection   + GDP_Total+  Year, data = Sp)

C <- dataset %>% filter(Country == "Cyprus") 
lm_C6 <- lm(GPG_Including_Gov  ~      Spending_Social_Protection + GDP_Total+  Year, data = C)

Ne <- dataset %>% filter(Country == "Netherlands") 
lm_Ne6 <- lm(GPG_Including_Gov  ~    Spending_Social_Protection   + GDP_Total+  Year, data = Ne)

Lat <- dataset %>% filter(Country == "Latvia") 
lm_Lat6 <- lm(GPG_Including_Gov  ~     Spending_Social_Protection   + GDP_Total+  Year, data = Lat)

Lit <- dataset %>% filter(Country == "Lithuania") 
lm_Lit6 <- lm(GPG_Including_Gov  ~     Spending_Social_Protection  + GDP_Total+  Year, data = Lit)

Slove <- dataset %>% filter(Country == "Slovenia") 
lm_Slove6 <- lm(GPG_Including_Gov  ~    Spending_Social_Protection  + GDP_Total+  Year, data = Slove)

Slova <- dataset %>% filter(Country == "Slovakia") 
lm_Slova6 <- lm(GPG_Including_Gov  ~     Spending_Social_Protection   + GDP_Total+ Year, data = Slova)

Esto <- dataset %>% filter(Country == "Estonia") 
lm_Esto6 <- lm(GPG_Including_Gov  ~    Spending_Social_Protection   + GDP_Total+ Year, data = Esto)

#------------------------------------------------------------------------



own_labels4 =  c("Spending: Education", "GDP","Year")
own_labels5 =  c("Spending: Health", "GDP","Year")
own_labels6 =  c("Spending: Social Protection", "GDP","Year")



# All countries: Education
stargazer(lm_G4, lm_Slova4, lm_Fr4, lm_Sp4, lm_Fi4, lm_Ne4, lm_Lit4, lm_C4,lm_Ire4, lm_Slove4, lm_Lat4, lm_Esto4,
          title="Table 2. Influence of Education Expenditures on Gender Pay Gap", 
          covariate.labels = own_labels4, 
          type = "text",
          column.labels = c("Germany", "Slovakia", "France", "Spain", "Finland", "Netherlands", "Lithuania",  "Cyprus", "Ireland", "Slovenia", "Latvia", "Estonia"), 
          omit.stat=c("f", "ser"),
          style = "default",
          dep.var.caption  = "Gender Pay Gap",
          dep.var.labels   = " ",
          out="0_FFF_Eta_1.html")

# All countries: Health
stargazer(lm_G5, lm_Slova5, lm_Fr5, lm_Sp5, lm_Fi5, lm_Ne5, lm_Lit5, lm_C5,lm_Ire5, lm_Slove5, lm_Lat5, lm_Esto5,
          title="Table 3. Influence of Health Expenditures on Gender Pay Gap", 
          covariate.labels = own_labels5, 
          type = "text",
          column.labels = c("Germany", "Slovakia", "France", "Spain", "Finland", "Netherlands", "Lithuania",  "Cyprus", "Ireland", "Slovenia", "Latvia", "Estonia"), 
          omit.stat=c("f", "ser"),
          style = "default",
          dep.var.caption  = "Gender Pay Gap",
          dep.var.labels   = " ",
          out="0_FFF_Eta_2.html")

# All countries: Social Protection
stargazer(lm_G6, lm_Slova6, lm_Fr6, lm_Sp6, lm_Fi6, lm_Ne6, lm_Lit6, lm_C6,lm_Ire6, lm_Slove6, lm_Lat6, lm_Esto6,
          title="Table 4. Influence of Social Protection Expenditures on Gender Pay Gap", 
          covariate.labels = own_labels6, 
          type = "text",
          column.labels = c("Germany", "Slovakia", "France", "Spain", "Finland", "Netherlands", "Lithuania",  "Cyprus", "Ireland", "Slovenia", "Latvia", "Estonia"), 
          omit.stat=c("f", "ser"),
          style = "default",
          dep.var.caption  = "Gender Pay Gap",
          dep.var.labels   = " ",
          out="0_FFF_Eta_3.html")


