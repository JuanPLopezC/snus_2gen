* Project: Parental Snus Use Initiation in Puberty and Respiratory Outcomes in Offspring Study
* Author: Juan Pablo Lopez-Cervantes
* Description: Main analysis of the association between 
* paternal snus use in puberty and offspring respiratory health.
* Input: RHINESSA_RHINE_data.dta
* Output: regression results

*----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------*

* 1. Load data (Using merged data from RHINESSA/RHINE Cohorts from Swedish study centres)
use "RHINESSA_RHINE_data.dta", clear


*----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------*

* 2. Prepare data: generate variables, exclusions. (OBS. Not every variable was used in the final model, but were potentially useful at some point during the analyses)

**Generating variables for demographic characteristics - parents and offspring**

*Parental demographic characteristics - from RHINE*
gen p_bdate=rhine_1
recast double p_bdate
format %td p_bdate
label variable p_bdate "Parent birthdate"

gen p_byear = yofd(rhine_1)
label variable p_byear "Parent year of birth"

gen p_bmonth = month(rhine_1)
label variable p_bmonth "Parent month of birth"

gen p_age = Age_rhine4
label variable p_age "Parent age"

gen p_sex=rhine_3
recode p_sex 1=0 2=1
label variable p_sex "Parent sex"
label define p_sex 0 "Male" 1 "Female"
label values p_sex p_sex

gen p_bmi = rh4_bmi
recode p_bmi min/18.499999 = 0
recode p_bmi 18.5/24.99999 = 1
recode p_bmi 25.0/29.999999 = 2
recode p_bmi 30.0/61.0 = 3
label variable p_bmi "Parent BMI"
label define p_bmi 0 "<18.5" 1 ">=18.5-25" 2 ">=25-30" 3 ">=30"
label values p_bmi p_bmi


*Offspring demographic characteristics-From RHINESSA*

gen ch_birthdate_ = birthdate
recast double ch_birthdate
format %td ch_birthdate
label variable ch_birthdate "Offspring birthdate from RHINESSA"

gen ch_bdate=birthdate
recast double ch_bdate
format %td ch_bdate
label variable ch_bdate "Offspring birthdate"

gen ch_byear = yofd(birthdate)
replace ch_byear = byearo if ch_byear==. & byearo!=.
label variable ch_byear "Offspring year of birth"

gen ch_bmonth = month(birthdate)
label variable ch_bmonth "Offspring month of birth"

gen ch_age = age 
label variable ch_age "Offspring age"

gen ch_sex=sex_rhinessa
recode ch_sex 1=0 2=1
label variable ch_sex "Offspring sex"
label define ch_sex 0 "Male" 1 "Female"
label values ch_sex ch_sex

gen ch_bmi = bmio_categ2_complete
recode ch_bmi 1=0 2=1 3=2 4=3
label variable ch_bmi "Offspring BMI"
label define ch_bmi 0 "<18.5" 1 ">=18.5-25" 2 ">=25-30" 3 ">=30"
label values ch_bmi ch_bmi


**Generating exposure and outcome variables, and other possible useful covariates. 
///Some of the variables were used to create other variables, but are not directly used in the analysis or tables**

*PARENTS-From RHINE*

*Nicotine-containing products use*

gen p_age_chbdate = datediff(p_bdate, ch_bdate, "year") //Variable to define age of parent when offspring was born//
label variable p_age_chbdate "Parent age when offspring was born"

**Nicotine products**

gen p_nicotprod = rhine_24
label variable p_nicotprod "Use of nicotine products regularly"
label define no_yes_ 0 "No/Never" 1 "Yes/Ever"
label values p_nicotprod no_yes_

**Snus**

gen p_eversnus = 0
replace p_eversnus = 1 if rhine_24==1 & rhine_24_snus >=1 & rhine_24_snus !=.
label variable p_eversnus "Parent ever snus"
label values p_eversnus no_yes_

gen p_agesnus_start = rhine_24_7 if rhine_24_7>0
recode p_agesnus_start 0=.
label variable p_agesnus_start "Age when started snusing"

gen p_agesnus_stop = (rhine_24_8 - p_byear) if rhine_24_snus>0
recode p_agesnus_stop 0=.
label variable p_agesnus_stop "Age when stopped snusing"

gen p_yrsnus_start = (p_age_chbdate - p_agesnus_start) //Variable to define the year when parent started snusing in relation to age of parent when offspring was born//
label variable p_yrsnus_start "Parent age at offspring birth minus snus start"

gen p_yrsnus_stop = (p_age_chbdate - p_agesnus_stop) //Variable to define the year when parent stopped snusing in relation to age of parent when offspring was born//
label variable p_yrsnus_stop "Parent age at offspring birth minus snus stop"

gen p_15snus = . 
replace p_15snus =0 if p_eversnus==0
replace p_15snus = 1 if p_eversnus ==1 & p_agesnus_start <=15
replace p_15snus = 2 if p_eversnus ==1 & p_agesnus_start >15 & p_agesnus_start !=.
label variable p_15snus "Snus debut in puberty (<=15yo)"
label define p_15snus 0 "Never snus" 1 "Snus <=15yo" 2 "Snus >15yo"
label values p_15snus p_15snus

**Smoking**

gen p_eversmoke = . 
replace p_eversmoke = 0 if rhine_22==0 & rhine_23==0
replace p_eversmoke = 1 if rhine_22 ==1 | rhine_23 ==1
recode p_eversmoke .=2
label variable p_eversmoke "Parent ever smoking"
label define p_eversmoke 0 "Never smoke" 1 "Ever smoke" 2 "Unknown"
label values p_eversmoke p_eversmoke

gen p_agesmoke_start = rhine_24_4
label variable p_agesmoke_start "Age when started smoking"

gen p_agesmoke_stop = (rhine_24_5 - p_byear) if rhine_23==1 
recode p_agesmoke_stop 0=.
label variable p_agesmoke_stop "Age when stopped smoking"

gen p_yearsmoking = rhine_24_3
replace p_yearsmoking = (p_age - p_agesmoke_start) if rhine_22==1 & p_yearsmoking==. //to replace for those that are currently smoking and that did not answer amount of years smoking//
label variable p_yearsmoking "Years smoking"

gen p_yrsmoke_start = (p_age_chbdate - p_agesmoke_start) //Variable to define the year when parent started smoking in relation to age of parent when offspring was born//
label variable p_yrsmoke_start "Parent age at offspring birth minus smoking start"

gen p_yrsmoke_stop = (p_age_chbdate - p_agesmoke_stop) //Variable to define the year when parent stopped smoking in relation to age of parent when offspring was born//
label variable p_yrsmoke_stop "Parent age at offspring birth minus smoking stop"

gen p_15smoke = . //Using the cutoff of 15 years - in puberty//
replace p_15smoke =0 if p_eversmoke==0
replace p_15smoke = 1 if p_eversmoke ==1 & p_agesmoke_start <=15
replace p_15smoke = 2 if p_eversmoke ==1 & p_agesmoke_start >15 & p_agesmoke_start !=.
label variable p_15smoke "Smoking in puberty (<=15yo)"
label define p_15smoke 0 "Never smoke" 1 "Smoke <=15yo" 2 "Smoke >15yo"
label values p_15smoke p_15smoke


**Other**

gen p_childasthma = .
replace p_childasthma = 0 if rhine_25==0
replace p_childasthma = 1 if rhine_25==1 & rhine_25_2 <=10
replace p_childasthma = 2 if rhine_25==1 & rhine_25_2 >10 & rhine_25_2!=.
label variable p_childasthma "Parent childhood asthma (<=10yo)"
label define p_childasthma 0 "Never asthma" 1 "Early asthma" 2 "Late asthma"
label values p_childasthma p_childasthma

gen p_hf = rhine_10
label variable p_hf "Parental ever hay fever"
label values p_hf no_yes_

**Mothers' smoking in offspring's childhood - From RHINESSA questionnaires=Offspring answering about their parents**
gen mothersmokeo_complete=mothersmokeo 
replace mothersmokeo_complete= 1 if mothersmoking_childhood_mq18o== 0 & mothersmokeo_complete==. //No smoking in childhood//
replace mothersmokeo_complete= 2 if mothersmoking_childhood_mq18o== 1 & mothersmokeo_complete==. //Smoking in childhood//
labe variable mothersmokeo_complete "Mothers smoking in offsprings childhood"
label values mothersmokeo_complete no_yes_


*OFFSPRING-From RHINESSA*

**Nicotine-containing products use-From RHINESSA**

gen ch_nicotprod = .
replace ch_nicotprod = 0 if nicotineprodnowo==1 & nicotineprodprevo==1
replace ch_nicotprod = 1 if nicotineprodnowo==2 | nicotineprodprevo==2
label variable ch_nicotprod "Ever use of nicotine products"
label values ch_nicotprod no_yes_
	
gen ch_nicotprod_other = .
replace ch_nicotprod_other = 0 if nicotineprodnowo==1 & nicotineprodprevo==1
replace ch_nicotprod_other = 1 if nicotineprodnowo==2 & nicotinepatcho==2 | nicotineprodprevo==2 & nicotinepatcho==2
label variable ch_nicotprod_other "Ever use of nicotine products diff. to snus"
label values ch_nicotprod_other no_yes_

**Snus**

gen ch_eversnus = 0
replace ch_eversnus=1 if snuffo == 2 | snus_mq46_6_1o==1 & snuffo==. //This variable will help us make the variable ch_15snus//
label variable ch_eversnus "Offspring ever snus"
label values ch_eversnus no_yes_

gen ch_15snus = .
replace ch_15snus =0 if ch_eversnus==0
replace ch_15snus =1 if ch_eversnus==1 & agestart_snus_complete <=15
replace ch_15snus=2 if ch_eversnus==1 & agestart_snus_complete >15 & agestart_snus_complete!=.
label variable ch_15snus "Offspring snus debut in puberty"
label values ch_15snus p_15snus

**Smoking**

gen ch_eversmoke = .
replace ch_eversmoke=0 if smokero == 1 | exsmokero ==1 | smoke_1year_mq46o ==0 //Taking into consideration both current and ex-smokers//
replace ch_eversmoke=1 if smokero == 2 | exsmokero ==2 | smoke_1year_mq46o ==1
replace ch_eversmoke=. if smokero==. & exsmokero==. & smoke_1year_mq46o==.
recode ch_eversmoke .=2
label variable ch_eversmoke "Offspring ever smoke"
label define ch_eversmoke 0 "Never" 1 "Yes/Ever" 2 "Unknown"
label values ch_eversmoke ch_eversmoke

gen ch_current_smoke = smokero
replace ch_current_smoke =2 if smoke_currently_mq46_3o==1 & smokero ==.
recode ch_current_smoke 1=0 2=1
label variable ch_current_smoke "Offspring current smoking"
label values ch_current_smoke no_yes_

**Asthma, asthma symptoms, allergies**

gen everasthma_complete =everasthmao
replace everasthma_complete = 1  if everasthma_mq11o ==0 & everasthma_complete==. //This will help us make var ch_evera. This puts together answers from the RHINESSA questionnaires//
replace everasthma_complete =2 if everasthma_mq11o ==1 & everasthma_complete==.
recode everasthma_complete .=3	

gen ch_evera = .
replace ch_evera = 0 if everasthma_complete==1
replace ch_evera = 1 if everasthma_complete ==2
label variable ch_evera "Offspring ever asthma"
label values ch_evera no_yes_

gen ch_currenta = .
replace ch_currenta = 0 if attack_asthma_complete ==1 & medication_asthma_complete ==1 | attack_asthma_complete==1 | medication_asthma_complete==1
replace ch_currenta = 1 if attack_asthma_complete ==2 & medication_asthma_complete ==2 | attack_asthma_complete==2 | medication_asthma_complete==2
label variable ch_currenta "Offspring current asthma"
label values ch_currenta no_yes_

gen ch_hf = hayfevero
replace ch_hf=0 if nasalallergies_mq12o==0 & ch_hf==. //This puts together answers from the RHINESSA questionnaires//
replace ch_hf=1 if nasalallergies_mq12o==1 & ch_hf==.
recode ch_hf . =2
recode ch_hf 2=.
label variable ch_hf "Offspring ever hay fever"
label values ch_hf no_yes_

gen ch_everala = . //Coded as never asthma, ever allergic asthma and ever non-allergic asthma//
replace ch_everala =0 if ch_evera ==0
replace ch_everala =1 if ch_evera==1 & ch_hf==1
replace ch_everala =2 if ch_evera==1 & ch_hf==0
label variable ch_everala "Offspring ever allergic and non-allergic asthma"
label define ch_everala 0 "Never asthma" 1 "Ever allergic asthma" 2 "Ever non-allergic asthma"
label values ch_everala ch_everala

gen ch_currentala = .
replace ch_currentala = 0 if ch_currenta ==0
replace ch_currentala = 1 if ch_currenta ==1 & ch_hf==1
replace ch_currentala = 2 if ch_currenta ==1 & ch_hf==0
label variable ch_currentala "Offspring current allergic and non-allergic asthma"
label define ch_currentala 0 "No current asthma" 1 "Current allergic asthma" 2 "Current non-allergic asthma"
label values ch_currentala ch_currentala \

gen ch_prodcough2 = .
replace ch_prodcough2 = 0 if productiveo==0 | productive2yo==1 | productive3mso==1
replace ch_prodcough2 = 1 if productiveo==1  & productive3mso==2 | productiveo==1 & productive2yo==2
label variable ch_prodcough2 "Offspring productive cough 3mo or 3mo+2y " //This will define chronic bronchitis, putting together productive cough and time with productive cough//
label values ch_prodcough2 no_yes_

gen ch_eczema = eczemao
recode ch_eczema 1=0 2=1
replace ch_eczema = evereczema_mq15o if ch_eczema == . | ch_eczema==0 & evereczema_mq15o==1
label variable ch_eczema "Offspring ever eczema"
label values ch_eczema no_yes_

**Following set is to create the asthma symptom score based on Pekkanen et al. 2005.**

* 1) These were created to put together RHINESSA questionnaires in one variable.
gen wheezeo_complete=wheezeo //Useful to make wheeze_breath_categ_complete//
replace wheezeo_complete =1 if wheeze12mo_mq1o ==0 & wheezeo_complete==. 
replace wheezeo_complete =2 if wheeze12mo_mq1o ==1 & wheezeo_complete==.
recode wheezeo_complete .=3	 

gen whbo_complete=whbo //Useful to make wheeze_breath_categ_complete//
replace whbo_complete = 1 if wheeb_mq1_1o ==0 & whbo_complete==.
replace whbo_complete = 2 if wheeb_mq1_1o ==1 & whbo_complete==.
recode whbo_complete .=3

gen wheeze_breath_categ_complete=.
replace wheeze_breath_categ_complete = 0 if wheezeo_complete ==1  & whbo_complete ==1
replace wheeze_breath_categ_complete = 1 if wheezeo_complete==2 & whbo_complete ==2
recode wheeze_breath_categ_complete .=0

gen wheeze_nocold12mo_categ=. //Useful to make wheeze_nocold12mo_categ_complete//
replace wheeze_nocold12mo_categ =0 if whnocoo ==1 & wheezeo ==1
replace wheeze_nocold12mo_categ =1 if whnocoo ==2 & wheezeo ==2

gen whnocoo_complete=whnocoo //Useful to make wheeze_nocold12mo_categ_complete//
replace whnocoo_complete =1 if wheeco_mq1_2o==0 & whnocoo_complete==.
replace whnocoo_complete =2 if wheeco_mq1_2o==1 & whnocoo_complete==.
recode whnocoo_complete .=3

gen wheeze_nocold12mo_categ_complete=.
replace wheeze_nocold12mo_categ_complete =0 if whnocoo_complete ==1 & wheezeo_complete ==1
replace wheeze_nocold12mo_categ_complete =1 if whnocoo_complete ==2 & wheezeo_complete ==2
recode wheeze_nocold12mo_categ_complete .=0

gen tighto_complete=tighto 
replace tighto_complete=1 if tight12mo_mq2o==0 & tighto_complete==.
replace tighto_complete=2 if tight12mo_mq2o==1 & tighto_complete==.
recode tighto_complete .=3

gen noctbreo_complete=noctbreo
replace noctbreo_complete=1 if woken_attack12mo_mq5o==0 & noctbreo_complete==.
replace noctbreo_complete=2 if woken_attack12mo_mq5o==1 & noctbreo_complete==.
recode noctbreo_complete . =3

gen noctcougho_complete=noctcougho
replace noctcougho_complete=1 if woken_attackcough12mo_mq6o==0 & noctcougho_complete==.
replace noctcougho_complete=2 if woken_attackcough12mo_mq6o==1 & noctcougho_complete==.
recode noctcougho_complete .=3

* 2) Recoding the previous variables before making the score*
gen tighto_complete_score = tighto_complete
recode tighto_complete_score 3=. 1=0 2=1

gen noctbreo_complete_score = noctbreo_complete
recode noctbreo_complete_score 3=. 1=0 2=1

gen noctcougho_complete_score = noctcougho_complete
recode noctcougho_complete_score 3=. 1=0 2=1

gen everasthma_complete_score =everasthma_complete
recode everasthma_complete_score 3=. 1=0 2=1

gen attack_asthma_complete_score = attack_asthma_complete
recode attack_asthma_complete_score 3=. 1=0 2=1

gen medication_asthma_complete_score = medication_asthma_complete
recode medication_asthma_complete_score 3=. 1=0 2=1

* 3) Making the score-continuous*

gen asthma_score2_cont = tighto_complete_score + noctbreo_complete_score + noctcougho_complete_score + everasthma_complete_score + attack_asthma_complete_score + medication_asthma_complete_score + wheeze_breath_categ_complete + wheeze_nocold12mo_categ_complete

* 4) Making the score categorized in <3 symptoms/>=3 symptoms*

gen ch_asympt_categ = . //This will categorize the continuous asthma score that we created above//
replace ch_asympt_categ= 0 if asthma_score2_cont<=2
replace ch_asympt_categ= 1 if asthma_score2_cont>=3 & asthma_score2_cont !=.
label variable ch_asympt_categ "Offspring asthma symptoms categorical"
label define ch_asympt_categ 0 "<3 sympt" 1 ">=3 sympt"
labe values ch_asympt_categ ch_asympt_categ

* 5) Using same score but now including presence/abscence of hay fever*

gen ch_asympt_categ_hf = .
replace ch_asympt_categ_hf = 0 if ch_asympt_categ==0
replace ch_asympt_categ_hf = 1 if ch_asympt_categ==1 & ch_hf==1
replace ch_asympt_categ_hf = 2 if ch_asympt_categ==1 & ch_hf==0
recode ch_asympt_categ_hf .=0 //all of them have either have hf or symptoms//
label variable ch_asympt_categ_hf "Offspring asthma sympt with hay fever"
label define ch_asympt_categ_hf 0 "<3 sympt" 1 ">=3 sympt with hf" 2 ">=3 sympt without hf"
label values ch_asympt_categ_hf ch_asympt_categ_hf

**Ends set**

gen ch_wheeze12_hf = .
replace ch_wheeze12_hf=0 if wheezeo_complete==1
replace ch_wheeze12_hf=1 if wheezeo_complete==2 & ch_hf==1
replace ch_wheeze12_hf=2 if wheezeo_complete==2 & ch_hf==0
recode ch_wheeze12_hf .=0
label variable ch_wheeze12_hf "Offspring wheeze 12mo with hay fever"
label define ch_wheeze12_hf 0 "No wheeze" 1 "Wheeze with hay fever" 2 "Wheeze without hay fever"
label values ch_wheeze12_hf ch_wheeze12_hf


**Other**

gen ch_educ = educationo
recode ch_educ 1=0 2=0 3=1 .=2
label variable ch_educ "Offspring education"
label define ch_educ 0 "Primary/Secondary" 1 "High" 2 "Unknown"
label values ch_educ ch_educ


*GRANDPARENTS*

**Grandparental education**
 
gen gp_educ = .
replace gp_educ = rh2_q45
replace gp_educ = rh2_q46 if rh2_q46>rh2_q45 & rh2_q46!=. | rh2_q45==.
recode gp_educ 1=0 2=0 3=1 .=2
label variable gp_educ "Grandparent education"
label define gp_educ 0 "Primary/Secondary" 1 "High" 2 "Unknown"
label values gp_educ gp_educ


*----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------*

* 3. Descriptive statistics

**For tables with general characteristics in the paternal line**
*First column - All participants - n=1090*
tabstat ch_age if p_15snus!=., by(p_sex) stat(median min max)
tab ch_sex p_sex if p_15snus!=., col
tab ch_educ p_sex if p_15snus!=., col
tab ch_currentala p_sex if p_15snus!=., col
tab ch_asympt_categ_hf p_sex if p_15snus!=., col
tab ch_wheeze12_hf p_sex if p_15snus!=., col
tab ch_prodcough2 p_sex if p_15snus!=., col
tab ch_hf p_sex if p_15snus!=., col
tab ch_eczema p_sex if p_15snus!=., col
tab ch_current_smoke p_sex if p_15snus!=., col
tab ch_15snus p_sex if p_15snus!=., col 
tab mothersmokeo_complete p_15snus if p_sex==0, col
tabstat p_age if p_15snus!=., by(p_sex) stat (median min max)
tab p_educ p_sex if p_15snus!=., col
tab p_15smoke p_sex if p_15snus!=., col
tab p_childasthma p_sex if p_15snus!=., col
tab p_hf if p_15snus!=. & p_sex==0
tab gp_educ p_sex if p_15snus!=., col


*By snus use initiation*
tabstat ch_age if p_sex==0, by(p_15snus) stat(median min max)
tab ch_sex p_15snus if p_sex==0, col
tab ch_educ p_15snus if p_sex==0, col
tab ch_currentala p_15snus if p_sex==0, col
tab ch_asympt_categ_hf p_15snus if p_sex==0, col
tab ch_wheeze12_hf p_15snus if p_sex==0, col
tab ch_prodcough p_15snus if p_sex==0, col
tab ch_prodcough2 p_15snus if p_sex==0, col
tab ch_hf p_15snus if p_sex==0, col
tab ch_eczema p_15snus if p_sex==0, col
tab ch_eczema12 p_15snus if p_sex==0, col
tab ch_current_smoke p_15snus if p_sex==0, col
tab ch_15snus p_15snus if p_sex==0, col 
tab mothersmokeo_complete p_15snus if p_sex==0, col
tabstat p_age if p_sex==0, by(p_15snus) stat (median min max)
tab p_educ p_15snus if p_sex==0, col
tab p_15smoke p_15snus if p_sex==0, col
tab p_childasthma p_15snus if p_sex==0, col
tab p_hf p_15snus if p_sex==0, col
tab gp_educ p_15snus if p_sex==0, col


**For table by Swedish study centres**

*First column - All participants - n=2511*
tabstat ch_age if p_15snus!=.,  stat(median min max)
tab ch_sex if p_15snus!=.
tab ch_currentala if p_15snus!=.
tab ch_asympt_categ_hf if p_15snus!=.
tab ch_wheeze12_hf if p_15snus!=.
tab ch_prodcough2 if p_15snus!=. 
tab ch_hf if p_15snus!=.
tab ch_eczema if p_15snus!=.
tab p_15snus

*By centre*
tabstat ch_age if p_15snus!=., by(centre) stat(median min max)
tab ch_sex centre if p_15snus!=., col
tab ch_currentala centre if p_15snus!=., col
tab ch_asympt_categ_hf centre if p_15snus!=., col
tab ch_wheeze12_hf centre if p_15snus!=., col
tab ch_prodcough2 centre if p_15snus!=., col
tab ch_hf centre if p_15snus!=., col
tab ch_eczema centre if p_15snus!=., col
tab p_15snus centre, col



*----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------*

* 4. Main analysis: logistic regression 


**Main model - association between parental snus use initiation around puberty and offspring outcomes: 
///including only those offspring with paternal exposure. Mixed-effect model (melogit) accounting for families (rhineid) and clustering for study centres. 

*Crude model*
melogit ch_currentala i.p_15snus if p_sex==0 & ch_currentala!=2 || rhineid:, or vce (cluster centre)
melogit ch_currentala i.p_15snus if p_sex==0 & ch_currentala!=1 || rhineid:, or vce (cluster centre)
melogit ch_asympt_categ_hf i.p_15snus if p_sex==0 & ch_asympt_categ_hf!=2 || rhineid:, or vce (cluster centre)
melogit ch_asympt_categ_hf i.p_15snus if p_sex==0 & ch_asympt_categ_hf!=1 || rhineid:, or vce (cluster centre)
melogit ch_prodcough2 i.p_15snus if p_sex==0 || rhineid:, or vce (cluster centre)
melogit ch_hf i.p_15snus if p_sex==0 || rhineid:, or vce (cluster centre)
melogit ch_eczema i.p_15snus if p_sex==0 || rhineid:, or vce (cluster centre)

*Adjusted model: for parental age, parental childhood asthma, offspring currents smoke, offspring sex and grandparents education*

melogit ch_currentala i.p_15snus i.p_15smoke p_age ch_age i.p_childasthma ch_current_smoke i.gp_educ ch_sex if p_sex==0 & ch_currentala!=2 || rhineid:, or vce (cluster centre)
melogit ch_currentala i.p_15snus i.p_15smoke p_age ch_age i.p_childasthma ch_current_smoke i.gp_educ ch_sex if p_sex==0 & ch_currentala!=1 || rhineid:, or vce (cluster centre)
melogit ch_asympt_categ_hf i.p_15snus i.p_15smoke p_age ch_age i.p_childasthma ch_current_smoke i.gp_educ ch_sex if p_sex==0 & ch_asympt_categ_hf!=2 || rhineid:, or vce (cluster centre)
melogit ch_asympt_categ_hf i.p_15snus i.p_15smoke p_age ch_age i.p_childasthma ch_current_smoke i.gp_educ ch_sex if p_sex==0 & ch_asympt_categ_hf!=1 || rhineid:, or vce (cluster centre)
melogit ch_prodcough2 i.p_15snus i.p_15smoke p_age ch_age i.p_childasthma ch_current_smoke i.gp_educ ch_sex if p_sex==0 || rhineid:, or vce (cluster centre)
melogit ch_hf i.p_15snus i.p_15smoke p_age ch_age i.p_childasthma ch_current_smoke i.gp_educ ch_sex if p_sex==0 || rhineid:, or vce (cluster centre)
melogit ch_eczema i.p_15snus i.p_15smoke p_age ch_age i.p_childasthma ch_current_smoke i.gp_educ ch_sex if p_sex==0 || rhineid:, or vce (cluster centre)

*----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------*

* 5. Sensitivity analyses: 

**Analysis among offspring who did not start snus use in puberty: Mixed-effect model accounting for families (rhineid) 
///and clustering for study centres. Adjusted for parental age, parental childhood asthma, offspring currents smoke, 
///offspring sex and grandparents education**

melogit ch_currentala i.p_15snus i.p_15smoke p_age ch_age i.p_childasthma ch_current_smoke i.gp_educ ch_sex if p_sex==0 & ch_currentala!=2 & ch_15snus!=1 || rhineid:, or vce (cluster centre)
melogit ch_currentala i.p_15snus i.p_15smoke p_age ch_age i.p_childasthma ch_current_smoke i.gp_educ ch_sex if p_sex==0 & ch_currentala!=1 & ch_15snus!=1 || rhineid:, or vce (cluster centre)
melogit ch_asympt_categ_hf i.p_15snus i.p_15smoke p_age ch_age i.p_childasthma ch_current_smoke i.gp_educ ch_sex if p_sex==0 & ch_asympt_categ_hf!=2 & ch_15snus!=1 || rhineid:, or vce (cluster centre)
melogit ch_asympt_categ_hf i.p_15snus i.p_15smoke p_age ch_age i.p_childasthma ch_current_smoke i.gp_educ ch_sex if p_sex==0 & ch_asympt_categ_hf!=1 & ch_15snus!=1 || rhineid:, or vce (cluster centre)
melogit ch_prodcough2 i.p_15snus i.p_15smoke p_age ch_age i.p_childasthma ch_current_smoke i.gp_educ ch_sex if p_sex==0 & ch_15snus!=1  || rhineid:, or vce (cluster centre)
melogit ch_hf i.p_15snus i.p_15smoke p_age ch_age i.p_childasthma ch_current_smoke i.gp_educ ch_sex if p_sex==0 & ch_15snus!=1  || rhineid:, or vce (cluster centre)
melogit ch_eczema i.p_15snus i.p_15smoke p_age ch_age i.p_childasthma ch_current_smoke i.gp_educ ch_sex if p_sex==0 & ch_15snus!=1 || rhineid:, or vce (cluster centre)


**Analysis among offspring of fathers without rhinitis/hayfever**
melogit ch_currentala i.p_15snus i.p_15smoke p_age ch_age i.p_childasthma ch_current_smoke i.gp_educ ch_sex if p_sex==0 & ch_currentala!=2 & p_hf!=1 || rhineid:, or vce (cluster centre)
melogit ch_currentala i.p_15snus i.p_15smoke p_age ch_age i.p_childasthma ch_current_smoke i.gp_educ ch_sex if p_sex==0 & ch_currentala!=1 & p_hf!=1 || rhineid:, or vce (cluster centre)
melogit ch_asympt_categ_hf i.p_15snus i.p_15smoke p_age ch_age i.p_childasthma ch_current_smoke i.gp_educ ch_sex if p_sex==0 & ch_asympt_categ_hf!=2 & p_hf!=1 || rhineid:, or vce (cluster centre)
melogit ch_asympt_categ_hf i.p_15snus i.p_15smoke p_age ch_age i.p_childasthma ch_current_smoke i.gp_educ ch_sex if p_sex==0 & ch_asympt_categ_hf!=1 & p_hf!=1 || rhineid:, or vce (cluster centre)
melogit ch_prodcough2 i.p_15snus i.p_15smoke p_age ch_age i.p_childasthma ch_current_smoke i.gp_educ ch_sex if p_sex==0 & p_hf!=1  || rhineid:, or vce (cluster centre)
melogit ch_hf i.p_15snus i.p_15smoke p_age ch_age i.p_childasthma ch_current_smoke i.gp_educ ch_sex if p_sex==0 & p_hf!=1  || rhineid:, or vce (cluster centre)
melogit ch_eczema i.p_15snus i.p_15smoke p_age ch_age i.p_childasthma ch_current_smoke i.gp_educ ch_sex if p_sex==0 & p_hf!=1  || rhineid:, or vce (cluster centre)


*----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------*

* 6. E-values for the main adjusted model

evalue or #, lcl (#) ucl (#) //This is the command. I used the OR, lower and upper limit of the confidence interval from the output of the adjusted main model above//


*----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------*

*7. Figures*

**Map of the prevalence of use of snus in puberty by study centre-Supp. figure 
//(Obs. Different database with all the RHINESSA offspring who have complete information on parental snus use in RHINE from all study centers)**
tab p_15snus centre, col


*Scatter plot of outcomes-Using OR and CI from main adjusted model and all outcomes*

set scheme s1color

* Forest plot with linear x-axis
twoway ///
    (rcap ul ll y if group=="In puberty", horizontal lwidth(thin) lcolor(blue)) ///
    (scatter y or if group=="In puberty", msymbol(circle) mcolor(blue) msize(small)) ///
    (rcap ul ll y if group=="After puberty", horizontal lwidth(thin) lcolor(orange)) ///
    (scatter y or if group=="After puberty", msymbol(circle) mcolor(orange) msize(small)) ///
    , ///
    xline(1, lpattern(dash) lcolor(gs8)) ///
    xlabel(0(1)6, labsize(small)) ///
    ylab(1.25 "Eczema" ///
         2.75 "Rhinitis" ///
         4.25 "Chronic bronchitis" ///
         5.75 "≥3 asthma symptoms without allergy" ///
         7.25 "≥3 asthma symptoms with allergy" ///
         8.75 "Non-allergic asthma" ///
         10.25 "Allergic asthma", ///
         angle(0) labsize(small)) ///
    legend(order(2 "In puberty" 4 "After puberty") ///
           size(small) position(6)) ///
    xtitle("Odds Ratio") ///
    ytitle("") ///
    graphregion(color(white))
	
	graph export "forest_plot_snus.png", width(2000) replace






