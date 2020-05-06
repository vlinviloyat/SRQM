* Stata setup
* -----------

* Check that the -fre- and -estout- command is installed.
which fre
which estout

* Allow Stata to scroll through the results.
set more off

/* ------------------------------------------ SRQM Assignment 1 ----------------

 GROUP:  T. Bosch, N. Dubler, V. Lin, 

 TOPIC:  Country-Level Determinants of Ratio of Women in the Workforce

 DATA:   Quality of Government (2016)

 DATE:   19 March 2020

 SUMMMARY
   We study variations in the ratio of women in the workplace relative to men, 
   in order to show how differences observed between a country's level of 
   education, economic growth, conservatism, and even employment in certain 
   sectors might influence workforce representation inequalities.
 
 QUESTIONS

 We address the following research question:
 Which country-level social and economic factors can explain cross-country
 variations in the share of women in the labor force wordwide?
  
 HYPOTHESES

 - (H1): The higher the GDP per-capita of a country, the higher the share of women 
 in the workforce.  .
 

 - (H2): The higher the fertility rate in a country, the lower the share of women
 in the workforce.
 

 - (H3): The more years of women's secondary schooling, the higher the share of women
 in the workforce.
 
 
 - (H4): The higher the rates of Catholicism or Islam belief in a country, 
 the lower the share of women in the workforce.
 
 
 - (H5): The higher the share of employment in agriculture relative to a countryís
 total employment, the higher the share of women in the workforce.
 
 
 Our variables come from the 2016 Quality of Government dataset (qog2016).
 The sample used in the analysis contains = 91 country observations.
 
     
----------------------------------------------------------------------------- */
* ====================
* = DATA EXPLORATION =
* ====================

* Load QOG 2016 dataset.
use data/qog2016, clear

* Summary of variables used in this research design.
d wef_wlf gle_cgdpc une_tfr bl_asys15f lp_catho80 lp_muslim80 wdi_empagr ///
wdi_conprev 


* I) Dependent variable:  Women in labor force, ratio to men

* MEASURES -- Ratio of women to men in a given country's labor force
* CODEBOOK -- QOG 2016, page 841

d wef_wlf

* Rename our DV to something more understandable
ren wef_wlf workforceratio

* The variable at hand illustrates the relative importance of women in the workforce
* compared to men, but the ratio measurement is not intuitively clear. A ratio of 0.6 
* of women in the workforce means that, say,  where there are 100 men in a country's
* workforce, there would be only 60 women. Any ratio below 1 thus implies that there are fewer
* women working then men. But to make the relative importance of women in the 
* workforce more intuitively understandeable, we generate a new variable, 
* which will show us the percentage of women as a share of the total workforce.
gen workforce = workforceratio / (1 + workforceratio)
la var workforce "Share of women in workforce"

* Because the DV is continuous, we use the -summarize- command to describe it.
su workforce, d

* For all future operations, we rename the country variable to "country" 
ren cname country

* Let's now sort the data by this new metric.
sort workforce

* We illustrate the countries where the share of women in the labor force is below 25%.
li country workforce if workforce < .25

* Show countries with the share of women in the labor force above 0.48.
li country workforce if workforce > .48

* The end of the listing shows a relatively high number of missing values (46 missing values). 

* This does not prevent us from continuing our analysis, but it is something to be kept in mind
* throughout our work, since missing values decrease the representativeness of our study. 
* We critically address these issues in our paper.

* For now, let's obtain the percentage of countries for which our DV is missing,
* by creating a 'missing values' dummy.

gen dv_missing = mi(workforce)
tab dv_missing

* Some 24% of missing data appear from this table. While this may not represent a complete,
* ideal data set, the analysis remains relevant since there are more than three-quarters of
* all countries worldwide for which we do have a non-missing measurement of women in the 
* workforce (n = 148).

* VISUALIZATION -- Create a histogram for the distribution of women in workforce.
hist workforce, freq normal ///
	name(workforce, replace)
	
* The DV looks rather skewed in the hsitogram, a finding that is also confirmed when
* reading the results from the 'summarize' command of the variable. We deal with the issue of
* skewness later below. 
	
* Create boxplot
gr hbox workforce, ///
	name(workforce_boxplot, replace)
	

*II) Independent variables
* ---------------------

fre gle_cgdpc une_tfr bl_asys15f lp_catho80 lp_muslim80 wdi_empagr

* IV (1) -- Gross Domestic Product per Capita (Current Prices)
* ------------------------------------------------------------

* MEASURES -- economic wealth at the country-level, divided by population
* CODEBOOK -- QOG 2016, page 300

d gle_cgdpc

* Rename the variable to something understandable.
ren gle_cgdpc GDPcapita

* Decription of the IV. 
su GDPcapita, d

* Histogram.
hist GDPcapita, freq normal ///
  name(gdp, replace)
  
* Note that there are 191 observations for this IV. This is a very complete number,
* as only three country cases do not have data for this variable.

* IV (2) -- Fertility
* --------------------------

* MEASURES -- fertility rate, total (births per woman)
* CODEBOOK -- QOG 2016, page 597
d une_tfr

* Rename the variable to something understandable.
ren une_tfr fertility

* Decription of the IV. 
su fertility, d

* Histogram.
hist fertility, freq normal ///
  name(fertility, replace)

* Note that this variable also has missing values - 11 country cases have no data for
* the fertility rate. As is always the case with missing values in the data set,
* this means that the sample of countries for which we have all variables measured 
* is going to decrease.

* IV (3) -- Female Education Levels
* ---------------------------------

* MEASURES -- Average Years of Secondary Schooling, Female (15+)
* CODEBOOK -- QOG 2016, page 95

d bl_asys15f

* Rename the variable to something understandable.
ren bl_asys15f education

* Decription of the IV. 
su education, d

* Again, this variable has a relatively high amount of missing values, which means that
* the sample of countries for which we have all variables measured is going to further
* decrease.

* Histogram.
hist education, freq normal ///
  name(education, replace)
  
* IV (4) -- Religion: Catholic
* ---------------------------------

* MEASURES -- Religion: Catholic: Catholics as percentage of population in 1980
* CODEBOOK -- QOG 2016, page 412

d lp_catho80

* Rename the variable to something understandable.
ren lp_catho80 catholic

* Decription of the IV. 
su catholic, d

* For the share of catholics among the population, the dataset delivers 150 valid
* country cases. This means that approximately 20% of the countries do not have data
* on this measure, which will further decrease the number of vountries for which whe have
* overall overlapping data.

* Histogram.
hist catholic, freq normal ///
  name(catholic, replace)


* IV (5) -- Religion: Muslim
* -------------------------------
* MEASURES -- Muslim as percentage of population in 1980
* CODEBOOK -- QOG 2016, page 413
d lp_muslim80 

* Rename the variable to something understandable.
ren lp_muslim80 muslim

* Decription of the IV. 
su muslim, d

* Just as with the "catholic" variable above, there are 150 valid country cases.

* Histogram.
hist muslim, freq normal ///
  name(muslim, replace)
  
* Both the histogramm for the "catholic" variable and the "muslim" variable are
* very left-skewed - something that we address later in this do-file.
  
  
* IV (6) -- Employment in agriculture (% of total employment)
* ------------------------------------------------------------

* MEASURES -- Employment is defined as persons above a specified age who 
* performed any work at all for pay or profit in the agriculture sector and 
* includes farming, hunting, forestry, and fishing.
* CODEBOOK -- QOG 2016, page 657

d wdi_empagr

* Rename the variable to something understandable.
ren wdi_empagr agriculture

* Decription of the IV. 
su agriculture, d

* Histogram.
hist agriculture, freq normal ///
  name(agriculture, replace)
    
  
* Additional considered variable:
* -- Contraceptive prevalence (% of women ages 15-49)
* ------------------------------------------------------------
* MEASURES -- Contraceptive prevalence rate is the percentage of women who are 
* practicing, or whose sexual partners are practicing, any form of contraception.
* It is usually measured for married women ages 15-49 only.

* CODEBOOK -- QOG 2016, page 643

* We would also be interested in analyzing to what extent and how the use of
* contraceptives in different countries may influence the share of women
* in the workforce. We depict this variable as usual.

d wdi_conprev

* Rename the variable to something understandable.
ren wdi_conprev contraceptive

* Decription of the IV. 
su contraceptive, d

* Histogram.
hist contraceptive, freq normal ///
  name(contraceptive, replace)
  
* The 'summarize' command shows that there are only 79 valid country case observations
* for this variable. This poses a significant problem to our analysis because it drastically 
* decreases the number of countries for which all variables would be available.
* Let's explore in more detail to what extent the inclusion of the "contraceptive prevalence"
* variable would distort our overall analysis.
 
* Because we are using country-level data, the next command is used with the
* 'freq' option to show missing data in frequencies instead of percentages.
misstable pat workforce GDPcapita fertility education catholic muslim agriculture ///
contraceptive, freq

* The results demonstrate that if we include in our analysis the DV and all the IVs so far
* discussed, there would be only 42 countries for which all variables are available.
* Moreover, the table demonstrates that the missing values for the "contraceptive" variable
* alone is responsible for 49 missing values in the sample.

* Let's explore what this means in terms of percentage points.
misstable pat workforce GDPcapita fertility education catholic muslim agriculture ///
contraceptive

* Inclusion of the "contraceptive" variable alone deprieves us of an additional 25% of
* all our observations. We consider that the inclusion of this variable strongly distorts the overall
* representativeness of our study, so we will drop the "contraceptive variable from our further
* analysis.

li country if !mi(workforce, GDPcapita, fertility, education, catholic, muslim, agriculture, contraceptive)

li country if !mi(workforce, GDPcapita, fertility, education, catholic, muslim, agriculture)

* As the above commands demonstrate, dropping the "contraceptive" variable
* from our sample increases the number of valid observations
* from 42 to 91.


* END of dealing with missing values.


* RE-CODING OF VARIABLES
* ----------------------------

* Since all the IVs are continuous, we will group them into observation groups
* for the sake of overview and later data exploration.

* We being by recoding the DV to groups by cutting the data to intervals 
* according to the percentiles. 
gen workforce4: workforce4 = irecode(workforce, 0, .3747195 , .4385278, .4685358, .5139225, .)  // quick recode
table workforce4, c(min workforce max workforce n workforce)              						// check result
la def workforce4 0 "17%-37.2%" 1 "37.5%-43.9%" 2 "44.2%-46.7%" 3 "46.9%-51.2%" 4 "51.3% +" 	 // value labels
la var workforce4 "Workforce (5 categories)" 													// label result
fre workforce4  									 											// final result

* We then recode the continuous fertility variable into different groups. 
recode fertility ///
	(min/0.49 = 0 "0") ///
	(0.5/1.49 = 1 "1") ///
	(1.5/2.49 = 2 "2") ///
	(2.5/3.49 = 3 "3") ///
	(3.5/4.49 = 4 "4") ///
	(4.5/max = 5 "5+") ///
	(else = .), gen(fertility6)
la var fertility6 "Fertility Rate (6 groups)"

hist fertility6, freq normal ///
	name(fertility6, replace) 

* We then recode the continuous GDP per capita variable into different 
* income groups.  We group per capita income into four income levels, following 
* the categorization of the World Bank.

recode GDPcapita ///
	(0/1025.99 = 1 "low income") ///
	(1026/3995.99 = 2 "low-middle income") ///
	(3996/12375.99 = 3 "upper-middle income") ///
	(12376/max = 4 "upper income") ///
	(else = .), gen(GDPcapita4)
la var GDPcapita4 "Income categories(4 groups)"

hist GDPcapita4, freq normal ///
	name(GDP_capita_catagories, replace) 

* Next, we recode the religion variables ("muslim" and "catholic" share of the 
* population into for categories.
	
recode muslim ///
	(min/24.9999 = 1 "low minority") ///
	(25/50.99999 = 2  "minority") ///
	(50/74.9999 = 3 "majority") ///
	(75/max = 4 "high majority") ///
	(else = .), gen(muslim4)
la var muslim4 "Muslim Percentage (4 groups)"

recode catholic ///
	(min/24.9999 = 1 "low minority") ///
	(25/50.99999 = 2  "minority") ///
	(50/74.9999 = 3 "majority") ///
	(75/max = 4 "high majority") ///
	(else = .), gen(catholic4)
la var catholic4 "Catholic Percentage (4 groups)"	

* We group the average years of women's secondary schooling into groups of
* completed school years.
recode education ///
	(min/0.99 = 0 "0") ///
	(1/1.99 = 1 "1") ///
	(2/2.99 = 2 "2") ///
	(3/3.99 = 3 "3") ///
	(4/4.99 = 4 "4") ///
	(5/5.99 = 5 "5") ///
	(6/max = 6 "6+") ///
	(else = .), gen(education7)
la var education7 "Years of completed secondary education"

*Finally, we group the agriculture variable into quartile range groups:
recode agriculture ///
	(min/24.9999 = 1 "low minority") ///
	(25/50.99999 = 2  "minority") ///
	(50/74.9999 = 3 "majority") ///
	(75/max = 4 "high majority") ///
	(else = .), gen(agriculture4)
la var agriculture4 "Share of employment in agriculture (4 groups)"


* III) Missing values assessment
* ------------------------------

* Because we are using a country-level data, the next command is used with the
* 'freq' option to show missing data in frequencies instead of percentages.
misstable pat workforce GDPcapita muslim education catholic ///
agriculture, freq

* Those are the geographic regions in the entire dataset
tab ht_region

* Let's now compare that distribution with our 'no missing data' country sample.
tab ht_region if !mi(workforce, fertility, GDPcapita, muslim, education, catholic, agriculture)

* The results show that our Religion and Agriculture IVs are going to force us 
* to ignore a fraction of the entire dataset, because it is missing for 18
* countries. This will limit the representativeness of our study. Let's find
* out how exactly.

* Subsetting
*-------------------------------

* Now, we finalize our dataset by deleting observations with missing data in our 
* of variables. The final count represents the actual sample siza that we will
* analysze at later stages of the research. 
drop if mi(workforce, fertility, GDPcapita, muslim, education, catholic, agriculture)

* Final count:
count


* IV) VARIABLE TRANSFORMATION
* ------------------------------

* Normality assessment of the dependent variable

* Visual assessment
* ---------------------------


hist workforce, bin (15) normal kdensity kdenopts(lp(dash) lc(black)) ///
note("Normal distribution (solid red) and kernel density (dashed black).") ///
name(workforce_hist, replace)
	
symplot workforce, ti("Symmetry plot") ///
	name(workforce_sym, replace)
	
* Another visualization plots the quantiles of the variable against those of the
* normal distribution. Perfect correspondence between the two distributions is
* observed at the straight red line.
qnorm workforce, ti("Normal quantile plot") ///
	name(workforce_qnorm, replace)


* Formal assessment
* -----------------

* Moving to statistical measures of normality, we can measure skewness, which
* measures symmetry and approaches 0 in quasi-normal distributions, along with
* kurtosis, which measures the size of the distribution tails and approaches 3
* in quasi-normal distributions. Use the -summarize- command with the -detail-
* option, respectively abbreviated as -su- and -d-.
su workforce, d

* There are more advanced tests to measure normality, but the tests above are
* sufficient to observe that we cannot assume the workforce variable to be normally
* distributed (i.e. we reject our distributional assumption).
	
	
* ====================
* = DATA PREPARATION =
* ====================

* Variable transformations
* ------------------------

* We previously observed that our DV was very skewed. Let's try to fix this.
gladder workforce

* We consider a square or log transformation as a potential improvement to the 
* variable so we will generate the two variables to analyze further. 
gen workforce_sq = workforce^2
gen workforce_log = log(workforce)

* Let's now look at the result: the squared transformation did bring the distribution of
* our DV much closer to a normal distribution (shown in red). The log transformation 
* does not improve the normality. 

hist workforce_sq, norm ///
  name(dv_sq, replace)
  
hist workforce_log, norm ///
  name(dv_log, replace)

* Inspect improvement in normality (by comparing values for kurtosis and skeweness)
* ---------------------------------------------------------------------------------

tabstat workforce workforce_sq workforce_log, s(n skewness kurtosis min max) c(s)

* What has already become apparent from the graph is confirmed more concretely in the
* above table, which compares the properties of the workforce, workforce_sq, and workforce_log
* distribution. The log transformation actually brings the dependant variable further 
* away from normality with skewness and kurtosis values that are less optimal. 
* Meanwhile, the square transformation brings our dependent variable closer to an ideal
* normality distribution. Our transformed DV is now less left-skewed; additionally, the 
* value for the kurtosis is now very close to 3 (3 indicating the value for a perfect normal
* distribution. However, due to the difficulty in interpreting a square transformed
* in further analysis, we will thus drop the transformed workforce_sq variable 
* as our DV and stick to our original variable. . 


* Exploring the hypothesis - display of DV over IV's
*----------------------------------------------------

* To illustrate how our different IVs and the DV are related, we create a number of individual
* boxplots. Each of the following boxplots shows how a given independent variable is correlated with
* the share of women in the workforce relative to men. 
* We also create spineplots to illustrate the relationship between all our IVs and the DV. The 
* scaterplot is also helpful because it depicts the the distribution of the DV
* along the categorical spread of the IVs.

* Box plots and spine plots.

gr hbox workforce, over(fertility6) l1title("Fertility (children per woman)") ///
	name(workforce_fertility, replace) 
spineplot workforce4 fertility6, scheme(burd6) ///
     name(fertility6_spine, replace)
	
gr hbox workforce, over(GDPcapita4) l1title("GDP per capita") ///
	name(workforce_GDPcapita, replace)
spineplot workforce4 GDPcapita4, scheme(burd6) ///
     name(GDPcapita4_spine, replace)
	
gr hbox workforce, over(education7) l1title("Education (finished school years)") ///
	name(workforce_education, replace)
spineplot workforce4 education7, scheme(burd6) ///
    name(education7_spine, replace)

* Looking at the boxplots of the above independent variables, we see an
* interesting result: Plottet against the DV, these IVs all seem to behave in a 
* similar, non-linear way. 
* Specifically, we find that the share of women in the labor force is particularly
* high in very poor AND very rich countries. By contrast, middle-income countries are associated with
* a less important share of women in the labor force. 
* Similarly, women form an important part of the workforce in countries with particularly high AND
* low fertility rates, but less so in countries with fertility rates that are "in the middle" of the
* global fertility rate continuum. 
* Finally, women play an important role in the labor force in countries where women receive a high level
* of secondary schooling AND also in countries with particularly low levels of secondary schooling.
* Just as for the GDP-per-capita and the fertility rate variable, the cases where the least women are 
* engaged in the labor force are the countries that are "in the middle" of the global continuum.
 
gr hbox workforce, over(muslim4) l1title("Muslims in total population") ///
	name(workforce_islam, replace)
spineplot workforce4 muslim4, scheme(burd6) ///
     name(muslim4_spine, replace)

gr hbox workforce, over(catholic4) l1title("Catholics in total population") ///
	name(workforce_catholicism, replace)
spineplot workforce4 catholic4, scheme(burd6) ///
     name(catholic4_spine, replace)

* Our religious belief variables also yield interesting results. The boxplot illustrating the association
* between the share of muslims in the population and the share of women in the workforce suggests that there
* is a negative relationship between the share of the muslim population in a given country and the share of 
* women in that countries workforce. Just like we hypothesised, a larger share of muslims in the population
* is associated with fewer women in the labor market.
* A similar tendency can be observed for the catholic believe variable. However, we the results are less clear:
* A low proportion of catholic believers is not necessarily associated with fewer women in the labor market.
* Instead, countries with a low share of catholic population are associated with female labor force
* representation that varies along the entire range of the distribution.

gr hbox workforce, over(agriculture4) l1title("Share of Employees in Agriculture") ///
	name(workforce_agriculture, replace)
spineplot workforce4 agriculture4, scheme(burd6) ///
     name(agriculture4_spine, replace)
	
* Finally, we observe a clear, positive relationship between the relative importance of the agricultural
* sector in a country's labor market and the proportion of women represented in the labor force. 
* The above boxplot provides support to our initial assumption that countries with an important
* agricultural sector will be associated with many women working. 

* Analysis of represented world regions
* -------------------------------------

* Those are the geographic regions in the entire dataset
tab ht_region

* Recode regions to less, shorter labels.
recode ht_region (6/10 = 6), gen(region)
la var region "Geographical region"
la val region region
la def region 1 "E. Europe and PSU" 2 "Lat. America" ///
    3 "N. Africa and M. East" 4 "Sub-Sah. Africa" ///
    5 "W. Europe and N. America" 6 "Asia, Pacific and Carribean" ///
    , replace

* Let's now compare that distribution with our 'no missing data' country sample.
tab region if !mi(workforce, GDPcapita4, muslim4, education7,  ///
catholic4, agriculture4)

* From the above list, it is apparent that countries from the world region coded as category
* number 9 are missing. If we show a list of all the world regions and the countries assigned to these
* regions, we learn that our set of valid country cases does not contain countries from group 9
* (the region called "the Pacific").

list ht_region country

* We thus note that countries from the Pacific are generally excluded from our analysis. Note, however,
* that the codebook defines Australia and New Zealand not as Pacific countries, but as those belonging
* to the group of countries from world region group 5 ("Western Europe and North America").
* We also note that our set of valid coutnry observations contains only 16 Asian countries, and only two
* countries from the Carribean.

* Export summary statistics
* -------------------------

* Export to a plain text file, from which we can easily create a table in a
* rich text editor later on (remember the 'Word demo' shown in class).
stab using bosch_dubler_lin_firstdraft_stats.txt, replace ///
	mean(workforce fertility GDPcapita muslim education catholic agriculture) ///
	prop(region) 


* END OF DO-FILE FIRST DRAFT.
*-----------------------------

* START OF DO-FILE FINAL.

* ============================================
* = CORRELATION AND SIMPLE LINEAR REGRESSION =
* ============================================

* We are now in a position to examine the relationships between our different 
* IVs and our DV further, first, by examining the simple correlations between a 
* given IV and our DV, and then by running simple linear regressions for each IV 
* on the DV.

* In our earlier visual analysis of variables far, we put the values 
* for independent variables into groups, so the IVs effectively became ordinal, 
* not continuous(lines 335-411 of this do-file). We did this temporarily for 
* illustration purposes in the descriptive statistics part, because a graphical 
* illustration with grouped values was convenient. However, for the following 
* inferential statistics part, we will go back to working with the original, 
* continuous IVs.

* Before we begin, we put a few settings in place that allow stata to illustrate 
* and process those commands better that we will be running frequently over the 
* rest of the do-file.
global ccode "ms(i) mlabpos(0) mlab(ccodealp) legend(off)"
global ci "legend(off) lp(dash)"

* IV (1): workforce and GDP-per-capita 
* ------------------------------------

* We start our correlation analysis by a visual inspection of the GDP-per-capita IV (now continuous),
* plotting it against the DV, and also introducing a linear fit line in the graph.
tw (sc workforce GDPcapita, $ccode) (lfit workforce GDPcapita, $ci), ///
	yti("Share of women in the workforce") ///
    name(workf_GDPcapita1, replace)

* Add 95% confidence intervals into the graphical illustration. The confidence 
* interval reflects the standard error of the mean (SEM), itself a reflection of 
* sample size. 

tw (sc workforce GDPcapita, $ccode) (lfitci workforce GDPcapita, $ci), ///
	yti("Share of women in the workforce") ///
    name(workf_GDPcapita2, replace)
	
* Now, we estimate the predicted effect of GDP-per-capita on the women workforce.
* Function: workforce = _cons (alpha) + Coef (beta) * GDPcapita.
* Equation: predicted Y (DV) = alpha + beta X (IV) + epsilon (error term).
reg workforce GDPcapita
  
* From our regression table, we see that the positive relationship between GDP-per-capita
* and women in the workforce which we examined in our descriptive statistics part above
* is not statistically significant (the p-value being 0.249).

* Plotting regression results
* ---------------------------

* We now examine in more detail the regression's results, with a focus on the residuals'
* distribution

* Simple residuals-versus-fitted plot.
rvfplot, yline(0) ///
    name(rvfplot1, replace)

* Get fitted values.
cap drop yhat
predict yhat

* Get residuals.
cap drop r
predict r, resid

* Plot DV with observed and predicted values of IV.
sc workforce GDPcapita || conn yhat GDPcapita, ///
    name(dv_yhat1, replace)
	
* In order for our data to fit the assumptions of the linear regression model ideally, 
* the distribution of the residuals should be even along the fitted values of the regression
* line's slope (the horizontal straight line in the "rvfplot" graph generated above).
* To evaluate the appropriateness of our linear regression model on the variables observed, three
* issues should be considered: First, the residuals should be approximately normally distributed; 
* second, the plot should reveal homoskedasticity; and third, no remaining trend should 
* emerge from the residuals-versus-fitted plot.
* We observe that this is only partially the case: There seem to be approximately as many values
* above the slope as below it, although especially for values on the left-hand side of the graph (i.e. 
* among the countries with lower GDP-per-capita), the residuals for some country cases are 
* very large. Furthermore, there is a large variance in the residuals: They are very broadly
* distributed among the low-income country cases, but  much more densly
* distributed among middle- and high-income countries. Thus, the model residuals reveal
* heteroskedasticity.
* Finally, the linear regression has also not completely eliminated any
* trend in the residuals in the "rvfplot". The -qfit- command allows more flexibility in that it
* generates a non-linear fitting line, which shows that a more advanced model might better 
* explain the DV-IV relationship:

tw (sc workforce GDPcapita, $ccode) (qfit workforce GDPcapita, $ci), ///
	yti("Share of women in the workforce") ///
    name(workf_GDPcapita1_qfit, replace)
	
* This hints to the fact that the linear regression overall has limitations in this case.
* However, a transformation of the IV (for instance, into its logarithmic form) does not improve
* this situation. We thus retain our IV, though this clearly show the limits of the linear 
* regression model.

* IV (2): workforce and fertility rate 
* ------------------------------------

* Next, we examine in more detail the correlation between the women workforce and a country's
* fertility rate: The continuous IV is plotted against the DV, introducing a linear fit line in the graph.
tw (sc workforce fertility, $ccode) (lfit workforce fertility, $ci), ///
	yti("Share of women in the workforce") ///
    name(workf_fertility1, replace)

* Add 95% CI.
tw (sc workforce fertility, $ccode) (lfitci workforce fertility, $ci), ///
	yti("Share of women in the workforce") ///
    name(workf_fertility2, replace)

* Estimate the predicted effect of the fertility rate on the share of women in the workforce.
reg workforce fertility
  
* The above scatterplot illustrates that there appears to be virtually no clear
* trend between the fertility rate and women in the workforce. Of anything, the modeled
* relationship is slightly positive (the regression table illustrates a coefficient of
* 0.0047432), but this  relationship is not statistically significant (p-value 0.441). 

* Plotting regression results
* ---------------------------

* We examine again to what extend the linear regression model is appropriate for our data:
* Simple residuals-versus-fitted plot.
rvfplot, yline(0) ///
    name(rvfplot2, replace)

* Get fitted values.
cap drop yhat
predict yhat

* Get residuals.
cap drop r
predict r, resid

* Plot DV with observed and predicted values of IV.
sc workforce fertility || conn yhat fertility, ///
    name(dv_yhat2, replace)

* Again, the linear regression model does not seem to capture the data very well. There is
* a large variation along the linear best-fit-line, which does not seem to be the ideal model.
* The residuals-versus-fitted plot shows that there is considerable variance in the residuals
* distribution, with strong outliers especially among the country cases with a low share of 
* women in the workforce. The variance along the fitted values vary, so there seems to be
* heteroskedasticity. 
* Finally, we check again whether a non-linear function can better approximate the data:
	
tw (sc workforce fertility, $ccode) (qfit workforce fertility, $ci), ///
	yti("Share of women in the workforce") ///
    name(workf_fertility_qfit, replace)
	
* The -qfit- command shows that a more advanced model might better explain the DV-IV relationship,
* as it looks less linear than quadratic: Y = a + bX could be replaced with Y = a + bX^2 to observe 
* a more correct fit. We thus generate a new, squared fertility variable and observe improvement:

* Variable transformation.
gen fertility_square = fertility^2
la var fertility_square "Fertility rate(square)"

* Visual inspection.
tw (sc workforce fertility_square, $ccode) (lfit workforce fertility_square, $ci), ///
    name(workf_fert_square_lfit, replace)

reg workforce fertility_square 	

rvfplot, yline(0) ///
    name(rvfplot3, replace)
	
* The squared fertility variable makes it harder to interpret the effect of fertility on the workforce.
* Also, a transformation does not improve our model, because the residuals still behave erratically when
* plotted against there fitted values.
* We will thus retain the simple "fertility" variable  for our multiple linear regression analysis.

* IV (3): workforce and female education 
* --------------------------------------

* We now examine the correlation between the women workforce and women's average years of
* schooling:
tw (sc workforce education, $ccode) (lfit workforce education, $ci), ///
	yti("Share of women in the workforce") ///
    name(workf_education1, replace)

* Add 95% CI.
tw (sc workforce education, $ccode) (lfitci workforce education, $ci), ///
	yti("Share of women in the workforce") ///
    name(workf_education2, replace)

* Estimate the predicted effect of the female education on the share of women in the workforce.
reg workforce education
  
* The regression model confirms the slight modeled relationship between women's secondary
* schooling years and the share of women in the workforce (coefficient in the regression model
* of  .0071628), but again, this relationship is statistically insignificant (p-value 0.106).

* Plotting regression results
* ---------------------------

* Simple residuals-versus-fitted plot.
rvfplot, yline(0) ///
    name(rvfplot4, replace)

* Get fitted values.
cap drop yhat
predict yhat

* Get residuals.
cap drop r
predict r, resid

* Plot DV with observed and predicted values of IV.
sc workforce education || conn yhat education, ///
    name(dv_yhat4, replace)

* We observe a residuals-versus-fitted plot that looks very similar to the previous ones with
* the other IVs: The residuals are not exactly normally distributed, and there seems to be 
* more variance in the left-hand side of the plot than on the right hand side, 
* indicating some heteroskedasticity. 
* The following command also shows that a relaxation of the strict linearity assumption may
* allow to better represent the distribution of the data:
tw (sc workforce education, $ccode) (qfit workforce education, $ci), ///
	yti("Share of women in the workforce") ///
    name(workf_education_qfit, replace)

* A square transformation appears to bring our variable much closer to approximate a linear
* resgression model. We thus undertake a  square variable transformation and observe improvement.
gen education_square = education^2
la var education_square "Average secondary schooling years (square)"

* Visual inspection.
tw (sc workforce education_square, $ccode) (lfit workforce education_square, $ci), ///
    name(fert_edu_square_lfit, replace)

reg workforce education_square 

rvfplot, yline(0) ///
    name(rvfplot5, replace)

* As we see from executing the above command, the squared transformation of the IV 
* improves our model in important ways: Not only does the R-squared value increase, 
* but the correlation between "education_square" and the workforce is now also 
* statistically significant (p-value 0.015), which was not the case in the 
* regression with the simple "education" variable (p-value 0.106).

* More importantly, as the residuals-versus-fit plow ("rvfplot5") demonstrates, the residuals of 
* the transformed variable are more normally distributed than those of the untransformed variable,
* and they also reveal less heteroskedasticity.

tw (sc workforce education_square, $ccode) (qfit workforce education_square, $ci), ///
	yti("Share of women in the workforce") ///
    name(workf_education_square_qfit, replace)	
	
* Finally, the above command reveals that the transformed variable, when plotted against
* the DV, is now much closer to a linear regression model than previously
* (compare the "workf_education_qfit" and the "workf_education_square_qfit" plots).
* Overall, the transformation thus leads to significant improvement. We thus retain 
* the "education_square" variable for further examination during our multiple linear
* regression, which we undertake below.


 *IV (4): workforce and share of muslim population
 *------------------------------------------------
 
* Examine the correlation between the women workforce and the respective countrie's share of muslim population:
tw (sc workforce muslim, $ccode) (lfit workforce muslim, $ci), ///
	yti("Share of women in the workforce") ///
    name(workf_muslim1, replace)

* Add 95% CI.
tw (sc workforce muslim, $ccode) (lfitci workforce muslim, $ci), ///
	yti("Share of women in the workforce") ///
    name(workf_muslim2, replace)

* Estimate the predicted effect of the share of a country's muslim population on the share of women
* in the workforce.
reg workforce muslim

* The scatterplot illustrates a negative relationship between the share of muslims in the
* population and the share of women in the workforce, as we hypothesised at the beginning
* of our paper. The linear regression of the "muslim" variable on the "workforce" variable
* shows a slight, but marked negative trend with a coefficient of -0.0016518. This linear model
* can thus be interpreted as follows: An increase of the muslim population by, say, 20%, would be 
* predicted to be associated with a decrease of the share of women in the workforce of
* approximately 3% (20 * 0.0016518 = 0.033036). The negative relationship may be slight, but it 
* is highly significant (p-value 0.000). The r-squared value is 0.5139, so 51% of the variation in
* the share of women in the workforce is explained by the variance in the share of muslim population.

 
* Plotting regression results
* ---------------------------

* Simple residuals-versus-fitted plot.
rvfplot, yline(0) ///
    name(rvfplot6, replace)

* Get fitted values.
cap drop yhat
predict yhat

* Get residuals.
cap drop r
predict r, resid

* Plot DV with observed and predicted values of IV.
sc workforce muslim || conn yhat muslim, ///
    name(dv_yhat6, replace)

 * The above residuals-versus-fitted plot reveals that the linear regression model seems
 * to fit our data rather well in this case: The residuals appear to be approximately 
 * normally distributed, with the vast majority of country cases grouped closely along 
 * the fitted linear line, and some other country cases rather far away from the linear
 * fitted line, either above or below the linear fit. There appears to be little variance
 * in the residuals-versus-fitted plot, although the variance seems to be a little 
 * larger among the left-hand side of the graph (i.e. among the countries with a high
 * share of muslim population. Finally, a clear remaining trend is not immediately 
 * recognizable from the residuals-versus-fitted plot. Still, in the following command,
 * we allow the fitted line to relax its linearity assumption and thake on a non-linear
 * fitted value.
 
tw (sc workforce muslim, $ccode) (qfit workforce muslim, $ci), ///
	yti("Share of women in the workforce") ///
    name(workf_muslim_qfit, replace)
 
 * Based on our ovbservations, we check whether a logarithmic transformation of the IV 
 * may better fit our linear model. In what follows, we thus generate the "muslim" 
 * variable in its logged form, plot it against the DV and run a regression for it.
 
* Variable transformation.
gen muslim_log = log(muslim)
la var muslim_log "Share of muslim population (log)"

* Visual inspection.
tw (sc workforce muslim_log, $ccode) (lfit workforce muslim_log, $ci), ///
    name(workforce_muslim_log_lfit, replace)

reg workforce muslim_log

* Simple residuals-versus-fitted plot.
rvfplot, yline(0) ///
    name(rvfplot7, replace)
* Get fitted values.
cap drop yhat
predict yhat
* Get residuals.
cap drop r
predict r, resid
* Plot DV with observed and predicted values of IV.
sc workforce muslim_log || conn yhat muslim_log, ///
    name(dv_yhat7, replace)

* Check whether the transformed variable approximates a linear model, when allowed
* for some leeway with the -qfit- command.
tw (sc workforce muslim_log, $ccode) (qfit workforce muslim_log, $ci), ///
	yti("Share of women in the workforce (log)") ///
    name(workf_muslim_log_qfit, replace)
	
* We observe that overall, a logged transformation of the variable does not improve
* our model. In fact, the "muslim_log" variable behaves even more erratic when plotted
* against our DV.  We thus stick to the original "muslim" and drop the logarithmic
* transformation.
 

 *IV (5): workforce and share of catholic population
 *------------------------------------------------
 
 * We repeat the correlation analysis for the other religion IV, namely the share of
 * the population in a given country that is catholic.
 tw (sc workforce catholic, $ccode) (lfit workforce catholic, $ci), ///
	yti("Share of women in the workforce") ///
    name(workf_catholic1, replace)

* Add 95% CI.
tw (sc workforce catholic, $ccode) (lfitci workforce catholic, $ci), ///
	yti("Share of women in the workforce") ///
    name(workf_catholic2, replace)
	
reg workforce catholic

* Running the regression, we first observe what already became apparent from the
* scatterplot: in a linear model, on average, there is almost no meaningful trend 
* in the relationship betweeen the share of the population that is catholic 
* and the share of women in the workforce. If anything, the relationship is slightly
* positive(coefficient: 0.0002486). We note that this is contrary to our hypothesis, 
* which assumed that the share of women working would decrease as the share of catholics
* increases. However, the relationship is not statistically significant, and so at least
* in a simple linear regression, we cannot assume catholic belief to have a non-random
* influence on the share of women in the workforce.  


* Plotting regression results
* ---------------------------

* Simple residuals-versus-fitted plot.
rvfplot, yline(0) ///
    name(rvfplot8, replace)

* Get fitted values.
cap drop yhat
predict yhat

* Get residuals.
cap drop r
predict r, resid

* Plot DV with observed and predicted values of IV.
sc workforce catholic || conn yhat catholic, ///
    name(dv_yhat8, replace) 
	
* We observe from the residuals-versus-fitted plot that the residuals are more or less
* equally distributedequally along the spectrum of residuals, and that there is no strong
* concentration of values along the fitted line. Thus, the distribution does not seem to 
* be normal. There is much larger variance on the left-hand side of the graph, thus, we 
* observe heteroskedasticity. Finally, a non-linear trend seems to remain among the residuals
* even after inclusion of the fitted line. We check whether a non-linear model may better 
* represent the data:	
tw (sc workforce catholic, $ccode) (qfit workforce catholic, $ci), ///
	yti("Share of women in the workforce") ///
    name(workf_catholic_qfit, replace)
	
* We observe that the -qfit- command reveals that the relationship might be better
* represented through a transformation of the IV (the "muslim" variable) into a different
* form. We undertake a transformation of the variable into its logarithmic form and observe
* improvement: 

* Variable transformation.
gen catholic_log = log(catholic)
la var catholic_log "Share of catholic population (log)"

* Visual inspection.
tw (sc workforce catholic_log, $ccode) (lfit workforce catholic_log, $ci), ///
    name(workforce_catholic_log_lfit, replace)

reg workforce catholic_log

* Note how the resulting regression table illustrates the usefulness of our regression:
* Transforming the IV into its logarithmic form makes the relationship more difficult
* to interpret, but the explanatory value of the variable is improved, with an R-squared
* of 0.1286 (as opposed to 0.014 previously). The positive relationship is now statistically
* significant, with a p-value of 0.001, which was not the case for the untransformed
* variable (p-value 0.263).

* Simple residuals-versus-fitted plot.
rvfplot, yline(0) ///
    name(rvfplot9, replace)
* Get fitted values.
cap drop yhat
predict yhat
* Get residuals.
cap drop r
predict r, resid
* Plot DV with observed and predicted values of IV.
sc workforce catholic_log || conn yhat catholic_log, ///
    name(dv_yhat9, replace) 

* The above residuals-versus-fitted plot reveal a significant improvement
* if the residuals' behavior after a logarithmic transformation of the IV: The residuals
* are now more normally distributed, with a stronger concentration of values close to
* the fitted line. The graph also reveals less heteroskedasticity.
* We thus retain both the untransformed "catholic" variable and its transformed form
* "catholic_log" for the multilinear regression model.


 *IV (6): workforce and the importance of agriculture
 *---------------------------------------------------

 * We finally analyze the correlation betweent the share of women in the workforce and the
 * proportion of the workforce employed in the agricultural sector:
 tw (sc workforce agriculture, $ccode) (lfit workforce agriculture, $ci), ///
	yti("Share of women in the workforce") ///
    name(workf_agriculture1, replace)

* Add 95% CI.
tw (sc workforce agriculture, $ccode) (lfitci workforce agriculture, $ci), ///
	yti("Share of women in the workforce") ///
    name(workf_agriculture2, replace)
	
reg workforce agriculture

* The scatterplot of the proportion of the total workforce employed in agriculture and the share
* of women in the workforce reveals a positive correlation. Relying on a linear model, there is a slight
* positive relationship (coefficient: 0.0005667), but this coefficient is not significant 
* (p-value 0.178). Thus, judging from this linear regression, we cannot assume the relevance of
* the agriculture in a given country to have a statistically significant impact on the share of 
* women active in the workforce.

* Plotting regression results
* ---------------------------

* Simple residuals-versus-fitted plot.
rvfplot, yline(0) ///
    name(rvfplot10, replace)

* Get fitted values.
cap drop yhat
predict yhat

* Get residuals.
cap drop r
predict r, resid

* Plot DV with observed and predicted values of IV.
sc workforce agriculture || conn yhat agriculture, ///
    name(dv_yhat10, replace) 

* Unfortunately, the "agriculture" IV behaves quite erratic when plotted
* against the fitted values of a linear regression. The residuals are not
* normally distributed. Instead, the residuals are quite equally dispersed,
* and no clear concentration surrounding the fitteld values line.
* The residuals-versus-fitted plot also reveals heteroskedasticity.

* We thus check to what extent a transformation of the variable conforms
* better to the assumptoons of the linear model.

tw (sc workforce agriculture, $ccode) (qfit workforce agriculture, $ci), ///
	yti("Share of women in the workforce") ///
    name(workf_agriculture_qfit, replace)
	
* A square transformation may bring our variable closer to approximate a linear
* resgression model. We thus undertake a  square variable transformation and observe improvement.
gen agriculture_square = agriculture^2
la var agriculture_square " Share of employment in agriculture(square)"

* Visual inspection.
tw (sc workforce agriculture_square, $ccode) (lfit workforce agriculture_square, $ci), ///
    name(workf_agri_square_lfit, replace)

reg workforce agriculture_square 

* Create a new residuals-versus-fitted values plot to check whether there is
* improvement:
rvfplot, yline(0) ///
    name(rvfplot11, replace)
	
* The residuals-versus fitted plot demonstrates that there is barely any
* improvement observable. The residuals still behave somewhat erratic.

* Finally, we compare the "agriculture" variable with the "agriculture_square"
* transformation when plotted against the DV, checking to what extend they
* approximate a linear model.	
tw (sc workforce agriculture_square, $ccode) (qfit workforce agriculture_square, $ci), ///
	yti("Share of women in the workforce") ///
    name(workf_agriculture_square_qfit, replace)
	
* Comparing the "workf_agriculture_qfit" and "workf_agriculture_square_qfit"
* graphs, we see that the latter is closer to a linear function. 
* We thus consider that the squared transformation is useful for the purpose
* of our linear regression analysis, and we stick to it for the purpose of
* the multiple linear regression later.
	

* ==============================
* = MULTIPLE LINEAR REGRESSION =
* ==============================

* We now begin our multiple linear regression analysis, meaning that we examine
* all our IVs and how they behave on the DV when integrated in one single regression
* model.
 
* Scatterplot matrixes
* --------------------

* We begin with a visual inspection of the data organized as a scatterplot matrix.
* A scatterplot matrix contains all possible bivariate relationships between
* any number of variables. Building a matrix of your DV and IVs allows to spot
* relationships between IVs, which will be useful later on in your analysis.

gr mat workforce GDPcapita muslim fertility education_square catholic_log agriculture, half ///
	name(gr_matrix, replace)
	
* The relationships resulting from this graphic illustration are difficult to read 
* and not very clear. This is partly due to the fact that the relationship
* between the IVs and our DV are not always unambiguously linear. As we explored in previous
* sections of this do-file, the variables behave somewhat erraticly and do not perfectly
* conform to the assumption of linearity.
	
* The most practical way to consider all possible correlations in a list of
* predictors (or independent variables) is to build a correlation matrix out
* of their respective pairwise correlations. "Pair-wise" indicates that the
* correlation coefficient uses only pairs of valid, nonmissing observations,
* and disregards all observations where any of the variables is missing.
pwcorr workforce GDPcapita fertility education_square catholic_log muslim agriculture, star(.05)

* Run basic linear model.
reg workforce GDPcapita fertility education_square catholic_log muslim agriculture

* This basic linear regression model shows the preliminary results: 
* First, we find that a country's average income (measured in GDP per capita) is associated with a higher
* share of women in the workforce. This positive correlation is statistically significant (p-value 0.001).
* This finding is in accordance with our hypothesis according to which higher GDP is correlated
* with a higher share of women in the workforce.

* Second, we find that a country's fertlity rate is positively correlated with the share of women in
* the workforce. The correlation coefficient is positive (0.023) and statistically significant 
* (p-value 0.002). The correlation can be interpreted as follows: Increasing a country's fertlity rate
* by one child per woman leads to an increase of the share of women in the workforce by 2,3%.
* Interestingly, this trend is in contradiction to our initial hypothesis, as we had assumed 
* fertility rate to be negatively correlated with the share of women in the workforce.

* Third we find a negative and statistically significant relationship between the share of muslims
* in the population and the share of women in the workforce (coefficient -0.0019; p-value 0.000).
* Thus, a 10% increase in the share of muslims across the population would be estimated to be 
* associated with a 0.01 % decrease in the share of women in the population. This relationship is 
* extremely weak, but in conformity with our initial hypothesis.
* Similarly, the share of catholics among the population is weakly negatively correlated with the 
* share of women in the workforce. However, in contrast to the "muslim" variable, this relationship
* is not statistically significant.

* Fourth, we observe that the "education_square" variable is positively correlated with the share of 
* women in the workforce. However, the model does not allow us to find our hypothesis confirmed because
* the relationship is statistically insignificant (p-value 0.209).

* Finally, we observe that the relative importance of the agricultural sector (as a share of total
* employment in a given country) is positively associated with women in the workforce. This is
* contrary to our initial hypothesis, but we cannot draw any strong conclusions as the relationship
* remains statistically insignificant.

* Overall, we thus find only moderate preliminary support for our initial hypotheses, as only 
* GDP per capita and the share of the muslim population are correlated in a statistically
* significant way with the share of women in the population, in line with our assumptions.
* However, these results are only preliminary, and we will have to wait until the inclusion of
* interaction terms to draw final conclusions. This is part of the regression diagnostics, which
* we perform below.

* Storing fitted (predicted) values.
cap drop yhat
predict yhat

* REGRESSION DIAGNOSTICS
* ----------------------

* A) RESIDUALS

* Store the unstandardized (metric) residuals.
cap drop r
predict r, resid

* We now analyze to what extent the regression "fits" the assumptions of our linear
* regression model. The steps carried out here are similar to those that we carried
* out in the simple linear regression analyses which we carried out for all our IVs above.
* The difference, however, is that we now diagnose the multiple regression model, where
* all IVs are included in one regression.
* Just like for the simple linear regressions earlier, the residuals analysis follows
* in three steps: First, we assess the distribution of the residuals (whether they are normally
* distributed); second, we assess whether they reveal homoskedasticity; third, we check
* that no trend remains among the plotted residuals.


* Store the standardized residuals.
cap drop rst
predict rst, rsta

* Distribution of the standardized residuals.
hist rst, normal ///
	name(rst, replace)
	
* Store the predicted values.
cap drop yhat
predict yhat

* Assess the normality of residuals.
kdensity r, norm legend(off) ti("") ///
    name(diag_kdens, replace)
	
* The graph reveals that our residuals behave fairly normally. The kernel density graph
* (blue line) is pretty close to a perfect normal distribution (red line), with a little
* skewness to the right-hand side.

* Plot the distribution of the standardized residuals over geographic regions.
hist rst, normal by(region, legend(off)) bin(10) xline(0) ///
	name(rsta_by_region1, replace)
	
* Some world regions are very close to a idealized normal distribution (e.g. Latin America 
* and Western Europe and North America. On the other hand, countries in North Africa and the
* Middle East (and to a lesser degree countries in Asia, Pacific and the Carribean)
* constitute potential outliers, which are responsible for deviations of the real country
* cases distribution from an ideal-type normal distribution.

* With the next few lines, we identify in more detail some outliers:
* Plot the residuals-versus-fitted values, showing two geographic regions where
* the standardized residuals seem to be more correlated than for other ones.
tw sc rst yhat if !inlist(region, 3, 6) || ///
	sc rst yhat if region == 3 || ///
	sc rst yhat if region == 6, ///
	legend(order(1 "Sample" 2 "N. Africa and M. East" 3 "Asia, Pacific and Carribean") row(1)) ///
	name(rst_by_region2, replace)
	
* We see that many of the outliers seem to be countries in either North Africa and the Middle
* East or such countries in the world region Asia, Pacific and the Carribean.

* Identify outliers beyond 2 standard deviation units.
sc rst yhat, yline(-2 2) || sc rst yhat if abs(rst) > 2, ///
    ylab(-3(1)3) mlab(ccodealp) legend(lab(2 "Outliers")) ///
    name(diag_rst, replace)
* Among our dataset with 91 countries, we find five particular outliers: India, Sri Lanka,
* Senegal, the Gambia and Bangladesh.

* Homoskedasticity of the residuals versus fitted values (DV).
rvfplot, yline(0) ms(i) mlab(ccodealp) name(diag_rvf, replace)

* The graph reveals a moderate heteroskedasticity as the values seem to become
* somewhat more concentrated on the right-hand side of the graph. On the 
* left-hand side of the graph, the values are somewhat more dispersed, and
* some outliers exist. However, these results over all do not unreasonably violate
* the assumptions of the linear regression model.


* B) MULTI-COLINEARITY
* Variance inflation factor analysis.
* Critical cut-off points for variance inflation are VIF > 10 or 1/VIF < .1 (tolerance). Each
* VIF is computed as the reciprocal of the inverse R-squared, 1/(1-R^2), for
* each predictor in the model (that is, the R-squared of that variable minus
* the R-squared of the entire model without it).
vif

* We observe that the variance inflaction factor (vif) remains clearly below the value of 10
* for all of our IVs, and the mean cif is 2.52. This allows us to continue our diagnostics
* without severly violating the requirements of the linear regression model.

* C) INTERACTION TERMS
* Finally, we refine our model by including a number of interaction terms which will refine the findings of our project.
* Creating interaction terms for those variables that are classical development indicators
* (GDPcapita, fertility, education).

* Regression 
reg workforce muslim catholic_log agriculture c.GDPcapita##(c.fertility /// 
c.education_square) c.fertility#c.education_square

* When thinking about our project in theoretical terms, it also crossed our minds that
* the fertility rate and the share of the workforce employed in the agricultural sector may be highly
* correlated, and thus interact. We were thinking especially of Sub-Saharan African countries, where both
* fertility rate and the share of agricultural employment would be high.

* A simple scatterplot confirms our assumption:
sc agriculture fertility

* We thus run the regression with an interaction effect between those two variables.
reg workforce muslim catholic_log agriculture c.GDPcapita##(c.fertility /// 
c.education_square) c.fertility#(c.education_square c.agriculture)

*Finally, we also integrate an interaction term for the share of Muslim population and the "agriculture" IV.
reg workforce muslim catholic_log agriculture c.GDPcapita##(c.fertility /// 
c.education_square) c.fertility#(c.education_square c.agriculture) /// 
c.muslim#c.agriculture

* Having included all these interaction terms, we find that the results of our analysis have changed
* significantly. We find that our initial relationships are retained, but only the "religious variables" appear
* statistically significant: 

* A higher share of muslim population is associated with a lower share of women represented in the
* workforce, but this relationship is extremely weak (correlation coefficient -0.0024). 
* This means that a 10% increase in the share of muslim population is associated with a 0.02%
* decrease of women in the workforce. The relationship is extremely weak, but statistically significant
* (p-value 0.000). 

* Similarly, the share of the catholic population is associated with a lower share of women
* represented in the workforce. Since the catholic variable is in its logarithmic form, the
* interpretation is of the form 
* --> Y = alpha + beta ln X.
* --> Share of women in the workforce = alpha + (-0.0051) ln catholic.
* This means that a 1% increase in the catholic population is associated with
* a 0.01*beta unit increase in Y. Thus, here, a 1% increase in the catholic population is associated
* with a  0.0051% in the share of women in the workforce. The relationship is significant, with a p-value
* of 0.04.

* There is no doubt that while being statistically significant, these relationships are very weak.
* Furthermore, other than that, none of our IVs are statistically significant when included in our regression
* model.

* -----------------------------------------
* Chi-square tests - only an illustration
* -----------------------------------------
* As both our DV and our IVs are continuous, we used correlations and regressions to illustrate
* the relationship between our IVs and our DV.
* However, just to apply what we have learned during the course, we will now recode two variables in categories
* and run a chi-squared test. 

tab workforce4 fertility6, chi2     

tab workforce4 education7, chi2

* ========================
* = EXPORT MODEL RESULTS =
* ========================

* Wipe any previous regression estimates.
eststo clear

* Model 1: 'Baseline model'.
eststo M1: qui reg workforce GDPcapita muslim fertility education_square catholic_log agriculture

* Re-read, in simplified form.
leanout:

* Model 2: Adding the interactions.
eststo M2: qui reg workforce muslim catholic_log agriculture c.GDPcapita##(c.fertility /// 
c.education_square) c.fertility#(c.education_square c.agriculture) /// 
c.muslim#c.agriculture
 
* Re-read, in simplified form.
leanout:

* Compare all models on screen.
esttab M1 M2, lab b(1) se(1) sca(rmse) ///
mti("Without interaction" "With interaction") replace

* Export all models for comparison and reporting.
esttab M1 M2 using bosch_dubler_lin_regressions.txt, replace /// 
	lab b(1) se(1) sca(rmse) ///
  



* END OF DO-FILE FINAL.
*-----------------------------
*    .-~~^-.
*  .'  O    \
* (_____,    \
*  `----.     \
*        \     \
*         \     \
*          \     `.             _ _
*           \       ~- _ _ - ~       ~ - .
*            \                              ~-.
*             \                                `.
*              \    /               /       \    \
*               `. |         }     |         }    \
*                 `|        /      |        /       \
*                  |       /       |       /          \
*                  |      /`- _ _ _|      /.- ~ ^-.     \
*                  |     /         |     /          `.    \
*                  |     |         |     |             -.   ` . _ _ _ _ _ _
*                  |_____|         |_____|                ~ . _ _ _ _ _ _ _ >
* HANS
  
* Where did Hans come from so quickly? https://www.asciiart.eu/animals/reptiles/dinosaurs 
