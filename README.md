# Spatial-Stats
Spatial Stats project on race and gender wage gap

ABSTRACT
For as long as anyone can remember, there has always existed gender and racial inequality in American wages. That said, there are reasons as to why this discrepancy exists. Some argue that after controlling for type of job, industry, and educational attainment, the discrepancy between male and female wages is not so large after all. Others will rebuttal that the gap prevails even after adjustments and thus, needs to be resolved. On the other hand, there seems to be less debate surrounding whether or not there is a difference in wages for various racial groups—in other words, it is almost a consensus that there indeed exists discrepancies in income for various racial groups. This paper looks at the distribution of males, females, whites, blacks, and Asians, and how their income compare to population density as well as educational attainment.

BACKGROUND

Just recently published in the New York Times, Claire Miller discusses how the gender pay gap persists and how the Trump administration is failing to tackle this problem. Miller notes “while the pay gap shrinks after controlling for factors like industry, education and hours worked,” a gap still persists nevertheless (Miller). According to the Pew Research Center, even after controlling for education, white women’s earnings are a mere 82% of white men’s. Similarly, for other racial groups: Asian women earn 87% of what white men earn, black women 65%, and hispanic women 58% (Patten). Clearly, white men are earning higher wages than women of all different racial groups, and there exists further discrimination based on race. Interestingly, Asian men earn more on average than white men, and Asian women earn slightly more on average than white women.

In order to analyze and see why this is, this report looks at data from the American Community Survey (ACS), a nationwide survey that collects over 3.5 million households and produces information on demographic, social, economic, and housing characteristics about the U.S.’s population. Information from the ACS provides companies and the government with tools of ways to analyze the current state of the nation and how communities are changing, on both an economic and social scale. Not only does the ACS data help the nation zoom in on specific groups and create policies to ensure greater equality and reduce bias, but it also looks holistically at the entirety of the nation’s well-being and predicts future progressions.

DATA

Variables included in this analysis were 
Total Population: B01003
Sex by Age: B01001
Race: B02001
Sex by Educational Attainment For the Population 25 Years or Older: B15002
Per Capita Income In the Past 12 Months (White Alone): B19301A
Per Capita Income In the Past 12 Months (Black Alone): B19301B
Per Capita Income In the Past 12 Months (Asian Alone): B19301D

First, this paper takes a close look at the total population, the division of males and females, and their respective income levels in the U.S.
A graph of the total population by county is shown below on the left. The most populated areas have a lighter, yellow color, while the less populated counties are a darker blue. Double checking the result of the graph with information from online, the yellow county in California correlates to the fact that Los Angeles, California is the most populated county in the U.S., at 10 million. The graph on the right side shows the median income based on county. Counties that are lighter in color have higher median incomes, and according to the map, these lighter counties correspond to cities. This makes sense since the median household income is greater in urban areas than in rural areas. 

Next, the spread of females vs. males from ages 18-64 is delineated in the two graphs below. This age range was chosen because it is the most probable age range when people are working. Note that the overall color of the graph on the left (proportion of the population that is female) is lighter and more yellow than the one on the right (for males). This corresponds to the fact that there are more females in the U.S than there are males.  The real point of question, however, is why even though there are more females, they are still getting paid less than men.

To depict the relationship among population density, income, and sex, two ggplots were created. The plot on the left shows population density on the x axis, the proportion of males on the y axis, and the income—denoted as log median income—as the color. As seen on the right portion of this graph, in more urban areas, males tend to earn more (represented by the lighter, yellow color). The plot on the right depicts population density on the x axis, the proportion of females on the y axis, and the income as the color. 
  
The proportion of females is higher than males, and hence the data points are clustered at a higher y-value than the data points are for the graph of males on the left. Likewise to males, females tend to earn more in urban areas. But whereas there are some rural areas with higher proportion of males, the distribution of the proportion of females seems to be more scattered than as for males. Although it appears that there may be more data points that are lighter in color for the male graph, it’s difficult to tell if this is really the case, and if the female graph is darker overall (which would indicate that females are getting paid less than males). Overall, both seem to have similar distributions in terms of income, and more analysis is necessary.
Initially, this paper was aimed to take a deep dive into analyzing the reasons as to why females were getting paid less than males. But upon research, it was discovered that these reasons—such as biologically female inclined traits, female preferences in jobs, and employer preferences—were hard to capture in data. For example, women generally value flexibility in hours over wage earnings, as they may take absences due to pregnancy or to take care of children. Moreover, many of these possible reasons were not available in the ACS dataset. 

Next, this paper takes a look at the relationship among population density, race, educational attainment, and income. 

The ACS data denoted race by per capita income in the past 12 months. The chosen specific community groups were whites, blacks, and Asians. First, a ggplot was created for whites only, denoting log population density on the x axis, the proportion of the population that is white on the y axis, and the log of median income (for white people only) as color. In more rural areas, the proportion of whites is nearly 1, meaning that the county consists almost entirely of white people. The log median income for white people ranges from 9.5 to 11. Based on the color scheme, the average white person seems to earn 10 log units of income. It appears that on average in rural areas, whites earn more when the county has more white people. The most wealthy of the white population, however, are in cities, but in cities the proportion of white people decreases significantly. This makes sense as there tends to be the most diversity in large cities. 

The same approach was taken to study the black population in the U.S. Like before, log population density is on the x axis, the proportion of the population that is black on the y axis, and the log of median income (for black people only) is color. Here, the log income for black people ranges from 8 to 12, this is a wider range than it was for whites only. Based on this graph, it appears that the majority of black people make roughly the same amount of money as white people, as most of the data points for black people are a green color, indicating roughly 10 units of log income. However, there does not seem to be as many black people who make 12 units of log income, meaning that there may be less wealthy black people than white people. The portion of black people that make the least income (a dark blue color at 6 log units of income), are located in the rural areas where the proportion of black people is not extremely high. Furthermore, there’s a large spike around -12logpopdens, an area that isn’t extremely high in population density but that has a spread of black people in various locations, some more highly populated with black people. Like white people, the richest portion of black people seem to be located in the most urban areas.

Lastly, the same thing is done for Asians in the U.S. Again, log population density is on the x axis, the proportion of the population that is Asian on the y axis, and the log of median income (for Asian people only) is color. Compared to both whites and blacks, there is a much lower proportion of Asians in the U.S. Thus, the data points are mostly clustered around the horizontal axis, where the proportion is closer to zero. Like whites and blacks, the richest of Asians are located in the most urban areas. Unlike whites but like blacks, there seems to be less yellow data points, meaning that there may not be extremely wealthy Asian people. It appears that the highest proportion, or the densest of Asians, are located in cities. Like black people, the range of log income for Asians people is greater than the range for white people. But the range for Asians is 7 to 12, which is an even lower minimum than that of black people. 
Based on the ggplots, it appears that Asians have a higher median income (at 10 or 11 log units of income) than do both whites and blacks (whose median income centers more around 10), with blacks having the lowest median income. This parallels the information from the Pew Research Center.

MODELS

In order to understand why there is such a discrepancy in income for the different races, a few statistical tests can be done. The response of interest is the income for the specific racial group (i.e. white, black, or Asian), and the covariates of interest are the specific racial group and educational attainment. Note that it would have been better to have the educational attainment for each specific racial group.

Originally, four models were fitted: model in uncorrelated and constant variance residuals, CAR model, SAR model, and flat earth geostatistical model (Matérn with nugget).Unfortunately, the flat earth geostatistical model (Matérn with nugget) kept crashing, so there were only three models. 

In order to find the best model for the data, the three models were first fitted for whites only. The model with the largest log likelihood would then be used to fit for blacks only and Asians only as well. The table below is for whites only, where the response was income for whites only, and the covariates were the white racial group and educational attainment. The income response was a quadratic function of the log population density for whites and a linear function of educational attainment. The table has coefficient values for the intercept, population density, population density squared, and education. It also has the standard errors for each term, as well as the log likelihood at the end. It is clear here that the SAR model is the best, with the highest log likelihood at 2316.581.

Note that the coefficient for education is lower for the SAR model than for the uncorrelated and CAR models. The population coefficient is negative for the uncorrelated and SAR models while it is positive for the CAR model. And similarly, the coefficient for population squared is positive for the uncorrelated and SAR models but negative for the CAR model, meaning that the curve is convex for the uncorrelated and SAR models and concave for the CAR model. All the standard errors are pretty similar. The intercept coefficient for the SAR model is in between the coefficients of the uncorrelated and CAR model intercept coefficients. Overall, besides the education coefficient, the models are not very different, but the log likelihood for the SAR model is the highest, at a whopping 2316.581. It would be best to see how it compares to the geostatistical model, but unfortunately, after many attempts, R would crash after trying to run that model.

A few residual plots are given for the uncorrelated and constant variance residuals model. The Normal QQ plot shows that the residuals are overall pretty normally distributed and don’t deviate much from the line. There seems to be three data points that might be outliers, but overall the residuals look good. That said, this model is not the best one, so that means that the SAR plot must have an even better fit, but of course, constant variance residual plots cannot be shown for the SAR or CAR models.

The summary output below is for the SAR model, and it is for whites only. The coefficient of interest here is really the proportion of the population that has a college degree or higher, which is 1.470662. This will be noted later when compared to the coefficients for other racial groups.

Next, an SAR model is fit for blacks only, where the response and covariates of interest are all the same except that it is in terms of the black population. The intercept coefficient is similar to that of the model fit for the white population, but the coefficients for the population density of black people are lower. Interestingly, the p-values for the coefficient terms for population density are not statistically significant, signifying that perhaps population density is not closely related to income, or that a different relationship (not quadratic) would better capture the black population. Furthermore, the quadratic term coefficient for population density is positive for white people but negative for black people, meaning that the curve for the white population is convex while the curve for the black population is concave. This indicates that the average white income increases when areas are more densely populated with white people, whereas the opposite is true for areas are more densely populated with black people—income decreases. The coefficient for education is higher for blacks than for whites, meaning that educational attainment has a greater influence on income for blacks than income for whites. 

Lastly, an SAR model is fit for Asians only. Similar to the model for white people, the quadratic coefficient for population density for Asians is positive, meaning that the curve is convex, indicating that the average Asian income increases when areas are more densely populated with Asian people. The coefficient for the intercept is not very different from that of the white or black people models, but the coefficients for the population density are much higher, with the squared term extremely high. The coefficient for education is lower than for both blacks and whites.

ANALYSIS

A key finding from the SAR models is that the coefficient for educational attainment is significantly different across the three racial groups. It is the highest for blacks at 1.903735, then for whites at 1.470662, and the lowest for Asians at 1.206512. This means that educational attainment has a greater influence on income for black people than it does on white people or Asian people.

This information seems to make sense when pairing it with other statistics, such as the proportion of each racial group that has a bachelor’s degree or higher. According to the ACS, 27.9% of the total population aged 25 and older have a bachelor’s degree or higher. This percentage increases to 29.3% when looking solely at the white population, decreases to 17.7% for the black population only, and increases again to 50.2% for the Asian population (López). Over half of the Asian population in the U.S. has a bachelor’s degree or higher, while not even a fifth of the black population has a bachelor’s degree or higher. This, along with the overwhelming evidence that higher levels of education lead to higher income, makes sense as to why the coefficient for educational attainment is higher for blacks than it is for whites and Asians. Since only a portion of the black population has a bachelor’s degree or higher, the people that have this degree make significantly more than those who don’t. Moreover, if a black person receives educational attainment, his/her income could significantly increase, thus indicated by the larger education coefficient. On the other hand, a large portion of the Asian population already has this educational attainment, and thus, the coefficient makes less of an impact on the median income. 

Furthermore, it is crucial to note that there is an overwhelmingly larger portion of white people than both black and Asian people. According to the Census Bureau data, 76.9% of the entire U.S. population is white, 13.3% is black, and 5.6% is Asian (US Census Bureau). And upon a closer look, 73% of the adult Asian population was born in another country, with some already having educational degrees from their home country (López). Thus, the circumstance is a bit different for Asians than it is for whites and blacks, where the Asian population’s high educational attainment may be a result of the heavy share of the Asian population that immigrated to the U.S. with a bachelor’s degree or higher already. 

Although there was not as much information on female vs. male wages, it is important to address this gender gap as well. Next steps would be to gather data outside of ACS to analyze whether or not biological traits are statistically significantly for males and females. For example, studies show that females tend to negotiate less than males, are shyer when it comes to competition, are more agreeable than men, and are more risk averse. 

Economically speaking, increasing wage increases the probability of labor force participation due to the substitution effect. Thus, if females were to have an increase in their wages, then more females would participate in the labor force. According to the Bureau of Labor Statistics, the labor force participation rate for females in 2016 was 56.8%, while the LFP rate for males was 69.2% (Bureau of Labor Statistics). Although the LFP for females has been steadily rising and the LFP for males has been decreasing, males still have a significantly larger LFP than do females. Perhaps increasing female wages and decreasing the gender pay gap could incentivize some females to join the labor force. 

A study done by Jake Rosenfeld in Britain found that making people aware of the issue helped minimize the pay gap. Rosenfeld drew a number of conclusions from his study: 1) some individuals did not even know that they were being paid less than others 2) this led them to speak up and negotiate their pay 3) since this tended to be females, this overall effect decreased the gender wage gap (Rosenfeld). If women or minority racial groups were told that they are being paid less than others, then perhaps this could push people to vocalize their pay rates and negotiate for fairer pay. 

CONCLUSION

Based on the data in this report, it appears that different racial groups earn different incomes based on location, population density, proportion of their racial group in the location, and educational attainment. All three racial groups tend to earn more in cities. Whites also tend to earn more in densely white-populated locations while Asians and blacks don’t really have a distinct pattern. Overall, however, while it appears that a good proportion of the black population earns the same relative to whites and Asians, there are fewer cases of extremely wealthy black people (denoted as the yellow data point) as compared to whites and Asians. Additionally, Asians on average seem to earn slightly more than both whites and blacks. This could stem from the fact that the proportion of Asians that have a bachelor’s degree or higher is vigorously greater than it is for whites and blacks. 

Second, educational attainment of a bachelor’s degree or higher has more of an impact on the log median income for black people than it does for white and Asian people. In particular, the median Asian income is least influenced by the educational attainment coefficient. This could be due to the fact that over half of the Asian population already has a bachelor’s degree or higher, while less than a fifth of the black population has reached this level of education. Since education most certainly influences wages, if a black person were to receive education, then this would highly influence his/her income. Whereas in the case for Asians, the coefficient is weaker (lower in value, not p-value significance) because many Asians have already attained a bachelor’s degree or higher. Whites have a moderate coefficient compared to blacks and Asians, and this could partially be due to the significantly larger proportion of white people in the U.S. with greater variation in educational attainment in the white population.

While there was not as much analytics done on male vs. female wages, it is clear that there are slightly more females in the U.S. than there are males, but females are still getting paid less than males. While one possible explanation may seem to be that the demand for females is lower than for males because there are more females available in the U.S., the female labor force participation rate for females is lower than males, indicating that the actual workforce lacks females. Further research on the biological and behavioral traits of females vs. males may be useful in studying this gender pay gap. 
In conclusion, there exists both a racial and gender inequality in American wages, and this may be a result of educational attainment, geographic location, and population density of racial groups. The government should continue to collect data like the ACS data used in this report so that closer inspection of changes and trends can be noted. The government should instigate policies to reduce the pay gap and create further equality in the U.S.
 



BIBLIOGRAPHY

Bureau of Labor Statistics, United States Department of Labor. 24 October, 2017. https://www.bls.gov/emp/ep_table_303.htm

López, Gustavo, et al. “Key Facts about Asian Americans, a Diverse and Growing Population.” Pew Research Center, 8 Sept. 2017, www.pewresearch.org/fact-tank/2017/09 /08/key-facts-about-asian-americans/

Miller, Claire Cain. “One Effort to Close the Gender Pay Gap Won’t Get a Try Under Trump.” New York Times, 31 August, 2017, https://www.nytimes.com/2017/08/31/upshot/one-effort-to-close-the-gender-pay-gap-wont-get-a-try.html

Patten, Eileen. “Racial, Gender Wage Gaps Persist in U.S. despite Some Progress.” Pew Research Center, PEW, 1 July, 2016, www.pewresearch.org/fact-tank/2016/07/01/racial-gender-wage-gaps-persist-in-u-s-despite-some-progress/

Rosenfeld, Jake. “The Power of Transparency” British Workplace Survey. 1 September, 2015. http://journals.sagepub.com/doi/abs/10.1177/0003122415597019

Tsang, Amie. “British Companies Must Reveal How They Pay Women vs. Men” New York Times, 6 April, 2017. https://www.nytimes.com/2017/04/06/business/britain-salary-gender-gap.html

United States Census Bureau. 2015 Census. U.S. Census Bureau, 2105. https://www.socialexplorer.com/data/ACS2015/metadata/?ds=ACS15
	
US Census Bureau. 1 July, 2016, https://www.census.gov/quickfacts/fact/table/US/PS T045216
