Study on the Evaluation of Training Programs - Linear Regression
================
Juan Martinez
October 3, 2019

### Summary

The key question of interest for this study is to find out if workers who receive job training tend to earn higher wages, on average, as compared to the workers who do not receive job training. To answer this question, we have implemented a linear regression model to see the impact, if any, treatment and other relevant predictors have on the postintervention earnings. This empirical study did not provide evidence of a statistically significant effect of treatment on the postintervention earnings. However, the model demonstrated that the effect of treatment differs by earlier income level. Moreover, the study showed a statistically significant positive relationship of education as well as pre-training real earnings with the post-training real earnings. Lastly, interestingly, the study shows a statistically significant negative association between the interaction term of treatment and pre-training earnings with the post-training earnings. However, it is important to note that the model used in this study has several limitations, which are described in detail in the last section.

### Introduction

Will job training help one to make more money? This question has been asked by numerous social scientists and economists, and there is no definite answer on this subject. However, many studies showed that job training has not been very effective in this regard and they were more likely to benefit women than men and youth (Lalonde, 1995).

**Literature Review:** The best way to answer this type of question would be to conduct an experimental study, where we could infer causal relationships and, potentially, use the results for policymaking. However, randomized experimental studies are often very expensive, time-consuming and may not be a feasible option. Then, the question becomes whether the observational studies could replicate or be very similar to the unbiased experimental findings. This topic was addressed by several researchers, including Lalonde (1986) who in his influential paper demonstrated that nonexperimental estimators are not the same as unbiased experimental findings. For his evaluation, Lalonde used both a randomized evaluation of the National Supported Work (NSW) and nonexperimental comparison data taken from survey datasets. However, Dehejia & Wahba (1999) using a subset of Lalonde’s data showed that with the propensity score method, the estimates of the composite study are similar to the experimental evaluations. Some researchers argue that such a favorable result was due to a specific subset of data, and the results would be sensitive to the choice of variables and a sample. It is important to note that the current study is based on the composite dataset (both experimental and nonexperimental data), which is why it’s important to not derive any causal relationships from it.

### Data

There are a total of ten variables in the data - *treat, age, educ, black, hispan, married, nodegree, re74, re75* and *re78*. The response variable for the linear regression model is *re78*. We have taken certain important data manipulation steps which are listed below:

1.  All the continuous predictor variables (*age*, *educ* and *re74*) were mean-centered in order to avoid potential multicollinearity problems when including transformations and interactions.

2.  As seen in the plots below, since *re78* was skewed to the left, therefore, we have used **square root transformation** for the response variable after trying several other transformations.

![](Evaluation_of_trainning_programs_-_Github_files/figure-markdown_github/unnamed-chunk-2-1.png)

1.  The focus of our linear model is to see the effect of the training on a person's salary in 1978. In other words, we are not interested in the people who had no salary in 1978. For this reason, we have **removed the data points where the salary in 1978 was zero**. We will include these data points in the logistic regression model where our goal will be to know whether a person is earning a salary or not.

2.  We have not used *re75* in the analysis because the experiment had already started in 1975, so the salaries in 1975 will be highly correlated with the response variable which we do not want. Also, the subjects had been paid to participate in the treatment.

Those were all the data manipulation steps taken prior to modeling. While regressing *re78* against the predictor variables, we also want to see interactions among different predictor variables. The following plot shows an interesting association between *re74* and *treat*.

<img src="Evaluation_of_trainning_programs_-_Github_files/figure-markdown_github/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

First, let's look at the group that did not go receive the training (shown in black). In this group, the slope of the regression line is positive which shows that the higher the salary of a person in '74, the higher the salary of the person in '78.

However, in the group that did receive the training (shown in pink), it seems that as the salary in '74 goes up, the salary in '78 more or less stays the same because the slope of the regression line is almost zero. From this graph, it seems that there is some interaction between *re74* and *treat*. We'll test this interaction, along with other interactions while modeling. Also, we found that the distribution of treat was not 50:50. Only about 30% of people received the treatment - something to keep in mind while interpreting the results of modeling.

### Model Selection & Evaluation

After performing a thorough exploratory data analysis on our dataset and consulting multiple academic references we approach the model design with a goal in mind: the final linear regression model should focus on finding relevant and interpretable associations. In this sense, we include all variables that seem to be related to the post-training real earnings of individuals in 1978, given our prior data analysis and background research, even if they are not statistically significant in the full model.

This process began with the inclusion of the main variables in our database: *treat*, *age*, *educ*, *black*, *hispan*, *married*, *nodegree*, and *re74*. Afterwards, relevant interactions are included in this baseline model, such as the interaction of *treat* with such demographic categorical features as age, education and baseline income of every observation in the study. Ultimately, we make sure to include the square term of the age and also, we have mean-centered the continuous variables to avoid multicollinearity issues.

In addition, we tried several transformations for the response variable *re78*, given that its probability distribution differs dramatically from the normal distribution. In every single transformation that we tried (log, square root, Box-Cox, ArcSin) we found that the high proportion of zeros in this variable make almost impossible to convert its distribution to normal. For this reason, we decided to drop the zeros from our response variable *re78*, making our analysis only valid to people that are employed and earn more than zero dollars a year.

After dropping the zeros, we tested the same set of transformations and found that the most suitable transformation in terms of the normality assumption is the square root. Not only this transformation makes the data more normal visually, but also when we see the QQ plot of the second-best transformation (log) compared to the root transformation it is vividly more effective in normalizing our *Y*.

Having ready all the ingredients for our model selection process, we proceeded to use together the BIC, AIC and Adjusted R squared as our metrics to find the best possible model. First, we performed a stepwise regression using both the AIC and BIC using the *forward* method, having as benchmark a null model with just the intercept and as scope our baseline model. Secondly, we did the same stepwise analysis but this time starting from the baseline model and going step by step dropping features based on both AIC and BIC (backward selection).

Since BIC generally places a heavier penalty on models with many variables (n &gt; 8) our tests showed simplified models when optimizing for BIC. On the other hand, we saw better results when using AIC when comparing metrics between the models, given that it kept the majority of variables that our EDA showed as potentially impactful to our *Y*.

Finally, for our final model, we chose the model with the lowest AIC (4736.97), the highest adjusted R squared and the highest quantity of relevant variables according to our exploratory data analysis. We also tested whether interactions of treatment and demographic features add relevant information to our final model using the ANOVA function in R. We found that they were not significant.

Also, as a safety check, we re-ran the model including the whole dataset (more specifically, including zeros for the variable *re78*) and found out that the main effects of our final model do not change significantly. However, when we do include the full dataset the effects for *age* and *black* turn out to be significant (at a 10% level) compared to our final model. Although these results seem interesting from an interpretation standpoint, we conclude that it is better to drop the zero entries in *re78* and transform the *Y* variable so our model complies with the linear regression assumptions.

Our final model goes as follows:

$$
{\\sqrt{RealAnnualEarnings78}}\_{i} = \\beta\_{0} + \\beta\_{1}  \\mathrm{AgeCentered}\_{i} + \\beta\_{2}  \\mathrm{EducationCentered}\_{i} + $$
*β*<sub>3</sub>*R**e**a**l**A**n**n**u**a**l**E**a**r**n**i**n**g**s*74*C**e**n**t**e**r**e**d*<sub>*i*</sub> + *β*<sub>4</sub>*R**e**c**e**i**v**e**d**T**r**e**a**t**m**e**n**t*<sub>*i*</sub>
+*β*<sub>5</sub>*I**s**B**l**a**c**k*<sub>*i*</sub> + *β*<sub>6</sub>*R**e**a**l**A**n**n**u**a**l**E**a**r**n**i**n**g**s*74 \* *R**e**c**e**i**v**e**d**T**r**e**a**t**m**e**n**t*<sub>*i*</sub> + *ϵ*<sub>*i*</sub>

Next, we show the main plots to evaluate the compliance of linear regression assumptions:

<img src="Evaluation_of_trainning_programs_-_Github_files/figure-markdown_github/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

As seen in the model evaluation plots above, the model met all the assumptions. We calculated the VIF coefficient for each of the variables finding no risk of multicollinearity issues. Moreover, the plot of the residuals against the fitted values shows no pattern indicating the not compliance of the independence assumption for the linear regression model. Furthermore, examining the QQ plot of the residuals shows that the normality of the errors is relatively met. When it comes to heteroscedasticity, our residual plots do not show concerning fluctuations in the variance of errors across the fitted values. Finally, when plotting the residuals against our predictors we do not discover any concerning pattern in the data, this demonstrates that our model does comply with the assumption of linearity.

### Results

In this section, we are going to answer the questions asked in the team assignment. We use our final model and its results to answer these questions. As seen in *Table 1*, for the **first question** that asks if workers who receive job training tend to earn higher wages than workers who do not receive job training, we can see that *t**r**e**a**t* predictor is not statistically significant with a larger p-value than 5% significance level, and positive coefficient of 4.7513. For the **second question** (regarding the range for the difference in earnings in 1978 for workers who were trained and who have never been trained), we can look at the 95% confidence interval for the *t**r**e**a**t* dummy variable. According to the 95% CIs of *t**r**e**a**t*, for the workers who receive job training, the square root of real earnings in 78 increases by an amount which is between -4.710 and 14.212 as compared to the workers who did not receive training.

Regarding the question about any evidence that the effects differ by demographic groups, we took our final model and started including various interaction terms one by one which involved treatment with all demographic variables testing both for the coefficient significance and whether there was a significant differences in the AICs by using ANOVA test. According to our analysis, there are no significant variations of the effect of treatment by anyone of the demographic features besides the ones present at our model. In addition, the model demonstrates that there are other interesting associations. From the result of the final model, the interactions between the income in 1974 and the binary value of whether the worker received job training is significant predictors. Owing to this interaction term, the square root of the income in 1978 decrease slightly as the previous income in 1974 of the employee who received the training decreases.

Also, the intercept and the non-interaction predictors, such as *e**d**u**c* and *r**e*74 are statistically significant. This means the square root of the earning in 1978 increases 1.976 and 0.0020 respectively when *e**d**u**c* and *r**e*74 go up by one unit, keeping other predictors constant.

### Conclusion & Remarks

In conclusion, in line with earlier studies on this subject, we asserted that job training does not have a highly statistically significant effect on the postintervention earnings of workers. This applies that the training programs in the 1970s were not highly effective in reducing the number of poor people. Also, this empirical study did not show evidence that the association between the postintervention earnings and treatment differs by demographical groups, except for the previous annual income (proxied by pre-training real earnings). In other words, on average, men who received training and had higher earnings in 1974 tend to make less in 1978 compared to men who did not receive the training. The possible reasons for this surprising finding are discussed earlier. Furthermore, the study demonstrates that men, with more education, on average, tend to earn higher earnings and that the higher the pre-training earnings a worker had, the higher his post-training earnings tend to be, on average.

**Limitations and Suggestions for Further Research:** It is important to note that the final linear model has several limitations. As mentioned in the literature review, econometric procedures (linear regression) may not replicate the experimentally determined results, which is why we should be aware of the specification errors when making inferences for this model. Also, the control and treatment groups differ and do not have the same type of people, which could introduce bias into our evaluation. Furthermore, the adjR2 for the final regression model was only 0.14, which implies that there are probably other relevant predictors ( e.g. proxies for market conditions in the labor market, economic health, and others). Lastly, from the diagnostic plots, we could see that the normal Q-Q plot was slightly skewed from both sides, which we should keep in mind if we were to predict the post-training earnings based on the predictors in the model.

For future research, apart from adding the above-noted predictors, it would be interesting to see the effect of job training on women and youth. Earlier studies showed that the effect of treatment on the postintervention earnings was higher for women than men (Lalonde, 1995). Lastly, it would be interesting to check the robustness of the model by using different econometric methods. However, due to the limited time and resources, these suggestions could not be implemented for the current study.
