# betareg-stepwise-AIC Example usage

```
#list all the predictors in your model
predictor_list = c(  'match_type',
                     'ad_placement',
                     'device',
                     'day_of_week',
                     'avg_position',
                     'avg_cost',
                     'sim_keyword_headline',
                     'sim_keyword_description',
                     'wm_keyword_headline',
                     'wm_keyword_description',
                     'headline_count_words',
                     'headline_count_characters_capital',
                     'headline_count_characters',
                     'headline_count_punct',
                     'description_count_words',
                     'description_count_characters_capital',
                     'description_count_characters',
                     'description_count_punct',
                     'keyword_count_words',
                     'keyword_count_characters'
)

#call the function for backward stepwise AIC betareg
CTR_backwards_df = get_steps_aic(predictor_list, direction='backward', response_var=ctr_data$CTR, dataset=ctr_data)

#call the function for forward stepwise AIC betareg
CTR_forward_df = get_steps_aic(predictor_list, direction='forward', response_var=ctr_data$CTR, dataset=ctr_data)
```
