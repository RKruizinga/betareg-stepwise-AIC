get_steps_aic = function(predictor_list, direction='backward', response_var=data$response, dataset=data) {
  predictors = predictor_list
  my_results_df = get_step_df()
  #if forward >>
  get_next_level = TRUE
  
  if (direction =='forward') {
    current_step = list('1')
    model_name = as.character(current_step)
    model_name = paste(model_name, collapse = '+')
    model_name = paste(response_var, model_name, sep="~")
  } else {
    current_step = predictors
    model_name = as.character(predictors)
    model_name = paste(model_name, collapse = '+')
    model_name = paste(response_var, model_name, sep="~")
  }
  
  #print(as.formula(model_name))
  my_results_df = get_ctr_aic(model_name, as.formula(model_name), my_results_df, current_step, response_var, dataset)
  
  current_best_step =  my_results_df[my_results_df$aic == min(my_results_df$aic),]
  
  while (get_next_level) {
    step_df = get_step_df()
    last_added_predictor = tail(current_best_step$step, n=1)
    if (direction=='forward') {
      predictors = predictors[!predictors %in% last_added_predictor[[1]]]
    }
    
    for (predictor in predictors) {
      if (current_best_step$step[[1]][1] == '1') {
        current_step = predictor
        model_name = as.character(current_step)
      } else {
        if (direction =='forward') {
          if (is.null(predictor)) {
            current_step = current_best_step$step[[1]]
          } else {
            current_step = append(current_best_step$step[[1]], predictor)
          }
        } else {
          current_step = predictors[predictors != predictor]
        }
        #print(current_step)
      }
      model_name = as.character(current_step)
      model_name = paste(model_name, collapse = '+')
      model_name = paste(response_var, model_name, sep="~")
      
      my_results_df = get_ctr_aic(model_name, as.formula(model_name), my_results_df, current_step, response_var)
      
      step_df[nrow(step_df)+1,] <- my_results_df[nrow(my_results_df),]
      #print(as.formula(model_name))
    }
    
    step_df$aic = as.numeric(step_df$aic)
    if (current_best_step$aic <= step_df$aic[step_df$aic == min(step_df$aic)]) {
      get_next_level = FALSE
    }
    
    current_best_step = step_df[step_df$aic == min(step_df$aic),]
    if (direction == 'backward') {
      predictors = current_best_step$step[[1]]
    }
    
    #print(current_best_step$name)
  }
  
  return(my_results_df)
}

get_aic <- function(name, formula_ctr, results_df, current_step, response_var, dataset) {
  #print(results_df)
  dataset_stat = report_aic_kfold_results(dataset, response_var, formula_ctr)
  
  new_row = nrow(results_df)+1
  results_df[as.character(new_row),]$name = name
  results_df[as.character(new_row),]$aic = dataset_stat[1]
  results_df[as.character(new_row),]$step = list(current_step)
  
  return(results_df)
} 


report_aic_kfold_results <-function(data, selector, formula) {
  
  kfold_amount = 10
  folds <- createFolds(selector, k = kfold_amount)
  
  aic_list = c()
  
  for (i in 1:kfold_amount) {
    if (i < 10) {
      fold_id = paste(c("Fold0", i), collapse = "")
    } else {
      fold_id = paste(c("Fold", i), collapse = "")
    }
    #print(fold_id)
    train_set = data[-folds[[fold_id]],]
    
    
    model = betareg(formula, data = train_set)
    
    aic_list[length(aic_list)+1] <- AIC(model)
  }
  aic = mean(unlist(aic_list)) 
  
  return(c(aic))
}

get_step_df <- function() {
  step_df = data.frame(matrix(c('none',100000, ''),nrow=1,ncol=3))
  colnames(step_df) = c('name', 
                        'aic',
                        'step')
  step_df['name'] = as.character(step_df['name'])
  step_df$aic = as.numeric(as.character(step_df$aic))
  step_df$step = as.list(step_df$step)
  
  return(step_df)
}