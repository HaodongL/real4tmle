
####First attempt
#example reference https://github.com/tlverse/tmle3mopttx/blob/master/R/Optimal_Rule_Revere.R#L200


Realistic_Rule_Asmany <- R6Class(
  classname = "Realistic_Rule_Asmany",
  portable = TRUE,
  class = TRUE,
  inherit = tmle3_Spec,
  lock_objects = FALSE,
  public = list(
    initialize = function(tmle_task, tmle_spec, likelihood, V = NULL, p=0.05, learners) {
      private$.tmle_task <- tmle_task
      private$.tmle_spec <- tmle_spec
      private$.likelihood <- likelihood
      private$.learners <- learners
      private$.p <- p
      
      if (missing(V)) {
        V <- tmle_task$npsem$W$variables
      }
      
      private$.V <- V
    },
    
    
    train_data = function(tmle_task, fold = NULL) {
      if (is.null(fold)) {
        tmle_task$data[, self$V, with = FALSE]
      } else {
        tmle_task$data[, self$V, with = FALSE][tmle_task$folds[[fold]]$training_set, ]
      }
    },
    
    valid_data = function(tmle_task, fold = NULL) {
      if (is.null(fold)) {
        tmle_task$data[, self$V, with = FALSE]
      } else {
        tmle_task$data[, self$V, with = FALSE][tmle_task$folds[[fold]]$validation_set, ]
      }
    },
    
    rule = function(tmle_task, fold_number = "full") {
      
      likelihood <- self$likelihood
      Y <- tmle_task$get_tmle_node("Y")
      
      # Need to grab the propensity score:
      g_learner <- likelihood$factor_list[["A"]]$learner
      
      g_task_v <- make_sl3_Task(data = valid_data, covariates = self$V,
                              outcome = Y, outcome_type = 'binomial')
      
      g_task_t <- make_sl3_Task(data = train_data, covariates = self$V,
                              outcome = Y, outcome_type = 'binomial')
      
      g_fit <- g_learner$train(g_task_t)
      
      g_preds <- g_fit$predict(g_task_v)
      
      # Only for binary A
      rule_preds <- as.numeric(g_preds >= self$p)
      
      return(rule_preds)
    }
  ),
    active = list(
      tmle_task = function() {
        return(private$.tmle_task)
      },
      tmle_spec = function() {
        return(private$.tmle_spec)
      },
      likelihood = function() {
        return(private$.likelihood)
      },
      V = function() {
        return(private$.V)
      },
      A_library = function() {
        return(private$.learners$A)
      }
    ),
    private = list(
      .tmle_task = NULL,
      .tmle_spec = NULL,
      .likelihood = NULL,
      .V = NULL,
      .learners = NULL,
      .opt_delta = NULL,
      .opt_A = NULL,
      .Q_vals = NULL
    )
  )
    
