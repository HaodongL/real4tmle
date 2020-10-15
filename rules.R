
####First attempt
#example reference https://github.com/tlverse/tmle3mopttx/blob/master/R/Optimal_Rule_Revere.R#L200



Realistic_Rule_Asmany <- R6Class(
  classname = "Optimal_Rule_Revere",
  portable = TRUE,
  class = TRUE,
  inherit = tmle3_Spec,
  lock_objects = FALSE,
  public = list(
    initialize = function(tmle_task, tmle_spec, likelihood, V = NULL, p=0.05,
                          blip_type = "blip2", learners, maximize = TRUE, realistic = FALSE,
                          shift_grid = seq(-1, 1, by = 0.5)) {
      private$.tmle_task <- tmle_task
      private$.tmle_spec <- tmle_spec
      private$.likelihood <- likelihood
      private$.blip_type <- blip_type
      private$.learners <- learners
      private$.maximize <- maximize
      private$.realistic <- realistic
      private$.shift_grid <- shift_grid
      private$.p <- p
      
      if (missing(V)) {
        V <- tmle_task$npsem$W$variables
      }
      
      private$.V <- V
    },
    
    factor_to_indicators = function(x, x_vals) {
      ind_mat <- sapply(x_vals, function(x_val) as.numeric(x_val == x))
      colnames(ind_mat) <- x_vals
      return(ind_mat)
    },
    
    V_data = function(tmle_task, fold = NULL) {
      if (is.null(fold)) {
        tmle_task$data[, self$V, with = FALSE]
      } else {
        tmle_task$data[, self$V, with = FALSE][tmle_task$folds[[fold]]$training_set, ]
      }
    },
    
    rule = function(tmle_task, fold_number = "full") {
      
      likelihood <- self$likelihood
      
      # Need to grab the propensity score:
      g_learner <- likelihood$factor_list[["A"]]$learner
      
      g_task <- make_sl3_Task(data = V_data, covariates = self$V,
                              outcome = outcomes, outcome_type = 'binomial')
      
      g_preds <- g_learner$predict(g_task)
      
      # g_task <- tmle_task$get_regression_task("A")
      # g_preds <- g_learner$predict_fold(g_task, fold_number)
      
      # Only for binary A
      rule_preds <- as.numeric(g_preds >= self$p)
      
      
      return(rule_preds)
    },
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
      blip_type = function() {
        return(private$.blip_type)
      },
      blip_fit = function() {
        return(private$.blip_fit)
      },
      blip_library = function() {
        return(private$.learners$B)
      },
      A_library = function() {
        return(private$.learners$A)
      },
      shift_grid = function() {
        return(private$.shift_grid)
      }
    ),
    private = list(
      .tmle_task = NULL,
      .tmle_spec = NULL,
      .likelihood = NULL,
      .V = NULL,
      .blip_type = NULL,
      .blip_fit = NULL,
      .learners = NULL,
      .maximize = NULL,
      .realistic = NULL,
      .shift_grid = NULL,
      .opt_delta = NULL,
      .opt_A = NULL,
      .Q_vals = NULL
    )
  )
    
