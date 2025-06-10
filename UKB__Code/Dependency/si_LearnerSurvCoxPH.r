# OG: LearnerSurvCoxPH -- https://github.com/mlr-org/mlr3proba/blob/8b4d15cbe6ce8f02fa75caeb0521c1a7b2d9d27b/R/LearnerSurvCoxPH.R
si_LearnerSurvCoxPH = R6::R6Class("si_LearnerSurvCoxPH",
  inherit = LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "si_surv.coxph",
        param_set = ps(
          ties        = p_fct(default = "efron", levels = c("efron", "breslow", "exact"), tags = "train"),
          singular.ok = p_lgl(default = TRUE, tags = "train"),
          type        = p_fct(default = "efron", levels = c("efron", "aalen", "kalbfleisch-prentice"), tags = "predict"),
          stype       = p_int(default = 2L, lower = 1L, upper = 2L, tags = "predict")
        ),
        predict_types = c("distr", "crank", "lp"),
        feature_types = c("logical", "integer", "numeric", "factor"),
        properties = "weights",
        packages = c("survival", "distr6")
        # label = "Cox Proportional Hazards",
        # man = "mlr3proba::mlr_learners_surv.coxph"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")

      if ("weights" %in% task$properties) {
        pv$weights = task$weights$weight
      }
      print("pv ......")
      print(pv)

      print("task$data() ......")
      print(head(task$data() ))

      mdl <- mlr3misc::invoke(survival::coxph, 
        formula = as.formula(
        paste0("survival::Surv(time=futime, event=status) ~",paste(c(task$feature_names[!task$feature_names %in% c("eid")], "cluster(eid)"),  collapse="+")
        )), 
        data = task$data(),  x = TRUE) # .args = pv,
      print(mdl)
      return(mdl)
    },

    .predict = function(task) {

      newdata = task$data(cols = task$feature_names)

      # We move the missingness checks here manually as if any NAs are made in predictions then the
      # distribution object cannot be create (initialization of distr6 objects does not handle NAs)
      if (matrixStats::anyMissing(newdata)) {
        stop(sprintf(
          "Learner %s on task %s failed to predict: Missing values in new data (line(s) %s)\n",
          self$id, task$id,
          paste0(which(!complete.cases(newdata)), collapse = ", ")))
      }

      pv = self$param_set$get_values(tags = "predict")
      print(pv)

      # Get predicted values
      cat("\nsi_LearnerSurvCoxPH$.predict >> newdata ...... \n")
      print(head(newdata))
      fit = mlr3misc::invoke(
        survival::survfit, 
        formula = self$model, 
        newdata = newdata,
        se.fit = FALSE, .args = pv)

      # cat("\nsi_LearnerSurvCoxPH$.predict >> self$model ...... \n")
      # print(self$model)
      lp = predict(self$model, type = "lp", newdata = newdata)

      mlr3proba::`.surv_return`(times = fit$time, surv = t(fit$surv), lp = lp)
    }
  )
)


environment(si_LearnerSurvCoxPH) <- asNamespace("mlr3proba")
# assignInNamespace("si_LearnerSurvCoxPH", si_LearnerSurvCoxPH, ns = "mlr3proba")
godmode:::assignAnywhere("si_LearnerSurvCoxPH", si_LearnerSurvCoxPH)