# OG: LearnerSurvCoxPH -- https://github.com/mlr-org/mlr3proba/blob/8b4d15cbe6ce8f02fa75caeb0521c1a7b2d9d27b/R/LearnerSurvCoxPH.R
si_LearnerSurvCoxPH_stepwise_bidirectional = R6::R6Class("si_LearnerSurvCoxPH_stepwise_bidirectional",
  inherit = LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "si_surv.coxph_stepwise_bidirectional",
        param_set = ps(
          ties        = p_fct(default = "efron", levels = c("efron", "breslow", "exact"), tags = "train"),
          singular.ok = p_lgl(default = TRUE, tags = "train"),
          type        = p_fct(default = "efron", levels = c("efron", "aalen", "kalbfleisch-prentice"), tags = "predict"),
          stype       = p_int(default = 2L, lower = 1L, upper = 2L, tags = "predict")
        ),
        predict_types = c("distr", "crank", "lp"),
        feature_types = c("logical", "integer", "numeric", "factor"),
        properties = "weights",
        packages = c("survival", "distr6", "rms", "dplyr")
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
      # print("pv ......")
      # print(pv)

      print("task$data() ......")
      print(str(task$data()))

      full_fml <- paste0("survival::Surv(time=futime, event=status) ~",paste(c(task$feature_names[!task$feature_names %in% c("eid")]),  collapse="+"))  #no need cluster(eid) for AIC
      
      min_cols <- task$feature_names[grepl("^gen_|^core_age|^core_sex", task$feature_names)]
      min_fml <- paste0("survival::Surv(time=futime, event=status) ~",ifelse(length(min_cols)==0, 1, paste(min_cols,  collapse="+"))) #no need cluster(eid) for AIC

      # fit_full <- rms::cph(formula = as.formula(full_fml),## Run full model with all covariates
      #                      data= task$data(), x = TRUE, y = TRUE)
      # backwards <- rms::fastbw(fit_full, rule = "p", sls = 0.05)# Perform backwards selection with p-value rule and threshold of 0.2    
      full_fit <- survival::coxph(as.formula(full_fml), data= task$data())
      min_fit <-  survival::coxph(as.formula(min_fml), data= task$data())  # Null model

      fit_stepwise_bidirectional <- stats::step(
        min_fit,
        scope = list(lower = min_fit, upper = full_fit),
        data= task$data(), steps=500, direction="both")
      
      covars_selected <- attributes(fit_stepwise_bidirectional$terms)$term.labels 
      cat("covars_selected......\n")
      print(covars_selected)

      covars_kept <- c(covars_selected) %>% unique()
      cat("covars_kept (inc forced)......\n")
      print(covars_kept)
      
      red_fml <- paste0("survival::Surv(time=futime, event=status) ~",paste(c(covars_kept[!covars_kept %in% c("eid")], "cluster(eid)"),  collapse="+"))
      if (length(covars_kept)==0){
        red_fml <- "survival::Surv(time=futime, event=status) ~ 1"
      }


      cat("red_fml......\n")
      print(red_fml)

      mdl <- mlr3misc::invoke(survival::coxph, 
        formula = as.formula(red_fml), 
        data = task$data(),  x = TRUE) # .args = pv,
      print(mdl)
      mdl
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
      print(str(newdata)); print(self$model)
      fit = mlr3misc::invoke(
        survival::survfit, 
        formula = self$model, 
        newdata = newdata,
        se.fit = FALSE, .args = pv)
      cat("\nsi_LearnerSurvCoxPH$.predict >> done fit for ...... \n"); print(self$model)
      # cat("\nsi_LearnerSurvCoxPH$.predict >> self$model ...... \n")
      # print(self$model)
      lp = predict(self$model, type = "lp", newdata = newdata)
      cat("\nsi_LearnerSurvCoxPH$.predict >> done lp (predict) for ...... \n"); print(self$model)

      if (is.null(dim(fit$surv))){
        cat("\nis.null(dim(fit$surv)) TRIGGERED for ...... \n"); print(self$model); print(str(fit)); print(str(newdata))
        N_ppl <- dim(newdata)[1]
        fit$surv <- replicate(N_ppl, fit$surv)
        dimnames(mat_intcpt1) <- list(NULL, 1:N_ppl)
      }

      mlr3proba::`.surv_return`(times = fit$time, surv = t(fit$surv), lp = lp)
    }
  )
)


environment(si_LearnerSurvCoxPH_stepwise_bidirectional) <- asNamespace("mlr3proba")
# assignInNamespace("si_LearnerSurvCoxPH", si_LearnerSurvCoxPH, ns = "mlr3proba")
godmode:::assignAnywhere("si_LearnerSurvCoxPH_stepwise_bidirectional", si_LearnerSurvCoxPH_stepwise_bidirectional)