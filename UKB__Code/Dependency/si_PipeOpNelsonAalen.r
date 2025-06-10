si_PipeOpNelsonAalen = R6::R6Class("si_PipeOpNelsonAalen",
    lock_objects = FALSE,
  inherit = mlr3pipelines::PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "nelsonaalen", param_vals = list()) {
      super$initialize(id = id, param_set = NULL, param_vals = NULL, feature_types = c("numeric", "factor"))
    }
  ),
  private = list(
    .train_dt = function(dt, levels=NULL, target=NULL) {
      si_nelsonaalen = function(coxdat){
                  coxdat <- coxdat %>% dplyr::mutate(
                      futime=as.numeric(futime),
                      status=as.numeric(status)
                  )
                  cat("si_nelsonaalen -- coxdat......\n")
                  print(str(coxdat))
                  hazard <- survival::basehaz(survival::coxph(survival::Surv(coxdat$futime, coxdat$status) ~ 1))
                  idx <- match(signif(coxdat$futime, digits=10), signif(hazard[, "time"], digits=10)) #mice::nelsonaalen match causing NAs!
                  nelaa <- hazard[idx, "hazard"]
                  cat("any NAs in nelaa......", any(is.na(nelaa)), "\n")
                  return(nelaa)
              }
        print("HERE......")
        print(str(dt))
        dt <- task$data() %>% as.data.frame() %>% 
            dplyr::mutate(
                nelaa = si_nelsonaalen(.),
                status=as.integer(status), eid=as.factor(eid)) #eid kept for resampling grouping and cluster() in coxph
        cat("si_PipeOpNelsonAalen -- .train_dt -- nelaa......")
        print(str(dt))
        print(head(dt %>% dplyr::select(futime, status, nelaa) %>% dplyr::filter(status==1), 10) %>% arrange(futime))
        
        self$state = list(train_coxdat_survobj = dt %>% dplyr::select(futime, status))
        dt
    },

    .predict_dt = function(dt, levels=NULL) {
        si_nelsonaalen = function(coxdat){
              coxdat <- coxdat %>% dplyr::mutate(
                  futime=as.numeric(futime),
                  status=as.numeric(status)
              )
              cat("si_nelsonaalen -- coxdat......\n")
              print(str(coxdat))
              hazard <- survival::basehaz(survival::coxph(survival::Surv(coxdat$futime, coxdat$status) ~ 1))
              idx <- match(signif(coxdat$futime, digits=10), signif(hazard[, "time"], digits=10)) #mice::nelsonaalen match causing NAs!
              nelaa <- hazard[idx, "hazard"]
              cat("any NAs in nelaa......", any(is.na(nelaa)), "\n")
              return(nelaa)
          }
        dt <- task$data() %>% as.data.frame() 
        nrows_test <- dim(dt)[[1]]
        coxdat_combined <- dplyr::bind_rows(dt, self$state$train_coxdat_survobj)
        cat("si_PipeOpNelsonAalen -- .predict_dt -- coxdat_combined......\n")
        print(str(coxdat_combined))
        dt <- coxdat_combined %>%
            dplyr::mutate(nelaa = si_nelsonaalen(.)) %>% 
            dplyr::mutate(status=as.integer(status), eid=as.factor(eid)) #eid kept for resampling grouping and cluster() in coxph
        cat("si_PipeOpNelsonAalen -- .predict_dt -- nelaa......\n")
        print(str(dt[1:nrows_test,]))
        print(head(dt %>% dplyr::select(futime, status, nelaa) %>% dplyr::filter(status==1), 10) %>% arrange(futime))
        dt[1:nrows_test,]
    }
  )
)


environment(si_PipeOpNelsonAalen) <- asNamespace('mlr3pipelines')
godmode:::assignAnywhere("si_PipeOpNelsonAalen", si_PipeOpNelsonAalen)
