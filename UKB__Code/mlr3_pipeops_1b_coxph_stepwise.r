source("~/DS_Project_Code/mlr3_pipeops_0_init.r")

future::plan(list(future::tweak("multisession", workers = 8) #outer
))

# NB task's eid needed for cluster() also.
# >> encode ----
po_encode$param_set$values$method = "treatment"
po_encode$param_set$values$affect_columns = selector_invert(selector_name("eid"))

# >> model (coxph) ----
# source("~/ACED-RREDD-EHR/sam/scripts/models/si_LearnerSurvCoxPH_bkwdselect.r")
source("~/DS_Project_Code/Dependency/si_LearnerSurvCoxPH_stepwise_bidirectional.r")
po_coxph_stepwise_bidirectional <- si_LearnerSurvCoxPH_stepwise_bidirectional$new()

# build pipe ----
# graph <- po_scale %>>% po_mice %>>% po_encode %>>% po_select_rmnelaa %>>% po_coxph
graph <- po_scale %>>%  po_coxph_stepwise_bidirectional #po_encode %>>%


graph$keep_results=FALSE #TRUE # for debugging

# graph learner ----
graph_learner <-  GraphLearner$new(graph)

# run pipe ----
### >> no resampling ----
cat(">> no resampling ......\n")
graph$train(task) #no resampling

