#' Analyzing DHS data and other survey data
#'


svy.proptable = function(data = data,
                         psu = "psu",
                         weights = "weights",
                         strata = "strata",
                         outcome = "outcome",
                         predictors = varlist,
                         tablename = tablename){

  if (!requireNamespace("survey", quietly = TRUE)) {
    stop("Package \"survey\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("DescTools", quietly = TRUE)) {
    stop("Package \"DescTools\" needed for this function to work. Please install it.",
         call. = FALSE)
  }


  library(survey);library(DescTools)
  svydesign <- eval(parse(text=paste0("svydesign(ids=~",psu, ",data=data", ",weights=~",weights, ",strata=~",strata,")",sep="")))

  total_category = length(table(data[[outcome]]))
  tab = data.frame()

  for (i in predictors){
    length = length(table(data[[i]]))+1
    tab0 = as.data.frame(matrix(ncol = total_category+1, nrow = 0))

    for (j in c(1:length(table(data[[i]])))){
      observed = svytable(as.formula(paste0("(~",outcome,"+",i,")",sep="")),svydesign)[,j]
      total = sum(observed)
      prop= (BinomCI(observed, total, conf.level = 0.95, method = "logit")*100)
      tab0[1,1] = i
      tab0[1,c(1:total_category+1)] = ""
      tab0[c(2:length),1] = rownames(table(data[[i]]))
      tab0[j+1,c(1:total_category+1)]=paste(sprintf("%3.1f",(prop[,1])),"% (",
                                            sprintf("%3.1f",(prop[,2])),"%-",
                                            sprintf("%3.1f",(prop[,3])),"%)", sep="")
    }
    tab = rbind(tab,tab0)
  }
  print(tab)
  write.csv(tab, paste(deparse(substitute(tablename)),".csv",sep=""))
}

#svy.proptable (data= data,
#               psu = "psu",
#               weights = "weights",
#               strata = "strata",
#               outcome = "outcome_variable",
#               predictors = c("predictor1","predictor2","predictor3","predictor4"),
#               tablename = Table2)
