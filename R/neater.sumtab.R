#' @title Neater sumtab, summary tables
#' @description returns a summary table froma  dataset
#' @param dataset The dataset containing variables to summarize
#' @param interest.vars A list of the numeric variables you want summarized
#' @param summary.vars A list of factorial variables you want the interest.vars split by
#' @details Takes a data.frame and summarises the interest.vars by the summary.vars you specify
#'
#' Outputs a table with columns: 1. Subset, 2 Sample size (N), 3:end summarised variables
#'
#' Rows: 1. Full data characteristics, 2:end levels of summary.vars
#'
#' Numeric variables are reported as mean (Sd)
#'
#'
#' @examples
#' set.seed(695)
#' x<-data.frame(gl(2,5,labels=c("Level 1", "Level 2")),rnorm(10,4))
#' names(x)<-c("factor.var","numeric.var")
#' neater.sumtab(dataset=x, interest.vars="numeric.var", summary.vars="factor.var")
#'
#' @export
neater.sumtab<-function(dataset, interest.vars, summary.vars){
  means<-s.t.creator(dataset = dataset,interest.vars = interest.vars,summary.vars = summary.vars, req.fun=mean)
  sds<-s.t.creator(dataset = dataset,interest.vars = interest.vars,summary.vars = summary.vars, req.fun=sd)
  out.table<-means
  for (i in 3:length(names(out.table))){
    out.table[,i]<-paste(means[,i]," (",sds[,i],")",sep= "")
  }
  return(out.table)
}
