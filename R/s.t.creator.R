# SUmmary table creator
# required inputs: dataset - the data.frame object containing variables you want summarised
# interest vars -  a list of the variable names you want summarised, in quotations
# summary vars -  a list of the variable names you want to summarise the interest vars by, in quotations
# req.fun -  what funtion you with to use to summarise them by e.g. mean, sd etc...
s.t.creator<-function(dataset, interest.vars, summary.vars, req.fun){
  level.lengths<-0 # a variable working out how big the output data.frame needs to be
  labels<-"All.data" # a variable which labels the data
  samp.size<-length(dataset[,1]) # the sample size of the data
  # populate the three variables above
  for (i in 1:length(summary.vars)){
    level.lengths<-level.lengths+length(levels(dataset[,summary.vars[i]]))
    labels<-append(labels,levels(dataset[,summary.vars[i]]))
    this.samp<-aggregate(dataset[,1]~dataset[,summary.vars[i]],data=dataset,FUN=length)
    samp.size<-append(samp.size,this.samp[[2]])
  }

  # create a matrix to populate with output and add sample size and labels to it
  data.matrix<-matrix(nrow=level.lengths+1,ncol=length(interest.vars)+2)
  data.matrix[,1]<-labels
  data.matrix[,2]<-samp.size


  # populate the remainder of the matrix with the required function
  for (i in 1:length(interest.vars)){
    base<-req.fun(na.omit(dataset[,interest.vars[i]]))
    for (j in 1:length(summary.vars)){
      current.summary<-aggregate(dataset[,interest.vars[i]]~dataset[,summary.vars[j]],data=dataset,FUN= function(x) req.fun(na.omit(x)))
      base<-append(base,current.summary[[2]])
      labels<-append(labels,levels(dataset[,summary.vars[j]]))
    }
    base<-round(base,1)
    data.matrix[,i+2]<-base
  }
  data.matrix<-data.frame(data.matrix) # convert the matrix into a dataframe
  names(data.matrix)<-c("Subset","N",interest.vars) # name the variables
  return(data.matrix)
}



