#' Plot Survey Summaries - SHINY VERSION
#'
#' Plots the different results from `surveySimShiny()`
#'
#' @details This function will plot the results of the `surveySimShiny` Simulations using Kernel Density plots.
#' All the grids that are to be compared should be grouped into 1 list (list(a,b,c,etc...))
#' The plot function allows you to choose different parts of the survey summaries produced by `SurveySim()` you want to plot.
#'
#' @param SummaryList a list of survey summaries, the output of `SurveySim()`
#' @param plot what variable to plot. Options are:
#'
#'   `sites.found` - plots frequency of sites found
#'
#'   `survey.hits` - plots frequency of surveys that hit a site
#'
#'   `success.rate.index` - plots the success rate of surveys, i.e. the ratio of successful surveys over total surveys
#'
#'   `sites.found.ARTI` - plots frequency of sites found **based on artifact present in sites**
#'
#'   `survey.hits.ARTI` - plots frequency of surveys that found **at least one artifact**
#' @param labels vector with name of each item in list, to be added to the legend. If `NULL`, names will be taken from list
#' @examples
#'  #create 3 Simulations with sites of different sizes:
#'  small.sites<-ParametersExample
#'  small.sites$site.area=500
#'
#'  medium.sites<-ParametersExample
#'  medium.sites$site.area=1000
#'
#'  large.sites<-ParametersExample
#'  large.sites$site.area=2000
#'
#'  #run the 3 simulations
#'  small.survey<-SurveySim(small.sites)
#'  medium.survey<-SurveySim(medium.sites)
#'  large.survey<-SurveySim(large.sites)
#'
#'  #create the comparative plot.
#'  #note that the results go into a list. If labels are not given, legend is built on list names
#'  PlotSurveySumm(list(small.survey,medium.survey,large.survey),plot="sites.found",labels=c("Small sites","Medium sites","Large sites"))
#'
#' @export
plotSurveySummShiny<-function(SummaryList,plot="sites.found",labels=NULL){
  
  #1.Define the variable to be plotted
  if(plot=="sites.found"){
    targetmatrix=2
    targetcol=5
    MainTitle="Frequency of sites intercepted"
    colorscale="magma"
    color.min=0
    color.max=0.7
  }
  
  if(plot=="survey.hits"){
    targetmatrix=2
    targetcol=6
    MainTitle="Frequency of STPs that intercepted sites"
    colorscale="inferno"
    color.min=0.3
    color.max=1
  }
  
  if(plot=="success.rate.index"){
    targetmatrix=2
    targetcol=10
    MainTitle="STP Success Rate Index"
    colorscale="plasma"
    color.min=0
    color.max=1
  }
  
  if(plot=="sites.foundARTI"){
    targetmatrix=3
    targetcol=5
    MainTitle="Frequency of sites detected"
    colorscale="viridis"
    color.min=0
    color.max=1
  }
  
  if(plot=="survey.hitsARTI"){
    targetmatrix=3
    targetcol=6
    MainTitle="Frequency of STPS that detected sites"
    colorscale="cividis"
    color.min=0
    color.max=1
  }
  
  #2.Create the data.frame that will be passed to ggplot
  plotlabels = rep("",length(SummaryList))
  plotdata=data.frame(matrix(NA,0,2))
  means = data.frame(matrix(NA,length(SummaryList),2))
  
  for(a in 1:length(SummaryList)){
    if(is.null(labels)==TRUE){
      if(is.null(names(SummaryList))==TRUE){
        plotlabels[a]=paste("Summary",a)
      }else{
        plotlabels[a]=names(SummaryList)[a]
      }
    }else{
      plotlabels[a]=labels[a]
    }
    
    tmp.dataframe<-data.frame(matrix(NA,nrow(SummaryList[[a]][[targetmatrix]]),2))
    
    tmp.dataframe[,1]=rep(plotlabels[a],nrow(SummaryList[[a]][[targetmatrix]]))
    tmp.dataframe[,2]=SummaryList[[a]][[targetmatrix]][,targetcol]
    plotdata = rbind(plotdata,tmp.dataframe)
    
    means[a,1] = plotlabels[a]
    means[a,2] = mean(tmp.dataframe[,2])
    
  }
  
  colnames(plotdata)=c("groups","data")
  colnames(means)=c("groups","mean")
  plotdata[,1]<-factor(plotdata[,1])
  means[,1]<-factor(means[,1])
  
  if(plot=="success.rate.index"){
    breaks=seq(min(plotdata$data),max(plotdata$data),length=11)
  }else{
    breaks=seq(0,1,length=11)
  }
  
  #3.Create the plot
  ggplot(plotdata,aes(x=data,color=groups,fill=groups))+
    #geom_density(alpha=0.5)+
    geom_histogram(alpha=0.5, breaks=breaks,position="dodge")+
    geom_vline(data=means,aes(xintercept=mean, color=groups), linetype="dashed", size=1)+
    #xlim(-0.05,ifelse(plot=="success.rate.index",max(plotdata$data),1))+
    ggtitle(MainTitle)+
    labs(x=ifelse(plot=="success.rate.index","Success Rate Index","frequency"),
         color = "Summaries",fill="Summaries")+
    theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
    scale_color_viridis(discrete=TRUE, option=colorscale,begin=color.min, end=color.max)+
    scale_fill_viridis(discrete=TRUE, option=colorscale,begin=color.min, end=color.max)
  
  
}

