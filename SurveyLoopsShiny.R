#' Survey Loops - SHINY version
#'
#' Perform multiple survey simulations changing values on one variable and plot the results
#'
#' @details
#' `SurveyLoops` will run a series of simulations along one variable with values
#'  provided by user. Through this function, the user can simulate and evaluate the changes
#'  in efficiency and efficacy of specific variables, while holding every other value constant. The function
#'  runs multiple instances of `SurveySim` using values of `SurveyParameters` and replacing one of them with a sequence of values offered by the user.
#'
#' @param SurveyParameters list of parameters (object class `SurveySim`)
#'
#' @param LoopVariable variable to be looped. Can be any of the variables that exist in `SurveyParameters`
#'
#' `col.width` - vector of numbers with distances between STP rows
#'
#' `grid.type` - vector of strings with names of grid types
#'
#' `simulations` - vector of numbers with numbers of simulations
#'
#' `area` - **list** with 2 vectors, one for x and one for y of area (vectors MUST be same length)
#'
#' `site.density` - vector with numbers of site density OR **list** with 2 vectors, one with minimum site densities, one with maximum (vectors MUST be same length)
#'
#' `site.area` - vector with numbers of site areas OR **list** with 4 vectors, one with means, one with st.devs,one with minimums, one with maximums (vectors MUST be same length)
#'
#' `overlap` - vector of numbers with varying overlaps
#'
#' `obj.density` - vector with varying artifact density OR **list** with 2 vectors, one with minimum artifact densities, one with maximum (vectors MUST be same length)
#'
#' `obj.distribution` - vector of names of artifact distribution
#'
#' `survey.radius` vector with varying survey radii
#' @param LoopSequence object with varying values, as defined above

#' @examples
#' #Loop the impact of increasing distances between survey rows
#' width.loop<-SurveyLoops(ParametersExample,"col.width",c(25,50,75,100,125,150),"SitesFound")
#'
#' #Loop the impact of different artifact distributions on surveys
#' distr.loop<-SurveyLoops(ParametersExample,"obj.distribution",c("uniform","linear","spherical","sinusoidal"),"SitesFoundOnArtifacts")
#'
#' @export

surveyLoops<-function(SurveyParameters,LoopVariable,LoopSequence,grid_ratio=NULL){

  #1.we do a series of tests to allow for partial text matching and checking for errors

  LOOPVARIABLE<-c("col.width","grid.type","simulations","area","site.density","site.area",
                  "overlap", "obj.density", "obj.distribution", "survey.radius")

  LoopVariable<-pmatch(LoopVariable,LOOPVARIABLE)


  if(is.na(LoopVariable)==TRUE){
    stop("ERROR: LoopVariable not valid. Chose a valid variable to loop.\n")
  }

  if(is.list(LoopSequence)==TRUE){
    same.size=TRUE
    for (a in 1:(length(LoopSequence)-1)){
      if(length(LoopSequence[[a]])!=length(LoopSequence[[a+1]])){
        same.size=TRUE
      }
    }
    if(same.size==FALSE){
      stop("ERROR: Vectors inside list in LoopSequence are not the same size.\n")
    }
  }

  if(LoopVariable==4 & is.list(LoopSequence)==FALSE){#if Area input is not a list
    stop("ERROR: LoopSequence for Area must be a list with 2 vectors (mins and maxs).\n")
  }

  #2. Here we start with creating the basics and results object

  if(is.list(LoopSequence)==TRUE){
    nloops<-length(LoopSequence[[1]])
  }else{
    nloops<-length(LoopSequence)
  }
  
  #in shiny, I'm saving results as dataframes with loop on column 1 to plot is through ggplot
  results<-list(SurveysPerSim = data.frame(matrix(NA,nloops,2)),
                SitesFound = data.frame(matrix(NA,nloops,7)),
                SitesFoundOnArtifacts = data.frame(matrix(NA,nloops,7)),
                ArtifactsPerSurvey = data.frame(matrix(NA,nloops,7)),
                SuccessRateIndex = data.frame(matrix(NA,nloops,7)))

  colnames(results[[1]])<-c("Loop Value","N")
  colnames(results[[2]])<-c("Loop Value","Mean","St Dev","Min","Max","Quant 2.5%","Quant 97.5%")
  colnames(results[[3]])<-c("Loop Value","Mean","St Dev","Min","Max","Quant 2.5%","Quant 97.5%")
  colnames(results[[4]])<-c("Loop Value","Mean","St Dev","Min","Max","Quant 2.5%","Quant 97.5%")
  colnames(results[[5]])<-c("Loop Value","Mean","St Dev","Min","Max","Quant 2.5%","Quant 97.5%")

  if(is.list(LoopSequence)==TRUE){
    results[[1]][,1]<-paste0(LoopSequence[[1]],"|",LoopSequence[[2]])
    results[[2]][,1]<-paste0(LoopSequence[[1]],"|",LoopSequence[[2]])
    results[[3]][,1]<-paste0(LoopSequence[[1]],"|",LoopSequence[[2]])
    results[[4]][,1]<-paste0(LoopSequence[[1]],"|",LoopSequence[[2]])
    results[[5]][,1]<-paste0(LoopSequence[[1]],"|",LoopSequence[[2]])
  }else{
    results[[1]][,1]<-as.character(LoopSequence)
    results[[2]][,1]<-as.character(LoopSequence)
    results[[3]][,1]<-as.character(LoopSequence)
    results[[4]][,1]<-as.character(LoopSequence)
    results[[5]][,1]<-as.character(LoopSequence)
  }
  #3. We start the loop here, and get all the parameters we need for each run. Lots of ifs...

  tmp.SurveyParameters<-SurveyParameters
  
  
  
  if(is.null(grid_ratio)){
    grid_ratio = rep(NA,nloops)
  }
  
  
  
  withProgress(message=paste0("Iteration ",0," of ",nloops),min=0,max=nloops,value=0,{
  for(a in 1:nloops){
    
    incProgress(1,message=paste0("Iteration ",a," of ",nloops))
    
    if(LoopVariable==1){ #col.width
      tmp.SurveyParameters[[1]]<-LoopSequence[a]
    }

    if(LoopVariable==2){ #grid.type
      tmp.SurveyParameters[[2]]<-LoopSequence[a]
    }

    if(LoopVariable==3){ #simulations
      tmp.SurveyParameters[[3]]<-LoopSequence[a]
    }

    if(LoopVariable==4){ #area
      tmp.SurveyParameters[[4]]<-c(LoopSequence[[1]][a],LoopSequence[[2]][a])
    }

    if(LoopVariable==5){ #site.density
      if(is.list(LoopSequence)==TRUE){
        tmp.SurveyParameters[[5]]<-c(LoopSequence[[1]][a],LoopSequence[[2]][a])
      }else{
        tmp.SurveyParameters[[5]]<-LoopSequence[a]
      }
    }

    if(LoopVariable==6){ #site.area NOTE ORDER IS DIFFERENT HERE, since shiny input area is different
      if(is.list(LoopSequence)==TRUE){
        tmp.SurveyParameters[[6]]<-c(LoopSequence[[3]][a],LoopSequence[[4]][a],LoopSequence[[1]][a],LoopSequence[[2]][a])
      }else{
        tmp.SurveyParameters[[6]]<-LoopSequence[a]
      }
    }

    if(LoopVariable==7){ #overlap
      tmp.SurveyParameters[[7]]<-LoopSequence[7]
    }

    if(LoopVariable==8){ #obj.density
      if(is.list(LoopSequence)==TRUE){
        tmp.SurveyParameters[[8]]<-c(LoopSequence[[1]][a],LoopSequence[[2]][a])
      }else{
        tmp.SurveyParameters[[8]]<-LoopSequence[a]
      }
    }

    if(LoopVariable==9){ #obj.distribution
      tmp.SurveyParameters[[9]]<-LoopSequence[a]
    }

    if(LoopVariable==10){ #survey.radius
      tmp.SurveyParameters[[10]]<-LoopSequence[a]
    }

    #4.Here we call the SurveySim and run the loop iteration
    
    tmp.results<-surveySimShiny(tmp.SurveyParameters,plot=FALSE, grid_ratio = grid_ratio[a])

    results[[1]][a,2]<-tmp.results[[1]][1,2]
    results[[2]][a,2:7]<-tmp.results[[1]][2,2:7]
    results[[3]][a,2:7]<-tmp.results[[1]][3,2:7]
    results[[4]][a,2:7]<-tmp.results[[1]][4,2:7]
    results[[5]][a,2:7]<-tmp.results[[1]][5,2:7]
  }
})
  
  return(results)

}
