pacePredictor <- function(DateNew=268,
                                HR=170,
                                Vert=100,
                                Distance=26.2,
                                Cadence=170,
                                Elevation=100,
                                Track=FALSE,
                                restDays=1,
                                Effort = NULL,
                                modFit){
  if(!is.null(Effort)){
    predDf=data.frame(DateNew=DateNew,
                      HR=HR,
                      Vert=Vert,
                      Distance=Distance,
                      Cadence=Cadence,
                      Elevation=Elevation,
                      Track=Track,
                      restDays=restDays,
                      Effort=Effort,stringsAsFactors=F)
  }else{
    predDf=data.frame(DateNew=DateNew,
                      HR=HR,
                      Vert=Vert,
                      Distance=Distance,
                      Cadence=Cadence,
                      Elevation=Elevation,
                      Track=Track,
                      restDays=restDays,stringsAsFactors=F)
  }
  res <- predict(modFit,
                 predDf,
                 interval='prediction')
  return(res)
}