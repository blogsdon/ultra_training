foo <- data.table::fread('data/hundo_training.csv')
foo$Date <- as.Date(foo$Date,tryFormats=c("%m/%d/%y"))

g <- ggplot2::ggplot(foo,ggplot2::aes(x=HR,
                                      y=GAP,
                                      color=Effort,
                                      size=Distance,
                                      alpha=Date))

g <- g + ggplot2::geom_point()

g


g <- ggplot2::ggplot(foo,ggplot2::aes(x=Date,
                                      y=HR*GAP,
                                      color=Effort,
                                      size=Distance,
                                      alpha=log2(1/(Vert+1))))

g <- g + ggplot2::geom_point()

g

foo$bpgam <- foo$GAP*foo$HR
foo$bpmile <- foo$Pace*foo$HR
g <- ggplot2::ggplot(foo,ggplot2::aes(x=Date,
                                      y=(bpgam)))

g <- g + ggplot2::geom_smooth()

g <- g + ggplot2::geom_point()

g

foo$DateNew <- as.numeric(foo$Date)-min(as.numeric(foo$Date))
foo$restDays <- c(1,diff(foo$DateNew))
summary(MASS::rlm(bpmile ~ DateNew + Vert+log10(Distance)+Cadence+log10(Elevation),foo))
summary(lm(bpmile ~ DateNew + Vert+log10(Distance)+(Cadence)+log10(Elevation) + Vert*log10(Distance) + Cadence*Vert + Track,foo))
summary(lm(bpmile ~ DateNew + Vert+log10(Distance)+(Cadence)+log10(Elevation) + Vert*log10(Distance) + Cadence*Vert + Track+restDays,foo))

bar <- lm(bpmile ~ Vert+log10(Distance)+Cadence+log10(Elevation) + Vert*log10(Distance) + Cadence*Vert + Track +restDays,foo)

bar2 <- lm(bpmile ~ Vert+log10(Distance)+Cadence+log10(Elevation) + Vert*log10(Distance) + Cadence*Vert + Track + log10(Effort) +DateNew +restDays,foo)

summary(bar2)

bar3 <- lm(bpmile ~ Vert+log10(Distance)+Cadence+log10(Elevation) + Vert*log10(Distance) + Cadence*Vert + Track + DateNew+restDays,foo)


summary(bar2)

dM <- dplyr::select(foo,
                    DateNew,
                    Vert,
                    Distance,
                    Cadence,
                    Elevation,
                    Effort,
                    Track,
                    Temp,
                    HR,
                    Pace,
                    Track,
                    GAP,
                    restDays)

dM$Distance <- log10(dM$Distance)
dM$Elevation <- log10(dM$Elevation)
dM$Effort <- log10(dM$Effort)
res2 <- umap::umap(scale(dM))



umapDf <- data.frame(dim1=res2$layout[,1],
                     dim2=res2$layout[,2],
                     stringsAsFactors = F)

umapDf <- cbind(umapDf,foo)

g <- ggplot2::ggplot(umapDf,
                     ggplot2::aes(dim1,
                                  dim2,
                                  color=Track,
                                  alpha=DateNew,
                                  size=Distance))

g <- g+ggplot2::geom_point()

g


foo$resid <- bar$residuals
foo$residFull <- scale(bar3$residuals)
foo$residEffort <- scale(bar2$residuals)
foo$pEffort <- pchisq(foo$residEffort^2,1,lower.tail=F)

g <- ggplot2::ggplot(foo,ggplot2::aes(x=Date,
                                                                                  y=resid))

g <- g + ggplot2::geom_smooth()

g <- g + ggplot2::geom_point()

g


g <- ggplot2::ggplot(foo,ggplot2::aes(x=Date,
                                      y=scale(residFull)))

g <- g + ggplot2::geom_smooth()

g <- g + ggplot2::geom_point()

g


pacePredictor <- function(DateNew=268,
                          HR=170,
                          Vert=100,
                          Distance=26.2,
                          Cadence=170,
                          Elevation=100,
                          Track=FALSE,
                          restDays=1,
                          modFit){
  predDf=data.frame(DateNew=DateNew,
                    HR=HR,
                    Vert=Vert,
                    Distance=Distance,
                    Cadence=Cadence,
                    Elevation=Elevation,
                    Track=Track,
                    restDays=restDays,stringsAsFactors=F)                          
  res <- predict(modFit,predDf,interval='prediction')
  return(res)
}

pacePredictorEffort <- function(DateNew=268,
                          HR=170,
                          Vert=100,
                          Distance=26.2,
                          Cadence=170,
                          Elevation=100,
                          Track=FALSE,
                          restDays=1,
                          Effort = 20,
                          modFit){
  predDf=data.frame(DateNew=DateNew,
                    HR=HR,
                    Vert=Vert,
                    Distance=Distance,
                    Cadence=Cadence,
                    Elevation=Elevation,
                    Track=Track,
                    restDays=restDays,
                    Effort=Effort,stringsAsFactors=F)                          
  res <- predict(modFit,predDf,interval='prediction')
  return(res)
}



res<-(lm(Pace ~ DateNew +
           (HR) +
           Vert+
           log10(Distance)+
           (Cadence)+
           log10(Elevation) +
           Vert*log10(Distance) +
           HR*Cadence+
           Cadence*Vert +
           Track + 
           restDays,foo))

resEffort <-(lm(Pace ~ DateNew +
           (HR) +
           Vert+
           log10(Distance)+
           (Cadence)+
           log10(Elevation) +
           Vert*log10(Distance) +
           HR*Cadence+
           Cadence*Vert +
           Track + 
           restDays +
           log10(Effort),foo))


res2<-(lm(Pace ~ DateNew +
           (HR) +
           Vert+
           log10(Distance)+
           (Cadence)+
           log10(Elevation) +
           Track + 
           restDays,foo))

res2Effort<-(lm(Pace ~ DateNew +
            (HR) +
            Vert+
            log10(Distance)+
            (Cadence)+
            log10(Elevation) +
            Track + 
            restDays + 
            log10(Effort),foo))

pacePredictor(HR=135,
              DateNew=(298),
              Distance=100,
              Cadence=172,
              Vert=6000,
              Elevation=300,
              restDays=1,
              modFit=res2)

pacePredictorEffort(HR=141,
              DateNew=(298),
              Distance=100,
              Cadence=174,
              Vert=6000,
              Elevation=300,
              restDays=1,
              Effort=300,
              modFit=res2Effort)


pacePredictor(HR=164,
              DateNew=(298),
              Distance=6.2,
              Cadence=180,
              Vert=1,
              Elevation=10,
              restDays=1,
              modFit=res)

pacePredictor(HR=170,
              DateNew=(298),
              Distance=26.2,
              Cadence=180,
              Vert=1,
              Elevation=100,
              restDays=1,
              modFit=res2)

pacePredictorEffort(HR=159,
              DateNew=(200+365),
              Distance=26.30,
              Cadence=178,
              Vert=787,
              Elevation=100,
              restDays=0,
              Effort=266,
              modFit=resEffort)
 
