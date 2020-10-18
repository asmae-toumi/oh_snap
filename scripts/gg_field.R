library(ggplot2)


## ggplot function (default is for whole field)
gg_field <- function(yardmin=0, yardmax=120, buffer=5){
  
  ## field dimensions (units=yards)
  xmin <- 0
  xmax <- 120
  
  ymin <- 0
  ymax <- 53.33
  
  ## field number locations
  numlocs <- c(20,30,40,50,60,70,80,90,100) - 1
  fieldnums <- data.frame(nums=c(1,2,3,4,5,4,3,2,1), 
                          x=numlocs, y1=ymin+12, y2=ymax-12)
  numlocs2 <- c(20,30,40,50,60,70,80,90,100) + 1
  fieldnums2 <- data.frame(nums=0, 
                           x=numlocs2, y1=ymin+12, y2=ymax-12)
  
  ## tick mark locations
  tickmarks <- data.frame(bottom=(70*12+9)/36, top = ymax - (70*12+9) / 36, yard=11:109)
  tickmarks2 <- data.frame(bottom=ymin + 0.5, top = ymax-0.5, yard=11:109)
  
  
  ## plot object
  p <- ggplot() + 
  
    ## add grass (with buffer on sidelines in case players run OOB)
    geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin-buffer, ymax=ymax+buffer), 
              fill='forestgreen') +
  
    ## add end zones
    geom_rect(aes(xmin=xmin, xmax=xmin+10, ymin=ymin, ymax=ymax), fill='darkgreen') +
    geom_rect(aes(xmin=xmax-10, xmax=xmax, ymin=ymin, ymax=ymax), fill='darkgreen') +
  
    ## add yardlines every 5 yards
    geom_vline(aes(xintercept=seq(15,105,by=5)),col='white') +
  
    ## add thicker lines for endzones, midfield, and sidelines
    geom_vline(aes(xintercept=c(10,60,110)), lwd=1.5, col='white') +
    geom_hline(aes(yintercept=c(ymin, ymax)), lwd=1.5, col='white') + 
  
    ## add numbers every 10 yards
    geom_text(data=fieldnums, aes(x=x, y=y1, label=nums), col='white', cex=5) + 
    geom_text(data=fieldnums2, aes(x=x, y=y1, label=nums), col='white', cex=5) + 
  
    ## upside-down numbers top of field
    geom_text(data=fieldnums, aes(x=x+2, y=y2, label=nums), col='white', cex=5, angle=180) + 
    geom_text(data=fieldnums2, aes(x=x-2, y=y2, label=nums), col='white', cex=5, angle=180) + 
  
  
    ## add tick marks every yard - middle of field
    geom_segment(data=tickmarks, aes(x=yard, y=bottom - 0.5, xend=yard, yend=bottom + 0.5), 
               color='white') + 
    geom_segment(data=tickmarks, aes(x=yard, y=top - 0.5, xend=yard, yend=top + 0.5), 
               color='white') +
  
    ## add tick marks every yard - sidelines
    geom_segment(data=tickmarks2, aes(x=yard, y=top - 0.5, xend=yard, yend=top + 0.5), 
               color='white') +
    geom_segment(data=tickmarks2, aes(x=yard, y=bottom - 0.5, xend=yard, yend=bottom + 0.5), 
               color='white') + 
  
    ## add weird conversion lines at 2-yard line
    geom_segment(aes(x=12, y=(ymax-1)/2, xend=12, yend=(ymax+1)/2), color="white") +
    geom_segment(aes(x=108, y=(ymax-1)/2, xend=108, yend=(ymax+1)/2), color="white") +
  
    ## cover up lines outside of field
    geom_rect(aes(xmin=0, xmax=xmax, ymin=ymax, ymax=ymax+buffer), fill='forestgreen') +
    geom_rect(aes(xmin=0, xmax=xmax, ymin=ymin-buffer, ymax=ymin), fill='forestgreen') +
  
    ## remove axis labels and tick marks
    labs(x="", y="") +
    theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
          axis.ticks = element_blank()) +

    ## clip view of field
    coord_cartesian(xlim=c(yardmin, yardmax),ylim = c(ymin-buffer,ymax+buffer), expand = FALSE)
  
  
  return(p)
}

