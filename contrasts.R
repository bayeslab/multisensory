new.contrast.matrix <-
  cbind(
        Ball2 = c(0, 1, 2, 3),
        Ball3 = c(0, 0, 1, 0),
        Ball5 = c(0, 0, 0, 1)
    )
contrasts(task2$VisCondition) <- new.contrast.matrix

B = matrix( 
    c(-1, 1, 0, -1, 0, 1),
    nrow=3, 
    ncol=2) 
contrasts(task2$MusicExp) <- B