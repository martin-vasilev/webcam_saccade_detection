get_xy <- function(data, smooth){
  
  d <- data %>%
    dplyr::transmute(
      x = x,
      y = y,
      time = time_start,
      GT = ground_truth
    )
  
  if(nrow(d) < 5) {
    return(d)
  }
  
  if(smooth == "raw") {
    
    return(d)
    
  } else if(smooth == "mean") {
    
    d <- eyemovements::SmoothSamples(
      d,
      method = "Mean",
      window_size = 3
    )
    
  } else if(smooth == "median") {
    
    d <- eyemovements::SmoothSamples(
      d,
      method = "Median",
      window_size = 3
    )
    
  } else if(smooth == "sg") {
    
    if(nrow(d) >= 23) {
      d$x <- signal::sgolayfilt(d$x, p = 5, n = 23)
      d$y <- signal::sgolayfilt(d$y, p = 5, n = 23)
    }
  }
  
  return(d)
}
