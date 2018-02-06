sfpi <- function(
  q,
  tdate,
  f,
  theta = 15,
  tcost = 0,
  int = TRUE
){
  
  # validation of arguments and digits option
  #############################################################################
  # missing arguments
  if (missing(q))
    stop("No volume specified")
  
  if (missing(tdate))
    stop("No date vector specified")
  
  if (missing(f))
    stop("No price vector specified")
  
  # invalid arguments
  
  if (tcost < 0)
    stop("Transaction cost cannot be a negative number")
  
  if (q < 0 & theta > 0)
    stop("Use negative theta for floor insurance")
  
  if (q > 0 & theta < 0)
    stop("Use positive theta for cap insurance")
  
  if (length(tdate) != length(f))
    stop("Date and price vectors must be of equal length")
  
  # volume restrictions
  if(int==FALSE){
    digits<-10        # model without tradeable volume restrictions (int=FALSE)
  } else {
    digits<-0         # model with smallest tradeable volume unit = 1 (int=TRUE)
  }
  #############################################################################
  
  # define vectors (for both strategies) and t = 1 calculations
  #############################################################################
  pp <- vector();pp2 <- vector()          # portfolio price
  h <- vector();h2 <- vector()            # hedge
  tr <- vector();tr2 <- vector()          # transaction
  hper <- vector();hper2 <- vector()      # hedge percentage
  exp <- vector();exp2 <- vector()        # exposed
  ch <- vector();ch2 <- vector()          # hedge cost
  r <- vector();r2 <- vector()            # portfolio t-day return
  s <- vector();s2 <- vector()            # sigmoid value at t

  
  # t = 1 initial return, sigmoid value, hedge, transaction, (same for both strategies)
  # exposure, costs and portfolio unit price, including transaction costs
  r[1] <- r2[1] <- 0                           # t-day return in the portfolio is r_t = 0
  s[1] <- s2[1] <- 1/(1 + exp(-theta * r[1]))  # the sigmoid at t = 1
  h[1] <- h2[1] <- round((s[1]*q),digits)            
  tr[1] <- tr2[1] <- h[1]
  exp[1] <- exp2[1] <- q - h[1]
  hper[1] <- hper2[1] <- abs(h[1]/q)
  ch[1] <- ch2[1] <- (f[1] + tcost*sign(tr[1]))*tr[1]
  pp[1] <- pp2[1] <- (f[1] * exp[1] + ch[1])/q
  #############################################################################
  
  # t = 2,..,T for first strategy
  #############################################################################
  for(t in 2:(length(f))){
    # evaluate mtm of portofolio with market price at time t
    pp[t] <- (ch[t-1] + f[t] * exp[t-1])/q
    
    if (q > 0) {                        # t-day return in the portfolio
      #r[t] <- (pp[t] - pp[1])/(pp[1])
      r[t] <- log(pp[t]/pp[1])
    } else {
      #r[t] <- (pp[1] - pp[t])/(pp[1])
      r[t] <- log(pp[1]/pp[t])
    }
    s[t] <- 1/(1 + exp(-theta * r[t]))  # the sigmoid at t
    
    h[t] <- round((s[t]*q), digits)
    tr[t] <- h[t] - h[t-1]
    exp[t] <- q - h[t]
    hper[t] <- abs(h[t]/q)
    # calculate portfolio unit price, including transactions costs
    ch[t] <- ch[t-1] + (f[t] + sign(tr[t]) * tcost) * tr[t]
    pp[t] <- (f[t] * exp[t] + ch[t])/q
  }
  #############################################################################
  
  # t = 2,..,T for first strategy
  #############################################################################
  for(t in 2:(length(f))){
    # evaluate mtm of portofolio with market price at time t
    pp2[t] <- (ch2[t-1] + f[t] * exp2[t-1])/q
    
    if (q > 0) {                        # t-day return in the portfolio
      #r2[t] <- (f[t] - f[1])/(f[1])
      r2[t] <- log(f[t]/f[1])
    } else {
      #r2[t] <- (f[1] - f[t])/(f[1])
      r2[t] <- log(f[1]/f[t])
    }
    s2[t] <- 1/(1 + exp(-theta * r2[t]))  # the sigmoid at t
    
    h2[t] <- round((s2[t]*q), digits)
    tr2[t] <- h2[t] - h2[t-1]
    exp2[t] <- q - h2[t]
    hper2[t] <- abs(h2[t]/q)
    # calculate portfolio unit price, including transactions costs
    ch2[t] <- ch2[t-1] + (f[t] + sign(tr2[t]) * tcost) * tr2[t]
    pp2[t] <- (f[t] * exp2[t] + ch2[t])/q
  }
  #############################################################################
  
  # dataframe with results
  Results <- data.frame(Date = tdate,
                        Price = f,
                        # strategy 1
                        Traded = tr,
                        Exposed = exp,
                        Hedged = h,
                        HedgeRate = hper,
                        PortfPrice = pp,
                        # strategy 2
                        Traded2 = tr2,
                        Exposed2 = exp2,
                        Hedged2 = h2,
                        HedgeRate2 = hper2,
                        PortfPrice2 = pp2)
  
  # data frame with results
  return(Results)
}