#' Logistic Asset Allocation in R (laar)
#'
#' Implements logistic asset allocation strategy
#' @param v initial value
#' @param tdate date vector with trading days
#' @param s numeric price vector
#' @param k numeric value for option strike price
#' @param vol value for volatility
#' @param r value for interest rate, r = 3 implements 3 %
#' @param tcost numeric transaction costs in %
#' @return instance of the laar class
#' @export

laar <- function(
  v,
  tdate,
  s,
  theta = 15,
  r = 3,
  tcost = 0
){

  # validation of arguments

  # missing arguments
  if (missing(v))
    stop("No initial value specified")

  if (missing(tdate))
    stop("No date vector specified")

  if (missing(f))
    stop("No price vector specified")

  # invalid arguments

  if (tcost < 0)
    stop("Transaction cost cannot be negative")

  if (v < 0 & theta > 0)
    stop("Use negative theta for floor insurance")

  if (v > 0 & theta < 0)
    stop("Use positive theta for cap insurance")

  if (length(tdate) != length(s))
    stop("Date and price vectors must be of equal length")

  # define vectors
  pv <- vector(length(s), mode = "numeric")        # portfolio value
  h <- vector(length(s), mode = "numeric")         # hedge pos in s
  tr <- vector(length(s), mode = "numeric")        # transaction
  hper <- vector(length(s), mode = "numeric")      # hedge percentage
  exp <- vector(length(s), mode = "numeric")       # exposed
  ch <- vector(length(s), mode = "numeric")        # hedge cost
  r <- vector(length(s), mode = "numeric")         # portfolio t-day return
  sig <- vector(length(s), mode = "numeric")       # sigmoid value at t

  # t=1 initial return, sigmoid value, hedge, transaction,
  # exposure, costs and portfolio unit price, including transaction costs
  r[1] <- 0                             # t-day return in the portfolio is r_t = 0
  sig[1] <- 1/(1 + exp(-theta * r[1]))  # the sigmoid at t = 1
  h[1] <- (sig[1]*v)
  tr[1] <- h[1]
  exp[1] <- v - h[1]
  hper <- abs(h[1]/v)
  ch[1] <- (s[1] + tcost*sign(tr[1]))*tr[1]
  pv[1] <- (s[1] * exp[1] + ch[1])

  # t=2,..,T
  for(t in 2:(length(s))){
    # evaluate mtm of portofolio with market price at time t
    pp[t] <- (ch[t-1] + s[t] * exp[t-1])/v

    if (v > 0) {                        # t-day log return in the portfolio
      r[t] <- (log(pp[t]/pp[1])-1) * 100
    } else {
      r[t] <- (log(pp[1]/pp[t])-1) * 100
    }
    sig[t] <- 1/(1 + exp(-theta * r[t]))  # the sigmoid at t

    h[t] <- (sig[t]*v)
    tr[t] <- h[t] - h[t-1]
    exp[t] <- v - h[t]
    hper[t] <- abs(h[t]/v)
    # calculate portfolio value, including transactions costs
    ch[t] <- ch[t-1] + (s[t] + sign(tr[t]) * tcost) * tr[t]
    pp[t] <- (s[t] * exp[t] + ch[t])/v
  }

  Results <- data.frame(Date = tdate,
                       Price = s,
                       Traded = tr,
                       Exposed = exp,
                       Hedged = h,
                       HedgeRate = hper,
                       Value = pp)

  # data frame with results
  return(Results)

  # # create an instance of the laar class
  # out <- new("laar",
  #            Name="laar",
  #            InitialValue=v,
  #            TargetPrice=TargetPrice,
  #            StrikePrice=k,
  #            AnnVol=vol,
  #            InterestRate=r,
  #            TradingDays=tdays,
  #            TransCost=tcost,
  #            TradeisInt=int,
  #            Results=data.frame(
  #              Date=tdate,
  #              Price=s,
  #              Traded=tr,
  #              Exposed=exp,
  #              Hedged=h,
  #              HedgeRate=hper,
  #              Target = rep(TargetPrice,length(s)),
  #              PortfPrice=pp))
  #
  # # return OBPI object
  # return(out)
}
