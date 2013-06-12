# ======================================
# Utilities for analyzing Bitcoin prices
# ======================================

# I should group market orders that are a single order.
# If two orders are same type (limit/market) and same side and were sent within 1 sec of each other then same order.
# I should add heatmaps

library(quantmod)
library(ggplot2)
library(gridExtra)

# change options to print with microseconds
op <- options(digits.secs=6)

# Retrieves the historical trades from MtGox
# Uses get_mtgox_trades.py
# 
# Sample run (careful with the date format)
# trades = load_trades("2013-05-25 02:00:00.00","2013-08-25 02:00:00.00")
#
load_trades = function(start_time, end_time) {
  system(paste("python /db/private/bitcoin/historical_trades/get_mtgox_trades.py -s",'"',start_time, '"'," -e ",'"',end_time,'"', sep=""))
  trades = read.csv("/db/private/bitcoin/historical_trades/mtgox_trades.csv", header = FALSE)
  colnames(trades)=c("time", "price", "volume", "properties", "type")
  trades$time = as.POSIXct(strptime(trades$time, '%Y-%m-%d %H:%M:%OS', tz="GMT"))
  trades
}

# Computes average lag for a period
ave_lag = function(start_time, end_time) { mean(lag$lag[lag$Timestamp>start_time & lag$Timestamp < end_time])  }

# Calculates one sided trade periods
# A one sided trade period is period when sellers/buyers are responsible for at least percent (ex 90%) of the volume.
# 
# Input variables
# (start_time, end_time) is the time interval to search in
# max_depth control the recursion depth (and indirectly the number of jumps returned)
# percent is the minimum percentage of buyers/sellers for the interval to be considered one sided
#
# Sample run 
# one_sided_trade_period_all_wrapper = function(trades, start_time, end_time, max_depth = 15, percent = 0.90)
#
# Implementation
# Defines a global tradesframe called jumps and populates it with one_sided_trade_period_all
# 
one_sided_trade_period_all_wrapper = function(trades, start_time, end_time, max_depth, percent) {
  #create an empty tradesframe, with column names.
  thecolnames = c("start_time", "end_time", "volume_ask","volume_bid", "volume_diff", "sold_percent", "vwap_ask",  "vwap_bid", "price_change", "price_open", "duration")
  jumps <<- data.frame(t(rep(NA,length(thecolnames))))
  colnames(jumps) = thecolnames
  # make jumps global to use with recursion
  jumps <<- jumps[-1,]
  one_sided_trade_period_all(trades, start_time, end_time, max_depth = max_depth, buyers_flag = FALSE,percent = percent)
  one_sided_trade_period_all(trades, start_time, end_time, max_depth = max_depth, buyers_flag = TRUE, percent = percent)
  jumps<-jumps[jumps$duration>3,]
  jumps<-jumps[order(jumps$volume_diff),]
  # calculate a few extra variables
  jumps$ave_lag = mapply(ave_lag, jumps$start_time, jumps$end_time)
  jumps<-jumps[order(abs(jumps$volume_diff), decreasing = TRUE),]
  jumps$shares5min = jumps$volume_diff/as.numeric(jumps$duration)*5
  jumps$price_change5min = jumps$price_change/as.numeric(jumps$duration)*5
}

# Returns all periods when sellers/buyers are responsible for at least percent (ex 90%) of the volume.
# Better use the function one_sided_trade_period_all_wrapper which does not require the jumps tradesframe to be defined
#
# Input variables
# (start_time, end_time) is the time interval to search in
# depth_idx and max_depth control the recursion depth
# buyers_flag determines whether we are looking for buys or sells
# percent is the minimum percentage of buyers/sellers for the interval to be considered one sided
#
# Sample run (requires jumps to be a global variable for the recursion)
# thecolnames = c("start_time", "end_time", "volume_ask","volume_bid", "volume_diff", "sold_percent", "vwap_ask",  "vwap_bid", "price_change", "price_open", "duration")
# jumps <<- data.frame(t(rep(NA,length(thecolnames))))
# colnames(jumps) = thecolnames
# jumps <<- jumps[-1,]
# one_sided_trade_period_all(trades, start_time, end_time, max_depth = 15, buyers_flag = FALSE,percent = 0.90)
#
# Implementation
# split the time interval in the the hard sell you just found, the right interval and left interval
# if the hard_sell you just found is small then break, otherwise record it and
# search left and right for hard_sells that are smaller than the one you just found.
# Uses function one_sided_trade_period.
# 
one_sided_trade_period_all = function(trades, start_time, end_time, depth_idx=0, max_depth=5, buyers_flag, percent) {
  start_time=as.POSIXct(strptime(start_time, '%Y-%m-%d %H:%M:%S', tz="GMT"))
  end_time=as.POSIXct(strptime(end_time, '%Y-%m-%d %H:%M:%S', tz="GMT"))
  depth_idx =+ 1
  if (depth_idx>max_depth) {return(0)}
  # two checks for time interval lead to fewer errors
  if (difftime(end_time, start_time, units="secs") < 180 ) {return(0)}
  
  trades_one_sided_trade = one_sided_trade_period(trades,start_time, end_time, percent, buyers = buyers_flag)
  # if the period is too small then breaking up into minutes does not work
  if (difftime(end_time, start_time, units="secs") < 180 ) {return(0)}
  # if the volume in the jump is small return
  if (sum(trades_one_sided_trade$volume) < 600) { return (0)}
  
  jumps <<- rbind(jumps,trade_stats(trades_one_sided_trade))
  one_sided_trade_period_all(trades, start_time, head(trades_one_sided_trade$time,1), depth_idx, max_depth, buyers_flag, percent)
  one_sided_trade_period_all(trades, tail(trades_one_sided_trade$time,1), end_time, depth_idx, max_depth, buyers_flag, percent)  
}

# Return price and volume candlesticks in the format of an xtc object
# 
# Input variables:
# time, price and volume of trades
# fmt is the format for the periods of the candlesticks
ohlc_xts <- function(ttime,tprice,tvolume,fmt)
{
  ttime.int <- as.POSIXct(strptime(format(ttime,fmt),fmt))
  Open = tapply(tprice,ttime.int,function(x) {head(x,1)})
  High = tapply(tprice,ttime.int,max)
  Low = tapply(tprice,ttime.int,min)
  Close = tapply(tprice,ttime.int,function(x) {tail(x,1)})
  Average = tapply(tprice,ttime.int,mean)
  Volume = tapply(tvolume,ttime.int,function(x) {sum(x)})
  time = ttime[tapply(1:length(ttime),ttime.int,function(x) {head(x,1)})]
  xts(cbind(Open, High, Low, Close, Average, Volume), order.by = as.POSIXct(strptime(format(time,fmt),fmt)))
}


# Calculate the period that had the largest difference between seller volume and buyer volume, in amount of volume. 
# Returns a tradesframe with the trades in the period
#
# Input variables
# (start_time, end_time) is the time interval to search in
#
period_soft_sell = function(trades, start_time, end_time) {
  start_time=as.POSIXct(strptime(start_time, '%Y-%m-%d %H:%M:%S', tz="GMT"))
  end_time=as.POSIXct(strptime(end_time, '%Y-%m-%d %H:%M:%S', tz="GMT"))
  trades_to_sum <- trades[trades$time>start_time & trades$time<end_time,]
  trades_to_sum <- trades_to_sum[!is.na(trades_to_sum$time),]
  
  temp = trades_to_sum
  temp$volume[trades_to_sum$type != "bid"] = 0
  trades_to_sum$cumsum_bid=cumsum(temp$volume)
  temp = trades_to_sum
  temp$volume[trades_to_sum$type != "ask"] = 0
  trades_to_sum$cumsum_ask=cumsum(temp$volume)
  trades_to_sum$diff = trades_to_sum$cumsum_bid - trades_to_sum$cumsum_ask
  # if the min is after the time that the max occurs then this is a sell period
  if (trades_to_sum$time[which.min(trades_to_sum$diff)] < trades_to_sum$time[which.max(trades_to_sum$diff)]) {
    trades_soft_sell = trades_to_sum[which.min(trades_to_sum$diff):which.max(trades_to_sum$diff),]
  }
  else {
    trades_soft_sell = trades_to_sum[which.max(trades_to_sum$diff):which.min(trades_to_sum$diff),]
  }
  trades_soft_sell
}

# Returns candlesticks with volume on the ask and volume on the bid 
# It is used in one_sided_trade_period function
#
ohlc_bid_ask <- function(ttime, tprice, tvolume, ttype, fmt)
{
  # create vectors for bid and ask volume
  volume_ask = tvolume*sapply(ttype, function(x) {if(x=="ask") 1 else 0})
  volume_bid = tvolume*sapply(ttype, function(x) {if(x=="bid") 1 else 0})
  ttime.int <- as.POSIXct(strptime(format(ttime,fmt),fmt))
  #ttime.int <- format(ttime,fmt)
  data.frame(Open = tapply(tprice,ttime.int,function(x) {head(x,1)}),
             High = tapply(tprice,ttime.int,max),
             Low = tapply(tprice,ttime.int,min),
             Close = tapply(tprice,ttime.int,function(x) {tail(x,1)}),
             Average = tapply(tprice,ttime.int,mean),
             time = ttime[tapply(1:length(ttime),ttime.int,function(x) {head(x,1)})],
             ask = tapply(volume_ask,ttime.int,function(x) {sum(x)}),
             bid = tapply(volume_bid,ttime.int,function(x) {sum(x)})
  )
}

# Return the largest period when sellers/buyers are responsible for at least percent (ex 90%) of the volume.
#
# Input variables
# (start_time, end_time) is the time interval to search in
# buyers_flag determines whether we are looking for buys or sells
# percent is the minimum percentage of buyers/sellers for the interval to be considered one sided
#
# Sample run
# one_sided_trade_period(trades, start_time, end_time, buyers_flag = FALSE, percent = 0.90)
#
# Implementation
# Breaks down intervals in minutes then uses function mssl
# 
one_sided_trade_period = function (trades, start_time, end_time, percent, buyers = FALSE) {
  start_time=as.POSIXct(strptime(start_time, '%Y-%m-%d %H:%M:%S', tz="GMT"))
  end_time=as.POSIXct(strptime(end_time, '%Y-%m-%d %H:%M:%S', tz="GMT"))
  trades_to_sum <- trades[trades$time>start_time & trades$time<end_time,]
  trades_to_sum <- trades_to_sum[!is.na(trades_to_sum$time),]
  ohlc.1min <- ohlc_bid_ask(trades_to_sum$time, trades_to_sum$price, trades_to_sum$volume, trades_to_sum$type,"%Y%m%d %H%M")
  ohlc.1min$ratio = ohlc.1min$ask/(ohlc.1min$bid+ohlc.1min$ask)
  
  # if the ratio is nan then there was no trading during that minute
  # remove there values
  ohlc.1min<-ohlc.1min[!is.nan(ohlc.1min$ratio),]
  
  if (buyers == FALSE) {
    # use the total volume (bid+asks) as the weights so that ratio*volume=shares sold
    temp = mssl(ohlc.1min$ratio, (ohlc.1min$ask+ohlc.1min$bid), percent)
    fmt="%Y%m%d %H%M"
    # the conversion has default timezone PDT so I must explicitly state GMT 
    # add a minute to the end time
    trades_one_sided_trade = trades_to_sum[trades_to_sum$time>as.POSIXct(strptime(format(ohlc.1min$time[temp[1]],fmt, tz="GMT"),fmt,tz="GMT")) & 
                                         trades_to_sum$time<as.POSIXct(strptime(format(ohlc.1min$time[temp[2]]+60,fmt, tz="GMT"),fmt, tz="GMT")),] 
    
  }
  else {
    temp = mssl(1-ohlc.1min$ratio, (ohlc.1min$ask+ohlc.1min$bid), percent)
    fmt="%Y%m%d %H%M"
    # add a minute to the end time
    trades_one_sided_trade = trades_to_sum[trades_to_sum$time>as.POSIXct(strptime(format(ohlc.1min$time[temp[1]],fmt, tz="GMT"),fmt,tz="GMT")) & 
                                         trades_to_sum$time<as.POSIXct(strptime(format(ohlc.1min$time[temp[2]]+60,fmt, tz="GMT"),fmt, tz="GMT")),] 
  }
  
  trades_one_sided_trade
}

# Return some statistics for a tradesframe of trades
#
trade_stats = function (trades) {
  thecolnames = c("start_time", "end_time", "volume_ask","volume_bid", "volume_diff", "sold_percent", "vwap_ask",  "vwap_bid", "price_change", "price_open", "duration")
  volume_ask = sum(trades$volume*sapply(trades$type, function(x) {if(x=="ask") 1 else 0}))
  volume_bid = sum(trades$volume*sapply(trades$type, function(x) {if(x=="bid") 1 else 0}))
  # for the vwap
  cash_ask = sum(trades$price*trades$volume*sapply(trades$type, function(x) {if(x=="ask") 1 else 0}))
  vwap_ask = cash_ask/volume_ask
  cash_bid = sum(trades$price*trades$volume*sapply(trades$type, function(x) {if(x=="bid") 1 else 0}))
  vwap_bid = cash_bid/volume_bid
  open = head(trades$price,1)
  close = tail(trades$price,1)
  # use as.difftime or the units are messed up!
  time = difftime(tail(trades$time,1),head(trades$time,1), units = "mins")  
  
  # return object must be a data.frame because it contains different trades types
  # add colnames for rbind to work
  tmp<-data.frame(head(trades$time,1), tail(trades$time,1), volume_ask, volume_bid, volume_bid-volume_ask, volume_ask/(volume_ask+volume_bid), vwap_ask, vwap_bid, log(close/open), open, time)
  colnames(tmp)<-thecolnames
  tmp
}

# returns the indices of the largest continuous interval (in terms of the weights of the interval)
# such that the weighted average is larger than percent
mssl = function(trades, w, percent) {
  l = trades - percent
  best = 0
  cur = 0
  curi = 1
  starti = 1
  besti = 1
  for (idx in 0:(length(l)-1)) {
    # maximize the sumproduct
    ave_cur_i = sum(l[curi:(idx+1)]*w[curi:(idx+1)]) #/sum(w[cur:(idx+1)])
    if (ave_cur_i > 0) {
      cur = ave_cur_i
    }
    else {
      cur = 0
      curi = idx+1
    }
    # use this when you maximize the sumproduct
    # if (cur > best) {
    # use this when you are maximizing the legth of the interval
    # with the condition > percent
    if ((idx+1-curi) > (besti-starti)) {
      starti = curi
      besti = idx + 1
      best = cur
    }
  }
  c(starti, besti, sum(l[starti:besti]*w[starti:besti])/sum(w[starti:besti])+percent)
}

# plot market impact for large excess volume
# uses the global variable jumps, generated by one_sided_trade_period_all_wrapper
# 
plot_excess_volume = function(percent = 0.0001) {
  g1 = ggplot()
  # check that there are buys before you plot
  if (length(jumps$price_change[jumps$price_change>0]) > 0) {
    g1 = g1 + geom_point(trades=jumps[jumps$price_change>0,], aes(x=abs(volume_diff), y=as.numeric(duration), size=abs(price_change)), colour="dark green")
  }
  if (length(jumps$price_change[jumps$price_change<0]) > 0) {
    g1 = g1 + geom_point(trades=jumps[jumps$price_change<0,], aes(x=abs(volume_diff), y=as.numeric(duration), size=abs(price_change)), colour="dark red" )
  }
  g1 = g1 + scale_size(range = c(2, 15))+xlab("excess shares") + ylab("duration (min)") + 
    ggtitle(paste("Market impact when sell volume =",1/(1-percent),"x buy volume (and vice versa)")) #+ ylim(0,30)+xlim(0,10000) #+theme(legend.position = "none")
  g1
}

# plot market impact for large excess volume/ 5min
# uses the global variable jumps, generated by one_sided_trade_period_all_wrapper
# 
plot_excess_volume5min = function(percent = 0.0001) {
  g1 = ggplot()
  # check that there are buys before you plot
  if (length(jumps$price_change[jumps$price_change>0]) > 0) {
    g1 = g1 + geom_point(trades=jumps[jumps$price_change>0,], aes(x=abs(shares5min), y=as.numeric(duration), size=abs(price_change5min)), colour="dark green")
  }
  if (length(jumps$price_change[jumps$price_change<0]) > 0) {
    g1 = g1 + geom_point(trades=jumps[jumps$price_change<0,], aes(x=abs(shares5min), y=as.numeric(duration), size=abs(price_change5min)), colour="dark red" )
  }
  g1 = g1 + scale_size(range = c(2, 15))+xlab("excess shares") + ylab("duration (min)") + 
    ggtitle(paste("Market impact when sell volume =",1/(1-percent),"x buy volume (and vice versa)")) #+ ylim(0,30)+xlim(0,10000) #+theme(legend.position = "none")
  g1
}



# plot spot price, trade volume and lag for the interval (start_time, end_time)
#
# Input variables
# (start_time, end_time) is the time interval to search in
# lag contains the lag
#
plot_price_tradevolume_lag = function(trades, lag, start_time, end_time) {
  start_time=as.POSIXct(strptime(start_time, '%Y-%m-%d %H:%M:%S', tz="GMT"))
  end_time=as.POSIXct(strptime(end_time, '%Y-%m-%d %H:%M:%S', tz="GMT"))
  trades_to_plot <- trades[trades$time>start_time & trades$time<end_time,]
  trades_to_plot <- trades_to_plot[!is.na(trades_to_plot$time),]
  lag_to_plot <- lag[lag$Timestamp>start_time & lag$Timestamp<end_time,]
  lag_to_plot <- lag_to_plot[!is.na(lag_to_plot$Timestamp),]
  # nice trick: put the geom_point after geom_line, to have the points be visible!
  p1 = ggplot(trades=trades_to_plot) +geom_line(aes(x=time, y=price)) + geom_point(aes(x=time, y=price, colour=type))+theme(legend.position = "none")
  p2 = ggplot(trades=trades_to_plot) + geom_point(aes(x=time, y=volume, colour=properties))+geom_line(aes(x=time, y=volume))+theme(legend.position = "none")
  p3 = ggplot(trades=lag_to_plot) + geom_point(aes(x=Timestamp, y=lag))+geom_line(aes(x=Timestamp, y=lag))+ylim(0,min(max(lag_to_plot$lag),100))+xlab("time")
  grid.arrange(p1,p2,p3)
}

# plot spot price, trade volume and excess volume for a time interval
# Excess volume is the volume multiplied with the +1 if it is a buy and -1 if it is a sell
#
# Input variables
# (start_time, end_time) is the time interval to search in
#
plot_price_tradevolume_excessvolume = function(trades, start_time, end_time) {
  start_time=as.POSIXct(strptime(start_time, '%Y-%m-%d %H:%M:%S', tz="GMT"))
  end_time=as.POSIXct(strptime(end_time, '%Y-%m-%d %H:%M:%S', tz="GMT"))
  trades_to_plot <- trades[trades$time>start_time & trades$time<end_time,]
  trades_to_plot <- trades_to_plot[!is.na(trades_to_plot$time),]
  # for the plot of cumulative excess volume 
  trades_to_sum <- trades[trades$time>start_time & trades$time<end_time,]
  trades_to_sum <- trades_to_sum[!is.na(trades_to_sum$time),]
  temp = trades_to_sum
  temp$volume[trades_to_sum$type != "bid"] = 0
  trades_to_sum$cumsum_bid=cumsum(temp$volume)
  temp = trades_to_sum
  temp$volume[trades_to_sum$type != "ask"] = 0
  trades_to_sum$cumsum_ask=cumsum(temp$volume)
  trades_to_sum$diff = trades_to_sum$cumsum_bid - trades_to_sum$cumsum_ask
  
  # nice trick: put the geom_point after geom_line, to have the points be visible!
  p1 = ggplot(trades=trades_to_plot) +geom_line(aes(x=time, y=price)) + geom_point(aes(x=time, y=price, colour=type))+theme(legend.position = "none")
  p2 = ggplot(trades=trades_to_plot) + geom_point(aes(x=time, y=volume, colour=properties))+geom_line(aes(x=time, y=volume))+theme(legend.position = "none")
  p3 = ggplot(trades=trades_to_sum) + geom_point(aes(x=time, y=diff))
  grid.arrange(p1,p2,p3)
}

# plot spot price, trade volume with candlesticks
#
# Input variables
# (start_time, end_time) is the time interval to search in
# fmt is the format for the periods of the candlesticks
# 
# Sample run (hourly candlesticks)
# plot_candlestick_excess(trades, start_time, end_time,"%Y%m%d %H") 
#
plot_candlestick = function(trades, start_time, end_time,fmt) {
  start_time=as.POSIXct(strptime(start_time, '%Y-%m-%d %H:%M:%S', tz="GMT"))
  end_time=as.POSIXct(strptime(end_time, '%Y-%m-%d %H:%M:%S', tz="GMT"))
  trades_to_plot <- trades[trades$time>start_time & trades$time<end_time,]
  trades_to_plot <- trades_to_plot[!is.na(trades_to_plot$time),]
  BTC=ohlc_xts(trades_to_plot$time,trades_to_plot$price,trades_to_plot$volume,fmt)
  chartSeries(BTC, bar.type="ohlc", type=c("candlesticks"), TA=c(addVo()))
}

# plot spot price, excess trade volume with candlesticks
#
# Input variables
# (start_time, end_time) is the time interval to search in
# fmt is the format for the periods of the candlesticks
# 
# Sample run (for hourly candlesticks)
# plot_candlestick_excess(trades, start_time, end_time,"%Y%m%d %H") 
#
plot_candlestick_excess = function(trades, start_time, end_time,fmt) {
  start_time=as.POSIXct(strptime(start_time, '%Y-%m-%d %H:%M:%S', tz="GMT"))
  end_time=as.POSIXct(strptime(end_time, '%Y-%m-%d %H:%M:%S', tz="GMT"))
  trades_to_plot <- trades[trades$time>start_time & trades$time<end_time,]
  trades_to_plot <- trades_to_plot[!is.na(trades_to_plot$time),]
  # time series for buyers and sellers separately
  BTCask=ohlc_xts(trades_to_plot$time[trades_to_plot$type=="ask"],trades_to_plot$price[trades_to_plot$type=="ask"],trades_to_plot$volume[trades_to_plot$type=="ask"],fmt)
  BTCbid=ohlc_xts(trades_to_plot$time[trades_to_plot$type=="bid"],trades_to_plot$price[trades_to_plot$type=="bid"],trades_to_plot$volume[trades_to_plot$type=="bid"],fmt)
  BTCfull=ohlc_xts(trades_to_plot$time,trades_to_plot$price,trades_to_plot$volume,fmt)  
  BTC=merge(BTCfull, BTCask, BTCbid)
  
  # adjust the volume to be the excess volume. 
  BTC$Volume=-ifelse(is.na(BTC$Volume.1),0,BTC$Volume.1)+ifelse(is.na(BTC$Volume.2),0,BTC$Volume.2)
  chartSeries(BTC, bar.type="ohlc", type=c("candlesticks"), TA=c(addVo()))
}



