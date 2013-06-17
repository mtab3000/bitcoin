#!/usr/bin/env python

""" Calls the MtGox API to get historical trades and saves them in an text file. Checks that trades are 
	don't already exist in the ouput file. 

	Run with:
	python get_mtgox_data.py -s "2013-04-27 08:00:00.00" -e "2013-06-27 13:30:00.00" 

"""

import datetime, urllib2, calendar, json
import pandas as pd
import argparse
import sys

input_dateformat = "%Y-%m-%d %H:%M:%S.%f"
input_dateformat_nomsec = "%Y-%m-%d %H:%M:%S" # use this when the input has no milliseconds
infile_name = "/db/private/bitcoin/historical_data/mtgox_trades.csv"
outfile_name = "/db/private/bitcoin/historical_data/mtgox_orders.csv"


def get_humantime(unixtime):
	""" returns a string of the unixtime in human readable format
		from a unixtime. Includes microseconds
	"""
	return str(pd.datetime.utcfromtimestamp(int(unixtime)/1000000.0))

def get_unixtime(humantime, dateformat):
	""" returns a unixtime from a time in dateformat
	"""
	temp = datetime.datetime.strptime(humantime, dateformat)
	# add microseconds which are dropped by timetuple
	return int(calendar.timegm(temp.timetuple()))*1000000.0+temp.microsecond


class mtgox_trade():
	def __init__(self, tradetuple):
		self.timestamp, self.price, self.amount, self.ordertype, self.trade_type = tradetuple

	# constructor with input the json format of MtGox
	#format: {"date":1365881612,"price":"105","amount":"0.15","price_int":"10500000","amount_int":"15000000","tid":"1365881612029984",
	#			"price_currency":"USD","item":"BTC","trade_type":"bid","primary":"Y","properties":"market"} 
	@classmethod
	def from_mtgox_tradedict(cls, tradedict):
		# tid is the tradeid, which is the most accurate version of the time of a trade.
		# will not use the "date" field at all
		tradetuple = []
		tradetuple.append(int(tradedict['tid']))
		tradetuple.append(float(tradedict['price']))
		tradetuple.append(float(tradedict['amount']))
		# properties field sometimes contains "mixed_currency" which I dont know what it means
		if "limit" in str(tradedict['properties']):
			tradetuple.append("limit")
		elif "market" in str(tradedict['properties']):
			tradetuple.append("market")
		else:
			tradetuple.append("unkown")
		tradetuple.append(str(tradedict["trade_type"]))
		return cls(tradetuple)

	# constructor from the string that was output to the file
	# call with instance_name = mtgox_trade.from_tradestring("...")
	@classmethod
	def from_tradestring(cls, tradestring):
		tradelist = []
		try: 
			tradelist.append(get_unixtime(tradestring.split(",")[0], input_dateformat))
		except ValueError:
			tradelist.append(get_unixtime(tradestring.split(",")[0], input_dateformat_nomsec))
		tradelist.append( float(tradestring.split(",")[1]) )		
		tradelist.append( float(tradestring.split(",")[2]) )		
		tradelist.extend(tradestring.split(",")[3:5])
		return cls(tradelist)


	def trade_to_string(self):
		return get_humantime(self.timestamp)+","+str(self.price)+","+str(self.amount)+","+self.ordertype+","+self.trade_type


def fetch_data(start):
	# cast start into int before casting into string so that it is not in scientific notation
	url = "https://data.mtgox.com/api/1/BTCUSD/trades?raw&since=" + str(int(start))
	req = urllib2.Request(url)
	res = urllib2.urlopen(req)
	data = res.read()
	res.close()
	return data

def main():

	try:
		with open(infile_name, "r") as in_file:
			trades = [mtgox_trade.from_tradestring(a) for a in in_file.readlines()]
			N = len(trades)

			with open(outfile_name, "w") as out_file:
				idx = 0
				while idx <= N-2:
					# if the order is a single trade/print
					if trades[idx+1].timestamp - trades[idx].timestamp > 500000 or \
							trades[idx+1].ordertype != trades[idx].ordertype or \
							trades[idx+1].trade_type != trades[idx].trade_type:
						out_file.write(trades[idx].trade_to_string()+"\n")
						idx += 1
					# if the order has multiple trades/prints
					else:
						timestamp = trades[idx].timestamp
						amount = trades[idx].amount
						price = trades[idx].price
						order_type = trades[idx].ordertype
						trade_type = trades[idx].trade_type
						while True:							
							price = (price * amount + trades[idx+1].price * trades[idx+1].amount)/(amount + trades[idx+1].amount)							
							amount += trades[idx+1].amount
							idx += 1
							# the exit condition is at the end
							if idx >= N-1:
								break
							if trades[idx+1].timestamp - trades[idx].timestamp > 500000 or \
								trades[idx+1].ordertype != trades[idx].ordertype or \
								trades[idx+1].trade_type != trades[idx].trade_type:
								idx += 1
								break
						out_file.write(mtgox_trade([timestamp, price, amount, order_type, trade_type]).trade_to_string()+"\n")						
	except IOError:
		print "Input file not found. Exiting."

if __name__ == "__main__":
	main()
