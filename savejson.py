import requests
import json
import sys

DATAPATH = "C:\\Users\\My Files\\Desktop\\Data Science Resources\\Basketball Prediction Models"

#URLS and parameter sets for requests
#Play by Play from 1996-97
pbp_url = "http://stats.nba.com/stats/playbyplay"
pbp_params = {"GameID":0, "RangeType":0, "StartPeriod":0, "EndPeriod":0, "StartRange":0, "EndRange":0, "playbyplay":"undefined"}
#Shot Chart for 96-97 onward
sc_url = "http://stats.nba.com/stats/shotchartdetail"
sc_params = {"Season":"", "SeasonType":"Regular Season", "LeagueID":"00", "TeamID":0, "PlayerID":0, "GameID":0, "Outcome":"", "Location":"", "Month":0, "SeasonSegment":"", "DateFrom":"", "DateTo":"", "OpponentTeamID":0, "VsConference":"", "VsDivision":"", "Position":"", "RookieYear":"", "GameSegment":"", "Period":0, "LastNGames":0, "ContextFilter":"", "ContextMeasure":"FG_PCT", "zone-mode":"basic", "viewShots":"true"}
#Box score, works for historic games back to 1949
bs_url = "http://stats.nba.com/stats/boxscore"
bs_params = {"GameID":0, "RangeType":0, "StartPeriod":0, "EndPeriod":0, "StartRange":0, "EndRange":0, "playbyplay":"undefined"}
#SportVu data, might be large so comment out if you want to save time
do_sportvu = False
sv_url = "http://stats.nba.com/stats/locations_getmoments/"
sv_params = {"eventid":0, "gameid":0}

def write_game_json(gameid):
	#boxscore
	print ("Game %s, box score" % gameid)
	f = open("%s/json/bs_%s.json" % (DATAPATH, gameid), "w")
	bs_params["GameID"] = gameid 
	bs = requests.get(bs_url, params=bs_params).json()["resultSets"]
	json.dump(bs, f)
	f.close()
	
	#play by play

	print ("Game %s, play by play" % gameid)
	f = open("%s/json/pbp_%s.json" % (DATAPATH, gameid), "w")
	pbp_params["GameID"] = gameid 
	pbp = requests.get(pbp_url, params=pbp_params).json()["resultSets"][0]
	json.dump(pbp, f)
	f.close()

	#shot chart 

	print ("Game %s, shot chart" % gameid)
	f = open("%s/json/shots_%s.json" % (DATAPATH, gameid), "w")
	sc_params["GameID"] = gameid 
	sc_params["SeasonType"] = "Playoffs"
	sc = requests.get(sc_url, params=sc_params).json()["resultSets"][0]
	json.dump(sc, f)
	f.close()

	#sport vu 

	if not do_sportvu:
		return

	print ("Game %s, SportVu" % gameid)
	sv_params["gameid"] = gameid 

	eventids = []
	for p in pbp["rowSet"][1:]:
		eventids.append(p[1])
	print ("Number of events: " + str(eventids[-1]))

	#loop over events, get sport vu data for each one, keep track of errors
	errlist = []
	for eventid in eventids:
		print (str(eventid))
		try:
			sv_params["eventid"] = eventid
			sv = requests.get(sv_url, params=sv_params).json()
			f = open("%s/json/sv_%s_%s.json" % (DATAPATH, gameid, eventid), "w")
			json.dump(sv, f)
			f.close()
		except:
			errlist.append(str(eventid))
			print ("Error on " + str(eventid))

def write_gamelist_json(gamelist):
	f = open(gamelist, "r")
	f.readline()
	for r in f.readlines():
		gameid = r.split(",")[0]
		write_game_json(gameid)

def main():
	write_gamelist_json(sys.argv[1])

if __name__ == "__main__":
	main()