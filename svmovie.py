import re
import json
import pandas as pd 
import numpy as np 
import matplotlib.pyplot as plt 
import matplotlib.image as mpimg 
import matplotlib.animation as animation

team_list = ['ATL', 'BOS', 'BKN', 'CHA', 'CHI', 'CLE', 'DAL', 'DEN', 'DET', 'GSW', 'HOU', 'IND', 'LAC', 'LAL', 'MEM', 'MIA', 'MIL', 'MIN', 'NOP', 'NYK', 'OKC', 'ORL', 'PHI', 'PHO', 'POR', 'SAC', 'SAS', 'TOR', 'UTA', 'WAS']
USERHOME = "/home/gmf"
NBAHOME = USERHOME + "/Code/git/nba"
DATAPATH = USERHOME + "/unsynced/nba/data"
gameid = "0021400001"

def get_boxscore(gameid):
	J = json.load( open( "%s/json/bs_%s.json" % (DATAPATH, gameid), "r"))
	return [pd.DataFrame(data=j["rowSet"], columns=j["headers"]) for j in J]

def get_pbp(gameid):
	J = json.load( open( "%s/json/pbp_%s.json" % (DATAPATH, gameid), "r"))
	return pd.DataFrame(data=J["rowSet"], columns=J["headers"])

def get_shots(gameid):
	J = json.load( open( "%s/json/shots_%s.json" % (DATAPATH, gameid), "r"))
	return pd.DataFrame(data=J["rowSet"], columns=J["headers"])

pbp = get_pbp(gameid)
boxscore = get_boxscore(gameid)
shots = get_shots(gameid)

def get_sportvu(gameid, eventnum):
	if eventnum==0:
		print ("Warning: Event Num = 0 passed. Switching to event number 1.")
		eventnum += 1

	J = json.load( open( "%s/json/sv_%s_%s.json" % (DATAPATH, gameid, str(eventnum).zfill(4)), "r"))
	player_fields = J["home"]["players"][0].keys()
	home_players = pd.DataFrame(data=[J["home"]["players"][i].values() for i in range(len(J["home"]["players"]))], columns=player_fields)
	away_players = pd.DataFrame(data=[J["visitor"]["players"][i].values() for i in range(len(J["visitor"]["players"]))], columns=player_fields)
	players = pd.merge(home_players, away_players, how="outer")
	jerseydict = dict(zip(players.playerid.values, players.jersey.values))
	playerteamdict = dict(zip(players.playerid.values, [j["home"]["teamid"] for n in range(len(home_players))] + [J["visitor"]["teamid"] for n in range(len(away_players))]))
	moments = J["moments"]

	#initialize arrays
	num_moments = len(moments)
	num_players = 10
	fields = ["playerx", "playery", "playerid", "playernum", "playername", "playerpos", "teamid", "ballx", "bally",
	"ballz", "shotclock_remain", "sec_remain", "momentid", "period"]
	num_fields = len(fields)
	D = pd.DataFrame(data=[[None for i in range(num_fields)] for j in range(num_moments)], columns=fields)
	player_fields = ["playerid", "playernum", "playername", "playerpos", "playerx", "playery", "teamid"]
	for f in player_fields: D[f] = [np.zeros(num_players, dtype=int) for n in range(num_moments)]

	for i in range(num_moments):
		D.period.iloc[i], D.momentid.iloc[i], D.sec_remain.iloc[i], D.shotclock_remain.iloc[i] = moments[i][0:4]
		D.ballx.iloc[i], D.bally.iloc[i], D.ballz.iloc[i] = moments[i][-1][0][2:5]
		#Initializing lists for player info
		#filling out player info
		for j in range(num_players):
			D.playerid[i][j] = moments[i][-1][j+1][1]
			D.playerx[i][j], D.playery[i][j] = moments[i][-1][j+1][2:4]
			D.playernum[i][j] = jerseydict[D.playerid[i][j]]
			D.teamid[i][j] = playerteamdict[D.playerid[i][j]]
		return D 

def get_reb_eventnum(gameid):
	pbp = get_pbp(gameid)
	homereb = []
	awayreb = []
	for i, p in pbp.iterrows():
		if p.HOMEDESCRIPTION and re.search("REBOUND", p.HOMEDESCRIPTION):
			homereb.append(p)
		if p.VISITORDESCRIPTION and re.search("REBOUND", p.VISITORDESCRIPTION):
			awayreb.append(p)
	i0 = [a.EVENTNUM for a in homereb]
	i1 = [a.EVENTNUM for a in awayreb]
	return i0, i1

def get_team_games(team, seasonid="00214"):
	games = open("%s/csv/games_00-14.csv" % DATAPATH, "r")
	G = games.readlines()
	headers = G[0].strip().split(",")
	G.remove(G[0])
	G = [g.strip().split(",") for g in G]
	D = pd.DataFrame(data=G, columns = headers)
	D = D.iloc[np.where(D.seasonid==seasonid)]
	gamesids = np.concatenate((np.flatnonzero(D.home==team), np.flatnonzero(D.away==team)))
	return D.gameid_num.iloc[gameids].values

#draw court, simple version
def draw_court(axis):
	#fig = plt.figure(figsize=(15, 7.5))
	img = mpimg.imread(NBAHOME + "/image/nba_court_T.png")
	plt.imshow(img, extent=axis, zorder=0)

def init():
	#draw court and zoom out slightly to give light buffer
	draw_court([xmin, xmax, ymin, ymax])
	for i in range(3): info_text[i].set_text("")
	for i in range(10):
		player_text[i].set_text("")
		ax.add_patch(player_circ[i])
	ax.add_patch(ball_circ)
	ax.axis("off")
	dx = 5
	plt.xlim([xmin-dx, xmax+dx])
	plt.ylim([ymin-dx, ymax+dx])
	plt.title(play_description)
	return tuple(info_text) + tuple(player_text) + tuple(player_circ) + (ball_circ,)

def animate(n):
	#1. draw players by team, with jersey numbers
	for i in range(10):
		if i<5:
			col="b"
		else:
			col="r"
		player_circ[i].center = (sv.iloc[n].playerx[i], sv.iloc[n].playery[i])
		player_text[i].set_text(str(sv.iloc[n].playernum[i]))
		player_text[i].set_x(sv.iloc[n].playerx[i])
		player_text[i].set_y(sv.iloc[n].playery[i])
	#2. draw ball
	ball_circ.center = (sv.iloc[n].ballx, sv.iloc[n].bally)
	#fluctuate ball radius to indicate z position
	ball_circ.radius = 1 + sv.iloc[n].ballz/17*2

	#print game clock info
	info_text[0].set_text(str(sv.iloc[n].period))
	info_text[1].set_text(str(sv.iloc[n].sec_remain))
	info_text[2].set_text(str(sv.iloc[n].shotclock_remain))

	plt.show()
	return tuple(info_text) + tuple(player_text) + tuple(player_circ) + (ball_circ,)

eventnum = 3
filename = "test"

#get sportvu data and downsample
sv = get_sportvu(gameid, eventnum)
dn = 2
sv = sv.iloc[range(0, len(sv), dn)]

plt.close("all")
fig = plt.figure(figsize=(15, 7.5))
ax = plt.gca()

#court dimensions
xmin = 0
xmax = 100
ymin = 0
ymax = 50

#play by play data for given event num 
playi = pbp.iloc[np.flatnonzero(pbp.EVENTNUM==eventnum)]
if playi.HOMEDESCRIPTION.values[0]:
	play_description = str(playi.HOMEDESCRIPTION.values[0])
else:
	play_description = str(playi.VISITORDESCRIPTION.values[0])

#sportsvu start/stop times and time axis for given event number
#tstart = sv.sec_remain.iloc[0]
#tend = sv.sec_remain.iloc[-1]
#nsec = tend - tstart
#time = np.linspace(0, nsec, num=len(sv))
#nbins = len(time)

#get shot information for given event
#shots = get_shots(gameid)
#shots_missed = shots.iloc[ np.flatnonzero( 1 - shots.SHOT_MADE_FLAG ) ]
#for any pbp rebounce event, get nearest preceding miss 
#shot = shots_missed.iloc[ np.flatnonzero(shots_missed.GAME_EVENT_ID == i)]

#Animated elements
info_text = range(3)
info_text[0] = ax.text(0, -5, "")
info_text[1] = ax.text(5, -5, "")
info_text[2] = ax.text(20, -5, "")
player_text = range(10)
player_circ = range(10)
R = 2.2
for i in range(10):
	player_text[i] = ax.text(0,0,"",color="w",ha="center",va="center")
	if i < 5:
		col="b"
	else:
		col="r"
	player_circ[i] = plt.Circle((0,0), R, color=col)
ball_circ = plt.Circle((0,0), R, color=[1,0.4,0])

#play animation
ani = animation.FuncAnimation(fig, animate, frames=len(sv), init_func=init, blit=True, interval=4, repeat=False)

ani.save("/home/gmf/%s.mp4" % filename, fps=25, extra_args=["-vcodec", "libx264"])

#plot game plays in sequence
plt.close("all")
fig = plt.figure(figsize=(15, 7.5))
ax = plt.gca()
ax.axis("off")

for eventnum in range(50, 100):
	#get sportvu data and downsample
	sv = get_sportvu(gameid, eventnum)
	dn = 2
	sv = sv.iloc[range(0, len(sv), dn)]

	#court dimensions
	xmin = 0
	xmax = 100
	ymin = 0
	ymax = 50

	#Play by play data for given event num
	#TO do dont get index get closest to current time
	playi = pbp.iloc[np.flatnonzero(pbp.EVENTNUM==eventnum)]
	#treb = playi.PCTIMESTRING.values[0]
	if playi.HOMEDESCRIPTION.values[0]:
		play_description = str(playi.HOMEDESCRIPTION.values[0])
	else:
		play_description = str(playi.VISITORDESCRIPTION.values[0])

	#Spotvu start/stop times and time axis for given event number
	#tstart = sv.sec_remain.iloc[0]
	#tend = sv.sec_remain.iloc[-1]
	#nsec = tend = tstart
	#time = np.linspace(0, nsec, num=len(sv))
	#nbins = len(time)

	#get shot info for given event
	#shots = get_shots(gameid)
	#shots_missed = shots.iloc[ np.flatnonzero( 1 - shots.SHOT_MADE_FLAG)] 
	#for any pbp rebound event, get nearest preceding miss
	#

	info_text = range(3)
	info_text[0] = ax.text(0, -5, "")
	info_text[1] = ax.text(5, -5, "")
	info_text[2] = ax.text(20, -5, "")
	player_text = range(10)
	player_circ = range(10)
	R = 2.2
	for i in range(10):
		player_text[i] = ax.text(0, 0, "", color="w", ha="center", va="center")
		if i < 5:
			col="b"
		else:
			col = "r"
		player_circ[i] = plt.Circle(0,0), R, color=col)
	ball_circ = plt.Circle((0,0), R, color=[1,0.4, 0])

	#Play animation
	ani = animation.FuncAnimation(fig, animate, frames=len(sv), init_func=init, blit=True, interval=5, repeat=False)
	plt.pause(2)
	plt.clf()