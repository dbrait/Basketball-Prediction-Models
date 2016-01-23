import urllib2
import re
import datetime
from urlparse import urlparse
from bs4 import BeautifulSoup as bs

def daterange(start, end):
	"""Generator for days between two specific days."""
	for n in range((end - start).days):
		yield start + datetime.timedelta(n)

def _format_scoreboard_url(day, league="nba"):
	"""Format ESPN scoreboard link to scrape individual box scores from"""
	league = league.lower()
	link = [league + "/scoreboard?date="]
	if isinstance(day, datetime.date):
		link.append(day.strftime("%Y%m%d"))
	else:
		link.append(day)
	if league == "ncb":
		link.append("&confId=50")
	scoreboard_link = "".join(["http://scores.espn.go.com/", "".join(link)])
	return scoreboard_link

def scrape_links(espn_scoreboard):
	"""Scrape Espn's scoreboard for play by play links"""
	url = urllib2.urlopen(espn_scoreboard)
	print (url.geturl())
	soup = bs(url.read(), ["fast", "lxml"])
	div = soup.find("div", {"class": "span-4"})
	links = (a["href"] for a in div.findAll("a") if re.match("Play.*", a.contents[0]))
	queries = [urlparse(link).query for link in links]
	return queries

def adjust_game(plays, league="nba"):
	"""
	Take plays from parse_plays and league which it is parsing
	returns a list of plays in dict format, which is better for lookups

	dict contains following info: quarter, quarter_time,
	overall_time, home_score, away_score, home_play, away_play, and
	official_play
	"""
	game = []
	quarter = 1
	end_of_quarter = False
	for play in plays:
		new_play = _play_as_dict(play)
		time = play[0]
		time_dict, quarter, end_of_quarter = _adjust_time(time,
			quarter, end_of_quarter, league)
		new_play.update(time_dict)
		try:
			scores = play[2]
		except IndexError:
			if len(game) > 0:
				last_play = game[-1]
				new_play["away_score"] = last_play["away_score"]
				new_play["home_score"] = last_play["home_score"]
			else:
				new_play["away_score"] = 0
				new_play["home_score"] = 0
		else:
			away_score, home_score = scores.split("-")
			new_play["away_score"] = int(away_score)
			new_play["home_score"] = int(home_score)
		game.append(new_play)
	return game

def _adjust_time(time, quarter, end_of_quarter, league):
	"""
	takes time logic out of adjust game, returns a dict, quarter and end of quarter
	"""
	new_time = re.split(":", time)
	minutes = int(new_time[0])
	seconds = int(new_time[1])
	if minutes is 0 and not end_of_quarter:
		end_of_quarter = True
	elif end_of_quarter and minutes > 1:
		quarter += 1
		end_of_quarter = False
	overall_time = _calc_overall_time(seconds, minutes, quarter, league)
	time_dict = {}
	time_dict["overall_time"] = overall_time
	time_dict["quarter_time"] = time
	time_dict["quarter"] = quarter
	return time_dict, quarter, end_of_quarter

def _league_time(league):
	"""
	Return league specific game info -- number of quarters, reg time limit, reg quarter length
	"""
	if league is "nba":
		num_quarters = 4
		regulation_time = 48
		regular_quarter = 12
	else:
		num_quarters = 2
		regulation_time = 40
		regular_quarter = 20
	return num_quarters, regulation_time, regular_quarter

	def _calc_overall_time(seconds, minutes, quarter, league):
		"""
		Calculate overall time elapse for given game. 
		"""
		num_quarters, regulation_time, regular_quarter = _league_time(league)
		if quarter > num_quarters:
			#overtime 
			quarter_length = 5
			overtimes = quarter - num_quarters
			previous_time = datetime.timedelta(minutes=(regulation_time + 5 * (overtimes - 1)))
		else:
			quarter_length = regular_quarter
			previous_time = datetime.timedelta(minutes=(quarter_length * (quarter - 1)))
		mins = datetime.timedelta(minutes=quarter_length) -\
				datetime.timedelta(minutes=minutes, seconds=seconds)
		overall_time = str(mins + previous_time)
		return overall_time

def _play_as_dict(play):
	"""
	Give it a play in list/tuple format, get back dict containing official_play, home_play, away_play data 
	"""
	new_play = {}
	if len(play) is 2:
		new_play["official_play"] = play[1]
		new_play["home_play"] = None
		new_play["away_play"] = None
	else:
		new_play["official_play"] = None
		away_play = play[1]
		home_play = play[3]
		if len(away_play) < 10:
			new_play["away_play"] = None
			new_play["home_play"] = home_play
		elif len(home_play) < 10:
			new_play["away_play"] = away_play
			new_play["home_play"] = None
	return new_play

def parse_plays(game_id, league="nba"):
	"""Parse a game's play by play on espn"""
	league = league.lower()
	espn = "http://scores.espn.go.com/" + league + "/playbyplay?" +\
			game_id + "&period=0"
	url = urllib2.urlopen(espn)
	print (url.geturl())

	soup = bs(url.read(), ["fast", "lxml"])
	table = soup.find("table", {"class": "mod-data"})
	thead = [thead.extract() for thead in table.findAll("thead")]
	rows = (list(tr(text=True)) for tr in table.findAll("tr"))
	game = adjust_game(rows, league)
	teams = thead[0].findChildren("th", {"width": "40%"})
	away_team, home_team = [team.string.title() for team in teams]
	print (len(game), away_team, home_team)
	return away_team, home_team, game

def get_games(day, league="nba", iterable=False):
	"""
	Get games, play-by-play data from espn for a date
	"""
	espn_scoreboard = _format_scoreboard_url(day, league=league)
	all_games = scrape_links(espn_scoreboard)
	if not iterable:
		games = [parse_plays(game, league=league) for game in all_games]
	else:
		games = (parse_plays(game, league=league) for game in all_games)
	return games

def main():
	yesterday = datetime.date.today() - datetime.timedelta(1)
	for game in get_games(yesterday, iterable=True):
		print (game)

if __name__ == "__main__":
	import time
	start = time.time()
	main()
	print (time.time() - start, "seconds")