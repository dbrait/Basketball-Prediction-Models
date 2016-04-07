packages <-
  c("nbastatR", "ggplot2", "rvest", "ggrepel")
lapply(packages, library, character.only=T)

team_data <-
  get_nba_team_stat_table()

number_ticks <- function(n) {function(limits) pretty(limits, n)}

team_defense <- get_all_team_synergy_stats(include_defense=T,
                                           include_offense=F)

team_offense <- get_all_team_synergy_stats(include_offense=T,
                                           include_defense=F)

team_defense %<>% left_join(team_data %>% dplyr::select(team, wins, losses))

team_offense %<>% left_join(team_data %>% dplyr::select(team, wins, losses))

team_defense %>%
  ggplot(aes(x = pct.efg.transition, y = losses, label=slug.team)) +
  geom_point(color="red") +
  geom_text_repel(aes(label=slug.team)) +
  geom_rug(sides="l") +
  geom_smooth() +
  theme_classic(base_size=12) +
  scale_x_continuous(breaks=number_ticks(12)) +
  scale_y_continuous(breaks=number_ticks(12)) +
  geom_hline(yintercept = teamDefense$losses %>% mean(), color="dark green") +
  geom_vline(xintercept = teamDefense$pct.efg.transition %>% mean(), color="red") +
  ylab(label="Total Losses") +
  xlab(label="Opponent EFG% on Transition") +
  ggtitle("Transition D")


team_defense %>%
  ggplot(aes(x = pct.play_type.transition, y= losses, label=slug.team)) +
  geom_point(color="red") +
  geom_text_repel(aes(label=slug.team)) +
  geom_rug(sides="l") +
  geom_smooth() +
  theme_classic(base_size=12) +
  scale_x_continuous(breaks=number_ticks(12)) +
  scale_y_continuous(breaks=number_ticks(12)) +
  geom_hline(yintercept = teamDefense$losses %>% mean(), color="dark green") +
  geom_vline(xintercept = teamDefense$pct.play_type.transition %>% mean(), color="red") +
  ylab(label="Total Losses") +
  xlab(label="Perc of Opportunities in Transition") +
  ggtitle("Perc of Transition")

team_defense %>%
  ggplot(aes(x = pct.efg.prrollman, y = losses, label=slug.team)) +
  geom_point(color="red") +
  geom_text_repel(aes(label=slug.team)) +
  geom_rug(sides="l") +
  geom_smooth() +
  theme_classic(base_size=12) +
  scale_x_continuous(breaks=number_ticks(12)) +
  scale_y_continuous(breaks=number_ticks(12)) +
  ylab(label="Total Losses") +
  xlab(label="EFG of Rollman") +
  ggtitle("Defense against Rollman")

team_defense %>%
  ggplot(aes(x=pct.efg.prballhandler, y=losses, label=slug.team)) +
  geom_point(color="red") +
  geom_text_repel(aes(label=slug.team)) +
  geom_rug(sides="l") +
  geom_smooth() +
  theme_classic(base_size=12) +
  scale_x_continuous(breaks=number_ticks(12)) +
  scale_y_continuous(breaks=number_ticks(12)) +
  ylab(label="Total Losses") +
  xlab(label="EFG of Ball Handler") +
  ggtitle("Defense against Ball Handler")

team_defense %>%
  ggplot(aes(x=pct.efg.cut, y=losses, label=slug.team)) +
  geom_point(color="red") +
  geom_text_repel(aes(label=slug.team)) +
  geom_rug(sides="l") +
  geom_smooth() +
  theme_classic(base_size=12) +
  scale_x_continuous(breaks=number_ticks(12)) +
  scale_y_continuous(breaks=number_ticks(12)) +
  ylab(label="Total Losses") +
  xlab(label="EFG of Cut Man") +
  ggtitle("Defense against Cuts")

team_defense %>%
  ggplot(aes(x=pct.play_type.cut, y=losses, label=slug.team)) +
  geom_point(color="red") +
  geom_text_repel(aes(label=slug.team)) +
  geom_rug(sides="l") +
  geom_smooth() +
  theme_classic(base_size=12) +
  scale_x_continuous(breaks=number_ticks(12)) +
  scale_y_continuous(breaks=number_ticks(12)) +
  ylab(label="Total Losses") +
  xlab(label="Perc play type cuts") +
  ggtitle("Defense against cuts")

team_defense %>%
  ggplot(aes(x=pct.efg.offscreen, y=losses, label=slug.team)) +
  geom_point(color="red") +
  geom_text_repel(aes(label=slug.team)) +
  geom_rug(sides="l") +
  geom_smooth() +
  theme_classic(base_size=12) +
  scale_x_continuous(breaks=number_ticks(12)) +
  scale_y_continuous(breaks=number_ticks(12)) +
  ylab(label="Total Losses") +
  xlab(label="EFG against ball screens") +
  ggtitle("Defense against off screens")

team_defense %>%
  ggplot(aes(x=pct.play_type.offscreen, y=losses, label=slug.team)) +
  geom_point(color="red") +
  geom_text_repel(aes(label=slug.team)) +
  geom_rug(sides="l") +
  geom_smooth() +
  theme_classic(base_size=12) +
  scale_x_continuous(breaks=number_ticks(12)) +
  scale_y_continuous(breaks=number_ticks(12)) +
  ylab(label="Total Losses") +
  xlab(label="Perc of play ball screens") +
  ggtitle("Percent of shots that are off screen")

team_defense %>%
  ggplot(aes(x=pct.play_type.isolation, y=losses, label=slug.team)) +
  geom_point(color="red") +
  geom_text_repel(aes(label=slug.team)) +
  geom_rug(sides="l") +
  geom_smooth() +
  theme_classic(base_size=12) +
  scale_x_continuous(breaks=number_ticks(12)) +
  scale_y_continuous(breaks=number_ticks(12)) +
  ylab(label="Total Losses") +
  xlab(label="Perc play type in iso") +
  ggtitle("Perc in Iso")

team_defense %>%
  ggplot(aes(x=pct.efg.isolation, y=losses, label=slug.team)) +
  geom_point(color="red") +
  geom_text_repel(aes(label=slug.team)) +
  geom_rug(sides="l") +
  geom_smooth() +
  theme_classic(base_size=12) +
  scale_x_continuous(breaks=number_ticks(12)) +
  scale_y_continuous(breaks=number_ticks(12)) +
  ylab(label="Total Losses") +
  xlab(label="EFG Defense against iso") +
  ggtitle("EFG against Iso")

team_defense %>%
  ggplot(aes(x=pct.efg.postup, y=losses, label=slug.team)) +
  geom_point(color="red") +
  geom_text_repel(aes(label=slug.team)) +
  geom_rug(sides="l") +
  geom_smooth() +
  theme_classic(base_size=12) +
  scale_x_continuous(breaks=number_ticks(12)) +
  scale_y_continuous(breaks=number_ticks(12)) +
  ylab(label="Total Losses") +
  xlab(label="EFG post defense") +
  ggtitle("EFG against postups")

team_defense %>%
  ggplot(aes(x=pct.play_type.postup, y=losses, label=slug.team)) +
  geom_point(color="red") +
  geom_text_repel(aes(label=slug.team)) +
  geom_rug(sides="l") +
  geom_smooth() +
  theme_classic(base_size=12) +
  scale_x_continuous(breaks=number_ticks(12)) +
  scale_y_continuous(breaks=number_ticks(12)) +
  ylab(label="Total Losses") +
  xlab(label="Perc of Plays Postups") +
  ggtitle("Percent of d that is against postups")

team_offense %>%
  ggplot(aes(x=pct.play_type.isolation, y=losses, label=slug.team)) +
  geom_point(color="red") +
  geom_text_repel(aes(label=slug.team)) +
  geom_rug(sides="l") +
  geom_smooth() +
  theme_classic(base_size=12) +
  scale_x_continuous(breaks=number_ticks(12)) +
  scale_y_continuous(breaks=number_ticks(12)) +
  ylab(label="Total Losses") +
  xlab(label="Perc of Plays in Iso") +
  ggtitle("Perc in Iso")

team_offense %>%
  ggplot(aes(x=pct.efg.isolation, y=losses, label=slug.team)) +
  geom_point(color="red") +
  geom_text_repel(aes(label=slug.team)) +
  geom_rug(sides="l") +
  geom_smooth() +
  theme_classic(base_size=12) +
  scale_x_continuous(breaks=number_ticks(12)) +
  scale_y_continuous(breaks=number_ticks(12)) +
  ylab(label="Total Losses") +
  xlab(label="EFG in Iso") +
  ggtitle("Best offensive teams in ISO")

team_offense %>%
  ggplot(aes(x=pct.efg.transition, y=losses, label=slug.team)) +
  geom_point(color="red") +
  geom_text_repel(aes(label=slug.team)) +
  geom_rug(sides="l") +
  geom_smooth() +
  theme_classic(base_size=12) +
  scale_x_continuous(breaks=number_ticks(12)) +
  scale_y_continuous(breaks=number_ticks(12)) +
  ylab(label="Total Losses") +
  xlab(label="EFG in Transition") +
  ggtitle("EFG in Transition relationship with losses")

team_offense %>%
  ggplot(aes(x=pct.efg.prrollman, y=losses, label=slug.team)) +
  geom_point(color="red") +
  geom_text_repel(aes(label=slug.team)) +
  geom_rug(sides="l") +
  geom_smooth() +
  theme_classic(base_size=12) +
  scale_x_continuous(breaks=number_ticks(12)) +
  scale_y_continuous(breaks=number_ticks(12)) +
  ylab(label="Total Losses") +
  xlab(label="EFG of Rollman") +
  ggtitle("EFG of Rollman vs losses")

team_offense %>%
  ggplot(aes(x=pct.efg.prballhandler, y=losses, label=slug.team)) +
  geom_point(color="red") +
  geom_text_repel(aes(label=slug.team)) +
  geom_rug(sides="l") +
  geom_smooth() +
  theme_classic(base_size=12) +
  scale_x_continuous(breaks=number_ticks(12)) +
  scale_y_continuous(breaks=number_ticks(12)) +
  ylab(label="Total Losses") +
  xlab(label="EFG of Ball Handler") +
  ggtitle("EFG of Ball Handler vs losses")

team_offense %>%
  ggplot(aes(x=pct.play_type.postup, y=losses, label=slug.team)) +
  geom_point(color="red") +
  geom_text_repel(aes(label=slug.team)) +
  geom_rug(sides="l") +
  geom_smooth() +
  theme_classic(base_size=12) +
  scale_x_continuous(breaks=number_ticks(12)) +
  scale_y_continuous(breaks=number_ticks(12)) +
  ylab(label="Total Losses") +
  xlab(label="Pct of Offense that is a postup") +
  ggtitle("Percent of offense that comes from post")
