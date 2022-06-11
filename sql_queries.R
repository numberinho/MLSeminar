transfermarkt_sql <- "SELECT
    pm.owner_since,
    gw.gameWeek,
    card_rarity,
    pc.player_slug,
    pm.club_slug,
    pc.player_position,
    pc.player_age,
    pm.card_serial,
    if(card_serial = card_shirt,1,0) card_shirt,
    owner_number,
    ETH,
    EUR,
    player_opta_uuid
FROM
    datenbank.sorare_publicMarket pm
        LEFT JOIN
    sorare_club sc ON pm.club_slug = sc.club_slug
		LEFT JOIN
	sorare_playercopy pc on pc.player_slug = pm.player_slug
		LEFT JOIN 
	sorare_gameWeeks gw on gw.startDate < owner_since and gw.endDate > owner_since
WHERE
    league_slug = 'bundesliga-de'
        AND (card_rarity = 'limited');"

gameWeeks_sql <- "SELECT gameWeek, endDate FROM datenbank.sorare_gameWeeks;"

score_sql <- "SELECT gameWeek, player_slug, score, mins_played  FROM datenbank.sorare_score"
