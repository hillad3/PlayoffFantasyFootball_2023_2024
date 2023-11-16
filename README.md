# R_NFL_Playoff_Fantasy_HelpeR
A collection of R-language scripts for setting up and managing an NFL Playoff Fantasy contest. 

# Rules
- The buy in is $25.
- Prizes will be awarded to the top 5 scoring entries.
- Spread the word: The more participants, the larger the prizes.
  - Actual prize amount will be announced wildcard weekend as prizes are dependent on number of paid participants.
- You pick your own team using the designated 'Signup Roster.xlsx'.
  - Rosters may only consist of 1 PLAYER from each team that is in the playoffs for a total of 14 players (including a Defensive / Special Teams).
  - You must pick players from the following positions:
    - Quarterback, QB: 3
    - Runningback, RB: 3
    - Wide Receiver, WR: 3
    - Tightend, TE: 2
    - FLEX: 1 (either a RB, WR, or TE)
    - Kicker, K: 1
    - Defense / Special Teams, D/ST: 1 
  - The 'Signup Roster.xlsx' is prepopulated with valid playoff players and team, and includes data validation and checks to help ensure your picks are valid.
- Points are accumulative throughout the playoffs, including the Super Bowl.
- Further point scoring is detailed below

# Scoring
## Passing
Category | Description | Points 
--- | --- | ---
Passing	| Every 50 passing yards | 1
Passing	| 400+ Yards | 2
Passing	| 40+ Yard Passing TDs | 2
Passing	| Interceptions Thrown | -2
Passing	| TD Pass | 6
Passing	| 2pt Passing Conversion | 2
Passing	| Fumble | -2

## Rushing
Category | Description | Points 
--- | --- | ---
Rushing	| Every 10 Rushing Yards | 1
Rushing	| 200+ Yards | 2
Rushing	| 40+ Yard Rushing TDs | 4
Rushing	| TD Rush | 6
Rushing	| 2pt Rushing Conversion | 2
Rushing	| Fumble | -2

## Receiving
Category | Description | Points 
--- | --- | ---
Receiving	| Every 10 Receiving Yards | 1
Receiving	| 200+ Yards | 2
Receiving	| 40+ Yard Receiving TDs | 2
Receiving	| TD Reception | 6
Receiving	| 2pt Receiving Conversion | 2
Receiving	| Fumble | -2

## Kicking
Category | Description | Points 
--- | --- | ---
Kicking	| Each PAT Made | 1
Kicking	| FG Made (0-39 yards) | 3
Kicking	| FG Made (40-49 yards) | 4
Kicking	| FG Made (50+ yards) | 5
Kicking	| Each PAT Missed | -1
Kicking	| Each FG Missed | -1

## Defense / Special Teams
Category | Description | Points 
--- | --- | ---
Defense / Special Teams	| Each Sack | 1
Defense / Special Teams	| Each Interception | 2
Defense / Special Teams	| Each Safety | 2
Defense / Special Teams	| Each Fumble Recovery | 2
Defense / Special Teams	| Blocked Punt, PAT or FG | 2
Defense / Special Teams	| Interception Return TD | 6
Defense / Special Teams	| Fumble Return TD | 6
Defense / Special Teams	| Kickoff return TD | 6
Defense / Special Teams	| Punt Return TD | 6
Defense / Special Teams	| Block Punt/FG return for TD | 6
Defense / Special Teams	| Fumble recovered for TD | 6
Defense / Special Teams	| INT recovered for TD | 6
Defense / Special Teams	| 0 Points Allowed | 10
Defense / Special Teams	| 1-6 Points Allowed | 7
Defense / Special Teams	| 7-13 Points Allowed | 4
Defense / Special Teams	| 14-21 Points Allowed | 1
Defense / Special Teams	| 22-27 Points Allowed | -1
Defense / Special Teams	| 28-34 Points Allowed | -4
Defense / Special Teams	| 35-45 Points Allowed | -7
Defense / Special Teams	| 46+ Points Allowed | -10
