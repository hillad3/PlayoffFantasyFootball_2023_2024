# R_NFL_Playoff_Fantasy_HelpeR
A collection of R-language scripts for setting up and managing an NFL Playoff Fantasy contest. 

## About
- Playoff Fantasy Football is an elimination based version of Fantasy Football. Each contestant will create a diversified roster prior to the start of playoffs (Note: Multiple rosters are allowed per "owner", as long as each are paid for in full). All playoff games, including wildcards and the Super Bowl, will be considered in the scoring.
- The roster will be locked from changes after submission to the Commissioner.
- The Commissioner will provide weekly updates on Fantasy Team standings throughout the contest.  
- Scoring will follow typical Fantasy Football rules (detailed below).
- Rosters must be submitted, valid, and paid for by the start of the first wildcard game.
- Prizes will be awarded to the top 5 scoring entries.
  - Prize purses will be announced after wildcard playoff weekend, since prizes are dependent on number of paid entries.
- If you think you're going to win, spread the word: The more participants, the larger the prizes.
- If you think you're going to lose, spread the word: Imagine the commaraderie of shared experience!

## Team Selection
- Pick your own team using the 'Signup Roster.xlsx' in the Forms folder.
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
- Further point scoring is detailed below.

## Scoring
### Passing
Description | Points 
--- | ---
Every 50 passing yards | 1
400+ Yards | 2
40+ Yard Passing TDs | 2
Interceptions Thrown | -2
TD Pass | 6
2pt Passing Conversion | 2
Fumble | -2

### Rushing
Description | Points 
--- | ---
Every 10 Rushing Yards | 1
200+ Yards | 2
40+ Yard Rushing TDs | 4
TD Rush | 6
2pt Rushing Conversion | 2
Fumble | -2

### Receiving
Description | Points 
--- | ---
Every 10 Receiving Yards | 1
200+ Yards | 2
40+ Yard Receiving TDs | 2
TD Reception | 6
2pt Receiving Conversion | 2
Fumble | -2

### Kicking
Description | Points 
--- | ---
Each PAT Made | 1
FG Made (0-39 yards) | 3
FG Made (40-49 yards) | 4
FG Made (50+ yards) | 5
Each PAT Missed | -1
Each FG Missed | -1

### Defense / Special Teams
Description | Points 
--- | ---
Each Sack | 1
Each Interception | 2
Each Safety | 2
Each Fumble Recovery | 2
Blocked Punt, PAT or FG | 2
Interception Return TD | 6
Fumble Return TD | 6
Kickoff return TD | 6
Punt Return TD | 6
Block Punt/FG return for TD | 6
Fumble recovered for TD | 6
INT recovered for TD | 6
0 Points Allowed | 10
1-6 Points Allowed | 7
7-13 Points Allowed | 4
14-21 Points Allowed | 1
22-27 Points Allowed | -1
28-34 Points Allowed | -4
35-45 Points Allowed | -7
46+ Points Allowed | -10
