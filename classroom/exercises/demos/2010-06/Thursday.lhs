ghci -Wall Thursday

> module Thursday
> where
> import Prelude hiding (flip)
> import List (sortBy)

Hacking session
---------------

A live ticker.

> data Team
>   =  England
>   |  USA
>   |  Algeria
>   |  Slovenia
>   deriving (Show, Eq, Ord, Enum)

> data Event  =  Tick | Goal Team
>   deriving (Show)

> type Match  =  (Team, Team)

> type Score  =  (Int, Int)

GH (goals home), GA (goals away).

> type Standings  =  [(Team, Stats)]

> type Stats  =  (Int, Int, Int, Int, Int)

W(ins), D(raws), L(osses), GF (goals for), GA (goals against).

The heart of the live ticker.

> liveTicker :: [Match] -> Standings -> [Event] -> [Standings]
> liveTicker matches standings
>   = map (livetable standings) . scanl tick (initialise matches)

> initialise :: [Match] -> [(Match, (Int, Int))]
> initialise  =  map (\ match -> (match, (0, 0)))

> tick :: [(Match, Score)] -> Event -> [(Match, Score)]
> tick livescores Tick         =  livescores
> tick livescores (Goal team)  =  map (updateScore team) livescores

> livetable :: Standings -> [(Match, Score)] -> Standings
> livetable standings
>   =  sortBy cmp . foldr updateStandings standings

A team has scored, update the score.

> updateScore :: Team -> (Match, Score) -> (Match, Score)
> updateScore team livescore@((home, away), (g1, g2))
>     | home == team  =  ((home, away), (g1 + 1, g2))
>     | away == team  =  ((home, away), (g1, g2 + 1))
>     | otherwise     =  livescore

Incorporating a live score into the table of standings.

> updateStandings :: (Match, Score) -> Standings -> Standings
> updateStandings livescore  =  map (updateStanding livescore)

> updateStanding :: (Match, Score) -> (Team, Stats) -> (Team, Stats)
> updateStanding ((home, away), (g1, g2)) (team, stat)
>   | home == team  =  (team, stat + points (g1, g2))
>   | away == team  =  (team, stat + points (g2, g1))
>   | otherwise     =  (team, stat)

Turning a score into points.

> points :: Score -> Stats
> points (g1, g2)
>   =  case compare g1 g2 of
>      LT -> (0, 0, 1, g1, g2)
>      EQ -> (0, 1, 0, g1, g2)
>      GT -> (1, 0, 0, g1, g2)

> instance (Num a, Num b, Num c, Num d, Num e) => Num (a, b, c, d, e) where
>   (a1, b1, c1, d1, e1) + (a2, b2, c2, d2, e2)
>     =  (a1 + a2, b1 + b2, c1 + c2, d1 + d2, e1 + e2)
>   (a1, b1, c1, d1, e1) * (a2, b2, c2, d2, e2)
>     =  (a1 * a2, b1 * b2, c1 * c2, d1 * d2, e1 * e2)
>   (a1, b1, c1, d1, e1) - (a2, b2, c2, d2, e2)
>     =  (a1 - a2, b1 - b2, c1 - c2, d1 - d2, e1 - e2)
>   abs (a, b, c, d, e)
>     =  (abs a, abs b, abs c, abs d, abs e)
>   signum (a, b, c, d, e)
>     =  (signum a, signum b, signum c, signum d, signum e)
>   fromInteger n
>     =  (fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n)

Two teams, which one is better (which translates into smaller)?

> cmp :: (Team, Stats) -> (Team, Stats) -> Ordering
> cmp (_t1, (w1, d1, _l1, gf1, ga1)) (_t2, (w2, d2, _l2, gf2, ga2))
>   =  compare pts2 pts1
>      >>> compare w2 w1
>      >>> compare (gf2 - ga2) (gf1 - ga1)
>      >>> compare gf2 gf1
>      where pts1  =  3 * w1 + d1
>            pts2  =  3 * w2 + d2

Lexicographic ordering.

> infixr 7 >>>
> (>>>) :: Ordering -> Ordering -> Ordering
> LT >>> _    =  LT
> EQ >>> ord  =  ord
> GT >>> _    =  GT

Test data.

> day3  :: [Match]
> day3  =  [ (Slovenia, England)
>          , (USA,      Algeria) ]

> standings0  ::  Standings
> standings0
>   =   [ (team, (0, 0, 0, 0, 0)) | team <- [England .. Slovenia] ]

> events :: [Event]
> events
>   =  [ Goal England
>      , Tick
>      , Goal USA
>      , Goal England
>      , Goal Slovenia
>      , Goal Slovenia
>      , Goal Slovenia ]

> main :: IO ()
> main  =  mapM_ print (liveTicker day3 standings0 events)