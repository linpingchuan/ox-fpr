ghci -i$HOME/Oxford/Courses/FPR/SOE/haskore/src: Tequila.lhs
play tequila

 {-# LANGUAGE Haskell98 #-}

> module Tequila where
> import Haskore
> import System.Process ( system )
> import Data.Ratio

The song consists of an intro, the body, and a coda.

> tequila :: Music
> tequila = tequilaIntro :+: tequilaBody :+: tequilaCoda
> tequilaIntro =
>   drumIntro :+:
>   (drums :=: bass) :+:
>   (drums :=: bass :=: guitar) :+:
>   (drums :=: bass :=: guitar :=: brassIntro)
> tequilaBody =
>   cut 32 (repeatM (twice (drums :=: bass :=: guitar) :=: brass))
> tequilaCoda =
>   drumCoda :=: bassCoda :=: guitarCoda :=: brassCoda

> twice m = m :+: m

brass is eight bars long, everything else is four bars

> brass, brassIntro :: Music
> brassIntro 
>   = Instr "Brass Section" (line [
>           wnr,
>           wnr,
>           wnr,
>           Rest (tie dhn en'), d en''
>       ])
>     where
>       d d = Note (D,4) d []
> brass = Instr "Brass Section" brassRiff
> brassRiff = line [
>           g qn, g en', f en'', a en', f (tie en'' en'), g (tie en''
>           en'), d (tie en'' (tie hn en')), d en'',
>           g qn, g en', f en'', a en', f (tie en'' en'), g (tie en''
>           (tie dhn en')), d en'',
>           g qn, g en', f en'', a en', f (tie en'' en'), g (tie en''
>           en'), d (tie en'' (tie hn en')), d en'',
>           g qn, g en', f en'', a en', f (tie en'' en'), d (tie en''
>           (tie hn qn)), en'r, d en''
>         ]
>   where
>     g d = Note (G,4) d []
>     f d = Note (F,4) d []
>     a d = Note (A,4) d []
>     d d = Note (D,4) d []
> brassCoda = Instr "Brass Section" (
>   cut 2 brassRiff :+:
>   line [ g qn, g qn, f qn ] :+: 
>   Phrase [-- Art (Slurred 1.5), -- sounds terrible on ThinkPad!
>           Dyn (Diminuendo 1)] (line [ g qn, g qn, g qn, g qn, g qn ]))
>   where
>     g d = Note (G,4) d []
>     f d = Note (F,4) d []

The guitar part is four bars long.

> guitar :: Music
> guitar = Instr "Electric Guitar (jazz)" chordSeq
> chordSeq =
>   line [g qn, g qn, f (tie qn en'), g (tie en'' 
>          en'), g (tie en'' en'), g en'', f en', f en'', f en', f en'',
>          g qn, g qn, f (tie qn en'), g (tie en'' 
>          en'), f (tie en'' (tie qn en')), f en'', f en', f en'']
>   where
>     g = eChord G; f = eChord F

> guitarCoda = Instr "Electric Guitar (jazz)" (
>   cut 2 chordSeq :+:
>   line [g qn, g qn, f qn, g (tie qn wn)])
>   where
>     g = eChord G; f = eChord F

This is a standard E-position chord on guitar:

> eShape :: Dur -> [Music]
> eShape dur = [ n o dur [Volume 30] | (n,o) <- 
>              -- [(e,3),(b,3),(e,4),(gs,4),(b,4),(gs,5)] ]
>              [(e,3),(b,3),(e,4)] ]

...and this shifts it up the neck:

> eChord :: PitchClass -> Dur -> Music
> eChord key d 
>   | pc < pcE  = Trans (12+pc-pcE) (chord (eShape d))
>   | otherwise = Trans (pc-pcE) (chord (eShape d))
>   where 
>     pc = pitchClass key
>     pcE = pitchClass E

It don't mean a thing if it ain't got that swing!

> en', en'' :: Dur -- swung eighth notes
> en'  = 2%12
> en'' = 1%12
> en'r = Rest en'
> en''r = Rest en''

> tie :: Dur -> Dur -> Dur
> tie d d' = d + d'

The drum part is based on Paul Hudak's "jazz groove":

> drums = Instr "Drums" (drumIntro :=: cut 4 (repeatM
>                  ((qnr :+: p2 en' :+: p2 en'')
>                   :=: p3 hn) ))
>   where
>     p1 d = perc RideCymbal2   d [Volume 50]
>     p2 d = perc AcousticSnare d [Volume 30]
>     p3 d = perc LowTom        d [Volume 50]

> drumCoda = Instr "Drums" (cut 2 drums :+: 
>   line [ 
>     chord [ p1 qn, p2 qn, p3 qn], 
>     chord [ p1 qn, p2 qn, p3 qn], 
>     chord [ p1 qn, p2 qn, p3 qn], 
>     chord [ p1 qn, p2 qn, p3 qn, p4 (tie qn wn)] ])
>   where
>     p1 d = perc RideCymbal2   d [Volume 50]
>     p2 d = perc AcousticSnare d [Volume 30]
>     p3 d = perc LowTom        d [Volume 50]
>     p4 d = perc SplashCymbal  d [Volume 100]

> drumIntro = Instr "Drums" (cut 4 (repeatM (
>             p qn :+: p en' :+: p en'')))
>   where p d = perc PedalHiHat d [Volume 50]

The bass is a simple repeated riff:

> bass = Instr "Fretless Bass" bassline
> bassline = cut 4 (repeatM (
>          line [ g 2 (tie qn en') [], 
>                 f 3 (tie en'' en') [], 
>                 c 3 en'' [], 
>                 a 2 qn [] ]))
> bassCoda = Instr "Fretless Bass" (
>   cut 2 bassline :+: 
>   line [g 2 qn [], g 2 qn [], f 2 qn [], g 2 en' [], en''r, wnr])

We override the default context to 300 beats per minute:

> write m = outputMidiFile "test.mid" (goMidi m)
> goMidi m = performToMidi (goPerf m) defUpm
> goPerf m = perform defPMap defCon' m
> defCon' = defCon { cDur = metro 300 qn }

> playWin95 = do
>                 system "mplayer test.mid"
>                 return ()

> playWin2000 = do
>                 system "\"c:\\program files\\windows media player\\mplayer2\" test.mid"
>                 return ()

> playXLinux = do
>                 system "timidity test.mid"
>                 return ()

> playALinux = do
>                 system "export term=ansi" -- doesn't have the desired effect
>                 system "playmidi -rf test.mid"
>                 return ()

> go m = do { write m ; playXLinux }
