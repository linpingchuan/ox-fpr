\section{Midi}
\label{midi}

Midi (``musical instrument digital interface'') is a standard protocol
adopted by most, if not all, manufacturers of electronic instruments.
At its core is a protocol for communicating {\em musical events} (note
on, note off, key press, etc.) as well as so-called {\em meta events}
(select synthesizer patch, change volume, etc.).  Beyond the logical
protocol, the Midi standard also specifies electrical signal
characteristics and cabling details.  In addition, it specifies what
is known as a {\em standard Midi file} which any Midi-compatible
software package should be able to recognize.

Over the years musicians and manufacturers decided that they also
wanted a standard way to refer to {\em common} or {\em general}
instruments such as ``acoustic grand piano,'' ``electric piano,''
``violin,'' and ``acoustic bass,'' as well as more exotic ones such as
``chorus aahs,'' ``voice oohs,'' ``bird tweet,'' and ``helicopter.''
A simple standard known as {\em General Midi} was developed to fill
this role.  It is nothing more than an agreed-upon list of instrument
names along with a {\em program patch number} for each, a parameter in
the Midi standard that is used to select a Midi instrument's sound.

Most ``sound-blaster''-like sound cards on conventional PC's know
about Midi, as well as General Midi.  However, the sound generated by
such modules, and the sound produced from the typically-scrawny
speakers on most PC's, is often quite poor.  It is best to use an
outboard keyboard or tone generator, which are attached to a computer
via a Midi interface and cables.  It is possible to connect several
Midi instruments to the same computer, with each assigned a different
{\em channel}.  Modern keyboards and tone generators are quite amazing
little beasts.  Not only is the sound quite good (when played on a
good stereo system), but they are also usually {\em multi-timbral},
which means they are able to generate many different sounds
simultaneously, as well as {\em polyphonic}, meaning that simultaneous
instantiations of the same sound are possible.

If you decide to use the General Midi features of your sound-card, you
need to know about another set of conventions known as ``Basic Midi.''
The most important aspect of Basic Midi is that Channel 10 (9 in
Haskore's 0-based numbering) is dedicated to {\em percussion}.  A
future release of Haskore should make these distinctions more
concrete.

Haskore provides a way to specify a Midi channel number and General
Midi instrument selection for each {\tt IName} in a Haskore
composition.  It also provides a means to generate a Standard Midi
File, which can then be played using any conventional Midi software.
Finally, it provides a way for existing Midi files to be read and
converted into a Music object in Haskore.  In this section the
top-level code needed by the user to invoke this functionality will be
described, along with the gory details.  

\begin{verbatim}

> module HaskToMidi (performToMidi, UserPatchMap,
>                    module GeneralMidi, module OutputMidi,
>                    module LoadMidi)
>        where
> import Basics
> import Performance
> import MidiFile
> import GeneralMidi
> import Data.List(partition)
> import Data.Char(toLower,toUpper)
> import OutputMidi
> import LoadMidi

\end{verbatim} 

Instead of converting a Haskore {\tt Performance} directly into a Midi
file, Haskore first converts it into a datatype that {\em represents}
a Midi file, which is then written to a file in a separate pass.  This
separation of concerns makes the structure of the Midi file clearer,
makes debugging easier, and provides a natural path for extending
Haskore's functionality with direct Midi capability.

Here is the basic structure of the key modules (*) and functions (=):
\begin{verbatim}
             *LoadMidi*                    *ReadMidi*
 +------+  =loadMidiFile=  +-----------+   =readMidi=    +-----------+
 | MIDI	|----------------->| MidiFile  |---------------->| Music     |
 | File |                  | data type |                 | data type |
 |      |<-----------------|           |<----------------|           |
 +------+                  +-----------+  *HaskToMidi*   +-----------+ 
            *OutputMidi*    *MidiFile*    *Performance*    *Basics*
          =outputMidiFile=                 =makeMidi=
\end{verbatim}

A {\tt UserPatchMap} is a user-supplied table for mapping instrument
names ({\tt IName}'s) to Midi channels and General Midi patch names.
The patch names are by default General Midi names, although the user
can also provide a {\tt PatchMap} for mapping Patch Names to
unconventional Midi Program Change numbers.
\begin{verbatim}

> type UserPatchMap = [(IName,GenMidiName,MidiChannel)]

\end{verbatim}

See Appendix \ref{test-functions} for an example of a useful user
patch map.

Given a {\tt UserPatchMap}, a performance is converted to a datatype
representing a Standard Midi File using the {\tt performToMidi}
function.  If the given {\tt UserPatchMap} is invalid, it creates a new
one by matching the instrument names with General Midi names and mapping
the instruments to channels one by one.
\begin{verbatim}

> performToMidi :: Performance -> UserPatchMap -> MidiFile
> performToMidi pf pMap =
>   let splitList   = splitByInst pf
>       insts       = map fst splitList
>       rightMap    = if (allValid pMap insts)
>                       then pMap
>                       else (makeGMMap insts)
>   in MidiFile mfType (Ticks division)
>          (map (performToMEvs rightMap) (splitByInst pf))

\end{verbatim}

A table of General Midi assignments called {\tt genMidiMap} is
imported from {\tt GeneralMidi} in Appendix~\ref{general-midi}.  The
Midi file datatype itself is imported from the module {\tt MidiFile},
functions for writing it to files are found in the module {\tt OutputMidi},
and functions for reading MIDI files come from the modules {\tt LoadMidi}
and {\tt ReadMidi}.  All these modules are described later in this section.

The following function is used to test whether or not every instrument
in a list is found in a {\tt UserPatchMap}.  

\begin{verbatim}

> allValid :: UserPatchMap -> [IName] -> Bool
> allValid upm = and . map (lookupB upm)
>
> lookupB :: UserPatchMap -> IName -> Bool
> lookupB [] _           = False
> lookupB ((y,_,_):ys) x = x `partialMatch` y || lookupB ys x

\end{verbatim}

If a Haskore user only uses General Midi instrument names as {\tt
INames}, we can define a function that automatically creates a {\tt
UserPatchMap} from these names.  Note that, since there are only 15
Midi channels plus percussion, we can handle only 15 instruments.
Perhaps in the future a function could be written to test whether or
not two tracks can be combined with a Program Change (tracks can be
combined if they don't overlap).
\begin{verbatim}

> makeGMMap :: [IName] -> UserPatchMap
> makeGMMap iList = makeGMMap' 0 iList
>   where makeGMMap' _ []        = []
>         makeGMMap' n _ | n>=15 = 
>                    error "Too many instruments; not enough MIDI channels."
>         makeGMMap' n (i:is)    = 
>                    if map toLower i `elem` percList
>                    then (i, "Acoustic Grand Piano", 9) : makeGMMap' n is
>                    else (i, gmMatch i genMidiMap, chanList !! n)
>                                                        : makeGMMap' (n+1) is
>         gmMatch i ((gmInst,chan):gcs) = 
>                   if i `partialMatch` gmInst -- or use == ?
>                   then gmInst
>                   else gmMatch i gcs
>         gmMatch i [] = error("instrument " ++ i ++ " not found")
>         percList = ["percussion", "perc", "drums"]
>         chanList = [0..8] ++ [10..15]  -- 10th channel (#9) is for percussion

\end{verbatim}

\subsection{The Gory Details}

Some preliminaries, otherwise known as constants:
\begin{verbatim}

> mfType   = 1   :: MFType -- midi-file type 1 always used
> division = 96  :: Int    -- time-code division: 96 ticks per quarter note

\end{verbatim}

Since we are implementing Type 1 Midi Files, we can associate each
instrument with a separate track.  So first we partition the event list
into separate lists for each instrument.  (Again, due to the limited
number of MIDI channels, we can handle no more than 15 instruments.)
\begin{verbatim}

> splitByInst :: Performance ->  [(IName,Performance)]
> splitByInst [] = []
> splitByInst pf = (i,pf1) : splitByInst pf2
>                    where i         = eInst (head pf)
>			   (pf1,pf2) = partition (\e -> eInst e == i) pf

\end{verbatim}

The crux of the conversion process is {\tt performToMEvs}, which
converts a {\tt Performance} into a stream of {\tt MEvents}.
\begin{verbatim}

> performToMEvs :: UserPatchMap -> (IName,Performance) -> [MEvent]
> performToMEvs pMap (inm,perf) =
>   let (midiChan,progNum) = unMap pMap inm
>       setupInst          = MidiEvent 0 (ProgChange midiChan progNum)
>       setTempo           = MetaEvent 0 (SetTempo defST)
>       loop []     = []
>       loop (e:es) = let (mev1,mev2) = mkMEvents midiChan e
>                     in  mev1 : insertMEvent mev2 (loop es)
>   in  setupInst : setTempo : loop perf

\end{verbatim}

A source of incompatibilty between Haskore and Midi is that Haskore
represents notes with an onset and a duration, while Midi represents
them as two separate events, an note-on event and a note-off event.
Thus {\tt MkMEvents} turns a Haskore {\tt Event} into two {\tt
MEvents}, a {\tt NoteOn} and a {\tt NoteOff}.
\begin{verbatim}

> mkMEvents :: MidiChannel -> Event -> (MEvent,MEvent)
> mkMEvents mChan (Event {eTime = t, ePitch = p, eDur = d, eVol = v})
>                   = (MidiEvent (toDelta t)     (NoteOn  mChan p v'),
>                      MidiEvent (toDelta (t+d)) (NoteOff mChan p v') )
>           where v' = min 127 (round v)
>
> toDelta t = round (t * 4.0 * float division)

\end{verbatim}

The final critical function is {\tt insertMEvent}, which inserts an
{\tt MEvent} into an already time-ordered sequence of {\tt MEvents}.
\begin{verbatim}

> insertMEvent :: MEvent -> [MEvent] -> [MEvent]
> insertMEvent mev1  []         = [mev1]
> insertMEvent mev1@(MidiEvent t1 _) mevs@(mev2@(MidiEvent t2 _):mevs') = 
>       if t1 <= t2 then mev1 : mevs
>                   else mev2 : insertMEvent mev1 mevs'

\end{verbatim}

The following functions lookup {\tt IName}'s in {\tt UserPatchMaps} to
recover channel and program change numbers.  Note that the function
that does string matching ignores case, and allows substring matches.
For example, {\tt "chur"} matches {\tt "Church Organ"}.  Note also
that the {\em first} match succeeds, so using a substring should be
done with care to be sure that the correct instrument is selected.
\begin{verbatim} 

> unMap :: UserPatchMap -> IName -> (MidiChannel,ProgNum)
> unMap pMap iName = (channel, gmProgNum gmName)
>   where (gmName, channel) = lookup23 iName pMap
> 
> gmProgNum :: GenMidiName -> ProgNum
> gmProgNum gmName = lookup2 gmName genMidiMap
> 
> partialMatch       :: String -> String -> Bool
> partialMatch "piano" "Acoustic Grand Piano" = True
> partialMatch s1 s2 = 
>   let s1' = map toLower s1;  s2' = map toLower s2
>       len = min (length s1) (length s2)
>   in take len s1' == take len s2'
>
> lookup2  x ((y,z):ys) = if x `partialMatch` y then z else lookup2 x ys
> lookup2  x []           = error ("Instrument " ++ x ++ " unknown")
>
> lookup23 x ((y,z,q):ys) = if x `partialMatch` y then (z,q) else lookup23 x ys
> lookup23 x []           = error ("Instrument " ++ x ++ " unknown")

\end{verbatim}