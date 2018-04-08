;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                       ;;
;; This model is a two-dimensional, dynamic implementation of the model from Fujita and Ogawa (1982)     ;;
;;                                                                                                       ;;
;; Author : Justin Delloye                                                                               ;;
;; Version : February 2017                                                                               ;;
;;                                                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Global variables that are controled by the user
;
;  n                  ;The number of households
;  alpha              ;The locational potential parameter
;  t                  ;The commuting cost per unit of distance
;  randomSeed         ;The random seed
;  repetitions        ;The desired number of runs
;  name               ;The filename for exporting the results
;  buildings          ;Show/hide agents
;  journeys           ;Show/hide commuting lines
;  balance            ;Power balance in wage negociation
;


;Other global variables
globals [
  lb                  ;The number of workers hired by business firms
  ra                  ;The agricultural land rent
  k                   ;The monetary conversion rate of the locational potential
  sb                  ;The land used by the business firms
  sh                  ;The land consumed by the households
  actualSeed          ;A number that describe the pseudo-random state of the current run
  settledN            ;The number of settled households
  m                   ;The number of settled business firms
  paramOK             ;TRUE if all the parameter values are allowed
  eqNumber            ;Equilibrium number
]


;Sets of agents
breed [ households household ]                  ;The set of households
breed [ businesses business ]                   ;The set of business firms
directed-link-breed [ commutings commuting ]    ;The set of commuting segments


;Agents variables
patches-own [
  b                  ;The number of business firms on this cell
  f                  ;The locational potential on this cell
  h                  ;The number of households on this cell
  i_A                ;Anticipated income
  l                  ;The weighted mean distance to workers
  o                  ;The available space on this cell
  phi_A              ;Anticipated land rent of businesses
  psi_A              ;Anticipated land rent of households
  r                  ;The land rent of this cell
  w_A                ;Anticipated wage proposed by businesses
]


households-own [
  income              ;The wage of this households
  utility             ;The utility (or consumption of composite good) of this households
  boss
]


businesses-own [
  wage               ;The wage payed by this business firm
  profit             ;The profit of the business firm
]



;; Observer's procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Households move loop
to o_adjust
  while [min [utility] of households < 0] [
    o_move
  ]
end ;to adjust


;; Households move somewhere else
to o_move
  ask households with [utility < 0] [
    
    let path sort-on [distance [boss] of myself] (patches with [o >= sh])
    let i 0
    
    while [utility < 0] [
 
      ask patch-here [
        set h (h - 1)
        set o (o + sh)
      ]
      
      move-to item i path
      
      ask patch-here [
        set h (h + 1)
        set o (o - sh)
      ]
      
      o_setWagesOf boss
      set i (i + 1)
      
    ]

  ]
end ;to o_move


;; New business settling
to-report o_bSettling
  if (n - settledN) < lb [
    report false
  ]
  let winPatch max-one-of (patches with [b = 0]) [phi_A]
  if [phi_A] of winPatch < ra [
    report false
  ]
  ask turtles-on winPatch [die]
  create-businesses 1 [
    set size 0.95
    set color gray
    set xcor [pxcor] of winPatch
    set ycor [pycor] of winPatch
    set wage [w_A] of winPatch
  ]
  report true
end ;to-report o_bSettling


;; report anticipated wage
to-report p_w_A
  let wmin (ra * sh + t * l)
  let wmax (k * f - ra * sb) / (lb)
  report precision ( wmin + (wmax - wmin) * (balance / 100) ) 10
end ;to-report p_w_A


;; report commuting distance
to-report p_l
  let lsum 0
  let nsum 0
  ask other patches [
    let ntmp (h + ceiling (o / sh))
    set nsum (nsum + ntmp)
    set lsum (lsum + (distance myself) * ntmp)
  ]
  report precision (lsum / nsum) 10
end; to-report p_l


;; Check parameter values
to-report o_checkParam
  let ok true
  if alpha < 0 [
    output-print "Error : 'alpha' must be bigger than 0"
    set ok false
  ]
  if t < 0 [
    output-print "Error : 't' must be bigger than 0"
    set ok false
  ]
  if repetitions <= 0 [
    output-print "Error : 'repetitions' must be bigger than 0"
    set ok false
  ]
  if (repetitions != 0) and not (repetitions mod (floor repetitions) = 0)  [
    output-print "Error : 'repetitions' must be an integer"
    set ok false
  ]
  if (round randomSeed > 2147483647) or ( round randomSeed < (- 2147483648)) [
    output-print "Error : 'randomSeed' must be between -2147483648 and 2147483647"
    set ok false    
  ]
  if not (randomSeed mod (floor randomSeed) = 0)  [
    output-print "Error : 'randomSeed' must be an integer"
    set ok false
  ]
  report ok
end ;to-report o_checkParam


;; Launch the dynamic model
to o_goEq
  repeat repetitions [
    if (not paramOK) [
      output-print "The last 'Setup' procedure failed"
      stop
    ]
    if (not o_nextEq) [
      output-print ""
      output-print "No business wants to settle down anymore"
      output-print word "Run finished at : " date-and-time
      stop
    ]
    tick
    output-print ""
    output-print word "Equilibrium " word eqNumber word " reached at " date-and-time
    if Export [
      export-view word name (word "_move" (word ticks "_view.png"))
      export-world word name (word "_move" (word ticks "_world.csv"))
    ]
  ]
end ;to o_goEq


;; generate a new random seed
to o_newSeed
  set randomSeed new-seed
end ; to o_newSeed


;; Main dynamic cycle
to-report o_nextEq
  if (not o_bSettling) [
    o_updatePatches
    o_view
    report false
  ]
  o_updatePatches
  o_view
  tick
  while [settledN < m * lb] [
    o_hSettling
    o_updatePatches
    o_view
    tick
  ]
  clear-links
  ask households [
    set boss nobody
  ]
  o_setJobs
  o_setWagesOf businesses
  o_view
  o_adjust
  set eqNumber (eqNumber + 1)
  report true
end ;to-report o_nextEq


; Clear job market
to o_setJobs
  foreach sort-on [(- wage)] businesses [
    ask min-n-of lb (households with [boss = nobody]) [distance ?] [
      create-commuting-to ? [
        set color yellow
        set thickness 0.07
        set shape "empty"
      ]
      set boss ?
    ]
  ]
end ;to o_setJobs


;; Businesses change wages and other agents adapt
to o_setWagesOf [thoseBusinesses]
  
  let thoseHouseholds turtle-set ([in-commuting-neighbors] of thoseBusinesses)
  let thosePatches patch-set ([patch-here] of thoseHouseholds)
  
  ask thoseBusinesses [
    let dtmp max-one-of in-commuting-neighbors [distance myself]
    let wmin (ra * sh + t * distance dtmp)
    let wmax (k * [f] of patch-here - ra * sb) / (lb)
    set wage precision ( wmin + (wmax - wmin) * (balance / 100) ) 10
    ask patch-here [
      set r precision ( (1 / sb) * ( k * [f] of self - lb * [wage] of myself ) ) 10
    ]
    set profit precision ( k * [f] of patch-here - lb * wage - sb * [r] of patch-here ) 10
  ]
  ask thoseHouseholds [
    set income precision ( [wage] of boss - t * distance boss ) 10
  ]
  ask thosePatches [
    let imax max [income] of households-here
    set r precision ( (1 / sh) * imax ) 10
    ask households-here [
      set utility precision ( income - sh * [r] of myself ) 10
    ]
  ]

end; to o_setWagesOf
  

;; Businesses hire households
to o_equilibrate
  foreach sort-on [(- wage)] businesses [
    ask min-n-of lb (households with [boss = nobody]) [distance ?] [
      create-commuting-to ? [
        set color yellow
        set thickness 0.07
        set shape "empty"
      ]
      set boss ?
    ]
  ]
  ask businesses [
    let dtmp max-one-of in-commuting-neighbors [distance myself]
    let wmin (ra * sh + t * distance dtmp)
    let wmax (k * [f] of patch-here - ra * sb) / (lb)
    set wage precision ( wmin + (wmax - wmin) * (balance / 100) ) 10
    ask patch-here [
      set r precision ( (1 / sb) * ( k * [f] of self - lb * [wage] of myself ) ) 10
    ]
    set profit precision ( k * [f] of patch-here - lb * wage - sb * [r] of patch-here ) 10

  ]
  ask households [
    set income precision ( [wage] of boss - t * distance boss ) 10
  ]
  ask patches [
    let imax max [income] of households-here
    set r precision ( (1 / sh) * imax ) 10
  ]
  ask households [
    set utility precision ( income - sh * [r] of patch-here ) 10
  ]
end ;to equilibrate


;; Househols settle
to o_hSettling
  if m * lb <= settledN [
    stop
  ]
  let winPatch max-one-of (patches with [h <= round (1 / sh)]) [psi_A]
  if [psi_A] of winPatch < ra [
    stop
  ]
  ;!!! tenir compte de disponibilité d'espace!
  create-households 1 [
    set size 0.5
    set color gray
    set xcor [pxcor] of winPatch
    set ycor [pycor] of winPatch
    set boss nobody
  ]
end ;to o_hSettling


;; Initialise the model
to o_setup
  
  clear-all
  reset-ticks

  set paramOK o_checkParam
  if (not paramOK) [
    stop
  ]

  random-seed randomSeed
  
  set eqNumber 0
  set lb 2
  set ra 1
  set sh precision (1 / lb) 10
  set sb 1
  set k 100
  set settledN 0
  set m 0
  
  ask patches [
    set r ra
  ]
  o_updatePatches
  
  set-default-shape businesses "office building"
  set-default-shape households "house"
  o_view
  output-print word "Setup finished at : " date-and-time
end ;to o_setup


;; Change view of the world
to o_view
  ask turtles [
    set hidden? (not buildings)
  ]
  
  ask links [
    set hidden? (not journeys)
  ]
  
  if (view = "composite") [
    ask patches [
      if o = 1 [
        set pcolor white
        update-plots update-plots
        stop
      ]
      ifelse b = 1 [
        set pcolor blue
      ][
        ;set pcolor 19 - floor ( (h - 1) / 2)
        set pcolor 19 - 2 * h
      ]
    ]
    update-plots update-plots
    stop
  ]
  
  if (view = "b") [
    let low min [b] of patches
    let up max [b] of patches
    ifelse (low = up) [
      ask patches [set pcolor blue]
    ][
      ask patches [set pcolor scale-color blue b up low]
    ]
    update-plots update-plots
    stop
  ]
  
  if (view = "F") [
    let low min [f] of patches
    let up max [f] of patches
    ifelse (low = up) [
      ask patches [set pcolor magenta]
    ][
      ask patches [set pcolor scale-color magenta f up low]
    ]
    update-plots update-plots
    stop
  ]
  
  if (view = "h") [
    let low min [h] of patches
    let up max [h] of patches
    ifelse (low = up) [
      ask patches [set pcolor red]
    ][
      ask patches [set pcolor scale-color red h up low]
    ]
    update-plots update-plots
    stop
  ]
  
  if (view = "i") [
    if (settledn = 0) [
      ask patches [
        set pcolor white
      ]
      update-plots update-plots
      stop
    ]
    let up max [income] of households
    let low min [income] of households
    ifelse (low = up) [
      ask patches with [h > 0] [set pcolor violet]
      ask patches with [h <= 0] [set pcolor white]
    ][
      ask patches with [h > 0] [set pcolor scale-color violet (one-of [income] of households-here) up low]
      ask patches with [h <= 0] [set pcolor white]
    ]
    update-plots update-plots
    stop
  ]
  
  if (view = "i_A") [
    let low min [i_A] of patches
    let up max [i_A] of patches
    ifelse (low = up) [
      ask patches [set pcolor violet]
    ][
      ask patches [set pcolor scale-color violet i_A up low]
    ]
    update-plots update-plots
    stop
  ]
  
  if (view = "l") [
    let low min [l] of patches
    let up max [l] of patches
    ifelse (low = up) [
      ask patches [set pcolor green]
    ][
      ask patches [set pcolor scale-color green l up low]
    ]
    update-plots update-plots
    stop
  ]
  
  if (view = "o") [
    let low min [o] of patches
    let up max [o] of patches
    ifelse (low = up) [
      ask patches [set pcolor green]
    ][
      ask patches [set pcolor scale-color green o up low]
    ]
    update-plots update-plots
    stop
  ]
  
  if (view = "phi_A") [
    let low min [phi_A] of patches
    let up max [phi_A] of patches
    ifelse (low = up) [
      ask patches [set pcolor orange]
    ][
      ask patches [set pcolor scale-color orange phi_A up low]
    ]
    update-plots update-plots
    stop
  ]
  
  if (view = "pi") [
    if (m = 0) [
      ask patches [
        set pcolor white
      ]
      update-plots update-plots
      stop
    ]
    let up max [profit] of businesses
    let low min [profit] of businesses
    ifelse (low = up) [
      ask patches with [b > 0] [set pcolor gray]
      ask patches with [b <= 0] [set pcolor white]
    ][
      ask patches with [b > 0] [set pcolor scale-color gray (one-of [profit] of businesses-here) up low]
      ask patches with [b <= 0] [set pcolor white]
    ]
    update-plots update-plots
    stop
  ]
  
  if (view = "psi_A") [
    let low min [psi_A] of patches
    let up max [psi_A] of patches
    ifelse (low = up) [
      ask patches [set pcolor orange]
    ][
      ask patches [set pcolor scale-color orange psi_a up low]
    ]
    update-plots update-plots
    stop
  ]
  
  if (view = "r") [
    let low min [r] of patches
    let up max [r] of patches
    ifelse (low = up) [
      ask patches [set pcolor orange]
    ][
      ask patches [set pcolor scale-color orange r up low]
    ]
    update-plots update-plots
    stop
  ]
  
  if (view = "rb") [
    if (m = 0) [
      ask patches [
        set pcolor white
      ]
      update-plots update-plots
      stop
    ]
    let low min [r] of patches with [b > 0]
    let up max [r] of patches with [b > 0]
    ifelse (low = up) [
      ask patches [
        ifelse b > 0
        [set pcolor orange]
        [set pcolor white]
      ]
    ][
      ask patches [
        ifelse b > 0
        [set pcolor scale-color orange r up low]
        [set pcolor white]
      ]
    ]
    update-plots update-plots
    stop
  ]
  
  if (view = "rh") [
    if (settledn = 0) [
      ask patches [
        set pcolor white
      ]
      update-plots update-plots
      stop
    ]
    let low min [r] of patches with [h > 0]
    let up max [r] of patches with [h > 0]
    ifelse (low = up) [
      ask patches [
        ifelse h > 0
        [set pcolor orange]
        [set pcolor white]
      ]
    ][
      ask patches [
        ifelse h > 0
        [set pcolor scale-color orange r up low]
        [set pcolor white]
      ]
    ]
    update-plots update-plots
    stop
  ]
  
  if (view = "u") [
    if (settledN = 0) [
      ask patches [
        set pcolor white
      ]
      update-plots update-plots
      stop
    ]
    let up max [utility] of households
    let low min [utility] of households
    ifelse (low = up) [
      ask patches with [h > 0] [set pcolor gray]
      ask patches with [h <= 0] [set pcolor white]
    ][
      ask patches with [h > 0] [set pcolor scale-color gray (one-of [utility] of households-here) up low]
      ask patches with [h <= 0] [set pcolor white]
    ]
    update-plots update-plots
    stop
  ]
  
  if (view = "w") [
    if (m = 0) [
      ask patches [
        set pcolor white
      ]
      update-plots update-plots
      stop
    ]
    let up max [wage] of businesses
    let low min [wage] of businesses
    ifelse (low = up) [
      ask patches with [b > 0] [set pcolor violet]
      ask patches with [b <= 0] [set pcolor white]
    ][
      ask patches with [b > 0] [set pcolor scale-color violet (one-of [wage] of businesses-here) up low]
      ask patches with [b <= 0] [set pcolor white]
    ]
    update-plots update-plots
    stop
  ]
  
  if (view = "w_A") [
    let low min [w_A] of patches
    let up max [w_A] of patches
    ifelse (low = up) [
      ask patches [set pcolor violet]
    ][
      ask patches [set pcolor scale-color violet w_A up low]
    ]
    update-plots update-plots
    stop
  ]
end ;to o_view


;; Update paches variables
to o_updatePatches
  set settledN count households
  set m count businesses
  
  ask patches [
    set h count households-here
    set b count businesses-here
    set o precision (1 - (b * sb) - (h * sh)) 10
  ]
  
  ask patches [
    set l p_l
    set f p_f
  ]
  
  ask patches [
    set w_A p_w_A
    set phi_A p_phi_A
    set i_A p_i_A
    set psi_A p_psi_A
  ]
  
  ask businesses [
    set wage [w_A] of patch-here
  ]
end ;to o_updatePatches


;; report anticipated bid rent of households
to-report p_psi_A
  let psi precision ( (1 / sh) * i_A ) 10
  if (psi < r) [
   report 0
  ] 
  report psi
end ;to-report p_psi_A


;; report anticipated income earned by households
to-report p_i_A
  if (m = 0) or ([b] of self > 0)[
    report 0
  ]
  let isum 0
  ask businesses [
    set isum (isum + (wage - t * distance myself))
  ]
  report precision (isum / m) 10
end ;to-report p_i_A


;; report locational potential
to-report p_f
  let ftmp 0
  ask other patches [
    set ftmp ( ftmp + (b * exp((- alpha) * distance myself)) )
  ]
  report precision (ftmp + 1) 10
end ;to-report pFHere


;; report anticipated bid rent of businesses
to-report p_phi_A
  let phi precision ( (1 / sb) * ( k * f - lb * w_A ) ) 10
  if (phi < r) [
    report 0
  ] 
  report phi
end ;to-report p_phi_A
@#$#@#$#@
GRAPHICS-WINDOW
573
10
1087
545
10
10
24.0
1
10
1
1
1
0
0
0
1
-10
10
-10
10
1
1
1
moves
30.0

INPUTBOX
10
51
165
111
alpha
0.01
1
0
Number

INPUTBOX
268
233
379
293
repetitions
1
1
0
Number

INPUTBOX
270
57
425
117
randomSeed
-1593781217
1
0
Number

INPUTBOX
253
472
478
532
name
test
1
0
String

BUTTON
270
17
405
50
New Random Seed
o_newSeed
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
253
434
356
467
Export
Export
1
1
-1000

INPUTBOX
10
118
165
178
t
0.1
1
0
Number

BUTTON
11
12
75
45
Setup
o_setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
15
455
153
500
view
view
"composite" "b" "F" "h" "i" "i_A" "l" "o" "phi_A" "pi" "psi_A" "r" "rb" "rh" "u" "w" "w_A"
0

BUTTON
15
506
78
539
View
o_view
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
268
298
378
331
Next equilibria
o_goEq
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
388
233
488
293
Go forever
o_goEq
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

OUTPUT
573
557
1081
763
12

SWITCH
15
417
138
450
Journeys
Journeys
0
1
-1000

SWITCH
15
380
118
413
Buildings
Buildings
0
1
-1000

PLOT
1096
10
1562
364
Histogram
values
effecive
0.0
10.0
0.0
10.0
true
false
"set-histogram-num-bars (2 * max-pxcor + 1) ^ 2\nset-plot-y-range 0 1\n\nlet low 0\nlet up 1\n\nif (view = \"composite\") [\n  set-histogram-num-bars 1\n]\n  \nif (view = \"b\") [\n  set-histogram-num-bars 2\n]\n  \nif (view = \"F\") [\n  set low min [f] of patches\n  set up max [f] of patches\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"h\") [\n  set low min [h] of patches\n  set up max [h] of patches\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n  set-histogram-num-bars 11\n]\n  \nif (view = \"i\") and (settledn > 0) [\n  set up max [income] of households\n  set low min [income] of households\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"i_A\") [\n  set low min [i_A] of patches\n  set up max [i_A] of patches\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"l\") [\n  set low min [l] of patches\n  set up max [l] of patches\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"o\") [\n  set low min [o] of patches\n  set up max [o] of patches\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"phi_A\") [\n  set low min [phi_A] of patches\n  set up max [phi_A] of patches\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"pi\") and (m > 0) [\n  set up max [profit] of businesses\n  set low min [profit] of businesses\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"psi_A\") [\n  set low min [psi_A] of patches\n  set up max [psi_A] of patches\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"r\") [\n  set low min [r] of patches\n  set up max [r] of patches\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"rb\") and (m > 0) [\n  set low min [r] of patches with [b > 0]\n  set up max [r] of patches with [b > 0]\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"rh\") and (settledn > 0) [\n  set low min [r] of patches with [h > 0]\n  set up max [r] of patches with [h > 0]\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"u\") and (settledN > 0) [\n  set up max [utility] of households\n  set low min [utility] of households\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"w\") and (m > 0) [\n  set up max [wage] of businesses\n  set low min [wage] of businesses\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"w_A\") [\n  set low min [w_A] of patches\n  set up max [w_A] of patches\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n\nset-plot-x-range low up" "set-histogram-num-bars 20\nset-plot-y-range 0 1\n\nlet low 0\nlet up 1\n\nif (view = \"composite\") [\n  set-histogram-num-bars 1\n  \n]\n  \nif (view = \"b\") [\n  set-histogram-num-bars 2\n]\n  \nif (view = \"F\") [\n  set low min [f] of patches\n  set up max [f] of patches\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"h\") [\n  set low min [h] of patches\n  set up max [h] of patches\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n  set-histogram-num-bars 11\n]\n  \nif (view = \"i\") and (settledn > 0) [\n  set up max [income] of households\n  set low min [income] of households\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"i_A\") [\n  set low min [i_A] of patches\n  set up max [i_A] of patches\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"l\") [\n  set low min [l] of patches\n  set up max [l] of patches\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"o\") [\n  set low min [o] of patches\n  set up max [o] of patches\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"phi_A\") [\n  set low min [phi_A] of patches\n  set up max [phi_A] of patches\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"pi\") and (m > 0) [\n  set up max [profit] of businesses\n  set low min [profit] of businesses\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"psi_A\") [\n  set low min [psi_A] of patches\n  set up max [psi_A] of patches\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"r\") [\n  set low min [r] of patches\n  set up max [r] of patches\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"rb\") and (m > 0) [\n  set low min [r] of patches with [b > 0]\n  set up max [r] of patches with [b > 0]\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"rh\") and (settledn > 0) [\n  set low min [r] of patches with [h > 0]\n  set up max [r] of patches with [h > 0]\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"u\") and (settledN > 0) [\n  set up max [utility] of households\n  set low min [utility] of households\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"w\") and (m > 0) [\n  set up max [wage] of businesses\n  set low min [wage] of businesses\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n  \nif (view = \"w_A\") [\n  set low min [w_A] of patches\n  set up max [w_A] of patches\n  if (low = up) [\n    set up (low + 1)\n    set-histogram-num-bars 1\n  ]\n]\n\nset-plot-x-range low up"
PENS
"default" 1.0 1 -16777216 true "" "set-plot-pen-color black\n\nif (view = \"composite\") [\n  histogram (list 0)\n]\n  \nif (view = \"b\") [\n  set-plot-pen-color blue\n  histogram [b] of patches\n]\n  \nif (view = \"F\") [\n  set-plot-pen-color violet\n  histogram [f] of patches\n]\n  \nif (view = \"h\") [\n  set-plot-pen-color red\n  histogram [h] of patches\n]\n  \nif (view = \"i\") [\n  ifelse (settledn > 0) [\n    set-plot-pen-color violet\n    histogram [income] of households\n  ][\n    set-plot-pen-color violet\n    histogram (list 0)\n  ]\n]\n  \nif (view = \"i_A\") [\n  set-plot-pen-color violet\n  histogram [i_A] of patches\n]\n  \nif (view = \"l\") [\n  set-plot-pen-color green\n  histogram [l] of patches\n]\n  \nif (view = \"o\") [\n  set-plot-pen-color green\n  histogram [o] of patches\n]\n  \nif (view = \"phi_A\") [\n  set-plot-pen-color orange\n  histogram [phi_A] of patches\n]\n  \nif (view = \"pi\") [\n  ifelse (m > 0) [\n    set-plot-pen-color gray\n    histogram [profit] of businesses\n  ][\n    set-plot-pen-color gray\n    histogram (list 0)\n  ]\n]\n  \nif (view = \"psi_A\") [\n  set-plot-pen-color orange\n  histogram [psi_A] of patches\n]\n  \nif (view = \"r\") [\n  set-plot-pen-color orange\n  histogram [r] of patches\n]\n  \nif (view = \"rb\") [\n  ifelse (m > 0) [\n    set-plot-pen-color orange\n    histogram [r] of patches with [b > 0]\n  ][\n    set-plot-pen-color orange\n    histogram (list 0)\n  ]\n]\n  \nif (view = \"rh\") [\n  ifelse (settledn > 0) [\n    set-plot-pen-color orange\n    histogram [r] of patches with [h > 0]\n  ][\n    set-plot-pen-color orange\n    histogram (list 0)\n  ]\n]\n  \nif (view = \"u\") [\n  ifelse (settledn > 0) [\n    set-plot-pen-color gray\n    histogram [utility] of households\n  ][\n    set-plot-pen-color gray\n    histogram (list 0)\n  ]\n]\n  \nif (view = \"w\") [\n  ifelse (m > 0) [\n    set-plot-pen-color violet\n    histogram [wage] of businesses\n  ][\n    set-plot-pen-color violet\n    histogram (list 0)\n  ]\n]\n  \nif (view = \"w_A\") [\n  set-plot-pen-color violet\n  histogram [w_A] of patches\n]"

MONITOR
1097
373
1174
418
Households
settledN
17
1
11

MONITOR
1193
372
1266
417
Businesses
m
17
1
11

INPUTBOX
11
184
166
244
n
150
1
0
Number

SLIDER
10
280
182
313
balance
balance
0
100
50
5
1
%
HORIZONTAL

TEXTBOX
12
249
162
277
Households power in wage negociation
11
0.0
1

@#$#@#$#@
## GENERAL INFORMATION

This computer program models a two-sector city with heterogeneous mobility. It aims to explore the short-run and long-run urban configurations that emerge during the establishment of a new city under exogeneous population growth.

This model was developed for the International Master in Geographical Modelling 2017 in Rouen. We assume no responsability for any other use.

## CREDITS AND REFERENCES

### About NetLogo

Copyright 2012. Uri Wilensky.

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, US

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

### About the present file

Copyright 2017. Justin Delloye justin.delloye@uclouvain.be 
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

computer server
false
0
Rectangle -7500403 true true 75 30 225 270
Line -16777216 false 210 30 210 195
Line -16777216 false 90 30 90 195
Line -16777216 false 90 195 210 195
Rectangle -10899396 true false 184 34 200 40
Rectangle -10899396 true false 184 47 200 53
Rectangle -10899396 true false 184 63 200 69
Line -16777216 false 90 210 90 255
Line -16777216 false 105 210 105 255
Line -16777216 false 120 210 120 255
Line -16777216 false 135 210 135 255
Line -16777216 false 165 210 165 255
Line -16777216 false 180 210 180 255
Line -16777216 false 195 210 195 255
Line -16777216 false 210 210 210 255
Rectangle -7500403 true true 84 232 219 236
Rectangle -16777216 false false 101 172 112 184

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

office building
false
0
Rectangle -7500403 true true 75 30 225 270
Rectangle -7500403 false true 105 120 15 105
Rectangle -16777216 true false 90 45 105 90
Rectangle -16777216 true false 120 45 135 90
Rectangle -16777216 true false 165 45 180 90
Rectangle -16777216 true false 195 45 210 90
Rectangle -16777216 true false 90 105 105 150
Rectangle -16777216 true false 120 105 135 150
Rectangle -16777216 true false 165 105 180 150
Rectangle -16777216 true false 195 105 210 150
Rectangle -16777216 true false 195 165 210 210
Rectangle -16777216 true false 165 165 180 210
Rectangle -16777216 true false 120 165 135 210
Rectangle -16777216 true false 90 165 105 210
Rectangle -16777216 true false 120 240 150 270
Rectangle -16777216 true false 150 240 180 270

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="sensitivity analysis" repetitions="1" runMetricsEveryStep="true">
    <setup>o_setup
set name word name behaviorspace-run-number</setup>
    <go>o_goEq</go>
    <final>export-view word name (word "_move" (word ticks "_view.png"))
export-world word name (word "_move" (word ticks "_world.csv"))</final>
    <enumeratedValueSet variable="randomSeed">
      <value value="-1593781217"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="repetitions">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="alpha" first="0" step="0.5" last="2.5"/>
    <steppedValueSet variable="t" first="0" step="0.2" last="1"/>
    <enumeratedValueSet variable="Export">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Journeys">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="view">
      <value value="&quot;composite&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Buildings">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="name">
      <value value="&quot;run_&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="balance">
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

empty
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0

@#$#@#$#@
0
@#$#@#$#@
