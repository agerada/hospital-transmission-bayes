extensions [csv time]

globals
[
  ward-outline ;; structure of wards in format: ward bed-number bay?
  wards-per-row

  ;;bay-proportion
  ;;toilet-contamination-effect ;; max contamination effect when toilet used
  ;;toilet-cleaning-effect ;; reduction in contamination when toilet cleaned
  ;;toilet-cleaning-rate ;; times per 24 ticks toilet cleaned
  ;;toilet-frequenting-rate ;; times per 24 ticks toilet needed
  ;;community-colonisation-rate
  ;;antibiotic-prescription-rate
  ;;proportion-redistributed
  ;;antibiotic-effect
  ;;random-colonisation-thresh
  ;;amission-days
  ;;bay-capacity the number of beds in each bay

  total-patients-admitted
  total-colonised
  total-hospital-infections
  admission-durations

  patients-waiting-move

  current-inpatients ;; current number of inpatients
  current-colonised
  current-community-infections
  current-hospital-infections

  b-toilet-frequenting-rate
  b-toilet-contamination-effect
  b-toilet-cleaning-effect
  b-toilet-cleaning-rate
  b-community-colonisation-rate
  b-antibiotic-prescription-rate
  b-proportion-redistributed

  all-toilets
  all-bedspaces
  bay-bedspaces
  side-room-bedspaces
]

breed [patients patient]

patients-own
[
  community-infection?
  hospital-infection?
  antibiotic-treated?
  my-toilet
  my-bedspaces
  colonised?
  admission-tick
]

patches-own
[
  ward
  bed-number
  bed-capacity
  bed-availability
  bay?
  toilet?
  toilet-contamination ;; 0 = not contaminanted, 1 = fully contaminated
]

to setup
  clear-all
  reset-ticks
  set wards-per-row sqrt wards-total
  resize-world 0 - (wards-total * 5) (wards-total * 5) 0 - (wards-total * 5) (wards-total * 5)

  set ward-outline []
  set admission-durations []

  set b-toilet-frequenting-rate toilet-frequenting-rate
  set b-toilet-contamination-effect toilet-contamination-effect
  set b-toilet-cleaning-effect toilet-cleaning-effect
  set b-toilet-cleaning-rate toilet-cleaning-rate
  set b-community-colonisation-rate community-colonisation-rate
  set b-antibiotic-prescription-rate antibiotic-prescription-rate
  set b-proportion-redistributed proportion-redistributed

  set total-patients-admitted 0
  set total-colonised 0
  setup-grid

  set all-toilets patches with [ toilet? = true ]
  set all-bedspaces patches with [ toilet? = false and bed-number > 0 ]
  set bay-bedspaces all-bedspaces with [ bay? ]
  set side-room-bedspaces all-bedspaces with [ not bay? ]
  populate-patients
end

to go
  set current-community-infections count patients with [ community-infection? ]
  set current-hospital-infections count patients with [ hospital-infection? ]
  set current-inpatients count patients
  set current-colonised count patients with [ community-infection? or hospital-infection? ]
  set-variable-parameters
  update-patients
;  clean-toilets
  schedule-toilet-use
  schedule-toilet-clean
  redistribute-patients
  tick
;  print time:show-schedule
  time:go-until ticks
end

to setup-grid
  ;; sets up a grid of wards using a central bedspace as a marker for the lattice
  ;; code adapted from https://stackoverflow.com/questions/49968890/how-to-set-a-regular-lattice-of-turtles-in-the-world
  ;;
  ask patches [ set pcolor black ]
  let h-int world-width / wards-per-row
  let v-int world-height / wards-per-row

  let h-vals ( range ( min-pxcor + h-int / 2 ) max-pxcor h-int )
  let v-vals ( range ( min-pycor + v-int / 2 ) max-pycor h-int )

  let possible-coords []

  foreach v-vals [
    v -> set possible-coords ( sentence possible-coords map [ i -> ( list i v ) ] h-vals )
  ]

  let use-coords sublist possible-coords 0 wards-total
  let ward-counter 1

  ;; use dummy turtle to populate the ward with bedspaces using create-ward
  foreach use-coords [
    coords ->
    create-turtles 1 [
      setxy item 0 coords item 1 coords
      ask self [ create-ward self ward-counter bedspaces-per-ward white ]
      set ward-counter ward-counter + 1
      die
    ]
  ]
end

to create-ward [ t w beds ward-color ]
  ;; using a dummy turtle, start by creating a central bedspace for each ward
  ;; each bedspace is 1 patch surrounded by all neighbors
  let bed-counter 1
  let temp-outline []

  ask t [
    set heading 0
    ask patch-here
    [
      set pcolor ward-color
      set toilet? false
      set ward w
      set bed-number bed-counter
      ask neighbors [
        set toilet? false
        set pcolor ward-color
        set ward w
        set bed-number bed-counter
      ]
      ask one-of neighbors [
        set pcolor 37
        set toilet? true
      ]
      set bed-counter bed-counter + 1
      ifelse random-float 1.0 < bay-proportion
      [
        set bay? true
        set bed-capacity 4
        set bed-availability 4
        ask neighbors [
          set bay? true
          set bed-capacity 4
          set bed-availability 4
        ]
      ]
      [
        set bay? false
        set bed-capacity 1
        set bed-availability 1
        ask neighbors [
          set bay? false
          set bed-capacity 1
          set bed-availability 1
        ]
      ]
      set temp-outline (list w bed-number bay?) ;; create temp info for bed
    ]
  ;; update global outline of ward structures
  set ward-outline lput temp-outline ward-outline

  repeat beds - 1
    [
      forward 4
      while  [ [ pcolor ] of patch-here = ward-color ]
      [
        back 4
        set heading one-of [ 0 90 180 270 ]
        forward 4
      ]
      ask patch-here
      [
        set pcolor ward-color
        set toilet? false
        set ward w
        set bed-number bed-counter
        ask neighbors
        [
          set pcolor ward-color
          set toilet? false
          set ward w
          set bed-number bed-counter
        ]
        ask one-of neighbors
        [
          set pcolor 37
          set toilet? true
        ]
        set bed-counter bed-counter + 1
        ifelse (random-float 1 <= bay-proportion)
        [
          set bay? true
          set bed-capacity 4
          set bed-availability 4
          ask neighbors [
            set bay? true
            set bed-capacity 4
            set bed-availability 4
          ]
        ]
        [
          set bay? false
          set bed-capacity 1
          set bed-availability 1
          ask neighbors [
            set bay? false
            set bed-capacity 1
            set bed-availability 1
          ]
        ]
        set temp-outline (list w bed-number bay?) ;; create temp info for bed

      ]
      right 90
      ;; update global outline of ward structures
      set ward-outline lput temp-outline ward-outline
    ]

  ]
end

to populate-patients

  foreach ward-outline
  [
    outline ->
    ;; procedure for single room
    ifelse (item 2 outline = false)
    [
      ask one-of patches with [ward = item 0 outline
        and bed-number = item 1 outline
        and toilet? != true]
      [ make-patient ]
    ]
    ;; procedure for bay
    [
      ask n-of 4 patches with [ward = item 0 outline
          and bed-number = item 1 outline
          and toilet? != true]
      [ make-patient ]
    ]
  ]
end

to update-patients
;  ask patients [
;    let n-times-to-toilet random-poisson toilet-frequenting-rate
;    repeat random-poisson toilet-frequenting-rate [
;      move-to my-toilet
;      contaminate-toilet self
;      check-toilet-colonise-patient self
;    ]
;
;    move-to one-of my-bedspaces with [ not any? patients-here ]
;  ]

  ;; discharge some patients and replace with new admissions
;  ask patients [ if ticks - admission-tick > random-poisson ( admission-days ) [
;;      ask patch-here [make-patient]
;      discharge-patient
;    ]
;  ]

  ;; see if patients get randomly colonised
  ask patients [
    let abx-multiplier ifelse-value (antibiotic-treated?) [antibiotic-effect] [1]
    let de-novo-event 1 - exp ( - ( random-colonisation * ( 1 / 10000 ) ) * abx-multiplier )
    if random-float 1 < de-novo-event
    [
      set colonised? true
      set color red
      set hospital-infection? true
      set total-hospital-infections total-hospital-infections + 1
      set total-colonised total-colonised + 1
    ]
  ]
end

to schedule-toilet-use
  ask patients [
    let n-times-to-toilet random-poisson toilet-frequenting-rate
    repeat random-poisson toilet-frequenting-rate [
      time:schedule-event self [ [] -> use-toilet self ] ( ticks + random-float 1 )
    ]
  ]
end

to use-toilet [ t ]
  ask t [
    move-to my-toilet
    contaminate-toilet self
    check-toilet-colonise-patient self
  ]
  move-to one-of my-bedspaces with [ not any? patients-here ]
end

to contaminate-toilet [ t ]
  ;; contaminates toilet up to a maximum of 1.0 (fully contaminated)
  ask t [ if colonised? = true [set toilet-contamination min list 1.0 ( toilet-contamination + random-float toilet-contamination-effect ) ] ]
end

to check-toilet-colonise-patient [ t ]
  ask t [
    if not colonised? [
      let abx-multiplier ifelse-value (antibiotic-treated?) [antibiotic-effect] [1]
      let toilet-event 1 - exp ( - ( toilet-contamination * abx-multiplier ) )
      if random-float 1 < toilet-event [
        set colonised? true
        set hospital-infection? true
        set total-hospital-infections total-hospital-infections + 1
        set total-colonised total-colonised + 1
        set color red
      ]
    ]
  ]
end

to schedule-toilet-clean
  ask all-toilets [
    let n-times-clean-toilet random-poisson toilet-cleaning-rate
    repeat n-times-clean-toilet [
      time:schedule-event self [ [] -> clean-toilets ] ( ticks + random-float 1 )
    ]
  ]
end

to make-random-colonised [ p ]
  ;; unused
  ask patients [ ifelse random-float 1 < p
    [
      set colonised? true
      set color red
    ]
    [
      set colonised? false
      set color green
    ]
  ]
end

to make-patient
  ;; make a patient, using community colonisation rate global
  ;; to decide if colonised
  ;; call this procedure from within an ask patch
  sprout-patients 1 [
    set shape "person"
    ifelse random-float 1 < community-colonisation-rate [
      set colonised? true
      set community-infection? true
      set total-colonised total-colonised + 1
      set color red
    ]
    [
      set colonised? false
      set community-infection? false
      set color green
    ]

    ifelse random-float 1 < antibiotic-prescription-rate [
      set antibiotic-treated? true
    ]
    [
      set antibiotic-treated? false
    ]

    set total-patients-admitted total-patients-admitted + 1
    set hospital-infection? false
    set admission-tick ticks
    set my-toilet one-of all-toilets with [ ward = [ward] of myself and bed-number = [bed-number] of myself]
    set my-bedspaces all-bedspaces with [ ward = [ward] of myself and bed-number = [bed-number] of myself]
    ask my-bedspaces [
      set bed-availability bed-availability - 1
    ]

    time:schedule-event self [ [] -> discharge-patient ]  ( ticks + random-poisson admission-days )
  ]
end

to clean-toilets
;  ask all-toilets
;  [
;    if random-float 1 < (toilet-cleaning-rate / 24)
;    repeat random-poisson toilet-cleaning-rate [
    set toilet-contamination toilet-contamination - (toilet-contamination * toilet-cleaning-effect)
;    ]
;  ]
end

to redistribute-patients
  ask n-of ( proportion-redistributed * current-colonised ) patients with [ colonised? ] [
    if bay? [
      let swap-candidate one-of patients with [ not colonised? and not bay?]
      ifelse swap-candidate = nobody [
        ;debug show "No side room available for me"
      ] [
;        set color blue
        ;debug show (word "Swapping locations with " swap-candidate ", xcor: " [xcor] of swap-candidate ", ycor: " [ycor] of swap-candidate )
        clean-toilets
        let tmp-xcor xcor
        let tmp-ycor ycor
        let tmp-my-toilet my-toilet
        let tmp-my-bedspaces my-bedspaces
        set xcor [xcor] of swap-candidate
        set ycor [ycor] of swap-candidate
        set my-toilet [my-toilet] of swap-candidate
        set my-bedspaces [my-bedspaces] of swap-candidate
        ask swap-candidate [
;          set color yellow
          clean-toilets
          set xcor tmp-xcor
          set ycor tmp-ycor
          set my-toilet tmp-my-toilet
          set my-bedspaces tmp-my-bedspaces
        ]
      ]
    ]
  ]
end

to discharge-patient
  ;; discharge patient
  ;; call this procedure from within an ask patch
  set admission-durations lput ( ticks - admission-tick ) admission-durations
  ask my-bedspaces [
    set bed-availability bed-availability + 1
  ]
  ask patch-here [ make-patient ]
  clean-toilets
  die
end

to-report bool-to-int [ b ]
  ifelse b [ report 1 ] [ report 0 ]
end

to set-variable-parameters
  if outbreak?
  [
    if ticks > outbreak-start
    [
      set toilet-frequenting-rate o-toilet-frequenting-rate
      set toilet-contamination-effect o-toilet-contamination-effect
      set toilet-cleaning-effect o-toilet-cleaning-effect
      set toilet-cleaning-rate o-toilet-cleaning-rate
      set community-colonisation-rate o-community-colonisation-rate
      set antibiotic-prescription-rate o-antibiotic-prescription-rate
      set proportion-redistributed o-proportion-redistributed
    ]
    if ticks > outbreak-end
    [
      set toilet-frequenting-rate b-toilet-frequenting-rate
      set toilet-contamination-effect b-toilet-contamination-effect
      set toilet-cleaning-effect b-toilet-cleaning-effect
      set toilet-cleaning-rate b-toilet-cleaning-rate
      set community-colonisation-rate b-community-colonisation-rate
      set antibiotic-prescription-rate b-antibiotic-prescription-rate
      set proportion-redistributed b-proportion-redistributed
    ]
  ]


  if infection-control?
  [
    if ticks > control-start
    [
      set toilet-cleaning-effect c-toilet-cleaning-effect
      set toilet-cleaning-rate c-toilet-cleaning-rate
      set antibiotic-prescription-rate c-antibiotic-prescription-rate
      set proportion-redistributed c-proportion-redistributed
    ]
    if ticks > control-end
    [
      set toilet-cleaning-effect b-toilet-cleaning-effect
      set toilet-cleaning-rate b-toilet-cleaning-rate
      set antibiotic-prescription-rate b-antibiotic-prescription-rate
      set proportion-redistributed b-proportion-redistributed
    ]
  ]
end

to read-abc-params
  let f user-file
  let params []
  if f != false [
    carefully [
      set params csv:from-file f
    ] [
      user-message "The parameter file provided does not appear to be a valid .csv"
    ]
  ]

  if not empty? params [
    carefully [
      foreach params [ x ->
        let param-name (first x)
        let values (butfirst x)
        let values-mean precision mean values 3
        run (word "set " param-name " " values-mean)
      ]
    ] [
      user-message "The parameter file provided does not appear to be in a valid format. Please see model info"
    ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
810
10
2138
1339
-1
-1
8.2
1
10
1
1
1
0
1
1
1
-80
80
-80
80
0
0
1
ticks
30.0

BUTTON
26
33
92
66
NIL
setup
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
115
33
178
66
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
21
140
194
173
bedspaces-per-ward
bedspaces-per-ward
0
20
14.0
1
1
NIL
HORIZONTAL

TEXTBOX
21
74
171
92
Setup options
11
0.0
1

SLIDER
21
277
216
310
toilet-contamination-effect
toilet-contamination-effect
0
1
0.822
0.05
1
NIL
HORIZONTAL

SLIDER
20
177
192
210
bay-proportion
bay-proportion
0
1
0.6
0.1
1
NIL
HORIZONTAL

SLIDER
20
313
218
346
toilet-cleaning-effect
toilet-cleaning-effect
0
1
0.741
0.05
1
NIL
HORIZONTAL

SLIDER
22
351
217
384
toilet-cleaning-rate
toilet-cleaning-rate
0
24
3.794
0.1
1
NIL
HORIZONTAL

SLIDER
21
239
216
272
toilet-frequenting-rate
toilet-frequenting-rate
0
24
5.165
0.1
1
NIL
HORIZONTAL

SLIDER
21
412
226
445
community-colonisation-rate
community-colonisation-rate
0
1
0.03
0.01
1
NIL
HORIZONTAL

TEXTBOX
22
225
172
243
Toilet options
11
0.0
1

TEXTBOX
24
396
174
414
Admission options
11
0.0
1

PLOT
207
10
494
190
Proportion colonised
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"colonised" 1.0 0 -2674135 true "" "plot current-colonised"
"not colonised" 1.0 0 -13840069 true "" "plot count turtles with [ colonised? = False ]"

CHOOSER
21
92
159
137
wards-total
wards-total
4 9 16 32
2

SLIDER
21
485
225
518
antibiotic-prescription-rate
antibiotic-prescription-rate
0
1
0.376
0.001
1
NIL
HORIZONTAL

SLIDER
22
587
231
620
antibiotic-effect
antibiotic-effect
0
100
1.557
0.1
1
RR/OR
HORIZONTAL

SLIDER
22
447
194
480
admission-days
admission-days
0
100
9.0
1
1
NIL
HORIZONTAL

TEXTBOX
23
570
173
588
Colonisation parameters
11
0.0
1

TEXTBOX
248
195
398
213
Outbreak parameters
11
0.0
1

SWITCH
246
216
366
249
outbreak?
outbreak?
1
1
-1000

SLIDER
246
252
398
285
outbreak-start
outbreak-start
1
10000
1028.516
1
1
ticks
HORIZONTAL

SLIDER
245
321
420
354
o-toilet-frequenting-rate
o-toilet-frequenting-rate
0.1
24
5.165
0.1
1
NIL
HORIZONTAL

SLIDER
245
356
449
389
o-toilet-contamination-effect
o-toilet-contamination-effect
0.01
1
0.822
0.01
1
NIL
HORIZONTAL

SLIDER
244
391
444
424
o-toilet-cleaning-effect
o-toilet-cleaning-effect
0.01
1
0.37
0.01
1
NIL
HORIZONTAL

SLIDER
244
426
433
459
o-toilet-cleaning-rate
o-toilet-cleaning-rate
0
24
1.486
0.1
1
NIL
HORIZONTAL

SLIDER
244
461
466
494
o-community-colonisation-rate
o-community-colonisation-rate
0.01
1
0.03
0.01
1
NIL
HORIZONTAL

SLIDER
243
496
483
529
o-antibiotic-prescription-rate
o-antibiotic-prescription-rate
0.01
1
0.636
0.01
1
NIL
HORIZONTAL

SWITCH
477
223
646
256
infection-control?
infection-control?
1
1
-1000

SLIDER
478
259
650
292
control-start
control-start
0
10000
1049.0
1
1
NIL
HORIZONTAL

SLIDER
477
329
686
362
c-toilet-cleaning-effect
c-toilet-cleaning-effect
0.01
1
0.741
0.01
1
NIL
HORIZONTAL

SLIDER
480
365
668
398
c-toilet-cleaning-rate
c-toilet-cleaning-rate
0
12
3.794
0.1
1
NIL
HORIZONTAL

SLIDER
479
400
729
433
c-antibiotic-prescription-rate
c-antibiotic-prescription-rate
0.01
1
0.376
0.01
1
NIL
HORIZONTAL

SLIDER
245
288
417
321
outbreak-end
outbreak-end
1
10000
1283.066
1
1
NIL
HORIZONTAL

SLIDER
478
294
650
327
control-end
control-end
1
10000
1340.953
1
1
NIL
HORIZONTAL

BUTTON
21
665
159
698
Load Parameters
read-abc-params
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
21
625
337
658
random-colonisation
random-colonisation
0
1000
6.796
0.1
1
cases per 10,000 bed days
HORIZONTAL

TEXTBOX
481
203
631
221
Enhanced IPC parameters
11
0.0
1

PLOT
494
10
787
190
Acquisition
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"community" 1.0 0 -6459832 true "" "plot current-community-infections"
"hospital" 1.0 0 -14070903 true "" "plot current-hospital-infections"

SLIDER
20
521
229
554
proportion-redistributed
proportion-redistributed
0
1
0.849
0.01
1
NIL
HORIZONTAL

SLIDER
478
435
701
468
c-proportion-redistributed
c-proportion-redistributed
0
1
0.849
0.01
1
NIL
HORIZONTAL

SLIDER
243
531
467
564
o-proportion-redistributed
o-proportion-redistributed
0
1
0.358
0.01
1
NIL
HORIZONTAL

@#$#@#$#@
## Introduction
This is an agent-based model of a hospital environment for the purpose of simulating transmission of faecal-oral transmitted healtchare infection/colonisation. The model is intended to simulate colonisation/infection that transmits mainly through toilet use and is increased with antimicrobial exposure, e.g., _Clostridioides difficile_ infection. It also attempts to optionally simulate an outbreak and control through enhanced infection prevention mechanisms.

## How it works

Firstly, a hospital structure is set up, with multiple wards. Each ward can have a mixture of bays and side rooms. The main difference is that bays can have multiple patients sharing a toilet. At each tick (corresponding to a day in real time), patients are admitted and discharged to maintain full capacity of the hospitals. Toilets are used and contaminated. Patients can get colonised/infected through: 

* use of a contaminated toilet
* randomly, which occur more often if the patient is on antimicrobial therapy

Every tick, toilets are cleaned and patients redistributed such that infected/colonised patients are moved to side rooms.

An outbreak may be triggerred at a particular time point in the simulation. If this happens, some parameters change to encourage increased transmission. Then, an optional limited period of enhanced infection control practice may be triggerred to deal with the increased transmission.

## How to use it

Firstly, use the "Setup options" to define the hospital structure: 

1) _wards-total_: the total number of wards in the hospital,
2) _bedspaces-per-ward_: the number of bedspaces available in each ward (each bedspace can be a bay or side-room; bays can accomodoate 4 patients, while side-rooms only one),
3) _bay-proportion_: the proportion of bedspaces that are bays.

Now, press "Setup" to create this hospital. The hospital window on the right may exceed the viewing area on the monitor. To adjust this window, right click on the hospital, click "Select", and adjust the size.

Next, there are many parameters that can be changed which can affect transmission and control of infection/colonisation. The model comes with a default configuration of parameters that can be loaded. These parameters have been generated by calibrating the model to data of a _C. difficile_ outbreak reported by Salgado et al. [1]. 

Assuming that the parameters are left at these defaults, now just press "Go" to run the simulation. Observe that patients are admitted, use toilets, and may get colonised/infected. Green patients are uninfected, red are infected. Eventually, once the simualtion reaches the outbreak time-point, the number of cases (observe the two plots) increase. The outbreak is then recognised and enhanced infection control measures are implemented to control it, bringing rates back down to baseline. 

## Paremeter details

* _random-colonisation_: Daneman, 2015 [2] -- Rate of 6.2 cases per 10,000 patient bed days for _C. difficile_.

## Things to try

Try changing the different parameters to look at the impact on transmission. Start by turning off both the "outbreak?" and "infection-control?" switches so that no outbreak takes place. Run the simulation using the default settings. While the simulation is running, try changing different paramaters to pusuh the rate of infection up. Which parameters seem to have the most impact? 

Try comparing the following two scenarios:

Firstly load the default parameters using "Load Parameters". Make sure "outbreak?" and "infection-control?" are turned on, to simulate and outbreak and enchanced infection control. Run the simulation and observe the outbreak curve. 

Now, re-load the default parameters. This time, keep "outbreak?" turned on, but turn off "infection-control?". Re-run the simulation. What happens to the outbreak curve now?

## Parameter import data format

The model can load parameters from a .csv file, using "Load Parameters". The folowing format is required:

* No column names (i.e., data starts from the first row)
* Column 1 is the parameter name
* From columns 2 onwards, parameter values for each parameter; if multiple values are provided, the model will calculate and use the mean.

An example of this format is provided with the model.

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## Changelog

* 0.44.1 - patched all-toilets container

* 0.44 - now includes a "terminal clean". A toilet clean is performed when patients are discharged or when moving location (i.e., through a swap). Currently, the clean is the same as any other clean (i.e., same cleaning effect).

* 0.43 - adds exponential functions to the bernoulli trials for random colonisation and toilet colonisation. This does not change functionality of the model, but aligns with mathematical notation.

* 0.42 - uses time extension to schedule toilet clean and use during the day using discrete event simulation.

## CREDITS AND REFERENCES

1. Salgado CD, Mauldin PD, Fogle PJ, Bosso JA. Analysis of an outbreak of Clostridium difficile infection controlled with enhanced infection control measures. American Journal of Infection Control. 2009 Aug;37(6):458–64. 

2. Daneman N, Guttmann A, Wang X, Ma X, Gibson D, Stukel T. The association of hospital prevention processes and patient risk factors with the risk of Clostridium difficile infection: a population-based cohort study. BMJ Qual Saf. 2015 Jul;24(7):435–43. 
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
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count turtles with [colonised? = True] / count turtles with [colonised? != True]</metric>
    <enumeratedValueSet variable="bedspaces-per-ward">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wards-total">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="discharge-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="toilet-cleaning-rate">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="toilet-cleaning-effect">
      <value value="0.9"/>
    </enumeratedValueSet>
    <steppedValueSet variable="bay-proportion" first="0" step="0.1" last="1"/>
    <enumeratedValueSet variable="community-colonisation-rate">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="toilet-contamination-effect">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="toilet-frequenting-rate">
      <value value="2"/>
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
@#$#@#$#@
0
@#$#@#$#@
