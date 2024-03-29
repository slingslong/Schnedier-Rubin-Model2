;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
globals [
         remover ;; tracks the number of ticks so we can "retire" scientists once the number of scientists reaches num-scientists
         ticks-on-contest ;; tracks the number of ticks used on display-contest to calibrate aspects of model which depend on time-steps
       ]
turtles-own [group] ;; if a turtle has group = 0, it is a part of a HEG. If a turtle has group = 1, it is part of a HUG.

to setup
  clear-all
  set-default-shape turtles "circle"
  ;; make the initial network of two turtles and an edge
  create-turtles num-links [
    set color red
    create-links-with other turtles
    fd 8
  ]
  set remover 3 - num-scientists
  set ticks-on-contest 0
  reset-ticks
end

;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;


;; go procedure for building and maintaining network
to go
  ask turtles [set color red] ;; reset color if the model has just finished running display-contest
  ask links [ set color gray ]
  make-node
  set remover remover + 1
  if remover >= 0 [
    ask turtle remover [
      die
    ]
  ]
  layout
  tick
end

;; reports the difference between the proportion of contests won by a HEG member and the proportion of contests won by a HUG member
to-report heg-advantage
  let hug-wins 0
  let heg-wins 0
  repeat num-contests [
    ifelse run-contest = 0 [
      set heg-wins heg-wins + 1
    ]
    [ if run-contest = 1 [
      set hug-wins hug-wins + 1
      ]
    ]
  ]
  report (heg-wins - hug-wins ) / num-contests
end

;; reports the winner of a contest
to-report run-contest
  ;code for assigning contestants
  random-seed new-seed
  let winner nobody
  let hug-player one-of turtles with [group = 1]
  let heg-player one-of turtles with [group = 0]
  ask hug-player [set color blue]
  ask heg-player [set color green]
  while [count turtles with [color = red] != 0] [
    let scientist one-of turtles with [color = red]
    let blues turtles with [color = blue]
    let greens turtles with [color = green]
    let blues-list sort blues
    let greens-list sort greens
    set blues-list map [i -> path-length scientist i ] blues-list
    set greens-list map [i -> path-length scientist i] greens-list
    let distance1 min blues-list
    let distance2 min greens-list
    ifelse distance1 < distance2 [
      ask scientist [set color blue]
    ] [
      ifelse distance2 < distance1 [
        ask scientist [set color green]
      ] [
        ask scientist [set color one-of [blue green]]
      ]
    ]
  ]
  ifelse (count turtles with [color = blue] > 66) [
    set winner hug-player
    print ("Player from HUG wins!")
  ] [
    ifelse (count turtles with [color = green] > 66) [
      set winner heg-player
      print ("Player from HEG wins!")
    ] [
      print ("Tie!")
    ]
  ]
  reset-contest
  ifelse winner != nobody [
    report [group] of winner
  ]
  [ report 2]
end

;; a procedure that displays each step of a attribution contest.
to display-contest
  random-seed new-seed
  let winner nobody
  let hug-player one-of turtles with [group = 1]
  let heg-player one-of turtles with [group = 0]
  ask hug-player [set color blue]
  ask heg-player [set color green]
  while [count turtles with [color = red] != 0] [
    let scientist one-of turtles with [color = red]
    let blues turtles with [color = blue]
    let greens turtles with [color = green]
    let blues-list sort blues
    let greens-list sort greens
    set blues-list map [i -> path-length scientist i ] blues-list
    set greens-list map [i -> path-length scientist i] greens-list
    let distance1 min blues-list
    let distance2 min greens-list
    print (word "The scientist is " distance1 " steps away from the closest blue scientist. "
               "The scientist is " distance2 " steps away from the closest green scientist.")
    ifelse distance1 < distance2 [
      ask scientist [set color blue]
    ] [
      ifelse distance2 < distance1 [
        ask scientist [set color green]
      ] [
        ask scientist [set color one-of [blue green]]
      ]
    ]
    tick
    set ticks-on-contest ticks-on-contest + 1
  ]
  ifelse (count turtles with [color = blue] > 66) [
    set winner hug-player
    print ("Player from HUG wins!")
  ] [
    ifelse (count turtles with [color = green] > 66) [
      set winner heg-player
      print ("Player from HEG wins!")
    ] [
      print ("Tie!")
    ]
  ]
end

;; procedure to reset a contest
to reset-contest
  ask turtles [set color red]
end

;; procedure to make a node and connect it with other nodes.
to make-node
  create-turtles 1
  [
    set color red
    let community-proportion population_proportion / (1 + 10 * e ^ (-0.5 * (ticks - ticks-on-contest))) ;; here we use ticks-on-contest so that the ticks used in displaying a contest do not count towards the actual time-steps.
    ifelse random-float 1 <= community-proportion [
      set group 1
      set shape "box"
      set size 3
    ]
    [
      set group 0
    ]
    let partners find-partners self
    let old-nodes map [i -> item 0 i] partners
    foreach old-nodes [
      i ->
      create-link-with i [ set color green ]
      move-to i
      fd 8
    ]
    ;; code for adding advisors
    if count turtles >= 50 [
      let partner-list first partners
      let partner-turtle first partner-list
      let partner-link-neighbors sort [link-neighbors] of partner-turtle
      foreach partner-link-neighbors [
        i ->
        if self != i [ ; Ensure we're not linking to ourselves
          let ran random-float 1
          if ran < .5 [
            create-link-with i [ set color blue ]
          ]
        ]
      ]
    ]
  ]
end

;; procedure to find partners that a new node will connect to
to-report find-partners [new-node] ;; issue with turtle 178
  let old-nodes sort turtles with [self != new-node]
  let degrees sum map [i -> count [link-neighbors] of i] old-nodes
  let similarities sum map [i -> similarity new-node i] old-nodes
  let turtle-weights map [i -> list (i) (homophily * (similarity new-node i / (1 + similarities)) +
    (1 - homophily) * (count [link-neighbors] of i / degrees))] old-nodes
  ;; We weigh each turtle's chances of connecting based on their similarity and degree. See Rubin and Schneider (2021).
  report lottery-winners turtle-weights
end

;; reports 1 if two nodes are in the same group and 0 otherwise.
to-report similarity [new-node old-node]
  ifelse [group] of new-node = [group] of old-node [
    report 1
  ]
  [
    report 0
  ]
end

;; procedure that determines the group of nodes a new node will connect to.
to-report lottery-winners [turtle-weights]
  set turtle-weights shuffle turtle-weights
  let results []

  repeat num-links [
    let weights map [i -> item 1 i] turtle-weights
    let pick random-float sum weights
    let result nobody

    foreach turtle-weights [
      i ->
      if result = nobody [
        ifelse pick <= item 1 i [
          set result i
        ] [
          set pick pick - item 1 i
        ]
      ]
    ]

    if result != nobody [ ;;issue here
      set turtle-weights remove result turtle-weights
      set results lput result results
    ]
  ]
  report results
end

;; a procedure that reports the shortest path-length between two turtles using depth-first search
to-report path-length [start-turtle end-turtle]
  if start-turtle = end-turtle [ report 0 ] ; Path length is 0 if both turtles are the same

  let visited-nodes []
  let queue (list (list start-turtle 0)) ; Queue contains pairs of turtle and path length

  while [not empty? queue] [
    let current-item first queue
    let current-turtle item 0 current-item
    let current-length item 1 current-item
    set queue butfirst queue

    if not member? current-turtle visited-nodes [
      set visited-nodes lput current-turtle visited-nodes

      if current-turtle = end-turtle [
        report current-length ; Found the end turtle, report the path length
      ]

      ask current-turtle [
        let next-neighbors link-neighbors with [not member? self visited-nodes]
        let next-neighbors-list sort next-neighbors
        foreach next-neighbors-list [
          x ->
          set queue lput (list x (current-length + 1)) queue
        ]
      ]
    ]
  ]
  report -1 ; Return -1 or some indicator if no path is found
end

;;;;;;;;;;;;;;
;;; Layout ;;;
;;;;;;;;;;;;;;

;; resize-nodes, change back and forth from size based on degree to a size of 1
to resize-nodes
  ifelse all? turtles [size <= 1]
  [
    ;; a node is a circle with diameter determined by
    ;; the SIZE variable; using SQRT makes the circle's
    ;; area proportional to its degree
    ask turtles [ set size sqrt count link-neighbors ]
  ]
  [
    ask turtles [ set size 1 ]
  ]
end

to layout
  ;; the number 3 here is arbitrary; more repetitions slows down the
  ;; model, but too few gives poor layouts
  repeat 3 [
    ;; the more turtles we have to fit into the same amount of space,
    ;; the smaller the inputs to layout-spring we'll need to use
    let factor sqrt count turtles
    ;; numbers here are arbitrarily chosen for pleasing appearance
    layout-spring turtles links (1 / factor) (350 / factor) (20 / factor)
    display  ;; for smooth animation
  ]
  ;; don't bump the edges of the world
  let x-offset max [xcor] of turtles + min [xcor] of turtles
  let y-offset max [ycor] of turtles + min [ycor] of turtles
  ;; big jumps look funny, so only adjust a little each time
  set x-offset limit-magnitude x-offset 0.1
  set y-offset limit-magnitude y-offset 0.1
  ask turtles [ setxy (xcor - x-offset / 2) (ycor - y-offset / 2) ]
end

to-report limit-magnitude [number limit]
  if number > limit [ report limit ]
  if number < (- limit) [ report (- limit) ]
  report number
end
@#$#@#$#@
GRAPHICS-WINDOW
345
10
808
474
-1
-1
5.0
1
10
1
1
1
0
0
0
1
-45
45
-45
45
1
1
1
ticks
60.0

BUTTON
6
25
72
58
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
93
64
170
97
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
0

BUTTON
6
64
91
97
go-once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
237
100
316
145
# of nodes
count turtles
3
1
11

INPUTBOX
134
150
200
210
homophily
0.8
1
0
Number

MONITOR
191
294
341
339
Percentage of HUG scientists
count turtles with [group = 1] / count turtles
17
1
11

INPUTBOX
7
149
120
209
population_proportion
0.5
1
0
Number

MONITOR
233
165
318
210
NIL
count turtles
17
1
11

INPUTBOX
7
299
86
359
num-links
4.0
1
0
Number

INPUTBOX
7
223
95
283
num-scientists
100.0
1
0
Number

INPUTBOX
118
223
202
283
num-contests
25.0
1
0
Number

BUTTON
532
506
673
539
NIL
show heg-advantage
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
378
505
488
538
NIL
display-contest
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

This is a replication of a model from Rubin and Schneider's 2021 paper "Priority and Privilege in Scientific Discovery," in which they argue that the social dynamics of credit attribution in scientific communities can create structural disadvantages in socially diverse science.

The priority rule in science is the social norm that awards credit to the first scientist or group of scientists to make a discovery. However, Rubin and Schneider argue that beliefs by individual scientists about who made the first discovery depends on their position in a social network. If two agents make a discovery at the same time (which tends to be a common occurence in science), who ends up obtaining credit for the discovery may depend heavily on who they are connected to in the network. This model demonstrates how members of a historically under-represented group (HUG) may be less connected than their historically entrenched counterparts (HEG), and therefore unfairly harmed by the priority rule.


## HOW IT WORKS

The model begins with a number of fully connected scientists. Members of the HEG are represented by circles, while HUG members are represented by cubes.

At each step, a new scientist is added. The probability that this new scientist belongs to the HUG is determined by a logistic growth equation; as time progresses, this probability approaches the population proportion of the HUG. A new scientist randomly selects existing scientists to connect with, albeit with a bias. More specifically, a scientist's likelihood of being selected is directly proportional to its number of connections, or 'degree,' and its similarity to the new scientist.

Once the model includes more than 50 scientists, each new scientist has a 50% chance of connecting with all link-neighbors of the first scientist they choose to connect with. This scenario represents a scientific field that has become established and adopts practices for training new scientists. Additionally, once the network exceeds a specified size, older scientists 'retire,' along with all their connections.

To model an instance of simultaneous discovery and the subsequent attribution contest, one scientist from each group is randomly chosen (the selected HUG member turns blue, and the HEG member turns green). Then, at each time step, a randomly chosen scientist in the network turns either blue or green, depending on whether the closest non-red scientist is blue or green. This dynamic represents each scientist learning about a discovery from someone in the network who already holds a belief about the discoverer's identity. A group wins the attribution contest when a supermajority of scientists believe that a member of their group made the discovery.

The HEG advantage is calculated by running a specified number of contests and determining the difference between the proportion of contests won by an HEG member and those won by a HUG member.

## HOW TO USE IT

Pressing the GO ONCE button adds one new scientist.  To continuously add scientists, press GO.

The DISPLAY-CONTEST button runs an attribution contest.

The NUM-LINKS input determines the number of initial scientists and number of links a new scientist will create, minus the "advisors" it may also link to. 

The POPULATION-PROPORTION input determines the background societal proportion of HUG members.

The HOMOPHILY input determines how strongly new scientists consider similarity between themselves and target scientists when deciding their link-neighbors.

The NUM-SCIENTISTS input determines the greatest possible size of the scientific community.

Press the HEG-ADVANTAGE button to print the HEG advantage. The NUM-CONTESTS input determines the number of contests to run when determining the HEG advantage.



## THINGS TO TRY

Play with the inputs and compare your results with those of Rubin and Schneider's. Are they the same?

Advanced: Consider whether Rubin and Schneider's model assumptions accurately reflect real scientific communities. If not, change the process by which the network develops and contests are run to better reflect the real world. Do you still get the same results as Rubin and Schneider?  



## CREDITS AND REFERENCES

Rubin, H., & Schneider, M. D. (2021). Priority and privilege in scientific discovery. Studies in history and philosophy of science, 89, 202–211. https://doi.org/10.1016/j.shpsa.2021.08.005

The network mechanics of this model is based on:
Wilensky, U. (2005).  NetLogo Preferential Attachment model.  http://ccl.northwestern.edu/netlogo/models/PreferentialAttachment.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

	
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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Attribution Contest" repetitions="250" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="501"/>
    <metric>count turtles</metric>
    <metric>heg-advantage</metric>
    <runMetricsCondition>count turtles / 25 = 0</runMetricsCondition>
    <enumeratedValueSet variable="layout?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophily">
      <value value="0"/>
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
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
