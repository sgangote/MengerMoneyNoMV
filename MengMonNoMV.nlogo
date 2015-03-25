;;*****************************************************
;; Author: Santiago Gangotena
;; Created: 9/11/2014
;; Description: Dynamic Coordinative Dis-Equilibrium
;; 12 goods origin of money strategy
;; Last Updated: 20/3/2015
;; 
;; This version includes agg-xsdemand calculated as the
;; vector sum of individual xs-demand of turtles. This is wrong. Suppose there are 2 agents
;; and one good. Agent 1 has xs-demand 3 and agent 2 has xs-demand -3. If agg-xsdemamd is
;; calculated as the vector sum them the magnitude of agg-xsdemand = 0 leading to the impression
;; that agents don't want anything even if they havent traded! In reality agg-xsdemand should be
;; |agg-xsdemand| = |xs-demand1| + |xs-demand2| = 3 + 3 = 6 which means there is in the aggregate
;; xs-demand can be reduced by 6 units if agent 1 and agent 2 are able to trade, which is the whole
;; point of the model. The vector sum of individual xs-demands (agg-xsdemand will still be used 
;; because of the conventional definition) is still usefull because it give as lower bound to the 
;; maxmum possible gains from trade. The magnitude of xs-demands is a measure of how far away from 
;; acheiving satisfaction the agent currently is can be called catallactic unease/dissatisfaction/xs-demand
;; following Mises because it is a measure of the individual xs-demands that can be abated through
;; trade. The sum over all individuals of catallactic unease is then the a measure of the level of 
;; discoordination. Hence for the ith individual;
;; | xs-demand_i | = catallactic-unease_i and 
;; discoordination = SUM catallactic-unease_i over all i individuals.
;; And discoordination is bounded below by agg-xsdemand and above by the intial discoordination or the
;; discoordination at time 0.
;;
;; NOTE: 3/25/2015 This is the final version with no marginal valuation.
;; Backup version is "DCNE_MengMonUnifromTradeAttemptsNoMarginalValuation2.nlogo"
;;
;;
;;
;;*****************************************************
;;EXTENSIONS
extensions [ nw ]


;;#####################################
;;########## SETUP VARIABLES ##########
;;#####################################

;;###GLOBAL VARIABLES###
globals
[
  ;;initial aggregate xsdemand
  agg-xsdemand0
  ;;aggregate xs demand
  agg-xsdemand
  ;;global-strategy contour
  agg-moe-strategy-good
  agg-moe-strategy-vector
  ;;transactions
  agg-transactions 
  ;;initial level of discoordination (see above)
  discoordination0
  discoordination
  
  ;;want-discoordination
  ;;have-discoordination
  want-discoordination
  have-discoordination
  total-good0
]

;;###LINK VARIABLES###
links-own
[
  age 
]

to links-step
  if search-strategy != "random-partner"
  [ 
    ask links 
    [ 
      set age age + 1 
      if age > link-age-threshold
      [
        die 
      ]
    ]
  ]
end



;;###TURTLE VARIABLES###
turtles-own
[
  ;;preference parameters
  ;;consumption horizon determines how many periods a turtle will attempt to exchange goods
  ;;it is called consumption horizon because it is the maximum horizon for consumption of goods-stock
  ;;max-trade-attempts
  ;;per period consumption requirement
  goods-requirements  
  ;;production possibilities
  goods-endowment

  ;;state variables
  state
  transactions
  wants?
  has?
  not-traded?

  ;;control variables
  ;;stock of goods intitialized to random endowment
  goods-stock
  ;;xs-demand = goods-requirements - goods-stock
  xs-demand
  ;;the following depend on whether free disposal is assumed
  ;;both are calculated in the find-xsdemand proc
  ;;neg-xs-demand vector magnitude of goods in surplus
  ;;pos-xs-demand vector magnitude of goods in surplus
  neg-xs-demand
  pos-xs-demand
  ;;number of trades
  trade-attempts
  
  ;;trade-priorities of goods when attempting to exchange
  trade-priorities 
  
  ;;strategy variables
  moe-strategy-good
  moe-strategy-vector
  maxbid
  
  ;;strategy evaluation
  initial-mag-xs-demand
  final-mag-xs-demand
  lastperiod-change-xs-demand
  thisperiod-change-xs-demand
  accepted-moe
  gave-moe
  
  
  ;;network variables
  trading-partner
  potential-trading-partners
  
  ;;layout variables
  xcor0
  ycor0
  
]

;;############################
;;##### SETUP PROCEDURES #####
;;############################

;;###MAIN SETUP PROCEDURE###
to setup
  clear-all
  
  random-seed new-seed ;behaviorspace-run-number
  setup-turtles
  setup-globals
  reset-ticks
end

;;### TURTLE SETUP PROCEDURES ###
;;main turtles setup procedure
to setup-turtles
  set-default-shape turtles "person"
  crt population 
  [
    set size 2.2
    
    ;;setup endowments and preference parameters such that every turtle
    ;;wants at least some amount of one good and has a surplus of at least one good
    setup-goods-variables

    ;;state variables
    set state "I"
    set color red
    set transactions 0
    
    ;;strategy
    set trade-attempts 0
    set trading-partner Nobody
    set potential-trading-partners link-neighbors
    
    set lastperiod-change-xs-demand 0
  ]
  
  layout-circle turtles (world-width / 2 - 2)
  ask turtles
  [
   set xcor0 xcor
   set ycor0 ycor 
  ]
  
end

;;make sure that all agents have a deficit and a surplus of xs-demand for at least one good each
to setup-goods-variables
  ;;flags to check if there is a surplus or 
  ifelse (kiyotaki-wright)
  [
     let goods n-values num-of-goods [?]
     let wants one-of goods
     set goods remove wants goods
     let has one-of goods

     ifelse wants = has
     [
       show "++++++++++++ERROR+++++++++++++"
       stop
     ]
     [
       set goods-requirements n-values num-of-goods [ 0 ] 
       set goods-requirements replace-item wants goods-requirements 1
       set goods-endowment n-values num-of-goods [0]
       set goods-endowment replace-item has goods-endowment 1
       initialize-for-trade
       set xs-demand [] ;;n-values num-of-goods [0]
       set neg-xs-demand [] ;; n-values num-of-goods [0]
       set pos-xs-demand []
       find-xsdemand  
       ;;create list that orders the iteration of goods when exchanging
       set trade-priorities n-values num-of-goods [?]
       ;set trade-priorities shuffle trade-priorities
     ]
     
    
  ]
  ;;initialize such that there is an excess supply, and an excess demand of 
  [
    let badinit? True
    while [badinit?]
    [
      ;;flags to check for proper initialization
      let deficit-good? False
      let surplus-good? False
      
      ;;set possible preference and endowment parameters
      ;;preference parameters
      ;;homogenous consumption horizons for all turtles
      set goods-requirements n-values num-of-goods [random max-requirements]
      ;;set initial good endowments
      set goods-endowment n-values num-of-goods [random max-endowments]
      ;;intitialize goods-stock to goods-endowment
      set goods-stock n-values num-of-goods [0]
      initialize-for-trade
      set xs-demand [] ;;n-values num-of-goods [0]
      set neg-xs-demand [] ;; n-values num-of-goods [0]
      set pos-xs-demand []
      find-xsdemand
      ;;create list that orders the iteration of goods when exchanging
      set trade-priorities n-values num-of-goods [?]
      ;set trade-priorities shuffle trade-priorities
      
      ;;check if there is a surplus of at least one good and a deficit of at least one good   
      ;;if surplus and deficit is true then set badinit? to False
      if (has? and wants?)
      [
        set badinit? False 
      ]  
    ]
  ]
  
  ;;setup bucketbrigade strategy
  let temp magnitude pos-xs-demand
  set moe-strategy-vector n-values num-of-goods [ temp ]
  
end 

;;initialize goods-stock to goods-endowment, initialize trade-attempts
;;set trading-partner to nobody
to initialize-for-trade
  ;;variables
  set goods-stock goods-endowment
  set trade-attempts 0
  set trading-partner Nobody
  
  ;;search strategy
  if search-strategy = "random-partner"
  [
    ;;reset network when leaving state E
    ask my-links [die]
    return-to-coordinates0
  ]
  if search-strategy = "previous-partners"
  [
    ;;reset potential-trading-partners for nearest neighbor strategy
    set potential-trading-partners link-neighbors
  ]
  
  ;;indirect strategy
  set accepted-moe False
  set gave-moe False
end

;;compute xsdemands
to find-xsdemand
  ;;find excess demands
  set xs-demand (map - goods-requirements goods-stock)
  
  set neg-xs-demand []
  ;;create vector with only negative entries of xs-demand
  foreach xs-demand
  [
   ifelse (? < 0)
   [
    set neg-xs-demand lput ? neg-xs-demand 
   ]  
   [
     set neg-xs-demand lput 0 neg-xs-demand 
   ]
  ]
  
  set pos-xs-demand []
  ;;create vector with only positive entries of xs-demand
  foreach xs-demand
  [
   ifelse (? > 0)
   [
    set pos-xs-demand lput ? pos-xs-demand 
   ]  
   [
     set pos-xs-demand lput 0 pos-xs-demand 
   ]
  ]  
  
  ;;set wants? to TRUE if agent has non-zero pos-xs-demand
  ifelse ( magnitude pos-xs-demand > 0)
  [
    set wants? True
  ]
  [
    set wants? False
  ]
  
  ;;set wants? to TRUE if agent has non-zero neg-xs-demand
  ;;OR if agent has positive holding of good used for indirect exchange
  ifelse (magnitude neg-xs-demand > 0)
  [
    set has? True
  ]
  [
    set has? False
  ]  
end

;;procedure reports true if agent can trade and false if agent can't trade
to-report can-trade?
  ;;trade-attempts available?
  ifelse ((trade-attempts < max-trade-attempts) and wants?)
  [
    ifelse has?
    [
      report True
    ]
    [ 
      report False 
    ]
  ]
  [
    report False 
  ]
end

;;set transactions for all turtles to 0 and set not traded to True
;;for all turtles
to zero-transactions
  ask turtles [ set transactions 0 ] 
  ask turtles [ set not-traded? True ]
end

;;### GLOBAL SETUP PROCEDURES ###

;;main global variable setup
to setup-globals
  ;;any use to creating array of agg-demand composed 
  ;;individuals xs-demand vectors?
  ;;initialize agg-demand vector
  set agg-xsdemand n-values num-of-goods [0]
  ;;initialize agg count of turtles using each good as moe
  set agg-moe-strategy-good n-values num-of-goods [0]
  ;;find agg-xsdemand by summing xs-demand for each good over all turtles
  find-agg-data
  set agg-xsdemand0 agg-xsdemand
  set discoordination0 discoordination

end

;;find aggregate data
;;find aggregate data
to find-agg-data
  find-agg-xsdemand
  find-discoordination
  find-agg-transactions
  find-agg-moe-strategy-good
end

;;find the agg-xsdemand vector
to find-agg-xsdemand
  ask turtles [find-xsdemand]
  let i 0
  repeat num-of-goods
  [
   let agg-xsdemand-ithgood 0
   set agg-xsdemand-ithgood sum [item i xs-demand] of turtles
   set agg-xsdemand replace-item i agg-xsdemand agg-xsdemand-ithgood 
  ]
end

;;find the level of discoordination
to find-discoordination
  set discoordination sum [ magnitude xs-demand ] of turtles
  set want-discoordination sum [ magnitude pos-xs-demand ] of turtles
  set have-discoordination sum [ magnitude neg-xs-demand ] of turtles
end

;;find aggregate transactions for all turtles
to find-agg-transactions
  set agg-transactions (sum [transactions] of turtles) / 2
end

;;find aggregate moe strategy good
to find-agg-moe-strategy-good
  let i 0
  foreach agg-moe-strategy-good
  [
    let temp ( count turtles with [moe-strategy-good = i ] )
    set agg-moe-strategy-good replace-item i agg-moe-strategy-good temp
    set i i + 1
  ]
end


;;###############################################
;;############### GO PROCEDURES #################
;;###############################################


;;### MAIN GO PROCEDURE ###
to go
  zero-transactions
  turtles-step
  links-step
  find-agg-data
  tick
  if not all? turtles [ color = red ] [ layout ]
    ;;ask turtles [update-color]
end

;;#### TURTLES GO PROCEDURES ####

;;turtles go procedure
;;turtles update their state based on their information
;;then decide what state to go to next and formulate plans
to turtles-step
  ask turtles with [state = "I"]
  [
    rule1
  ]
  ask turtles with [state = "E"]
  [
    rule2
  ]
end

;;## GRAMMAR OF ACTION ##

;;rule1 maps from I to (I, E)
to rule1
  ;;initialize-for-trade
  ;;if agent wants something ( positive xs demand) and has something (negative xs-demand or moe)
  if (can-trade?)
  [
    ;;update state
    set state "E"
    update-color

    ;;initiate indirect exchange strategy
    if indirect-exchange
    [
      ;;set good for indirect exchange and collect info to update strategy
      choose-moe-strategy-good
    ]
  ]
  update-color
end

;;rule2 maps from E to (I, E)
to rule2
  ;;if trade attempts are available and can-trade?
  ifelse (can-trade?)
  [
    ;;search for a trading partner
    ;search-for-partner
    ;if (trading-partner != Nobody)
    ;[
      ;;try to exchange with partner
      exchange
      
      ;;if indirect exchange is on do barter-indirect-exchange
      ;;else just barter
 ;;     ifelse indirect-exchange
   ;;   [
     ;;   barter-indirect-exchange
      ;;]
      ;;[
        ;;barter
      ;;]
    ;]    
  ]
  ;;else go back to inactivity
  [
    if indirect-exchange 
    [
      evaluate-moe-strategy
    ]
    set state "I"
    initialize-for-trade
    update-color
  ]
end

;;## ACTION PROCEDURES ##

;;search for a trading partner according to search options
to search-for-partner
  
  ;;searches for a random partner that wants to exchange
  if search-strategy = "random-partner"
  [
    ;;set temp one-of other turtles with [(state = "E") and can-trade?]
    set trading-partner one-of other turtles with [(state = "E") and (trade-attempts < max-trade-attempts) and (not-traded?)] 
  ]
  
  ;;searches for one of the previous trading partners that wants to exchange
  ;;such that only one attempt per prvious trading partner while in state "E"
  ;;if there is no such partner seraches for a random trading partner
  if search-strategy = "previous-partners"
  [
    ;;try to trade with someone I have traded before
    set trading-partner one-of potential-trading-partners with [(state = "E") and (trade-attempts < max-trade-attempts) and (not-traded?)]
    
    ;;if there are no trading partners search for a new random partner
    ifelse trading-partner = NOBODY
    [
      ;;find a random trading partner
      set trading-partner one-of other turtles with [(state = "E") and (trade-attempts < max-trade-attempts) and (not-traded?)]
    ]
    [
      set potential-trading-partners potential-trading-partners with [self != trading-partner]
    ]
  ]
  
  ;;searches for trading partners based on their degree (how many links are attached to them)
  if search-strategy = "preferential-attachment"
  [
    ;;if there are no links search for a random partner
    ifelse count links = 0
    [
      set trading-partner one-of other turtles with [(state = "E") and (trade-attempts < max-trade-attempts) and (not-traded?)]
    ]
    [
      let temp [one-of both-ends with [(state = "E") and (trade-attempts < max-trade-attempts) and (not-traded?)] ] of one-of links
      ifelse temp = NOBODY
      [
        set trading-partner one-of other turtles with [(state = "E") and (trade-attempts < max-trade-attempts) and (not-traded?)] 
      ]
      [
        set trading-partner temp
      ]
    ]
  ]
  
  ;;mixed strategy where partner searches if possible via preferential attachment but first attempts to trade with previous partners
  if search-strategy = "preferential-habit"
  [
    ;;try to trade with someone I have traded before
    set trading-partner one-of potential-trading-partners with [(state = "E") and (trade-attempts < max-trade-attempts) and (not-traded?)]
    
    ;;if there are no trading partners search for a new random partner using preferential attachment search
    ifelse trading-partner = NOBODY
    [
      ;;find a trading partner proportional to the number of links (pref. attachment)
      
      ifelse count links = 0
      [
        set trading-partner one-of other turtles with [(state = "E") and (trade-attempts < max-trade-attempts) and (not-traded?)]
      ]
      [
        let temp [one-of both-ends with [(state = "E") and (trade-attempts < max-trade-attempts) and (not-traded?)] ] of one-of links
        ifelse temp = NOBODY
        [
          set trading-partner one-of other turtles with [(state = "E") and (trade-attempts < max-trade-attempts) and (not-traded?)] 
        ]
        [
          set trading-partner temp
        ]
      ]
    ]
    [
      ;;if there are previous trading partners update list to exclude partner that I just traded with
      set potential-trading-partners potential-trading-partners with [self != trading-partner]
    ]
    
    
  ]
  
end


;;##################################
;;###### EXCHANGE PROCEDURES #######
;;##################################

;;master exchange procedure
to exchange
  
  search-for-partner
  ifelse (trading-partner != Nobody )  
  [
    ;;show "##### BEGIN EXCHANGE ATTEMPT ######"
    log-turtle "start-exchange"
    
    ;;update trade attempts for both agents
    ;;show trade-attempts
    set trade-attempts trade-attempts + 1
    ;;$$$$$$$ TURN THIS ON FOR INTERESTING EFFECTS OF ASYNCHRONICITY $$$$$$$$$$$
    ask trading-partner [ set trade-attempts trade-attempts + 1 ]
    set not-traded? False
    ask trading-partner [ set not-traded? False ]
    
    ;;see what trading partner has (local copy of list for faster processing)
    let other-xs-demand [xs-demand] of trading-partner
    let other-goods-stock [goods-stock] of trading-partner
    let other-trade-priorities [trade-priorities] of trading-partner
    let other-moe-strategy-good [moe-strategy-good] of trading-partner
    let other-trade-attempts [trade-attempts] of trading-partner
    
    ;;flag for type of exchange
    let exchange-type "None"
    
    ;;while wecantrade? is true keep on trying to trade for 1A
    let wecantrade? True
    while [ wecantrade? ]
    [
      ;;for a single item for item exchange ( i gets good k and gives good l)
      let gives-good -1
      let gets-good -1
      
      ;;set gets-good-want to what does he have that i want
      set gets-good (haswhatiwant-good xs-demand other-xs-demand trade-priorities)
      
      ;;if he has what i want
      ifelse (gets-good >= 0)
      [
        ;show word "I want and he has good " gets-good-want
        ;;set gives-good to what does he want that i have?
        set gives-good (haswhatiwant-good other-xs-demand xs-demand other-trade-priorities )
        ;;if i have what he wants
        ifelse (gives-good >= 0 );;and gives-good != gets-good)
        [
          ;;set exchange type
          set exchange-type "1A"
          
          ;;gets (what I want)
          let a item gets-good xs-demand
          let b (abs ( item gets-good other-xs-demand ) )
          let temp1 list a b 
          let gets min temp1
          set goods-stock replace-item gets-good goods-stock ((item gets-good goods-stock) + gets)
          set other-goods-stock replace-item gets-good other-goods-stock ((item gets-good other-goods-stock) - gets) 
          
          ;;gives (what partner wants)
          let c (abs ( item gives-good xs-demand) ) 
          let d item gives-good other-xs-demand
          let temp2 list c d 
          let gives min temp2
          set goods-stock replace-item gives-good goods-stock ((item gives-good goods-stock) - gives)
          set other-goods-stock replace-item gives-good other-goods-stock ((item gives-good other-goods-stock) + gives)
          
          ;;find new xs-demands
          find-xsdemand
          ask trading-partner [ set goods-stock other-goods-stock]
          ask trading-partner [ find-xsdemand ]
          ;;reset local copies to reflect trade
          set other-xs-demand [xs-demand] of trading-partner

          
          ;;show trade of turtle 0
          if log-turtle-0?
          [    
            show-trade gets-good gives-good temp1 temp2 gets gives exchange-type
          ]
          
          ;;create link with trading-partner
          create-link-with trading-partner
          ask link-with trading-partner [set age 0]
          set transactions transactions + 1
          ask trading-partner [ set transactions transactions + 1] 
          
          ;;for 1A exchanges no one can accept moe but agent may give moe if he had accepted moe before
          if (accepted-moe and gives-good = moe-strategy-good )
          [
            set gave-moe True
          ]
          if ([accepted-moe] of trading-partner and gets-good = other-moe-strategy-good)
          [
            ask trading-partner [set gave-moe True] 
          ]
        ]
        [
          set wecantrade? False 
          ;show "I dont have what he wants"
          ;show word "we can trade? " wecantrade?
        ]
      ]
      [
        set wecantrade? False 
        ;show "He doesn't have what i want"
        ;show word"we can trade? " wecantrade?
      ]
    ]
    
    ;;check for 1B, 2A and 2B indirect exchange
    if indirect-exchange
    [
      ;;1B trade
      let gets-good-wants -1 
      let gives-good-WTA -1 
      set gets-good-wants (haswhatiwant-good xs-demand other-xs-demand trade-priorities)
      set gives-good-WTA (haswhatiWTA xs-demand other-moe-strategy-good other-trade-attempts)
      
      ;;if 1B is possible carry it out
      if (gets-good-wants >= 0 and gives-good-WTA >= 0)
      [ 
        ;;set exchange type
        set exchange-type "1B"
        
        ;;gets (what I wants)
        let a item gets-good-wants xs-demand
        let b (abs ( item gets-good-wants other-xs-demand ) )
        let temp1 list a b 
        let gets min temp1
        set goods-stock replace-item gets-good-wants goods-stock ((item gets-good-wants goods-stock) + gets)
        set other-goods-stock replace-item gets-good-wants other-goods-stock ((item gets-good-wants other-goods-stock) - gets) 
        
        ;;gives (what partner is WTA)
        let c (abs (item gives-good-WTA xs-demand))
        let d item gives-good-WTA other-xs-demand
        let temp2 list c d
        let gives c ;;partner is WTA all of the xs-supply of gives-good-WTA
        set goods-stock replace-item gives-good-WTA goods-stock ((item gives-good-WTA goods-stock) - gives)
        set other-goods-stock replace-item gives-good-WTA other-goods-stock ((item gives-good-WTA other-goods-stock) + gives)
        
        ;;find new xs-demands
        find-xsdemand
        ask trading-partner [ set goods-stock other-goods-stock]
        ask trading-partner [ find-xsdemand ]
        ;;reset local copies to reflect trade
        set other-xs-demand [xs-demand] of trading-partner

        
        ;;show trade of turtle 0
        if log-turtle-0?
        [    
          show-trade gets-good-wants gives-good-WTA temp1 temp2 gets gives exchange-type
        ]
        
        ;;create link with trading-partner
        create-link-with trading-partner  
        ask link-with trading-partner [set age 0]
        set transactions transactions + 1
        ask trading-partner [ set transactions transactions + 1]
        
        ;;i could have given moe
        if (accepted-moe and gives-good-WTA = moe-strategy-good )
        [
          set gave-moe True
        ]
        ;;if exchange was 1B trading partner accepted MOE (thus could not have given MOE)
        ask trading-partner [ set accepted-moe True ] 
      ]
      
      ;;2A trade
      let gets-good-WTA -1
      let gives-good-wants -1
      set gets-good-WTA (haswhatiWTA other-xs-demand moe-strategy-good trade-attempts)
      set gives-good-wants (haswhatiwant-good other-xs-demand xs-demand trade-priorities) 
      
      ;;if 2A is possible carry it out
      if (gets-good-WTA >= 0 and gives-good-wants >= 0)
      [
        ;;set exchange type
        set exchange-type "2A"
        
        ;;gets (what I am WTA)
        let a (item gets-good-WTA xs-demand)
        let b (abs (item gets-good-WTA other-xs-demand))
        let temp1 list a b
        let gets b
        set goods-stock replace-item gets-good-WTA goods-stock ((item gets-good-WTA goods-stock) + gets)
        set other-goods-stock replace-item gets-good-WTA other-goods-stock ((item gets-good-WTA other-goods-stock) - gets)
        
        ;;gives (what partner wants)
        let c (abs ( item gives-good-wants xs-demand) ) 
        let d item gives-good-wants other-xs-demand
        let temp2 list c d 
        let gives min temp2
        set goods-stock replace-item gives-good-wants goods-stock ((item gives-good-wants goods-stock) - gives)
        set other-goods-stock replace-item gives-good-wants other-goods-stock ((item gives-good-wants other-goods-stock) + gives)
        
        ;;find new xs-demands
        find-xsdemand
        ask trading-partner [ set goods-stock other-goods-stock]
        ask trading-partner [ find-xsdemand ]
        ;;reset local copies to reflect trade
        set other-xs-demand [xs-demand] of trading-partner

        
        ;;show trade of turtle 0
        if log-turtle-0?
        [    
          show-trade gets-good-WTA gives-good-wants temp1 temp2 gets gives exchange-type
        ]
        
        ;;create link with trading-partner
        create-link-with trading-partner  
        ask link-with trading-partner [set age 0]
        set transactions transactions + 1
        ask trading-partner [ set transactions transactions + 1] 
        
        ;;if exchange was 2A i accepted MOE (could not have given MOE)
        set accepted-moe True
        ;;trading partner could have given MOE
        if ([accepted-moe] of trading-partner and gets-good-WTA = other-moe-strategy-good)
        [
          ask trading-partner [set gave-moe True] 
        ]
      ]
      
      
      ;;NOTE*** Vernon Smith's finding that commodity trading is highly volatile falls under 2B types 
      ;;of exchanges, when these types of exchanges are absent (convergence to a moe) then the volatility
      ;;in trading should be low. Hence the question of how one moe stops being widely accepted or changes 
      ;;to another moe are what lead to high volatility in certain commodities. As a certain good goes into
      ;;(and out of) being used as a mengerian commodity the volatility in price increases (and decreases) 
      ;;because 1) the number of transactions in this commodity increases (decreases) and the exchange value
      ;;of the commodity increases and then decreases as demand and supply shift.
      
      ;;2B trade
      ;;first check if agents are using different moe-strategy
      if (moe-strategy-good != other-moe-strategy-good)
      [
        ;;2B trade
        set gets-good-WTA (haswhatiWTA other-xs-demand moe-strategy-good trade-attempts)
        set gives-good-WTA (haswhatiWTA xs-demand other-moe-strategy-good other-trade-attempts)
        
        ;;if 2B is possible carry it out
        if (gets-good-WTA >= 0 and gives-good-WTA >= 0)
        [
          ;;set exchange type
          set exchange-type "2B"
          
          ;;gets (what I am WTA)
          let a (item gets-good-WTA xs-demand)
          let b (abs (item gets-good-WTA other-xs-demand))
          let temp1 list a b
          let gets b
          set goods-stock replace-item gets-good-WTA goods-stock ((item gets-good-WTA goods-stock) + gets)
          set other-goods-stock replace-item gets-good-WTA other-goods-stock ((item gets-good-WTA other-goods-stock) - gets)
          
          ;;gives (what partner is WTA)
          let c (abs (item gives-good-WTA xs-demand))
          let d item gives-good-WTA other-xs-demand
          let temp2 list c d
          let gives c ;;partner is WTA all of the xs-supply of gives-good-WTA
          set goods-stock replace-item gives-good-WTA goods-stock ((item gives-good-WTA goods-stock) - gives)
          set other-goods-stock replace-item gives-good-WTA other-goods-stock ((item gives-good-WTA other-goods-stock) + gives)
          
          ;;find new xs-demands
          find-xsdemand
          ask trading-partner [ set goods-stock other-goods-stock]
          ask trading-partner [ find-xsdemand ]
          ;;reset local copies to reflect trade
          set other-xs-demand [xs-demand] of trading-partner

          
          ;;show trade of turtle 0
          if log-turtle-0?
          [    
            show-trade gets-good-WTA gives-good-WTA temp1 temp2 gets gives exchange-type
          ]
          
          ;;create link with trading-partner
          create-link-with trading-partner  
          ask link-with trading-partner [set age 0]
          set transactions transactions + 1
          ask trading-partner [ set transactions transactions + 1] 
          
          ;;if exchange was 2B trading partner and i both accepted MOE
          if (accepted-moe and gives-good-WTA = moe-strategy-good )
          [
            set gave-moe True
          ]
          if ([accepted-moe] of trading-partner and gets-good-WTA = other-moe-strategy-good)
          [
            ask trading-partner [set gave-moe True] 
          ]
          ask trading-partner [ set accepted-moe True ]
          set accepted-moe True
          
          
        ]
      ]  
    ]
    
    ;show "#####  END EXCHANGE ATTEMPT  ######"
    log-turtle "end-exchange"
    
  ]
  [
    ;show "No trading partner found" 
    
  ]  
end

;;check if he has what i am WTA and returns if it has at least one more trade attempt available
to-report haswhatiWTA [other-xsdemand moe-good attempts]
  ;;what are his hodlings of my moe-strategy-good
  let temp item moe-good other-xsdemand
  
  ;;if he has my moe-strategy-good and i have trade attempts 
  ifelse ( temp < 0 ) and ( attempts < (max-trade-attempts) )
  [
    report moe-good
  ]
  [
    report -1
  ]
end

;;inputs are two excess demand lists and an order of iteration list
;;returns the index of the good such that there is a positive excess demand (want) of first list
;;and negative excess demand (excess supply) of the second list iterating in the order of iteration list
to-report haswhatiwant-good [ my-xsdemand other-xsdemand order-of-goods]
  foreach order-of-goods
  [
    if ((item ? my-xsdemand > 0) and (item ? other-xsdemand < 0))
    [
      report ?
    ] 
  ]
  report -1
end

;;Check to see if he haswhatiwant. Takes two lists: if my-xsdemand for the ith good is
;;is negative (I want the ith good) and other-xsdemand is positive
;;(he has the ith good) reports true, otherwise reports false
to-report haswhatiwant [my-xsdemand other-xsdemand] 
  let haswhatiwant? false 
  ;;iterate through each element of both lists and check for haswhatiwant condition
  (
    foreach my-xsdemand other-xsdemand
    [    
      ;;show ?1
      ;;show ?2
      if ((?1 > 0) and (?2 < 0))
      [
        set haswhatiwant? true
        ;;show haswhatiwant?
        report haswhatiwant?
      ]    
    ] 
    )
  ;;report haswhatiwant?
  report False
end


;;######################################
;;###### MOE STRATEGY PROCEDURES #######
;;######################### #############

;; choose moe-strategy-good through bidding based on strategy stregth
to choose-moe-strategy-good
  ifelse strategy-convergence
  [
    ;;set for convergence on good 0 to be used for indirect xchange
    set moe-strategy-good 0
  ]
  [
    ;let bid 0
    let bids n-values num-of-goods [ random-float ( item ? moe-strategy-vector) ]
    set maxbid 0
    
    ;;find max bid
    set maxbid max bids
    ;;find position of maxbid
    let pos-maxbid "None"
    set pos-maxbid position maxbid bids
    set moe-strategy-good pos-maxbid
  ]
  
  ;;save initial magnitude of xs-demand for strategy evaluation
  find-xsdemand
  set initial-mag-xs-demand (magnitude pos-xs-demand)
  
  log-turtle "choosing-moe"
end

;;evaluate and update strategy strength
to evaluate-moe-strategy
  
  ;;find reduction in xs-demand after trade
  find-xsdemand
  set final-mag-xs-demand (magnitude pos-xs-demand)
  set thisperiod-change-xs-demand ( final-mag-xs-demand - initial-mag-xs-demand )
  
  let used-moe False
  if (accepted-moe and gave-moe)
  [
    set used-moe True
    ;show "USED MOE" 
  ]
  
  ;;pay the strategy if it helped get more wants if and only if strategy helped reduce. Otherwise give back money to strategy
  if used-moe
  [

    set moe-strategy-vector replace-item moe-strategy-good moe-strategy-vector ( (item moe-strategy-good moe-strategy-vector) - maxbid)
    
    if (thisperiod-change-xs-demand < 0)
    [
      let payment-to-strategy maxbid * return-to-strategy
      let temp (item moe-strategy-good moe-strategy-vector)       
      let temp2 temp + payment-to-strategy + (abs (thisperiod-change-xs-demand))
      set moe-strategy-vector replace-item moe-strategy-good moe-strategy-vector temp2;( (item moe-strategy-good moe-strategy-vector) + payment-to-strategy)   
      ;show "USED MOE SUCCESFULLY!!!"
      ;show word "paid:      " maxbid
      ;show word "received:  " payment-to-strategy
      ;show word "strength:  " moe-strategy-vector
      ;show word "  " magnitude pos-xs-demand
    ]

  ]
  
  log-turtle "evaluating-moe"
end


;;###############################################
;;############## LAYOUT PROCEDURES ##############
;;###############################################

;;layout for links
to layout
  repeat 10 [
    do-layout
    display  ;; so we get smooth animation
  ]
end

to do-layout
  ;;layout-radial turtles links (turtle 0)
  layout-spring (turtles with [any? link-neighbors]) links 0.4 10 10
end

;;return turtle to original coordinates
to return-to-coordinates0
  set xcor xcor0
  set ycor ycor0
end

;;take turtle with highest degree centrality and graph around it
to graph-centrality
  layout-radial turtles links (central-turtle)
end

;;report turtle with max betweenness-centrality
to-report central-turtle
   let b-centrality []
   set b-centrality [nw:betweenness-centrality] of turtles
   let max-central max b-centrality
   let max-central-turtle one-of turtles with [nw:betweenness-centrality = max-central]    
   report max-central-turtle
end

;;color turtles according to state
to update-color
  ifelse color-by-MOE?
  [
    if ( state = "I") 
    [
      set color (  (10 * moe-strategy-good)  + 15 )
    ]
    if (state = "E")
    [
      set color ( (10 * moe-strategy-good) + 15) 
    ]
  ]
  ;;color by state with no distinction to medium of exchange being used
  [
    if ( state = "I")
    [
      set color red
    ]
    if ( state = "E")
    [
      set color yellow
    ]
  ]
end

to kiyotaki-wright-color
;want 0 but have 1
ask turtles with [ item 0 goods-requirements > 0 and item 1 goods-endowment > 0] [ set color white ]
;want 1 but have 0
ask turtles with [ item 1 goods-requirements > 0 and item 0 goods-endowment > 0] [ set color white ]

;want 0 but have 2
ask turtles with [ item 0 goods-requirements > 0 and item 2 goods-endowment > 0] [ set color green ]
;want 2 but have 0
ask turtles with [ item 2 goods-requirements > 0 and item 0 goods-endowment > 0] [ set color green ]

;want 1 but have 2
ask turtles with [ item 1 goods-requirements > 0 and item 2 goods-endowment > 0] [ set color blue ]
;want 2 but have 1
ask turtles with [ item 2 goods-requirements > 0 and item 1 goods-endowment > 0] [ set color blue ]
end

;;###############################################
;;###### DEBUGGING AND GENERAL PROCEDURES #######
;;###############################################

;;reports the magnitude of a vector
to-report magnitude [a]
  ;;first find (a dot a) with map then add all the elements of (a dot a) with reduce
  report sqrt ( reduce + (map [? * ?] a) )
end

;;equivalent to str representation of agent in python
to show-turtle-info
  show word "I am turtle............. " who
  show word "My trade attempts are... " trade-attempts
  show word "My  goods-stock......... " goods-stock
  show word "My  goods-requirements.. " goods-requirements
  find-xsdemand
  show word "My  xs-demand........... " xs-demand
  show word "My trade priorityes..... " trade-priorities
  show word "My  |xs-demand| = " magnitude xs-demand
  show word "My indirect xchange strategy...  " moe-strategy-good
  ;;if has a partner show partner info as well
  if trading-partner != Nobody 
  [
    show word "Trading partner is...... " [who] of trading-partner
    show word "His trade attempts are.. " [trade-attempts] of trading-partner 
    show word "His goods-stock......... " [goods-stock] of trading-partner
    show word "His goods-requirements.. " [goods-requirements] of trading-partner
    ask trading-partner [find-xsdemand]
    show word "His xs-demand........... " [xs-demand] of trading-partner
    show word "His trade priorityes.... " [trade-priorities] of trading-partner
    show word "His |xs-demand| = " magnitude [xs-demand] of trading-partner
    show word "His indirect xchange strategy...  " [moe-strategy-good] of trading-partner
        
    ifelse ((haswhatiwant xs-demand [xs-demand] of trading-partner ) and (haswhatiwant [xs-demand] of trading-partner xs-demand))
    [
      show "$$$$$$$$$$$$$$$_____DOUBLE COINCIDENCE____$$$$$$$$$$$$$$$$$$$"
    ]
    [
      show "NO DOUBLE COINDCIDENCE"
      if (haswhatiwant xs-demand [xs-demand] of trading-partner) and (item ([moe-strategy-good] of trading-partner) xs-demand < 0)
      [
        show "$$$$___INDIRECT EXCHANGE POSSIBLE___$$$" 
      ]
    ]
  ]
end


;;log output to track turtle 0
to log-turtle [ section ]
  if log-turtle-0?
  [
    if section = "choosing-moe"
    [
      if who = 0
      [
        show      "================================================================"
        show word "======================== CHOOSING MOE GOOD =====================______START_____ " ticks
        show      "================================================================"
        show word "moe-strategy-good is:    " moe-strategy-good
        show word "xs-demand of moe:        " item moe-strategy-good xs-demand
        show word "goods-stock of moe:      " item moe-strategy-good goods-stock
        show word "consumption-req of moe:  " item moe-strategy-good goods-requirements
        show word "initial-mag-xs-demand    " initial-mag-xs-demand
        show      "================================================================"
        show      "================================================================"
      ] 
    ]
    if section = "start-exchange"
    [
      if who = 0
      [
        show word "$$$$$$$$$$$$$$$$$$$$$ BEGIN EXCHANGE ATTEMPT $$$$$$$$$$$$$$$$$$$______START_____ " ticks
        show word "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ TRADE-ATTEMPTS_____ " trade-attempts
        show      " "
        show word "TRADE ATTEMPT OF TURTLE: " who 
        show word "Tick: " ticks
        show      "****************************************************************"
        show      "********************* BEFORE TRADE ATTEMPT *********************"
        show      "****************************************************************"
        show-turtle-info 
        show      "****************************************************************"
        show      "********************* BEFORE TRADE ATTEMPT *********************"
        show      "****************************************************************"
        show      "----------------------------------------------------------------"
      ]
      if [who] of trading-partner = 0
      [
        show word "$$$$$$$$$$$$$$$$$$$$$ BEGIN EXCHANGE ATTEMPT $$$$$$$$$$$$$$$$$$$______START_____ " ticks
        show word "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ TRADE-ATTEMPTS_____ " [trade-attempts] of trading-partner
        show      " "
        show word "TRADE ATTEMPT OF TURTLE: " who 
        show word "Tick: " ticks
        show      "****************************************************************"
        show      "********************* BEFORE TRADE ATTEMPT *********************"
        show      "****************************************************************"
        show-turtle-info 
        show      "****************************************************************"
        show      "********************* BEFORE TRADE ATTEMPT *********************"
        show      "****************************************************************"
        show      "----------------------------------------------------------------"
      ] 
    ]
   
    if section = "end-exchange"
    [
      if who = 0
      [
        show      "****************************************************************"
        show      "********************* AFTER TRADE ATTEMPT  *********************"
        show      "****************************************************************"
        show-turtle-info 
        show      "****************************************************************"
        show      "********************* AFTER TRADE ATTEMPT  *********************"
        show      "****************************************************************"
        show      " "
        show word "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ CAN-TRADE?_________" can-trade?
        show      "$$$$$$$$$$$$$$$$$$$$$ END EXCHANGE ATTEMPT $$$$$$$$$$$$$$$$$$$$$______END"
      ]
      if [who] of trading-partner = 0
      [
        show      "****************************************************************"
        show      "********************* AFTER TRADE ATTEMPT  *********************"
        show      "****************************************************************"
        show-turtle-info 
        show      "****************************************************************"
        show      "********************* AFTER TRADE ATTEMPT  *********************"
        show      "****************************************************************"
        show      ""
        show word "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ CAN-TRADE?_________" [can-trade?] of trading-partner
        show      "$$$$$$$$$$$$$$$$$$$$$ END EXCHANGE ATTEMPT $$$$$$$$$$$$$$$$$$$$$______END" 
      ] 
    ]  
    
    if section = "evaluating-moe"
    [
      if who = 0
      [
        show      "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
        show word "+++++++++++++++++++++++  EVALUATING MOE  +++++++++++++++++++++++______START_____ " ticks
        show      "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
        show word "moe-strategy-good is:        " moe-strategy-good
        show word "xs-demand of moe:            " item moe-strategy-good xs-demand
        show word "goods-stock of moe:          " item moe-strategy-good goods-stock
        show word "consumption-req of moe:      " item moe-strategy-good goods-requirements
        show word "final-mag-xs-demand          " final-mag-xs-demand
        show word "initial-mag-xs-demand        " initial-mag-xs-demand
        show word "thisperiod-change-xs-demand  " thisperiod-change-xs-demand
        show      "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
        show      "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
      ] 
    ]
  ]   
end

to show-trade [gets-good gives-good temp1 temp2 gets gives exchange-type]  
  if ((who = 0) or (([who] of trading-partner) = 0))
  [
    if exchange-type = "None"
    [
      show "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!ERROR!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    ]
    if exchange-type = "1A"
    [
      show "DIRECT EXCHANGE 1A POSSIBLE"
      show      "%%%%%%%%%%%%%%%%%%%"
      show word "gets good   " gets-good
      show word "temp 1      " temp1 
      show word "gets        " gets
      show word "gives good  " gives-good
      show word "temp 2      " temp2
      show word "gives       " gives
      show      "%%%%%%%%%%%%%%%%%%%"
    ]  
    if exchange-type = "1B"
    [
      show "INDIRECT EXCHANGE 1B POSSIBLE"  
      show      "%%%%%%%%%%%%%%%%%%%%%%%"
      show word "gets good wants   " gets-good
      show word "temp 1            " temp1 
      show word "gets              " gets
      show word "gives good WTA    " gives-good
      show word "temp 2            " temp2
      show word "gives             " gives
      show      "%%%%%%%%%%%%%%%%%%%%%%%"
    ]
    if exchange-type = "2A"
    [
      if who = 0
      [
        show " "
        show " "
        show " "
        show " "
        show " "
        show " "
        show " "
        show " "
        show " "
        show " "
        show " "
        show " "
         
      ]
      show "INDIRECT EXCHANGE 2A POSSIBLE"  
      show      "%%%%%%%%%%%%%%%%%%%%%%%"
      show word "gets good WTA    " gets-good
      show word "temp 1           " temp1 
      show word "gets             " gets
      show word "gives good wants " gives-good
      show word "temp 2           " temp2
      show word "gives            " gives
      show      "%%%%%%%%%%%%%%%%%%%%%%%"
    ]
    if exchange-type = "2B"
    [
      show "INDIRECT EXCHANGE 2B POSSIBLE"  
      show      "%%%%%%%%%%%%%%%%%%%%%%%"
      show word "gets good WTA  " gets-good
      show word "temp 1         " temp1 
      show word "gets           " gets
      show word "gives good WTA " gives-good
      show word "temp 2         " temp2
      show word "gives          " gives
      show      "%%%%%%%%%%%%%%%%%%%%%%%"
    ]
  ]     
end

;;indirect-xchange-test
to indirect-xchange-test

  ;;indirect exchange tests

  if test? = "1B"
  [
    ;;set up no double coincidence possible
    ask turtle 0 [ set goods-requirements [ 1 0 0 ] set goods-endowment [ 0 1 0 ] ]
    ask turtle 1 [ set goods-requirements [ 0 1 0 ] set goods-endowment [ 0 0 1 ] ]
    ask turtle 2 [ set goods-requirements [ 0 0 1 ] set goods-endowment [ 1 0 0 ] ]
    ask turtles [set goods-stock goods-endowment]
    ask turtles [show-turtle-info]
    ;;go to active state
    ask turtles [rule1]
    ask turtles [ set moe-strategy-good 2]
    ;;turtle 1 and turtle 0 trade 1B
    ask turtle 1 [ set trading-partner turtle 0]
    ask turtle 1 [exchange]
    ;;turtle 0 and turtle 2 direct exchange
    ;ask turtle 0 [set trading-partner turtle 2]
    ;ask turtle 0 [exchange]
    ask turtle 2 [set trading-partner turtle 0]
    ask turtle 2 [exchange]
  ]
  if test? = "2A"
  [
    ;;set up no double coincidence possible
    ask turtle 0 [ set goods-requirements [ 1 0 0 ] set goods-endowment [ 0 1 0 ] ]
    ask turtle 1 [ set goods-requirements [ 0 1 0 ] set goods-endowment [ 0 0 1 ] ]
    ask turtle 2 [ set goods-requirements [ 0 0 1 ] set goods-endowment [ 1 0 0 ] ]
    ask turtles [ set goods-stock goods-endowment]
    ask turtles [show-turtle-info]
    ;;go to active state
    ask turtles [rule1]
    ask turtles [ set moe-strategy-good 2]
    ;;turtle 1 and turtle 0 trade 1B
    ask turtle 0 [ set trading-partner turtle 1]
    ask turtle 0 [exchange]
    ;;turtle 0 and turtle 2 direct exchange
    ask turtle 0 [set trading-partner turtle 2]
    ask turtle 0 [exchange]
    ;ask turtle 2 [set trading-partner turtle 0]
    ;ask turtle 2 [exchange]
  ]
  if test? = "2B"
  [
    ask turtle 0 [ set goods-requirements [ 1 0 0 ] set goods-endowment [ 0 1 0 ] ]
    ask turtle 1 [ set goods-requirements [ 1 0 0 ] set goods-endowment [ 0 1 1 ] ]
    ask turtle 2 [ set goods-requirements [ 0 0 1 ] set goods-endowment [ 1 0 0 ] ]
    ask turtles [set goods-stock goods-endowment]
    ask turtles [show-turtle-info]
    ;;go to active state
    ask turtles [rule1]
    ask turtle 0 [ set moe-strategy-good 2 ]
    ask turtle 1 [ set moe-strategy-good 1 ]
    ask turtle 2 [ set moe-strategy-good 2 ]
    ;;turtle 0 and turtle 1 trade 2B
    ask turtle 0 [set trading-partner turtle 1]
    ask turtle 0 [exchange]
    ;;turtle 0 and turtle 2 direct exchange
    ask turtle 0 [set trading-partner turtle 2]
    ask turtle 0 [exchange]
  ]
  
  ;;kiyotaki-wright convergence test
  if test? = "Kiyotaki-Wright"
  [
    set population 3
    setup
    ;;set up no double coincidence possible
    ask turtle 0 [ set goods-requirements [ 1 0 0 ] set goods-endowment [ 0 1 0 ] ]
    ask turtle 1 [ set goods-requirements [ 0 1 0 ] set goods-endowment [ 0 0 1 ] ]
    ask turtle 2 [ set goods-requirements [ 0 0 1 ] set goods-endowment [ 1 0 0 ] ]
    ask turtles [ set goods-stock goods-endowment]
    ask turtles [show-turtle-info]
  ]
end

;;######################################
;;########### LEFT-OVER CODE ###########
;;######################################

;;master exchange procedure
to exchange-direct-only
  
  ;;show "##### BEGIN EXCHANGE ATTEMPT ######"
  log-turtle "start-exchange"
 
  ;;update trade attempts for both agents
  ;;show trade-attempts
  set trade-attempts trade-attempts + 1
  ;;$$$$$$$ TURN THIS ON FOR INTERESTING EFFECTS OF ASYNCHRONICITY $$$$$$$$$$$
  ask trading-partner [ set trade-attempts trade-attempts + 1 ]
  set not-traded? False
  ask trading-partner [ set not-traded? False ]
  
  ;;see what trading partner has (local copy of list for faster processing)
  let other-xs-demand [xs-demand] of trading-partner
  let other-goods-stock [goods-stock] of trading-partner
  let other-trade-priorities [trade-priorities] of trading-partner
  let other-moe-strategy-good [moe-strategy-good] of trading-partner
  let other-trade-attempts [trade-attempts] of trading-partner
  
  ;;flag for type of exchange
  let exchange-type "None"
  
  ;;while wecantrade? is true keep on trying to trade
  let wecantrade? True
  while [ wecantrade? ]
  [
    ;;for a single item for item exchange ( i gets good k and gives good l)
    let gives-good -1
    let gets-good-want -1
    
    ;;set gets-good-want to what does he have that i want
    set gets-good-want (haswhatiwant-good xs-demand other-xs-demand trade-priorities)
    
    ;;if he has what i want
    ifelse (gets-good-want >= 0)
    [
      ;show word "I want and he has good " gets-good-want
      ;;set gives-good to what does he want that i have?
      set gives-good (haswhatiwant-good other-xs-demand xs-demand other-trade-priorities )
      ;;if i have what he wants
      ifelse (gives-good >= 0 );;and gives-good != gets-good)
      [
        ;show word "He wants and I have good " gives-good
        ;;WE CAN TRADE!
        ;show word"we can trade!? " wecantrade?
        ;;i wont accept more than i want
        let a item gets-good-want xs-demand
        let b (abs ( item gets-good-want other-xs-demand ) )
        let temp1 list a b 
        let gets min temp1
        set goods-stock replace-item gets-good-want goods-stock ((item gets-good-want goods-stock) + gets)
        set other-goods-stock replace-item gets-good-want other-goods-stock ((item gets-good-want other-goods-stock) - gets) 
        
        ;;i can't give more than he wants
        let c (abs ( item gives-good xs-demand) ) 
        let d item gives-good other-xs-demand
        let temp2 list c d 
        let gives min temp2
        set goods-stock replace-item gives-good goods-stock ((item gives-good goods-stock) - gives)
        set other-goods-stock replace-item gives-good other-goods-stock ((item gives-good other-goods-stock) + gives)
        
        ;;find new xs-demands
        find-xsdemand
        ask trading-partner [ set goods-stock other-goods-stock]
        ask trading-partner [ find-xsdemand ]
        ;;reset local copies to reflect trade
        set other-xs-demand [xs-demand] of trading-partner
        ;;set other-goods-stock [goods-stock] of trading-partner
        
        ;;show trade of turtle 0
        if log-turtle-0?
        [    
          show-trade gets-good-want gives-good temp1 temp2 gets gives exchange-type 
        ]
        
        ;;create link with trading-partner
        create-link-with trading-partner  
        set transactions transactions + 1
        ask trading-partner [ set transactions transactions + 1] 
      ]
      [
        set wecantrade? False 
        ;show "I dont have what he wants"
        ;show word "we can trade? " wecantrade?
      ]
    ]
    [
      set wecantrade? False 
      ;show "He doesn't have what i want"
      ;show word"we can trade? " wecantrade?
    ]
  ]

  ;show "#####  END EXCHANGE ATTEMPT  ######"
  log-turtle "end-exchange"
end

to-report find-corr [ moe ]
  let same-color []
  ask turtles with [color = moe ]
  [
    let my-color color 
    let all-neighbors count (link-neighbors)
    let same-color-neighbors count (link-neighbors with [color = my-color]) / all-neighbors
    let dif-color-neighbors 1 - same-color-neighbors
    set same-color fput same-color-neighbors same-color
  ]
  
  let avg-same-color mean same-color
  report avg-same-color
end
@#$#@#$#@
GRAPHICS-WINDOW
763
10
1253
521
-1
-1
4.7525
1
10
1
1
1
0
0
0
1
0
100
0
100
0
0
1
ticks
30.0

BUTTON
5
10
71
43
setup
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

SLIDER
3
47
147
80
population
population
3
1000
300
1
1
NIL
HORIZONTAL

BUTTON
86
10
149
43
go
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
3
86
146
119
num-of-goods
num-of-goods
3
12
3
1
1
NIL
HORIZONTAL

PLOT
9
681
328
828
Excess Demands of individual Goods
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" "let i 0\nlet j (i + num-of-goods)\nrepeat num-of-goods\n[\nlet temp_string_i (word i)\nlet temp_string_j (word j)\ncreate-temporary-plot-pen temp_string_i\nset-current-plot-pen temp_string_i\nset-plot-pen-color ( (i * 10) + 15)\nplot sum [ item i xs-demand] of turtles\ncreate-temporary-plot-pen temp_string_j\nset-current-plot-pen temp_string_j\nset-plot-pen-color ( (i * 10) + 16)\n;plot sum [ item i goods-requirements] of turtles\nset i (i + 1)\nset j (j + 1)\n]"
PENS
"default" 1.0 0 -16777216 true "" "plot 0"

SLIDER
4
165
148
198
max-requirements
max-requirements
0
20
20
1
1
NIL
HORIZONTAL

SLIDER
4
124
149
157
max-endowments
max-endowments
0
max-requirements
20
1
1
NIL
HORIZONTAL

CHOOSER
157
48
328
93
search-strategy
search-strategy
"random-partner" "previous-partners" "preferential-attachment" "preferential-habit"
1

SLIDER
158
100
330
133
max-trade-attempts
max-trade-attempts
1
num-of-goods * 5
4
1
1
NIL
HORIZONTAL

PLOT
337
10
757
328
Aggregate xs-demand
time
agggregate xs-demand
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"agg-xsD" 1.0 0 -955883 true "" "plot magnitude agg-xsdemand / discoordination0"
"agg-xsD_t = 0" 1.0 0 -16777216 true "" "plot magnitude agg-xsdemand0 / discoordination0"
"discoord_t = 0" 1.0 0 -13345367 true "" "plot discoordination0 / discoordination0"
"discoord" 1.0 0 -8630108 true "" "plot discoordination / discoordination0"
"altdiscoord" 1.0 0 -7500403 true "" ";plot (discoordination - ( magnitude agg-xsdemand)) / discoordination0"
"want-discoordination" 1.0 0 -13345367 true "" "plot want-discoordination / discoordination0"
"have-discoordination" 1.0 0 -2674135 true "" "plot have-discoordination / discoordination0"

PLOT
342
676
740
974
xs-demands of turtles
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"|xs-demand| turtle 0" 1.0 0 -5298144 true "" "plot magnitude [xs-demand] of turtle 0"
"pen-1" 1.0 0 -13840069 true "" ";plot magnitude [xs-demand] of turtle 1"
"pen-2" 1.0 0 -14070903 true "" ";plot magnitude [xs-demand] of turtle 2"
"trade-attempts turtle 0" 1.0 0 -10873583 true "" "plot [trade-attempts] of turtle 0"
"pen-4" 1.0 0 -14333415 true "" ";plot [trade-attempts] of turtle 1"
"pen-5" 1.0 0 -15390905 true "" ";plot [trade-attempts] of turtle 2"
"pen-6" 1.0 0 -7500403 true "" "plot max-trade-attempts"

PLOT
535
332
757
517
Aggregate Transactions
time
agg-transactions
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot agg-transactions"

PLOT
337
525
759
673
States
time
% of agents
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -2674135 true "" "plot count turtles with [state = \"I\"] / population"
"pen-1" 1.0 0 -1184463 true "" "plot count turtles with [state = \"E\"] / population"
"pen-2" 1.0 0 -13840069 true "" ";plot count turtles with [state = \"A\"] / population"

PLOT
335
333
530
517
Distribution of trade-attempts
trade-attemp
agents
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range 0 (max-trade-attempts + 5)\n;;set-plot-y-range 0 count turtles\nset-histogram-num-bars max-trade-attempts + 5" "histogram [color] of turtles"
PENS
"default" 1.0 1 -16777216 true "" "histogram [trade-attempts] of turtles"

PLOT
766
527
1022
673
Network Degree Distribution
degree
agents
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "let max-degree max [count link-neighbors] of turtles\nplot-pen-reset  ;; erase what we plotted before\nset-plot-x-range 0 (max-degree + 1)  ;; + 1 to make room for the width of the last bar\nhistogram [count link-neighbors] of turtles"

PLOT
1033
527
1253
673
Network Degree Distribution (log-log)
log(degree)
log(agents)
0.0
0.3
0.0
0.3
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" "let max-degree max [count link-neighbors] of turtles\n;; for this plot, the axes are logarithmic, so we can't\n;; use \"histogram-from\"; we have to plot the points\n;; ourselves one at a time\nplot-pen-reset  ;; erase what we plotted before\n;; the way we create the network there is never a zero degree node,\n;; so start plotting at degree one\nlet degree 1\nwhile [degree <= max-degree] [\n  let matches turtles with [count link-neighbors = degree]\n  if any? matches\n    [ plotxy log degree 10\n             log (count matches) 10 ]\n  set degree degree + 1\n]"

SWITCH
158
141
330
174
indirect-exchange
indirect-exchange
0
1
-1000

PLOT
7
222
330
420
Indirect Exchange Strategy this period
good
agents
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range 0 (num-of-goods)" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [moe-strategy-good] of turtles"

TEXTBOX
1265
153
1415
181
Debugging and Diagnostics\n
11
0.0
1

SWITCH
1263
42
1457
75
strategy-convergence
strategy-convergence
1
1
-1000

SWITCH
1264
79
1414
112
kiyotaki-wright
kiyotaki-wright
1
1
-1000

BUTTON
1265
188
1415
221
NIL
kiyotaki-wright-color
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1272
227
1422
283
Key\nwhite trade (0,1)\ngreen trade (0,2)\nblue trade (1,2)
11
0.0
0

SWITCH
1268
294
1406
327
log-turtle-0?
log-turtle-0?
1
1
-1000

SWITCH
158
10
303
43
color-by-MOE?
color-by-MOE?
0
1
-1000

BUTTON
1271
375
1440
408
NIL
indirect-xchange-test
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
1272
413
1416
458
test?
test?
"1B" "2A" "2B" "Kiyotaki-Wright"
3

SLIDER
1271
337
1444
370
return-to-strategy
return-to-strategy
1
100
1
1
1
NIL
HORIZONTAL

SLIDER
158
179
332
212
link-age-threshold
link-age-threshold
0
100
23
1
1
NIL
HORIZONTAL

PLOT
7
425
331
673
Indirect Exchange Strategy
time
% of agents using good i as moe
0.0
10.0
0.0
1.0
true
false
"" "let i 0\nrepeat num-of-goods\n[\nlet temp_string_i (word i)\ncreate-temporary-plot-pen temp_string_i\nset-current-plot-pen temp_string_i\nset-plot-pen-color ( (i * 10) + 15)\nplot  item i agg-moe-strategy-good / population\nset i (i + 1)\n]"
PENS
"pen-0" 1.0 0 -7500403 true "" ""

CHOOSER
1454
713
1624
758
layout?
layout?
"spring" "radial-by-centrality" "linear-by-centrality"
0

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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
NetLogo 5.1.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="habeamus peccunia" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <enumeratedValueSet variable="strategy-convergence">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="production-decisions">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="search-strategy">
      <value value="&quot;previous-partners&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kiyotaki-wright">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-endowments">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="log-turtle-0?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="indirect-exchange">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test?">
      <value value="&quot;Kiyotaki-Wright&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="color-by-MOE?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="660"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-trade-attempts">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-requirements">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-of-goods">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="return-to-strategy">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="age-threshold">
      <value value="40"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="1st run" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <metric>count turtles with [moe-strategy-good = 0]</metric>
    <metric>count turtles with [moe-strategy-good = 1]</metric>
    <metric>count turtles with [moe-strategy-good = 2]</metric>
    <metric>agg-transactions</metric>
    <metric>discoordination0 /   discoordination0</metric>
    <metric>discoordination / discoordination0</metric>
    <metric>(magnitude agg-xsdemand) / discoordination0</metric>
    <metric>want-discoordination / discoordination0</metric>
    <metric>have-discoordination / discoordination0</metric>
    <enumeratedValueSet variable="strategy-convergence">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="age-threshold">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="color-by-MOE?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="indirect-exchange">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-requirements">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-endowments">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="search-strategy">
      <value value="&quot;previous-partners&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-trade-attempts">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="production-decisions">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-of-goods">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="return-to-strategy">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="log-turtle-0?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kiyotaki-wright">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test?">
      <value value="&quot;Kiyotaki-Wright&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="660"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="KWfirstrun" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <metric>count turtles with [moe-strategy-good = 0]</metric>
    <metric>count turtles with [moe-strategy-good = 1]</metric>
    <metric>count turtles with [moe-strategy-good = 2]</metric>
    <metric>agg-transactions</metric>
    <metric>discoordination0 /   discoordination0</metric>
    <metric>discoordination / discoordination0</metric>
    <metric>(magnitude agg-xsdemand) / discoordination0</metric>
    <metric>want-discoordination / discoordination0</metric>
    <metric>have-discoordination / discoordination0</metric>
    <enumeratedValueSet variable="strategy-convergence">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="age-threshold">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="color-by-MOE?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="indirect-exchange">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-requirements">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-endowments">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="search-strategy">
      <value value="&quot;previous-partners&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-trade-attempts">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="production-decisions">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-of-goods">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="return-to-strategy">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="log-turtle-0?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kiyotaki-wright">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test?">
      <value value="&quot;Kiyotaki-Wright&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="1000"/>
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
