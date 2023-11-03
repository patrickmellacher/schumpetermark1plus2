; TERMS OF USE:
; Copyright (c) 2023 Patrick Mellacher

; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:

; 1.) The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.

; 2.) Any publication that contains results from this code (or significant portions) must cite the following publication:
; Mellacher, Patrick (2023). Growth, Inequality and Declining Business Dynamism in a Unified Schumpeter Mark I + II model

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHOR OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

extensions [Matrix csv]

  breed [industries industry]
  breed [firms firm]
  breed [entrepreneurs entrepreneur]
globals
[
  current_size_x
  current_size_y
  xcor_clicked
  ycor_clicked
  total_nominal_demand
  wage

  total_profits
  total_wages
  total_output
  total_real_wages
  total_potential_output
  employment_young_firms
  total_employment
  history_average_prices
  history_average_wages
  history_average_profits

  number_workers_current
  number_workers_old
  funds_added

  average_productivity_lag
  firm_entries

  average_firm_productivity_growth
  standard_deviation_firm_productivity_growth
  average_firm_output_growth
  standard_deviation_firm_output_growth
  average_firm_employment_growth
  standard_deviation_firm_employment_growth

  weighted_firm_productivity_growth

  average_herfindahl_index

  combined_marketshare_firstmovers
  combined_marketshare_others

  first_new_industry
  price_first_new_industry
  output_first_new_industry
  firms_first_new_industry

  gini_totalpopulation
  gini_entrepreneurs_income
  gini_entrepreneurs

  number_of_firms
  average_price
  ;rnd
  total_cumulated_difference

  mean_productivity_growth_rate
  employment_change

  worker_funds
  income_share_entrepreneurs
  income_share_entrepreneurs_old
  reallocation_rate


  GDP_growth_approximation
  employment_growth
  income_share_entrepreneurs_1939
  income_share_entrepreneurs_1980
  income_share_entrepreneurs_2018
  average_growth_rate_GDP
  stddev_growth_rate_GDP
  average_growth_rate_income_share_entrepreneurs_until_1980
  average_growth_rate_income_share_entrepreneurs_from_1980
  list_growth_rate_GDP_quarterly
  list_growth_rate_GDP_annual
  list_growth_rate_income_share_entrepreneurs
]

patches-own
[
  pot_payoff
]

industries-own
[
  number_workers_in_industry
  number_firms_in_industry
  nominal_labor_demand_in_industry
  nominal_labor_demand_in_industry_old
  labor_offered_parameter
  ;real_demand
  price
  productivity_growth_rate

  output_observed

  profits_observed
  monopoly_rent_observed
  herfindahl_index
  history_prices
  history_wages
  history_profits_observed
  history_monopoly_rent_observed
  history_investment_observed
  history_herfindahl_index
  history_output
  history_nominal_labor_demand_in_industry
  labor_productivity_leader
  labor_productivity_laggards
  expenditure_index
  birth_period
]

firms-own
[
  owner
  age
  labor_productivity
  labor_productivity_old
  labor_productivity_growth
  related_industry
  number_workers_in_firm
  number_workers_in_firm_old
  past_output
  past_past_output
  output
  output_growth
  revenue
  profit
  funds_available
  wage_payments
  monopoly_rent
  market_share
  first_mover
  competitor_output_list
  expected_competitor_output
  cumulated_difference
  antitrust_to_act
]

entrepreneurs-own
[
  number_firms
  combined_marketshare
  funds_available
  income
]




to setup
  clear-all
  resize-world 0 max_x (- max_y) 0
  set current_size_x  starting_x
  set current_size_y starting_y
  set-patch-size init_patch_size
  update_technological_possibilities
  set number_workers_current number_workers

  let number_industries_current 0
  while [number_industries_current < starting_industries * count patches with [pcolor = green]]
  [
    ask one-of patches with [pcolor = green and not any? industries-here]
    [
      sprout-industries 1
      [
        set shape "factory"
        set color grey
        set labor_productivity_leader labor_productivity_start
        set labor_productivity_laggards labor_productivity_start
        set labor_offered_parameter 1
        set expenditure_index (random 100) + 1
      ]
      set number_industries_current number_industries_current + 1
    ]
  ]

  let pot_entrepreneur_colors ( list 15 25 35 45 85 95 105 115 125 135)
  let i 2
  while [length pot_entrepreneur_colors < number_entrepreneurs and i <= 8]
  [
    let i2 10
    while [i2 <= 130]
    [
      if i2 = 50
      [
        set i2 80
      ]
      set pot_entrepreneur_colors lput (i2 + i) pot_entrepreneur_colors
      set i2 i2 + 10
    ]
    set i i + 1
  ]
  create-entrepreneurs number_entrepreneurs
  [
    set shape "person business"
    if empty? pot_entrepreneur_colors
    [
      set pot_entrepreneur_colors ( list 15 25 35 45 85 95 105 115 125 135)
    ]
    set color first pot_entrepreneur_colors
    set pot_entrepreneur_colors but-first pot_entrepreneur_colors
    set size 0.5
    setxy ( random current_size_x - 0.2 ) ( - random current_size_y - 0.2)
  ]

  ask industries
  [
    let current_number_firms 0
    let target_number_firms max (list min_firms_per_industry (number_entrepreneurs / number_industries_current))
    let possible_entrepreneurs sort-on [number_firms] entrepreneurs
    if distribution_firms_to_entrepreneurs = "random"
    [
      set possible_entrepreneurs shuffle possible_entrepreneurs
    ]
    while [current_number_firms < target_number_firms]
    [
      let target_entrepreneur first possible_entrepreneurs
      hatch-firms 1
      [
        let x_position (0.5 / target_number_firms ) * current_number_firms;let x_position random-float 0.5
        set xcor xcor + x_position
        set ycor ycor + 0.1
        set owner target_entrepreneur
        set shape "flag"
        set color [color] of target_entrepreneur
        set labor_productivity labor_productivity_start
        set related_industry myself
        set size 0.5
        set first_mover 1
        set competitor_output_list []
        ask owner
        [
          set number_firms number_firms + 1
        ]
      ]
      set number_firms_in_industry number_firms_in_industry + 1
      set possible_entrepreneurs but-first possible_entrepreneurs
      set current_number_firms current_number_firms + 1
    ]
  ]


  ask firms
  [
    set funds_available number_workers_current * ( [expenditure_index] of related_industry / sum [expenditure_index] of industries )  / count firms-here
  ]


  let target_reservation_wage sum [funds_available] of firms / number_workers

  ask patches with [pxcor > current_size_x or pycor < (- current_size_y)]
  [
    set pot_payoff -1 ;patches out of technological reach should not be chosen
  ]
  set wage 1 ;numeraire
  set list_growth_rate_GDP_annual []
  set list_growth_rate_GDP_quarterly []
  set list_growth_rate_income_share_entrepreneurs []
reset-ticks
end

to go
;set-patch-size
  set firm_entries 0

  update_xcor_ycor_clicked
  update_policy_scenarios
  let change_x random-float 1
  ifelse change_x < likelihood_change_x and current_size_x < max_x
  [
    set change_x 1
  ]
  [
    set change_x 0
  ]

  let change_y random-float 1
  ifelse change_y < likelihood_change_y and current_size_y < max_y
  [
    set change_y 1
  ]
  [
    set change_y 0
  ]

  set current_size_x current_size_x + change_x
  set current_size_y current_size_y + change_y
  update_technological_possibilities
  population_change
  move_entrepreneurs

  production_decision
  labor_market_interaction
  produce
  perform_rnd
  pay_wages_and_rents
  consume
  calc_profit
  ;calc_payoff_patches
  update_records
  update_history
  if calc_gini = true
  [
    calc_gini_entrepreneurs
    calc_gini_totalpopulation
  ]
  antitrust
  tick
end

to update_policy_scenarios
if policy_scenario = "historical USA"
  [
    if ticks = 0
    [
      set maximum_marketshare_companies 1
    ]
    if ticks = 780 ;year 1940 - power approach!
    [
      set maximum_marketshare_companies maximum_marketshare_scenario
    ]
    if ticks = 940 ;year 1980
    [
      set maximum_marketshare_companies 1
    ]
  ]
if policy_scenario = "counterfactual USA - longer antitrust"
  [
    if ticks = 0
    [
      set maximum_marketshare_companies 1
    ]
    if ticks = 780
    [
      set maximum_marketshare_companies maximum_marketshare_scenario
    ]
  ]

  if policy_scenario = "counterfactual USA - no technological distance penalty"
  [
    if ticks = 0
    [
      set imitation_distance_parameter 0
      set maximum_marketshare_companies 1
    ]
    if ticks = 780
    [
      set maximum_marketshare_companies maximum_marketshare_scenario
    ]
    if ticks = 940;year 1980
    [
      set maximum_marketshare_companies 1
    ]
  ]

  if policy_scenario = "counterfactual USA - more technological opportunities"
  [
    if ticks = 0
    [

      set maximum_marketshare_companies 1
    ]
    if ticks = 780
    [
      set maximum_marketshare_companies maximum_marketshare_scenario
    ]
    if ticks = 940 ;year 1980
    [
      set maximum_marketshare_companies 1
      set likelihood_change_x 0.012
      set likelihood_change_y 0.012
    ]
  ]

end

to update_technological_possibilities
ask patches with [pxcor <= current_size_x and pycor >= (- current_size_y)]
  [
    set pcolor green
  ]

  let max_dis max (list (current_size_x * patch-size) (current_size_y * patch-size))
  if max_dis > max_dis_size  and behaviorspace-run-number = 0
  [
    set-patch-size patch-size / (max_dis / max_dis_size)
  ]

end

to population_change
  ;workers
  set funds_added (number_workers_current * (population_growth_parameter)) * wage
  set number_workers_current (number_workers_current * (1 + population_growth_parameter))
  ;entrepreneurs
  ask entrepreneurs
  [
    let old_entrepreneur self
    if (random-float 1) < (population_growth_parameter) and number_firms > 1
    [
      set number_firms count firms with [owner = myself]
      let number_firms_to_split floor (number_firms / 2)
      let new_entrepreneur 0
      set funds_available funds_available / 2
      set number_firms number_firms - number_firms_to_split
      hatch-entrepreneurs 1
      [
        set number_firms number_firms_to_split
        set new_entrepreneur self
        set funds_available [funds_available] of myself
        set color one-of ( list 15 25 35 45 85 95 105 115 125 135)
      ]

      let industries_with_two_firms_same_entrepreneur industries with [count firms with [owner = old_entrepreneur and related_industry = myself] > 1]
      ask industries_with_two_firms_same_entrepreneur
      [
        ask one-of firms with [owner = old_entrepreneur and related_industry = myself]
        [
          set owner new_entrepreneur
          set color [color] of new_entrepreneur
        ]
      ]
      ask up-to-n-of (number_firms_to_split - count industries_with_two_firms_same_entrepreneur) (firms with [owner = old_entrepreneur and not member? related_industry industries_with_two_firms_same_entrepreneur])
      [
        set owner new_entrepreneur
        set color [color] of new_entrepreneur
      ]
    ]
  ]
end

to move_entrepreneurs
  ask entrepreneurs
  [
    set income 0
  ]
  if entrepreneurs_mobility = "free mobility"
  [
    ask entrepreneurs with [funds_available > wage ]
    [
      let asking_entrepreneur self

      let target_patches patches with [pcolor = green and not any? firms-here with [owner = asking_entrepreneur]]

      calc_payoff_patches target_patches funds_available

      let target_patch one-of target_patches with-max [pot_payoff]
      if is-patch? target_patch
      [
        if [pcolor] of target_patch = green
        [
          setxy ( [pxcor] of target_patch - 0.2 ) ( [pycor] of target_patch - 0.2 )

          ifelse not any? industries-here ;radical innovation
          [
            if random-float 1 <= radical_innovation_likelihood
            [
              hatch-industries 1
              [
                set shape "factory"
                set color grey
                set size 1
                setxy pxcor pycor
                set labor_offered_parameter 1
                set number_firms_in_industry 1
                set labor_productivity_leader labor_productivity_start
                set labor_productivity_laggards 1
                create_fake_history self
                set expenditure_index (random 100) + 1
                if first_new_industry = 0
                [
                  set first_new_industry self
                ]
                set birth_period ticks
              ]
              hatch-firms 1
              [
                set first_mover 1
                set xcor pxcor
                set ycor pycor + 0.1
                set owner myself
                set shape "flag"
                set color [color] of myself
                set labor_productivity labor_productivity_start
                set related_industry one-of industries-here
                set size 0.5
                set funds_available [funds_available] of owner
                set competitor_output_list []
                ask owner
                [
                  set number_firms number_firms + 1
                  set funds_available 0
                ]
                reorganize_firms_view one-of industries-here
                set firm_entries firm_entries + 1
              ]
            ]
          ]
          ;try to imitate
          [
            let imitated_industry one-of industries-here
            let imitation_target [labor_productivity_leader] of imitated_industry
            if imitation_target_option = "closest competitor"
            [
              set imitation_target [labor_productivity] of first sort-on [labor_productivity] firms with [related_industry = imitated_industry]
            ]
            let imitation_possible 1 - e ^ (- imitation_likelihood_parameter_entrepreneurs )
            if imitation_likelihood = "technological distance"
            [
              set imitation_possible 1 - e ^ (- imitation_likelihood_parameter_entrepreneurs / (1 + imitation_distance_parameter * ( imitation_target / labor_productivity_start ) ))
            ]
            if random-float 1 < imitation_possible
            [
              hatch-firms 1
              [
                set first_mover 0
                set xcor pxcor
                set ycor pycor + 0.1
                set owner myself
                set shape "flag"
                set color [color] of myself
                set labor_productivity imitation_target
                set related_industry one-of industries-here
                set size 0.5
                set funds_available [funds_available] of owner
                set competitor_output_list []
                ask owner
                [
                  set number_firms number_firms + 1
                  set funds_available 0
                ]
                reorganize_firms_view one-of industries-here
                set firm_entries firm_entries + 1
              ]
            ]
          ]
        ]
      ]
    ]
  ]

end


to update_xcor_ycor_clicked
  if  behaviorspace-run-number = 0
  [
    if mouse-down?
    [
      set xcor_clicked round(mouse-xcor)
      set ycor_clicked round(mouse-ycor)
    ]
  ]
end

to production_decision
  ask industries
  [
    set nominal_labor_demand_in_industry_old nominal_labor_demand_in_industry
    set nominal_labor_demand_in_industry 0
  ]

  ask firms
  [

    set monopoly_rent 0


      ifelse output > 0
      [
        if output_decision = "cournot"
        [
          let reference_wage wage;[wage] of related_industry
          ;if reference_wage = 0
          ;[
          ;  set reference_wage mean [wage ] of industries with [wage > 0]
          ;]
          let weighted_difference (expected_competitor_output - ([output_observed] of related_industry - output)) * [price] of related_industry
          set cumulated_difference cumulated_difference + weighted_difference
          set total_cumulated_difference total_cumulated_difference + weighted_difference

          set expected_competitor_output 0

          if [output_observed] of related_industry - output > 0
          [
            set competitor_output_list lput ([output_observed] of related_industry - output ) competitor_output_list
             set expected_competitor_output precision (max list 0 item 0 matrix:forecast-compound-growth competitor_output_list) 4
          ]

          let expected_nominaldemand_sector 0
          if length (filter [ i -> i > 0 ] sublist ([history_nominal_labor_demand_in_industry] of related_industry) ([birth_period] of related_industry) (ticks)) > 0
          [
            set expected_nominaldemand_sector (item 0 matrix:forecast-compound-growth filter [ i -> i > 0 ] sublist ([history_nominal_labor_demand_in_industry] of related_industry) ([birth_period] of related_industry) (ticks) )
          ]

          let desired_cournot_output max (list 0.1 (sqrt (expected_nominaldemand_sector * expected_competitor_output / (reference_wage / labor_productivity )) - expected_competitor_output ))
          let cournot_wage_payments desired_cournot_output * (reference_wage / labor_productivity ) * ( 1 + propensity_rnd)
          let expected_profit expected_nominaldemand_sector * desired_cournot_output / (desired_cournot_output + expected_competitor_output) - cournot_wage_payments
          let expected_cournot_monopoly_rent expected_profit / cournot_wage_payments
          ifelse expected_cournot_monopoly_rent <= target_profit_rate
          [
            let cournot_target_wagepayments min (list cournot_wage_payments (revenue))
            set wage_payments cournot_target_wagepayments
          ]
          [
            set wage_payments  revenue  / (1 + target_profit_rate )
          ]
          set monopoly_rent revenue - wage_payments
        ]
      ]
      [
        ifelse total_output = 0
        [
          ifelse start_in_equilibrium = TRUE
          [
             let desired_cournot_output (([number_firms_in_industry] of related_industry - 1 ) * labor_productivity * (number_workers  * [expenditure_index] of related_industry / sum [expenditure_index] of industries)) / ([number_firms_in_industry] of related_industry ^ 2);number_workers * [number_firms_in_industry] of related_industry * labor_productivity * (1 - savings_propensity_entrepreneurs) * [expenditure_index] of related_industry / sum [expenditure_index] of industries / (1 + 2 * [number_firms_in_industry] of related_industry + [number_firms_in_industry] of related_industry ^ 2 - [number_firms_in_industry] of related_industry ^ 2 * [expenditure_index] of related_industry / sum [expenditure_index] of industries + (1 - savings_propensity_entrepreneurs) * [number_firms_in_industry] of related_industry ^ 2 * count industries * [expenditure_index] of related_industry / sum [expenditure_index] of industries)
            let cournot_wage_payments desired_cournot_output * (wage / labor_productivity ) * ( 1 + propensity_rnd)

            let expected_profit funds_available - cournot_wage_payments;expected_nominaldemand_sector * desired_cournot_output / (desired_cournot_output + expected_competitor_output) - cournot_wage_payments

            let expected_cournot_monopoly_rent expected_profit / cournot_wage_payments

            ifelse expected_cournot_monopoly_rent <= target_profit_rate
            [
              let cournot_target_wagepayments min (list cournot_wage_payments (funds_available))
              set wage_payments cournot_target_wagepayments
            ]
            [
              set wage_payments  funds_available / (1 + target_profit_rate )
            ]
            set monopoly_rent funds_available - wage_payments
          ]
          [
            set wage_payments funds_available
          ]
        ]
        [
          ;new industries
          let expected_nominaldemand_sector (total_nominal_demand * (1 + population_growth_parameter)) / (count industries with [output_observed > 0] + 1)
          ;if industry is not new
          if length (filter [ i -> i > 0 ] sublist ([history_nominal_labor_demand_in_industry] of related_industry) ([birth_period] of related_industry) (ticks)) > 0
          [
            set expected_nominaldemand_sector (item 0 matrix:forecast-compound-growth filter [ i -> i > 0 ] sublist ([history_nominal_labor_demand_in_industry] of related_industry) ([birth_period] of related_industry) (ticks) )
          ]
          set wage_payments min (list funds_available ( expected_nominaldemand_sector / (1 + target_profit_rate)))
          set monopoly_rent funds_available - wage_payments
        ]
      ]



    ask related_industry
    [
      set nominal_labor_demand_in_industry nominal_labor_demand_in_industry + [wage_payments] of myself
    ]

  ]
end

to labor_market_interaction
  ask firms
  [
    set number_workers_in_firm_old number_workers_in_firm
    set number_workers_in_firm wage_payments / wage
  ]
end

to perform_rnd
  ask industries
  [
    let firms_this_industry firms with [related_industry = myself]
    if any? firms_this_industry
    [
      let industry_leader one-of firms_this_industry with-max [labor_productivity]
      set labor_productivity_leader [labor_productivity] of industry_leader
      let other_firms []
      ask industry_leader
      [
        set other_firms other firms_this_industry
      ]


      ifelse any? other_firms
      [
        set labor_productivity_laggards mean [labor_productivity] of other_firms
      ]
      [
        set labor_productivity_laggards 1
      ]
    ]
  ]


  ask firms
  [
    set labor_productivity_growth 0
    let workers_invention propensity_rnd * number_workers_in_firm * workers_invention_split
    let workers_imitation propensity_rnd * number_workers_in_firm * (1 - workers_invention_split)
    let industry_leader_productivity [labor_productivity_leader] of related_industry
    if labor_productivity = industry_leader_productivity
    [
      set workers_invention propensity_rnd * number_workers_in_firm
      set workers_imitation 0
    ]
    ;invention
    let invention_possible 1 - e ^ (- invention_likelihood_parameter * workers_invention)
    if random-float 1 < invention_possible ; check whether attempt was succesful
    [
      let delta_labor_productivity precision (random-normal invention_mean invention_stddev) 5
      if delta_labor_productivity > 0
      [
        set labor_productivity labor_productivity * (1 + delta_labor_productivity)
        set labor_productivity_growth delta_labor_productivity
      ]
    ]

    ;imitation

    if workers_imitation > 0
    [
      let imitation_target industry_leader_productivity
      if imitation_target_option = "closest competitor"
      [
        carefully [set imitation_target [labor_productivity] of first sort-on [labor_productivity_old] firms with [related_industry = [related_industry] of myself and labor_productivity_old > [labor_productivity_old] of myself]] [set imitation_target industry_leader_productivity]
      ]
      let imitation_possible 1 - e ^ (- imitation_likelihood_parameter * workers_imitation)
      if imitation_likelihood = "technological distance"
      [
        carefully [set imitation_possible 1 - e ^ ( ( - imitation_likelihood_parameter / (1 + imitation_distance_parameter * ( industry_leader_productivity / labor_productivity ) ) ) * workers_imitation)] [set imitation_possible 0]
      ]
      if random-float 1 < imitation_possible ; check whether attempt was succesful
      [
        if labor_productivity < industry_leader_productivity ;only if a firm is still a laggard even after possibly innovating
        [
          set labor_productivity_growth ( industry_leader_productivity / labor_productivity ) - 1
          set labor_productivity industry_leader_productivity
        ]
      ]
    ]
   ]
end
to produce
  ask firms
  [
    set labor_productivity_old labor_productivity
    ifelse number_workers_in_firm * (1 - propensity_rnd) <= 1
    [
      set past_past_output past_output
      set past_output output
      set output number_workers_in_firm * (1 - propensity_rnd) * labor_productivity
    ]
    [
      set past_past_output past_output
      set past_output output
      set output ( number_workers_in_firm * (1 - propensity_rnd)  ) * labor_productivity
    ]
    if past_output > 0
    [
        set output_growth ( output / past_output ) - 1
    ]
  ]
  ask industries
  [
    let old_output output_observed
    set output_observed sum [output] of firms with [related_industry = myself]
    ifelse old_output > 0 and nominal_labor_demand_in_industry > 0
    [
      let old_productivity old_output / nominal_labor_demand_in_industry_old
      let new_productivity output_observed / nominal_labor_demand_in_industry
      set productivity_growth_rate new_productivity / old_productivity - 1
    ]
    [
      set productivity_growth_rate 0
    ]
  ]
  set mean_productivity_growth_rate mean [productivity_growth_rate] of industries
end

to pay_wages_and_rents

  set worker_funds sum [nominal_labor_demand_in_industry] of industries + funds_added


  ask firms
  [
    ask owner
      [
        set funds_available funds_available + [monopoly_rent] of myself
        set income income + [monopoly_rent] of myself
      ]
    set funds_available funds_available - wage_payments - monopoly_rent
  ]
  set income_share_entrepreneurs_old income_share_entrepreneurs
  set income_share_entrepreneurs sum [income] of entrepreneurs / (sum [income] of entrepreneurs + worker_funds)
end


to consume
  set total_nominal_demand worker_funds + ( sum [funds_available] of entrepreneurs * (1 - savings_propensity_entrepreneurs))
  set worker_funds 0
  ask entrepreneurs
  [
    set funds_available funds_available * savings_propensity_entrepreneurs
  ]
  if demand_function = "equal share"
  [
    let nominal_demand_industry 0
    if count industries with [output_observed > 0] > 0
    [
       set nominal_demand_industry total_nominal_demand / count industries with [output_observed > 0]
    ]
    ask industries with [output_observed > 0]
    [
      set price nominal_demand_industry / sum [output] of firms with [related_industry = myself]
    ]
  ]
  if demand_function = "expenditure share"
  [
    let total_expenditure_index sum [expenditure_index] of industries with [output_observed > 0]
    if count industries with [output_observed > 0] = 0 ;first period
    [
      set total_expenditure_index sum [expenditure_index] of industries
    ]
    ask industries with [output_observed > 0]
    [
      set price total_nominal_demand * (expenditure_index / total_expenditure_index) / (output_observed )
    ]
  ]


end

to calc_profit


  let industries_to_reorganize (list)
  let entrepreneurs_losing_firms (turtle-set)
  ask firms
  [
    set revenue [price] of related_industry * output
    set profit revenue - wage_payments

    set funds_available funds_available + revenue
    if funds_available < 0.1
    [
      let funds_available_remaining funds_available
      set entrepreneurs_losing_firms (turtle-set entrepreneurs_losing_firms owner)
      ask owner
      [
         set funds_available funds_available + funds_available_remaining
      ]
      set industries_to_reorganize fput related_industry industries_to_reorganize
      die
    ]
  ]
  ask entrepreneurs_losing_firms
  [
    set number_firms count firms with [owner = myself]

    if number_firms = 0
    [
      set number_workers_current number_workers_current + 1
      die
    ]
  ]

  foreach industries_to_reorganize [ [a] ->
      reorganize_firms_view a
    ]
end

to calc_payoff_patches [patch_set investment]

  ask patches with [not member? self patch_set]
  [
    set pot_payoff (-1)
  ]

  ask patch_set
  [
    ifelse any? industries-here = true
    [
      let respective_industry  one-of industries-here
      let current_labor_productivity_leader [labor_productivity_leader] of respective_industry

      ;if yes: imitate
      let imitation_possible 1 - e ^ (- imitation_likelihood_parameter_entrepreneurs )
      if imitation_likelihood = "technological distance"
      [
        set imitation_possible 1 - e ^ (- imitation_likelihood_parameter_entrepreneurs / (1 + imitation_distance_parameter * ( current_labor_productivity_leader / labor_productivity_start ) ))
      ]

      let expected_competitor_output_new ( [output_observed] of respective_industry ) ;* ( 1 + [expected_growthrate] of related_industry)
      let expected_nominaldemand_sector (total_nominal_demand * (1 + population_growth_parameter)) / (count industries with [output_observed > 0] + 1)
      ;if industry is not new
      if length (filter [ i -> i > 0 ] sublist ([history_nominal_labor_demand_in_industry] of respective_industry) ([birth_period] of respective_industry) (ticks)) > 0
      [
        set expected_nominaldemand_sector (item 0 matrix:forecast-compound-growth filter [ i -> i > 0 ] sublist ([history_nominal_labor_demand_in_industry] of respective_industry) ([birth_period] of respective_industry) (ticks) )
      ]
      let reference_wage wage
      let desired_cournot_output max (list 0.1 (sqrt (expected_nominaldemand_sector * expected_competitor_output_new / (reference_wage / current_labor_productivity_leader )) - expected_competitor_output_new ))
      let cournot_wage_payments min (list investment ( desired_cournot_output * (reference_wage / current_labor_productivity_leader ) * ( 1 + propensity_rnd)))
      let expected_profit expected_nominaldemand_sector * desired_cournot_output / (desired_cournot_output + expected_competitor_output_new) - cournot_wage_payments
      set pot_payoff imitation_possible * expected_profit

    ]
    [
      ;if no: radically innovate
      let expected_nominaldemand_sector (total_nominal_demand * (1 + population_growth_parameter)) / (count industries with [output_observed > 0] + 1)
      let wage_payments_expected min (list (expected_nominaldemand_sector / (1 + target_profit_rate )) investment)
      set pot_payoff radical_innovation_likelihood * max (list 0 (expected_nominaldemand_sector - wage_payments_expected))
    ]
  ]


end




to update_records
  set total_profits sum [profit] of firms

  set total_wages sum [nominal_labor_demand_in_industry] of industries
  set total_output sum [output] of firms
  let share_of_wages_in_total_demand 0
  if total_nominal_demand > 0
  [
    set share_of_wages_in_total_demand total_wages / total_nominal_demand
  ]
  set total_real_wages total_output * share_of_wages_in_total_demand
  set total_potential_output sum [number_workers_in_industry / (1 + propensity_rnd) * labor_productivity_leader] of industries with [labor_productivity_leader > 0]
  set employment_young_firms 0 ;updated below
  let total_employment_old total_employment
  set total_employment 0 ;updated below
  ask firms
  [
    let industry_in_question related_industry
    ifelse sum [output] of firms with [related_industry = industry_in_question] > 0
    [
      set market_share output / sum [output] of firms with [related_industry = industry_in_question]
    ]
    [
      set market_share 0
    ]
    set age age + 1
    if age < 20
    [
      set employment_young_firms employment_young_firms + number_workers_in_firm
    ]
    set total_employment total_employment + number_workers_in_firm
  ]
  if total_employment_old > 0
  [
    set employment_change total_employment / total_employment_old - 1
  ]

  ask entrepreneurs
  [
    set combined_marketshare sum [market_share] of firms with [owner = myself]
  ]
  ifelse any? industries with [labor_productivity_leader > 0]
  [
    set average_productivity_lag mean [labor_productivity_laggards / labor_productivity_leader] of industries with [labor_productivity_laggards > 0 and labor_productivity_leader > 0]
  ]
  [
    set average_productivity_lag "NA"
  ]


  set average_herfindahl_index mean [herfindahl_index] of industries
  if number_workers_old > 0
  [
  set employment_growth total_employment / number_workers_current - total_employment_old / number_workers_old

  ]
  set number_workers_old number_workers_current
  set average_firm_output_growth mean [output_growth] of firms

  set GDP_growth_approximation ( 1 + employment_growth) * (1 + weighted_firm_productivity_growth) - 1


  set number_of_firms count firms
  if number_of_firms > 1
  [
    set average_firm_productivity_growth mean [labor_productivity_growth] of firms
    set weighted_firm_productivity_growth sum [labor_productivity_growth * market_share] of firms / count industries
    set standard_deviation_firm_productivity_growth  standard-deviation [labor_productivity_growth] of firms
    set standard_deviation_firm_output_growth  standard-deviation [output_growth] of firms

    if any? firms with [number_workers_in_firm_old > 0]
    [
      set reallocation_rate sum [abs ( number_workers_in_firm  - number_workers_in_firm_old)] of firms / sum [number_workers_in_firm_old] of firms
      set average_firm_employment_growth mean [number_workers_in_firm  / number_workers_in_firm_old - 1] of firms with [number_workers_in_firm_old > 0]
       set standard_deviation_firm_employment_growth standard-deviation [number_workers_in_firm / number_workers_in_firm_old - 1] of firms  with [number_workers_in_firm_old > 0]
    ]

  ]
  set average_price mean [price] of industries with [price > 0]

  set combined_marketshare_firstmovers sum [market_share] of firms with [first_mover = 1]
  set combined_marketshare_others sum [market_share] of firms with [first_mover = 0]

  if first_new_industry != 0
  [
    set price_first_new_industry [price] of first_new_industry
    set output_first_new_industry [output_observed] of first_new_industry
    set firms_first_new_industry [number_firms_in_industry] of first_new_industry
  ]





  if ticks >= 300 ;burn-in phase
  [
    set list_growth_rate_GDP_quarterly fput GDP_growth_approximation list_growth_rate_GDP_quarterly

    if length list_growth_rate_GDP_quarterly = 4
    [
      set list_growth_rate_GDP_annual fput (sum list_growth_rate_GDP_quarterly) list_growth_rate_GDP_annual
      set list_growth_rate_GDP_quarterly []
    ]
    if income_share_entrepreneurs_old > 0
    [
      set list_growth_rate_income_share_entrepreneurs fput (income_share_entrepreneurs / income_share_entrepreneurs_old - 1) list_growth_rate_income_share_entrepreneurs
    ]
  ]

  if ticks = 779 ;1939 - before enacting the "power" approach
  [
    set income_share_entrepreneurs_1939 income_share_entrepreneurs
  ]
  if ticks = 940 ;1980
  [
    set income_share_entrepreneurs_1980 income_share_entrepreneurs
    set average_growth_rate_income_share_entrepreneurs_until_1980 mean list_growth_rate_income_share_entrepreneurs
    set list_growth_rate_income_share_entrepreneurs []
  ]
  if ticks = 1092 ;2018
  [
    set income_share_entrepreneurs_2018 income_share_entrepreneurs
    set average_growth_rate_income_share_entrepreneurs_from_1980 mean list_growth_rate_income_share_entrepreneurs
  ]

  if length list_growth_rate_GDP_annual > 1
  [
    set average_growth_rate_GDP mean list_growth_rate_GDP_annual
    set stddev_growth_rate_GDP standard-deviation list_growth_rate_GDP_annual
  ]

end

to update_history
  ask industries with [is-list? history_wages = true]
   [
     set profits_observed sum [profit] of firms with [related_industry = myself and profit >= 0]
     set monopoly_rent_observed sum [monopoly_rent] of firms with [related_industry = myself]
     set herfindahl_index sum [market_share ^ 2] of firms with [related_industry = myself]
     set history_prices lput price history_prices
     set history_wages lput wage history_wages
     set history_investment_observed lput nominal_labor_demand_in_industry history_investment_observed
     set history_profits_observed lput profits_observed history_profits_observed
     set history_monopoly_rent_observed lput monopoly_rent_observed history_monopoly_rent_observed
     set history_herfindahl_index lput herfindahl_index history_herfindahl_index
     set history_output lput output_observed history_output
     set history_nominal_labor_demand_in_industry lput (output_observed * price) history_nominal_labor_demand_in_industry

   ]
  ask industries with [is-list? history_wages = false]
   [
     set profits_observed sum [profit] of firms with [related_industry = myself and profit >= 0]
     set monopoly_rent_observed sum [monopoly_rent] of firms with [related_industry = myself]
     set herfindahl_index sum [market_share ^ 2] of firms with [related_industry = myself]
     set history_prices (list  price )
     set history_wages (list wage )
     set history_investment_observed (list nominal_labor_demand_in_industry)
     set history_profits_observed (list profits_observed )
     set history_monopoly_rent_observed (list monopoly_rent_observed )
     set history_herfindahl_index (list herfindahl_index )
     set history_output (list output_observed)
      set history_nominal_labor_demand_in_industry (list (output_observed * price) )
   ]

   ifelse is-list? history_average_prices = true
   [
     set history_average_prices lput mean [price] of industries with [price > 0] history_average_prices
     set history_average_wages lput mean [wage] of industries  with [wage > 0] history_average_wages
     set history_average_profits lput mean [profits_observed] of industries history_average_profits
   ]
   [
    set history_average_prices (list mean [price] of industries with [price > 0] )
    set history_average_wages (list mean [wage] of industries with [wage > 0] )
    set history_average_profits (list mean [profits_observed] of industries )
   ]


end

to update_price_history
  let shown_industry one-of industries with [pxcor = xcor_clicked and pycor = ycor_clicked]

  if is-industry? shown_industry = true
  [
    ask shown_industry
    [
      let time_horizon length history_prices
      let i show_from_period
      while [i < time_horizon]
      [
        let relative_price item i history_prices / item i history_average_prices
        plotxy i relative_price
        set i i + 1
      ]
    ]
  ]

end

to update_wage_history
  let shown_industry one-of industries with [pxcor = xcor_clicked and pycor = ycor_clicked]

  if is-industry? shown_industry = true
  [
    ask shown_industry
    [
      let time_horizon length history_wages
      let i show_from_period
      while [i < time_horizon]
      [
        let relative_wage item i history_wages / item i history_average_wages
        plotxy i relative_wage
        set i i + 1
      ]
    ]
  ]

end

to update_herfindahlindex_history
  let shown_industry one-of industries with [pxcor = xcor_clicked and pycor = ycor_clicked]

  if is-industry? shown_industry = true
  [
    ask shown_industry
    [
      let time_horizon length history_herfindahl_index
      let i show_from_period
      while [i < time_horizon]
      [
        let herfindahl_index_i item i history_herfindahl_index
        plotxy i herfindahl_index_i
        set i i + 1
      ]
    ]
  ]

end

to update_output_history
  let shown_industry one-of industries with [pxcor = xcor_clicked and pycor = ycor_clicked]

  if is-industry? shown_industry = true
  [
    ask shown_industry
    [
      let time_horizon length history_prices
      let i show_from_period
      while [i < time_horizon]
      [
        let output_i item i history_output
        plotxy i output_i
        set i i + 1
      ]
    ]
  ]

end

to update_profit_history [absolute]
  let shown_industry one-of industries with [pxcor = xcor_clicked and pycor = ycor_clicked]

  if is-industry? shown_industry = true
  [
    ask shown_industry
    [
      let time_horizon length history_profits_observed
      let i show_from_period
      while [i < time_horizon]
      [
        let absolute_profit item i history_profits_observed
        ifelse absolute = true
        [
          plotxy i absolute_profit
        ]
        [
          let absolute_investment item i history_investment_observed
          ifelse absolute_investment > 0
          [
            plotxy i absolute_profit / item i history_investment_observed
          ]
          [
            plotxy i 0
          ]
        ]
        set i i + 1
      ]
    ]
  ]

end

to update_monopoly_rent_history [absolute]
  let shown_industry one-of industries with [pxcor = xcor_clicked and pycor = ycor_clicked]

  if is-industry? shown_industry = true
  [
    ask shown_industry
    [
      let time_horizon length history_monopoly_rent_observed
      let i show_from_period
      while [i < time_horizon]
      [
        let absolute_monopoly_rent item i history_monopoly_rent_observed

        ifelse absolute = true
        [
          plotxy i absolute_monopoly_rent
        ]
        [
          let absolute_investment item i history_investment_observed
          ifelse absolute_investment > 0
          [
            plotxy i absolute_monopoly_rent / absolute_investment
          ]
          [
            plotxy i 0
          ]
        ]

        set i i + 1
      ]
    ]
  ]

end

to update_distribution_firms
  let entrepreneurs_list sort-on [ (- number_firms )] entrepreneurs
  let i 1
  while [empty? entrepreneurs_list = false]
  [
    plotxy i [number_firms] of first entrepreneurs_list
    set entrepreneurs_list but-first entrepreneurs_list
    set i i + 1
  ]

end

to update_distribution_marketshare
  let entrepreneurs_list sort-on [ (- combined_marketshare )] entrepreneurs
  let i 1
  while [empty? entrepreneurs_list = false]
  [
    plotxy i [combined_marketshare] of first entrepreneurs_list
    set entrepreneurs_list but-first entrepreneurs_list
    set i i + 1
  ]
end

to update_distribution_funds
  let entrepreneurs_list sort-on [ (- funds_available )] entrepreneurs
  let i 1
  while [empty? entrepreneurs_list = false]
  [
    plotxy i [funds_available] of first entrepreneurs_list
    set entrepreneurs_list but-first entrepreneurs_list
    set i i + 1
  ]

end

to view_industry
if ticks > 0
[
    update_xcor_ycor_clicked
    set-current-plot "Industry - rel. wages and prices"
    clear-plot
    set-plot-x-range show_from_period ticks
    set-plot-y-range 0 1
    set-current-plot-pen _clarify-duplicate-plot-pen-name "relative price"
    update_price_history
    set-current-plot-pen _clarify-duplicate-plot-pen-name "relative wage"
    update_wage_history


    set-current-plot "Industry - profits"
    clear-plot
    set-plot-x-range show_from_period ticks
    set-plot-y-range 0 0.2
    set-current-plot-pen _clarify-duplicate-plot-pen-name "profit"
    update_profit_history false


    set-current-plot "Industry - herfindahlindex"
    clear-plot
    set-plot-x-range show_from_period ticks
    set-plot-y-range 0 1
    set-current-plot-pen _clarify-duplicate-plot-pen-name "herfindahlindex"
    update_herfindahlindex_history

    set-current-plot "Industry - output"
    clear-plot
    set-plot-x-range show_from_period ticks
    set-plot-y-range 0 1
    set-current-plot-pen _clarify-duplicate-plot-pen-name "industry output"
    update_output_history
]

end

to reorganize_firms_view [ industry_to_reorganize ]

  ask industry_to_reorganize
  [
    let firms_list sort-on [xcor] firms with [related_industry = myself]
    let firms_list_length length firms_list
    set number_firms_in_industry firms_list_length
    let i 0
    while [length firms_list > 0]
    [
      ask first firms_list
      [
        let x_position (0.5 / firms_list_length) * i   ;let x_position random-float 0.5
        set xcor [pxcor] of myself + x_position
      ]
      set firms_list but-first firms_list
      set i i + 1
    ]

  ]
end

to create_fake_history [ target_industry]
  ask target_industry
  [
     set history_prices (list  0 )
     set history_wages (list 0 )
     set history_investment_observed (list 0)
     set history_profits_observed (list 0 )
     set history_monopoly_rent_observed (list 0 )
     set history_herfindahl_index (list 0 )
     set history_output (list 0 )
     set history_nominal_labor_demand_in_industry (list 0)
    while [ length history_monopoly_rent_observed < max [length history_monopoly_rent_observed] of industries]
    [
      set history_prices lput 0 history_prices
      set history_wages lput 0 history_wages
      set history_investment_observed lput 0 history_investment_observed
      set history_profits_observed lput 0 history_profits_observed
      set history_monopoly_rent_observed lput 0 history_monopoly_rent_observed
      set history_herfindahl_index lput 0 history_herfindahl_index
      set history_output lput 0 history_output
      set history_nominal_labor_demand_in_industry lput 0 history_nominal_labor_demand_in_industry
    ]
  ]
end

to calc_gini_entrepreneurs

  let entrepreneurs_list sort [  combined_marketshare ] of entrepreneurs
  let entrepreneurs_list_length length entrepreneurs_list
  let calculated_sum 0
  let i 1
  foreach entrepreneurs_list
  [ x ->
    set calculated_sum calculated_sum + x * (2 * i - entrepreneurs_list_length - 1)
    set i i + 1
  ]
  if entrepreneurs_list_length = 1
  [
    set calculated_sum 1
  ]
  set gini_entrepreneurs 1 / (entrepreneurs_list_length ^ 2 * mean entrepreneurs_list) * calculated_sum

end

to calc_gini_totalpopulation
  let population_list sort ( [income] of entrepreneurs  )
  let population_list_length length population_list
  let calculated_sum 0
  let i 1
  foreach population_list
  [ x ->
    set calculated_sum calculated_sum + x * (2 * i - population_list_length - 1)
    set i i + 1
  ]
  if sum [income] of entrepreneurs > 0
  [
    set gini_entrepreneurs_income 1 / (population_list_length ^ 2 * mean population_list) * calculated_sum
  ]
  if population_list_length = 1
  [
    set gini_entrepreneurs_income 1
  ]

end

to-report _clarify-duplicate-plot-pen-name [ name ]
  let name-map [["output" "output"] ["Output" "Output_1"]]
  let replacement filter [ rename -> first rename = name] name-map
  let reported-name name
  if not empty? replacement [
    set reported-name item 1 replacement
  ]
  report reported-name
end

to set_full_opportunity_growth_scenario [day]
  if ticks = day
  [
    set likelihood_change_x 1
    set likelihood_change_y 1
  ]
end

to antitrust
  ask firms
  [
    ifelse market_share > maximum_marketshare_companies
    [
      set antitrust_to_act antitrust_to_act + 1
    ]
    [
      set antitrust_to_act 0
    ]

    if random-float 1 > ( 1 - antitrust_probability) ^ antitrust_to_act
    [
      let target_entrepreneur owner
      hatch-firms 1
      [
        let x_position random-float 0.5
        set xcor xcor ;+ x_position
        set ycor ycor
        set owner target_entrepreneur
        set shape "flag"
        set color [color] of target_entrepreneur
        set labor_productivity [labor_productivity] of myself
        set related_industry [related_industry] of myself
        set size 0.5
        set first_mover 0
        set age 0
        set competitor_output_list (list ([output_observed] of related_industry - [output] of myself / 2))
        set funds_available [funds_available] of myself / 2
        set output output / 2
        set revenue [revenue] of myself / 2
        ask owner
        [
          set number_firms number_firms + 1
        ]
        set antitrust_to_act 0
      ]
      set antitrust_to_act 0
      ask related_industry
      [
        set number_firms_in_industry number_firms_in_industry + 1
      ]
      reorganize_firms_view related_industry
      set competitor_output_list (list ([output_observed] of related_industry - output / 2))
      set funds_available funds_available / 2
      set revenue revenue / 2
      set output output / 2
      set firm_entries firm_entries + 1
    ]
  ]
end

to output_csv
  file-close-all
  if file-exists? (word csv_file_name ".csv");"industry_outcomes.csv"
  [
    file-delete (word csv_file_name ".csv");"industry_outcomes.csv"
  ]
  file-open (word csv_file_name ".csv");"industry_outcomes.csv"
  file-print "industryid,time,output, price, hhi, expenditure_index"
  let industryid 0
  ask industries
  [
    let time 0
    while [(time) < length history_output]
    [
      file-print (word industryid "," (time + 1) "," (item time history_output) "," (item time history_prices) "," (item time history_herfindahl_index)  "," expenditure_index)
      set time time + 1
    ]
    set industryid industryid + 1
  ]
  file-close-all
end


to output_wealth_distribution
  file-close-all
  if file-exists? (word csv_file_name_wealth ".csv")
  [
    file-delete (word csv_file_name_wealth ".csv")
  ]
  file-open (word csv_file_name_wealth ".csv")
  file-print "entrepreneur_number,wealth,total_entrepreneurs"

  let entrepreneurs_list sort-on [ (- funds_available )] entrepreneurs
  let entrepreneur_id 1
  foreach entrepreneurs_list
  [ ?a ->
    ask ?a
    [
      file-print (word entrepreneur_id "," (funds_available) "," count entrepreneurs)
      set entrepreneur_id entrepreneur_id + 1
    ]
  ]

  file-close-all
end
@#$#@#$#@
GRAPHICS-WINDOW
1095
172
2736
1814
-1
-1
77.77777777777777
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
20
-20
0
0
0
1
ticks
30.0

SLIDER
556
91
728
124
starting_x
starting_x
0
10
1.0
1
1
NIL
HORIZONTAL

SLIDER
556
133
728
166
starting_y
starting_y
0
10
1.0
1
1
NIL
HORIZONTAL

BUTTON
316
462
414
495
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

INPUTBOX
557
205
640
265
max_x
20.0
1
0
Number

INPUTBOX
646
205
728
265
max_y
20.0
1
0
Number

INPUTBOX
1115
92
1198
152
max_dis_size
700.0
1
0
Number

INPUTBOX
15
93
133
153
number_workers
10000.0
1
0
Number

INPUTBOX
142
93
272
153
number_entrepreneurs
100.0
1
0
Number

SLIDER
557
275
730
308
likelihood_change_x
likelihood_change_x
0
1
0.008
0.001
1
NIL
HORIZONTAL

SLIDER
558
318
730
351
likelihood_change_y
likelihood_change_y
0
1
0.008
0.001
1
NIL
HORIZONTAL

SLIDER
556
169
729
202
starting_industries
starting_industries
0
1
1.0
0.01
1
NIL
HORIZONTAL

BUTTON
317
500
413
533
go x 1
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
1204
92
1288
152
init_patch_size
100.0
1
0
Number

INPUTBOX
315
386
413
457
random_seed
1.0
1
0
Number

BUTTON
419
423
523
456
set random seed
random-seed random_seed
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
419
387
524
420
generate new seed
set random_seed new-seed
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
18
157
204
202
distribution_firms_to_entrepreneurs
distribution_firms_to_entrepreneurs
"equalized" "random"
0

INPUTBOX
405
92
532
152
labor_productivity_start
1.0
1
0
Number

INPUTBOX
277
93
403
153
min_firms_per_industry
10.0
1
0
Number

PLOT
29
713
295
882
Industry - rel. wages and prices
NIL
NIL
0.0
2.0
0.0
2.0
true
true
"" "clear-plot \nset-plot-x-range show_from_period ticks\nset-plot-y-range 0 1"
PENS
"relative price" 1.0 0 -16777216 true "" "update_price_history"
"relative wage" 1.0 0 -7500403 true "" "update_wage_history"

BUTTON
713
647
818
680
view industry
view_industry
NIL
1
T
OBSERVER
NIL
A
NIL
NIL
1

PLOT
28
928
347
1109
Economy - nominal
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
"Total profits" 1.0 0 -7500403 true "" "plot total_profits"
"Total wages" 1.0 0 -2674135 true "" "plot total_wages"
"Total demand" 1.0 0 -6459832 true "" "plot total_nominal_demand"

INPUTBOX
777
155
944
215
invention_likelihood_parameter
0.3
1
0
Number

INPUTBOX
949
156
1045
216
invention_mean
0.0
1
0
Number

INPUTBOX
948
219
1046
279
invention_stddev
0.01149187
1
0
Number

INPUTBOX
776
89
901
149
propensity_rnd
0.02
1
0
Number

INPUTBOX
777
219
945
279
imitation_likelihood_parameter
0.3
1
0
Number

INPUTBOX
908
90
1045
150
workers_invention_split
0.5
1
0
Number

PLOT
311
713
584
881
Industry - profits
NIL
NIL
0.0
1.0
0.0
0.1
true
true
"" "clear-plot\nset-plot-x-range show_from_period ticks\nset-plot-y-range 0 0.2"
PENS
"profit" 1.0 0 -16777216 true "" "update_profit_history false"

INPUTBOX
557
387
734
447
target_profit_rate
0.3321438
1
0
Number

CHOOSER
211
157
354
202
entrepreneurs_mobility
entrepreneurs_mobility
"free mobility"
0

CHOOSER
782
522
1047
567
imitation_likelihood
imitation_likelihood
"uniform" "technological distance"
1

INPUTBOX
778
348
1046
408
radical_innovation_likelihood
0.01
1
0
Number

INPUTBOX
554
529
735
589
savings_propensity_entrepreneurs
0.2
1
0
Number

PLOT
271
1337
547
1526
Distribution of entrepreneurs
entrepreneurs
NIL
0.0
10.0
0.0
10.0
true
true
"" "clear-plot\nlet max_y_show max (list max [number_firms] of entrepreneurs max [combined_marketshare] of entrepreneurs )\nset-plot-x-range 1 count entrepreneurs \nset-plot-y-range 0 max_y_show"
PENS
"firms" 1.0 0 -16777216 true "" "update_distribution_firms"
"market share" 1.0 0 -2674135 true "" "update_distribution_marketshare"

PLOT
596
714
817
881
Industry - herfindahlindex
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" "clear-plot\nset-plot-x-range show_from_period ticks\nset-plot-y-range 0 1"
PENS
"herfindahlindex" 1.0 0 -16777216 true "" "update_herfindahlindex_history"

CHOOSER
19
207
169
252
demand_function
demand_function
"equal share" "expenditure share"
1

INPUTBOX
939
646
1041
706
show_from_period
0.0
1
0
Number

PLOT
353
929
676
1109
Economy - real
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
"Output" 1.0 0 -16777216 true "" "plot total_output"
"Real wages" 1.0 0 -2674135 true "" "plot total_real_wages"
"Workers" 1.0 0 -7500403 true "" "plot number_workers_current"

PLOT
829
714
1043
882
Industry - output
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" "clear-plot\nset-plot-x-range show_from_period ticks\nset-plot-y-range 0 0.2"
PENS
"Industry output" 1.0 0 -16777216 true "" "update_output_history"

CHOOSER
556
451
735
496
output_decision
output_decision
"cournot"
0

INPUTBOX
778
411
1045
471
imitation_distance_parameter
150.3954
1
0
Number

SWITCH
633
1340
788
1373
calc_gini
calc_gini
0
1
-1000

MONITOR
633
1428
789
1473
Gini total population
gini_totalpopulation
10
1
11

MONITOR
634
1377
788
1422
Gini comb. marketshare
gini_entrepreneurs
10
1
11

PLOT
420
1117
763
1296
Gini index
NIL
NIL
0.0
1.0
0.0
1.0
true
true
"" ""
PENS
"Comb. marketshare" 1.0 0 -13345367 true "" "plot gini_entrepreneurs"
"Entrepreneurs income" 1.0 0 -7500403 true "" "plot gini_entrepreneurs_income"

BUTTON
420
500
524
533
go x 1109
repeat 1109\n[\ngo\n]
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
419
462
523
495
go forever
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

PLOT
770
1116
989
1297
Wage share
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"wage_share_pen" 1.0 0 -16777216 true "" "if ((total_profits + total_wages) > 0)\n[\nplot total_wages / (total_profits + total_wages)\n]"

SWITCH
175
208
353
241
start_in_equilibrium
start_in_equilibrium
1
1
-1000

INPUTBOX
29
436
266
496
maximum_marketshare_companies
1.0
1
0
Number

INPUTBOX
30
565
268
625
antitrust_probability
0.2
1
0
Number

INPUTBOX
778
283
1046
343
imitation_likelihood_parameter_entrepreneurs
3.0
1
0
Number

PLOT
28
1116
407
1296
changes in employment and productvity
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"employment" 1.0 0 -16777216 true "" "plot employment_change"
"labor productivity" 1.0 0 -2674135 true "" "plot mean_productivity_growth_rate"

INPUTBOX
367
156
533
216
population_growth_parameter
0.004418605
1
0
Number

MONITOR
267
301
440
346
current number of entrepreneurs
count entrepreneurs
17
1
11

MONITOR
113
302
260
347
current number of workers
number_workers_current
0
1
11

PLOT
34
1337
265
1526
Funds of entrepreneurs
entrepreneurs
funds
0.0
10.0
0.0
10.0
false
false
"" "clear-plot\nlet max_y_show max [funds_available] of entrepreneurs\nset-plot-x-range 1 count entrepreneurs \nset-plot-y-range 0 precision max_y_show 0"
PENS
"default" 1.0 0 -16777216 true "" "update_distribution_funds"

CHOOSER
29
388
265
433
policy_scenario
policy_scenario
"none" "historical USA" "counterfactual USA - longer antitrust" "counterfactual USA - no technological distance penalty" "counterfactual USA - more technological opportunities"
1

MONITOR
634
1474
788
1519
NIL
income_share_entrepreneurs
5
1
11

PLOT
686
930
994
1109
Share of income received by entrepreneurs
NIL
NIL
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot income_share_entrepreneurs"

INPUTBOX
30
501
267
561
maximum_marketshare_scenario
0.149987
1
0
Number

BUTTON
33
1641
239
1674
output industry outcomes csv
output_csv
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
32
1575
240
1635
csv_file_name
industry_outcomes_schumarkI+II
1
0
String

CHOOSER
780
475
1045
520
imitation_target_option
imitation_target_option
"industry leader" "closest competitor"
0

INPUTBOX
330
1571
512
1631
csv_file_name_wealth
wealth_distribution
1
0
String

BUTTON
330
1639
513
1672
output wealth distribution
output_wealth_distribution
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
333
1551
534
1583
Necessary to replicate Figure 7
13
0.0
1

TEXTBOX
33
1552
259
1590
Necessary to replicate Figures 2-5
13
0.0
1

TEXTBOX
555
69
738
107
Technological opportunities
15
0.0
1

TEXTBOX
845
70
995
89
Innovation & Imitation
15
0.0
1

TEXTBOX
1179
69
1329
88
Display
15
0.0
1

TEXTBOX
582
363
694
381
Behavior of firms
15
0.0
1

TEXTBOX
211
69
361
88
Agent initialization\n
15
0.0
1

TEXTBOX
191
278
386
316
Current number of agents
15
0.0
1

TEXTBOX
100
369
250
388
Scenario analysis
15
0.0
1

TEXTBOX
377
362
527
381
Start simulation
15
0.0
1

TEXTBOX
35
694
1073
722
Hover over an industry (display on the right), click on it with the left mouse button and then click \"A\" on your keyboard to view industry-specific data (only possible AFTER period 0)
11
0.0
1

TEXTBOX
396
898
717
948
Macroeconomic Data
20
0.0
1

TEXTBOX
187
1310
486
1360
Distribution of Entrepreneurs
20
0.0
1

TEXTBOX
419
656
639
681
Industry-level Data
20
0.0
1

TEXTBOX
559
508
737
526
Behavior of entrepreneurs
15
0.0
1

TEXTBOX
24
26
1020
44
(c) Patrick Mellacher 2023                                                please cite as: Mellacher, Patrick (2023). Growth, Inequality and Declining Business Dynamism in a Unified Schumpeter Mark I + II model\n
11
0.0
1

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

factory
false
0
Rectangle -7500403 true true 76 194 285 270
Rectangle -7500403 true true 36 95 59 231
Rectangle -16777216 true false 90 210 270 240
Line -7500403 true 90 195 90 255
Line -7500403 true 120 195 120 255
Line -7500403 true 150 195 150 240
Line -7500403 true 180 195 180 255
Line -7500403 true 210 210 210 240
Line -7500403 true 240 210 240 240
Line -7500403 true 90 225 270 225
Circle -1 true false 37 73 32
Circle -1 true false 55 38 54
Circle -1 true false 96 21 42
Circle -1 true false 105 40 32
Circle -1 true false 129 19 42
Rectangle -7500403 true true 14 228 78 270

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

person business
false
0
Rectangle -1 true false 120 90 180 180
Polygon -13345367 true false 135 90 150 105 135 180 150 195 165 180 150 105 165 90
Polygon -7500403 true true 120 90 105 90 60 195 90 210 116 154 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 183 153 210 210 240 195 195 90 180 90 150 165
Circle -7500403 true true 110 5 80
Rectangle -7500403 true true 127 76 172 91
Line -16777216 false 172 90 161 94
Line -16777216 false 128 90 139 94
Polygon -13345367 true false 195 225 195 300 270 270 270 195
Rectangle -13791810 true false 180 225 195 300
Polygon -14835848 true false 180 226 195 226 270 196 255 196
Polygon -13345367 true false 209 202 209 216 244 202 243 188
Line -16777216 false 180 90 150 165
Line -16777216 false 120 90 150 165

person lumberjack
false
0
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Polygon -2674135 true false 60 196 90 211 114 155 120 196 180 196 187 158 210 211 240 196 195 91 165 91 150 106 150 135 135 91 105 91
Circle -7500403 true true 110 5 80
Rectangle -7500403 true true 127 79 172 94
Polygon -6459832 true false 174 90 181 90 180 195 165 195
Polygon -13345367 true false 180 195 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285
Polygon -6459832 true false 126 90 119 90 120 195 135 195
Rectangle -6459832 true false 45 180 255 195
Polygon -16777216 true false 255 165 255 195 240 225 255 240 285 240 300 225 285 195 285 165
Line -16777216 false 135 165 165 165
Line -16777216 false 135 135 165 135
Line -16777216 false 90 135 120 135
Line -16777216 false 105 120 120 120
Line -16777216 false 180 120 195 120
Line -16777216 false 180 135 210 135
Line -16777216 false 90 150 105 165
Line -16777216 false 225 165 210 180
Line -16777216 false 75 165 90 180
Line -16777216 false 210 150 195 165
Line -16777216 false 180 105 210 180
Line -16777216 false 120 105 90 180
Line -16777216 false 150 135 150 165
Polygon -2674135 true false 100 30 104 44 189 24 185 10 173 10 166 1 138 -1 111 3 109 28

person worker
false
15
Polygon -1 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Polygon -13345367 true false 60 196 90 211 114 155 120 196 180 196 187 158 210 211 240 196 195 91 165 91 150 106 150 135 135 91 105 91
Circle -1 true true 110 5 80
Rectangle -1 true true 127 79 172 94
Polygon -1184463 true false 174 90 181 90 180 195 150 195
Polygon -13345367 true false 180 195 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285
Polygon -1184463 true false 126 90 119 90 120 195 150 195
Rectangle -6459832 true false 60 195 150 210
Polygon -1184463 true false 105 45 105 45 210 30 180 15 173 10 166 1 138 -1 111 3 105 15
Rectangle -1184463 true false 120 135 180 195
Line -16777216 false 150 135 150 195
Polygon -16777216 true false 135 180 135 225 135 225 135 225 150 225 150 225 150 225 150 195

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
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="calibration_historical" repetitions="1000" runMetricsEveryStep="false">
    <setup>setup
set invention_stddev random-float 0.1
set maximum_marketshare_scenario random-float 1
set invention_mean 0
set imitation_distance_parameter random-float 300
set target_profit_rate random-float 1</setup>
    <go>go</go>
    <timeLimit steps="1109"/>
    <metric>invention_stddev</metric>
    <metric>invention_mean</metric>
    <metric>imitation_distance_parameter</metric>
    <metric>maximum_marketshare_scenario</metric>
    <metric>target_profit_rate</metric>
    <metric>total_output</metric>
    <metric>total_real_wages</metric>
    <metric>total_potential_output</metric>
    <metric>gini_totalpopulation</metric>
    <metric>gini_entrepreneurs</metric>
    <metric>average_productivity_lag</metric>
    <metric>employment_young_firms</metric>
    <metric>total_employment</metric>
    <metric>firm_entries</metric>
    <metric>count industries</metric>
    <metric>average_firm_productivity_growth</metric>
    <metric>standard_deviation_firm_productivity_growth</metric>
    <metric>average_firm_output_growth</metric>
    <metric>standard_deviation_firm_output_growth</metric>
    <metric>average_herfindahl_index</metric>
    <metric>combined_marketshare_firstmovers</metric>
    <metric>combined_marketshare_others</metric>
    <metric>number_of_firms</metric>
    <metric>average_price</metric>
    <metric>price_first_new_industry</metric>
    <metric>output_first_new_industry</metric>
    <metric>firms_first_new_industry</metric>
    <metric>count entrepreneurs</metric>
    <metric>number_workers_current</metric>
    <metric>gini_entrepreneurs_income</metric>
    <metric>employment_change</metric>
    <metric>mean_productivity_growth_rate</metric>
    <metric>income_share_entrepreneurs</metric>
    <metric>income_share_entrepreneurs_1939</metric>
    <metric>income_share_entrepreneurs_1980</metric>
    <metric>income_share_entrepreneurs_2018</metric>
    <metric>average_growth_rate_GDP</metric>
    <metric>stddev_growth_rate_GDP</metric>
    <metric>average_growth_rate_income_share_entrepreneurs_until_1980</metric>
    <metric>average_growth_rate_income_share_entrepreneurs_from_1980</metric>
    <enumeratedValueSet variable="entrepreneurs_mobility">
      <value value="&quot;free mobility&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="target_profit_rate_option">
      <value value="&quot;market share&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_entrepreneurs">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start_in_equilibrium">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="invention_likelihood_parameter">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation_likelihood_parameter">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_workers">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="propensity_rnd">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radical_innovation_likelihood">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="technological_opportunities_scenario">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_growth_parameter">
      <value value="0.004418605"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_patch_size">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="likelihood_change_x">
      <value value="0.008"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="likelihood_change_y">
      <value value="0.008"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand_function">
      <value value="&quot;expenditure share&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_y">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum_marketshare_companies">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_from_period">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output_expectation_parameter">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation_likelihood_parameter_entrepreneurs">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_dis_size">
      <value value="700"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output_decision">
      <value value="&quot;cournot&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_antitrust_to_act">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers_invention_split">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy_scenario">
      <value value="&quot;historical USA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting_industries">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution_firms_to_entrepreneurs">
      <value value="&quot;equalized&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor_productivity_start">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation_likelihood">
      <value value="&quot;technological distance&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="savings_propensity_entrepreneurs">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_x">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting_x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting_y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_firms_per_industry">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_gini">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="calibrated_runs_other_imitation_target" repetitions="1" runMetricsEveryStep="true">
    <setup>setup
set invention_stddev 0.01149187
set invention_mean 0
set imitation_distance_parameter 150.3954
set maximum_marketshare_scenario 0.149987
set target_profit_rate  0.3321438</setup>
    <go>go</go>
    <timeLimit steps="1109"/>
    <metric>invention_stddev</metric>
    <metric>invention_mean</metric>
    <metric>imitation_distance_parameter</metric>
    <metric>maximum_marketshare_scenario</metric>
    <metric>target_profit_rate</metric>
    <metric>total_output</metric>
    <metric>total_real_wages</metric>
    <metric>total_potential_output</metric>
    <metric>gini_totalpopulation</metric>
    <metric>gini_entrepreneurs</metric>
    <metric>average_productivity_lag</metric>
    <metric>employment_young_firms</metric>
    <metric>total_employment</metric>
    <metric>firm_entries</metric>
    <metric>count industries</metric>
    <metric>average_firm_productivity_growth</metric>
    <metric>standard_deviation_firm_productivity_growth</metric>
    <metric>average_firm_output_growth</metric>
    <metric>standard_deviation_firm_output_growth</metric>
    <metric>average_herfindahl_index</metric>
    <metric>combined_marketshare_firstmovers</metric>
    <metric>combined_marketshare_others</metric>
    <metric>number_of_firms</metric>
    <metric>average_price</metric>
    <metric>price_first_new_industry</metric>
    <metric>output_first_new_industry</metric>
    <metric>firms_first_new_industry</metric>
    <metric>count entrepreneurs</metric>
    <metric>number_workers_current</metric>
    <metric>gini_entrepreneurs_income</metric>
    <metric>employment_change</metric>
    <metric>mean_productivity_growth_rate</metric>
    <metric>income_share_entrepreneurs</metric>
    <metric>income_share_entrepreneurs_1939</metric>
    <metric>income_share_entrepreneurs_1980</metric>
    <metric>income_share_entrepreneurs_2018</metric>
    <metric>average_growth_rate_GDP</metric>
    <metric>stddev_growth_rate_GDP</metric>
    <metric>average_firm_employment_growth</metric>
    <metric>standard_deviation_firm_employment_growth</metric>
    <metric>average_growth_rate_income_share_entrepreneurs_until_1980</metric>
    <metric>average_growth_rate_income_share_entrepreneurs_from_1980</metric>
    <metric>reallocation_rate</metric>
    <steppedValueSet variable="random-seed" first="1" step="1" last="1000"/>
    <enumeratedValueSet variable="entrepreneurs_mobility">
      <value value="&quot;free mobility&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="target_profit_rate_option">
      <value value="&quot;market share&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_entrepreneurs">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start_in_equilibrium">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="invention_likelihood_parameter">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation_likelihood_parameter">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_workers">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="propensity_rnd">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radical_innovation_likelihood">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="technological_opportunities_scenario">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_growth_parameter">
      <value value="0.004418605"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_patch_size">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="likelihood_change_x">
      <value value="0.008"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="likelihood_change_y">
      <value value="0.008"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand_function">
      <value value="&quot;expenditure share&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_y">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum_marketshare_companies">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_from_period">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output_expectation_parameter">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation_likelihood_parameter_entrepreneurs">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_dis_size">
      <value value="700"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output_decision">
      <value value="&quot;cournot&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="antitrust_probability">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers_invention_split">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy_scenario">
      <value value="&quot;historical USA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting_industries">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution_firms_to_entrepreneurs">
      <value value="&quot;equalized&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor_productivity_start">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation_likelihood">
      <value value="&quot;technological distance&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="savings_propensity_entrepreneurs">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_x">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting_x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting_y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_firms_per_industry">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_gini">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation_target_option">
      <value value="&quot;closest competitor&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="calibrated_runs_baseline" repetitions="1" runMetricsEveryStep="true">
    <setup>setup
set invention_stddev 0.01149187
set invention_mean 0
set imitation_distance_parameter 150.3954
set maximum_marketshare_scenario 0.149987
set target_profit_rate  0.3321438</setup>
    <go>go</go>
    <timeLimit steps="1109"/>
    <metric>invention_stddev</metric>
    <metric>invention_mean</metric>
    <metric>imitation_distance_parameter</metric>
    <metric>maximum_marketshare_scenario</metric>
    <metric>target_profit_rate</metric>
    <metric>total_output</metric>
    <metric>total_real_wages</metric>
    <metric>total_potential_output</metric>
    <metric>gini_totalpopulation</metric>
    <metric>gini_entrepreneurs</metric>
    <metric>average_productivity_lag</metric>
    <metric>employment_young_firms</metric>
    <metric>total_employment</metric>
    <metric>firm_entries</metric>
    <metric>count industries</metric>
    <metric>average_firm_productivity_growth</metric>
    <metric>standard_deviation_firm_productivity_growth</metric>
    <metric>average_firm_output_growth</metric>
    <metric>standard_deviation_firm_output_growth</metric>
    <metric>average_herfindahl_index</metric>
    <metric>combined_marketshare_firstmovers</metric>
    <metric>combined_marketshare_others</metric>
    <metric>number_of_firms</metric>
    <metric>average_price</metric>
    <metric>price_first_new_industry</metric>
    <metric>output_first_new_industry</metric>
    <metric>firms_first_new_industry</metric>
    <metric>count entrepreneurs</metric>
    <metric>number_workers_current</metric>
    <metric>gini_entrepreneurs_income</metric>
    <metric>employment_change</metric>
    <metric>mean_productivity_growth_rate</metric>
    <metric>income_share_entrepreneurs</metric>
    <metric>income_share_entrepreneurs_1939</metric>
    <metric>income_share_entrepreneurs_1980</metric>
    <metric>income_share_entrepreneurs_2018</metric>
    <metric>average_growth_rate_GDP</metric>
    <metric>stddev_growth_rate_GDP</metric>
    <metric>average_firm_employment_growth</metric>
    <metric>standard_deviation_firm_employment_growth</metric>
    <metric>average_growth_rate_income_share_entrepreneurs_until_1980</metric>
    <metric>average_growth_rate_income_share_entrepreneurs_from_1980</metric>
    <metric>reallocation_rate</metric>
    <steppedValueSet variable="random-seed" first="1" step="1" last="1000"/>
    <enumeratedValueSet variable="entrepreneurs_mobility">
      <value value="&quot;free mobility&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="target_profit_rate_option">
      <value value="&quot;market share&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_entrepreneurs">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start_in_equilibrium">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="invention_likelihood_parameter">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation_likelihood_parameter">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_workers">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="propensity_rnd">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radical_innovation_likelihood">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="technological_opportunities_scenario">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_growth_parameter">
      <value value="0.004418605"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_patch_size">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="likelihood_change_x">
      <value value="0.008"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="likelihood_change_y">
      <value value="0.008"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand_function">
      <value value="&quot;expenditure share&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_y">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum_marketshare_companies">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_from_period">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output_expectation_parameter">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation_likelihood_parameter_entrepreneurs">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_dis_size">
      <value value="700"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output_decision">
      <value value="&quot;cournot&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="antitrust_probability">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers_invention_split">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy_scenario">
      <value value="&quot;historical USA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting_industries">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution_firms_to_entrepreneurs">
      <value value="&quot;equalized&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor_productivity_start">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation_likelihood">
      <value value="&quot;technological distance&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="savings_propensity_entrepreneurs">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_x">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting_x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting_y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_firms_per_industry">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_gini">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation_target_option">
      <value value="&quot;industry leader&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="calibrated_runs_counterfactual1" repetitions="1" runMetricsEveryStep="true">
    <setup>setup
set invention_stddev 0.01149187
set invention_mean 0
set imitation_distance_parameter 150.3954
set maximum_marketshare_scenario 0.149987
set target_profit_rate  0.3321438</setup>
    <go>go</go>
    <timeLimit steps="1109"/>
    <metric>invention_stddev</metric>
    <metric>invention_mean</metric>
    <metric>imitation_distance_parameter</metric>
    <metric>maximum_marketshare_scenario</metric>
    <metric>target_profit_rate</metric>
    <metric>total_output</metric>
    <metric>total_real_wages</metric>
    <metric>total_potential_output</metric>
    <metric>gini_totalpopulation</metric>
    <metric>gini_entrepreneurs</metric>
    <metric>average_productivity_lag</metric>
    <metric>employment_young_firms</metric>
    <metric>total_employment</metric>
    <metric>firm_entries</metric>
    <metric>count industries</metric>
    <metric>average_firm_productivity_growth</metric>
    <metric>standard_deviation_firm_productivity_growth</metric>
    <metric>average_firm_output_growth</metric>
    <metric>standard_deviation_firm_output_growth</metric>
    <metric>average_herfindahl_index</metric>
    <metric>combined_marketshare_firstmovers</metric>
    <metric>combined_marketshare_others</metric>
    <metric>number_of_firms</metric>
    <metric>average_price</metric>
    <metric>price_first_new_industry</metric>
    <metric>output_first_new_industry</metric>
    <metric>firms_first_new_industry</metric>
    <metric>count entrepreneurs</metric>
    <metric>number_workers_current</metric>
    <metric>gini_entrepreneurs_income</metric>
    <metric>employment_change</metric>
    <metric>mean_productivity_growth_rate</metric>
    <metric>income_share_entrepreneurs</metric>
    <metric>income_share_entrepreneurs_1939</metric>
    <metric>income_share_entrepreneurs_1980</metric>
    <metric>income_share_entrepreneurs_2018</metric>
    <metric>average_growth_rate_GDP</metric>
    <metric>stddev_growth_rate_GDP</metric>
    <metric>average_firm_employment_growth</metric>
    <metric>standard_deviation_firm_employment_growth</metric>
    <metric>average_growth_rate_income_share_entrepreneurs_until_1980</metric>
    <metric>average_growth_rate_income_share_entrepreneurs_from_1980</metric>
    <metric>reallocation_rate</metric>
    <steppedValueSet variable="random-seed" first="1" step="1" last="1000"/>
    <enumeratedValueSet variable="entrepreneurs_mobility">
      <value value="&quot;free mobility&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="target_profit_rate_option">
      <value value="&quot;market share&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_entrepreneurs">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start_in_equilibrium">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="invention_likelihood_parameter">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation_likelihood_parameter">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_workers">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="propensity_rnd">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radical_innovation_likelihood">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="technological_opportunities_scenario">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_growth_parameter">
      <value value="0.004418605"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_patch_size">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="likelihood_change_x">
      <value value="0.008"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="likelihood_change_y">
      <value value="0.008"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand_function">
      <value value="&quot;expenditure share&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_y">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum_marketshare_companies">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_from_period">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output_expectation_parameter">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation_likelihood_parameter_entrepreneurs">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_dis_size">
      <value value="700"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output_decision">
      <value value="&quot;cournot&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="antitrust_probability">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers_invention_split">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy_scenario">
      <value value="&quot;counterfactual USA - longer antitrust&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting_industries">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution_firms_to_entrepreneurs">
      <value value="&quot;equalized&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor_productivity_start">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation_likelihood">
      <value value="&quot;technological distance&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="savings_propensity_entrepreneurs">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_x">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting_x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting_y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_firms_per_industry">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_gini">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation_target_option">
      <value value="&quot;industry leader&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="calibrated_runs_counterfactual2" repetitions="1" runMetricsEveryStep="true">
    <setup>setup
set invention_stddev 0.01149187
set invention_mean 0
set imitation_distance_parameter 150.3954
set maximum_marketshare_scenario 0.149987
set target_profit_rate  0.3321438</setup>
    <go>go</go>
    <timeLimit steps="1109"/>
    <metric>invention_stddev</metric>
    <metric>invention_mean</metric>
    <metric>imitation_distance_parameter</metric>
    <metric>maximum_marketshare_scenario</metric>
    <metric>target_profit_rate</metric>
    <metric>total_output</metric>
    <metric>total_real_wages</metric>
    <metric>total_potential_output</metric>
    <metric>gini_totalpopulation</metric>
    <metric>gini_entrepreneurs</metric>
    <metric>average_productivity_lag</metric>
    <metric>employment_young_firms</metric>
    <metric>total_employment</metric>
    <metric>firm_entries</metric>
    <metric>count industries</metric>
    <metric>average_firm_productivity_growth</metric>
    <metric>standard_deviation_firm_productivity_growth</metric>
    <metric>average_firm_output_growth</metric>
    <metric>standard_deviation_firm_output_growth</metric>
    <metric>average_herfindahl_index</metric>
    <metric>combined_marketshare_firstmovers</metric>
    <metric>combined_marketshare_others</metric>
    <metric>number_of_firms</metric>
    <metric>average_price</metric>
    <metric>price_first_new_industry</metric>
    <metric>output_first_new_industry</metric>
    <metric>firms_first_new_industry</metric>
    <metric>count entrepreneurs</metric>
    <metric>number_workers_current</metric>
    <metric>gini_entrepreneurs_income</metric>
    <metric>employment_change</metric>
    <metric>mean_productivity_growth_rate</metric>
    <metric>income_share_entrepreneurs</metric>
    <metric>income_share_entrepreneurs_1939</metric>
    <metric>income_share_entrepreneurs_1980</metric>
    <metric>income_share_entrepreneurs_2018</metric>
    <metric>average_growth_rate_GDP</metric>
    <metric>stddev_growth_rate_GDP</metric>
    <metric>average_firm_employment_growth</metric>
    <metric>standard_deviation_firm_employment_growth</metric>
    <metric>average_growth_rate_income_share_entrepreneurs_until_1980</metric>
    <metric>average_growth_rate_income_share_entrepreneurs_from_1980</metric>
    <metric>reallocation_rate</metric>
    <steppedValueSet variable="random-seed" first="1" step="1" last="1000"/>
    <enumeratedValueSet variable="entrepreneurs_mobility">
      <value value="&quot;free mobility&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="target_profit_rate_option">
      <value value="&quot;market share&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_entrepreneurs">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start_in_equilibrium">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="invention_likelihood_parameter">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation_likelihood_parameter">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_workers">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="propensity_rnd">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radical_innovation_likelihood">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="technological_opportunities_scenario">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_growth_parameter">
      <value value="0.004418605"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_patch_size">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="likelihood_change_x">
      <value value="0.008"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="likelihood_change_y">
      <value value="0.008"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand_function">
      <value value="&quot;expenditure share&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_y">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum_marketshare_companies">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_from_period">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output_expectation_parameter">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation_likelihood_parameter_entrepreneurs">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_dis_size">
      <value value="700"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output_decision">
      <value value="&quot;cournot&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="antitrust_probability">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers_invention_split">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy_scenario">
      <value value="&quot;counterfactual USA - more technological opportunities&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting_industries">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution_firms_to_entrepreneurs">
      <value value="&quot;equalized&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor_productivity_start">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation_likelihood">
      <value value="&quot;technological distance&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="savings_propensity_entrepreneurs">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_x">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting_x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting_y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_firms_per_industry">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_gini">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation_target_option">
      <value value="&quot;industry leader&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="calibrated_runs_counterfactual3" repetitions="1" runMetricsEveryStep="true">
    <setup>setup
set invention_stddev 0.01149187
set invention_mean 0
set imitation_distance_parameter 150.3954
set maximum_marketshare_scenario 0.149987
set target_profit_rate  0.3321438</setup>
    <go>go</go>
    <timeLimit steps="1109"/>
    <metric>invention_stddev</metric>
    <metric>invention_mean</metric>
    <metric>imitation_distance_parameter</metric>
    <metric>maximum_marketshare_scenario</metric>
    <metric>target_profit_rate</metric>
    <metric>total_output</metric>
    <metric>total_real_wages</metric>
    <metric>total_potential_output</metric>
    <metric>gini_totalpopulation</metric>
    <metric>gini_entrepreneurs</metric>
    <metric>average_productivity_lag</metric>
    <metric>employment_young_firms</metric>
    <metric>total_employment</metric>
    <metric>firm_entries</metric>
    <metric>count industries</metric>
    <metric>average_firm_productivity_growth</metric>
    <metric>standard_deviation_firm_productivity_growth</metric>
    <metric>average_firm_output_growth</metric>
    <metric>standard_deviation_firm_output_growth</metric>
    <metric>average_herfindahl_index</metric>
    <metric>combined_marketshare_firstmovers</metric>
    <metric>combined_marketshare_others</metric>
    <metric>number_of_firms</metric>
    <metric>average_price</metric>
    <metric>price_first_new_industry</metric>
    <metric>output_first_new_industry</metric>
    <metric>firms_first_new_industry</metric>
    <metric>count entrepreneurs</metric>
    <metric>number_workers_current</metric>
    <metric>gini_entrepreneurs_income</metric>
    <metric>employment_change</metric>
    <metric>mean_productivity_growth_rate</metric>
    <metric>income_share_entrepreneurs</metric>
    <metric>income_share_entrepreneurs_1939</metric>
    <metric>income_share_entrepreneurs_1980</metric>
    <metric>income_share_entrepreneurs_2018</metric>
    <metric>average_growth_rate_GDP</metric>
    <metric>stddev_growth_rate_GDP</metric>
    <metric>average_firm_employment_growth</metric>
    <metric>standard_deviation_firm_employment_growth</metric>
    <metric>average_growth_rate_income_share_entrepreneurs_until_1980</metric>
    <metric>average_growth_rate_income_share_entrepreneurs_from_1980</metric>
    <metric>reallocation_rate</metric>
    <steppedValueSet variable="random-seed" first="1" step="1" last="1000"/>
    <enumeratedValueSet variable="entrepreneurs_mobility">
      <value value="&quot;free mobility&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="target_profit_rate_option">
      <value value="&quot;market share&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_entrepreneurs">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start_in_equilibrium">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="invention_likelihood_parameter">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation_likelihood_parameter">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_workers">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="propensity_rnd">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radical_innovation_likelihood">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="technological_opportunities_scenario">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_growth_parameter">
      <value value="0.004418605"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_patch_size">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="likelihood_change_x">
      <value value="0.008"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="likelihood_change_y">
      <value value="0.008"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand_function">
      <value value="&quot;expenditure share&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_y">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum_marketshare_companies">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show_from_period">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output_expectation_parameter">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation_likelihood_parameter_entrepreneurs">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_dis_size">
      <value value="700"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output_decision">
      <value value="&quot;cournot&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="antitrust_probability">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="workers_invention_split">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy_scenario">
      <value value="&quot;counterfactual USA - no technological distance penalty&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting_industries">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution_firms_to_entrepreneurs">
      <value value="&quot;equalized&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor_productivity_start">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation_likelihood">
      <value value="&quot;technological distance&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="savings_propensity_entrepreneurs">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_x">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting_x">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting_y">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_firms_per_industry">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="calc_gini">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation_target_option">
      <value value="&quot;industry leader&quot;"/>
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
