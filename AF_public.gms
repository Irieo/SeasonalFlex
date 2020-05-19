$Title AF version 4 (18.12.2018)

$ontext

Code for the paper "Seasonal Flexibility in the European Natural Gas Market"
Iegor Riepin, LSEW BTU Cottbus-Senftenberg

This code is published under a Creative Commons Attribution-ShareAlike 4.0 License 
that provides everyone with free and perpetual permission to access, edit and share copies of this work.
See: https://creativecommons.org/licenses/by-sa/4.0/

If you use this code (or parts of it) for your research, I would appreciate it if you would cite the working paper:
Riepin, I., Müsgens, F. (2019): Seasonal Flexibility in the European Natural Gas Market, EPRG Working Paper series,
https://doi.org/10.17863/CAM.43923

Feedback, bug reportings and suggestions are highly welcome: iegor.riepin@b-tu.de

$offtext


*###############################################################################
*                                  DEFAULT OPTIONS
*###############################################################################

$eolcom #

*           YEARS TO SOLVE
$Setglobal  YEARS_inc  y2015*y2033

*###############################################################################
*                            DIRECTORIRY and FILE MANAGEMENT
*###############################################################################

* Path to input files
$set datadir    data\

* For this project all data is in one file
$Set DataIn     Monitor_AF_public

* Path to output files
$set resultdir  output\

*###############################################################################
*                                SCENARIO & TIME CONTROL
*###############################################################################

* Name of a scenario
$setglobal GlobalSCEN scen_reference_public

* Name of an output file
$set       result     AF_%GlobalSCEN%

*###############################################################################
*                                MODEL SPECIFICATIONS
*###############################################################################
*Include / exclude long-term contract agreements

$setglobal Inc_LTC      ""
$ifthen "%Inc_LTC%" ==  ""   $setglobal Exc_LTC "*"
$else                        $setglobal Exc_LTC ""
$endif

*###############################################################################
*                               DECLARING TIME MANAGEMENT SETS
*###############################################################################

SETS
    Year_all            all possible years              /y2013*y2035/
    Year(Year_all)      year modelled                   /%YEARS_inc%/
    month               set of month                    /m1*m12/

    time(month)                     aggregated time set for 1 year
    Year_M (year, month)            mapping

    m_first(month)                   first month    in year
    m_last(month)                    last month     in year
    y_first(year)                    first year     in model
    y_last(year)                     last year      in model
    m_first_global(year, month)      first month    in model
    m_last_global(year, month)       last month     in model
    ;

*###############################################################################
*                  TIME MANAGEMENT [for this project YEARS to MONTHS only]
*###############################################################################

*                               MAPPING TIME

loop(year,
    Year_M(year, month)     = yes;
    );

    m_first(month) = yes$(ord(month) eq 1);
    m_last(month)  = yes$(ord(month) eq card(month));

    y_first(year) = yes$(ord(year) eq 1);
    y_last(year)  = yes$(ord(year) eq card(year));

    m_first_global(year, month) = yes$(m_first(month) and y_first(year));
    m_last_global(year, month) = yes$(m_last(month) and y_last(year));

* Years to be included in result processing

SETS    Year_LDC(Year)   /y2015, y2020, y2025, y2030/;

SETS    Year_FlReg(Year) /y2017*y2032/
        year_FLRegStart(Year_FlReg);

year_FLRegStart(Year_FlReg) = yes$(ord(Year_FlReg) eq 1);

*                               CHECK TIME INPUT

*execute_unload '%datadir%1_check_timeinput.gdx'
*$stop

*###############################################################################
*                               DECLARING TOPOLOGY SETS
*###############################################################################

********************************  MODEL TOPOLOGY ***************************
SETS
    n                all nodes
    co(n)            only consumption nodes for gas
    ;

SETS

        prall(n)     only gas production nodes
        trgen(n)     only trading nodes             #without LNG terminals
        lng_p(n)     lng suppliers cut              #for separation of LNG and pipeline deliveries in postprocessing

        regn(n)      only regasificatino terminals
        liqn(n)      only liquifation terminals
        storall(n)   only storage nodes

    P   gas producers
        eu_p(p)      European-only producers

    W   gas wholesalers
    ;

    alias(n,m);
    alias(n,nn);
    alias(co,coco);
    alias(trgen,jtrgen);

*###############################################################################
*                               DECLARING PARAMETERS
*###############################################################################

    PARAMETERS

    #production capacities LP
        LP_prod_costs_gas(p,n)          linearized production cost
        LP_prodCap(p,n,year,month)      capacity blocks at each producer node
        LP_prod_table                   data table

    #capacity transport
        transCap(n,m)                  transportation capacity
        transMC(n,m)                   transportation marginal costs
        extra_cap(n,m, year, month)    exogenous capacity additions for all infrastructure elements

    #gas demand
        cons_ref(co, year, month)      Consumption reference points for demand function calibration

    #mapping (used for $ conditions)
        prod_to_node(P,n)               connects the each producer to its respective production node
        exp_direction(P,n)              all nodes that a producer can sell gas to
        exp_pipelines(P,n,m)            path for physical gas flow from producer to node m

        exp_eu_cut(eu_p,trgen)          European-only producers

        ws_direction_cut(W,n,co)        direction a wholesaler can deliver gas
        ws_to_home(W,n)                 connects the each wholesaler to its respective transport node
        ws_to_cons(n,co)                connects the each wholesaler to its respective consumption node
        ws_to_stor(co)                  nodes with storages
        reg_to_trans(n,m)               regasification node (terminal(m)) to its wholesaler

        prod_to_liq(p,n)                connects production node to its liquiefaction node (terminal)
        prod_w_liq(P)                   producers with liquifation terminals

    #storage
        stor_cons_cap(co,year, month)    storage capacity constraint
        stor_min_obl (co)                minimum storage level (obligation)
        stor_loss(co)                    storage gas loss per cycle
        stor_cons_in(co, year, month)    storage injection speed constraint
        stor_cons_out(co, year, month)   storage withdraw speed constraint
        st_lev_start(co)

    #other
        LTC(n,m,year, month)     long-term contracts obligation
        ;

    SCALARS

        TOP             Take-or-Pay obligation part in LTCs
        stor_cost_in    cost for gas injection into storage
        stor_cost_out   cost for gas withdrawal from storage
        SlevelF         storage stock level on first period
        SlevelL         storage stock level
        ;

*###############################################################################
*                               READING OF SETS & DATA
*###############################################################################

$onecho > ImportData.txt

        set=N               rng=nodemaps!B2       rdim=1
        set=P               rng=nodemaps!D60      rdim=1
        set=W               rng=nodemaps!D38:D57  rdim=1

        set=prall           rng=nodemaps!E4       rdim=1
        set=trgen           rng=nodemaps!E38:E57  rdim=1
        set=co              rng=nodemaps!F38:F57  rdim=1
        set=Liqn            rng=nodemaps!I4       rdim=1
        set=Regn            rng=nodemaps!L4       rdim=1
        set=storall         rng=nodemaps!M38      rdim=1

        set=lng_p           rng=nodemaps!P4      rdim=1
        set=eu_p            rng=nodemaps!D132    rdim=1
        par=exp_eu_cut      rng=nodemaps!H132    rdim=2

        par=prod_to_node    rng=nodemaps!D60    rdim=2
        par=ws_to_home      rng=nodemaps!H38    rdim=2
        par=ws_to_cons      rng=nodemaps!E38    rdim=2
        par=ws_to_stor      rng=nodemaps!M38    rdim=1
        par=reg_to_trans    rng=nodemaps!L4     rdim=2
        par=prod_to_liq     rng=nodemaps!H60    rdim=2

        par=LTC             rng=LTC_M!B2        rdim=2 cdim=2
        par=TOP             rng=scalars!D30     dim=0

        par=stor_cost_in    rng=scalars!C4  dim=0
        par=stor_cost_out   rng=scalars!C5  dim=0
        par=stor_loss       rng=scalars!B10 cdim=0 rdim=1
        par=SlevelF         rng=scalars!I4  dim=0
        par=SlevelL         rng=scalars!I5  dim=0

        par=LP_prod_table   rng=prod_cost_gas!A60   rdim=2 cdim=2

        par=transCap        rng=cap.pipeREFm!c3     cdim=1 rdim=1
        par=extra_cap       rng=cap.pipeREFm!C66    rdim=2 cdim=2
        par=transMC         rng=MTCref!c3           cdim=1 rdim=1

        par=cons_ref        rng=Dem_EUCO30!b30      cdim=2 rdim=1

        par=stor_cons_cap   rng=StorRef!B29         cdim=2 rdim=1
        par=stor_min_obl    rng=StorRef!AB6         cdim=0 rdim=1
        par=stor_cons_in    rng=StorRef!B54         cdim=2 rdim=1
        par=stor_cons_out   rng=StorRef!B79         cdim=2 rdim=1

$offecho

$call GDXXRW I=%datadir%%DataIn%.xlsx O=%datadir%%DataIn%.gdx cmerge=1 @ImportData.txt
$gdxin %datadir%%DataIn%.gdx

$LOAD P, W, N, CO,liqn, prall, trgen, storall,regn
$LOAD eu_p, exp_eu_cut, lng_p
$LOAD transCap, extra_cap, transMC
$LOAD LP_prod_table, cons_ref
$LOAD prod_to_node, ws_to_home, ws_to_cons, ws_to_stor,reg_to_trans,  prod_to_liq
$LOAD stor_cost_in, stor_cost_out, stor_loss, stor_cons_cap, stor_min_obl, stor_cons_in, stor_cons_out, SlevelF, SlevelL
$LOAD LTC, TOP

$gdxin

scalars
        conversion_factor_gas   conversion from MWhth to BCM;
*       conversion_factor_gas = 1/(10.76*1000000);
        conversion_factor_gas = 1;

Scalar
        volg             dummy value for gas shedding (for INFES checks);
        volg             = 100000;

*###############################################################################
*                               PREPARING SETS AND DATA
*###############################################################################

*********************************  GAS  ROUTES MAPPING  *********************************

        prod_w_liq(p)  = sum(liqn, prod_to_liq(p,liqn));

        ws_direction_cut(w,n,co)$(ws_to_home(w,n) and ws_to_cons(n,co)) = 1;

        exp_direction(p,trgen) = 1;
        exp_direction(p,n)$prod_to_liq(p,n) = 1;
        exp_direction(p,regn)$prod_w_liq(p) = 1;
        exp_direction(eu_p,trgen)$(not exp_eu_cut(eu_p,trgen)) = 0;

        exp_pipelines(p,n,m)$(prod_to_node(p,n) and transCap(n,m)) = 1;
        exp_pipelines(p,trgen,jtrgen)$transCap(trgen,jtrgen) = 1;
        exp_pipelines(p,prall,liqn)$(prod_to_node(p,prall) AND prod_to_liq(p,liqn)) = 1;
        exp_pipelines(p,n,regn)$prod_to_liq(p,n) = 1;
        exp_pipelines(p,regn,trgen)$reg_to_trans(regn,trgen) =1;

*********************************************************************************

        LP_prod_costs_gas(p,n)       = LP_prod_table(p,n,'cost','cost') ;
        LP_prodCap(p,n,year,month)   = LP_prod_table(p,n,'capacities (bcm)', year) / card(month);

*       Setting a storage level for the first period
        st_lev_start(co) = SlevelF * stor_cons_cap(co,'y2015','m1');

*execute_unload '%datadir%2_check_datainput.gdx';
*$stop

****************************** scaling of quantities in Gas Model

        LP_prodCap(p,n,year,month)          =     LP_prodCap(p,n,year,month)     /conversion_factor_gas ;
        transCap(n,m)                       =     transCap(n,m)                  /conversion_factor_gas ;
        extra_cap(n,m, year, month)         =     extra_cap(n,m, year, month)    /conversion_factor_gas ;
        cons_ref(co,year, month)            =     cons_ref(co,year, month)       /conversion_factor_gas ;
        stor_cons_cap(co,year, month)       =     stor_cons_cap(co,year, month)  /conversion_factor_gas ;
        stor_cons_in(co, year, month)       =     stor_cons_in(co,year, month)   /conversion_factor_gas ;
        stor_cons_out(co,year, month)       =     stor_cons_out(co,year, month)  /conversion_factor_gas ;
        st_lev_start(co)                    =     st_lev_start(co)               /conversion_factor_gas ;
        LTC(n,m,year, month)                =     LTC(n,m,year, month)           /conversion_factor_gas ;

*###############################################################################
*                               REPORTING INPUT
*###############################################################################

execute_unload '%datadir%2_check_datainput.gdx'
*$stop

*###############################################################################
*                                    VARIABLES
*###############################################################################

    POSITIVE VARIABLES

        prod_vol(p,n,m,year,month)        gas produced by a producer P in a node N and sold to node M
        export_physical(p,n,m,year,month) physical gas flows from producer that correspond to exports
        whole_sale(w,n,co,year,month)     gas delivered by a wholesaler W located in a node N(consum) its own node

        totalflow(n,m,year,month)        total physical gas flows between nodes

        st_lev(co,year,month)           stock level of gas storages
        st_in(co,year,month)            storage injection variable
        st_out(co,year,month)           sotage withdrawal variable

        gas_shed(co, year, month)
        ;

    VARIABLES

        COST_GAS                    LP - cost objective
        ;

*   Treating Finite Horizon problem - storage levels are fixed for the last period
        st_lev.fx(co,y_last,m_last)$ws_to_stor(co) = SlevelL*stor_cons_cap(co,y_last,m_last);

*###############################################################################
*                                       EQUATIONS
*###############################################################################

EQUATIONS

    LP_OBJECTIVE_GAS                         objective function (NEGATIVE WELFARE FUNCTION)

*** CAPACITY CONSTRAINTS

    LP_capCons(p,prall,year,month)          production capacity constraint
    LP_nodeTransMax(n,m,year,month)         transmission capacity constraint

*** DEMAND CONSTRAINT

    LP_demand_cons(n,year,month)            supply + stor(out) = demand + stor (in)
    LP_demand_clearing(co,year,month)       define demand

*** BALANCES ON SUPPLIER AND WHOLESALER LEVELS

    LP_Supplier_balance(p,n,year,month)     exporter sale <-> physical flow balance
    LP_Gasflow(n,m,year,month)              physical gas flow definition over a pipeline

*** STORAGE

    LP_stor_cons_cap_equ(co,year,month)    storage capacity constraint
    LP_stor_cons_in_equ(co,year,month)     injection speed constraint
    LP_stor_cons_out_equ(co,year,month)    withdrawal speed constraint
    LP_stor_balance_endofM(co,year,month)
    LP_stor_balance_endofJan(co,year)
    LP_stor_balance_Jan2015(co,year,month)

*** DUTCH FLEX
    Dutch_flexlim_up_M(n,year,month)
    Dutch_flexlim_down_M(n,year,month)
    Dutch_flexlim_up_Y(n,year,month)
    Dutch_flexlim_down_Y(n,year,month)

*** CONDITIONAL

%Inc_LTC%LP_ltc_oblig(n,m,year,month)     take-or-pay obligations
;

LP_OBJECTIVE_GAS..
            COST_GAS =e=

                    sum((year,month),

                    sum((p,prall,trgen)$(prod_to_node(p,prall) AND exp_direction(p,trgen)),
                       prod_vol(p,prall,trgen,year,month) * (LP_prod_costs_gas(p,prall)) )                              #Production costs                                               #production costs

                  + sum((p,n,m)$exp_pipelines(p,n,m),export_physical(p,n,m,year,month)*transMC(n,m))                    #deliverycosts

                  + sum((co)$ws_to_stor(co), (st_in(co,year,month) * stor_cost_in
                                              + st_out(co,year,month) * stor_cost_out))                                 #storage costs

                  + sum(co, gas_shed(co, year, month)*volg)                                                             #Dummy var
                    )
;

***************************** CAPACITY CONSTRAINTS

LP_capCons(p,prall,year,month)$prod_to_node(p,prall)..
                        sum(trgen,prod_vol(p,prall,trgen,year,month)) =l= LP_prodCap(p,prall,year,month);

LP_nodeTransMax(n,m,year,month)$(transCap(n,m)<>0)..
                        totalflow(n,m,year,month) =l= transCap(n,m)
                        + extra_cap(n,m,year,month)$extra_cap(n,m,year,month);

***************************** DEMAND CONSTRAINT

LP_demand_cons(n,year,month)$trgen(n)..
                        sum((p,m)$(prod_to_node (p,m) and exp_direction(p,n)), prod_vol(p,m,n,year,month))                   #SUPPLY

                        - sum((w,co), (whole_sale(w,n,co,year,month)$ws_direction_cut(w,n,co))) =e= 0;                       #DEMAND


LP_demand_clearing(co,year,month)..
                          cons_ref(co,year, month)                                                                              #CONSUMPTION
                        - gas_shed(co, year, month)                                                                             #dummy var
                        =e=
                          sum((w,trgen)$(ws_to_home(w,trgen)), whole_sale(w,trgen,co,year,month)$ws_direction_cut(w,trgen,co))  #DEMAND +- storage
                        + st_out(co,year,month)$ws_to_stor(co) - st_in(co,year,month)$ws_to_stor(co);

***************************** BALANCES ON SUPPLIER AND WHOLESALER LEVELS

LP_Supplier_balance (p,n,year,month)$(prod_to_node (p,n) or exp_direction(p,n))..

                          (sum(m, prod_vol(p,n,m,year,month)$(prod_to_node (p,n) and exp_direction(p,m)))
                        -  sum(m, export_physical(p,n,m,year,month)$exp_pipelines(p,n,m)) )

                        + (sum(m, export_physical(p,m,n,year,month)$exp_pipelines(p,m,n))
                        - sum(m, prod_vol(p,m,n,year,month)$(prod_to_node (p,m) and exp_direction(p,n))))

                          =e= 0;

LP_Gasflow(n,m,year,month)$transCap(n,m)..
                        totalflow(n,m,year,month) - sum(p, export_physical(p,n,m,year,month)$exp_pipelines(p,n,m)) =e= 0;

***************************** STORAGE

LP_stor_cons_cap_equ(co,year,month)$ws_to_stor(co)..
                        st_lev(co,year,month) =l= stor_cons_cap(co,year,month) * (1-stor_min_obl(co));


LP_stor_cons_in_equ(co,year,month)$ws_to_stor(co)..
                        st_in(co,year,month) =l= stor_cons_in(co,year,month);

LP_stor_cons_out_equ(co,year,month)$ws_to_stor(co)..
                        st_out(co,year,month) =l= stor_cons_out(co,year,month);


LP_stor_balance_endofM(co,year,month)$(ws_to_stor(co) and not m_first(month))..
                        st_lev(co,year,month) =l= st_lev(co,year,month-1)
                                         + (1-stor_loss(co)) * st_in(co,year,month) - st_out(co,year,month);

LP_stor_balance_endofJan(co,year)$(ws_to_stor(co) and not y_first(year))..
                         st_lev(co,year,'m1') =l= st_lev(co,year-1,'m12')
                                 + (1-stor_loss(co)) * st_in(co,year,'m1') - st_out(co,year,'m1');

LP_stor_balance_Jan2015(co,year,month)$(ws_to_stor(co) and m_first_global(year,month))..
                         st_lev(co,year,month) =l= st_lev_start(co)  + (1-stor_loss(co)) * st_in(co,year,month) - st_out(co,year,month);


***************************** LIMIT on DUTCH INDIGENOUS PRODUCTION FLEXIBILITY

Dutch_flexlim_up_M('Pn_NL',Year_FlReg,month)$(not m_first(month))..
                         sum((p,trgen), prod_vol(p,'Pn_NL',trgen,Year_FlReg,month)) =l= sum((p,trgen), prod_vol(p,'Pn_NL',trgen,Year_FlReg,month-1))*1.2;

Dutch_flexlim_down_M('Pn_NL',Year_FlReg,month)$(not m_first(month))..
                         sum((p,trgen), prod_vol(p,'Pn_NL',trgen,Year_FlReg,month)) =g= sum((p,trgen), prod_vol(p,'Pn_NL',trgen,Year_FlReg,month-1))*0.8;

Dutch_flexlim_up_Y('Pn_NL',Year_FlReg,month)$(not year_FLRegStart(Year_FlReg))..
                         sum((p,trgen), prod_vol(p,'Pn_NL',trgen,Year_FlReg,month)) =l= sum((p,trgen), prod_vol(p,'Pn_NL',trgen,Year_FlReg-1,'m12'))*1.2;

Dutch_flexlim_down_Y('Pn_NL',Year_FlReg,month)$(not year_FLRegStart(Year_FlReg))..
                         sum((p,trgen), prod_vol(p,'Pn_NL',trgen,Year_FlReg,month)) =g= sum((p,trgen), prod_vol(p,'Pn_NL',trgen,Year_FlReg-1,'m12'))*0.8;

***************************** CONDITIONAL

%Inc_LTC%LP_ltc_oblig(n,m,year,month)$(LTC(n,m,year,month))..
%Inc_LTC%               sum(p,prod_vol(p,n,m,year,month)$prod_to_node (p,n)) =g= LTC(n,m,year,month)*TOP;


*###############################################################################
*                          MODEL formulation and solving
*###############################################################################


*********************************  GAS MODEL LP *********************************
MODEL LSEW_gas_LP   /

    LP_OBJECTIVE_GAS

    LP_capCons
    LP_nodeTransMax
    LP_demand_cons
    LP_demand_clearing
    LP_Supplier_balance
    LP_Gasflow

    LP_stor_cons_cap_equ
    LP_stor_cons_in_equ
    LP_stor_cons_out_equ
    LP_stor_balance_endofM
    LP_stor_balance_endofJan
    LP_stor_balance_Jan2015

    Dutch_flexlim_up_M
    Dutch_flexlim_down_M
    Dutch_flexlim_up_Y
    Dutch_flexlim_down_Y

%Inc_LTC%LP_ltc_oblig
                /;

    option LP = CPLEX;
    option limcol = 1;
    option limrow = 1;

    LSEW_gas_LP.SCALEOPT = 1;
    LP_OBJECTIVE_GAS.scale = 1e006;

    LSEW_gas_LP.holdfixed = 1;
*    LSEW_gas_LP.OptFile = 1;

* No Nord Stream 2 scenario
* extra_cap('pn_RU','tn_DE',year,month) = 0;

SOLVE LSEW_gas_LP using LP minimizing COST_GAS;

*execute_unload '%resultdir%3_check_output.gdx' ;
*$stop

*###############################################################################
*                                  AFTERMATH
*###############################################################################


*   Display all values for storage activity

        st_lev.l(co,year,month)$(not st_lev.l(co,year,month) and ws_to_stor(co)) = eps;
        st_in.l (co,year,month)$(not  st_in.l(co,year,month)  and ws_to_stor(co)) = eps;
        st_out.l(co,year,month)$(not st_out.l(co,year,month) and ws_to_stor(co)) = eps;

*   CONVERT TO BCM primal variables

    Parameters

        prod_vol_BCM(p,n,m,year,month)
        export_physical_BCM(p,n,m,year,month)
        whole_sale_BCM(w,n,co,year,month)
        totalflow_BCM(n,m,year,month)

        st_lev_BCM(co,year,month)
        st_in_BCM(co,year,month)
        st_out_BCM(co,year,month)
        ;

        prod_vol_BCM(p,n,m,year,month)        = prod_vol.l(p,n,m,year,month)            * conversion_factor_gas;
        export_physical_BCM(p,n,m,year,month) = export_physical.l(p,n,m,year,month)     * conversion_factor_gas;
        whole_sale_BCM(w,n,co,year,month)     = whole_sale.l(w,n,co,year,month)         * conversion_factor_gas;
        totalflow_BCM(n,m,year,month)         = totalflow.l(n,m,year,month)             * conversion_factor_gas;

        st_lev_BCM(co,year,month)            = st_lev.l(co,year,month)                * conversion_factor_gas;
        st_in_BCM(co,year,month)             = st_in.l(co,year,month)                 * conversion_factor_gas;
        st_out_BCM(co,year,month)            = st_out.l(co,year,month)                * conversion_factor_gas;

*  CONVERT TO BCM input data

        LP_prodCap(p,n,year,month)      =     LP_prodCap(p,n,year,month)     *conversion_factor_gas ;
        transCap(n,m)                   =     transCap(n,m)                  *conversion_factor_gas ;
        extra_cap(n,m, year, month)     =     extra_cap(n,m, year, month)    *conversion_factor_gas ;
        cons_ref(co,year, month)        =     cons_ref(co,year, month)        *conversion_factor_gas ;
        stor_cons_cap(co,year, month)   =     stor_cons_cap(co,year, month)   *conversion_factor_gas ;
        stor_cons_in(co, year, month)   =     stor_cons_in(co,year, month)     *conversion_factor_gas ;
        stor_cons_out(co, year, month)  =     stor_cons_out(co,year, month)    *conversion_factor_gas ;
        st_lev_start(co)                =     st_lev_start(co)                 *conversion_factor_gas ;
        LTC(n,m,year, month)            =     LTC(n,m,year, month)             *conversion_factor_gas ;
        ;

*-------------------------------------------------------------------------------------------

*   AFTERMATH PARAMETERS

    PARAMETERS

*       PARAMETERS IN MONTHLY RESOLUTION
            price_gas(n,year,month)              dual variable for gas balance   (euro)           per marginal unit per month

            Production(n,year,month)             production and sales per node   (bcm)            per month
            salesLNG(liqn,year,month)            LNG sales                       (bcm)            per month
            Sales_to(n,m,year,month)             sales per node to destination   (bcm)            per month

            purchases(n,year,month)              full imports (before storage)   (bcm)            per month
            purchasesLNG(regn,year,month)        LNG imports                     (bcm)            per month
            purchasesLNG2(n,year,month)          PIPELINE imports                (bcm)            per month
            purchasesPIPE(n,year,month)          PIPELINE imports                (bcm)            per month

            imports(co,year,month)               imports (after storage)         (bcm)            per month
            consum(co,year,month)                full consumption (+inner prod)  (bcm)            per month

            storage_level_full(co,year,month)   (invludes fixed storage volume) (bcm)             per month

            Congall(n,m,year,month)              congestion of all infr. elements (%)
            CongReg(regn,year,month)             congestion of all REG terminals  (%)
            CongLiq(liqn,year,month)             congestion of all LIQ terminals  (%)
            CongStor(co,year,month)              congestions of storages          (%)

*       PARAMETERS IN ANNUAL RESOLUTION: definition

            price_gas_YEAR(n,year)               average price for gas per node  (euro)          per marginal unit per year

            Production_Year(n,year)              production per node             (bcm)           per year
            Prod_cong_Year(n,year)               congestion of prod cap          (bcm)           per year
            Sales_Year(n,m,year)                 export sales per node           (bcm)           per year

            purchases_Year(n,year)               full imports (before storage)   (bcm)           per year
            purchasLNG_Year(regn,year)           LNG imports                     (bcm)           per year
            imports_Year(co,year)                imports (after storage)         (bcm)           per year
            consum_Year(co,year)                 full consumption (+inner prod)  (bcm)           per year

            st_in_year(co,year)
            st_out_year(co,year)
            ;

*       PARAMETERS IN MONTHLY RESOLUTION: calculation

            price_gas(n,year,month)               = LP_demand_cons.m(n,year,month);

            Production(n,year,month)              = sum ((p,m), prod_vol_BCM(p,n,m,year,month));
            salesLNG(liqn,year,month)             = sum((p,regn),export_physical_BCM(p,liqn,regn,year,month));
            Sales_to(n,m,year,month)              = sum (p$prod_to_node(p,n), prod_vol_BCM(p,n,m,year,month));

            purchases(n,year,month)               = sum((p,prall)$prod_to_node(p,prall),prod_vol_BCM(p,prall,n,year,month));
            purchasesLNG(regn,year,month)         = sum((p,liqn),export_physical_BCM(p,liqn,regn,year,month));
            #purchasesLNG2(n,year,month)          = sum((p,regn),export_physical_BCM(p,regn,n,year,month));
            purchasesLNG2(trgen,year,month)       = sum((p,lng_p),prod_vol_BCM(p,lng_p,trgen,year,month));
            purchasesPIPE(n,year,month)           = purchases(n,year,month) - purchasesLNG2(n,year,month);

            imports(co,year,month)                = sum((w,n), whole_sale_BCM(w,n,co,year,month));
            consum(co,year,month)                 = sum((w,n), whole_sale_BCM(w,n,co,year,month))
                                                         + st_out_BCM(co,year,month) - st_in_BCM(co,year,month);

            storage_level_full(co,year,month)$ws_to_stor(co) = st_lev_BCM(co,year,month) + stor_min_obl(co)* stor_cons_cap(co,year, month);

            CongAll(n,m,year,month)$totalflow_BCM(n,m,year,month)
                                                     = totalflow_BCM(n,m,year,month)
                                                       /(transCap(n,m) + extra_cap(n,m,year,month))
            ;
            CongReg(regn,year,month)            = sum((p,liqn),export_physical_BCM(p,liqn,regn,year,month))
                                                        /sum(m,(transCap(regn,m) + extra_cap(regn,m,year,month)))
            ;
            CongLiq(liqn,year,month)$(sum(n,transCap(n,liqn)) > 0)
                                                        = sum((p,regn),export_physical_BCM(p,liqn,regn,year,month))
                                                         /sum(n,transCap(n,liqn))
            ;
            CongStor(co,year,month)$(stor_cons_cap(co,year, month) > 0) = storage_level_full(co,year,month) / stor_cons_cap(co,year, month);

*       PARAMETERS IN ANNUAL RESOLUTION: calculation

            price_gas_YEAR(n,year)               = sum(month$Year_M(year,month), LP_demand_cons.m(n,year,month)/12);

            Production_Year (n,year)             = sum(month$Year_M(year,month), sum((p,m), prod_vol_BCM(p,n,m,year,month)));
            Prod_cong_Year(prall,year)$(Production_Year (prall,year) >0)
                                                      = Production_Year (prall,year) / sum((p,month), LP_prodCap(p,prall,year,month));
            Sales_Year(n,m,year)                 = sum(month$Year_M(year,month), sum(p, prod_vol_BCM(p,n,m,year,month)));

            purchases_Year(n,year)               = sum(month$Year_M(year,month), purchases(n,year,month));
            purchasLNG_Year(regn,year)           = sum(month$Year_M(year,month), purchasesLNG(regn,year,month));
            imports_Year(co,year)                = sum(month$Year_M(year,month), imports(co,year,month));
            consum_Year(co,year)                 = sum(month$Year_M(year,month), consum(co,year,month));

            st_in_year(co,year)                  = sum(month$Year_M(year,month), st_in_BCM(co,year,month)*(-1));
            st_out_year(co,year)                 = sum(month$Year_M(year,month), st_out_BCM(co,year,month));


Parameter
    report_DE(*,*,year,month)     report parameter for country supply mix
    report_IT(*,*,year,month)
    report_UK(*,*,year,month)
    report_FR(*,*,year,month)
    report_PL(*,*,year,month)
    report_CZ(*,*,year,month)
    report_AT(*,*,year,month)
    report_SK(*,*,year,month)
    report_BE(*,*,year,month)
    report_HU(*,*,year,month)
    report_RO(*,*,year,month)
    report_DK(*,*,year,month)
    report_Baltic(*,*,year,month)
    report_Balkan(*,*,year,month)
    report_Iberia(*,*,year,month)
    report_SE(*,*,year,month)
    report_NL(*,*,year,month)
    report_EU(*,*,year,month)

    report_DE_LDC(year,month,*,*)
    report_UK_LDC(year,month,*,*)
    report_NL_LDC(year,month,*,*)
    report_EU_LDC(year,month,*,*)

    report_CV_DE(*,*,year)
    report_CV_UK(*,*,year)
    report_CV_NL(*,*,year)
    report_CV_EU(*,*,year)
    ;

    purchasesPIPE(n,year,month)$(not purchasesPIPE(n,year,month))                = EPS;
    purchasesLNG2(n,year,month)$(not purchasesLNG2(n,year,month))                = EPS;
    st_in_BCM(co,year,month)$(not st_in_BCM(co,year,month) and ws_to_stor(co))   = EPS;
    st_out_BCM(co,year,month)$(not st_out_BCM(co,year,month) and ws_to_stor(co)) = EPS;
    st_in_BCM(co,year,month)                                                     = st_in_BCM(co,year,month)*(-1);

    report_DE('Germany','Inner_prod',year,month)  = sum(p, prod_vol_BCM(p,'pn_DE','tn_DE',year,month)) + EPS;
    report_DE('Germany','PIPE_import',year,month) = purchasesPIPE('tn_DE',year,month) - sum(p, prod_vol_BCM(p,'pn_DE','tn_DE',year,month));
    report_DE('Germany','LNG_import',year,month)  = purchasesLNG2('tn_DE',year,month);
    report_DE('Germany','withdrawal',year,month)  = st_out_BCM('cn_DE',year,month);
    report_DE('Germany','CONSUMPTION',year,month) = consum('cn_DE',year,month);
    report_DE('Germany','injection',year,month)   = st_in_BCM('cn_DE',year,month);

    report_IT('Italy','Inner_prod',year,month)   = sum(p, prod_vol_BCM(p,'pn_IT','tn_IT',year,month)) + EPS;
    report_IT('Italy','PIPE_import',year,month)  = purchasesPIPE('tn_IT',year,month) - sum(p, prod_vol_BCM(p,'pn_IT','tn_IT',year,month));
    report_IT('Italy','LNG_import',year,month)   = purchasesLNG2('tn_IT',year,month);
    report_IT('Italy','withdrawal',year,month)   = st_out_BCM('cn_IT',year,month);
    report_IT('Italy','CONSUMPTION',year,month)  = consum('cn_IT',year,month);
    report_IT('Italy','injection',year,month)    = st_in_BCM('cn_IT',year,month);

    report_UK('UK','Inner_prod',year,month)   = sum(p, prod_vol_BCM(p,'pn_UK','tn_UK',year,month)) + EPS;
    report_UK('UK','PIPE_import',year,month)  = purchasesPIPE('tn_UK',year,month) - sum(p, prod_vol_BCM(p,'pn_UK','tn_UK',year,month));
    report_UK('UK','LNG_import',year,month)   = purchasesLNG2('tn_UK',year,month);
    report_UK('UK','withdrawal',year,month)   = st_out_BCM('cn_UK',year,month);
    report_UK('UK','CONSUMPTION',year,month)  = consum('cn_UK',year,month);
    report_UK('UK','injection',year,month)    = st_in_BCM('cn_UK',year,month);

    report_FR('France','Inner_prod',year,month)   = sum(p, prod_vol_BCM(p,'pn_FR','tn_FR',year,month)) + EPS;
    report_FR('France','PIPE_import',year,month)  = purchasesPIPE('tn_FR',year,month) - sum(p, prod_vol_BCM(p,'pn_FR','tn_FR',year,month));
    report_FR('France','LNG_import',year,month)   = purchasesLNG2('tn_FR',year,month);
    report_FR('France','withdrawal',year,month)   = st_out_BCM('cn_FR',year,month);
    report_FR('France','CONSUMPTION',year,month)  = consum('cn_FR',year,month);
    report_FR('France','injection',year,month)    = st_in_BCM('cn_FR',year,month);

    report_PL('PL','Inner_prod',year,month)   = sum(p, prod_vol_BCM(p,'pn_PL','tn_PL',year,month)) + EPS;
    report_PL('PL','PIPE_import',year,month)  = purchasesPIPE('tn_PL',year,month) - sum(p, prod_vol_BCM(p,'pn_PL','tn_PL',year,month));
    report_PL('PL','LNG_import',year,month)   = purchasesLNG2('tn_PL',year,month);
    report_PL('PL','withdrawal',year,month)   = st_out_BCM('cn_PL',year,month);
    report_PL('PL','CONSUMPTION',year,month)  = consum('cn_PL',year,month);
    report_PL('PL','injection',year,month)    = st_in_BCM('cn_PL',year,month);

    report_CZ('CzechRep','Inner_prod',year,month)   = sum(p, prod_vol_BCM(p,'pn_CZ','tn_CZ',year,month)) + EPS;
    report_CZ('CzechRep','PIPE_import',year,month)  = purchasesPIPE('tn_CZ',year,month) - sum(p, prod_vol_BCM(p,'pn_CZ','tn_CZ',year,month));
    report_CZ('CzechRep','LNG_import',year,month)   = purchasesLNG2('tn_CZ',year,month);
    report_CZ('CzechRep','withdrawal',year,month)   = st_out_BCM('cn_CZ',year,month);
    report_CZ('CzechRep','CONSUMPTION',year,month)  = consum('cn_CZ',year,month);
    report_CZ('CzechRep','injection',year,month)    = st_in_BCM('cn_CZ',year,month);

    report_AT('Austria','Inner_prod',year,month)   = sum(p, prod_vol_BCM(p,'pn_AT','tn_AT',year,month)) + EPS;
    report_AT('Austria','PIPE_import',year,month)  = purchasesPIPE('tn_AT',year,month) - sum(p, prod_vol_BCM(p,'pn_AT','tn_AT',year,month));
    report_AT('Austria','LNG_import',year,month)   = purchasesLNG2('tn_AT',year,month);
    report_AT('Austria','withdrawal',year,month)   = st_out_BCM('cn_AT',year,month);
    report_AT('Austria','CONSUMPTION',year,month)  = consum('cn_AT',year,month);
    report_AT('Austria','injection',year,month)    = st_in_BCM('cn_AT',year,month);

    report_SK('Slovakia','Inner_prod',year,month)   = sum(p, prod_vol_BCM(p,'pn_SK','tn_SK',year,month)) + EPS;
    report_SK('Slovakia','PIPE_import',year,month)  = purchasesPIPE('tn_SK',year,month) - sum(p, prod_vol_BCM(p,'pn_SK','tn_SK',year,month));
    report_SK('Slovakia','LNG_import',year,month)   = purchasesLNG2('tn_SK',year,month);
    report_SK('Slovakia','withdrawal',year,month)   = st_out_BCM('cn_SK',year,month);
    report_SK('Slovakia','CONSUMPTION',year,month)  = consum('cn_SK',year,month);
    report_SK('Slovakia','injection',year,month)    = st_in_BCM('cn_SK',year,month);

    report_BE('Belgium','Inner_prod',year,month)   = sum(p, prod_vol_BCM(p,'pn_BE','tn_BE',year,month)) + EPS;
    report_BE('Belgium','PIPE_import',year,month)  = purchasesPIPE('tn_BE',year,month) - sum(p, prod_vol_BCM(p,'pn_BE','tn_BE',year,month));
    report_BE('Belgium','LNG_import',year,month)   = purchasesLNG2('tn_BE',year,month);
    report_BE('Belgium','withdrawal',year,month)   = st_out_BCM('cn_BE',year,month);
    report_BE('Belgium','CONSUMPTION',year,month)  = consum('cn_BE',year,month);
    report_BE('Belgium','injection',year,month)    = st_in_BCM('cn_BE',year,month);

    report_HU('Hungary','Inner_prod',year,month)   = sum(p, prod_vol_BCM(p,'pn_HU','tn_HU',year,month)) + EPS;
    report_HU('Hungary','PIPE_import',year,month)  = purchasesPIPE('tn_HU',year,month) - sum(p, prod_vol_BCM(p,'pn_HU','tn_HU',year,month));
    report_HU('Hungary','LNG_import',year,month)   = purchasesLNG2('tn_HU',year,month);
    report_HU('Hungary','withdrawal',year,month)   = st_out_BCM('cn_HU',year,month);
    report_HU('Hungary','CONSUMPTION',year,month)  = consum('cn_HU',year,month);
    report_HU('Hungary','injection',year,month)    = st_in_BCM('cn_HU',year,month);

    report_RO('Romania','Inner_prod',year,month)   = sum(p, prod_vol_BCM(p,'pn_RO','tn_RO',year,month)) + EPS;
    report_RO('Romania','PIPE_import',year,month)  = purchasesPIPE('tn_RO',year,month) - sum(p, prod_vol_BCM(p,'pn_RO','tn_RO',year,month));
    report_RO('Romania','LNG_import',year,month)   = purchasesLNG2('tn_RO',year,month);
    report_RO('Romania','withdrawal',year,month)   = st_out_BCM('cn_RO',year,month);
    report_RO('Romania','CONSUMPTION',year,month)  = consum('cn_RO',year,month);
    report_RO('Romania','injection',year,month)    = st_in_BCM('cn_RO',year,month);

    report_DK('Denmark','Inner_prod',year,month)   = sum(p, prod_vol_BCM(p,'pn_DK','tn_DK',year,month)) + EPS;
    report_DK('Denmark','PIPE_import',year,month)  = purchasesPIPE('tn_DK',year,month) - sum(p, prod_vol_BCM(p,'pn_DK','tn_DK',year,month));
    report_DK('Denmark','LNG_import',year,month)   = purchasesLNG2('tn_DK',year,month);
    report_DK('Denmark','withdrawal',year,month)   = st_out_BCM('cn_DK',year,month);
    report_DK('Denmark','CONSUMPTION',year,month)  = consum('cn_DK',year,month);
    report_DK('Denmark','injection',year,month)    = st_in_BCM('cn_DK',year,month);

    report_Baltic('Baltic','Inner_prod',year,month)   = sum(p, prod_vol_BCM(p,'pn_Baltic','tn_Baltic',year,month)) + EPS;
    report_Baltic('Baltic','PIPE_import',year,month)  = purchasesPIPE('tn_Baltic',year,month) - sum(p, prod_vol_BCM(p,'pn_Baltic','tn_Baltic',year,month));
    report_Baltic('Baltic','LNG_import',year,month)   = purchasesLNG2('tn_Baltic',year,month);
    report_Baltic('Baltic','withdrawal',year,month)   = st_out_BCM('cn_Baltic',year,month);
    report_Baltic('Baltic','CONSUMPTION',year,month)  = consum('cn_Baltic',year,month);
    report_Baltic('Baltic','injection',year,month)    = st_in_BCM('cn_Baltic',year,month);

    report_Balkan('Balkan','Inner_prod',year,month)   = sum(p, prod_vol_BCM(p,'pn_Balkan','tn_Balkan',year,month)) + EPS;
    report_Balkan('Balkan','PIPE_import',year,month)  = purchasesPIPE('tn_Balkan',year,month) - sum(p, prod_vol_BCM(p,'pn_Balkan','tn_Balkan',year,month));
    report_Balkan('Balkan','LNG_import',year,month)   = purchasesLNG2('tn_Balkan',year,month);
    report_Balkan('Balkan','withdrawal',year,month)   = st_out_BCM('cn_Balkan',year,month);
    report_Balkan('Balkan','CONSUMPTION',year,month)  = consum('cn_Balkan',year,month);
    report_Balkan('Balkan','injection',year,month)    = st_in_BCM('cn_Balkan',year,month);

    report_Iberia('Iberia','Inner_prod',year,month)   = sum(p, prod_vol_BCM(p,'pn_Iberia','tn_Iberia',year,month)) + EPS;
    report_Iberia('Iberia','PIPE_import',year,month)  = purchasesPIPE('tn_Iberia',year,month) - sum(p, prod_vol_BCM(p,'pn_Iberia','tn_Iberia',year,month));
    report_Iberia('Iberia','LNG_import',year,month)   = purchasesLNG2('tn_Iberia',year,month);
    report_Iberia('Iberia','withdrawal',year,month)   = st_out_BCM('cn_Iberia',year,month);
    report_Iberia('Iberia','CONSUMPTION',year,month)  = consum('cn_Iberia',year,month);
    report_Iberia('Iberia','injection',year,month)    = st_in_BCM('cn_Iberia',year,month);

    report_SE('Sweden','Inner_prod',year,month)   = 0 + EPS;
    report_SE('Sweden','PIPE_import',year,month)  = purchasesPIPE('tn_SE',year,month) - 0 + EPS;
    report_SE('Sweden','LNG_import',year,month)   = purchasesLNG2('tn_SE',year,month);
    report_SE('Sweden','withdrawal',year,month)   = st_out_BCM('cn_SE',year,month);
    report_SE('Sweden','CONSUMPTION',year,month)  = consum('cn_SE',year,month);
    report_SE('Sweden','injection',year,month)    = st_in_BCM('cn_SE',year,month);

*    report_NL('Netherlands','Inner_prod',year,month)   = sum(p, prod_vol_BCM(p,'Pn_NL','tn_NL',year,month)) + EPS;
    report_NL('Netherlands','Inner_prod',year,month)   = sum((p,m), prod_vol_BCM(p,'Pn_NL',m,year,month)) + EPS;
    report_NL('Netherlands','PIPE_import',year,month)  = purchasesPIPE('tn_NL',year,month) - sum(p, prod_vol_BCM(p,'Pn_NL','tn_NL',year,month)) + EPS;
    report_NL('Netherlands','LNG_import',year,month)   = purchasesLNG2('tn_NL',year,month);
    report_NL('Netherlands','withdrawal',year,month)   = st_out_BCM('cn_NL',year,month);
    report_NL('Netherlands','CONSUMPTION',year,month)  = consum('cn_NL',year,month);
    report_NL('Netherlands','injection',year,month)    = st_in_BCM('cn_NL',year,month);
    report_NL('Netherlands','exports',year,month)       = sum((p,m), prod_vol_BCM(p,'Pn_NL',m,year,month)) - sum(p, prod_vol_BCM(p,'Pn_NL','tn_NL',year,month));

    report_EU('Europe','Inner_prod',year,month) =   report_DE('Germany','Inner_prod',year,month)
                                                + report_IT('Italy','Inner_prod',year,month)
                                                + report_UK('UK','Inner_prod',year,month)
                                                + report_FR('France','Inner_prod',year,month)
                                                + report_PL('PL','Inner_prod',year,month)
                                                + report_CZ('CzechRep','Inner_prod',year,month)
                                                + report_AT('Austria','Inner_prod',year,month)
                                                + report_SK('Slovakia','Inner_prod',year,month)
                                                + report_BE('Belgium','Inner_prod',year,month)
                                                + report_HU('Hungary','Inner_prod',year,month)
                                                + report_RO('Romania','Inner_prod',year,month)
                                                + report_DK('Denmark','Inner_prod',year,month)
                                                + report_Baltic('Baltic','Inner_prod',year,month)
                                                + report_Balkan('Balkan','Inner_prod',year,month)
                                                + report_Iberia('Iberia','Inner_prod',year,month)
                                                + report_SE('Sweden','Inner_prod',year,month)
                                                + report_NL('Netherlands','Inner_prod',year,month);

    report_EU('Europe','PIPE_import',year,month)  = report_DE('Germany','PIPE_import',year,month)
                                                + report_IT('Italy','PIPE_import',year,month)
                                                + report_UK('UK','PIPE_import',year,month)
                                                + report_FR('France','PIPE_import',year,month)
                                                + report_PL('PL','PIPE_import',year,month)
                                                + report_CZ('CzechRep','PIPE_import',year,month)
                                                + report_AT('Austria','PIPE_import',year,month)
                                                + report_SK('Slovakia','PIPE_import',year,month)
                                                + report_BE('Belgium','PIPE_import',year,month)
                                                + report_HU('Hungary','PIPE_import',year,month)
                                                + report_RO('Romania','PIPE_import',year,month)
                                                + report_DK('Denmark','PIPE_import',year,month)
                                                + report_Baltic('Baltic','PIPE_import',year,month)
                                                + report_Balkan('Balkan','PIPE_import',year,month)
                                                + report_Iberia('Iberia','PIPE_import',year,month)
                                                + report_SE('Sweden','PIPE_import',year,month)
                                                + report_NL('Netherlands','PIPE_import',year,month);

    report_EU('Europe','LNG_import',year,month)   = report_DE('Germany','LNG_import',year,month)
                                                + report_IT('Italy','LNG_import',year,month)
                                                + report_UK('UK','LNG_import',year,month)
                                                + report_FR('France','LNG_import',year,month)
                                                + report_PL('PL','LNG_import',year,month)
                                                + report_CZ('CzechRep','LNG_import',year,month)
                                                + report_AT('Austria','LNG_import',year,month)
                                                + report_SK('Slovakia','LNG_import',year,month)
                                                + report_BE('Belgium','LNG_import',year,month)
                                                + report_HU('Hungary','LNG_import',year,month)
                                                + report_RO('Romania','LNG_import',year,month)
                                                + report_DK('Denmark','LNG_import',year,month)
                                                + report_Baltic('Baltic','LNG_import',year,month)
                                                + report_Balkan('Balkan','LNG_import',year,month)
                                                + report_Iberia('Iberia','LNG_import',year,month)
                                                + report_SE('Sweden','LNG_import',year,month)
                                                + report_NL('Netherlands','LNG_import',year,month);

    report_EU('Europe','withdrawal',year,month)   = report_DE('Germany','withdrawal',year,month)
                                                + report_IT('Italy','withdrawal',year,month)
                                                + report_UK('UK','withdrawal',year,month)
                                                + report_FR('France','withdrawal',year,month)
                                                + report_PL('PL','withdrawal',year,month)
                                                + report_CZ('CzechRep','withdrawal',year,month)
                                                + report_AT('Austria','withdrawal',year,month)
                                                + report_SK('Slovakia','withdrawal',year,month)
                                                + report_BE('Belgium','withdrawal',year,month)
                                                + report_HU('Hungary','withdrawal',year,month)
                                                + report_RO('Romania','withdrawal',year,month)
                                                + report_DK('Denmark','withdrawal',year,month)
                                                + report_Baltic('Baltic','withdrawal',year,month)
                                                + report_Balkan('Balkan','withdrawal',year,month)
                                                + report_Iberia('Iberia','withdrawal',year,month)
                                                + report_SE('Sweden','withdrawal',year,month)
                                                + report_NL('Netherlands','withdrawal',year,month);

    report_EU('Europe','CONSUMPTION',year,month)  = report_DE('Germany','consumption',year,month)
                                                + report_IT('Italy','consumption',year,month)
                                                + report_UK('UK','consumption',year,month)
                                                + report_FR('France','consumption',year,month)
                                                + report_PL('PL','consumption',year,month)
                                                + report_CZ('CzechRep','consumption',year,month)
                                                + report_AT('Austria','consumption',year,month)
                                                + report_SK('Slovakia','consumption',year,month)
                                                + report_BE('Belgium','consumption',year,month)
                                                + report_HU('Hungary','consumption',year,month)
                                                + report_RO('Romania','consumption',year,month)
                                                + report_DK('Denmark','consumption',year,month)
                                                + report_Baltic('Baltic','consumption',year,month)
                                                + report_Balkan('Balkan','consumption',year,month)
                                                + report_Iberia('Iberia','consumption',year,month)
                                                + report_SE('Sweden','CONSUMPTION',year,month)
                                                + report_NL('Netherlands','consumption',year,month);

    report_EU('Europe','injection',year,month)    = report_DE('Germany','injection',year,month)
                                                + report_IT('Italy','injection',year,month)
                                                + report_UK('UK','injection',year,month)
                                                + report_FR('France','injection',year,month)
                                                + report_PL('PL','injection',year,month)
                                                + report_CZ('CzechRep','injection',year,month)
                                                + report_AT('Austria','injection',year,month)
                                                + report_SK('Slovakia','injection',year,month)
                                                + report_BE('Belgium','injection',year,month)
                                                + report_HU('Hungary','injection',year,month)
                                                + report_RO('Romania','injection',year,month)
                                                + report_DK('Denmark','injection',year,month)
                                                + report_Baltic('Baltic','injection',year,month)
                                                + report_Balkan('Balkan','injection',year,month)
                                                + report_Iberia('Iberia','injection',year,month)
                                                + report_SE('Sweden','injection',year,month)
                                                + report_NL('Netherlands','injection',year,month);


**************************************** LDC ***********************************************

    report_DE_LDC(Year_LDC,month, 'Germany','Inner_prod')  = report_DE('Germany','Inner_prod',Year_LDC,month);
    report_DE_LDC(Year_LDC,month, 'Germany','PIPE_import') = report_DE('Germany','PIPE_import',Year_LDC,month);
    report_DE_LDC(Year_LDC,month, 'Germany','LNG_import')  = report_DE('Germany','LNG_import',Year_LDC,month);
    report_DE_LDC(Year_LDC,month, 'Germany','withdrawal')  = report_DE('Germany','withdrawal',Year_LDC,month);
    report_DE_LDC(Year_LDC,month, 'Germany','supply')      =  report_DE('Germany','Inner_prod',Year_LDC,month)
                                                        + report_DE('Germany','PIPE_import',Year_LDC,month)
                                                        + report_DE('Germany','LNG_import',Year_LDC,month)
                                                        + report_DE('Germany','withdrawal',Year_LDC,month);
    report_DE_LDC(Year_LDC,month, 'Germany','CONSUMPTION') = report_DE('Germany','CONSUMPTION',Year_LDC,month);

    report_UK_LDC(Year_LDC,month, 'UK','Inner_prod')  = report_UK('UK','Inner_prod',Year_LDC,month);
    report_UK_LDC(Year_LDC,month, 'UK','PIPE_import') = report_UK('UK','PIPE_import',Year_LDC,month);
    report_UK_LDC(Year_LDC,month, 'UK','LNG_import')  = report_UK('UK','LNG_import',Year_LDC,month);
    report_UK_LDC(Year_LDC,month, 'UK','withdrawal')  = report_UK('UK','withdrawal',Year_LDC,month);
    report_UK_LDC(Year_LDC,month, 'UK','supply')      =  report_UK('UK','Inner_prod',Year_LDC,month)
                                                        + report_UK('UK','PIPE_import',Year_LDC,month)
                                                        + report_UK('UK','LNG_import',Year_LDC,month)
                                                        + report_UK('UK','withdrawal',Year_LDC,month);
    report_UK_LDC(Year_LDC,month, 'UK','CONSUMPTION') = report_UK('UK','CONSUMPTION',Year_LDC,month);

    report_NL_LDC(Year_LDC,month, 'Netherlands','Inner_prod')  = report_NL('Netherlands','Inner_prod',Year_LDC,month);
    report_NL_LDC(Year_LDC,month, 'Netherlands','PIPE_import') = report_NL('Netherlands','PIPE_import',Year_LDC,month);
    report_NL_LDC(Year_LDC,month, 'Netherlands','LNG_import')  = report_NL('Netherlands','LNG_import',Year_LDC,month);
    report_NL_LDC(Year_LDC,month, 'Netherlands','withdrawal')  = report_NL('Netherlands','withdrawal',Year_LDC,month);
    report_NL_LDC(Year_LDC,month, 'Netherlands','supply')      =  report_NL('Netherlands','Inner_prod',Year_LDC,month)
                                                        + report_NL('Netherlands','PIPE_import',Year_LDC,month)
                                                        + report_NL('Netherlands','LNG_import',Year_LDC,month)
                                                        + report_NL('Netherlands','withdrawal',Year_LDC,month);
    report_NL_LDC(Year_LDC,month, 'Netherlands','CONSUMPTION') = report_NL('Netherlands','CONSUMPTION',Year_LDC,month);


    report_EU_LDC(Year_LDC,month, 'Europe','Inner_prod')  = report_EU('Europe','Inner_prod',Year_LDC,month);
    report_EU_LDC(Year_LDC,month, 'Europe','PIPE_import') = report_EU('Europe','PIPE_import',Year_LDC,month);
    report_EU_LDC(Year_LDC,month, 'Europe','LNG_import')  = report_EU('Europe','LNG_import',Year_LDC,month);
    report_EU_LDC(Year_LDC,month, 'Europe','withdrawal')  = report_EU('Europe','withdrawal',Year_LDC,month);
    report_EU_LDC(Year_LDC,month, 'Europe','supply')      =  report_EU('Europe','Inner_prod',Year_LDC,month)
                                                        + report_EU('Europe','PIPE_import',Year_LDC,month)
                                                        + report_EU('Europe','LNG_import',Year_LDC,month)
                                                        + report_EU('Europe','withdrawal',Year_LDC,month);
    report_EU_LDC(Year_LDC,month, 'Europe','CONSUMPTION') = report_EU('Europe','CONSUMPTION',Year_LDC,month);


************************************************** CV ****************************************************

    report_CV_DE('Germany', 'sum_inner',year) = sum(month, report_DE('Germany','Inner_prod',year,month)) + EPS;
    report_CV_DE('Germany', 'sum_pipe',year)  = sum(month, report_DE('Germany','PIPE_import',year,month)) + EPS;
    report_CV_DE('Germany', 'sum_lng',year)   = sum(month, report_DE('Germany','LNG_import',year,month)) + EPS;
    report_CV_DE('Germany', 'sum_stor',year)  = sum(month, report_DE('Germany','withdrawal',year,month)) + EPS;
    report_CV_DE('Germany', 'sum_total',year) = report_CV_DE('Germany', 'sum_inner', year)
                                                + report_CV_DE('Germany', 'sum_pipe', year)
                                                + report_CV_DE('Germany', 'sum_lng', year)
                                                + report_CV_DE('Germany', 'sum_stor', year);

    report_CV_DE('Germany', 'perc_inner',year) =  report_CV_DE('Germany', 'sum_inner', year) / report_CV_DE('Germany', 'sum_total', year) ;
    report_CV_DE('Germany', 'perc_pipe',year)  =  report_CV_DE('Germany', 'sum_pipe', year) / report_CV_DE('Germany', 'sum_total', year) ;
    report_CV_DE('Germany', 'perc_lng',year)   =  report_CV_DE('Germany', 'sum_lng', year)  / report_CV_DE('Germany', 'sum_total', year) ;
    report_CV_DE('Germany', 'perc_stor',year)  =  report_CV_DE('Germany', 'sum_stor', year)  / report_CV_DE('Germany', 'sum_total', year) ;

    report_CV_DE('Germany', 'mean_inner',year) =   report_CV_DE('Germany', 'sum_inner', year)/ card (month);
    report_CV_DE('Germany', 'mean_pipe',year)  =   report_CV_DE('Germany', 'sum_pipe', year) / card (month);
    report_CV_DE('Germany', 'mean_lng',year)   =   report_CV_DE('Germany', 'sum_lng', year)  / card (month);
    report_CV_DE('Germany', 'mean_stor',year)  =   report_CV_DE('Germany', 'sum_stor', year)  / card (month);

    report_CV_DE('Germany', 'stdev_inner',year) = sqrt(
                                                sum(month, sqr(report_DE('Germany','Inner_prod',year,month) - report_CV_DE('Germany', 'mean_inner', year)))
                                                / (card (month)-1) );
    report_CV_DE('Germany', 'stdev_pipe',year) = sqrt(
                                                sum(month, sqr(report_DE('Germany','PIPE_import',year,month) - report_CV_DE('Germany', 'mean_pipe', year)))
                                                / (card (month)-1) );
    report_CV_DE('Germany', 'stdev_lng',year) = sqrt(
                                                sum(month, sqr(report_DE('Germany','LNG_import',year,month) - report_CV_DE('Germany', 'mean_lng', year)))
                                                / (card (month)-1) );
    report_CV_DE('Germany', 'stdev_stor',year) = sqrt(
                                                sum(month, sqr(report_DE('Germany','withdrawal',year,month) - report_CV_DE('Germany', 'mean_stor', year)))
                                                / (card (month)-1) );

    report_CV_DE('Germany', 'cv_inner', year)$(report_CV_DE('Germany', 'mean_inner', year) > 0)
                                             = report_CV_DE('Germany', 'stdev_inner', year) / report_CV_DE('Germany', 'mean_inner', year);
    report_CV_DE('Germany', 'cv_pipe', year)$(report_CV_DE('Germany', 'mean_pipe', year) > 0)
                                           = report_CV_DE('Germany', 'stdev_pipe', year) / report_CV_DE('Germany', 'mean_pipe', year);
    report_CV_DE('Germany', 'cv_lng', year)$(report_CV_DE('Germany', 'mean_lng', year) > 0)
                                            = report_CV_DE('Germany', 'stdev_lng', year) / report_CV_DE('Germany', 'mean_lng', year);
    report_CV_DE('Germany', 'cv_stor', year)$(report_CV_DE('Germany', 'mean_stor', year) > 0)
                                            = report_CV_DE('Germany', 'stdev_stor', year) / report_CV_DE('Germany', 'mean_stor', year);

    report_CV_DE('Germany', 'cvs_inner', year) =  report_CV_DE('Germany', 'cv_inner', year) * report_CV_DE('Germany', 'perc_inner',year);
    report_CV_DE('Germany', 'cvs_pipe', year)  =  report_CV_DE('Germany', 'cv_pipe', year) *  report_CV_DE('Germany', 'perc_pipe',year);
    report_CV_DE('Germany', 'cvs_lng', year)   =  report_CV_DE('Germany', 'cv_lng', year) *  report_CV_DE('Germany', 'perc_lng',year);
    report_CV_DE('Germany', 'cvs_stor', year)  =  report_CV_DE('Germany', 'cv_stor', year) *  report_CV_DE('Germany', 'perc_stor',year);

************************* UK

    report_CV_UK('UK', 'sum_inner',year) = sum(month, report_UK('UK','Inner_prod',year,month)) + EPS;
    report_CV_UK('UK', 'sum_pipe',year)  = sum(month, report_UK('UK','PIPE_import',year,month)) + EPS;
    report_CV_UK('UK', 'sum_lng',year)   = sum(month, report_UK('UK','LNG_import',year,month)) + EPS;
    report_CV_UK('UK', 'sum_stor',year)  = sum(month, report_UK('UK','withdrawal',year,month)) + EPS;
    report_CV_UK('UK', 'sum_total',year) = report_CV_UK('UK', 'sum_inner', year)
                                                + report_CV_UK('UK', 'sum_pipe', year)
                                                + report_CV_UK('UK', 'sum_lng', year)
                                                + report_CV_UK('UK', 'sum_stor', year);

    report_CV_UK('UK', 'perc_inner',year) =  report_CV_UK('UK', 'sum_inner', year) / report_CV_UK('UK', 'sum_total', year) ;
    report_CV_UK('UK', 'perc_pipe',year)  =  report_CV_UK('UK', 'sum_pipe', year) / report_CV_UK('UK', 'sum_total', year) ;
    report_CV_UK('UK', 'perc_lng',year)   =  report_CV_UK('UK', 'sum_lng', year)  / report_CV_UK('UK', 'sum_total', year) ;
    report_CV_UK('UK', 'perc_stor',year)  =  report_CV_UK('UK', 'sum_stor', year)  / report_CV_UK('UK', 'sum_total', year) ;

    report_CV_UK('UK', 'mean_inner',year) =   report_CV_UK('UK', 'sum_inner', year)/ card (month);
    report_CV_UK('UK', 'mean_pipe',year)  =   report_CV_UK('UK', 'sum_pipe', year) / card (month);
    report_CV_UK('UK', 'mean_lng',year)   =   report_CV_UK('UK', 'sum_lng', year)  / card (month);
    report_CV_UK('UK', 'mean_stor',year)  =   report_CV_UK('UK', 'sum_stor', year)  / card (month);

    report_CV_UK('UK', 'stdev_inner',year) = sqrt(
                                                sum(month, sqr(report_UK('UK','Inner_prod',year,month) - report_CV_UK('UK', 'mean_inner', year)))
                                                / (card (month)-1) );
    report_CV_UK('UK', 'stdev_pipe',year) = sqrt(
                                                sum(month, sqr(report_UK('UK','PIPE_import',year,month) - report_CV_UK('UK', 'mean_pipe', year)))
                                                / (card (month)-1) );
    report_CV_UK('UK', 'stdev_lng',year) = sqrt(
                                                sum(month, sqr(report_UK('UK','LNG_import',year,month) - report_CV_UK('UK', 'mean_lng', year)))
                                                / (card (month)-1) );
    report_CV_UK('UK', 'stdev_stor',year) = sqrt(
                                                sum(month, sqr(report_UK('UK','withdrawal',year,month) - report_CV_UK('UK', 'mean_stor', year)))
                                                / (card (month)-1) );

    report_CV_UK('UK', 'cv_inner', year)$(report_CV_UK('UK', 'mean_inner', year) > 0)
                                             = report_CV_UK('UK', 'stdev_inner', year) / report_CV_UK('UK', 'mean_inner', year);
    report_CV_UK('UK', 'cv_pipe', year)$(report_CV_UK('UK', 'mean_pipe', year) > 0)
                                           = report_CV_UK('UK', 'stdev_pipe', year) / report_CV_UK('UK', 'mean_pipe', year);
    report_CV_UK('UK', 'cv_lng', year)$(report_CV_UK('UK', 'mean_lng', year) > 0)
                                            = report_CV_UK('UK', 'stdev_lng', year) / report_CV_UK('UK', 'mean_lng', year);
    report_CV_UK('UK', 'cv_stor', year)$(report_CV_UK('UK', 'mean_stor', year) > 0)
                                            = report_CV_UK('UK', 'stdev_stor', year) / report_CV_UK('UK', 'mean_stor', year);

    report_CV_UK('UK', 'cvs_inner', year) =  report_CV_UK('UK', 'cv_inner', year) * report_CV_UK('UK', 'perc_inner',year);
    report_CV_UK('UK', 'cvs_pipe', year)  =  report_CV_UK('UK', 'cv_pipe', year) *  report_CV_UK('UK', 'perc_pipe',year);
    report_CV_UK('UK', 'cvs_lng', year)   =  report_CV_UK('UK', 'cv_lng', year) *  report_CV_UK('UK', 'perc_lng',year);
    report_CV_UK('UK', 'cvs_stor', year)  =  report_CV_UK('UK', 'cv_stor', year) *  report_CV_UK('UK', 'perc_stor',year);

************************* NL

    report_cv_NL('Netherlands', 'sum_inner',year) = sum(month, report_NL('Netherlands','Inner_prod',year,month)) + EPS;
    report_cv_NL('Netherlands', 'sum_pipe',year)  = sum(month, report_NL('Netherlands','PIPE_import',year,month)) + EPS;
    report_cv_NL('Netherlands', 'sum_lng',year)   = sum(month, report_NL('Netherlands','LNG_import',year,month)) + EPS;
    report_cv_NL('Netherlands', 'sum_stor',year)  = sum(month, report_NL('Netherlands','withdrawal',year,month)) + EPS;
    report_cv_NL('Netherlands', 'sum_total',year) = report_cv_NL('Netherlands', 'sum_inner', year)
                                                + report_cv_NL('Netherlands', 'sum_pipe', year)
                                                + report_cv_NL('Netherlands', 'sum_lng', year)
                                                + report_cv_NL('Netherlands', 'sum_stor', year);

    report_cv_NL('Netherlands', 'perc_inner',year) =  report_cv_NL('Netherlands', 'sum_inner', year) / report_cv_NL('Netherlands', 'sum_total', year) ;
    report_cv_NL('Netherlands', 'perc_pipe',year)  =  report_cv_NL('Netherlands', 'sum_pipe', year) / report_cv_NL('Netherlands', 'sum_total', year) ;
    report_cv_NL('Netherlands', 'perc_lng',year)   =  report_cv_NL('Netherlands', 'sum_lng', year)  / report_cv_NL('Netherlands', 'sum_total', year) ;
    report_cv_NL('Netherlands', 'perc_stor',year)  =  report_cv_NL('Netherlands', 'sum_stor', year)  / report_cv_NL('Netherlands', 'sum_total', year) ;

    report_cv_NL('Netherlands', 'mean_inner',year) =   report_cv_NL('Netherlands', 'sum_inner', year)/ card (month);
    report_cv_NL('Netherlands', 'mean_pipe',year)  =   report_cv_NL('Netherlands', 'sum_pipe', year) / card (month);
    report_cv_NL('Netherlands', 'mean_lng',year)   =   report_cv_NL('Netherlands', 'sum_lng', year)  / card (month);
    report_cv_NL('Netherlands', 'mean_stor',year)  =   report_cv_NL('Netherlands', 'sum_stor', year)  / card (month);

    report_cv_NL('Netherlands', 'stdev_inner',year) = sqrt(
                                                sum(month, sqr(report_NL('Netherlands','Inner_prod',year,month) - report_cv_NL('Netherlands', 'mean_inner', year)))
                                                / (card (month)-1) );
    report_cv_NL('Netherlands', 'stdev_pipe',year) = sqrt(
                                                sum(month, sqr(report_NL('Netherlands','PIPE_import',year,month) - report_cv_NL('Netherlands', 'mean_pipe', year)))
                                                / (card (month)-1) );
    report_cv_NL('Netherlands', 'stdev_lng',year) = sqrt(
                                                sum(month, sqr(report_NL('Netherlands','LNG_import',year,month) - report_cv_NL('Netherlands', 'mean_lng', year)))
                                                / (card (month)-1) );
    report_cv_NL('Netherlands', 'stdev_stor',year) = sqrt(
                                                sum(month, sqr(report_NL('Netherlands','withdrawal',year,month) - report_cv_NL('Netherlands', 'mean_stor', year)))
                                                / (card (month)-1) );

    report_cv_NL('Netherlands', 'cv_inner', year)$(report_cv_NL('Netherlands', 'mean_inner', year) > 0)
                                             = report_cv_NL('Netherlands', 'stdev_inner', year) / report_cv_NL('Netherlands', 'mean_inner', year);
    report_cv_NL('Netherlands', 'cv_pipe', year)$(report_cv_NL('Netherlands', 'mean_pipe', year) > 0)
                                           = report_cv_NL('Netherlands', 'stdev_pipe', year) / report_cv_NL('Netherlands', 'mean_pipe', year);
    report_cv_NL('Netherlands', 'cv_lng', year)$(report_cv_NL('Netherlands', 'mean_lng', year) > 0)
                                            = report_cv_NL('Netherlands', 'stdev_lng', year) / report_cv_NL('Netherlands', 'mean_lng', year);
    report_cv_NL('Netherlands', 'cv_stor', year)$(report_cv_NL('Netherlands', 'mean_stor', year) > 0)
                                            = report_cv_NL('Netherlands', 'stdev_stor', year) / report_cv_NL('Netherlands', 'mean_stor', year);

    report_cv_NL('Netherlands', 'cvs_inner', year) =  report_cv_NL('Netherlands', 'cv_inner', year) * report_cv_NL('Netherlands', 'perc_inner',year);
    report_cv_NL('Netherlands', 'cvs_pipe', year)  =  report_cv_NL('Netherlands', 'cv_pipe', year) *  report_cv_NL('Netherlands', 'perc_pipe',year);
    report_cv_NL('Netherlands', 'cvs_lng', year)   =  report_cv_NL('Netherlands', 'cv_lng', year) *  report_cv_NL('Netherlands', 'perc_lng',year);
    report_cv_NL('Netherlands', 'cvs_stor', year)  =  report_cv_NL('Netherlands', 'cv_stor', year) *  report_cv_NL('Netherlands', 'perc_stor',year);

************************* EU

    report_cv_EU('Europe', 'sum_inner',year) = sum(month, report_EU('Europe','Inner_prod',year,month)) + EPS;
    report_cv_EU('Europe', 'sum_pipe',year)  = sum(month, report_EU('Europe','PIPE_import',year,month)) + EPS;
    report_cv_EU('Europe', 'sum_lng',year)   = sum(month, report_EU('Europe','LNG_import',year,month)) + EPS;
    report_cv_EU('Europe', 'sum_stor',year)  = sum(month, report_EU('Europe','withdrawal',year,month)) + EPS;
    report_cv_EU('Europe', 'sum_total',year) = report_cv_EU('Europe', 'sum_inner', year)
                                                + report_cv_EU('Europe', 'sum_pipe', year)
                                                + report_cv_EU('Europe', 'sum_lng', year)
                                                + report_cv_EU('Europe', 'sum_stor', year);

    report_cv_EU('Europe', 'perc_inner',year) =  report_cv_EU('Europe', 'sum_inner', year) / report_cv_EU('Europe', 'sum_total', year) ;
    report_cv_EU('Europe', 'perc_pipe',year)  =  report_cv_EU('Europe', 'sum_pipe', year) / report_cv_EU('Europe', 'sum_total', year) ;
    report_cv_EU('Europe', 'perc_lng',year)   =  report_cv_EU('Europe', 'sum_lng', year)  / report_cv_EU('Europe', 'sum_total', year) ;
    report_cv_EU('Europe', 'perc_stor',year)  =  report_cv_EU('Europe', 'sum_stor', year)  / report_cv_EU('Europe', 'sum_total', year) ;

    report_cv_EU('Europe', 'mean_inner',year) =   report_cv_EU('Europe', 'sum_inner', year)/ card (month);
    report_cv_EU('Europe', 'mean_pipe',year)  =   report_cv_EU('Europe', 'sum_pipe', year) / card (month);
    report_cv_EU('Europe', 'mean_lng',year)   =   report_cv_EU('Europe', 'sum_lng', year)  / card (month);
    report_cv_EU('Europe', 'mean_stor',year)  =   report_cv_EU('Europe', 'sum_stor', year)  / card (month);

    report_cv_EU('Europe', 'stdev_inner',year) = sqrt(
                                                sum(month, sqr(report_EU('Europe','Inner_prod',year,month) - report_cv_EU('Europe', 'mean_inner', year)))
                                                / (card (month)-1) );
    report_cv_EU('Europe', 'stdev_pipe',year) = sqrt(
                                                sum(month, sqr(report_EU('Europe','PIPE_import',year,month) - report_cv_EU('Europe', 'mean_pipe', year)))
                                                / (card (month)-1) );
    report_cv_EU('Europe', 'stdev_lng',year) = sqrt(
                                                sum(month, sqr(report_EU('Europe','LNG_import',year,month) - report_cv_EU('Europe', 'mean_lng', year)))
                                                / (card (month)-1) );
    report_cv_EU('Europe', 'stdev_stor',year) = sqrt(
                                                sum(month, sqr(report_EU('Europe','withdrawal',year,month) - report_cv_EU('Europe', 'mean_stor', year)))
                                                / (card (month)-1) );

    report_cv_EU('Europe', 'cv_inner', year)$(report_cv_EU('Europe', 'mean_inner', year) > 0)
                                             = report_cv_EU('Europe', 'stdev_inner', year) / report_cv_EU('Europe', 'mean_inner', year);
    report_cv_EU('Europe', 'cv_pipe', year)$(report_cv_EU('Europe', 'mean_pipe', year) > 0)
                                           = report_cv_EU('Europe', 'stdev_pipe', year) / report_cv_EU('Europe', 'mean_pipe', year);
    report_cv_EU('Europe', 'cv_lng', year)$(report_cv_EU('Europe', 'mean_lng', year) > 0)
                                            = report_cv_EU('Europe', 'stdev_lng', year) / report_cv_EU('Europe', 'mean_lng', year);
    report_cv_EU('Europe', 'cv_stor', year)$(report_cv_EU('Europe', 'mean_stor', year) > 0)
                                            = report_cv_EU('Europe', 'stdev_stor', year) / report_cv_EU('Europe', 'mean_stor', year);

    report_cv_EU('Europe', 'cvs_inner', year) =  report_cv_EU('Europe', 'cv_inner', year) * report_cv_EU('Europe', 'perc_inner',year);
    report_cv_EU('Europe', 'cvs_pipe', year)  =  report_cv_EU('Europe', 'cv_pipe', year) *  report_cv_EU('Europe', 'perc_pipe',year);
    report_cv_EU('Europe', 'cvs_lng', year)   =  report_cv_EU('Europe', 'cv_lng', year) *  report_cv_EU('Europe', 'perc_lng',year);
    report_cv_EU('Europe', 'cvs_stor', year)  =  report_cv_EU('Europe', 'cv_stor', year) *  report_cv_EU('Europe', 'perc_stor',year);


*###############################################################################
*                                   GDX & EXCEL
*###############################################################################

execute_unload          '%resultdir%%result%.gdx'
execute "XLSTALK -c      %resultdir%%GlobalSCEN%.xlsx"
execute "XLSTALK -c      %resultdir%%GlobalSCEN%.xlsm"

$onecho >display_report.tmp

            par=report_DE               rng=LC!c3              cdim=2 rdim=2
            par=report_UK               rng=LC!c13             cdim=2 rdim=2
            par=report_NL               rng=LC!c23             cdim=2 rdim=2
            par=report_EU               rng=LC!c34             cdim=2 rdim=2

            par=report_DE_LDC           rng=LDCs!c3             cdim=2 rdim=2
            par=report_UK_LDC           rng=LDCs!l3             cdim=2 rdim=2
            par=report_NL_LDC           rng=LDCs!u3             cdim=2 rdim=2
            par=report_EU_LDC           rng=LDCs!ad3            cdim=2 rdim=2

            par=report_CV_DE            rng=CV!c3               cdim=1 rdim=2
            par=report_CV_UK            rng=CV!c29              cdim=1 rdim=2
            par=report_CV_NL            rng=CV!c57              cdim=1 rdim=2
            par=report_CV_EU            rng=CV!c85              cdim=1 rdim=2

$offecho

execute 'gdxxrw %resultdir%%result%.gdx o=%resultdir%%GlobalSCEN%.xlsm EpsOut=0 @display_report.tmp';


*###############################################################################
*###############################################################################
