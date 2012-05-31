FIT   							                                             ##Run type (FIT, BOT, or IRF)

"White Marlin 2012 meeting"

LOGISTIC YLD SSE 						                                     ##Model type, conditioning type, objective function

12 								                                               ##Verbosity

501 								                                             ##Number of bootstrap trials, <= 1000

1 100000 							                                           ##0=no MC search, 1=search, 2=repeated srch; N trials
1.00000e-08 							                                       ##Convergence crit. for simplex
3.00000e-08 6							                                       ##Convergence crit. for restarts, N restarts
1.00000e-04 0 							                                     ##Convergence crit. for estimating effort; N steps/yr
8.00000 							                                           ##Maximum F allowed in estimating effort

0.0 								                                             ##Weighting for B1 > K as residual (usually 0 or 1)

7 								                                               ##Number of fisheries (data series)
1.0 1.0 1.0 1.0 1.0 1.0 1.0  					                           ##Statistical weights for data series
1.0 								                                             ##B1/K (starting guess, usually 0 to 1)
1000 								                                             ##MSY (starting guess)
10000								                                             ##K (carrying capacity) (starting guess)
2.35e-05 1.82e-04 5.21e-04 1.63e-05 1.11e-03 2.05e-05 7.050e-06  ##q starting guesses
0 1 1 1 1 1 1 1 0 0 						                                 ##Estimate flags (0 or 1) (B1/K,MSY,K,q1...qn)
100 10000 							                                         ##Min and max constraints -- MSY
2000 1000000 							                                       ##Min and max constraints -- K

5103079 							                                           ##Random number seed

55 55 55 55 55 55 55 						                                 ##Number of years of data in each series
