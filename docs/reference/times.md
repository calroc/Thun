------------------------------------------------------------------------

## times

Combinator

Expect a quoted program and an integer `n` on the stack and do the
program `n` times.

       ... n [Q] . times
    -----------------------  w/ n <= 0
             ... .

       ... 1 [Q] . times
    -----------------------
             ... . Q

       ... n [Q] . times
    -------------------------------------  w/ n > 1
             ... . Q (n-1) [Q] times

### Definition

> \[\-- dip\] cons \[swap\] infra \[0 \>\] swap while pop :


### Discussion

This works by building a little [while] program and running it:

                     1 3 [++] • [-- dip] cons [swap] infra [0 >] swap while pop                                                                                                                 
            1 3 [++] [-- dip] • cons [swap] infra [0 >] swap while pop                                                                                                                          
            1 3 [[++] -- dip] • [swap] infra [0 >] swap while pop                                                                                                                               
     1 3 [[++] -- dip] [swap] • infra [0 >] swap while pop                                                                                                                                      
                  dip -- [++] • swap [3 1] swaack [0 >] swap while pop                                                                                                                          
                  dip [++] -- • [3 1] swaack [0 >] swap while pop                                                                                                                               
            dip [++] -- [3 1] • swaack [0 >] swap while pop                                                                                                                                     
            1 3 [-- [++] dip] • [0 >] swap while pop                                                                                                                                            
      1 3 [-- [++] dip] [0 >] • swap while pop                                                                                                                                                  
      1 3 [0 >] [-- [++] dip] • while pop                                                                                                                                                       

This is a common pattern in Joy.  You accept some parameters from the
stack which typically include qouted programs and use them to build
another program which does the actual work.  This is kind of like macros
in Lisp, or preprocessor directives in C.

