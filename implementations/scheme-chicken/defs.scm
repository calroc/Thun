(define (defs) (list "eq [false] [true] [false] cmp" "gt [true] [false] [false] cmp" "lt [false] [false] [true] cmp" "neq [true] [false] [true] cmp" "le [false] [true] [true] cmp" "ge [true] [true] [false] cmp" "? dup bool" "!- 0 >=" "++ 1 +" "-- 1 -" "<{} [] swap" "<<{} [] rollup" "abs dup 0 < [] [neg] branch" "anamorphism [pop []] swap [dip swons] genrec" "and nulco [nullary [false]] dip branch" "app1 grba infrst" "app2 [grba swap grba swap] dip [infrst] cons ii" "app3 3 appN" "appN [grabN] codi map reverse disenstacken" "at drop first" "average [sum] [size] cleave /" "b [i] dip i" "binary unary popd" "ccccons ccons ccons" "ccons cons cons" "choice [pop] [popd] branch" "clear [] swaack pop" "cleave fork popdd" "clop cleave popdd" "cmp [[>] swap] dipd [ifte] ccons [=] swons ifte" "codi cons dip" "codireco codi reco" "dinfrirst dip infrst" "dipd [dip] codi" "dipdd [dip] cons dipd" "dipddd [dipd] cons dipd" "disenstacken swaack pop" "divmod [/] [%] clop" "down_to_zero [0 >] [dup --] while" "drop [rest] times" "dupdd [dup] dipd" "dupd [dup] dip" "dupdipd dup dipd" "dupdip dupd dip" "enstacken stack [clear] dip" "first uncons pop" "first_two uncons first" "flatten <{} [concat] step" "fork [i] app2" "fourth rest third" "gcd true [tuck mod dup 0 >] loop pop" "genrec [[genrec] ccccons] nullary swons concat ifte" "getitem [rest] times first" "grabN <{} [cons] times" "grba [stack popd] dip" "ifte [nullary] dipd swap branch" "ii [dip] dupdip i" "infra swons swaack [i] dip swaack" "infrst infra first" "<< lshift" "lshift [2 *] times" "make_generator [codireco] ccons" "mod %" "modulus %" "neg 0 swap -" "not [true] [false] branch" "nulco [nullary] cons" "nullary [stack] dinfrirst" "null _isnt_list bool not" "of swap at" "or nulco [nullary] dip [true] branch" "over [dup] dip swap" "pam [i] map" "pick getitem" "pm [+] [-] clop" "popdd [pop] dipd" "popd [pop] dip" "popopdd [popop] dipd" "popopd [popop] dip" "popopop pop popop" "popop pop pop" "pow 1 roll> swap [*] cons times" "pred --" "product 1 swap [*] step" "quoted [unit] dip" "range [0 <=] [-- dup] anamorphism" "range_to_zero unit [down_to_zero] infra" "reco rest cons" "rest uncons popd" "reverse <{} shunt" "rolldown roll<" "roll< swapd swap" "roll> swap swapd" "rollup roll>" "rrest rest rest" ">> rshift" "rshift [2 /] times" "run <{} infra" "second rest first" "shift uncons [swons] dip" "shunt [swons] step" "size [pop ++] step_zero" "small dup null [rest null] [pop true] branch" "spiral_next [[[abs] ii <=] [[<>] [pop !-] or] and] [[!-] [[++]] [[--]] ifte dip] [[pop !-] [--] [++] ifte] ifte" "split_at [drop] [take] clop" "split_list [take reverse] [drop] clop" "sqr dup mul" "stackd [stack] dip" "step_zero 0 roll> step" "stuncons stack uncons" "succ --" "sum [+] step_zero" "swapd [swap] dip" "swoncat swap concat" "swons swap cons" "tailrec [i] genrec" "take <<{} [shift] times pop" "ternary binary popd" "third rest second" "tuck dup swapd" "unary nullary popd" "uncons [first] dupdip rest" "unit [] cons" "unquoted [i] dip" "unstack [[] swaack] dip swoncat swaack pop" "unswons uncons swap" "while swap nulco dupdipd concat loop" "x dup i" "step [_step0] x" "_step0 _step1 [popopop] [_stept] branch" "_step1 [?] dipd roll<" "_stept [uncons] dipd [dupdipd] dip x" "times [_times0] x" "_times0 _times1 [popopop] [_timest] branch" "_times1 [dup 0 >] dipd roll<" "_timest [[--] dip dupdipd] dip x" "map [_map0] cons [[] [_map?] [_mape]] dip tailrec" "_map? pop bool not" "_mape popd reverse" "_map0 [_map1] dipd _map2" "_map1 stackd shift" "_map2 [infrst] cons dipd roll< swons" "_isnt_bool [false] [true] branch" "_isnt_two_bools [_isnt_bool] ii" "_\\/_ [_isnt_bool] [not] branch" "/\\ _isnt_two_bools [pop false] [] branch" "\\/ _isnt_two_bools [] [pop true] branch" "_isnt_list [] swoncat" "zip [null] [pop] [uncons-pair] [i cons] genrec" "uncons-pair uncons-two [quote-two] dipd" "uncons-two [uncons] ii swapd" "quote-two unit cons" "empty? dup null" "max-of-two [>] [pop] [popd] ifte" "max empty? [uncons [max-of-two] step] [] branch"))