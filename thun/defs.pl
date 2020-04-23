% Apparently there's no good way to have multi-line string literals in
% Prolog code.  I could do something like this:

def(`-- 1 -`).
def(`? dup bool`).
def(`++ 1 +`).
def(`anamorphism [pop []] swap [dip swons] genrec`).
def(`app1 grba infrst`).
def(`app2 [grba swap grba swap] dip [infrst] cons ii`).
def(`app3 3 appN`).
def(`appN [grabN] cons dip map disenstacken`).
def(`at drop first`).
def(`average [sum 1.0 *] [size] cleave /`).
def(`b [i] dip i`).
def(`binary unary popd`).
def(`ccons cons cons`).
def(`cleave fork popdd`).
def(`clop cleave popdd`).
def(`codireco cons dip rest cons`).
def(`dinfrirst dip infrst`).
def(`disenstacken ? [uncons ?] loop pop`).
def(`down_to_zero [0 >] [dup --] while`).
def(`drop [rest] times`).
def(`dupd [dup] dip`).
def(`dupdd [dup] dipd`).
def(`dupdipd dup dipd`).
def(`enstacken stack [clear] dip`).
def(`flatten [] swap [concat] step`).
def(`fork [i] app2`).
def(`fourth rest third`).
def(`gcd true [tuck mod dup 0 >] loop pop`).
def(`grabN [] swap [cons] times`).
def(`grba [stack popd] dip`).
def(`hypot [sqr] ii + sqrt`).
def(`ifte [nullary] dipd swap branch`).
def(`ii [dip] dupdip i`).
def(`infra swons swaack [i] dip swaack`).
def(`infrst infra first`).
def(`make_generator [codireco] ccons`).
def(`neg 0 swap -`).
def(`not [true] [false] branch`).
def(`nullary [stack] dinfrirst`).
def(`of swap at`).
def(`pam [i] map`).
def(`pm [+] [-] clop`).
def(`popd [pop] dip`).
def(`popdd [pop] dipd`).
def(`popop pop pop`).
def(`popopd [popop] dip`).
def(`popopdd [popop] dipd`).
def(`primrec [i] genrec`).
def(`product 1 swap [*] step`).
def(`quoted [unit] dip`).
def(`range [0 <=] [1 - dup] anamorphism`).
def(`range_to_zero unit [down_to_zero] infra`).
def(`reverse [] swap shunt`).
def(`rrest rest rest`).
def(`run [] swap infra`).
def(`second rest first`).
def(`shift uncons [swons] dip`).
def(`shunt [swons] step`).
def(`size 0 swap [pop ++] step`).
def(`split_at [drop] [take] clop`).
def(`sqr dup *`).
def(`step_zero 0 roll> step`).
def(`sum 0 swap [+] step`).
def(`swons swap cons`).
def(`take [] rolldown [shift] times pop`).
def(`ternary binary popd`).
def(`third rest second`).
def(`unary nullary popd`).
def(`unit [] cons`).
def(`unquoted [i] dip`).
def(`unswons uncons swap`).
def(`while swap [nullary] cons dup dipd concat loop`).
def(`x dup i`).