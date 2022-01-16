mod stack;
use lazy_static::lazy_static;
use regex::Regex;
use std::io;

#[derive(Debug)]
enum JoyVal {
    True,
    False,
    List(stack::List<JoyVal>),
    Int(i64),
    Sym(String),
}

fn tokenize(str: &str) -> JoyVal {
    lazy_static! {
        static ref RE: Regex = Regex::new(
            r"(?x)
              (?P<true>true)\b
            | (?P<false>false)\b
            | (?P<lbracket>\[)
            | (?P<rbracket>\])
            | (?P<int>\d+)
            | (?P<atom>[^\s+\[\]]+) "
        )
        .unwrap();
    }

    let mut frame = vec![];
    let mut stack = vec![];
    for cap in RE.captures_iter(str) {
        // Is this better than the if/elif/else chain below?
        // More importantly, how would I go about figuring that out?
        match cap.name("true") {
            Some(_) => {
                frame.push(JoyVal::True);
                continue;
            }
            None => {}
        }
        // Is there a better way to do this?
        //if cap.name("true") != None {
        //    println!("T");
        //} else
        if cap.name("false") != None {
            frame.push(JoyVal::False);
        } else if cap.name("lbracket") != None {
            stack.push(frame);
            frame = vec![];
        } else if cap.name("rbracket") != None {
            let v = frame;
            frame = stack.pop().expect("Extra closing bracket.");
            frame.push(JoyVal::List(stack::List::vec_to_list(v)));
        } else if cap.name("int") != None {
            frame.push(JoyVal::Int(cap[0].parse().unwrap()));
        } else if cap.name("atom") != None {
            frame.push(JoyVal::Sym(String::from(&cap[0])));
        } else {
            println!("wtf");
        }
    }
    JoyVal::List(stack::List::vec_to_list(frame))
}

fn joy(mut s: stack::List<JoyVal>, mut e: stack::List<JoyVal>) -> stack::List<JoyVal> {
    while !e.empty() {
        let term = e.head().expect("How!?");
        e = e.tail();
        match term {
            JoyVal::True => { s = s.push(JoyVal::True) }
            JoyVal::False => { s = s.push(JoyVal::False) }
            JoyVal::List(_) => { s = s.push(term) }
            JoyVal::Int(_) => { s = s.push(term) }
            JoyVal::Sym(_) => { s = s.push(term) }
        }
    }
    s
}

fn main() {
    println!("Enter a number:");
    let mut n = String::new();
    let m = io::stdin().read_line(&mut n).expect("WTF?");
    println!("{:?}", tokenize(&n));
    println!("Entered: {} {}", m, n);
}
