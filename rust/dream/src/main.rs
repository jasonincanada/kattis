/*  https://open.kattis.com/problems/dream

    This program passes the two sample tests but gives an incorrect answer to the 3rd test on
    the kattis servers. The content of those tests are not known to us competitors, so I'm not
    sure where the issue is. The runtime for the first two tests is 0.12s though which would
    place this solution in second if the code were actually giving the correct answer

*/

use std::collections::HashMap;
use std::io::BufRead;

fn main() {
    let stdin = std::io::stdin();
    let mut input = stdin.lock().lines();
        
    // map of events to their index in the list
    let mut event_map = HashMap::<String, usize>::new();

    // keep a separate ordered list of events so we know which to remove from the
    // event map during a dream() step (by popping off the end n times)
    let mut event_vec = Vec::<String>::new();
    
    // discard the first line, the line count
    input.next();
    
    while let Some(line) = input.next() {
        let inner = line.unwrap();
        let split = inner.split(" ");
        let tokens : Vec<&str> = split.collect();

        match tokens[0] {
            "E"  => event(&mut event_map,
                          &mut event_vec,
                          tokens[1]),

            "D"  => dream(&mut event_map,
                          &mut event_vec,
                          tokens[1].parse().unwrap()),
            
            "S"  => match scenario(&event_map,
                                   &event_vec,
                                   &tokens[2..]) {
                                      Outcome::PlotError     => println!("Plot Error"),
                                      Outcome::JustADream(r) => println!("{} Just A Dream", r),
                                      Outcome::Yes           => println!("Yes")
                                  },

             _   => panic!("Unknown line type")
        };
    }
}

// record an event that has occurred
fn event(event_map: &mut HashMap<String, usize>,
         event_vec: &mut Vec<String>,
         event:     &str)
{    
    event_map.insert(event.to_owned(), event_vec.len());
    event_vec.push(event.to_owned());
}

// delete the last n events, which were just a dream
fn dream(event_map: &mut HashMap<String, usize>,
         event_vec: &mut Vec<String>,
         count:     usize)
{
    for _ in 1..=count {
        let removed = event_vec.pop().unwrap();
        event_map.remove(&removed);
    }    
}

enum Outcome {
    PlotError,
    JustADream(usize),
    Yes
}

// test whether a scenario is true or would be true if we were to forget recent events
fn scenario(event_map: &HashMap<String, usize>,
            event_vec: &[String],
            events:    &[&str]) -> Outcome
{
    // check first all events that must have happened
    // if we miss even one, we can bail here with a Plot Error
    for event in events {
        if event.starts_with('!') {
            continue;
        }

        if !event_map.contains_key::<str>(event) {
            return Outcome::PlotError
        }
    }

    // go through the events again, this time considering the non-events. see how far back in
    // time we have to go to make this scenario true

    // nonevents is the set of events that must NOT have happened for this scenario to be true
    let mut non_events = Vec::<&str>::new();

    for event in events {
        if !event.starts_with('!') {
            continue;
        }

        non_events.push(&event[1..]);
    }

    if non_events.is_empty() {
        return Outcome::Yes
    }

    // get the earliest index of the events that we've already seen that we want to forget
    let indices : Vec<usize> = get_values_for(event_map, &non_events);

    if indices.len() == 0 {
        return Outcome::Yes
    }

    let lowest_index = indices.iter().min().unwrap();
    let distance_back = event_vec.len() - lowest_index;

    Outcome::JustADream(distance_back)
}

// get the values from a map for all keys passed
fn get_values_for(map:  &HashMap<String, usize>,
                  keys: &[&str]) -> Vec<usize>
{
    let mut vec = Vec::<usize>::new();

    for key in keys {
        if let Some(val) = map.get(key.to_owned()) { // TODO
            vec.push(*val);
        }
    }

    vec
}

/*
    C:\Users\Jason\Documents\GitHub\kattis\rust\dream> type sample-1.txt | cargo run
      Plot Error
      3 Just A Dream
      Yes
      Plot Error

    C:\Users\Jason\Documents\GitHub\kattis\rust\dream> type sample-2.txt | cargo run
      Yes
      2 Just A Dream
      1 Just A Dream
      Yes
*/
