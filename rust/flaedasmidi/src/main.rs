use std::ops::RangeInclusive;
use num_rational::{Rational32, Ratio};

extern crate num_rational;

fn main() {
    println!();
    println!("```mermaid");
    println!("stateDiagram-v2");

    let n: u16 = 1;
    let m: u16 = 9;

    if n == 1 {
        if m.is_power_of_two()
        {
            let k = f32::from(m).log2() as u32;
            case_1_over_2_to_the_k(k);
        }
        else if (m-1).is_power_of_two()
        {
            let k = f32::from(m-1).log2() as u32;
            case_1_over_2_to_the_k_plus_1(k);
        }
        else {
            unimplemented!("m is not a power of 2 or 1 more than a power of 2")
        }
    } else {
        unimplemented!("n <> 1");
    }

    for i in 1..=n { println!("    class {} source", i); }
    for i in 1..=m { println!("    class {} sink", i+1); }

    println!("    classDef source fill:#C44");
    println!("    classDef sink fill:#3CD");
    println!("```")
}

fn case_1_over_2_to_the_k(k: u32)
{
    let n = 1;                   // sources
    let m = 2_u32.pow(k);        // customers
    let v = 2_u32.pow(k+1) - 1;  // joints
    let e = 2_u32.pow(k+1) - 2;  // pipes

    println!("{} {}", v, e);

    let mut customers = (n+1)..=(n+m);
    let mut joints    = (n+m+1)..=(n+m+v);

    let pipe = Rational32::new_raw(m as i32, m as i32);

    standard_branching(1, &pipe, &mut customers, &mut joints);
}

fn case_1_over_2_to_the_k_plus_1(k: u32)
{
    let n = 1;                   // sources
    let m = 2_u32.pow(k) + 1;    // customers
    let v = 22; // 2_u32.pow(k+1) - 1;  // joints
    let e = 999; // 2_u32.pow(k+1) - 2;  // pipes

    //println!("{} {}", v, e);

    let mut customers = (n+1)..=(n+m);
    let mut joints    = (n+m+1)..=(n+m+v);

    let pipe = Rational32::new_raw(m as i32,
                                   m as i32);

    second_kind(1, &pipe, &mut customers, &mut joints);
}

// a standard branching is the only branching type used in a 1/2^k graph.
// it forms a tree with 2^k leaves
fn standard_branching(joint     : u32,
                      pipe      : &Ratio<i32>,
                      customers : &mut RangeInclusive<u32>,
                      joints    : &mut RangeInclusive<u32>)
{
    assert!(pipe.numer() % 2 == 0);
    assert!(pipe.numer() - 2 >= 0);

    // there are actually two new pipes but we only need to create one of them since they're always equal
    let new_pipe = Rational32::new_raw(*pipe.numer() / 2,
                                       *pipe.denom());

    if pipe.numer() == &2 {
        println!("    {joint} --> {}: {new_pipe}", customers.next().unwrap()); // left customer
        println!("    {joint} --> {}: {new_pipe}", customers.next().unwrap()); // right customer
        return
    }

    // depth-first joint numbering
    let left_joint = joints.next().unwrap();
    standard_branching(left_joint, &new_pipe, customers, joints);

    let right_joint = joints.next().unwrap();
    standard_branching(right_joint, &new_pipe, customers, joints);

    println!("    {joint} --> {left_joint}: {new_pipe}");
    println!("    {joint} --> {right_joint}: {new_pipe}");
}

fn second_kind(joint    : i32,
               pipe     : &Ratio<i32>,
               customers: &mut RangeInclusive<u32>,
               joints   : &mut RangeInclusive<u32>)
{
    assert!(pipe.numer() % 2 == 1);
    assert_eq!(*pipe.numer(),
               *pipe.denom());

    let new_pipe = Rational32::new_raw(*pipe.numer() - 1,
                                       *pipe.denom());

    let left_joint = joints.next().unwrap();
    standard_branching(left_joint, &new_pipe, customers, joints); 
    let right_joint = joints.next().unwrap();
    println!("    {joint} --> {left_joint}: {new_pipe} 1 --> tree");
    println!("    {joint} --> {right_joint}: {new_pipe} 1 --> collectoid");

    let (final_joint, return_pipe) = collectoid_branching(right_joint, &new_pipe, customers, joints);
    println!("    {final_joint} --> 1: return {}", return_pipe);
}

fn collectoid_branching(joint    : u32,
                        pipe     : &Ratio<i32>,
                        customers: &mut RangeInclusive<u32>,
                        joints   : &mut RangeInclusive<u32>) -> (u32, Ratio<i32>)
{
    assert!(pipe.numer() % 2 == 0);
    assert!(pipe.numer() - 2 >= 0);

    // joints always divide their input in half
    let new_pipe = Rational32::new_raw(*pipe.numer() / 2,
                                       *pipe.denom());

    if new_pipe.numer() == &1 {
        let customer_joint = customers.next().unwrap();
        let co_joint = joints.next().unwrap();
        println!("    {joint} --> {customer_joint}: {new_pipe}");
        println!("    {joint} --> {co_joint}: {new_pipe} joint --> co_joint");
        return (co_joint, new_pipe)
    }

    let left_joint = joints.next().unwrap();
    println!("    {joint} --> {left_joint}: {new_pipe}");

    let (right_joint, rec_pipe) = collectoid_branching(left_joint, &new_pipe, customers, joints);

    match joints.next() {
        Some(added_joint) => {
            println!("    {right_joint} --> {added_joint}: {} right->added", new_pipe + rec_pipe);
            println!("    {joint} --> {right_joint}: {new_pipe} S(added)");
            (added_joint, new_pipe + rec_pipe)
        }
        None => {
            println!("    {joint} --> {right_joint}: {new_pipe} N");
            (right_joint, new_pipe + rec_pipe)
        },
    }
}

/*
$ cargo run

```mermaid
stateDiagram-v2
    13 --> 2: 1/9
    13 --> 3: 1/9
    14 --> 4: 1/9
    14 --> 5: 1/9
    12 --> 13: 2/9
    12 --> 14: 2/9
    16 --> 6: 1/9
    16 --> 7: 1/9
    17 --> 8: 1/9
    17 --> 9: 1/9
    15 --> 16: 2/9
    15 --> 17: 2/9
    11 --> 12: 4/9
    11 --> 15: 4/9
    1 --> 11: 8/9 1 --> tree
    1 --> 18: 8/9 1 --> collectoid
    18 --> 19: 4/9
    19 --> 20: 2/9
    20 --> 10: 1/9
    20 --> 21: 1/9 joint --> co_joint
    21 --> 22: 1/3 right->added
    19 --> 21: 2/9 S(added)
    22 --> 23: 7/9 right->added
    18 --> 22: 4/9 S(added)
    23 --> 1: return 7/9
    class 1 source
    class 2 sink
    class 3 sink
    class 4 sink
    class 5 sink
    class 6 sink
    class 7 sink
    class 8 sink
    class 9 sink
    class 10 sink
    classDef source fill:#C44
    classDef sink fill:#3CD
```
*/
