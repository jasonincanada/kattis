use std::io::BufRead;

fn main() {
    let stdin = std::io::stdin();
    let mut lines = stdin.lock().lines();

    // skip the first line, parse the rest
    _ = lines.next();

    let knights = 
        lines.enumerate()
             .map(|(index, line)| {
                 let line = line.unwrap();
                 let mut s = line.split_whitespace();
 
                 Knight {
                     index   : index+1, // 1-based indexing
                     health  : s.next().unwrap().parse().unwrap(),
                     strength: s.next().unwrap().parse().unwrap()
                 }
             });

    let winner = tournament(knights);
    println!("{}", winner.index)
}

#[derive(Clone, Debug, PartialEq)]
struct Knight {
    index: usize,
    health: i32,
    strength: i32
}

fn tournament<I>(mut knights: I) -> Knight
where
    I: Iterator<Item=Knight>
{
    let first = knights.next().unwrap();

    knights.fold(first, |winner, next_up| {
        fight(winner, next_up)
    })
}

// this is the constant-time method of finding and returning the winner while applying
// total damage one time only, instead of once per round. chatgpt helped with this
fn fight(mut k1: Knight, mut k2: Knight) -> Knight {

    // Calculate how many turns k1 needs to defeat k2
    let turns_for_k1 = (k2.health + k1.strength - 1) / k1.strength;

    // Calculate how many turns k2 needs to defeat k1
    let turns_for_k2 = (k1.health + k2.strength - 1) / k2.strength;

    // Determine the winner
    if turns_for_k1 <= turns_for_k2 {
        k1.health -= k2.strength * (turns_for_k1 - 1);
        k1
    } else {
        k2.health -= k1.strength * turns_for_k2;
        k2
    }
}

// this is the much slower method of finding the winner by simulating each round and
// incurring the damage mutably to both knights as it goes. chatgpt wrote this
fn fight_with_loop(mut k1: Knight, mut k2: Knight) -> Knight {

    loop {
        // k1's turn to attack               // k1 is the challenger and strikes first
        k2.health -= k1.strength;
        
        // Check if k2 is defeated
        if k2.health <= 0 {
            return k1;
        }

        // k2's turn to attack
        k1.health -= k2.strength;

        // Check if k1 is defeated
        if k1.health <= 0 {
            return k2;
        }
    }
}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fight() {
        let knight1: Knight = Knight { index: 1, health: 4, strength: 1 };
        let knight2: Knight = Knight { index: 2, health: 4, strength: 1 };

        assert_eq!(Knight {
            index: 1,
            health: 1,
            strength: 1
        }, fight(knight1, knight2));
    }

    #[test]
    fn test_fight_single_knight() {
        let input: Vec<Knight> = vec![
            Knight { index: 1, health: 4, strength: 1 },
        ];
        assert_eq!(1, tournament(input.into_iter()).index);
    }


    /* Samples from the problem description */

    #[test]
    fn test_sample_1() {
        let input: Vec<Knight> = vec![
            Knight { index: 1, health: 4, strength: 1 },
            Knight { index: 2, health: 4, strength: 1 },
            Knight { index: 3, health: 2, strength: 1 },
        ];
        assert_eq!(3, tournament(input.into_iter()).index);
    }

    #[test]
    fn test_sample_2() {
        let input: Vec<Knight> = vec![
            Knight { index: 1, health: 12, strength: 1 },
            Knight { index: 2, health: 4, strength: 3 },
            Knight { index: 3, health: 2, strength: 6 },
            Knight { index: 4, health: 3, strength: 4 },
            Knight { index: 5, health: 1, strength: 12 },
            Knight { index: 6, health: 6, strength: 2 },
        ];
        assert_eq!(3, tournament(input.into_iter()).index);
    }

    #[test]
    fn test_sample_3() {
        let input: Vec<Knight> = vec![
            Knight { index: 1, health: 14, strength: 3 },
            Knight { index: 2, health: 43, strength: 6 },
            Knight { index: 3, health: 32, strength: 8 },
            Knight { index: 4, health: 13, strength: 9 },
            Knight { index: 5, health: 29, strength: 5 },
        ];
        assert_eq!(5, tournament(input.into_iter()).index);
    }


    /* Try to find the first input that differs between the two fighting styles */

    #[test]
    fn probe_space() {

        for k1_health in 1..10 {
        for k1_strength in 1..10 {

            let k1 = Knight {
                index   : k1_health as usize * 1_000_000 + k1_strength as usize,
                health  : k1_health,
                strength: k1_strength
            };
            println!("k1_health = {} k1_strength = {}", k1_health, k1_strength);

            for k2_health in 1..10 {
            for k2_strength in 1..10 {

                let k2 = Knight {
                    index   : k2_health as usize * 1_000 + k2_strength as usize,
                    health  : k2_health,
                    strength: k2_strength
                };
                println!("k2_health = {} k2_strength = {}", k2_health, k2_strength);

                assert_eq!(          fight(k1.clone(), k2.clone()),
                           fight_with_loop(k1.clone(), k2.clone()));
            }}
        }}
    }

    #[test]
    fn test_fight_first_deviation() {
        let knight1: Knight = Knight { index: 1, health: 1, strength: 1 };
        let knight2: Knight = Knight { index: 2, health: 2, strength: 1 };

        assert_eq!(Knight {
            index: 2,
            health: 1,
            strength: 1
        }, fight(knight1, knight2));
    }
}
