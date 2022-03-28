/*  https://open.kattis.com/problems/simplecronspec  */

struct CronJob {
    hours   : Vec<u8>,
    minutes : Vec<u8>,
    seconds : Vec<u8>,
}

impl CronJob {

    fn new(input: &str) -> Self {
        let split: Vec<&str> = input.split(" ").collect();

        CronJob {
            hours  : CronJob::parse_times(split[0], 24),
            minutes: CronJob::parse_times(split[1], 60),
            seconds: CronJob::parse_times(split[2], 60),
        }
    }
  
    /* 
        "30"  -> 30
        "1-5" -> 1,2,3,4,5
        "*"   -> 1,2,...,59  (if minutes/seconds)
    */
    fn parse_times(input: &str, max: u8) -> Vec<u8> {
        
        // hour, minute, or second values (0..23 or 0..59)
        let mut hms: Vec<u8> = Vec::new();

        if input == "*" {
                        
            for t in 0..max {
                hms.push(t);
            }

        } else {
            let split: Vec<&str> = input.split(",").collect();

            for s in split {

                // if there's a hyphen anywhere in it, it's a range
                if s.contains("-") {
                    let split: Vec<&str> = s.split("-").collect();

                    let from = split[0].parse().unwrap();
                    let to   = split[1].parse().unwrap();

                    for t in from..=to {
                        hms.push(t);
                    }
                }

                // otherwise it's a single number                
                else {
                    hms.push(s.parse().unwrap());
                }
            }
        }
        
        hms
    }    
}


use std::io::BufRead;

fn main() {
    let     stdin = std::io::stdin();
    let mut input = stdin.lock().lines();

    // skip line count
    input.next();

    // how many jobs are set to start at every second of the day
    let mut seconds: Vec<u32> = vec![0; 86400]; 
    
    // one cronjob per line
    while let Some(line) = input.next() {
        let line = line.unwrap();

        // parse it into a CronJob object
        let cronjob = CronJob::new(&line);

        tally(cronjob, &mut seconds);
    }

    // how many seconds have at least one job starting (ie, count the bools that are true)
    let at_least_one = &seconds.iter()
                               .filter(|s| *s > &0)
                               .count();

    let total : u32  = seconds.iter()
                              .sum();

    println!("{} {}", at_least_one, total);
}

fn tally(cronjob: CronJob, seconds: &mut [u32]) {

    for h in &cronjob.hours {
    for m in &cronjob.minutes {
    for s in &cronjob.seconds {        
        let i = *h as usize * 3600
              + *m as usize * 60
              + *s as usize;

        seconds[i] += 1;
    }
    }
    }
}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_times() {

        let job = CronJob::new("* 10 20,25,30-33");
        
        assert_eq!(24, job.hours.len());
        assert_eq!(0 , job.hours[0]);
        assert_eq!(23, job.hours[23]);

        assert_eq!(1 , job.minutes.len());
        assert_eq!(10, job.minutes[0]);
        
        assert_eq!(6 , job.seconds.len());
        assert_eq!(20, job.seconds[0]);
        assert_eq!(25, job.seconds[1]);
        assert_eq!(30, job.seconds[2]);
        assert_eq!(33, job.seconds[5]);
    }
}
