/*  https://open.kattis.com/problems/charged  */

use core::f64::consts::PI;
use std::io::BufRead;

struct Particle {
    row: i8,
    col: i8,
    charge: i8
}

impl Particle {
    fn measure_potential_from(&self, row: &i8, col: &i8) -> f64 {

        let dr = (self.row as f64) - (*row as f64);
        let dc = (self.col as f64) - (*col as f64);
        
        let distance : f64 = (dr*dr + dc*dc).sqrt();
    
        self.charge as f64 / distance
    }
}

fn main() {
    let     stdin = std::io::stdin();
    let mut lines = stdin.lock().lines();

    // read the dimensions of the board
    let first_line = lines.next().unwrap().unwrap();
    let (rows, cols) = parse_header(first_line);
       
    // load the particles
    let mut particles : Vec<Particle> = Vec::new();

    while let Some(line) = lines.next() {
        let line = line.unwrap();
        let particle = parse_particle(line);

        particles.push(particle);
    }

    // render the field
    for row in 1..=rows {

        // build the row one character at a time and print the finished row
        let mut string = String::new();

        for col in 1..=cols {
            let draw : char;

            // if there's a particle at this exact point just print its sign
            if let Some(sign) = sign_of_particle_at(&particles, &row, &col) {
                
                if sign > 0 {
                    draw = '+';                
                } else {
                    draw = '-';
                }                
                    
            } else {
                let mut sum = 0.0;

                // sum the contributions to this point from every particle
                for particle in &particles {
                    sum += particle.measure_potential_from(&row, &col);
                }

                // for negative potentials use {%,X,x}
                // for positive potentials use {0,O,o}
                let abs = sum.abs();
                let pos = sum > 0.0;

                let tier1 = 1.0 / (PI);
                let tier2 = 1.0 / (PI * PI);
                let tier3 = 1.0 / (PI * PI * PI);

                if abs > tier1 {
                    if pos { draw = '0' } else { draw = '%' };
                } else if abs > tier2 {
                    if pos { draw = 'O' } else { draw = 'X' };
                } else if abs > tier3 {
                    if pos { draw = 'o' } else { draw = 'x' };
                } else {
                    draw = '.';
                }
            }

            string.push(draw);
        }

        println!("{}", string);
    }  
}

fn sign_of_particle_at(particles: &[Particle], row: &i8, col: &i8) -> Option<i8> {
    for p in particles {
        if p.row == *row && p.col == *col {
            return Some(p.charge)
        }
    }

    None
}


/* Parsing */

fn parse_header(line: String) -> (i8, i8) {
    let split: Vec<&str> = line.split(' ').collect();
    
    let rows = split[0].parse().unwrap();
    let cols = split[1].parse().unwrap();

    (rows, cols)
}

fn parse_particle(line: String) -> Particle {
    let split: Vec<&str> = line.split(' ').collect();

    let col    = split[0].parse().unwrap();
    let row    = split[1].parse().unwrap();
    let charge = match split[2] {
                        "+" => 1,
                        "-" => -1,
                         _  => panic!("Unknown charge")
                    };

    Particle {
        row: row,
        col: col,
        charge: charge
    }
}
