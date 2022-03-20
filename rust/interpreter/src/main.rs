/*  https://open.kattis.com/problems/interpreter  */

use std::{io::BufRead};

struct Computer {
    registers : Vec<u32>,
    ram       : Vec<u32>,
    ip        : usize
}

impl Computer {

    fn new() -> Self {
        Computer {
            registers: vec![0; 10],
            ram:       vec![0; 1000],
            ip:        0
        }
    }

    fn run(&mut self) -> u32 {

        // return this at the end, the number of opcodes we ran
        let mut count_ops = 0;

        // reset execution to beginning of RAM
        self.ip = 0;
        
        // loop until we have a 1xx halt instruction
        loop {
            let (opcode, x, y) = self.decode_next();

            count_ops += 1;

            match opcode {
                1 => break,
                2 => self.set_register(x, y),
                3 => self.add_to_register(x, y),
                4 => self.multiply_register_by(x, y),
                5 => self.set_register_to_register(x, y as usize),
                6 => self.add_register_to_register(x, y as usize),
                7 => self.multiply_register_by_register(x, y as usize),
                8 => self.set_register_by_ram_in_reg(x, y as usize),
                9 => self.set_ram_by_ram_ref(x, y as usize),
                0 => self.goto(x, y as usize),

                _ => panic!("Unknown opcode {}", opcode)
            }

            // if the opcode is 0 we already chose a new location, otherwise advance by one
            if opcode != 0 || self.registers[y as usize] == 0 {
                self.ip += 1;
            }
        }

        count_ops
    }
    
    fn set_ram(&mut self, address : usize, val : u32) {
        self.ram[address] = val;
    }

    // 2dn means set register d to n (between 0 and 9)
    fn set_register(&mut self, d: usize, n: u32) {
        self.registers[d] = n;
    }

    // 3dn means add n to register d
    fn add_to_register(&mut self, d: usize, n: u32) {
        self.registers[d] = (self.registers[d] + n) % 1000;
    }

    // 4dn means multiply register d by n
    fn multiply_register_by(&mut self, d: usize, n: u32) {
        self.registers[d] = (self.registers[d] * n) % 1000;
    }

    // 5ds means set register d to the value of register s
    fn set_register_to_register(&mut self, d: usize, s: usize) {
        self.registers[d] = self.registers[s];
    }

    // 6ds means add the value of register s to register d
    fn add_register_to_register(&mut self, d: usize, s: usize) {
        self.registers[d] = (self.registers[d] + self.registers[s]) % 1000;
    }
    
    // 7ds means multiply register d by the value of register s
    fn multiply_register_by_register(&mut self, d: usize, s: usize) {
        self.registers[d] = (self.registers[d] * self.registers[s]) % 1000;
    }

    // 8da means set register d to the value in RAM whose address is in register a
    fn set_register_by_ram_in_reg(&mut self, d: usize, a: usize) {
        self.registers[d] = self.ram[self.registers[a] as usize];
    }

    // 9sa means set the value in RAM whose address is in register a to the value of register s
    fn set_ram_by_ram_ref(&mut self, s: usize, a: usize) {
        self.set_ram(self.registers[a] as usize, self.registers[s]);
    }

    // 0ds means goto the location in register d unless register s contains 0
    fn goto(&mut self, d: usize, s: usize) {
        if self.registers[s] == 0 {
            return
        }

        self.ip = self.registers[d] as usize;
    }

    fn decode_next(&self) -> (u32, usize, u32) {
        let opcode = self.ram[self.ip];

        Computer::decode(opcode)
    }

    fn decode(mut opcode : u32) -> (u32, usize, u32) {
        let y = opcode % 10;  opcode = opcode / 10;
        let x = opcode % 10;  opcode = opcode / 10;
        
        (opcode, x as usize, y)
    }
}

fn main() {
    let     stdin = std::io::stdin();
    let mut input = stdin.lock().lines();

    // new computer with everything initalized to zero
    let mut computer = Computer::new();
    let mut ram_idx : usize = 0;

    // one instruction per line
    while let Some(line) = input.next() {
        let opcode = line.unwrap();

        computer.set_ram(ram_idx, opcode.parse().unwrap());
        ram_idx += 1;
    }

    let count_ops = computer.run();

    println!("{}", count_ops);
}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decode() {
        assert_eq!((1,2,3), Computer::decode(123 as u32));
    }
}
