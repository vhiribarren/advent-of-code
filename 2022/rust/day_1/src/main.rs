use std::error::Error;
use std::fs::File;
use std::io::{BufRead, BufReader};

const INPUT_FILENAME: &str = "input.txt";

fn main() -> Result<(), Box<dyn Error>> {
    let file = File::open(INPUT_FILENAME)?;

    let mut elves = Vec::<usize>::new();

    let buf_reader = BufReader::new(file);
    let mut calory_counter = 0;
    for line in buf_reader.lines() {
        let calory_result = line?.trim().parse::<usize>();
        match calory_result {
            Ok(calory) => calory_counter += calory,
            Err(_) => {
                elves.push(calory_counter);
                calory_counter = 0;
            }
        }
    }

    elves.sort();
    elves.reverse();

    println!("{}", elves.first().unwrap());
    println!("{}", elves.iter().take(3).sum::<usize>());

    Ok(())
}
