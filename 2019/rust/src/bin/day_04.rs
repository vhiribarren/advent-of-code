use std::{error::Error, ops::RangeInclusive};

const RANGE: RangeInclusive<u32> = 206938..=679128;

fn main() -> Result<(), Box<dyn Error>> {
    println!("Problem 1: {}", problem_1()?);
    println!("Problem 2: {}", problem_2()?);
    Ok(())
}

fn problem_1() -> Result<String, Box<dyn Error>> {
    let mut total = 0;
    for password in RANGE {
        if is_valid_password_prob1(password) {
            total += 1;
        }
    }
    Ok(total.to_string())
}

fn problem_2() -> Result<String, Box<dyn Error>> {
    let mut total = 0;
    for password in RANGE {
        if is_valid_password_prob2(password) {
            total += 1;
        }
    }
    Ok(total.to_string())
}

fn is_valid_password_prob1(password: u32) -> bool {
    let mut has_double = false;
    for w in password
        .to_string()
        .chars()
        .map(|v| v.to_digit(10).unwrap())
        .collect::<Vec<_>>()
        .windows(2)
    {
        if w[0] > w[1] {
            return false;
        }
        if w[0] == w[1] {
            has_double = true;
        }
    }
    has_double
}

fn is_valid_password_prob2(password: u32) -> bool {
    let mut has_double = false;
    let mut double_count = 0;
    for w in password
        .to_string()
        .chars()
        .map(|v| v.to_digit(10).unwrap())
        .collect::<Vec<_>>()
        .windows(2)
    {
        if w[0] > w[1] {
            return false;
        }
        if w[0] == w[1] {
            double_count += 1;
        } else {
            if double_count == 1 {
                has_double = true
            }
            double_count = 0;
        }
    }
    if double_count == 1 {
        has_double = true
    }
    has_double
}
