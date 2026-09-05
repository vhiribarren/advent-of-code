use std::{
    collections::{BTreeMap, HashMap},
    error::Error,
    fs,
    path::{Path, PathBuf},
    println,
    sync::LazyLock,
    unimplemented,
};

const INPUT_FILENAME: &str = "day_14.txt";

static INPUT_FILE: LazyLock<PathBuf> = LazyLock::new(|| {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../inputs")
        .join(INPUT_FILENAME)
});

fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string(&*INPUT_FILE)?;
    println!("Problem 1: {}", problem_1(&input)?);
    println!("Problem 2: {}", problem_2(&input)?);
    Ok(())
}

fn scan_elem(
    elem: &String,
    req_amount: usize,
    conv_table: &BTreeMap<ElemAmount, Vec<ElemAmount>>,
    leftovers: &mut HashMap<String, usize>,
) -> usize {
    if elem == "ORE" {
        return req_amount;
    }
    let leftover_elem = leftovers.entry(elem.clone()).or_insert(0);
    if *leftover_elem >= req_amount {
        *leftover_elem -= req_amount;
        return 0;
    }
    let (elem_result, elem_ingredients) =
        &conv_table.iter().find(|(k, _)| &k.elem == elem).unwrap();
    let recipie_factor =
        ((req_amount - *leftover_elem) as f64 / elem_result.amount as f64).ceil() as usize;
    *leftover_elem = recipie_factor * elem_result.amount - (req_amount - *leftover_elem);
    elem_ingredients
        .iter()
        .map(|i| scan_elem(&i.elem, recipie_factor * i.amount, conv_table, leftovers))
        .sum()
}

fn problem_1(input: &str) -> Result<String, Box<dyn Error>> {
    let elements = parse_input(input);
    let leftovers = &mut HashMap::<String, usize>::new();
    let needed_ore = scan_elem(&String::from("FUEL"), 1, &elements, leftovers);
    Ok(needed_ore.to_string())
}

fn problem_2(input: &str) -> Result<String, Box<dyn Error>> {
    unimplemented!()
}

#[derive(Debug, Hash, PartialEq, PartialOrd, Ord, Eq)]
struct ElemAmount {
    amount: usize,
    elem: String,
}

fn parse_input(input: &str) -> BTreeMap<ElemAmount, Vec<ElemAmount>> {
    let mut elements = BTreeMap::new();
    for line in input.lines() {
        let (left, right) = line.split_once(" => ").unwrap();
        let (res_amount, res_elem) = right.split_once(" ").unwrap();
        let mut req_elems = Vec::new();
        for new_elem_amount in left.split(", ") {
            let (new_amount, new_elem) = new_elem_amount.split_once(" ").unwrap();
            req_elems.push(ElemAmount {
                amount: new_amount.parse().unwrap(),
                elem: new_elem.to_string(),
            });
        }
        elements.insert(
            ElemAmount {
                elem: res_elem.to_string(),
                amount: res_amount.parse().unwrap(),
            },
            req_elems,
        );
    }
    elements
}
