#[derive(Debug)]
pub struct ParseResult {
    ignored: Vec<String>,
}

impl ParseResult {
    pub fn new() -> Self {
        ParseResult { ignored: vec![] }
    }
}

pub fn parse(content: &str) -> ParseResult {
    let mut result = ParseResult::new();

    for l in content.lines() {
        result.ignored.push(l.to_string())
    }

    result
}

#[cfg(test)]
mod tests {

    use super::*;

    // Scenario: Ignoring unrecognized lines
    //   Given gibberish ← a file containing:
    //     """
    //     There was a young lady named Bright
    //     who traveled much faster than light.
    //     She set out one day
    //     in a relative way,
    //     and came back the previous night.
    //     """
    //   When parser ← parse_obj_file(gibberish)
    //   Then parser should have ignored 5 lines
    #[test]
    fn ignoring_unrecognized_lines() {
        let content = "There was a young lady named Bright
             who traveled much faster than light.
             She set out one day
             in a relative way,
             and came back the previous night.";

        let res = parse(content);

        println!("res {:#?}", res);
        assert_eq!(5, res.ignored.len())
    }
}
