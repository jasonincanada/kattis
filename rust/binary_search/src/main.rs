/*
    Rust code for the generalized binary search from Brent Yorgey's blog:
    [1] https://byorgey.wordpress.com/2023/01/01/competitive-programming-in-haskell-better-binary-search/

    The article linked in the above entry: Binary Search a Little Simpler & More Generic:
    [2] https://julesjacobs.com/notes/binarysearch/binarysearch.pdf
*/

fn main() {
    println!("run 'cargo test' instead");
}

// fully generic search function ported from Yorgey's blog [1]. notice no constraints on type A!
// if there are any, they're applied by the particular mid function we choose
fn search<F,P,A>(mid  : F,
                 pred : P,
                 left : A,
                 right: A) -> (A, A)
where
    F: Fn(&A, &A) -> Option<A>,
    P: Fn(&A)     -> bool
{
    match mid(&left, &right) {
        Some(m) if pred(&m) => search(mid, pred, left, m),
        Some(m)             => search(mid, pred, m, right),
        None                => (left, right)
    }
}

// custom "mid" function to pass as the first argument to search() when we need a binary search
fn binary(left : &i64,
          right: &i64) -> Option<i64>
{
    if right - left > 1 {
        Some((left+right) / 2)
    } else {
        None
    }
}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    // the array from p. 2 of [2]
    const ARRAY: [usize; 8] = [2, 3, 3, 3, 6, 8, 8, 9];

    #[test] fn test_search_ge_7()
    {
        // find the first number >= 7
        let predicate = |i: &i64| ARRAY[*i as usize] >= 7;

        let index_pair = search(binary,
                                predicate,
                                -1,
                                ARRAY.len() as i64);

        // the search should return the pair of consecutive indices between which the
        // predicate flips from false to true (between the 6 and the 8 in ARRAY)
        assert_eq!(index_pair, (4,5));

        // for this problem we want the right-side index
        let index = index_pair.1 as usize;        
        assert_eq!(ARRAY[index], 8);
    }

    #[test] fn test_search_no_result()
    {
        // find the first number < 2 (there isn't one in the array)
        let predicate = |i: &i64| ARRAY[*i as usize] < 2;

        let index_pair = search(binary,
                                predicate,
                                -1,
                                ARRAY.len() as i64);

        // the returned pair of indices should be (len-1, len). for binary search we
        // use the right element of the pair as the final index, but if it's still set
        // to len() it'll be out of bounds; ie, the search didn't find a valid index
        assert_eq!(index_pair, (ARRAY.len() as i64 - 1,
                                ARRAY.len() as i64));
    }

    // this is the "integer square root" example from [1]
    #[test] fn test_square_root()
    {
        // find the pair of consecutive numbers where the second number squared is >= 150.
        // or said differently, find the bounding integers for the square root of 150
        let predicate = |x: &i64| x*x >= 150;

        let number_pair = search(binary,
                                 predicate,
                                 0,
                                 100);

        assert_eq!(number_pair, (12,13)); // because 12 < sqrt(150) <= 13
    }
}
