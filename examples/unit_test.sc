
// asap 
@[testcase]
test_case_1 :: () => {
    assert calc { 2 == (1+1) };
}

// important
@[benchmark]
benchmark_my_fn :: () => {
    fn();
}

// also would be noice to have
// fuzz_test_case_2 :: (i: fuzz.integer[min: -1, max: 1]) => {
//     fn_i(i);
// }
