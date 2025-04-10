fn loop_benchmark(n: i32) -> i32 {
    let mut sum = 0;
    for i in 0..n {
        sum += i;
    }
    sum
}

fn main(){
    loop_benchmark(100_000_000);
}