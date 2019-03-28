macro_rules! catch {
    ($($body:tt)*) => { (||{ $($body)* })() }
}