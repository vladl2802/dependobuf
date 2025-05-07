mod tests {
    mod basic {
        include!("./canon/basic.rs");
    }

    #[test]
    fn basic() {}

    mod nat_vec {
        include!("./canon/nat_vec.rs");
    }
}
