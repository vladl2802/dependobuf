use std::io::{BufReader, BufWriter};

#[allow(warnings)]
#[allow(clippy::all)]
mod basic {
    include!("./canon/basic.rs");
}

#[test]
fn basic_counting() {
    let four1 = {
        let zero = basic::Nat::zero().expect("couldn't construct zero");
        let one = basic::Nat::suc(Box::new(zero)).expect("couldn't construct one");

        let mut following = one;
        for _ in 2..5 {
            following = basic::Nat::suc(Box::new(following)).expect("couldn't construct Following");
        }
        following
    };

    let four2 = {
        let zero = basic::Nat::zero().expect("couldn't construct zero");
        let one = basic::Nat::suc(Box::new(zero)).expect("couldn't construct one");
        let two = basic::Nat::suc(Box::new(one)).expect("couldn't construct two");
        let three = basic::Nat::suc(Box::new(two)).expect("couldn't construct three");

        basic::Nat::suc(Box::new(three)).expect("couldn't construct four")
    };

    assert_eq!(four1, four2);
}

#[test]
fn basic_serde() {
    let zero = basic::Nat::zero().expect("couldn't construct zero");
    let one = basic::Nat::suc(Box::new(zero)).expect("couldn't construct one");

    let mut writer = BufWriter::new(Vec::new());
    one.clone()
        .serialize(&mut writer)
        .expect("couldn't serialize to writer");

    let buffer = writer.into_inner().expect("couldn't retrieve buffer");

    let mut reader = BufReader::new(buffer.as_slice());
    let one_new = basic::Nat::deserialize(basic::nat::Dependencies {}, &mut reader)
        .expect("couldn't deserialize");

    assert_eq!(one, one_new);
}

#[allow(warnings)]
#[allow(clippy::all)]
mod nat_vec {
    include!("./canon/nat_vec.rs");
}

#[test]
fn nat_vec_extending() {
    let nil = nat_vec::Vec::nil().expect("couldn't construct nil");
    let cons = nat_vec::Vec::cons(
        nil.dependencies.n.clone(),
        Box::new(nat_vec::Nat::zero().expect("couldn't generate zero")),
        Box::new(nil),
    )
    .expect("couldn't construct cons");

    let mut following = cons;
    for _ in 2..5 {
        following = nat_vec::Vec::cons(
            following.dependencies.n.clone(),
            Box::new(nat_vec::Nat::zero().expect("couldn't generate zero")),
            Box::new(following),
        )
        .expect("couldn't construct Following");
    }
}

#[test]
fn nat_vec_serde() {
    let len = nat_vec::Nat::suc(Box::new(
        nat_vec::Nat::zero().expect("couldn't construct zero"),
    ))
    .expect("couldn't construct one");

    let vec = nat_vec::Vec::cons(
        Box::new(nat_vec::Nat::zero().expect("couldn't construct zero")),
        Box::new(nat_vec::Nat::zero().expect("couldn't construct zero")),
        Box::new(nat_vec::Vec::nil().expect("couldn't construct nil")),
    )
    .expect("couldn't construct cons");

    let mut writer = BufWriter::new(Vec::new());
    vec.clone()
        .serialize(&mut writer)
        .expect("couldn't serialize to writer");

    let buffer = writer.into_inner().expect("couldn't retrieve buffer");

    let mut reader: BufReader<&[u8]> = BufReader::new(buffer.as_slice());
    let vec_new =
        nat_vec::Vec::deserialize(nat_vec::vec::Dependencies { n: Box::new(len) }, &mut reader)
            .expect("couldn't deserialize");

    assert_eq!(vec, vec_new);
}
