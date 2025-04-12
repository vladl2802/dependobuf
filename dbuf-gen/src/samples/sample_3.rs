// message Checker (n Nat) (l List n) {}

// message Kek (n Nat) (l List n) {
//     c1 Checker n l
//     c2 Checker (Suc n) (Cons "a" l)
//     c3 Checker (Suc Suc n) (Cons "b" (Cons "a" l))
// }

// (
//     "Checker".to_owned(),
//     Constructor {
//         implicits: vec![
//             (
//                 "n".to_owned(),
//                 Expression::Type {
//                     name: "Nat".to_owned(),
//                     dependencies: Rec::new([]),
//                 },
//             ),
//             (
//                 "l".to_owned(),
//                 Expression::Type {
//                     name: "List".to_owned(),
//                     dependencies: Rec::new([]),
//                 },
//             ),
//         ],
//         fields: Vec::new(),
//         result_type: Expression::Type {
//             name: "Checker".to_owned(),
//             dependencies: Rec::new([
//                 Expression::Type {
//                     name: "Nat".to_owned(),
//                     dependencies: Rec::new([]),
//                 },
//                 Expression::Type {
//                     name: "List".to_owned(),
//                     dependencies: Rec::new([Expression::Variable {
//                         name: "n".to_owned(),
//                     }]),
//                 },
//             ]),
//         },
//     },
// )

// (
//     "Kek".to_owned(),
//     Constructor {
//         implicits: vec![
//             (
//                 "n".to_owned(),
//                 Expression::Type {
//                     name: "Nat".to_owned(),
//                     dependencies: Rec::new([]),
//                 },
//             ),
//             (
//                 "l".to_owned(),
//                 Expression::Type {
//                     name: "List".to_owned(),
//                     dependencies: Rec::new([]),
//                 },
//             ),
//         ],
//         fields: vec![
//             (
//                 "c1".to_owned(),
//                 Expression::Type {
//                     name: "Checker".to_owned(),
//                     dependencies: Rec::new([
//                         Expression::Variable {
//                             name: "n".to_owned(),
//                         },
//                         Expression::Variable {
//                             name: "l".to_owned(),
//                         },
//                     ]),
//                 },
//             ),
//             (
//                 "c2".to_owned(),
//                 Expression::Type {
//                     name: "Checker".to_owned(),
//                     dependencies: Rec::new([
//                         Expression::Constructor {
//                             name: "Suc".to_owned(),
//                             implicits: Rec::new([]),
//                             arguments: Rec::new([Expression::Variable {
//                                 name: "n".to_owned(),
//                             }]),
//                         },
//                         Expression::Constructor {
//                             name: "Cons".to_owned(),
//                             implicits: Rec::new([Expression::Variable {
//                                 name: "n".to_owned(),
//                             }]),
//                             arguments: Rec::new([
//                                 Expression::OpCall(OpCall::Literal(
//                                     super::operators::Literal::Str("a".to_owned()),
//                                 )),
//                                 Expression::Variable {
//                                     name: "l".to_owned(),
//                                 },
//                             ]),
//                         },
//                     ]),
//                 },
//             ),
//             (
//                 "c3".to_owned(),
//                 Expression::Type {
//                     name: "Checker".to_owned(),
//                     dependencies: Rec::new([
//                         Expression::Constructor {
//                             name: "Suc".to_owned(),
//                             implicits: Rec::new([]),
//                             arguments: Rec::new([Expression::Constructor {
//                                 name: "Suc".to_owned(),
//                                 implicits: Rec::new([]),
//                                 arguments: Rec::new([Expression::Variable {
//                                     name: "n".to_owned(),
//                                 }]),
//                             }]),
//                         },
//                         Expression::Constructor {
//                             name: "Cons".to_owned(),
//                             implicits: Rec::new([Expression::Constructor {
//                                 name: "Suc".to_owned(),
//                                 implicits: Rec::new([]),
//                                 arguments: Rec::new([Expression::Variable {
//                                     name: "n".to_owned(),
//                                 }]),
//                             }]),
//                             arguments: Rec::new([
//                                 Expression::OpCall(OpCall::Literal(
//                                     super::operators::Literal::Str("b".to_owned()),
//                                 )),
//                                 Expression::Constructor {
//                                     name: "Cons".to_owned(),
//                                     implicits: Rec::new([Expression::Variable {
//                                         name: "n".to_owned(),
//                                     }]),
//                                     arguments: Rec::new([
//                                         Expression::OpCall(OpCall::Literal(
//                                             super::operators::Literal::Str("a".to_owned()),
//                                         )),
//                                         Expression::Variable {
//                                             name: "l".to_owned(),
//                                         },
//                                     ]),
//                                 },
//                             ]),
//                         },
//                     ]),
//                 },
//             ),
//         ],
//         result_type: Expression::Type {
//             name: "Kek".to_owned(),
//             dependencies: Rec::new([
//                 Expression::Type {
//                     name: "Nat".to_owned(),
//                     dependencies: Rec::new([]),
//                 },
//                 Expression::Type {
//                     name: "List".to_owned(),
//                     dependencies: Rec::new([Expression::Variable {
//                         name: "n".to_owned(),
//                     }]),
//                 },
//             ]),
//         },
//     },
// )
