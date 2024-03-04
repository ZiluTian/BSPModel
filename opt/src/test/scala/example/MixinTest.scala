// package BSPModel

// class DynamicMixinSpec extends munit.FunSuite {

//   test("say hello") {

//     class Foo {
//       def bar = 5
//     }

//     trait Optimization {
//         def transform(h: HierarchicalBSP): HierarchicalBSP
//     }

//     trait Spam extends Optimization {
//       def transform() = 10
//     }

//     trait Ham extends Optimization {
//         def chicken = 30
//     }

//     assert((new Foo with Spam).eggs == 10)
//   }
// }
