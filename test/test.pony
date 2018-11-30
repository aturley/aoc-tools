use ".."
use "ponytest"

actor Main is TestList
  new create(env: Env) =>
    PonyTest(env, this)

  new make() =>
    None

  fun tag tests(test: PonyTest) =>
    test(_TestRotateArrayRight)
    test(_TestRotateArrayLeft)
    test(_TestGrid)
    test(_TestSparseGrid)

class iso _TestRotateArrayRight is UnitTest
  fun name():String => "RotateArrayRight"

  fun apply(h: TestHelper) ? =>
    let i1 = [as U32: 1; 2; 3; 4]
    let t1 = RotateArray[U32](i1, 1)?
    let e1 = [as U32: 4; 1; 2; 3]
    h.assert_array_eq[U32](t1, e1)

class iso _TestRotateArrayLeft is UnitTest
  fun name():String => "RotateArrayLeft"

  fun apply(h: TestHelper) ? =>
    let i2 = [as U32: 1; 2; 3; 4]
    let t2 = RotateArray[U32](i2, -1)?
    let e2 = [as U32: 2; 3; 4; 1]
    h.assert_array_eq[U32](t2, e2)

class iso _TestGrid is UnitTest
  fun name():String => "Grid"

  fun apply(h: TestHelper) ? =>
    let g = Grid[String](3, 4, "")
    g(1, 2)? = "b"
    h.assert_eq[String](g(1, 2)?, "b")

class iso _TestSparseGrid is UnitTest
  fun name():String => "SparseGrid"

  fun apply(h: TestHelper) ? =>
    let g = SparseGrid[String]
    g(1, 2) = "b"
    h.assert_eq[String](g(1, 2)?, "b")
