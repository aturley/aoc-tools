use "collections"
use "files"

class val AOCAppError
  let _msg: String

  new val create(s: String) =>
    _msg = s

  fun msg(): String =>
    _msg

trait AOCApp
  fun part1(file_lines: Array[String] val): (String | AOCAppError) ? =>
    AOCAppError("part1 is not implemented")

  fun part2(file_lines: Array[String] val): (String | AOCAppError) ? =>
    AOCAppError("part2 is not implemented")

primitive AOCAppRunner
  fun apply(aoc_app: AOCApp, env: Env) =>
    let file_name = try
      env.args(1)?
    else
      env.err.print("file name argument is missing")
      return
    end

    let part = try
      env.args(2)?
    else
      env.err.print("part argument is missing")
      return
    end

    let file_lines = try
      SimpleFileReader.as_lines(file_name, env.root as AmbientAuth)?
    else
      env.err.print("problem reading input file")
      return
    end

    let part_fun = match part
    | "1" =>
      aoc_app~part1()
    | "2" =>
      aoc_app~part2()
    else
      env.err.print("part must be 1 or 2")
      return
    end

    try
      match part_fun(file_lines)?
      | let s: String =>
        env.out.print(s)
      | let e: AOCAppError =>
        env.err.print(e.msg())
      end
    else
      env.err.print("there was an error running the app")
    end

primitive RotateArray
  fun apply[T: Any](a: Array[T] ref, n: ISize): Array[T!] ref ? =>
    if n.abs() >= a.size() then
      error
    end

    let arr = Array[T!]

    let nn = if n > 0 then
      n.usize()
    else
      a.size() + n.usize()
    end

    a.copy_to(arr, a.size() - nn, 0, nn)
    a.copy_to(arr, 0, nn, a.size() - nn)

    arr

class Grid[T]
  let _dim_x: USize
  let _dim_y: USize

  let _values: Array[T!]

  new create(dx: USize, dy: USize, init: T) =>
    _dim_x = dx
    _dim_y = dy
    _values = Array[T!].init(init, _dim_x * _dim_y)

  fun _c(x: USize, y: USize): USize ? =>
    if (x >= _dim_x) or (y >= _dim_y) then
      error
    end

    x + (y * _dim_y)

  fun apply(x: USize, y: USize): this->T ? =>
    _values(_c(x, y)?)?

  fun ref update(x: USize, y: USize, value: T) ? =>
    _values(_c(x, y)?)? = value

class val _GridCoord
  let x: ISize
  let y: ISize

  new val create(xx: ISize, yy: ISize) =>
    x = xx
    y = yy

  fun eq(that: box->_GridCoord): Bool =>
    (this.x == that.x) and (this.y == that.y)

  fun ne(that: box->_GridCoord): Bool =>
    not eq(that)

  fun hash(): USize =>
    (x.abs().usize() * 7913813) + y.abs().usize()

class SparseGrid[T]
  let _values: Map[_GridCoord, T!]

  new create() =>
    _values = Map[_GridCoord, T!]

  fun apply(x: ISize, y: ISize): this->T ? =>
    _values(_GridCoord(x, y))?

  fun ref update(x: ISize, y: ISize, value: T) =>
    _values(_GridCoord(x, y)) = value

  fun values(): MapValues[_GridCoord, T!, HashEq[_GridCoord val],
    this->HashMap[_GridCoord, T!, HashEq[_GridCoord val]]]^ =>

    _values.values()

class val GridWalkerCardinal
  let _x: ISize
  let _y: ISize

  new val create(x: ISize, y: ISize) =>
    _x = x
    _y = y

  fun xy(): (ISize, ISize) =>
    (_x, _y)

  fun north(): GridWalkerCardinal =>
    GridWalkerCardinal(_x, _y - 1)

  fun south(): GridWalkerCardinal =>
    GridWalkerCardinal(_x, _y + 1)

  fun east(): GridWalkerCardinal =>
    GridWalkerCardinal(_x + 1, _y)

  fun west(): GridWalkerCardinal =>
    GridWalkerCardinal(_x - 1, _y)

primitive North
primitive South
primitive East
primitive West

type Direction is (North | South | East | West)

class val GridWalkerOriented
  let _x: ISize
  let _y: ISize
  let _d: Direction

  new val create(x: ISize, y: ISize, d: Direction) =>
    _x = x
    _y = y
    _d = d

  fun xyd(): (ISize, ISize, Direction) =>
    (_x, _y, _d)

  fun _d_to_offset(d: Direction): (ISize, ISize) =>
    match d
    | North => (0, -1)
    | South => (0, 1)
    | East => (1, 0)
    | West => (-1, 0)
    end

  fun forward(): GridWalkerOriented =>
    let offset = _d_to_offset(_d)
    GridWalkerOriented(_x + offset._1, _y + offset._2, _d)

  fun cw(): GridWalkerOriented =>
    let d = match _d
    | North => East
    | South => West
    | East => South
    | West => North
    end

    GridWalkerOriented(_x, _y, d)

  fun ccw(): GridWalkerOriented =>
    let d = match _d
    | North => West
    | South => East
    | East => North
    | West => South
    end

    GridWalkerOriented(_x, _y, d)

primitive SimpleFileReader
  fun as_lines(file_name: String, auth: AmbientAuth): Array[String] val ? =>
    let lines = recover trn Array[String] end
    let path = FilePath(auth, file_name)?
    with f = OpenFile(path) as File do
      for l in FileLines(f) do
        lines.push(consume l)
      end
    end

    consume lines

class Counter[T: (Hashable val & Equatable[T] val)]
  let _values: Map[T, USize]

  new create() =>
    _values = _values.create()

  fun ref add(v: T) =>
    try
      _values.upsert(v, 1, {(x, y) => x + y})?
    end

  fun pairs(): Iterator[(this->T, USize)] =>
    _values.pairs()

  fun max(tb: {(this->T, this->T): this->T} = {(o, n) => o}):
    (this->T, USize) ?
  =>
    let pairs' = _values.pairs()
    (var max_value, var max_count)= pairs'.next()?

    for (v, c) in pairs' do
      if c > max_count then
        max_value = v
        max_count = c
      elseif c == max_count then
        max_value = tb(max_value, v)
      end
    end

    (max_value, max_count)

  fun min(tb: {(this->T, this->T): this->T} = {(o, n) => o}):
    (this->T, USize) ?
  =>
    let pairs' = _values.pairs()
    (var min_value, var min_count)= pairs'.next()?

    for (v, c) in pairs' do
      if c > min_count then
        min_value = v
        min_count = c
      elseif c == min_count then
        min_value = tb(min_value, v)
      end
    end

    (min_value, min_count)
