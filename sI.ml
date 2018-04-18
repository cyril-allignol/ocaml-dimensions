type ('length, 'mass, 'time, 'temperature) t = float

type 'n s = S of 'n

type 'i zero = 'i * 'i
type 'i one = 'i * 'i s
type 'i two = 'i * 'i s s
type 'i three = 'i * 'i s s s
type 'i four = 'i * 'i s s s s
type 'i m_one = 'i s * 'i
type 'i m_two = 'i s s * 'i
type 'i m_three = 'i s s s * 'i
type 'i m_four = 'i s s s s * 'i

type ('l, 'm, 't, 'tp) length = ('l one, 'm zero, 't zero, 'tp zero) t
type ('l, 'm, 't, 'tp) mass = ('l zero, 'm one, 't zero, 'tp zero) t
type ('l, 'm, 't, 'tp) time = ('l zero, 'm zero, 't one, 'tp zero) t
type ('l, 'm, 't, 'tp) temperature = ('l zero, 'm zero, 't zero, 'tp one) t

type ('l, 'm, 't, 'tp) dimensionless = ('l zero, 'm zero, 't zero, 'tp zero) t
type ('l, 'm, 't, 'tp) scalar = ('l, 'm, 't, 'tp) dimensionless

type ('l, 'm, 't, 'tp) area = ('l two, 'm zero, 't zero, 'tp zero) t
type ('l, 'm, 't, 'tp) volume = ('l three, 'm zero, 't zero, 'tp zero) t

type ('l, 'm, 't, 'tp) speed = ('l one, 'm zero, 't m_one, 'tp zero) t
type ('l, 'm, 't, 'tp) acceleration = ('l one, 'm zero, 't m_two, 'tp zero) t

type ('l, 'm, 't, 'tp) frequency = ('l zero, 'm zero, 't m_one, 'tp zero) t
type ('l, 'm, 't, 'tp) force = ('l one, 'm one, 't m_two, 'tp zero) t
type ('l, 'm, 't, 'tp) pressure = ('l m_one, 'm one, 't m_two, 'tp zero) t
type ('l, 'm, 't, 'tp) energy = ('l two, 'm one, 't m_two, 'tp zero) t
type ('l, 'm, 't, 'tp) power = ('l two, 'm one, 't m_three, 'tp zero) t

type ('l, 'm, 't, 'tp) density = ('l m_three, 'm one, 't zero, 'tp zero) t

let c = 299_792_458.

let g = 6.674_083_1e-11

let planck_constant = 6.626_070_040_81e-34
let reduced_planck_constant = 1.054_571_800_13e-34
let planck_length = 1.616_229_38e-35
let planck_mass = 2.176_470_51e-8
let planck_time = 5.391_161_3e-44
let planck_charge = 1.875_545_956_41e-18
let planck_temperature = 1.416_808_33e32

type prefix =
  | Yotta | Zetta | Exa | Peta | Tera | Giga | Mega | Kilo | Hecto | Deca
  | Yocto | Zepto | Atto | Femto | Pico | Nano | Micro | Milli | Centi | Deci

let prefix_modifier = function
  | Yotta -> 1e24 | Yocto -> 1e-24
  | Zetta -> 1e21 | Zepto -> 1e-21
  | Exa -> 1e18 | Atto -> 1e-18
  | Peta -> 1e15 | Femto -> 1e-15
  | Tera -> 1e12 | Pico -> 1e-12
  | Giga -> 1e9 | Nano -> 1e-9
  | Mega -> 1e6 | Micro -> 1e-6
  | Kilo -> 1e3 | Milli -> 1e-3
  | Hecto -> 1e2 | Centi -> 1e-2
  | Deca -> 1e1 | Deci -> 1e-1

let sprint_prefix = function
  | Yotta -> "Y" | Yocto -> "y"
  | Zetta -> "Z" | Zepto -> "z"
  | Exa -> "E" | Atto -> "a"
  | Peta -> "P" | Femto -> "f"
  | Tera -> "T" | Pico -> "p"
  | Giga -> "G" | Nano -> "n"
  | Mega -> "M" | Micro -> "µ"
  | Kilo -> "k" | Milli -> "m"
  | Hecto -> "h" | Centi -> "c"
  | Deca -> "da" | Deci -> "d"

type (_, _, _, _) units =
  | Metre : ('l one, 'm zero, 't zero, 'tp zero) units
  | Nauticalmile : ('l one, 'm zero, 't zero, 'tp zero) units
  | Feet : ('l one, 'm zero, 't zero, 'tp zero) units
  | SquareMetre : ('l two, 'm zero, 't zero, 'tp zero) units
  | CubicMetre : ('l three, 'm zero, 't zero, 'tp zero) units
  | Kilogram : ('l zero, 'm one, 't zero, 'tp zero) units
  | Ton : ('l zero, 'm one, 't zero, 'tp zero) units
  | Second : ('l zero, 'm zero, 't one, 'tp zero) units
  | Minute : ('l zero, 'm zero, 't one, 'tp zero) units
  | Hour : ('l zero, 'm zero, 't one, 'tp zero) units
  | Day : ('l zero, 'm zero, 't one, 'tp zero) units
  | Kelvin : ('l zero, 'm zero, 't zero, 'tp one) units
  | Celsius : ('l zero, 'm zero, 't zero, 'tp one) units
  | Metre_per_Second : ('l one, 'm zero, 't m_one, 'tp zero) units
  | Knot : ('l one, 'm zero, 't m_one, 'tp zero) units
  | Feet_per_Minute : ('l one, 'm zero, 't m_one, 'tp zero) units
  | Pascal : ('l m_one, 'm one, 't m_two, 'tp zero) units

let sprint_unit : type l m ti tp. (l, m, ti, tp) units -> string
  = function
  | Metre -> "m" | Nauticalmile -> "NM" | Feet -> "ft"
  | SquareMetre -> "m2"
  | CubicMetre -> "m3"
  | Kilogram -> "kg" | Ton -> "t"
  | Second -> "s" | Minute -> "min" | Hour -> "h" | Day -> "d"
  | Kelvin -> "K" | Celsius -> "°C"
  | Metre_per_Second -> "m/s" | Knot -> "kt" | Feet_per_Minute -> "ft/min"
  | Pascal -> "Pa"

let convert_linear = fun a -> (fun v -> v *. a), (fun v -> v /. a)
let convert_offset = fun b -> (fun v -> v +. b), (fun v -> v -. b)
let convert_affine = fun a b -> (fun v -> v *. a +. b), (fun v -> (v -. b) /. a)

let nauticalmile_to_metre, metre_to_nauticalmile = convert_linear 1852.
let feet_to_metre, metre_to_feet = convert_linear 0.3048
let ton_to_kilogram, kilogram_to_ton = convert_linear 1000.
let minute_to_second, second_to_minute = convert_linear 60.
let hour_to_second, second_to_hour = convert_linear 3600.
let day_to_second, second_to_day = convert_linear 86400.
let celsius_to_kelvin, kelvin_to_celsius = convert_offset 273.15

let make : type l m ti tp. ?prefix:prefix -> (l, m, ti, tp) units -> float ->
                (l, m, ti, tp) t =
  fun ?prefix u v ->
  let si = match u with
    | Metre -> v
    | Nauticalmile -> v |> nauticalmile_to_metre
    | Feet -> v |> feet_to_metre
    | SquareMetre -> v
    | CubicMetre -> v
    | Kilogram -> v
    | Ton -> v |> ton_to_kilogram
    | Second -> v
    | Minute -> v |> minute_to_second
    | Hour -> v |> hour_to_second
    | Day -> v |> day_to_second
    | Kelvin -> v
    | Celsius -> v |> celsius_to_kelvin
    | Metre_per_Second -> v
    | Knot -> v |> nauticalmile_to_metre |> second_to_hour
    | Feet_per_Minute -> v |> feet_to_metre |> second_to_minute
    | Pascal -> v in
  match prefix with
  | None -> si
  | Some p -> si *. prefix_modifier p

let get_value : type l m ti tp. ?prefix:prefix -> (l, m, ti, tp) units ->
                     (l, m, ti, tp) t -> float
  = fun ?prefix u v ->
  let v = match u with
    | Metre -> v
    | Nauticalmile -> v |> metre_to_nauticalmile
    | Feet -> v |> metre_to_feet
    | SquareMetre -> v
    | CubicMetre -> v
    | Kilogram -> v
    | Ton -> v |> kilogram_to_ton
    | Second -> v
    | Minute -> v |> second_to_minute
    | Hour -> v |> second_to_hour
    | Day -> v |> second_to_day
    | Kelvin -> v
    | Celsius -> v |> kelvin_to_celsius
    | Metre_per_Second -> v
    | Knot -> v |> metre_to_nauticalmile |> hour_to_second
    | Feet_per_Minute -> v |> metre_to_feet |> minute_to_second
    | Pascal -> v in
  match prefix with
  | None -> v
  | Some p -> v /. prefix_modifier p

let convert = fun u1 u2 v -> v |> make u1 |> get_value u2

let compare : float -> float -> int = Pervasives.compare
let equal : float -> float -> bool = Pervasives.(=)
let le : float -> float -> bool = Pervasives.(<=)
let lt : float -> float -> bool = Pervasives.(<)
let ge : float -> float -> bool = Pervasives.(>=)
let gt : float -> float -> bool = Pervasives.(>)

let add = ( +. )
let sub = ( -. )
let mult = ( *. )
let div = ( /. )
let min : float -> float -> float = Pervasives.min
let max : float -> float -> float = Pervasives.max

type (_, _) double =
  | DoubleZero : ('a zero, 'b zero) double
  | DoubleOne : ('a one, 'b two) double
  | DoubleTwo : ('a two, 'b four) double
  | DoubleMOne : ('a m_one, 'b m_two) double
  | DoubleMTwo : ('a m_two, 'b m_four) double

let square = fun dl dm dt dtp x -> x *. x
let sqrt = fun dl dm dt dtp x -> sqrt x

let opp = fun v -> -. v
let inv = fun v -> 1. /. v

let floor = Pervasives.floor
let ceil = Pervasives.ceil
let abs = Pervasives.abs_float

let mod_t = Pervasives.mod_float

module Operators = struct
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mult
  let ( / ) = div
  let ( ~- ) = opp
  let ( ** ) = Pervasives.( ** )
  let ( = ) = equal
  let ( <> ) : float -> float -> bool = Pervasives.(<>)
  let ( <= ) = le
  let ( < ) = lt
  let ( >= ) = ge
  let ( > ) = gt
  let ( >> ) = convert
end

let cos = Pervasives.cos
let sin = Pervasives.sin
let tan = Pervasives.tan
let acos = Pervasives.acos
let asin = Pervasives.asin
let atan = Pervasives.atan
let atan2 = Pervasives.atan2
let cosh = Pervasives.cosh
let sinh = Pervasives.sinh
let tanh = Pervasives.tanh
let exp = Pervasives.exp
let ln = Pervasives.log
let log = Pervasives.log
let ln2 = ln 2.
let ln10 = ln 10.
let log2 = fun x -> ln x /. ln2
let log10 = fun x -> ln x /. ln10
