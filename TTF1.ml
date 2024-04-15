
  (* MORE STUDENTS *)
type student = {
  id: int;
  name: string;
  grades: float list;
};;

type database = student list;;

let stud1 = {id = 1; name = "Tuxa"; grades = [98.; 98.; 88.; 88.; 74.]};;
let stud2 = {id = 2; name = "Lashuki"; grades = [70.5; 80.; 74.]};;
let stud3 = {id = 3; name = "Lekho"; grades = [89.; 90.; 80.]};;
let stud4 = {id = 4; name = "Gogicha"; grades = [89.9; 85.]};;

let db = [stud1; stud2; stud3; stud4];;



(* helper functions *)
let rec student_grade_sum grades = match grades with
  [] -> 0.0 |
  x::xs -> x +. student_grade_sum xs;;

let rec student_grade_count grades = match grades with 
  [] -> 0. |
  x::xs -> 1. +. student_grade_count xs;;

let rec students_grade_count db = match db with 
[] -> 0.0 |
x::xs -> student_grade_sum x.grades /. student_grade_count x.grades +. students_grade_count xs;;

let rec to_float n = match n with 
  0 -> 0. |
  _ -> 1. +. to_float (n - 1);;

(* end of helper functions *)



let rec remove_by_id id db = match db with
  [] -> [] |
  x::xs -> if x.id = id then xs else x::remove_by_id id db;;

let rec count_in_semester db = match db with
  [] -> 0 |
  x::xs -> 1 + count_in_semester xs;;

let rec student_avg_grade id db = match db with 
  [] -> 0.0 | 
  x::xs -> if x.id = id then student_grade_sum x.grades /. student_grade_count x.grades else student_avg_grade id xs;;

let rec course_average_grade db = match db with 
  [] -> 0.0 |
  x::xs -> students_grade_count db /. to_float (count_in_semester db);;


(* List Mismash *)
let rec interleave l1 l2 l3 = match l1, l2, l3 with 
  x::xs, y::ys, z::zs -> x::y::z::interleave xs ys zs |
  [], [], [] | _::_, _::_, [] |_::_, [], _::_ | [], _::_, _::_ | _::_, [], [] | [], _::_, [] | [], [], _::_ -> [];;


(* OCamlification *)
let rec foo x y b = if x > y then foo y x b else
  match x < y with 
    true -> if b then foo (x + 1) y (not b) else foo x (y - 1) (not b) |
    false -> x;;


(* Polynomial Party *)
