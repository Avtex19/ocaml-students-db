type student = {
  firstName : string;
  lastName: string;
  id: int;
  currentSemester: int;
  grades: (int*float) list;
}

type database = student list;;

let insert std db = std::db;;

let  rec find_by_id id db = match db with
    |[] -> []
    |x::xs -> if x.id = id then [x] else find_by_id id xs 

let find_by_last_name ln db = 
  let rec helper ln db acc = match db with 
  |[] -> acc
  |x::xs -> if x.lastName = ln then helper ln xs (x.firstName::acc) else helper ln xs acc 
in helper ln db [];;

let remove_by_id id db = 
  let rec helper id db acc = match db with
  |[] -> acc
  |x::xs -> if x.id = id then helper id xs acc else helper id xs acc@[x] 
in helper id db [];;

let count_in_semester cs db = 
  let rec helper cs db acc = match db with
  |[] -> acc
  |x::xs -> if x.currentSemester = cs then helper cs xs (1+acc) else helper cs xs acc 
in helper cs db 0;;

let student_avg_grade id db = 
  let myStudent = match find_by_id id db with |[] -> failwith "No student with that ID" | x::_ -> x
in 
let rec helper grades acc1 acc2 = 
  match grades with 
|[] -> if acc1 = 0.0 && acc2 = 0.0 then 0.0 else acc1/. acc2
|(_,grade)::xs -> helper (xs) (acc1 +. grade) (acc2 +. 1.0)
in helper (myStudent.grades) (0.0) (0.0)

let course_avg_grade semester db = 
  let rec getGradesInCurrSemester semester db acc = match db with 
  |[] -> acc
  |x::xs ->
    let rec getGrades grades =  match grades with 
  |[] -> getGradesInCurrSemester semester xs acc 
  |(sem,grd)::tail -> if semester = sem then getGradesInCurrSemester semester xs (grd::acc) else getGrades tail 
in 
    getGrades x.grades in
    let gradesInSemester = getGradesInCurrSemester semester db [] in
    let rec countAverage lst acc = match lst with 
    |[] -> acc /. float_of_int (List.length gradesInSemester)
    |x::xs -> countAverage xs (acc +. x) in 
    countAverage gradesInSemester 0.0;;
