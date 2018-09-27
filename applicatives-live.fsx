
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//
//           Applicatives IRL             //
//                                        //
//              <*> <*> <*>               //
//                                        //
// By: Jeremie Chassaing                  //
//                                        //
// (Availpro - FastBooking / Accor)       //
//                                        //
// https://thinkbeforecoding.com          //
//                                        //
//                         @thinkb4coding //
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//

let add x y = x + y


module Func =
    let apply f x = f x
    let (<*>) = apply

    add <*> 5 <*> 6

module Options =
    let x = Some 5

    let map f o =
        match o with
        | Some v -> Some (f v)
        | None -> None

    let map2 f ox oy =
        match ox, oy with
        | Some vx, Some vy -> Some (f vx vy)
        | _ -> None


    let what = map add (Some 5)

    let apply of' ox =
        map2 (fun f x -> f x) of' ox

    apply what (Some 6)         

    let (<!>) = map

    let (<*>) = apply

    add <!> Some 5 <*> Some 6

    let madd x y acc = acc + x * y

    madd <!> Some 5 <*> Some 6 <*> Some 7





module Results =
    let map f r =
        match r with
        | Ok v -> Ok (f v)
        | Error e -> Error e

    
    let map2 f rx ry =
        match rx, ry with
        | Ok vx, Ok vy -> Ok (f vx vy)
        | Error e, Ok _ 
        | Ok _, Error e -> Error e
        | Error ex, Error ey -> Error (ex + "\n" + ey)


    let apply rf rx =
        map2 (fun f x -> f x) rf rx

    let (<!>) = map
    let (<*>) = apply


    type User = {
        FirstName: string
        LastName: string
        Age: int
    }

    let build fn ln age = {
        FirstName = fn
        LastName = ln
        Age = age
    }

    let tryParse input =
        match System.Int32.TryParse input with
        | true, v -> Ok v
        | _ -> Error "Age should be an integer"

    let notEmpty input =
        if System.String.IsNullOrEmpty input then
            Error "Value should not be empty"
        else
            Ok input

    build
    <!> notEmpty "Jérémie"
    <*> notEmpty ""
    <*> tryParse "not42"






module Series =
    open System
    let month m d = DateTime(2018,m,d)
    let sep = month 9
    let oct = month 10
    let nov = month 11
    let dec = month 12

    fsi.AddPrinter(fun (d:DateTime) -> d.ToString("yyyy-MM-dd"))

    type Series<'a> = Series of 'a * ('a * DateTime) list

    let series s ps : Series<'a> = Series(s, ps)
    let mapl f (x,y) = f x, y    
    module List =
        let mapl f l = List.map (mapl f) l


    let map f (Series(s, ps)) =
        Series(f s, List.mapl f ps)



    let map2 f (Series(sx, psx)) (Series(sy, psy)) =
        let rec loop x y px py result =
            match px, py with
            | [], [] -> List.rev result
            | (vx,dx) :: tx, [] ->
                loop vx y tx [] ( (f vx y, dx) :: result )
            | [] , (vy,dy) :: ty ->
                loop x vy [] ty ( (f x vy, dy) :: result )
            | (vx,dx) :: tx, (vy,dy) :: ty ->
                if dx < dy then
                   loop vx y tx py ( (f vx y, dx) :: result ) 
                elif dx > dy then
                    loop x vy px ty ( (f x vy, dy) :: result )
                else
                    loop vx vy tx ty ( (f vx vy, dx) :: result )

        Series(f sx sy, loop sx sy psx psy [])

    let apply sf sx =
        map2 (fun f x -> f x) sf sx

    let (<!>) = map
    let (<*>) = apply
    let x = series 0 [ 1, sep 28; 2, sep 30; 0, oct 1]
    let y = series 100 [ 110, sep 1; 120, sep 29]


    add 
    <!> x
    <*> y

    let totalRevenue roomCount unitPrice closed =
        if closed then
            0m
        else
            decimal roomCount * unitPrice

    let roomCount = series 0 [ 2, sep 28; 1, oct 1] 
    let price = series 100m [ 120m, sep 29 ]

    let closed = series false [ true, sep 30; false, oct 1 ]

    totalRevenue
    <!> roomCount
    <*> price
    <*> closed










#load "bookingservice.fsx"

let booking =  BookingService.getBooking "JZ24C" ["TotalPrice"; "Arrival"; "Departure"; "Currency"] 
booking.["Arrival"]

module Services =
    open System
    type Query<'t> = {
        Properties: string Set
        GetValue: Map<string,string> -> 't
    } 


    let prop name = {
        Properties = set [name]
        GetValue= fun m -> m.[name]
    }

    let map f query =
        {
            Properties = query.Properties
            GetValue = fun m -> query.GetValue m |> f
        }

    let map2 f qx qy =
        {
            Properties = Set.union qx.Properties qy.Properties
            GetValue =
                fun m ->
                    let vx = qx.GetValue m
                    let vy = qy.GetValue m
                    f vx vy
        }


    let apply qf qx =
        map2 (fun f x -> f x) qf qx


    let (<!>) = map
    let (<*>) = apply


    let arrival = System.DateTime.Parse <!> prop "Arrival"
    let departure = System.DateTime.Parse <!> prop "Departure"


    let length (s: System.DateTime) (e: System.DateTime ) =
       e - s

    let l =
        length
        <!> arrival
        <*> departure


    let getbooking reference query =
        BookingService.getBooking reference (Set.toList query.Properties)
        |> query.GetValue

    getbooking "JZ24C" l

    let totalPrice = prop "TotalPrice"
