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


module Option =
    ()

































module Results =
    ()




































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
    let x = series 0 [ 1, sep 28; 2, sep 30; 0, oct 1]
    let y = series 100 [ 100, sep 1; 120, sep 29]

    let mapl f (x,y) = f x, y    
    module List =
        let mapl f l = List.map (mapl f) l





















































#load "bookingservice.fsx"

let booking =  BookingService.getBooking "JZ24C" ["TotalPrice"; "Arrival"; "Departure"; "Currency"] 
booking.["Arrival"]
























module Services =
    open System
    type Query<'t> = {
        Properties: string Set
        GetValue: Map<string,string> -> 't
    } 
