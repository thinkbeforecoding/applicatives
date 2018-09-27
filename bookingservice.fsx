[<AutoOpen>]
module AppService
#r "packages/fsharp.data/lib/net45/fsharp.data.dll"
#r "packages/newtonsoft.json/lib/net45/newtonsoft.json.dll"
open System
open Newtonsoft.Json
open FSharp.Data

module BookingService =
    
    let serializer = JsonSerializer.Create()

    let deserialize<'a> input : 'a =
        use r = new IO.StringReader(input)
        let jr = new JsonTextReader(r) 
        serializer.Deserialize<'a>(jr)


    let getBooking reference properties =
        let props = String.concat "," properties
        Http.RequestString(sprintf "http://localhost:9200/bookings/_doc/%s/_source" reference, query = ["_source_include", props])
        |> deserialize<Map<string,string>>

