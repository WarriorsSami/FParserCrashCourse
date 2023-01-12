open FParserCrashCourse
open FParserCrashCourse.Query

let books = Books.getAll()

let input = """
filterby Category = 'Fantasy'
orderby Rating desc
take 1
"""

let result = parse input

match result with
| Result.Ok res ->
    let queryResult = execute res books
    List.iter (fun i -> printfn $"{i}") queryResult
| Result.Error err -> printfn $"Error: {err}"
