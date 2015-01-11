open System
open System.IO

type QuoteStatus =
    | Unquoted
    | Quoted
    | Whitespace

let rec textParse' (c : char :: rest : char list) (cell : string) (row : string list) (doc : string list list) (status : QuoteStatus) =
    match (status, c, rest) with
    | (_, '"', [])                       -> (doc @ [row @ [cell]]) // End of the file. Wrap everything up
    | (_, _, [])                         -> (doc @ [row @ [cell + c.ToString()]]) // End of the file. Wrap everything up
    | (Unquoted, '"', _)                 -> textParse' rest "" row doc Quoted // Discard all the whitespace before in the cell
    | (Quoted, '"', _)                   -> textParse' rest cell row doc Whitespace // Until the comma, discard the whitespace
    | ((Unquoted | Whitespace), '\n', _) -> textParse' rest "" ([]) (doc @ [row @ [cell]]) Unquoted // End of line: add cell to row and row to doc
    | ((Unquoted | Whitespace), ',', _)  -> textParse' rest "" (row @ [cell]) doc Unquoted // End of cell. Add cell to row
    | (Whitespace, _, _)                 -> textParse' rest cell row doc status // Discard the whitespace
    | (_, _, _)                          -> textParse' rest (cell + c.ToString()) row doc status // Add the char to the cell

let textParse (csv : string) =
    let csvChar = List.ofSeq csv
    match csvChar with
    | [] -> [] : string list list
    | _  -> textParse' csvChar "" [] [] Unquoted

let readFile (path : string) =
    use reader = new StreamReader(path)
    reader.ReadToEnd()

[<EntryPoint>]
let main argv = 
    let csvStr = readFile "/Users/noelh/Projects/fs-csv/test.csv"
    let doc = textParse csvStr
    List.iter (List.iter (fun z -> printfn "%s" z)) doc
    0
