module CodeGen

open System
open System.Text
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Core.Printf
open System.IO

let tabs count = new String(' ', count*4)
let indentTabs count str = sprintf "%s%s" (tabs count) str 
let printTabs (tw : TextWriter) count = tw.Write(new String(' ',count*4))

let genConstructor (objType: Type)= 
    let asm = objType.Assembly
    let rec loop (acc:Map<string,string>) (queue:System.Type list) = 
        match queue with 
        | [] -> acc |> Map.toArray |> Array.map snd
        | t :: tail -> 
            if acc |> Map.containsKey (t.FullName) then
                loop acc tail 
            elif FSharpType.IsRecord t then 
                let fields = FSharpType.GetRecordFields(t) 
                let args = fields |> Array.map (fun p -> p.Name) |> Strings.join " "
                let name = sprintf "create%s" t.Name
                let body = fields |> Array.map (fun p -> sprintf "        %s = %s" p.Name p.Name) |> Strings.join "\n"
                let signature = sprintf  "let %s %s = \n    {\n%s }"  name args body 
                let nextAcc = acc |> Map.add (t.FullName) signature
                let newQueue = 
                    fields 
                        |> Array.map (fun p -> p.PropertyType)
                        |> Array.filter (fun p -> p.Assembly = asm )
                        |> Array.filter (fun p -> FSharpType.IsRecord p)
                        |> Array.toList
                        |> List.append tail
                loop nextAcc newQueue
            else
                printfn "not a record"
                loop acc tail
    loop Map.empty [objType] |> Strings.join "\n"

let genZero (objType : Type) =
    let getDefaultValue (t : Type) = 
        match t with 
        | typ when typ = typeof<string> -> @"""""" 
        | typ when typ.IsValueType -> Activator.CreateInstance(typ) |> sprintf "%A"
        | typ when typ.Name = "FSharpOption`1" -> "None"
        | typ when typ.Name = "FSharpList`1" -> "[]"
        | typ when typ.IsArray -> "[||]"
        | typ when typ.Name = "FSharpMap`2" -> "Map.empty"
        | typ when FSharpType.IsUnion(typ) -> FSharpType.GetUnionCases typ |> Seq.head |> (fun u -> u.Name)
        | typ -> sprintf "%s.Zero" typ.Name
        //| _ -> failwithf "Unsupported type %A" t

    if not <| FSharpType.IsRecord objType then
        failwith "only record types are supported" 
    else 
        let fields = FSharpType.GetRecordFields(objType)
        let lines = fields |> Array.map (fun pi -> sprintf "%s = %s"  pi.Name (getDefaultValue pi.PropertyType))
        sprintf "type %s with\n%sstatic member Zero =\n%s{\n%s\n%s}" 
            <| objType.Name
            <| tabs 1
            <| tabs 2
            <| (lines |> Array.map(indentTabs 3) |> Strings.join "\n")
            <| tabs 2

let genLenses objType =
    let normalize (s : string) = 
        let chars = s.ToCharArray()
        let sb = new Text.StringBuilder()
        sb.Append(Char.ToUpper(chars.[0])) |> ignore
        let mutable i = 1
        while i < chars.Length  do
            if chars.[i] = '_' && i < (chars.Length - 1) then
                sb.Append(Char.ToUpper(chars.[i+1])) |> ignore 
                i <- i + 2
            else 
                sb.Append(chars.[i])  |> ignore
                i <- i + 1
        sb.ToString()

    if not <| FSharpType.IsRecord objType then
        failwith "only record types are supported" 
    else 
        let fields = FSharpType.GetRecordFields(objType)
        let name = objType.Name
        let lines = fields |> Array.map (fun pi -> 
            sprintf "static member %s = (fun (x:%s) -> x.%s) |> Lens.create <| fun v x -> {x with %s = v }"
                (normalize pi.Name)
                name
                pi.Name
                pi.Name)
        sprintf "type %s with\n%s" name 
            <| (lines |> Array.map (indentTabs 1) |> Strings.join "\n")

let stringMethodsFrom typeName cases =
    let sb = new StringBuilder()
    bprintf sb "member this.toString =
match this with 
%s"
        (cases |> Array.map (fun c -> sprintf "    | %s -> \"%s\"" c c) |> Strings.join "\n")
    bprintf sb "\nstatic member fromString s =
match s with
%s
| x -> failwithf \"invalid value %%A for type %s\" x"
        (cases |> Array.map (fun c -> sprintf "    | \"%s\" -> %s" c c) |> Strings.join "\n") 
        typeName
    
    bprintf sb "\nstatic member tryFromString s =
match s with
%s
| _ -> None"
        (cases |> Array.map (fun c -> sprintf "    | \"%s\" -> Some %s" c c) |> Strings.join "\n") 
    sb.ToString()


let genStringMethods<'a> ()=
    let objType = typeof<'a> 
    if not <| FSharpType.IsUnion objType then
        failwith "only union types are supported"
    else
        let cases = FSharpType.GetUnionCases objType |> Array.map (fun c -> c.Name)
        stringMethodsFrom objType.Name cases

let genCopy<'a> name = 
    let objType = typeof<'a>
    if not <| FSharpType.IsRecord objType then
        failwith "only record types supported"
    else
        FSharpType.GetRecordFields(objType) 
            |> Array.map (fun pi -> sprintf "    %s = %s.%s" pi.Name name pi.Name) 
            |> Strings.join "\n" 
            |> sprintf "{\n%s\n}" 

let genUnion (typeName : string) (items: string array) =
    sprintf "type %s =\n%s\n%s\n" 
        typeName 
        (items |> Array.map (sprintf "| %s") |> Strings.join "\n")
        (items |> stringMethodsFrom typeName) 


let genJson objType = 
    if FSharpType.IsRecord objType then
        let toJsonFields = 
            FSharpType.GetRecordFields(objType)
            |> Array.map (fun pi -> sprintf "\"%s\" .= x.%s" pi.Name pi.Name)
            |> Array.map (indentTabs 3) 
            |> Strings.join "\n" 
                
        let fromJsonFields = 
            FSharpType.GetRecordFields(objType)
            |> Array.map (fun pi -> sprintf "let! %s = json .@ \"%s\"" pi.Name pi.Name)
            |> Array.map (indentTabs 3) 
            |> Strings.join "\n" 

        let returnStatement =
            FSharpType.GetRecordFields(objType)
            |> Array.mapi (fun i pi -> 
                if i = 0 then
                    sprintf "%s.%s = %s" (objType.Name) pi.Name pi.Name 
                else
                    sprintf "%s = %s" pi.Name pi.Name 
            )
            |> Array.map (indentTabs 4)
            |> Strings.join "\n"
            |> fun lines -> sprintf "%sreturn {\n%s\n%s}\n" (tabs 3) lines (tabs 3)
             
        let sb = new StringBuilder()
        bprintf sb "type %s with\n" objType.Name
        bprintf sb "%sstatic member ToJson(x:%s) =\n" (tabs 1) objType.Name
        bprintf sb "%sjobj [|\n%s\n%s|]\n\n" (tabs 2) toJsonFields (tabs 2)
        bprintf sb "%sstatic member FromJson(_:%s) =\n" (tabs 1) objType.Name
        bprintf sb "%sparseObj <| fun json -> jsonParse {\n%s\n%s%s}" (tabs 2) fromJsonFields returnStatement (tabs 2)
        sb.ToString()
    elif FSharpType.IsUnion objType then
        let cases = FSharpType.GetUnionCases(objType) |> Array.map(fun ui -> ui.Name)
        let eachcase f = Array.map(fun c -> sprintf "| %s -> %s" c (f c)) >> Array.map (indentTabs 2) >> Strings.join "\n"
        let invarianteachcase = Array.map(fun c -> sprintf "| InvariantEqual \"%s\" -> succeed %s" c c) >> Array.map (indentTabs 3) >> Strings.join "\n"
        let sb = new StringBuilder() 
        bprintf sb "type %s with\n" objType.Name
        bprintf sb "%sstatic member inline ToJson(x:%s) =\n" (tabs 1) (objType.Name) 
        bprintf sb "%smatch x with\n%s\n\n" (tabs 2) (cases |> eachcase (Strings.quote >> sprintf "toJson %s"))
        bprintf sb "%sstatic member FromJson(_:%s) = function\n" (tabs 1) (objType.Name) 
        bprintf sb "%s| JsonValue.String s ->\n%smatch s with\n%s\n" (tabs 1) (tabs 2) (cases |> invarianteachcase) 
        bprintf sb "%s| _ -> fail %s\n" (tabs 3) (Strings.quote (objType.Name.ToLower()))
        bprintf sb "%s| json -> failOn %s json\n" (tabs 1) (Strings.quote (objType.Name.ToLower()))

        sb.ToString()
    else
        failwith "only record types supported"


let genCodec (objType:Type) = 
    let name = objType.Name
    sprintf "type %s with\n%sstatic member EventCodec : EventCodec<%s> = %s \"%s\""
        <| name
        <| tabs 1
        <| name
        <| "jsonValueCodec"
        <| name

type GenKind = Lens | Zero | Json | Codec | Constructor 

let private genKind (typ:Type) = function
    | Lens -> genLenses typ
    | Zero -> genZero typ
    | Json -> genJson typ
    | Codec -> genCodec typ
    | Constructor -> genConstructor typ

let codeGen kinds typ = kinds |> Seq.map (genKind typ) |> Strings.join "\n\n\n"
let codeGenT<'a> recurse kinds = 
    if not recurse then
        codeGen kinds (typeof<'a>) 
    else
        let rec gatherTypes (acc:System.Collections.Generic.HashSet<_>) typ = 
            if FSharpType.IsRecord typ then
                acc.Add(typ) |> ignore
                for pi in FSharpType.GetRecordFields(typ) do
                    gatherTypes acc (pi.PropertyType)
            elif typ.Name = "FSharpList`1" then
                gatherTypes acc (typ.GenericTypeArguments.[0])
            elif typ.IsArray then
                gatherTypes acc (typ.GetElementType())
            else
                ()
        let hs = new System.Collections.Generic.HashSet<_>()
        gatherTypes hs (typeof<'a>)
        hs 
        |> Seq.toList
        |> List.rev
        |> List.map (codeGen kinds) 
        |> Strings.join "\n\n\n"


let codeGenT'<'a> recurse = Strings.copyClip << codeGenT<'a> recurse
let codeGenAllT<'a> recurse = codeGenT<'a> recurse [ Lens; Zero; Json; Codec; Constructor] 
let codeGenAllT'<'a> recurse = codeGenT'<'a> recurse [ Lens; Zero; Json; Codec; Constructor]

