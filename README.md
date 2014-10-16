FSITools
========

Collection of tools and functions to make working in FSI easier

This is very early stage, but has some useful functionality


#### Attaching to processes

```fsharp

attach ();; // will attach your current instance of visual studio to the current fsi session

attachPid 1234;; // will attach your current instance of visual studio to process with a PID of 1234

```

#### Handy utils
```fsharp

copyClip "string" // copies the string to the clipboard, useful when generating code using the code gen functions

it |> copyClip;; // copies the previous string result in FSI to the clip board
```


#### Code Generation

```fsharp

// will generate type extensions specified in the list
// boolean flag is whether to generate type extensions recursively
codeGenT<MyRecordType> false [ Json; Zero; Codec; Constructor ]

// same as above but automatically copies the result to the clipboard
codeGenT'<MyRecordType> false [ Json; Zero; Codec; Constructor ]

// will generate type extensions just for a single type by System.Type
codeGen [ Json; Zero; Codec; Constructor ] typeOf<MyRecordType> 

```


#### Getting Started

* Clone this repo
* Build with visual studio
* In Visual Studio goto Tools -> Options -> F# Tools
  * In F# interactive options add 
    `--use:C:\Tools\git\FsiTools\src\FsiTools\BootstrapInteractive.fsx` 

## Example

#### Create type
```fsharp
type Inner = {
    inner : string
}

type MyRecordType = {
    id : int
    name : string
    inner : Inner
}
```

#### Run in FSI
```fsharp

codeGenT'<MyRecordType> true [Lens; Zero; Json; Codec; Constructor; ]

```

#### Paste into your code
```fsharp
type Inner with
    static member Inner = (fun (x:Inner) -> x.inner) |> Lens.create <| fun v x -> {x with inner = v }


type Inner with
    static member Zero =
        {
            inner = ""
        }


type Inner with
    static member ToJson(x:Inner) =
        jobj [|
            "inner" .= x.inner
        |]

    static member FromJson(_:Inner) =
        parseObj <| fun json -> jsonParse {
            let! inner = json .@ "inner"
            return {
                Inner.inner = inner
            }
        }


type Inner with
    static member EventCodec : EventCodec<Inner> = codec "Inner"


let createInner inner = 
    {
        inner = inner }


type MyRecordType with
    static member Id = (fun (x:MyRecordType) -> x.id) |> Lens.create <| fun v x -> {x with id = v }
    static member Name = (fun (x:MyRecordType) -> x.name) |> Lens.create <| fun v x -> {x with name = v }
    static member Inner = (fun (x:MyRecordType) -> x.inner) |> Lens.create <| fun v x -> {x with inner = v }


type MyRecordType with
    static member Zero =
        {
            id = 0
            name = ""
            inner = Inner.Zero
        }


type MyRecordType with
    static member ToJson(x:MyRecordType) =
        jobj [|
            "id" .= x.id
            "name" .= x.name
            "inner" .= x.inner
        |]

    static member FromJson(_:MyRecordType) =
        parseObj <| fun json -> jsonParse {
            let! id = json .@ "id"
            let! name = json .@ "name"
            let! inner = json .@ "inner"
            return {
                MyRecordType.id = id
                name = name
                inner = inner
            }
        }


type MyRecordType with
    static member EventCodec : EventCodec<MyRecordType> = codec "MyRecordType"


let createInner inner = 
    {
        inner = inner }
let createMyRecordType id name inner = 
    {
        id = id
        name = name
        inner = inner }
```



